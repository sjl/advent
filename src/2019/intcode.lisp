(defpackage :advent/intcode
  #.cl-user::*advent-use*
  (:shadow :step :trace)
  (:export :init :step :run :run-machine :*trace*))

(in-package :advent/intcode)

(defparameter *trace* nil)
(defparameter *trace-lock* (bt:make-lock "intcode trace lock"))


;;;; Data Structures ----------------------------------------------------------
(defclass* machine ()
  ((pc :type (integer 0) :initform 0)
   (rb :type (integer 0) :initform 0)
   (memory :type hash-table)
   (input :type function)
   (output :type function)))

(define-with-macro machine pc rb memory input output)

(defun mref (machine address &optional (default 0))
  (gethash address (memory machine) default))

(defun (setf mref) (new-value machine address &optional (default 0))
  (setf (gethash address (memory machine) default)
        new-value))


(defclass* operation ()
  ((opcode :type (integer 0))
   (name :type symbol)
   (size :type (integer 1))
   (parameters :type list)
   (perform :type (or symbol function))))

(defun perform-operation (opcode parameter-modes machine)
  (funcall (perform (gethash opcode *operations*))
           parameter-modes machine))


;;;; Opcode Definition --------------------------------------------------------
(defun retrieve (machine parameter-mode operand &key out)
  ;; Note that (confusingly) output parameters don't use the same addressing
  ;; scheme as input parameters.  For example: in the instruction 00002,1,2,99
  ;; all the parameter modes are 0, which means "look up the value at address
  ;; N to get the parameter".  But for the destination (99) you *don't* look up
  ;; the value at 99 to find the destination address, you just store directly
  ;; into 99.  Effectively they're treated as if they were in parameter mode
  ;; 1 (immediate mode).  So we need to handle output parameters specially.
  ;;
  ;; Sigh.
  (ecase parameter-mode
    (0 (if out ; position
         operand
         (mref machine operand)))
    (1 operand) ; immediate
    (2 (if out ; relative
         (+ (rb machine) operand)
         (mref machine (+ (rb machine) operand))))))

(defmacro define-opcode ((opcode name) parameters &body body)
  (setf parameters (mapcar (lambda (param)
                             (if (symbolp param)
                               `(,param in)
                               param))
                           parameters))
  (let ((function-name (alexandria:symbolicate 'op- name)))
    (alexandria:with-gensyms (machine pmodes pm pms)
      `(progn
         (defun ,function-name (,pmodes ,machine)
           (declare (ignorable ,pmodes))
           (,@(if parameters
                `(flet ((pop-mode ()
                          (multiple-value-bind (,pms ,pm) (truncate ,pmodes 10)
                            (setf ,pmodes ,pms)
                            ,pm))))
                `(progn))
            (with-machine (,machine)
              (let (,@(iterate
                        (for (param kind) :in parameters)
                        (for offset :from 0)
                        (collect
                          `(,param (retrieve ,machine
                                             (pop-mode)
                                             (mref ,machine (+ pc ,offset))
                                             :out ,(ecase kind
                                                     (in nil)
                                                     (out t)))))))
                (incf pc ,(length parameters))
                (macrolet ((mem (addr)
                             `(mref ,',machine ,addr)))
                  ,@body)))))
         (setf (gethash ,opcode *operations*)
               (make-instance 'operation
                 :opcode ,opcode
                 :name ',name
                 :size ,(1+ (length parameters))
                 :parameters ',parameters
                 :perform #',function-name))
         ',function-name))))


;;;; Opcodes ------------------------------------------------------------------
(defparameter *operations* (make-hash-table))

(define-opcode (99 HLT) ()
  :halt)

(define-opcode (1 ADD) (x y (dest out))
  (setf (mem dest) (+ x y)))

(define-opcode (2 MUL) (x y (dest out))
  (setf (mem dest) (* x y)))

(define-opcode (3 INP) ((dest out))
  (setf (mem dest) (funcall input)))

(define-opcode (4 OUT) (val)
  (funcall output val))

(define-opcode (5 JPT) (x addr)
  (unless (zerop x)
    (setf pc addr)))

(define-opcode (6 JPF) (x addr)
  (when (zerop x)
    (setf pc addr)))

(define-opcode (7 LES) (x y (dest out))
  (setf (mem dest)
        (if (< x y) 1 0)))

(define-opcode (8 EQL) (x y (dest out))
  (setf (mem dest)
        (if (= x y) 1 0)))

(define-opcode (9 ARB) (val)
  (incf rb val))


;;;; Disassembly --------------------------------------------------------------
(defun parse-op (n)
  (multiple-value-bind (parameter-modes opcode) (truncate n 100)
    (values opcode parameter-modes)))

(defun disassemble-operation (machine address)
  (multiple-value-bind (opcode parameter-modes)
      (parse-op (mref machine address))
    (let ((op (gethash opcode *operations*)))
      (if op
        (values
          `(,(name op)
            ,@(iterate
                (for (param kind) :in (parameters op))
                (for addr :from (1+ address))
                (for value = (mref machine addr))
                (for mode = (mod parameter-modes 10))
                (collect `(,param ,(ecase kind
                                     (in (ecase mode
                                           (0 (vector value))
                                           (1 value)
                                           (2 (list :r value))))
                                     (out (ecase mode
                                            ((0 1) value)
                                            (2 (list :r value)))))))
                (setf parameter-modes (truncate parameter-modes 10))))
          (size op))
        (values `(data ,(mref machine address)) 1)))))

(defun disassemble-program (machine &key (start 0) (limit nil))
  (iterate
    (when limit
      (if (zerop limit)
        (return)
        (decf limit)))
    (with address = start)
    (with addresses = (-<> (memory machine)
                        alexandria:hash-table-keys
                        (sort <> #'<)))
    (with bound = (1+ (elt addresses (1- (length addresses)))))
    (flet ((advance (addr)
             (iterate
               (until (null addresses))
               (while (> addr (first addresses)))
               (pop addresses))))
      (advance address))
    (while addresses)
    (for (values instruction size) = (disassemble-operation machine address))
    (for end = (+ address size))
    (when (> end bound) ; hack to handle trailing data that looks instructionish
      (setf instruction `(data ,(mref machine address))
            size 1
            end (1+ address)))
    (for bytes = (iterate (for i :from address :below end)
                          (collect (mref machine i))))
    (format t "~4D | ~4D | ~{~5D~^ ~} ~42T| ~{~A~^ ~}~%" address (rb machine) bytes instruction)
    (incf address size)))


;;;; Running ------------------------------------------------------------------
(defun program->hash-table (program &key (test #'eql))
  (iterate (for x :in-whatever program)
           (for i :from 0)
           (collect-hash (i x) :test test)))

(defun init (program &key input output)
  (make-instance 'machine
    :memory (program->hash-table program)
    :input (or input #'read)
    :output (or output #'print)))

(defun step (machine &key (trace *trace*))
  (with-machine (machine)
    (when trace
      (bt:with-lock-held (*trace-lock*)
        (unless (member trace '(t nil))
          (format t "~A: " trace))
        (disassemble-program machine :start pc :limit 1)))
    (multiple-value-bind (opcode parameter-modes) (parse-op (mref machine pc))
      (incf pc)
      (perform-operation opcode parameter-modes machine))))

(defun run-machine (machine &key (trace *trace*))
  (iterate
    (case (step machine :trace trace)
      (:halt (return (mref machine 0))))))

(defun run (program &key input output (trace *trace*))
  (run-machine (init program :input input :output output) :trace trace))

;; #; Scratch --------------------------------------------------------------------

;; (defparameter *m* (init '(1101 100 -1 4 99)))
;; (dump *m*)
;; (disassemble-operation (memory *m*) 0)
;; (disassemble-program (memory *m*))
;; (step *m*)

;; (run #( 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)
;;      :input #'read :output #'print)
