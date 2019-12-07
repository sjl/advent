(defpackage :advent/intcode
  #.cl-user::*advent-use*
  (:shadow :step :trace)
  (:export :init :step :run :run-machine))

(in-package :advent/intcode)

(defparameter *trace-lock* (bt:make-lock "intcode trace lock"))

;;;; Data Structures ----------------------------------------------------------
(defclass* machine ()
  ((pc :type (integer 0) :initform 0)
   (memory :type (vector integer))
   (input :type function)
   (output :type function)))

(define-with-macro machine pc memory input output)


(defclass* operation ()
  ((opcode :type (integer 0))
   (name :type symbol)
   (size :type (integer 1))
   (parameters :type list)
   (perform :type function)))

(defun perform-operation (opcode parameter-modes machine)
  (funcall (perform (gethash opcode *operations*))
           parameter-modes machine))


;;;; Opcode Definition --------------------------------------------------------
(defun retrieve (machine parameter-mode operand)
  (ecase parameter-mode
    (0 (aref (memory machine) operand))
    (1 operand)))

(defmacro define-opcode ((opcode name) parameters &body body)
  ;; Note that (confusingly) output parameters don't use the same addressing
  ;; scheme as input parameters.  For example: in the instruction 00002,1,2,99
  ;; all the parameter modes are 0, which means "look up the value at address
  ;; N to get the parameter".  But for the destination (99) you *don't* look up
  ;; the value at 99 to find the destination address, you just store directly
  ;; into 99.  Effectively they're treated as if they were in parameter mode
  ;; 1 (immediate mode).  So we need to handle output parameters specially.
  ;;
  ;; Sigh.
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
                          (ecase kind
                            (in `(,param (retrieve ,machine
                                                   (pop-mode)
                                                   (aref memory (+ pc ,offset)))))
                            (out `(,param (progn
                                            (pop-mode)
                                            (aref memory (+ pc ,offset)))))))))
                (incf pc ,(length parameters))
                ,@body))))
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
  (setf (aref memory dest) (+ x y)))

(define-opcode (2 MUL) (x y (dest out))
  (setf (aref memory dest) (* x y)))

(define-opcode (3 INP) ((dest out))
  (setf (aref memory dest) (funcall input)))

(define-opcode (4 OUT) (val)
  (funcall output val))

(define-opcode (5 JPT) (x addr)
  (unless (zerop x)
    (setf pc addr)))

(define-opcode (6 JPF) (x addr)
  (when (zerop x)
    (setf pc addr)))

(define-opcode (7 LES) (x y (dest out))
  (setf (aref memory dest)
        (if (< x y) 1 0)))

(define-opcode (8 EQL) (x y (dest out))
  (setf (aref memory dest)
        (if (= x y) 1 0)))


;;;; Disassembly --------------------------------------------------------------
(defun parse-op (n)
  (multiple-value-bind (parameter-modes opcode) (truncate n 100)
    (values opcode parameter-modes)))

(defun disassemble-operation (program address)
  (multiple-value-bind (opcode parameter-modes)
      (parse-op (aref program address))
    (let ((op (gethash opcode *operations*)))
      (if op
        (values
          `(,(name op)
            ,@(iterate
                (for (param kind) :in (parameters op))
                (for value :in-vector program :from (1+ address))
                (for mode = (mod parameter-modes 10))
                (collect `(,param ,(ecase kind
                                     (in (ecase mode
                                           (0 (vector value))
                                           (1 value)))
                                     (out value))))
                (setf parameter-modes (truncate parameter-modes 10))))
          (size op))
        (values `(data ,(aref program address)) 1)))))

(defun disassemble-program (program &key (start 0) (limit nil))
  (iterate
    (when limit
      (if (zerop limit)
        (return)
        (decf limit)))
    (with address = start)
    (with bound = (length program))
    (while (< address bound))
    (for (values instruction size) = (disassemble-operation program address))
    (for end = (+ address size))
    (when (> end bound) ; hack to handle trailing data that looks instructionish
      (setf instruction `(data ,(aref program address))
            size 1
            end (1+ address)))
    (for bytes = (coerce (subseq program address end) 'list))
    (format t "~4D | ~{~5D~^ ~} ~36T| ~{~A~^ ~}~%" address bytes instruction)
    (incf address size)))


;;;; Running ------------------------------------------------------------------
(defun init (program &key input output)
  (make-instance 'machine
    :memory (fresh-vector program)
    :input (or input #'read)
    :output (or output #'print)))

(defun step (machine &key trace)
  (with-machine (machine)
    (when trace
      (bt:with-lock-held (*trace-lock*)
        (when (numberp trace)
          (format t "~D: " trace))
        (disassemble-program (memory machine) :start pc :limit 1)))
    (multiple-value-bind (opcode parameter-modes) (parse-op (aref memory pc))
      (incf pc)
      (perform-operation opcode parameter-modes machine))))

(defun run-machine (machine &key trace)
  (iterate
    (case (step machine :trace trace)
      (:halt (return (aref (memory machine) 0))))))

(defun run (program &key input output trace)
  (run-machine (init program :input input :output output) :trace trace))

;; #; Scratch --------------------------------------------------------------------

;; (defparameter *m* (init '(1101 100 -1 4 99)))
;; (dump *m*)
;; (disassemble-operation (memory *m*) 0)
;; (disassemble-program (memory *m*))
;; (step *m*)

;; (run #( 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)
;;      :input #'read :output #'print)
