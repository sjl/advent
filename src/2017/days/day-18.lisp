(defpackage :advent/2017/18 #.cl-user::*advent-use*)
(in-package :advent/2017/18)

(defmacro opcase (op &body clauses)
  (alexandria:once-only (op)
    `(case (first ,op)
       ,@(iterate
          (for ((opcode . args) . body) :in clauses)
          (collect `(,opcode (destructuring-bind ,args (rest ,op)
                               ,@body)))))))

(defclass* machine ()
  ((id :type integer)
   (program :type vector)
   (pc :initform 0)
   (registers :initform (make-hash-table))
   (queue :initform (make-queue))
   (waiting :initform nil)))

(defun reg (machine register)
  (gethash register (registers machine) 0))

(defun (setf reg) (new-value machine register)
  (setf (gethash register (registers machine) 0)
        new-value))

(defun val (machine register-or-constant)
  (etypecase register-or-constant
    (symbol (r machine register-or-constant))
    (number register-or-constant)))

(defun handle-common-ops (machine op)
  (macrolet ((r (x) `(reg machine ,x))
             (v (x) `(val machine ,x)))
    (opcase op
      ((set x y) (setf (r x) (v y)))
      ((add x y) (setf (r x) (+ (v x) (v y))))
      ((mul x y) (setf (r x) (* (v x) (v y))))
      ((mod x y) (setf (r x) (mod (v x) (v y))))
      ((jgz x y) (when (plusp (v x))
                   (incf (pc machine)
                         (1- (v y))))))))

(defun part1 (program)
  (iterate
    (with machine = (make-instance 'machine :program program))
    (with hz = nil)
    (with bound = (length program))
    (while (in-range-p 0 (pc machine) bound))
    (for op = (aref (program machine) (pc machine)))
    (incf (pc machine))
    (handle-common-ops machine op)
    (opcase op
      ((snd x) (setf hz (val machine x)))
      ((rcv x) (unless (zerop (val machine x))
                 (return hz))))))

(defun part2 (program)
  (iterate
    (with a = (make-instance 'machine :program program :id 0))
    (with b = (make-instance 'machine :program program :id 1))
    (with bound = (length program))
    (initially (setf (reg a 'p) 0
                     (reg b 'p) 1))
    (while (in-range-p 0 (pc a) bound))
    (for op = (aref (program a) (pc a)))
    (incf (pc a))
    (handle-common-ops a op)
    (opcase op
      ((snd x) (progn
                 (counting (= 1 (id a)))
                 (enqueue (val a x) (queue b))))
      ((rcv x) (cond ((not (queue-empty-p (queue a)))
                      (setf (reg a x) (dequeue (queue a))))
                     ((waiting b) (pr 'deadlock) (finish))
                     (t (progn (decf (pc a))
                               (setf (waiting a) t
                                     (waiting b) nil)
                               (rotatef a b))))))))

(define-problem (2017 18) (data read-lines) (1187)
  (setf data (map 'vector #'read-all-from-string data))
  (values (part1 data)
          (part2 data))
  )


#; Scratch --------------------------------------------------------------------

(run '("set a 1"
       "add a 2"
       "mul a a"
       "mod a 5"
       "snd a"
       "set a 0"
       "rcv a"
       "jgz a -1"
       "set a 1"
       "jgz a -2"))

(run '("snd 1"
       "snd 2"
       "snd p"
       "rcv a"
       "rcv b"
       "rcv c"
       "rcv d"))
