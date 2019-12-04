(defpackage :advent/2019/02 #.cl-user::*advent-use*)
(in-package :advent/2019/02)

(defun run-intcode (memory a b)
  (let ((memory (fresh-vector memory)))
    (macrolet ((m (addr &rest deltas)
                 `(aref memory (+ ,addr ,@deltas))))
      (iterate
        (initially (setf (m 1) a
                         (m 2) b))
        (with pc = 0)
        (for op   = (m pc))
        (for x    = (m (m pc 1)))
        (for y    = (m (m pc 2)))
        (for dest = (m pc 3))
        (ecase op
          (1 (setf (m dest) (+ x y)))
          (2 (setf (m dest) (* x y)))
          (99 (return (m 0))))
        (incf pc 4)))))

(define-problem (2019 2) (data read-numbers) ()
  (values
    (run-intcode data 12 2)
    (iterate
      (for-nested ((a :from 0 :to 99)
                   (b :from 0 :to 99)))
      (when (= 19690720 (run-intcode data a b))
        (return (+ (* 100 a) b))))))

