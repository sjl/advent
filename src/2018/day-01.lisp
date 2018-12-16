(defpackage :advent/2018/01 #.cl-user::*advent-use*)
(in-package :advent/2018/01)
(named-readtables:in-readtable :interpol-syntax)


(define-problem (2018 1) (data read-all)
  (values
    (summation data)
    (progn
      (setf (cdr (last data)) data) ; make data a circular list for easy looping
      (iterate
        (with seen = (make-hash-set :initial-contents '(0)))
        (for number :in data)
        (summing number :into frequency)
        (if (hset-contains-p seen frequency)
          (return frequency)
          (hset-insert! seen frequency))))))
