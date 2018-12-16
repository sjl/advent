(defpackage :advent/2018/01 #.cl-user::*advent-use*)
(in-package :advent/2018/01)


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

(1am:test test-2018/01
  (multiple-value-bind (part1 part2) (run)
    (1am:is (= 522 part1))
    (1am:is (= 73364 part2))))
