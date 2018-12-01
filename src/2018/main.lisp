(in-package :advent)

;;;; Problems -----------------------------------------------------------------
(define-problem (2018 1 1) (data read-all-from-file)
  (summation data))

(define-problem (2018 1 2) (data read-all-from-file)
  (setf (cdr (last data)) data) ; make data a circular list for easy looping
  (iterate
    (with seen = (make-hash-set :initial-contents '(0)))
    (for number :in data)
    (summing number :into frequency)
    (if (hset-contains-p seen frequency)
      (return frequency)
      (hset-insert! seen frequency))))
