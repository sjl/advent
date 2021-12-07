(advent:defpackage* :advent/2021/07)
(in-package :advent/2021/07)

(defun triangle (n)
  (/ (* n (1+ n)) 2))

(defun cost (crabs position &key modifier)
  (summation crabs :key (lambda (crab) (funcall modifier (abs (- crab position))))))

(defun find-best-cost (crabs &key cost-modifier)
  (multiple-value-bind (lo hi) (extrema #'< crabs)
    (iterate (for pos :from lo :to hi)
             (minimizing (cost crabs pos :modifier cost-modifier)))))

(define-problem (2021 7) (data read-comma-separated-integers) (328187 91257582)
  (values (find-best-cost data :cost-modifier #'identity)
          (find-best-cost data :cost-modifier #'triangle)))


#; Scratch --------------------------------------------------------------------
