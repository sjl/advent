(defpackage :advent/2019/01 #.cl-user::*advent-use*)
(in-package :advent/2019/01)


(defun fuel-required (module-mass)
  (max 0 (- (floor module-mass 3) 2)))

(defun complete-fuel-required (module-mass)
  (iterate
    (for fuel :first (fuel-required module-mass) :then (fuel-required fuel))
    (summing fuel)
    (until (zerop fuel))))

(define-problem (2019 1) (data read-all)
  (values (summation data :key #'fuel-required)
          (summation data :key #'complete-fuel-required)))

(define-problem-tests (2019 1) 3464458 1)
