(defpackage :advent/2017/02 #.cl-user::*advent-use*)
(in-package :advent/2017/02)

(defun find-quotient (row)
  (alexandria:map-permutations
    (lambda (pair)
      (multiple-value-bind (quotient remainder)
          (truncate (first pair) (second pair))
        (when (zerop remainder)
          (return-from find-quotient quotient))))
    row :length 2 :copy nil))

(defun checksum (row)
  (multiple-value-bind (lo hi) (extrema #'< row)
    (- hi lo)))

(define-problem (2017 2) (data read-lines-of-numbers-and-garbage) (53460 282)
  (iterate
    (for row :in data)
    (summing (checksum row) :into part1)
    (summing (find-quotient row) :into part2)
    (finally (return (values part1 part2)))))

