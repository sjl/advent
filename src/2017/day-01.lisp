(defpackage :advent/2017/01 #.cl-user::*advent-use*)
(in-package :advent/2017/01)


(define-problem (2017 1) (data read-line)
  (iterate
    (with digits = (map 'vector #'digit-char-p data))
    (for digit :in-vector digits)
    (for prev :previous digit :initially (aref digits (1- (length digits))))
    (for j :modulo (length digits) :from (truncate (length digits) 2))
    (for next = (aref digits j))
    (when (= digit prev) (sum digit :into part1))
    (when (= digit next) (sum digit :into part2))
    (finally (return (values part1 part2)))))

(1am:test test-2017/01
  (multiple-value-bind (part1 part2) (run)
    (1am:is (= 1049 part1))
    (1am:is (= 1508 part2))))
