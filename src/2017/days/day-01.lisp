(advent:defpackage* :advent/2017/01)
(in-package :advent/2017/01)


(define-problem (2017 1) (data read-line) (1049 1508)
  (iterate
    (with digits = (map 'vector #'digit-char-p data))
    (for digit :in-vector digits)
    (for prev :previous digit :initially (aref digits (1- (length digits))))
    (for j :modulo (length digits) :from (truncate (length digits) 2))
    (for next = (aref digits j))
    (when (= digit prev) (sum digit :into part1))
    (when (= digit next) (sum digit :into part2))
    (finally (return (values part1 part2)))))

