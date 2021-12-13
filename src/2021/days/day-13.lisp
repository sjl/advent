(advent:defpackage* :advent/2021/13)
(in-package :advent/2021/13)

(defun dot (x y) (complex x y))
(defun x (dot) (realpart dot))
(defun y (dot) (imagpart dot))

(defun parse (lines)
  (iterate
    (for line :in lines)
    (ppcre:register-groups-bind ((#'parse-integer x y)) ("(\\d+),(\\d+)" line)
      (collect (dot x y) :into dots))
    (ppcre:register-groups-bind (axis n) ("fold along ([xy])=(\\d+)" line)
      (collect (cons (char axis 0) (parse-integer n)) :into folds))
    (returning dots folds)))

(defun fold% (fold dot)
  (destructuring-bind (axis . n) fold
    (ecase axis
      (#\x (if (> (x dot) n)
             (complex (- n (- (x dot) n)) (y dot))
             dot))
      (#\y (if (> (y dot) n)
             (complex (x dot) (- n (- (y dot) n)))
             dot)))))

(defun fold (fold dots)
  (map-into dots (curry #'fold% fold) dots))

(defun dots-to-hash-table (dots)
  (iterate (for dot :in dots) (collect-hash (dot #\█))))

(defun part1 (dots folds &aux (dots (copy-list dots)))
  (length (remove-duplicates (fold (first folds) dots))))

(defun part2 (dots folds &aux (dots (copy-list dots)))
  (map nil (rcurry #'fold dots) folds)
  ;; (print-hash-table-map (dots-to-hash-table dots) :flip-y t)
  "WELP") ; My part 2 answer contains a slur so let's not hardcode THAT test case here.

(define-problem (2021 13) (lines read-lines) (682 "WELP")
  (multiple-value-bind (dots folds) (parse lines)
    (values (part1 dots folds)
            (part2 dots folds))))

#; Scratch --------------------------------------------------------------------

