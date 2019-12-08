(defpackage :advent/2018/03 #.cl-user::*advent-use*)
(in-package :advent/2018/03)
(named-readtables:in-readtable :interpol-syntax)


(defstruct (claim (:conc-name nil)) id left right top bottom)
(define-with-macro claim id left right top bottom)

(defun parse-claim (line)
  (ppcre:register-groups-bind
      ((#'parse-integer id col row width height))
      (#?r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
    (make-claim :id id
                :left col
                :top row
                :right (+ col width)
                :bottom (+ row height))))

(defun claims-intersect-p (claim1 claim2)
  (with-claim (claim1 id1 left1 right1 top1 bottom1)
    (with-claim (claim2 id2 left2 right2 top2 bottom2)
      (not (or (<= right2 left1)
               (<= right1 left2)
               (>= top2 bottom1)
               (>= top1 bottom2))))))

(defun make-fabric (claims)
  (let ((fabric (make-array (list 1000 1000) :initial-element 0)))
    (dolist (claim claims)
      (with-claim (claim)
        (do-range ((row top bottom)
                   (col left right))
                  (incf (aref fabric row col)))))
    fabric))


(define-problem (2018 3) (data read-lines) (107663 1166)
  (let* ((claims (mapcar #'parse-claim data))
         (fabric (make-fabric claims)))
    (values
      (iterate (for uses :in-array fabric)
               (counting (> uses 1)))
      (id (first (unique claims :test #'claims-intersect-p))))))

