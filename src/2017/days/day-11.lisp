(defpackage :advent/2017/11 #.cl-user::*advent-use*)
(in-package :advent/2017/11)

;; https://www.redblobgames.com/grids/hexagons/#coordinates

(defun coord+ (c1 c2)
  (map 'vector #'+ c1 c2))

(defun coord- (c1 c2)
  (map 'vector #'- c1 c2))

(defun distance (c1 &optional (c2 #(0 0 0)))
  (/ (reduce #'+ (coord- c1 c2) :key #'abs) 2))

(defun delta (direction)
  (ecase direction
    (:nw #(-1  0  1))
    (:n  #( 0 -1  1))
    (:ne #( 1 -1  0))
    (:se #( 1  0 -1))
    (:s  #( 0  1 -1))
    (:sw #(-1  1  0))))

(define-problem (2017 11) (data read-comma-separated-values) (773 1560)
  (iterate
    (with pos = #(0 0 0))
    (for direction :in (mapcar #'ensure-keyword data))
    (setf pos (coord+ pos (delta direction)))
    (maximizing (distance pos) :into furthest)
    (finally (return (values (distance pos) furthest)))))
