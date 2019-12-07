(defpackage :advent/2017/03 #.cl-user::*advent-use*)
(in-package :advent/2017/03)


(defun neighbors (coord)
  (iterate (for (dx dy) :within-radius 1 :skip-origin t)
           (collect (+ coord (complex dx dy)))))

(define-problem (2017 3) (data read) (552 330785)
  (values
    (manhattan-distance #c(0 0) (advent/spiral:number-coordinates data))
    (iterate
      (with memory = (make-hash-table))
      (initially (setf (gethash #c(0 0) memory) 1))
      (for n :from 2)
      (for coord = (advent/spiral:number-coordinates n))
      (for value = (summation (neighbors coord) :key (rcurry #'gethash memory 0)))
      (finding value :such-that (> value data))
      (setf (gethash coord memory) value))))


