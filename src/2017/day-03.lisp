(defpackage :advent/2017/03 #.cl-user::*advent-use*)
(in-package :advent/2017/03)

(defmacro let-complex (bindings &body body)
  `(let* (,@(iterate (for (x y val) :in bindings)
                     (for v = (gensym))
                     (collect `(,v ,val))
                     (collect `(,x (realpart ,v)))
                     (collect `(,y (imagpart ,v)))))
     ,@body))

(defun manhattan-distance (a b)
  (let-complex ((ax ay a)
                (bx by b))
    (+ (abs (- ax bx))
       (abs (- ay by)))))

(defun neighbors (coord)
  (iterate (for (dx dy) :within-radius 1 :skip-origin t)
           (collect (+ coord (complex dx dy)))))

(define-problem (2017 3) (data read)
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

(1am:test test-2017/03
  (multiple-value-bind (part1 part2) (run)
    (1am:is (= 552 part1))
    (1am:is (= 330785 part2))))
