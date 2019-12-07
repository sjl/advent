(defpackage :advent/2017/15 #.cl-user::*advent-use*)
(in-package :advent/2017/15)

(defun-inline gen (previous factor)
  (rem (* previous factor) 2147483647))

(defun-inline gen-harder (previous factor divisor)
  (iterate
    (for x :seed previous :then (gen x factor))
    (finding x :such-that (dividesp x divisor))))

(defun-inline matchp (a b)
  (= (ldb (byte 16 0) a)
     (ldb (byte 16 0) b)))

(define-problem (2017 15) (data read-numbers) (650 336)
  (destructuring-bind (a-start b-start) data
    (values
      (iterate
        (repeat 40000000)
        (for a :seed a-start :then (gen a 16807))
        (for b :seed b-start :then (gen b 48271))
        (counting (matchp a b)))
      (iterate
        (repeat 5000000)
        (for a :seed a-start :then (gen-harder a 16807 4))
        (for b :seed b-start :then (gen-harder b 48271 8))
        (counting (matchp a b))))))
