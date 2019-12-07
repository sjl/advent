(defpackage :advent/2016/03 #.cl-user::*advent-use*)
(in-package :advent/2016/03)

(defun validp (a b c)
  (when (> a b) (rotatef a b))
  (when (> b c) (rotatef b c))
  (< c (+ a b)))

(define-problem (2016 3) (data read-lines-of-numbers-and-garbage) (1050 1921)
  (values
    (iterate
      (for (a b c) :in data)
      (counting (validp a b c)))
    (iterate ;; this is just dumb
      (for (x y z) :on data :by #'cdddr)
      (for (ax bx cx) = x)
      (for (ay by cy) = y)
      (for (az bz cz) = z)
      (counting (validp ax ay az))
      (counting (validp bx by bz))
      (counting (validp cx cy cz)))))


#; Scratch --------------------------------------------------------------------
