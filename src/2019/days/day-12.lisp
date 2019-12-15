(advent:defpackage* :advent/2019/12)
(in-package :advent/2019/12)

(defclass* moon () (pos vel))

(defun make-moon (x y z)
  (make-instance 'moon
    :pos (vector x y z)
    :vel (vector 0 0 0)))

(defun parse-moons (data)
  (mapcar (curry #'apply #'make-moon) data))

(defun apply-gravity (moon other)
  (map-into (vel moon) (lambda (v m o)
                         (+ v (signum (- o m))))
            (vel moon)
            (pos moon)
            (pos other)))

(defun apply-all-gravity (moons)
  (alexandria:map-permutations (curry #'apply #'apply-gravity)
                               moons :length 2 :copy nil))

(defun vincf (vec delta)
  (map-into vec #'+ vec delta))

(defun apply-velocity (moon)
  (vincf (pos moon) (vel moon)))

(defun apply-all-velocity (moons)
  (map nil #'apply-velocity moons))

(defun tick (moons &optional (n 1))
  (do-repeat n
    (apply-all-gravity moons)
    (apply-all-velocity moons)))

(defmethod energy ((moon moon))
  (* (summation (pos moon) :key #'abs)
     (summation (vel moon) :key #'abs)))

(defmethod energy ((moons sequence))
  (summation moons :key #'energy))

(defun velocity-zeroed-p (moons axis)
  (iterate (for moon :in moons)
           (always (zerop (aref (vel moon) axis)))))

(defun part2 (moons)
  ;; Clever trick from https://www.reddit.com/r/adventofcode/comments/e9nqpq/day_12_part_2_2x_faster_solution/
  (iterate
    (for tick :from 1)
    (tick moons)
    (finding-first tick :such-that (velocity-zeroed-p moons 0) :into x)
    (finding-first tick :such-that (velocity-zeroed-p moons 1) :into y)
    (finding-first tick :such-that (velocity-zeroed-p moons 2) :into z)
    (until (and x y z))
    (returning (* 2 (lcm x y z)))))

(define-problem (2019 12) (data read-lines-of-numbers-and-garbage) (14780 279751820342592)
  (values
    (let ((moons (parse-moons data)))
      (tick moons 1000)
      (energy moons))
    (part2 (parse-moons data))))


#; Scratch --------------------------------------------------------------------

(defparameter *a* (make-moon '(0 0 0)))
(defparameter *b* (make-moon '(1 0 -2)))

(run '((-1  0  2)
       (2  -10  -7)
       (4  -8  8)
       (3  5  -1)))
