(advent:defpackage* :advent/2021/17 (:shadow x y))
(in-package :advent/2021/17)

(defstruct (p (:constructor p (&optional (x 0) (y 0))) (:conc-name nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct (box (:constructor box (xmin xmax ymin ymax)) (:conc-name nil))
  (xmin 0 :type fixnum)
  (xmax 0 :type fixnum)
  (ymin 0 :type fixnum)
  (ymax 0 :type fixnum))

(defun-inline pzerop (p) (and (zerop (x p)) (zerop (y p))))
(defun-inline p->complex (p) (complex (x p) (y p)))
(defun-inline p->char (p) (if (pzerop p) #\S #\#))
(defun-inline in-box-p (p box)
  (and (<= (xmin box) (x p) (xmax box))
       (<= (ymin box) (y p) (ymax box))))

(defun-inline step! (pos vel)
  (incf (x pos) (x vel))
  (incf (y pos) (y vel))
  (decf (x vel) (signum (x vel)))
  (decf (y vel)))

(defun simulate (vel target)
  (iterate (with v = (copy-p vel))
           (with p = (p 0 0))
           (step! p v)
           (maximizing (y p) :into height)
           (until (or (> (x p) (xmax target))
                      (< (y p) (ymin target))))
           (when (in-box-p p target)
             (return height))))

(defun solve (target)
  (iterate (for vx :from 0 :to (xmax target))
           (do-irange ((vy (ymin target) (- (ymin target))))
             (for height = (simulate (p vx vy) target))
             (when height
               (maximizing height :into part1)
               (counting t :into part2)))
           (returning part1 part2)))

(defun parse (stream &aux (line (read-line stream)))
  (ppcre:register-groups-bind ((#'parse-integer xmin xmax ymin ymax))
      ("target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)" line)
    (box xmin xmax ymin ymax)))

(define-problem (2021 17) (target parse) ()
  (solve target))

#; Scratch --------------------------------------------------------------------
