(advent:defpackage* :advent/2018/10
  (:shadow :x :y :bounds))
(in-package :advent/2018/10)

(defun parse-line (line)
  (destructuring-bind (x y vx vy) line
    (cons (complex x y)
          (complex vx vy))))

(defun x (star)
  (realpart (car star)))

(defun y (star)
  (imagpart (car star)))

(defun tick (stars)
  (dolist (star stars)
    (incf (car star) (cdr star))))

(defun bounds (stars)
  (values (x (extremum stars '< :key #'x)) ; left
          (x (extremum stars '> :key #'x)) ; right
          (y (extremum stars '< :key #'y)) ; bottom
          (y (extremum stars '> :key #'y)))) ; top

(defun field-size (stars)
  (multiple-value-bind (left right bottom top)
      (bounds stars)
    (* (- right left) (- top bottom))))

(defun draw (stars)
  (multiple-value-bind (left right bottom top) (bounds stars)
    (let* ((height (1+ (- top bottom)))
           (width (1+ (- right left)))
           (field (make-array height)))
      (do-array (line field)
        (setf line (make-string width :initial-element #\space)))
      (dolist (star stars)
        (setf (aref (aref field (- (y star) bottom))
                    (- (x star) left))
              #\*))
      (map nil #'write-line field))))

(define-problem (2018 10) (data read-lines-of-numbers-and-garbage)
    () ;; This can't really be tested automatically :(
  (iterate
    (with stars = (mapcar #'parse-line data))
    (with ticks = 0)
    (initially (iterate
                 (tick stars)
                 (incf ticks)
                 (until (< (field-size stars) 3000))))
    (format t "After tick ~D:~%" ticks)
    (draw stars)
    (until (string= "q" (read-line)))
    (tick stars)))
