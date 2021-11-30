(advent:defpackage* :advent/2016/02)
(in-package :advent/2016/02)

(defparameter *pad* 1)

(defparameter *pads*
  #(nil
    ((#c(-1  1) . #\1)   (#c(0  1) . #\2)   (#c(1  1) . #\3)
     (#c(-1  0) . #\4)   (#c(0  0) . #\5)   (#c(1  0) . #\6)
     (#c(-1 -1) . #\7)   (#c(0 -1) . #\8)   (#c(1 -1) . #\9))

    (                                        (#c(0  2) . #\1)
                         (#c(-1  1) . #\2)   (#c(0  1) . #\3)   (#c(1  1) . #\4)
     (#c(-2  0) . #\5)   (#c(-1  0) . #\6)   (#c(0  0) . #\7)   (#c(1  0) . #\8)   (#c(2  0) . #\9)
                         (#c(-1 -1) . #\A)   (#c(0 -1) . #\B)   (#c(1 -1) . #\C)
                                             (#c(0 -2) . #\D))))

(defun delta (direction)
  (ecase direction
    (#\U #c( 0  1))
    (#\D #c( 0 -1))
    (#\L #c(-1  0))
    (#\R #c( 1  0))))

(defun validp (position)
  (member position (aref *pads* *pad*) :key #'car))

(defun move (position direction)
  (let ((next (+ position (delta direction))))
    (if (validp next)
      next
      position)))

(defun button (position)
  (cdr (assoc position (aref *pads* *pad*))))

(defun find-button (directions start)
  (iterate
    (for dir :in-string directions)
    (for pos :seed start :then (move pos dir))
    (returning (button pos) pos)))

(defun find-code (lines)
  (iterate
    (with pos = 0)
    (for directions :in lines)
    (for (values button next) = (find-button directions pos))
    (setf pos next)
    (collect button :result-type 'string)))

(define-problem (2016 2) (data read-lines) ("95549" "D87AD")
  (values (let ((*pad* 1)) (find-code data))
          (let ((*pad* 2)) (find-code data))))

#; Scratch --------------------------------------------------------------------

(run '("ULL"
       "RRDDD"
       "LURDL"
       "UUUUD"))

(run '("LURDL"))
