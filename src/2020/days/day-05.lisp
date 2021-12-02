(advent:defpackage* :advent/2020/05)
(in-package :advent/2020/05)


(defun avg (x y)
  (/ (+ x y) 2))

(defun id (row col)
  (+ (* 8 row) col))

(defun decode (spec)
  (iterate (with rlo = 0)
           (with rhi = 128)
           (with clo = 0)
           (with chi = 8)
           (for ch :in-string spec)
           (ecase ch
             (#\F (setf rhi (avg rlo rhi)))
             (#\B (setf rlo (avg rlo rhi)))
             (#\L (setf chi (avg clo chi)))
             (#\R (setf clo (avg clo chi))))
    (returning (id rlo clo))))

(defun find-missing-seat (ids)
  (iterate (for id :in-vector ids)
           (for pid :previous id)
           (when (and pid (/= (1+ pid) id))
             (return (1+ pid)))))

(define-problem (2020 5) (data read-lines) (974 646)
  (let ((ids (sort (map 'vector #'decode data) #'<)))
    (values
      (aref ids (1- (length ids)))
      (find-missing-seat ids))))

#; Scratch --------------------------------------------------------------------


