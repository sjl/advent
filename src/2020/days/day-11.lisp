(advent:defpackage* :advent/2020/11)
(in-package :advent/2020/11)

(defun sref (seats row col)
  (when (array-in-bounds-p seats row col)
    (aref seats row col)))

(defun count-neighbors (seats row col)
  (iterate (for (r c) :within-radius 1 :origin (row col) :skip-origin t)
           (counting (eql #\# (sref seats r c)))))

(defun count-visible (seats row col &aux (result 0))
  (do-irange ((dr -1 1)
              (dc -1 1))
    (unless (= 0 dr dc)
      (iterate (for r :from (+ row dr) :by dr)
               (for c :from (+ col dc) :by dc)
               (for ch = (sref seats r c))
               (ecase ch
                 ((#\L nil) (return))
                 (#\# (progn (incf result) (return)))
                 (#\.)))))
  result)

(defun simulate (seats &key neighbors crowded)
  (let ((seats (alexandria:copy-array seats))
        (next (alexandria:copy-array seats)))
    (loop (iterate
            (with changed = nil)
            (for (current row col) :in-array seats)
            (unless (char= current #\.)
              (for n = (funcall neighbors seats row col))
              (setf (aref next row col)
                    (ecase current
                      (#\L (if (= n 0)
                             (progn (setf changed t) #\#)
                             #\L))
                      (#\# (if (>= n crowded)
                             (progn (setf changed t) #\L)
                             #\#)))))
            (finally
              (rotatef seats next)
              (when (not changed) (return-from simulate seats)))))))

(defun count-total (seats)
  (iterate (for ch :across-flat-array seats) (counting (char= ch #\#))))

(define-problem (2020 11) (data read-2d-array) (2261 2039)
  (values (count-total (simulate data :neighbors #'count-neighbors :crowded 4))
          (count-total (simulate data :neighbors #'count-visible :crowded 5))))


#; Scratch --------------------------------------------------------------------
