(advent:defpackage* :advent/2018/06)
(in-package :advent/2018/06)
(named-readtables:in-readtable :interpol-syntax)

(defun parse-line (line)
  (apply #'complex (mapcar #'parse-integer (str:split ", " line))))

(defun closest (point coordinates)
  (let ((results (extremums coordinates '<
                            :key (curry #'manhattan-distance point))))
    (case (length results)
      (1 (car results))
      (t nil))))

(define-problem (2018 6) (data read-lines) (3420 46667)
  (let* ((coordinates (mapcar #'parse-line data))
         (xs (mapcar #'realpart coordinates))
         (ys (mapcar #'imagpart coordinates))
         (left (alexandria:extremum xs #'<))
         (bottom (alexandria:extremum ys #'<))
         (right (alexandria:extremum xs #'>))
         (top (alexandria:extremum ys #'>))
         (counts (make-hash-table))
         (infinite (make-hash-set)))
    (iterate
      (for-nested ((x :from left :to right)
                   (y :from bottom :to top)))
      (for closest = (closest (complex x y) coordinates))
      (when closest
        (incf (gethash closest counts 0))
        (when (or (= left x) (= bottom y)
                  (= right x) (= top y))
          (hset-insert! infinite closest))))
    (values
      (iterate
        (for (point size) :in-hashtable counts)
        (unless (hset-contains-p infinite point)
          (maximizing size)))
      (iterate
        (for-nested ((x :from left :to right)
                     (y :from bottom :to top)))
        (for point = (complex x y))
        (for total-distance = (summation coordinates :key (curry #'manhattan-distance point)))
        (counting (< total-distance 10000))))))
