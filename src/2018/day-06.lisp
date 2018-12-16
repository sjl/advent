(defpackage :advent/2018/06 #.cl-user::*advent-use*)
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

(define-problem (2018 6) (data read-lines)
  (let* ((coordinates (mapcar #'parse-line data))
         (xs (mapcar #'realpart coordinates))
         (ys (mapcar #'imagpart coordinates))
         (left (extremum xs #'<))
         (bottom (extremum ys #'<))
         (right (extremum xs #'>))
         (top (extremum ys #'>))
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


(1am:test test-2018/06
  (multiple-value-bind (part1 part2) (run)
    (1am:is (= 3420 part1))
    (1am:is (= 46667 part2))))
