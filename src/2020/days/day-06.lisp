(advent:defpackage* :advent/2020/06)
(in-package :advent/2020/06)

(defun yes-counts (group)
  (let ((answers (mapcar (rcurry #'coerce 'list) group)))
    (cons (length (reduce #'union answers))
          (length (reduce #'intersection answers)))))

(define-problem (2020 6) (data read-chunks) (7110 3628)
  (let ((counts (mapcar #'yes-counts data)))
    (values (summation counts :key #'car)
            (summation counts :key #'cdr))))

#; Scratch --------------------------------------------------------------------


