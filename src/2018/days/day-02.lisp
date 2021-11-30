(advent:defpackage* :advent/2018/02)
(in-package :advent/2018/02)


(define-problem (2018 2) (data read-lines) (8296 "pazvmqbftrbeosiecxlghkwud")
  (values
    (let* ((freqs (mapcar #'frequencies data))
           (counts (mapcar #'alexandria:hash-table-values freqs)))
      (* (count 2 counts :test #'member)
         (count 3 counts :test #'member)))
    ;; just brute force it
    (multiple-value-bind (a b)
        (iterate
          (for (a . remaining) :on data)
          (for b = (find 1 remaining :key (curry #'hamming-distance a)))
          (when b
            (return (values a b))))
      (let ((i (mismatch a b)))
        (str:concat (subseq a 0 i)
                    (subseq a (1+ i)))))))
