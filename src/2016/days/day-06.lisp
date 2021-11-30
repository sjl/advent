(advent:defpackage* :advent/2016/06)
(in-package :advent/2016/06)

(defun freqs (lines)
  (iterate
    (for i :from 0 :below (length (first lines)))
    (collect (frequencies lines :key (rcurry #'elt i)))))

(defun freq-extrema (predicate freqs)
  (_ freqs
     alexandria:hash-table-alist
     (extrema predicate _ :key #'cdr)
     car))

(define-problem (2016 6) (data read-lines) ("nabgqlcw" "ovtrjcjh")
  (values
    (map 'string (curry #'freq-extrema #'>) (freqs data))
    (map 'string (curry #'freq-extrema #'<) (freqs data))))



#; Scratch --------------------------------------------------------------------
