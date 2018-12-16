(defpackage :advent/2018/08 #.cl-user::*advent-use*)
(in-package :advent/2018/08)
(named-readtables:in-readtable :interpol-syntax)

(defstruct (node (:conc-name nil))
  children metadata)

(defun read-node (stream)
  (let ((children-count (read stream))
        (metadata-count (read stream)))
    (make-node :children (iterate
                           (repeat children-count)
                           (collect (read-node stream) :result-type vector))
               :metadata (iterate
                           (repeat metadata-count)
                           (collect (read stream))))))

(defun node-value (node &aux (children (children node)))
  (if (emptyp children)
    (summation (metadata node))
    (iterate
      (for meta :in (metadata node))
      (for index = (1- meta))
      (when (array-in-bounds-p children index)
        (summing (node-value (aref children index)))))))

(define-problem (2018 8) (data)
  (let ((root (read-node data)))
    (values
      (recursively ((node root))
        (+ (summation (metadata node))
           (summation (children node) :key #'recur)))
      (node-value root))))

