(in-package :advent)

;;;; Utils --------------------------------------------------------------------
(defun read-file-of-digits (path)
  (-<> path
    read-file-into-string
    (map 'list #'digit-char-p <>)
    (remove nil <>)))


;;;; Problems -----------------------------------------------------------------
(defun day-1-part-1 ()
  (iterate (for (x . y) :pairs-of-list (read-file-of-digits "data/2017/01-1"))
           (when (= x y)
             (sum x))))

(defun day-1-part-2 ()
  (iterate
    (with data = (coerce (read-file-of-digits "data/2017/01-1") 'vector))
    (with length = (length data))
    (with offset = (truncate length 2))
    (for x :in-vector data :with-index i)
    (for y = (aref data (mod (+ i offset) length)))
    (when (= x y)
      (sum x))))
