(in-package :advent)

;;;; Utils --------------------------------------------------------------------
(defun read-file-of-digits (path)
  "Read all the ASCII digits in `path` into a list of integers.

  Any character in the file that's not an ASCII digit will be silently ignored.

  "
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
    (for x :in-vector data)
    (for iy :modulo length :from (truncate length 2))
    (for y = (aref data iy))
    (when (= x y)
      (sum x))))
