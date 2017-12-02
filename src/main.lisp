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

(defun read-file-of-lines-of-numbers (path)
  (iterate (for line :in-file path :using #'read-line)
           (collect (mapcar #'parse-integer (str:words line)))))


;;;; Problems -----------------------------------------------------------------
(defun day-1-part-1 ()
  (iterate (for (x . y) :pairs-of-list (read-file-of-digits "data/2017/01"))
           (when (= x y)
             (sum x))))

(defun day-1-part-2 ()
  (iterate
    (with data = (coerce (read-file-of-digits "data/2017/01") 'vector))
    (with length = (length data))
    (for x :in-vector data)
    (for iy :modulo length :from (truncate length 2))
    (for y = (aref data iy))
    (when (= x y)
      (sum x))))


(defun day-2-part-1 ()
  (flet ((checksum (line)
           (- (apply #'max line)
              (apply #'min line))))
    (summation (remove nil (read-file-of-lines-of-numbers "data/2017/02"))
               :key #'checksum)))

(defun day-2-part-2 ()
  (labels ((validp (a b)
             (dividesp (max a b) (min a b)))
           (head-valid-p (list)
             (some (curry #'validp (car list))
                   (cdr list)))
           (checksum (line)
             (somelist #'head-valid-p line)))
    (summation (remove nil (read-file-of-lines-of-numbers "data/2017/02"))
               :key #'checksum)))
