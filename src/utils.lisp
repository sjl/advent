(in-package :advent)

;;;; Problems -----------------------------------------------------------------
(defmacro define-problem ((year day &optional part)
                          (data-symbol reader)
                          &body body)
  (let ((function-name (if part
                         (symb 'advent- year '- day '/ part)
                         (symb 'advent- year '- day))))
    `(defun ,function-name ()
       (let ((,data-symbol (,reader ,(format nil "data/~D/~2,'0D.txt" year day))))
         ,@body))))


;;;; Readers ------------------------------------------------------------------
(defun read-form-from-file (path)
  "Read the first form from `path`."
  (with-open-file (s path)
    (read s)))

(defun read-lines-from-file (path)
  "Read the lines in `path` into a list of strings."
  (iterate (for line :in-file path :using #'read-line)
           (collect line)))


(defun read-file-of-digits (path)
  "Read all the ASCII digits in `path` into a list of integers.

  Any character in the file that's not an ASCII digit will be silently ignored.

  "
  (-<> path
    read-file-into-string
    (map 'list #'digit-char-p <>)
    (remove nil <>)))

(defun read-file-of-lines-of-numbers (path)
  "Read the lines of numbers in `path` into a list of lists of numbers.

  Each line must consist of whitespace-separated integers.  Empty lines will be
  discarded.

  "
  (iterate (for line :in-file path :using #'read-line)
           (for numbers = (mapcar #'parse-integer (str:words line)))
           (when numbers
             (collect numbers))))

(defun read-file-of-lines-of-words (path)
  (iterate (for line :in-file path :using #'read-line)
           (collect (str:words line))))


;;;; Miscellaneous ------------------------------------------------------------
(defun hash-table= (h1 h2 &optional (test #'eql))
  "Return whether `h1` and `h2` have the same keys and values.

  The consequences are undefined if `h1` and `h2` use different key tests.

  `test` is used to compare values.

  "
  (and (= (hash-table-count h1)
          (hash-table-count h2))
       (iterate (for (k v) :in-hashtable h1)
                (always (funcall test v (gethash k h2))))))

