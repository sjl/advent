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

(defun hamming-distance (sequence1 sequence2 &key (test #'eql))
  "Return the Hamming distance between `sequence1` and `sequence2`."
  ;; todo assert length=?
  (let ((result 0))
    (map nil (lambda (x y)
               (unless (funcall test x y)
                 (incf result)))
         sequence1
         sequence2)
    result))

(defun unique (list &key (test #'eql))
  "Return a fresh list of the unique elements in `LIST`.

  This differs from REMOVE-DUPLICATES in that *all* duplicate elements will be
  removed, not just all-but-the-last.

  This is O(n²).

  Example:

    (remove-duplicates '(3 1 3 2 3))
    ; => (1 2 3)

    (unique '(3 1 3 2 3))
    ; => (1 2)

  "
  (iterate
    (for a :in list)
    (for i :from 0)
    (unless (iterate (for b :in list)
                     (for j :from 0)
              (unless (= i j)
                (thereis (funcall test a b))))
      (collect a))))

(defun extremum+ (sequence predicate)
  "Like ALEXANDRIA:EXTREMUM but also return the position as a second value."
  (iterate
    (with value = nil)
    (with position = nil)
    (for i :from 0)
    (for v :in-whatever sequence)
    (if-first-time
      (setf value v
            position i)
      (when (funcall predicate v value)
        (setf value v
              position i)))
    (finally (return (values value position)))))
