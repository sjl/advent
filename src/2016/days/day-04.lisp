(advent:defpackage* :advent/2016/04)
(in-package :advent/2016/04)

(defun sort-predicate (a b)
  (destructuring-bind (ach . an) a
    (destructuring-bind (bch . bn) b
      (cond ((> an bn) t)
            ((< an bn) nil)
            (t (char< ach bch))))))

(defun checksum (name)
  (_ name
    (remove #\- _)
    frequencies
    alexandria:hash-table-alist
    (sort _ #'sort-predicate)
    (take 5 _)
    (map 'string #'car _)))

(defun rot (char n)
  (_ char
    char-code
    (- _ (char-code #\a))
    (+ _ n)
    (mod _ 26)
    (+ _ (char-code #\a))
    code-char))

(defun decrypt-char (char id)
  (if (char= char #\-)
    #\space
    (rot char id)))

(defun decrypt (string id)
  (map 'string (lambda (c) (decrypt-char c id)) string))

(define-problem (2016 4) (data alexandria:read-stream-content-into-string)
    (245102 324)
  (iterate
    (for (name (#'parse-integer id) checksum)
         :matching "([\\w-]+)-(\\d+)\\[(\\w+)\\]"
         :against data)
    (when (string= checksum (checksum name))
      (summing id :into part1))
    (finding-first id
      :such-that (string= (decrypt name id) "northpole object storage")
      :into part2)
    (returning part1 part2)))


#; Scratch --------------------------------------------------------------------
