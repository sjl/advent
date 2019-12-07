(defpackage :advent/2016/04 #.cl-user::*advent-use*)
(in-package :advent/2016/04)

(defun sort-predicate (a b)
  (destructuring-bind (ach . an) a
    (destructuring-bind (bch . bn) b
      (cond ((> an bn) t)
            ((< an bn) nil)
            (t (char< ach bch))))))

(defun checksum (name)
  (-<> name
    (remove #\- <>)
    frequencies
    alexandria:hash-table-alist
    (sort <> #'sort-predicate)
    (take 5 <>)
    (map 'string #'car <>)))

(defun rot (char n)
  (-<> char
    char-code
    (- <> (char-code #\a))
    (+ <> n)
    (mod <> 26)
    (+ <> (char-code #\a))
    code-char))

(defun decrypt-char (char id)
  (if (char= char #\-)
    #\space
    (rot char id)))

(defun decrypt (string id)
  (map 'string (lambda (c) (decrypt-char c id)) string))

(define-problem (2016 4) (data alexandria:read-stream-content-into-string)
    (158835 993)
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
