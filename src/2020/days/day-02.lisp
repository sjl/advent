(advent:defpackage* :advent/2020/02)
(in-package :advent/2020/02)


(defun parse-line (line)
  (ppcre:register-groups-bind
      ((#'parse-integer i j) char password)
      ("(\\d+)-(\\d+) (.): (.+)" line)
    (values i j (coerce char 'character) password)))

(defun validp-1 (min max char password)
  (<= min (count char password) max))

(defun validp-2 (i j char password)
  (alexandria:xor (char= char (aref password (1- i)))
                  (char= char (aref password (1- j)))))

(define-problem (2020 2) (data) (655 673)
  (iterate (for line :in-stream data :using #'read-line)
           (for (values i j char password) = (parse-line line))
           (counting (validp-1 i j char password) :into p1)
           (counting (validp-2 i j char password) :into p2)
           (returning p1 p2)))

#; Scratch --------------------------------------------------------------------


