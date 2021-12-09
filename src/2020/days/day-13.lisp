(advent:defpackage* :advent/2020/13)
(in-package :advent/2020/13)

(defun parse (stream)
    (cons (parse-integer (read-line stream))
            (_ (read-line stream)
              (str:split "," _)
              (remove "x" _ :test #'string=)
              (mapcar #'parse-integer _))))

(defun part1 (n buses)
  (iterate (for b :in buses)
           (for wait = (- (mod n (- b))))
           (finding (* b wait) :minimizing wait)))

(define-problem (2020 13) (data parse) (174)
  (destructuring-bind (n . buses) data
      (values (part1 n buses))))


#; Scratch --------------------------------------------------------------------
