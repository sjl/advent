(defpackage :advent/2017/10 #.cl-user::*advent-use*)
(in-package :advent/2017/10)

(defun reverse-chunk (vector start length)
  (iterate
    (repeat (truncate length 2))
    (for i :modulo (length vector) :from start)
    (for j :modulo (length vector) :downfrom (+ start (1- length)))
    (rotatef (aref vector i) (aref vector j))))

(defun simple-knot-hash (lengths)
  (iterate
    (with numbers = (coerce (alexandria:iota 256) 'vector))
    (with current = 0)
    (with skip = 0)
    (for length :in lengths)
    (reverse-chunk numbers current length)
    (zapf current (mod (+ % length skip) 256))
    (incf skip)
    (finally (return (* (aref numbers 0) (aref numbers 1))))))

(defun sparse->dense (numbers)
  (iterate
    (for i :from 0 :by 16 :below (length numbers))
    (collect (reduce #'logxor numbers :start i :end (+ i 16)))))

(defun bytes->hex (bytes)
  (format nil "~(~{~2,'0X~}~)" bytes))

(defun initial-lengths (string)
  (append (map 'list #'char-code string)
          (list 17 31 73 47 23)))

(defun full-knot-hash (string)
  (iterate
    (with lengths = (initial-lengths string))
    (with numbers = (coerce (alexandria:iota 256) 'vector))
    (with current = 0)
    (with skip = 0)
    (repeat 64)
    (iterate
      (for length :in lengths)
      (reverse-chunk numbers current length)
      (zapf current (mod (+ % length skip) 256))
      (incf skip))
    (finally (return (bytes->hex (sparse->dense numbers))))))

(define-problem (2017 10) (data alexandria:read-stream-content-into-string) ()
  (values
    (simple-knot-hash (read-numbers-from-string data))
    (full-knot-hash (str:trim data))))

