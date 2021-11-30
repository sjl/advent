(advent:defpackage* :advent/2017/10)
(in-package :advent/2017/10)

(define-problem (2017 10) (data alexandria:read-stream-content-into-string)
    (19591 "62e2204d2ca4f4924f6e7a80f1288786")
  (values
    (advent/knot-hash:simple-knot-hash (read-numbers-from-string data))
    (advent/knot-hash:full-knot-hash (str:trim data))))

