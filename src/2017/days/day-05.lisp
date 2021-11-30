(advent:defpackage* :advent/2017/05)
(in-package :advent/2017/05)

(defun compute (data modification-function)
  (iterate
    (with maze = (fresh-vector data))
    (with bound = (1- (length maze)))
    (with address = 0)
    (while (<= 0 address bound))
    (counting t)
    (for offset = (aref maze address))
    (callf (aref maze address) modification-function)
    (incf address offset)))

(define-problem (2017 5) (data read-all) (342669 25136209)
  (values
    (compute data #'1+)
    (compute data (lambda (value) (+ value (if (>= value 3) -1 1))))))


