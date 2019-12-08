(defpackage :advent/2017/08 #.cl-user::*advent-use*)
(in-package :advent/2017/08)

(defun == (x y) (= x y))
(defun != (x y) (/= x y))

(defun inc (delta) delta)
(defun dec (delta) (- delta))

(define-problem (2017 8) (data read-lines) (5215 6419)
  (let ((registers (make-hash-table)))
    (macrolet ((r (register) `(gethash ,register registers 0)))
      (iterate
        (for line :in data)
        (for (reg op delta nil cmp-reg cmp-op cmp-bound)
             := (let ((*package* (find-package :advent/2017/08)))
                  (read-all-from-string line)))
        (when (funcall cmp-op (r cmp-reg) cmp-bound)
          (maximizing (incf (r reg) (funcall op delta)) :into highest))
        (finally (return (values (alexandria:extremum (alexandria:hash-table-values registers) #'>) highest)))))))
