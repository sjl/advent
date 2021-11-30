(advent:defpackage* :advent/2018/05)
(in-package :advent/2018/05)
(named-readtables:in-readtable :interpol-syntax)

(defun reactivep (x y)
  (char= x (char-invertcase y)))

(defun react (string &aux result)
  (doseq (char string)
    (if (and result (reactivep char (car result)))
      (pop result)
      (push char result)))
  (coerce (nreverse result) 'string))

(define-problem (2018 5) (data alexandria:read-stream-content-into-string)
    (10708 5330)
  (setf data (delete #\newline data))
  (values
    (length (react data))
    (iterate
      (for unit :in-vector (remove-duplicates data :test #'char-equal))
      (for candidate = (react (remove unit data :test #'char-equal)))
      (minimizing (length candidate)))))
