(in-package :advent)

;;;; Utils --------------------------------------------------------------------
(defmacro define-problem ((day part) (data-symbol reader) &body body)
  (let ((function-name (symb 'day- day '/ part)))
    `(defun ,function-name ()
       (let ((,data-symbol (,reader ,(format nil "data/2018/~2,'0D.txt" day))))
         ,@body))))


;;;; Problems -----------------------------------------------------------------
(define-problem (1 1) (data read-all-from-file)
  (summation data))

(define-problem (1 2) (data read-all-from-file)
  (setf (cdr (last data)) data) ; make data a circular list for easy looping
  (iterate
    (with seen = (make-hash-set :initial-contents '(0)))
    (for number :in data)
    (summing number :into frequency)
    (if (hset-contains-p seen frequency)
      (return frequency)
      (hset-insert! seen frequency))))
