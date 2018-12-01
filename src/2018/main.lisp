(in-package :advent)

;;;; Utils --------------------------------------------------------------------
(defmacro define-problem ((day part) (data-symbol reader) &body body)
  (let ((function-name (symb 'day- day '/ part)))
    `(defun ,function-name ()
       (let ((,data-symbol (,reader ,(format nil "data/2018/~2,'0D.txt" day))))
         ,@body))))

;;;; Problems -----------------------------------------------------------------
(define-problem (1 1) (data read-all-from-file)
  (reduce #'+ data))

(define-problem (1 2) (data read-all-from-file)
  (let ((seen (make-hash-set :initial-contents '(0)))
        (frequency 0))
    (setf (cdr (last data)) data) ; make data a circular list for easy repetition
    (dolist (number data)
      (incf frequency number)
      (if (hset-contains-p seen frequency)
        (return frequency)
        (hset-insert! seen frequency)))))
