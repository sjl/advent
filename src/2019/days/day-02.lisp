(advent:defpackage* :advent/2019/02)
(in-package :advent/2019/02)

(define-problem (2019 2) (data read-numbers) (3790689 6533)
  (let ((program (fresh-vector data)))
    (flet ((run (a b)
             (setf (aref program 1) a
                   (aref program 2) b)
             (advent/intcode:run program)))
      (values
        (run 12 2)
        (iterate
          (for-nested ((a :from 0 :to 99)
                       (b :from 0 :to 99)))
          (when (= 19690720 (run a b))
            (return (+ (* 100 a) b))))))))

