(advent:defpackage* :advent/2019/05)
(in-package :advent/2019/05)

(define-problem (2019 5) (data read-numbers) (14522484 4655956)
  (values
    (car (last (gathering
                 (advent/intcode:run data :input (constantly 1) :output #'gather))))
    (car (gathering
           (advent/intcode:run data :input (constantly 5) :output #'gather)))))
