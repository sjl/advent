(advent:defpackage* :advent/2019/09)
(in-package :advent/2019/09)

(define-problem (2019 9) (data read-numbers) (2955820355 46643)
  (values
    (first (gathering
             (advent/intcode:run data
                                 :input (constantly 1)
                                 :output #'gather)))
    (first (gathering
             (advent/intcode:run data
                                 :input (constantly 2)
                                 :output #'gather)))))


#; Scratch --------------------------------------------------------------------

(let ((advent/intcode:*trace* nil))
  (run '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)))

(let ((advent/intcode:*trace* nil))
  (run '(1102 34915192 34915192 7 4 7 99 0)))

(let ((advent/intcode:*trace* t))
  (run '(104 1125899906842624 99)))
