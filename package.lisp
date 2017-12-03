(defpackage :advent
  (:use :cl :losh :iterate :advent.quickutils))

(defpackage :advent.spiral
  (:use :cl :losh :iterate :advent.quickutils)
  (:export :number-coordinates))
