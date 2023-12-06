(advent:defpackage* :advent/2022/02)
(in-package :advent/2022/02)

(defclass* reveal ()
  ((r :initform 0)
   (g :initform 0)
   (b :initform 0)))

(defmethod print-object ((o reveal) s)
  (print-unreadable-object (o s :type nil)
    (format s "~D/~D/~D" (r o) (g o) (b o))))

(defclass* game ()
  (id reveals))

(defmethod print-object ((o game) s)
  (print-unreadable-object (o s :type t)
    (format s "~D: ~A" (id o) (reveals o))))

(defun parse-reveal (reveal)
  (let ((cubes (mapcar #'str:trim (str:split #\, reveal))))
    (loop :with reveal = (make-instance 'reveal)
          :for cube :in cubes
          :for (n color) = (str:split #\space cube)
          :do (setf (slot-value reveal (alexandria:eswitch (color :test #'string=)
                                         ("red" 'r)
                                         ("green" 'g)
                                         ("blue" 'b)))
                    (parse-integer n))
          :finally (return reveal))))

(defun parse-reveals (reveals)
  (mapcar #'parse-reveal (str:split #\; reveals)))

(defun parse-line (line)
  (destructuring-bind (game reveals) (str:split #\: line)
    (make-instance 'game :id (parse-integer (second (str:split #\space game)))
                   :reveals (parse-reveals reveals))))

(defun reveal-possible-p (r g b reveal)
  (and (<= (r reveal) r)
       (<= (g reveal) g)
       (<= (b reveal) b)))

(defun game-possible-p (r g b game)
  (every (curry #'reveal-possible-p r g b) (reveals game)))

(defun game-min-cubes (game)
  (list (reduce #'max (reveals game) :key #'r)
        (reduce #'max (reveals game) :key #'g)
        (reduce #'max (reveals game) :key #'b)))

(defun game-power (game)
  (apply #'* (game-min-cubes game)))

(define-problem (2023 2) (data read-lines) (2600)
  (let ((games (mapcar #'parse-line data)))
    (values (summation (remove-if-not (curry #'game-possible-p 12 13 14) games)
                       :key #'id)
            (summation games :key #'game-power))))

#; Scratch --------------------------------------------------------------------

(result (mapcar #'part-2
        '("two1nineight"
          "eightwothree"
          "abcone2threexyz"
          "xtwone3four"
          "4nineeightseven2"
          "zoneight234"
          "7pqrstsixteen")))



