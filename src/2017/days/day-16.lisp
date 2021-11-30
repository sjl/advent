(advent:defpackage* :advent/2017/16)
(in-package :advent/2017/16)

(defparameter *initial* "abcdefghijklmnop")

(defun spin (dancers n)
  (alexandria:rotate dancers n))

(defun exchange (dancers i j)
  (rotatef (aref dancers i)
           (aref dancers j)))

(defun partner (dancers a b)
  (rotatef (aref dancers (position a dancers))
           (aref dancers (position b dancers))))

(defun dance (dancers steps &key (repeat 1))
  (do-repeat repeat
    (iterate
      (for step :in steps)
      (ecase (char step 0)
        (#\s (spin dancers (parse-integer step :start 1)))
        (#\x (destructuring-bind (i j) (read-numbers-from-string step)
               (exchange dancers i j)))
        (#\p (partner dancers (char step 1) (char step 3))))))
  dancers)

(defun find-cycle (steps)
  (iterate
    (with dancers = (copy-seq *initial*))
    (dance dancers steps)
    (counting t)
    (until (string= dancers *initial*))))

(define-problem (2017 16) (data read-comma-separated-values)
    ("ionlbkfeajgdmphc" "fdnphiegakolcmjb")
  (values
    (dance (copy-seq *initial*) data)
    (dance (copy-seq *initial*) data :repeat (mod 1000000000 (find-cycle data)))))
