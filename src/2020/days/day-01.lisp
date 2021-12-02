(advent:defpackage* :advent/2020/01)
(in-package :advent/2020/01)


(defun find-two-addends (seq sum)
  (iterate (for x :in-vector seq :with-index i)
           (iterate (for y :in-vector seq :from (1+ i))
                    (when (= sum (+ x y))
                      (return-from find-two-addends (values x y))))))

(defun find-three-addends (seq sum)
  (iterate (for x :in-vector seq :with-index i)
           (iterate (for y :in-vector seq :from (1+ i) :with-index j)
                    (iterate (for z :in-vector seq :from (1+ j))
                             (when (= sum (+ x y z))
                               (return-from find-three-addends (values x y z)))))))

(define-problem (2020 1) (data read-numbers) (928896 295668576)
  (setf data (coerce data 'vector))
  (values (multiple-value-call #'* (find-two-addends data 2020))
          (multiple-value-call #'* (find-three-addends data 2020))))

#; Scratch --------------------------------------------------------------------


