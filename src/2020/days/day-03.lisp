(advent:defpackage* :advent/2020/03)
(in-package :advent/2020/03)


(defun count-trees (world drow dcol)
  (destructuring-bind (rows cols) (array-dimensions world)
    (iterate (for row :from 0 :below rows :by drow)
             (for col :modulo cols :from 0 :by dcol)
             (counting (char= #\# (aref world row col))))))

(define-problem (2020 3) (data read-2d-array) (257 1744787392)
  (values (count-trees data 1 3)
          (* (count-trees data 1 1)
             (count-trees data 1 3)
             (count-trees data 1 5)
             (count-trees data 1 7)
             (count-trees data 2 1))))

#; Scratch --------------------------------------------------------------------


