(defpackage :advent/2018/11 #.cl-user::*advent-use*)
(in-package :advent/2018/11)
(named-readtables:in-readtable :interpol-syntax)

(defun cell (x y)
  (complex x y))

(defun x (cell)
  (realpart cell))

(defun y (cell)
  (imagpart cell))

(defun rack-id (cell)
  (+ (x cell) 10))

(defun power-level (serial-number cell)
  (-<> (rack-id cell)
    (* <> (y cell))
    (+ <> serial-number)
    (* <> (rack-id cell))
    (nth-digit 2 <>)
    (- <> 5)))

(define-problem (2018 11) (serial-number read)
  (let ((totals (make-array (list 300 300))))
    (flet ((gref (x y)
             (let ((x (1- x))
                   (y (1- y)))
               (if (array-in-bounds-p totals x y)
                 (aref totals x y)
                 0)))
           ((setf gref) (value x y)
            (setf (aref totals (1- x) (1- y)) value)))
      (iterate (for-nested ((x :from 300 :downto 1)
                            (y :from 300 :downto 1)))
               (setf (gref x y)
                     (+ (power-level serial-number (cell x y))
                        (gref (1+ x) y)
                        (gref x (1+ y))
                        (- (gref (1+ x) (1+ y))))))
      (labels ((square-power (x y n)
                 (let ((xn (+ x n))
                       (yn (+ y n)))
                   (+ (gref x y)
                      (- (gref xn y))
                      (- (gref x yn))
                      (gref xn yn))))
               (largest-square (n)
                 (iterate
                   (for-nested ((x :from 1 :to (- 301 n))
                                (y :from 1 :to (- 301 n))))
                   (for power = (square-power x y n))
                   (finding (list x y power) :maximizing power))))
        (values (subseq (largest-square 3) 0 2)
                (iterate (for n :from 1 :to 300)
                         (for (x y power) = (largest-square n))
                         (finding (list x y n) :maximizing power)))))))

