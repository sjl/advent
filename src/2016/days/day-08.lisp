(defpackage :advent/2016/08 #.cl-user::*advent-use*)
(in-package :advent/2016/08)

(defun make-screen ()
  (make-array (list 6 50) :initial-element 0))

(defun rotate-row (screen row &optional (n 1))
  (destructuring-bind (rows cols) (array-dimensions screen)
    (declare (ignore rows))
    (do-repeat n
      (let ((tmp (aref screen row (1- cols))))
        (dotimes (col cols)
          (rotatef tmp (aref screen row col)))))))

(defun rotate-col (screen col &optional (n 1))
  (destructuring-bind (rows cols) (array-dimensions screen)
    (declare (ignore cols))
    (do-repeat n
      (let ((tmp (aref screen (1- rows) col)))
        (dotimes (row rows)
          (rotatef tmp (aref screen row col)))))))

(defun rect (screen rows cols)
  (do-range ((row 0 rows)
             (col 0 cols))
    (setf (aref screen row col) 1)))

(defun parse-line (line)
  (destructuring-bind (command arg1 &optional arg2)
      (ppcre:split #\Space line :limit 3)
    (alexandria:eswitch (command :test #'string=)
      ("rect" (ppcre:register-groups-bind
                  ((#'parse-integer cols rows))
                  ("(\\d+)x(\\d+)" arg1)
                `(rect ,rows ,cols)))
      ("rotate" (ppcre:register-groups-bind
                    ((#'parse-integer i n))
                    ("\\w=(\\d+) by (\\d+)" arg2)
                  (alexandria:eswitch (arg1 :test #'string=)
                    ("row" `(rotate-row ,i ,n))
                    ("column" `(rotate-col ,i ,n))))))))

(defun draw-screen (screen)
  (draw-bitmap (iterate (for (x row col) :in-array screen)
                        (collect (cons (complex col row) x)))
               "out/2016-08.pbm"))

(define-problem (2016 8) (data read-lines) (110)
  (iterate
    (with screen = (make-screen))
    (for (function . args) :in (mapcar #'parse-line data))
    (apply function screen args)
    (returning
      (progn (draw-screen screen)
             (iterate
               (for x :across-flat-array screen)
               (counting (= 1 x)))))))


#; Scratch --------------------------------------------------------------------
