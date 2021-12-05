(advent:defpackage* :advent/2020/12)
(in-package :advent/2020/12)

(defun parse (stream)
  (iterate (for line :in-stream stream :using #'read-line)
           (ppcre:register-groups-bind (ch n)
               ("([NSEWLRF])([0-9]+)" line)
             (collect (cons (char ch 0) (parse-integer n))))))

(defun north (n) (* n #c( 0  1)))
(defun south (n) (* n #c( 0 -1)))
(defun east  (n) (* n #c( 1  0)))
(defun west  (n) (* n #c(-1  0)))
(defun rot   (n) (expt #c(0 1) (/ n 90)))

(defun part1 (path)
  (iterate (with pos = #c(0 0))
           (with heading = #c(1 0))
           (for (action . n) :in path)
           (ecase action
             (#\N (incf pos (north n)))
             (#\S (incf pos (south n)))
             (#\E (incf pos (east n)))
             (#\W (incf pos (west n)))
             (#\L (mulf heading (rot n)))
             (#\R (divf heading (rot n)))
             (#\F (incf pos (* heading n))))
           (returning pos)))

(defun part2 (path)
  (iterate (with waypoint = #c(10 1))
           (with pos = #c(0 0))
           (for (action . n) :in path)
           (ecase action
             (#\N (incf waypoint (north n)))
             (#\S (incf waypoint (south n)))
             (#\E (incf waypoint (east n)))
             (#\W (incf waypoint (west n)))
             (#\L (mulf waypoint (rot n)))
             (#\R (divf waypoint (rot n)))
             (#\F (incf pos (* waypoint n))))
           (returning pos)))

(define-problem (2020 12) (data parse) (1106 107281)
  (values (manhattan-distance (part1 data))
          (manhattan-distance (part2 data))))


#; Scratch --------------------------------------------------------------------
