(defpackage :advent/2019/03 #.cl-user::*advent-use*)
(in-package :advent/2019/03)

(defun print-grid (grid)
  (multiple-value-bind (left right bottom top)
      (bounds (alexandria:hash-table-keys grid))
    (iterate
      (for y :from (1+ top) :downto (1- bottom))
      (iterate
        (for x :from (1- left) :to (1+ right))
        (princ (gethash (complex x y) grid #\.)))
      (terpri))))

(defun parse-path (string)
  (iterate
    (for ((#'first-character direction) (#'parse-integer distance))
         :matching "([UDLR])(\\d+)" :against string)
    (collect (cons direction distance))))

(defun delta (direction)
  (ecase direction
    (#\U #c( 0  1))
    (#\D #c( 0 -1))
    (#\L #c(-1  0))
    (#\R #c( 1  0))))

(defun place-wire (grid path label)
  (iterate
    (with scores = (make-hash-table))
    (with steps = 0)
    (with pos = #c(0 0))
    (for (direction . distance) :in path)
    (for delta = (delta direction))
    (iterate
      (repeat distance)
      (incf pos delta)
      (incf steps)
      (for cur = (gethash pos grid))
      (cond ((null cur) (setf (gethash pos grid) label ; never seen anything
                              (gethash pos scores) steps))
            ((char= cur label)) ; already seen
            ((char= cur #\X)) ; already seen
            (t (setf (gethash pos grid) #\X ; seen the other wire
                     (gethash pos scores) steps))))
    (finally (return scores))))

(defun find-intersections (grid)
  (iterate (for (k v) :in-hashtable grid)
           (when (eql #\X v) (collect k))))

(defun make-grid ()
  (let-result (grid (make-hash-table))
    (setf (gethash #c(0 0) grid) #\o)))

(define-problem (2019 3) (data read-lines) (5357 101956)
  (let* ((path1 (parse-path (first data)))
         (path2 (parse-path (second data)))
         (grid (make-grid))
         (scores1 (place-wire grid path1 #\1))
         (scores2 (place-wire grid path2 #\2))
         (intersections (find-intersections grid)))
    (flet ((intersection-cost (point)
             (+ (gethash point scores1)
                (gethash point scores2))))
      (values
        (alexandria:extremum (mapcar #'manhattan-distance intersections) #'<)
        (alexandria:extremum (mapcar #'intersection-cost intersections) #'<)))))

;; (run '("R8,U5,L5,D3" "U7,R6,D4,L4"))
;; (run '(
;; "R75,D30,R83,U83,L12,D49,R71,U7,L72"
;; "U62,R66,U55,R34,D71,R55,D58,R83"
;;        ))

;; (run '(
;; "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
;; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

;;        ))
