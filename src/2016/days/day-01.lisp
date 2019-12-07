(defpackage :advent/2016/01 #.cl-user::*advent-use*)
(in-package :advent/2016/01)


(defun turn (direction heading)
  (* heading (ecase direction
               (#\L #c(0 1))
               (#\R #c(0 -1)))))

(define-problem (2016 1) (data read-line) (298 158)
  (iterate
    (with pos = #c(0 0))
    (with seen = (make-hash-set :initial-contents (list #c(0 0))))
    (for ((#'first-character direction) (#'parse-integer distance))
         :matching "([LR])(\\d+)" :against data)
    (for heading :seed #c(0 1) :then (turn direction heading))
    (do-repeat distance
      (incf pos heading)
      (finding-first pos :such-that (hset-contains-p seen pos) :into part2)
      (hset-insert! seen pos))
    (returning (manhattan-distance pos)
               (manhattan-distance part2))))


#; Scratch --------------------------------------------------------------------

