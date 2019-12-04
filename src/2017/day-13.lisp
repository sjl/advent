(defpackage :advent/2017/13 #.cl-user::*advent-use*)
(in-package :advent/2017/13)

;; There's a magical insight that you need to get if you want to do this
;; problem: the length of a scanner of range R is `2(R-1)`.
;;
;; Examples:
;;
;;     R = 2: S.     1
;;            .S     2 = 2(R-1) = 2(2-1) = 2(1) = 2
;;
;;     R = 3: S..    1
;;            .S.    2
;;            ..S    3
;;            .S.    4 = 2(R-1) = 2(3-1) = 2(2) = 4
;;
;;     R = 4: S...   1
;;            .S..   2
;;            ..S.   3
;;            ...S   4
;;            ..S.   5
;;            .S..   6 = 2(R-1) = 2(4-1) = 2(3) = 6
;;
;; The best "intuition" I can come up with for this is:
;;
;; * We spend 1 turn at each end position.
;; * We spend 2 turns at each inner position.
;; * We have R positions, 2 of which are an end and (R-2) of which are inner.
;; * So we have (2 * 1) + ((R-2) * 2) = 2 + 2(R-2) turns.
;; * Factor out the 2 and simplify: 2(1 + (R-2)) = 2(R-1).
;;
;; Figuring out the exact position of a scanner at a given time it tricky
;; because you need to wrap the position around properly during the second half,
;; but we can skip that because all we care about is whether it's at zero.

(defun catchesp (range time)
  (zerop (mod time (* (1- range) 2))))

(defun make-scanners (specs)
  (iterate
    (with scanners = (make-array (1+ (caar (last specs))) :initial-element nil))
    (for (level range) :in specs)
    (setf (aref scanners level) range)
    (finally (return scanners))))

(defun severity (level range)
  (* level range))

(defun traverse (scanners start-time)
  (iterate
    (for level :from 0)
    (for time :from start-time)
    (for range :in-vector scanners)
    (for caught = (and range (catchesp range time)))
    (oring caught :into ever-caught)
    (when caught
      (summing (severity level range) :into score))
    (returning score ever-caught)))

(define-problem (2017 13) (data read-lines-of-numbers-and-garbage)
    (2604 3941460)
  (let ((scanners (make-scanners data)))
    (values
      (traverse scanners 0)
      (iterate
        (for delay :from 0)
        (for (values score caught) = (traverse scanners delay))
        (finding delay :such-that (not caught))))))
