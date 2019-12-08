(defpackage :advent/2016/05 #.cl-user::*advent-use*)
(in-package :advent/2016/05)

(defparameter *fancy* nil)
(defparameter *spinner* '#1=(#\◴ #\◷ #\◶ #\◵ . #1#))

(defun string->bytes (string)
  (map 'vector #'char-code string))

(defun progress (i)
  (when *fancy*
    (format t "~C~C  Iteration: ~D" #\Return (pop *spinner*) i))
  (force-output))

(defun good-hash-p (hash)
  (and (zerop (aref hash 0))
       (zerop (aref hash 1))
       (zerop (ldb (byte 4 4) (aref hash 2)))))

(define-problem (2016 5) (data read-line) ("1a3099aa" "694190cd")
  ;; This is ugly for performance.  A prettier version takes over a minute, but
  ;; this is around 20 seconds.
  (iterate
    (with remaining = 8)
    (with part1 = (make-array 8 :element-type 'character :initial-element #\_ :fill-pointer 0))
    (with part2 = (make-string 8 :initial-element #\_))
    (returning part1 part2)

    ;; Instead create a fresh string of the entire ID every time, we keep
    ;; a buffer and just replace the numeric portion on each iteration.
    (with n = (length data))
    (with buffer = (make-array 1024 :fill-pointer n :element-type '(unsigned-byte 8)))
    (initially (replace buffer (string->bytes data)))

    (for i :from 0)
    (for id = (string->bytes (princ-to-string i)))
    (setf (fill-pointer buffer) (+ n (length id)))
    (replace buffer id :start1 n)
    (for hash = (md5:md5sum-sequence buffer))

    (when (dividesp i 100000)
      (progress i))

    (when (good-hash-p hash)
      (for hex = (bytes->hex hash))

      ;; We only need the first 8 results for part 1.
      (when (< (length part1) 8)
        (vector-push (aref hex 5) part1))

      (let ((pos (digit-char-p (aref hex 5) 16))) ; hex digit -> array index
        (when (and (< pos (length part2)) ; valid index
                   (char= (aref part2 pos) #\_)) ; not already seen
          (setf (aref part2 pos) (aref hex 6))
          (when (zerop (decf remaining))
            (when *fancy* (format t " Cracked.~%"))
            (finish))))
      (when *fancy* (format t " [~A] / [~A]~%" part1 part2))
      (progress i))))



#; Scratch --------------------------------------------------------------------
