(defpackage :advent/2016/05 #.cl-user::*advent-use*)
(in-package :advent/2016/05)

(defun md5 (string)
  (bytes->hex (md5:md5sum-string string)))

(define-problem (2016 5) (data read-line) ("1a3099aa" "694190cd")
  (iterate
    (with part1 = (make-array 8 :element-type 'character :initial-element #\_ :fill-pointer 0))
    (with part2 = (make-string 8 :initial-element #\_))
    (with remaining = 8)
    (with spinner = '#1=(#\◴ #\◷ #\◶ #\◵ . #1#))
    (returning part1 part2)
    (for i :from 0)
    (for hash = (md5 (format nil "~A~D" data i)))
    (when (dividesp i 100000)
      (format t "~C~C  Iteration: ~D" #\Return (pop spinner) i)
      (force-output))
    (when (str:starts-with-p "00000" hash)
      (when (< (length part1) 8)
        (vector-push (aref hash 5) part1))
      (let ((pos (digit-char-p (aref hash 5) 16)))
        (when (and (< pos (length part2))
                   (char= (aref part2 pos) #\_))
          (setf (aref part2 pos) (aref hash 6))
          (when (zerop (decf remaining))
            (format t " Cracked.~%")
            (finish))))
      (format t " [~A] / [~A]~%" part1 part2))))



#; Scratch --------------------------------------------------------------------
