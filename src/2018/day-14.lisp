(defpackage :advent/2018/14 #.cl-user::*advent-use*)
(in-package :advent/2018/14)

(defun combine (recipes elves)
  (digits (summation elves :key (curry #'aref recipes))))

(defun move-elves (recipes elves)
  (do-array (elf elves)
    (setf elf (mod (+ elf (aref recipes elf) 1)
                   (length recipes)))))

(defun format-output (scores)
  (str:join "" (coerce scores 'list)))

(define-problem (2018 14) (data read) ("3610281143" 20211326)
  #+sbcl (sb-ext:gc :full t)
  (iterate
    (with recipes = (make-array 2
                      :adjustable t
                      :fill-pointer t
                      :initial-contents '(3 7)))
    (with elves = (make-array 2 :initial-contents '(0 1)))
    (with part-1 = nil)
    (with part-2 = nil)
    (with target = (digits data :result-type 'vector))
    (with target-length = (length target))

    (until (and part-1 part-2))

    (unless part-1
      (when (>= (length recipes) (+ 10 data))
        (setf part-1 (format-output (subseq recipes data (+ data 10))))))

    (iterate (for new-recipe :in (combine recipes elves))
             (vector-push-extend new-recipe recipes)
             (for len = (length recipes))
             (for left = (- len target-length))
             (unless part-2
               (when (and (not (minusp left))
                          (search target recipes :start2 left))
                 (setf part-2 left))))

    (move-elves recipes elves)

    (finally (return (values part-1 part-2)))))
