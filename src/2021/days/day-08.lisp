(advent:defpackage* :advent/2021/08
  (:import-from :alexandria :length= :set-equal))

(in-package :advent/2021/08)

;;   0 . a b c   e f g   6
;;   1 .     c     f     2
;;   2 . a   c d e   g   5
;;   3 . a   c d   f g   5
;;   4 .   b c d   f     4
;;   5 . a b   d   f g   5
;;   6 . a b   d e f g   6
;;   7 . a   c     f     3
;;   8 . a b c d e f g   7
;;   9 . a b c d   f g   6

(defun parse (stream)
  (iterate
    (for line :in-stream stream :using #'read-line)
    (for words = (mapcar (rcurry #'coerce 'list) (str:split " " line)))
    (collect (split-sequence:split-sequence '(#\|) words :test #'equal))))

(defun input (entry) (first entry))
(defun output (entry) (second entry))

(defun uniquep (word)
  (member (length word) '(2 3 4 7)))

(defun supersetp (sup sub)
  (and (subsetp sub sup)
       (null (set-difference sub sup))))

(defun common-element (&rest sets)
  (let ((result (reduce #'intersection sets)))
    (assert (length= 1 result) () "No common element in ~S." sets)
    (first result)))

(defun solve-entry (entry)
  (let ((words (remove-duplicates (append (input entry) (output entry))
                                  :test #'set-equal))
        (solved (list))) ; alist of e.g. (1 . (#\a #\b))
    (flet ((solve (n predicate)
             (let ((result (find-if predicate words)))
                (alexandria:deletef words result :test #'set-equal)
                (push (cons n result) solved))))
      ;; * 1, 4, 7, 8 have unique lengths.
      (solve 1 (curry #'length= 2))
      (solve 4 (curry #'length= 4))
      (solve 7 (curry #'length= 3))
      (solve 8 (curry #'length= 7))
      ;; * 9 is a superset of 4.
      (solve 9 (rcurry #'supersetp (assocdr 4 solved)))
      ;; * 0 is the 6-element superset of 7.
      (solve 0 (lambda (word) (and (supersetp word (assocdr 7 solved)) (length= 6 word))))
      ;; * 3 is the 5-element superset of 7.
      (solve 3 (lambda (word) (and (supersetp word (assocdr 7 solved)) (length= 5 word))))
      ;; * 6 is the only 6-element group left.
      (solve 6 (curry #'length= 6))
      ;; * 5 contains the only element all the solved so far have in common.
      (solve 5 (lambda (word)
                 (member (apply #'common-element (mapcar #'cdr solved))
                         word)))
      ;; * 2 is the last one left.
      (solve 2 (constantly t))
      ;; Return the result.
      (digits->number
        (loop :for o :in (output entry)
              :collect (rassocar o solved :test #'set-equal))))))

(defun part1 (entries)
  (iterate (for entry :in entries)
           (summing (count-if #'uniquep (output entry)))))

(defun part2 (entries)
  (summation entries :key #'solve-entry))

(define-problem (2021 8) (data parse) (440 1046281)
  (values (part1 data)
          (part2 data)))


#; Scratch --------------------------------------------------------------------
(solve-entry
  '(((#\a #\b #\c #\e #\f #\g)
     (#\c #\f)
     (#\a #\c #\d #\e #\g)
     (#\a #\c #\d #\f #\g)
     (#\b #\c #\d #\f)
     (#\a #\b #\d #\f #\g)
     (#\a #\b #\d #\e #\f #\g)
     (#\a #\c #\f))
    ((#\a #\b #\c #\d #\e #\f #\g)
     (#\a #\b #\c #\d #\f #\g))))
