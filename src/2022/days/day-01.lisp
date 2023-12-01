(advent:defpackage* :advent/2022/01)
(in-package :advent/2022/01)

(defun parse (lines)
  (_ lines
    (split-sequence:split-sequence "" _ :test #'string=)
    (mapcar (lambda (strs) (summation (mapcar #'parse-integer strs))) _)
    (sort _ #'>)))

(defun part-1 (elves)
  (first elves))

(defun part-2 (elves)
  (summation (take 3 elves)))

(define-problem (2022 1) (data read-lines) (70296 205381)
  (let ((data (parse data)))
    (values (part-1 data)
            (part-2 data))))


#; Scratch --------------------------------------------------------------------

;; Some day I might make a little data parser that might make this a little
;; easier.  Or maybe it's not worth it, I'm not sure.

(parse-input data
             :pre (split-sequence:split-sequence "" _ :test #'string=)
             :map (summation (mapcar #'parse-integer _))
             :post (sort _ #'>)
             )
