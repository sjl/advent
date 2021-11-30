(advent:defpackage* :advent/2018/09)
(in-package :advent/2018/09)
(named-readtables:in-readtable :interpol-syntax)


(defun parse-input (line)
  (ppcre:register-groups-bind
      ((#'parse-integer players marbles))
      (#?r"(\d+) players\D*(\d+) points" line)
    (values players marbles)))

(defun play (players marbles)
  (let ((circle (ring 0))
        (elves (make-array players :initial-element 0)))
    (iterate
      (declare (iterate:declare-variables))
      (for elf :first 0 :then (mod (1+ elf) players))
      (for marble :from 1 :to marbles)
      (if (dividesp marble 23)
        (progn (incf (aref elves elf) marble)
               (ring-movef circle -7)
               (incf (aref elves elf) (ring-data circle))
               (ring-cutf circle))
        (progn (ring-movef circle 1)
               (ring-insertf-after circle marble))))
    (alexandria:extremum elves '>)))


(define-problem (2018 9) (data alexandria:read-stream-content-into-string)
    (398730 3349635509)
  (multiple-value-bind (players marbles) (parse-input data)
    #+sbcl (sb-ext:gc :full t)
    (values (play players marbles)
            (play players (* marbles 100)))))
