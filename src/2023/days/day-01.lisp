(advent:defpackage* :advent/2023/01)
(in-package :advent/2023/01)

(defun result (digit-lists)
  (iterate (for digits :in digit-lists)
           (summing (+ (* 10 (first digits))
                       (car (last digits))))))

(defun part-1 (line)
  (remove nil (map 'list #'digit-char-p line)))

(defun word->digit (word)
  (position word '(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
            :test #'string=))

(defun part-2 (line)
  ;; The puzzle is mean and uses overlapping words, e.g. nineight needs to
  ;; return (9 8), but PPCRE doesn't handle overlaps so we need to make sure to
  ;; do that.
  (iterate (for ((#'parse-integer n) word)
                :matching "([0-9])|(one|two|three|four|five|six|seven|eight|nine)"
                :against line
                :overlap t)
           (collect (or n (word->digit word)))))

(define-problem (2023 1) (data read-lines) (55386 54824)
  (values (result (mapcar #'part-1 data))
          (result (prl (mapcar #'part-2 data)))))

#; Scratch --------------------------------------------------------------------

(result (mapcar #'part-2
        '("two1nineight"
          "eightwothree"
          "abcone2threexyz"
          "xtwone3four"
          "4nineeightseven2"
          "zoneight234"
          "7pqrstsixteen")))

