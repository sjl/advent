; https://bitbucket.org/sjl/beef
(ql:quickload "beef")
(ql:quickload "split-sequence")
(ql:quickload "cl-arrows")
(ql:quickload "fset")
(ql:quickload "cl-ppcre")
(ql:quickload "ironclad")

(defpackage #:advent
  (:use #:cl)
  (:use #:cl-arrows)
  (:use #:split-sequence))

(in-package #:advent)


;;;; Day 1
(defun instruction-to-num (ch)
  (cond
    ((eql ch #\() 1)
    ((eql ch #\)) -1)
    (t 0)))

(defun advent-1-1 ()
  (loop :for c :across (beef:slurp "data/1")
        :sum (instruction-to-num c)))

(defun advent-1-2 ()
  (loop :for c :across (beef:slurp "data/1")
        :sum (instruction-to-num c) :into floor
        :sum 1
        :until (= floor -1)))


;;;; Day 2
(defun advent-2-data ()
  (->> (beef:slurp "data/2")
       beef:trim-whitespace-right
       beef:split-lines
       (mapcar (lambda (s)
                 (->> s
                      (split-sequence #\x)
                      (mapcar #'parse-integer))))))

(defun advent-2-1 ()
  (loop :for dims :in (advent-2-data)
        :for (w h l) = dims
        :for sides = (list (* w h)
                           (* w l)
                           (* h l))
        :for paper = (* 2 (apply #'+ sides))
        :for slack = (apply #'min sides)
        :sum (+ paper slack)))

(defun advent-2-2 ()
  (loop :for dims :in (advent-2-data)
        :for (w h l) = dims
        :for sides = (list (* 2 (+ w h))
                           (* 2 (+ w l))
                           (* 2 (+ h l)))
        :for ribbon = (apply #'min sides)
        :for bow = (apply #'* dims)
        :sum (+ ribbon bow)))


;;;; Day 3
(defun advent-3-data ()
  (beef:trim-whitespace (beef:slurp "data/3")))

(defun instruction-to-offsets (instruction)
  (case instruction
    (#\> '(1 0))
    (#\< '(-1 0))
    (#\^ '(0 1))
    (#\v '(0 -1))))

(defun step-santa (loc dir)
  (destructuring-bind (x y) loc
    (destructuring-bind (dx dy) (instruction-to-offsets dir)
      (list (+ x dx) (+ y dy)))))

(defun houses (data)
  (loop
    :with loc = '(0 0)
    :with visited = (fset:set '(0 0))
    :for dir :across data
    :do (setq loc (step-santa loc dir))
    :do (fset:includef visited loc)
    :finally (return visited)))

(defun advent-3-1 (data)
  (fset:size (houses data)))

(defun advent-3-2 (data)
  (fset:size
    (fset:union
      ;                                 come directly at me
      (houses (ppcre:regex-replace-all "(.)." data "\\1"))
      (houses (ppcre:regex-replace-all ".(.)" data "\\1")))))

;;;; Day 4
(defun advent-4-data ()
  "ckczppom")

(defun md5 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :md5
                              (ironclad:ascii-string-to-byte-array str))))

(defun mine (data zeroes)
  (let ((target (apply #'concatenate 'string
                       (loop :repeat zeroes :collect "0"))))
    (loop :for i :upfrom 1
          :for hash = (->> i
                           prin1-to-string
                           (concatenate 'string data)
                           md5)
          :until (equal target (subseq hash 0 zeroes))
          :finally (return i))))

(defun advent-4-1 (data)
  (mine data 5))

(defun advent-4-2 (data)
  (mine data 6))


;;;; Day 4
(defun advent-5-data ()
  (-> "data/5"
      beef:slurp
      beef:trim-whitespace-right
      beef:split-lines))

(defun join-strings (strings delim)
  "Join strings into a single string with the given delimiter string interleaved.

   Delim must not contain a ~.

   "
  (format nil (concatenate 'string "~{~A~^" delim "~}") strings))


(defparameter *bad-pairs* '("ab" "cd" "pq" "xy"))

(defun count-vowels (s)
  (length (ppcre:regex-replace-all "[^aeiou]" s "")))

(defun has-run (s)
  (when (ppcre:scan "(.)\\1" s) t))

(defun has-bad (s)
  (when (ppcre:scan (join-strings *bad-pairs* "|") s) t))

(defun is-nice (s)
  (and (>= (count-vowels s) 3)
       (has-run s)
       (not (has-bad s))))

(defun advent-5-1 (data)
  (count-if #'is-nice data))

(defun has-run-2 (s)
  (when (ppcre:scan "(..).*\\1" s) t))

(defun has-repeat (s)
  (when (ppcre:scan "(.).\\1" s) t))

(defun is-nice-2 (s)
  (and (has-run-2 s)
       (has-repeat s)))

(defun advent-5-2 (data)
  (count-if #'is-nice-2 data))

#+comment (advent-4-1 "abcdef")
#+comment (advent-5-2 (advent-5-data))
