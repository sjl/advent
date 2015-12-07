; https://bitbucket.org/sjl/beef
(ql:quickload "beef")
(ql:quickload "split-sequence")
(ql:quickload "cl-arrows")
(ql:quickload "fset")
(ql:quickload "cl-ppcre")
(ql:quickload "ironclad")
(ql:quickload "smug")
(ql:quickload "bit-smasher")

(defpackage #:advent
  (:use #:cl)
  (:use #:cl-arrows)
  (:use #:split-sequence)
  (:use #:smug))

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


;;;; Day 5
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


;;;; Day 6
(defun advent-6-data ()
  (beef:slurp-lines "data/6" :ignore-trailing-newline t))

(defmacro loop-array (arr name &rest body)
  (let ((i (gensym "index")))
    `(loop :for ,i :below (array-total-size ,arr)
           :for ,name = (row-major-aref ,arr ,i)
           ,@body)))

(defun parse-indexes (s)
  (->> s
       (split-sequence #\,)
       (mapcar #'parse-integer)))

(defun flip (b)
  (if (zerop b) 1 0))

(defun parse-line (line)
  (let ((parts (split-sequence #\space line)))
    (list (parse-indexes (beef:index parts -3))
          (parse-indexes (beef:index parts -1))
          (cond
            ((equal (car parts) "toggle") :toggle)
            ((equal (cadr parts) "on") :on)
            ((equal (cadr parts) "off") :off)
            (t (error "Unknown operation!"))))))

(defmacro loop-square (r c from-row from-col to-row to-col &rest body)
  `(loop :for ,r :from ,from-row :to ,to-row
         :do (loop :for ,c :from ,from-col :to ,to-col
                   ,@body)))

(defun advent-6-1 (data)
  (let ((lights (make-array '(1000 1000) :element-type 'bit)))
    (loop
      :for line :in data
      :for ((from-row from-col) (to-row to-col) operation) = (parse-line line)
      :do (loop-square
            r c from-row from-col to-row to-col
            :do (setf (bit lights r c)
                      (case operation
                        (:toggle (flip (bit lights r c)))
                        (:on 1)
                        (:off 0)
                        (t 0)))))
    (loop-array lights b
                :sum b)))

(defun advent-6-2 (data)
  (let ((lights (make-array '(1000 1000) :element-type 'integer)))
    (loop
      :for line :in data
      :for ((from-row from-col) (to-row to-col) operation) = (parse-line line)
      :do (loop-square
            r c from-row from-col to-row to-col
            :do (case operation
                  (:toggle (incf (aref lights r c) 2))
                  (:on (incf (aref lights r c)))
                  (:off (when (not (zerop (aref lights r c)))
                          (decf (aref lights r c)))))))
    (loop-array lights b
                :sum b)))


;;;; Day 7
(defun advent-7-data ()
  (beef:slurp-lines "data/7" :ignore-trailing-newline t))

(defun advent-7-2-data ()
  (beef:slurp-lines "data/7-2" :ignore-trailing-newline t))

(defun int->bits (i)
  (bitsmash:bits<- (format nil "~4,'0X" i)))

(defun bit-lshift (bit-array distance)
  (replace (make-array (length bit-array) :element-type 'bit)
           bit-array
           :start1 0
           :start2 (bitsmash:int<- distance)))

(defun bit-rshift (bit-array distance)
  (let ((width (length bit-array))
        (distance (bitsmash:int<- distance)))
    (replace (make-array width :element-type 'bit)
             bit-array
             :start1 distance
             :end2 (- width distance))))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defun .one-or-more (parser)
  (.let* ((x parser)
          (y (.zero-or-more parser)))
    (.identity (cons x y))))

(defun parse-7 (line)
  (labels ((.whitespace ()
             (.first (.one-or-more (.is 'member '(#\space #\tab)))))
           (.arrow ()
             (.first (.string= "->")))
           (.number ()
             (.let* ((digits (.first (.one-or-more (.is 'digit-char-p)))))
                    (.identity (parse-integer (concatenate 'string digits)))))
           (.wire ()
             (.let* ((chars (.first (.one-or-more (.is 'lower-case-p)))))
                    (.identity (concatenate 'string chars))))
           (.source ()
             (.or (.wire) (.number)))
           (.string-choice (strs)
             (if (not strs)
               (.fail)
               (.or (.string= (car strs))
                    (.string-choice (cdr strs)))))
           (.dest ()
             (.progn (.whitespace) (.arrow) (.whitespace)
                     (.wire)))
           (.constant-source ()
             (.let* ((val (.source)))
                    (.identity (list #'identity (list val)))))
           (.binary-op ()
             (let ((ops '(("AND" . bit-and)
                          ("OR" . bit-ior)
                          ("LSHIFT" . bit-lshift)
                          ("RSHIFT" . bit-rshift))))
               (.let* ((name (.string-choice (mapcar #'car ops))))
                      (.identity (cdr (assoc name ops :test #'equal))))))
           (.binary-source ()
             (.let* ((left (.source))
                     (_ (.whitespace))
                     (op (.binary-op))
                     (_ (.whitespace))
                     (right (.source)))
                    (.identity (list op (list left right)))))
           (.unary-op ()
             (.let* ((_ (.string= "NOT")))
                    (.identity #'bit-not)))
           (.unary-source ()
             (.let* ((op (.unary-op))
                     (_ (.whitespace))
                     (source (.source)))
                    (.identity (list op (list source)))))
           (.instruction ()
             (.let* ((source (.or (.binary-source)
                                  (.unary-source)
                                  (.constant-source)))
                     (dest (.dest)))
                    (.identity (concatenate 'list source (list dest))))))
    (parse (.instruction) line)))

(defun advent-7-1 (data)
  (let ((circuit (make-hash-table :test #'equal))
        (commands (mapcar #'parse-7 data)))
    (labels ((retrieve (source)
               (cond
                 ((stringp source) (gethash source circuit))
                 ((integerp source) (int->bits source))
                 (t (error "what?"))))
             (ready (args)
               (every #'identity args))
             (perform (fn args dest)
               (setf (gethash dest circuit)
                     (apply fn args)))
             (try-command (command)
               "If the command is ready to go, run it and return nil.  Otherwise,
                return the command itself."
               (destructuring-bind (fn args dest) command
                 (let ((vals (mapcar #'retrieve args)))
                   (if (ready vals)
                     (progn
                       (perform fn vals dest)
                       nil)
                     command)))))
      (loop :while commands
            :do (setf commands
                  (loop :for command :in commands
                        :when (try-command command)
                        :collect :it)))
      (bitsmash:bits->int (gethash "a" circuit)))))


;;;; Scratch
#+comment (advent-7-1 '("55 -> b" "b -> a"))
#+comment (advent-7-1 (advent-7-2-data))
