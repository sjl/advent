; https://bitbucket.org/sjl/beef
(ql:quickload "beef")
(ql:quickload "alexandria")
(ql:quickload "split-sequence")
(ql:quickload "cl-arrows")
(ql:quickload "fset")
(ql:quickload "cl-ppcre")
(ql:quickload "ironclad")
(ql:quickload "smug")
(ql:quickload "bit-smasher")
(ql:quickload "optima")

(defpackage #:advent
  (:use #:cl)
  (:use #:cl-arrows)
  (:use #:split-sequence)
  (:use #:smug))

(in-package #:advent)

(declaim (optimize (debug 3)))

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


;;;; Day 8
(defun advent-8-data ()
  (beef:slurp-lines "data/8" :ignore-trailing-newline t))


(defconstant +backslash+ #\\ )
(defconstant +quote+ #\" )

(defun parse-8 (line)
  (labels ((.hex-digit ()
             (.or
               (.is #'digit-char-p)
               (.is #'member '(#\a #\b #\c #\d #\e #\f))))
           (.hex-escape ()
             (.let* ((_ (.char= #\x))
                     (a (.hex-digit))
                     (b (.hex-digit)))
                    (.identity
                      (->> (format nil "~A~A" a b)
                           bitsmash:hex->int
                           code-char))))
           (.escaped-char ()
             (.progn
               (.char= +backslash+)
               (.or (.char= +backslash+)
                    (.char= +quote+)
                    (.hex-escape))))
           (.normal-char ()
             (.is-not 'member (list +backslash+ +quote+)))
           (.string-char ()
             (.or (.normal-char)
                  (.escaped-char)))
           (.full-string ()
             (.prog2
               (.char= +quote+)
               (.first (.zero-or-more (.string-char)))
               (.char= +quote+))))
    (parse (.full-string) line)))

(defun .wrap (fn parser)
  (.let* ((v parser))
         (.identity (funcall fn v))))

(defun parse-8-2 (line)
  (labels ((.special-char ()
             (.let* ((ch (.or (.char= +backslash+)
                              (.char= +quote+))))
                    (.identity (list +backslash+ ch))))
           (.normal-char ()
             (.wrap #'list
                    (.is-not 'member (list +backslash+ +quote+))))
           (.string-char ()
             (.or (.normal-char)
                  (.special-char)))
           (.full-string ()
             (.let* ((chars (.zero-or-more (.string-char))))
                    (.identity (apply #'concatenate 'list chars)))))
    (append (list +quote+)
            (parse (.full-string) line)
            (list +quote+))))

(defun advent-8-1 (data)
  (loop :for line :in data
        :for chars = (parse-8 line)
        :sum (- (length line)
                (length chars))))

(defun advent-8-2 (data)
  (loop :for line :in data
        :for chars = (parse-8-2 line)
        :sum (- (length chars)
                (length line))))


;;;; Day 9
(defun advent-9-data ()
  (beef:slurp-lines "data/9" :ignore-trailing-newline t))

; Thanks Norvig
; http://norvig.com/paip/gps.lisp
(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1 :test #'eq))))
              bag)))

(defun advent-9 (data)
  (let ((distances (make-hash-table :test #'equal)))
    (loop :for line :in data
          :for (a to b _ dist) = (split-sequence #\space line)
          :for distance = (parse-integer dist)
          :do (progn
                (setf (gethash (cons a b) distances) distance)
                (setf (gethash (cons b a) distances) distance)))
    (labels ((score-route (route)
               (optima:match route
                 ((list _) 0)
                 ((list* a b _) (+ (gethash (cons a b) distances)
                                   (score-route (cdr route))))))
             (dedupe (l)
               (remove-duplicates l :test #'equal)))
      (loop :for route :in (->> distances
                             beef:hash-keys
                             (mapcar #'car)
                             dedupe
                             permutations)
            :for score = (score-route route)
            :minimizing score :into min-dist
            :maximizing score :into max-dist
            :finally (return (cons min-dist max-dist))))))


(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~%~{~{    (~s : ~s)~}~%~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))


;;;; Day 10
(defun advent-10-data ()
  "1321131112")

(defun look-and-say (seq)
  (let ((runs (list))
        (len 1)
        (current -1))
    (flet ((mark-run ()
             (setf runs (cons current (cons len runs)))))
    (loop :for n :in seq
          :do (if (= current n)
                (incf len)
                (progn
                  (when (not (= -1 current))
                    (mark-run))
                  (setf len 1)
                  (setf current n)))
          :finally (mark-run))
    (reverse runs))))

(defun iterate (n f data)
  (declare (optimize speed (debug 0)))
  (dotimes (_ n)
    (setf data (funcall f data)))
  data)

(defun las-to-list (s)
  (loop :for digit :across s
        :collect (-> digit string parse-integer)))

(defun advent-10-1 (data)
  (length (iterate 40 #'look-and-say (las-to-list data))))

(defun advent-10-2 (data)
  (length (iterate 50 #'look-and-say (las-to-list data))))


;;;; Day 11
(defun advent-11-data ()
  "vzbxkghb")


(defparameter base 26)
(defparameter ascii-alpha-start (char-code #\a))
(defun num-to-char (n)
  (code-char (+ n ascii-alpha-start)))

(defun char-to-num (ch)
  (- (char-code ch) ascii-alpha-start))


(defmacro loop-window (seq width binding-form &rest body)
  (let ((i (gensym "IGNORE"))
        (s (gensym "SEQ"))
        (n (gensym "WIDTH"))
        (tail (gensym "TAIL")))
    `(let ((,s ,seq)
           (,n ,width))
       (loop :for ,i :upto (- (length ,s) ,n)
             :for ,tail :on ,s
             :for ,binding-form = (subseq ,tail 0 ,n)
             ,@body))))


(defun is-straight-3 (l)
  (destructuring-bind (a b c) l
    (and (= 1 (- b a))
         (= 1 (- c b)))))

(defun has-straight-3 (nums)
  (loop-window nums 3 triplet
               :thereis (is-straight-3 triplet)))


(defparameter bad-chars
  (mapcar #'char-to-num '(#\i #\l #\o)))

(defun no-bad (nums)
  (loop :for bad :in bad-chars
        :never (find bad nums)))


(defun two-pairs (nums)
  (-> nums
      (loop-window 2 (a b)
                   :when (= a b)
                   :collect a)
      remove-duplicates
      length
      (>= 2)))


(defun valid (nums)
  (and (has-straight-3 nums)
       (no-bad nums)
       (two-pairs nums)
       nums))


(defun incr-nums (nums)
  (labels ((inc-mod (x)
             (mod (1+ x) base))
           (inc (nums)
             (if (not nums)
               '(1)
               (let ((head (inc-mod (car nums))))
                 (if (zerop head)
                   (cons head (inc (cdr nums)))
                   (cons head (cdr nums)))))))
    (reverse (inc (reverse nums)))))


(defun advent-11-1 (data)
  (flet ((chars-to-string (chs)
           (apply #'concatenate 'string (mapcar #'string chs)))
         (nums-to-chars (nums)
           (mapcar #'num-to-char nums))
         (string-to-nums (str)
           (loop :for ch :across str
                 :collect (char-to-num ch))))
    (-> (loop :for pw = (incr-nums (string-to-nums data))
              :then (incr-nums pw)
              :thereis (valid pw))
        nums-to-chars
        chars-to-string)))


;;;; Day 12
(defun advent-12-data ()
  (beef:trim-whitespace (beef:slurp "data/12")))


(defun parse-json (s)
  (labels ((.number ()
             (.let* ((negation (.optional (.char= #\-)))
                     (digits (.first (.one-or-more (.is 'digit-char-p)))))
               (.identity (let ((i (parse-integer (concatenate 'string digits)))
                                (c (if negation -1 1)))
                            (* i c)))))
           (.string-char ()
             (.wrap #'list (.is-not 'member (list +quote+))))
           (.string-guts ()
             (.let* ((chars (.zero-or-more (.string-char))))
               (.identity (apply #'concatenate 'string chars))))
           (.string ()
             (.prog2
               (.char= +quote+)
               (.string-guts)
               (.char= +quote+)))
           (.map-pair ()
             (.let* ((key (.string))
                     (_ (.char= #\:))
                     (value (.expression)))
               (.identity (cons key value))))
           (.map-guts ()
             (.or
               (.let* ((p (.map-pair))
                       (_ (.char= #\,))
                       (remaining (.map-guts)))
                 (.identity (cons p remaining)))
               (.wrap #'list (.map-pair))
               (.identity '())))
           (.map ()
             (.prog2
               (.char= #\{)
               (.wrap (lambda (v) (cons :map v))
                      (.map-guts))
               (.char= #\})))
           (.array-guts ()
             (.or
               (.let* ((item (.expression))
                       (_ (.char= #\,))
                       (remaining (.array-guts)))
                 (.identity (cons item remaining)))
               (.wrap #'list (.expression))
               (.identity '())))
           (.array ()
             (.prog2
               (.char= #\[)
               (.wrap (lambda (v) (cons :array v))
                      (.array-guts))
               (.char= #\])))
           (.expression ()
             (.or (.array)
                  (.map)
                  (.string)
                  (.number))))
    (parse (.expression) s)))

(defun walk-sum (v)
  (cond
    ((not v) 0)
    ((typep v 'integer) v)
    ((typep v 'string) 0)
    ((eql (car v) :array) (loop :for value in (cdr v)
                                :sum (walk-sum value)))
    ((eql (car v) :map) (loop :for (key . value) :in (cdr v)
                              :sum (walk-sum value)))
    (:else (error (format nil "wat? ~a" v)))))

(defun walk-sum-2 (v)
  (cond
    ((not v) 0)
    ((typep v 'integer) v)
    ((typep v 'string) 0)
    ((eql (car v) :array) (loop :for value in (cdr v)
                                :sum (walk-sum-2 value)))
    ((eql (car v) :map)
     (if (member "red" (mapcar #'cdr (cdr v))
                 :test #'equal)
       0
       (loop :for (key . value) :in (cdr v)
             :sum (walk-sum-2 value))))
    (:else (error (format nil "wat? ~a" v)))))


(defun advent-12-1 (data)
  (walk-sum (parse-json data)))

(defun advent-12-2 (data)
  (walk-sum-2 (parse-json data)))


;;;; Day 13
(defun advent-13-data ()
  (beef:slurp-lines "data/13" :ignore-trailing-newline t))


(defvar *wat* nil)

(defmacro map-when (test fn val &rest args)
  (let ((v (gensym "VALUE")))
    `(let ((,v ,val))
       (if ,test
         (apply ,fn ,v ,args)
         ,v))))

(defun split-lines-13 (lines)
  (loop :for line :in lines
        :collect (ppcre:register-groups-bind
                   (a dir amount b)
                   ("(\\w+) would (gain|lose) (\\d+) .*? next to (\\w+)."
                    line)
                   (list a b (map-when (equal "lose" dir)
                                       #'-
                                       (parse-integer amount))))))

(defun rate-seating (vals arrangement)
  (labels ((find-val (a b)
             (or (gethash (cons a b) vals) 0))
           (rate-one-direction (arr)
             (+ (loop-window arr 2 (a b) :sum (find-val a b))
                (find-val (car (last arr)) (car arr)))))
    (+ (rate-one-direction arrangement)
       (rate-one-direction (reverse arrangement)))))

(defun advent-13-1 (data)
  (let* ((tups (split-lines-13 data))
         (attendees (remove-duplicates (mapcar #'car tups) :test #'equal))
         (vals (make-hash-table :test #'equal)))
    (loop :for (a b val) :in tups
          :do (setf (gethash (cons a b) vals) val))
    (loop :for arrangement :in (permutations attendees)
          :maximize (rate-seating vals arrangement))))

(defun advent-13-2 (data)
  (let* ((tups (split-lines-13 data))
         (attendees (cons "Self" (remove-duplicates (mapcar #'car tups) :test #'equal)))
         (vals (make-hash-table :test #'equal)))
    (loop :for (a b val) :in tups
          :do (setf (gethash (cons a b) vals) val))
    (loop :for arrangement :in (permutations attendees)
          :maximize (rate-seating vals arrangement))))


;;;; Day 14
(defun advent-14-data ()
  (beef:slurp-lines "data/14" :ignore-trailing-newline t))


(defun tick (deer)
  (destructuring-bind
    (name speed sprint-period rest-period traveled currently remaining)
    deer
    (let ((remaining (1- remaining)))
      (if (equal currently :resting)
        (list name speed sprint-period rest-period
              traveled
              (if (zerop remaining) :sprinting :resting)
              (if (zerop remaining) sprint-period remaining))
        (list name speed sprint-period rest-period
              (+ traveled speed)
              (if (zerop remaining) :resting :sprinting)
              (if (zerop remaining) rest-period remaining))))))

(defun parse-deers (lines)
  (loop :for line :in lines
        :collect (ppcre:register-groups-bind
                   (name speed sprint-period rest-period)
                   ("(\\w+) can fly (\\d+) km/s for (\\d+) .* (\\d+) seconds."
                    line)
                   (list name
                         (parse-integer speed)
                         (parse-integer sprint-period)
                         (parse-integer rest-period)
                         0
                         :sprinting
                         (parse-integer sprint-period)))))

(defun find-leaders (deers)
  (let ((dist (reduce #'max deers :key (beef:partial #'nth 4))))
    (remove-if-not (lambda (deer)
                     (= dist (nth 4 deer)))
                   deers)))


(defun advent-14-1 (data n)
  (apply #'max
         (loop :for i :upto n
               :for deers := (parse-deers data) :then (mapcar #'tick deers)
               :finally (return (mapcar (beef:partial #'nth 4) deers)))))

(defun advent-14-2 (data n)
  (let ((scores (make-hash-table :test #'equal))
        (deers (parse-deers data)))
    (loop :for (name) :in deers
          :do (setf (gethash name scores) 0))
    (loop :for i :upto n
          :do (setf deers (mapcar #'tick deers))
          :do (loop :for (name) :in (find-leaders deers)
                    :do (incf (gethash name scores))))
    (apply #'max (beef:hash-values scores))))




;;;; Scratch
#+comment (advent-14-2 (advent-14-data) 2503)
