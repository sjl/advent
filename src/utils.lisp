(in-package :advent)

;;;; Clipboard ----------------------------------------------------------------
(defun sh (command &key (input "") output)
  (uiop:run-program command
                    :output (when output :string)
                    :input (make-string-input-stream input)))

(defun pbcopy (object)
  (sh '("pbcopy") :input (aesthetic-string object))
  (values))

(defun pbpaste ()
  (values (sh '("pbpaste") :output t)))


;;;; Streams ------------------------------------------------------------------
(defun ensure-stream (input)
  (ctypecase input
    (stream input)
    (string (make-string-input-stream input))))

(defun ensure-string (input)
  (ctypecase input
    (stream (alexandria:read-stream-content-into-string input))
    (string (copy-seq input))))

(defun ensure-keyword (input)
  (values
    (ctypecase input
      (keyword input)
      (symbol (alexandria:make-keyword input))
      (string (alexandria:make-keyword (string-upcase (str:trim input)))))))


;;;; Problems -----------------------------------------------------------------
(defmacro define-problem-tests ((year day) part1 part2)
  `(1am:test ,(alexandria:symbolicate 'test-
                                      (princ-to-string year)
                                      '/
                                      (princ-to-string day))
     (multiple-value-bind (part1 part2) (,(alexandria:symbolicate 'run))
       (1am:is (equal ,part1 part1))
       (1am:is (equal ,part2 part2)))))

(defmacro define-problem ((year day)
                          (arg &optional (reader 'identity))
                          (&optional answer1 answer2)
                          &body body)
  (multiple-value-bind (body declarations docstring)
      (alexandria:parse-body body :documentation t)
    (with-gensyms (file)
      (let ((run (symb 'run)))
        `(progn
           (defun ,run (&optional ,arg)
             ,@(when docstring (list docstring))
             ,@declarations
             (let ((,file (unless ,arg (open (problem-data-path ,year ,day)))))
               (unwind-protect
                   (progn (unless ,arg
                            (setf ,arg (,reader (ensure-stream (or ,arg ,file)))))
                          ,@body)
                 (when ,file (close ,file)))))
           ,@(when answer1
               (list `(define-problem-tests (,year ,day) ,answer1 ,answer2)))
           'run)))))

(defun problem-data-path (year day)
  (make-pathname
    :directory `(:relative "data" ,(aesthetic-string year))
    :name (format nil "~2,'0D" day)
    :type "txt"))



;;;; Readers ------------------------------------------------------------------
(defun read-numbers-from-string (line)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-and-collect (stream reader)
  (iterate (for value :in-stream stream :using reader)
           (collect value)))


(defun read-all (stream)
  "Read all forms from `stream` and return them as a fresh list."
  (read-and-collect stream #'read))
(defun read-numbers (stream)
  (read-numbers-from-string (alexandria:read-stream-content-into-string stream)))

(defun read-lines (stream)
  "Read all lines from `stream` and return them as a fresh list of strings."
  (read-and-collect stream #'read-line))

(defun read-lines-of-numbers-and-garbage (stream)
  "Read the lines of numbers in `stream` into a list of lists of numbers.

  Numbers can be separated by anything, even garbage.

  Lines without any numbers will be discarded.

  "
  (iterate (for line :in-stream stream :using #'read-line)
           (for numbers = (read-numbers-from-string line))
           (when numbers
             (collect numbers))))

(defun read-lines-of-words (stream)
  (mapcar (lambda (line) (split-sequence:split-sequence #\space line))
          (read-lines stream)))

(defun read-comma-separated-values (stream)
  (str:split #\, (alexandria:read-stream-content-into-string stream)))


;;;; Rings --------------------------------------------------------------------
(declaim (inline ring-prev ring-next ring-data))

(defstruct (ring (:constructor make-ring%))
  (data)
  (prev nil :type (or null ring))
  (next nil :type (or null ring)))

(defmethod print-object ((ring ring) stream)
  (print-unreadable-object (ring stream :type t :identity t)
    (format stream "~S" (ring-list ring))))


(defun map-ring (function ring)
  (if (null ring)
    nil
    (cons (funcall function (ring-data ring))
          (loop
            :for r = (ring-next ring) :then (ring-next r)
            :until (eql r ring)
            :collect (funcall function (ring-data r))))))

(defmacro do-ring ((el ring) &body body)
  (once-only (ring)
    (with-gensyms (r started)
      `(if (null ,ring)
         nil
         (do* ((,r ,ring (ring-next ,r))
               (,started nil t))
             ((and ,started (eql ,ring ,r)) (values))
           (let ((,el (ring-data ring)))
             ,@body))))))


(defun ring-list (ring)
  (map-ring #'identity ring))


(defun ring-length (ring)
  (let ((result 0))
    (do-ring (el ring)
      (declare (ignore el))
      (incf result))
    result))


(defun ring-move (ring n)
  (check-type n fixnum)
  (if (minusp n)
    (loop :repeat (- n) :do (setf ring (ring-prev ring)))
    (loop :repeat n :do (setf ring (ring-next ring))))
  ring)


(defun ring-insert-after (ring element)
  (if (null ring)
    (ring element)
    (let* ((p ring)
           (n (ring-next ring))
           (new (make-ring% :data element :prev p :next n)))
      (setf (ring-next p) new
            (ring-prev n) new)
      new)))

(defun ring-insert-before (ring element)
  (if (null ring)
    (ring element)
    (let* ((p (ring-prev ring))
           (n ring)
           (new (make-ring% :data element :prev p :next n)))
      (setf (ring-next p) new
            (ring-prev n) new)
      new)))


(defun ring-cut (ring &key prev)
  (assert (not (null ring)) (ring) "Cannot cut from empty ring ~S" ring)
  (let ((n (ring-next ring)))
    (if (eql ring n)
      nil
      (let ((p (ring-prev ring)))
        (setf (ring-next p) n
              (ring-prev n) p
              (ring-next ring) nil
              (ring-prev ring) nil)
        (if prev p n)))))


(define-modify-macro ring-cutf (&rest keywords) ring-cut)
(define-modify-macro ring-movef (n) ring-move)
(define-modify-macro ring-nextf () ring-next)
(define-modify-macro ring-prevf () ring-prev)
(define-modify-macro ring-insertf-after (element) ring-insert-after)
(define-modify-macro ring-insertf-before (element) ring-insert-before)


(defun ring (&rest elements)
  (if (null elements)
    nil
    (iterate
      (with start)
      (for element :in elements)
      (for prev = ring)
      (for ring = (if-first-time
                    (setf start (make-ring% :data element))
                    (make-ring% :data element :prev prev)))
      (when prev
        (setf (ring-next prev) ring
              (ring-prev ring) prev))
      (finally (setf (ring-next ring) start
                     (ring-prev start) ring)
               (return start)))))


;;;; Iterate ------------------------------------------------------------------
(defmacro returning (&rest values)
  `(finally (return (values ,@values))))


;;;; Miscellaneous ------------------------------------------------------------
(defun hash-table= (h1 h2 &optional (test #'eql))
  "Return whether `h1` and `h2` have the same keys and values.

  The consequences are undefined if `h1` and `h2` use different key tests.

  `test` is used to compare values.

  "
  (and (= (hash-table-count h1)
          (hash-table-count h2))
       (iterate (for (k v) :in-hashtable h1)
                (always (funcall test v (gethash k h2))))))

(defun hamming-distance (sequence1 sequence2 &key (test #'eql))
  "Return the Hamming distance between `sequence1` and `sequence2`."
  ;; todo assert length=?
  (let ((result 0))
    (map nil (lambda (x y)
               (unless (funcall test x y)
                 (incf result)))
         sequence1
         sequence2)
    result))

(defun unique (list &key (test #'eql))
  "Return a fresh list of the unique elements in `LIST`.

  This differs from REMOVE-DUPLICATES in that *all* duplicate elements will be
  removed, not just all-but-the-last.

  This is O(n²).

  Example:

    (remove-duplicates '(3 1 3 2 3))
    ; => (1 2 3)

    (unique '(3 1 3 2 3))
    ; => (1 2)

  "
  (iterate
    (for a :in list)
    (for i :from 0)
    (unless (iterate (for b :in list)
                     (for j :from 0)
              (unless (= i j)
                (thereis (funcall test a b))))
      (collect a))))

(defun extremum+ (sequence predicate)
  "Like ALEXANDRIA:EXTREMUM but also return the position as a second value."
  (iterate
    (with value = nil)
    (with position = nil)
    (for i :from 0)
    (for v :in-whatever sequence)
    (if-first-time
      (setf value v
            position i)
      (when (funcall predicate v value)
        (setf value v
              position i)))
    (finally (return (values value position)))))

(defun extremums (sequence predicate &key (key #'identity))
  "Like ALEXANDRIA:EXTREMUM but return *all* values in case of a tie."
  (iterate
    (with results = nil)
    (with prev = nil)
    (for v :in-whatever sequence)
    (for k = (funcall key v))
    (if-first-time
      (progn (push v results)
             (setf prev k))
      (cond
        ((funcall predicate k prev) (setf results (list v)
                                          prev k))
        ((funcall predicate prev k) nil) ; noop
        (t (progn (push v results)
                  (setf prev k)))))
    (finally (return results))))

(defun char-invertcase (char)
  "Return `char` with its case inverted, if possible."
  (if (lower-case-p char)
    (char-upcase char)
    (char-downcase char)))

(defun-inline x (point)
  (realpart point))

(defun-inline y (point)
  (imagpart point))

(defun manhattan-distance (point1 &optional (point2 #c(0 0)))
  "Return the Manhattan distance between the two points on the complex plane."
  (+ (abs (- (x point1)
             (x point2)))
     (abs (- (y point1)
             (y point2)))))

(defun manhattan-neighbors (point)
  "Return points adjacent to point (excluding diagonals) on the complex plane."
  (list (+ point #c(0 1))
        (+ point #c(1 0))
        (+ point #c(0 -1))
        (+ point #c(-1 0))))


(defgeneric emptyp (collection)
  (:documentation "Return whether `collection` is empty."))

(defmethod emptyp ((list list))
  (null list))

(defmethod emptyp ((vector vector))
  (zerop (length vector)))

(defmethod emptyp ((hash-table hash-table))
  (zerop (hash-table-count hash-table)))

(defmethod emptyp ((digraph digraph:digraph))
  (digraph:emptyp digraph))

(defmethod emptyp ((hset hash-set))
  (hset-empty-p hset))


(defun-inline nth-digit (n integer &optional (radix 10))
  "Return the `n`th digit of `integer` in base `radix`, counting from the right."
  (mod (truncate integer (expt radix n)) radix))

(defun-inlineable integral-image (width height value-function)
  ;; https://en.wikipedia.org/wiki/Summed-area_table
  (let ((image (make-array (list width height)))
        (last-row (1- height))
        (last-col (1- width)))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref image x y)
              (+ (funcall value-function x y)
                 (if (= x last-col) 0 (aref image (1+ x) y))
                 (if (= y last-row) 0 (aref image x (1+ y)))
                 (if (or (= x last-col) (= y last-row))
                   0
                   (- (aref image (1+ x) (1+ y))))))))))


(defun positions-if (predicate sequence &key (start 0) end key)
  "Return a fresh list of all positions in `sequence` that satisfy `predicate`.

  Like `cl:position-if`, but returns a list of all the results.

  Example:

    (positions-if #'upper-case-p \"aBCdeF\")
    ; =>
    (1 2 5)

  "
  (let ((pos start))
    (nreverse (reduce (lambda (result value)
                        (prog1 (if (funcall predicate value)
                                 (cons pos result)
                                 result)
                          (incf pos)))
                      sequence
                      :start start
                      :end end
                      :key key
                      :initial-value nil))))


(defun digits (n &key (radix 10) from-end (result-type 'list))
  "Return a fresh list of the digits of `n` in base `radix`.

  By default, the digits are returned high-order first, as you would read them.
  Use `from-end` to get them low-order first:

    (digits 123)             ; => (1 2 3)
    (digits 123 :from-end t) ; => (3 2 1)

  "
  (let ((result (iterate
                  (for (values remaining digit) = (truncate n radix))
                  (collect digit)
                  (setf n remaining)
                  (until (zerop n)))))
    (coerce (if from-end
              result
              (nreverse result))
            result-type)))


(defun fresh-vector (sequence)
  (if (typep sequence 'vector)
    (copy-seq sequence)
    (coerce sequence 'vector)))


(defmacro let-result ((symbol initform) &body body)
  "Bind `symbol` to initform, execute `body`, and return `symbol`.

  This is useful for creating a object, doing some work on it, and returning the
  object.  For example:

    (let-result (table (make-hash-table))
      (setf (gethash 0 table) 'foo))
    ; ==>
    (let ((table (make-hash-table)))
      (setf (gethash 0 table) 'foo)
      table)

  "
  `(let ((,symbol ,initform))
     ,@body
     ,symbol))


;;;; A* Search ----------------------------------------------------------------
(defstruct path
  state
  (estimate 0)
  (cost 0)
  (previous nil))

(defun path-to-list (path &aux result)
  (recursively ((path path))
    (unless (null path)
      (push (path-state path) result)
      (recur (path-previous path))))
  result)

(defun astar (&key start neighbors goalp cost heuristic test limit)
  "Search for a path from `start` to a goal using A★.

  The following parameters are all required:

  * `start`: the starting state.

  * `neighbors`: a function that takes a state and returns all states reachable
    from it.

  * `goalp`: a predicate that takes a state and returns whether it is a goal.

  * `cost`: a function that takes two states `a` and `b` and returns the cost
    to move from `a` to `b`.

  * `heuristic`: a function that takes a state and estimates the distance
    remaining to the goal.

  * `test`: an equality predicate for comparing nodes.  It must be suitable for
    passing to `make-hash-table`.

  If the heuristic function is admissable (i.e. it never overestimates the
  remaining distance) the algorithm will find the shortest path.

  Note that `test` is required.  The only sensible default would be `eql`, but
  if you were using states that need a different predicate and forgot to pass it
  the algorithm would end up blowing the heap, which is unpleasant.

  The following parameters are optional:

  * `limit`: a maximum cost.  Any paths that exceed this cost will not be
    considered.

  "
  (let ((seen (make-hash-table :test test))
        (frontier (pileup:make-heap #'< :key #'path-estimate)))
    (labels ((mark-seen (path)
               (setf (gethash (path-state path) seen) (path-cost path)))
             (push-path (path)
               (mark-seen path)
               (pileup:heap-insert path frontier)))
      (iterate
        (initially (push-path (make-path :state start)))

        (for (values current found) = (pileup:heap-pop frontier))
        (unless found
          (return (values nil nil)))

        (for current-state = (path-state current))

        (when (funcall goalp current-state)
          (return (values (path-to-list current) t)))

        (for current-cost = (path-cost current))

        (iterate
          (for next-state :in (funcall neighbors current-state))
          (for next-cost = (+ current-cost (funcall cost current-state next-state)))
          (for (values seen-cost previously-seen) = (gethash next-state seen))
          (unless (and limit (> next-cost limit))
            (when (or (not previously-seen)
                      (< next-cost seen-cost))
              (for next-estimate = (+ next-cost (funcall heuristic next-state)))
              (push-path (make-path :state next-state
                                    :cost next-cost
                                    :estimate next-estimate
                                    :previous current)))))))))
