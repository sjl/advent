(in-package :advent)
(named-readtables:in-readtable :interpol-syntax)

;;;; Problems -----------------------------------------------------------------
(define-problem (2018 1 1) (data read-all-from-file)
  (summation data))

(define-problem (2018 1 2) (data read-all-from-file)
  (setf (cdr (last data)) data) ; make data a circular list for easy looping
  (iterate
    (with seen = (make-hash-set :initial-contents '(0)))
    (for number :in data)
    (summing number :into frequency)
    (if (hset-contains-p seen frequency)
      (return frequency)
      (hset-insert! seen frequency))))


(define-problem (2018 2 1) (data read-lines-from-file)
  (let* ((freqs (mapcar #'frequencies data))
         (counts (mapcar #'hash-table-values freqs)))
    (* (count 2 counts :test #'member)
       (count 3 counts :test #'member))))

(define-problem (2018 2 2) (data read-lines-from-file)
  ;; just brute force it
  (multiple-value-bind (a b)
      (iterate
        (for (a . remaining) :on data)
        (for b = (find 1 remaining :key (curry #'hamming-distance a)))
        (when b
          (return (values a b))))
    (let ((i (mismatch a b)))
      (str:concat (subseq a 0 i)
                  (subseq a (1+ i))))))


(defstruct claim id left right top bottom)

(define-problem (2018 3) (data read-lines-from-file)
  (labels ((parse-claim (line)
             (ppcre:register-groups-bind
                 ((#'parse-integer id col row width height))
                 (#?/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/ line)
               (make-claim :id id
                           :left col
                           :top row
                           :right (+ col width)
                           :bottom (+ row height))))
           (claims-intersect-p (claim1 claim2)
             (not (or (<= (claim-right claim2) (claim-left claim1))
                      (<= (claim-right claim1) (claim-left claim2))
                      (>= (claim-top claim2) (claim-bottom claim1))
                      (>= (claim-top claim1) (claim-bottom claim2))))))
    (let ((claims (mapcar #'parse-claim data))
          (fabric (make-array (list 1000 1000) :initial-element 0)))
      (dolist (claim claims)
        (do-range ((row (claim-top claim) (claim-bottom claim))
                   (col (claim-left claim) (claim-right claim)))
          (incf (aref fabric row col))))
      (values
        (iterate (for uses :in-array fabric)
                 (counting (> uses 1)))
        (claim-id (first (unique claims :test #'claims-intersect-p)))))))


(define-problem (2018 4) (data read-lines-from-file)
  ;; This problem gets much easier after you've unlocked the second question and
  ;; realize you can solve everything by building histograms of each guard's
  ;; sleeping minutes.
  (labels ((parse-line (line)
             "Parse `line` into `(minute :event id?)`"
             (ppcre:register-groups-bind
                 ((#'parse-integer minute) event)
                 (#?/\[\d+-\d+-\d+ \d+:(\d+)\] (.*)/ line)
               (list* minute
                      (cond
                        ((string= "falls asleep" event) (list :sleep nil))
                        ((string= "wakes up" event) (list :wake nil))
                        (t (ppcre:register-groups-bind
                               ((#'parse-integer id))
                               (#?/Guard #(\d+) begins shift/ event)
                             (list :guard id)))))))
           (sleep-intervals (events &aux start guard)
             "Transform `events` into a list of `(guard-id start end)`"
             (iterate
               (for (minute event id?) :in events)
               (ecase event
                 (:guard (setf guard id?))
                 (:wake (collect (list guard start minute)))
                 (:sleep (setf start minute)))))
           (guard-histograms (intervals)
             "Return a hash-table of histograms of the guards' sleeping minutes."
             (iterate
               (with result = (make-hash-table))
               (for (guard start end) :in intervals)
               (for histogram = (ensure-gethash guard result
                                                (make-array 60 :initial-element 0)))
               (do-range ((minute start end))
                 (incf (aref histogram minute)))
               (finally (return result)))))
    (let ((guard-histograms (-<> data
                              (sort <> #'string<)
                              (mapcar #'parse-line <>)
                              sleep-intervals
                              guard-histograms)))
      (nest
        (destructuring-bind
            (sleepy-guard sleepy-guard-preferred-minute)
            (iterate
              (for (guard histogram) :in-hashtable guard-histograms)
              (finding (list guard
                             (nth-value 1 (extremum+ histogram #'>)))
                       :maximizing (summation histogram))))
        (destructuring-bind
            (predictable-guard predictable-guard-time)
            (iterate
              (for (guard histogram) :in-hashtable guard-histograms)
              (for (values time preferred-minute) = (extremum+ histogram #'>))
              (finding (list guard preferred-minute) :maximizing time)))
        (values (* sleepy-guard
                   sleepy-guard-preferred-minute)
                (* predictable-guard
                   predictable-guard-time))))))

(define-problem (2018 5) (data read-file-into-string)
  (setf data (remove #\newline data))
  (labels ((reactivep (x y)
             (char= x (char-invertcase y)))
           (react (string &aux result)
             (doseq (char string)
               (if (and result (reactivep char (car result)))
                 (pop result)
                 (push char result)))
             (coerce (nreverse result) 'string)))
    (values (length (react data))
            (iterate
              (for unit :in-vector (remove-duplicates data :test #'char-equal))
              (for candidate = (react (remove unit data :test #'char-equal)))
              (minimizing (length candidate))))))


(define-problem (2018 6) (data read-lines-from-file)
  (flet ((parse-line (line)
           (apply #'complex (mapcar #'parse-integer (str:split ", " line))))
         (closest (point coordinates)
           (let ((results (extremums coordinates '<
                                     :key (curry #'manhattan-distance point))))
             (case (length results)
               (1 (car results))
               (t nil)))))
    (let* ((coordinates (mapcar #'parse-line data))
           (xs (mapcar #'realpart coordinates))
           (ys (mapcar #'imagpart coordinates))
           (left (extremum xs #'<))
           (bottom (extremum ys #'<))
           (right (extremum xs #'>))
           (top (extremum ys #'>))
           (counts (make-hash-table))
           (infinite (make-hash-set)))
      (iterate
        (for-nested ((x :from left :to right)
                     (y :from bottom :to top)))
        (for closest = (closest (complex x y) coordinates))
        (when closest
          (incf (gethash closest counts 0))
          (when (or (= left x) (= bottom y)
                    (= right x) (= top y))
            (hset-insert! infinite closest))))
      (values
        (iterate
          (for (point size) :in-hashtable counts)
          (unless (hset-contains-p infinite point)
            (maximizing size)))
        (iterate
          (for-nested ((x :from left :to right)
                       (y :from bottom :to top)))
          (for point = (complex x y))
          (for total-distance = (summation coordinates :key (curry #'manhattan-distance point)))
          (counting (< total-distance 10000)))))))


(define-problem (2018 7) (data read-lines-from-file)
  (labels ((parse-line (line)
             (ppcre:register-groups-bind
                 (((rcurry #'aref 0) requirement target))
                 (#?/Step (\w) must be finished before step (\w) can begin./ line)
               (list target requirement)))
           (make-graph (edges)
             (let* ((vertices (remove-duplicates (flatten-once edges)))
                    (graph (digraph:make-digraph :initial-vertices vertices)))
               (dolist (edge edges)
                 (digraph:insert-edge graph (first edge) (second edge)))
               graph))
           (char-number (char)
             (1+ (- (char-code char) (char-code #\A))))
           (task-length (task)
             (+ 60 (char-number task)))
           (decrement-workers (workers)
             (gathering
               (do-array (worker workers)
                 (when worker
                   (when (zerop (decf (cdr worker)))
                     (gather (car worker))
                     (setf worker nil)))))))
    (values
      (let ((graph (make-graph (mapcar #'parse-line data))))
        ;; (digraph.dot:draw graph)
        (recursively ((result nil))
          (if (emptyp graph)
            (coerce (nreverse result) 'string)
            (let ((next (extremum (digraph:leafs graph) 'char<)))
              (digraph:remove-vertex graph next)
              (recur (cons next result))))))
      (iterate
        (with graph = (make-graph (mapcar #'parse-line data)))
        ;; workers is a vector of (task . remaining-time) conses,
        ;; or NILs for idle workers
        (with workers = (make-array 5 :initial-element nil))
        ;; (pr elapsed workers)
        (for elapsed :from 0)
        (for finished-tasks = (decrement-workers workers))
        (map nil (curry #'digraph:remove-vertex graph) finished-tasks)
        (for current-tasks = (remove nil (map 'list #'car workers)))
        (for available-tasks = (-<> graph
                                 digraph:leafs
                                 (set-difference <> current-tasks)
                                 (sort <> 'char<)))
        (do-array (worker workers)
          (when (null worker)
            (when-let ((task (pop available-tasks)))
              (setf worker (cons task (task-length task))))))
        (when (and (emptyp graph) (every #'null workers))
          (return elapsed))))))


(define-problem (2018 8) (data)
  (labels
      ((make-node (children metadata) (cons metadata children))
       (children (node) (cdr node))
       (metadata (node) (car node))
       (read-node (stream)
         (let* ((children-count (read stream))
                (metadata-count (read stream))
                (children (iterate
                            (repeat children-count)
                            (collect (read-node stream) :result-type vector)))
                (metadata (iterate
                            (repeat metadata-count)
                            (collect (read stream)))))
           (make-node children metadata)))
       (node-value (node &aux (children (children node)))
         (if (emptyp children)
           (summation (metadata node))
           (iterate
             (for meta :in (metadata node))
             (for index = (1- meta))
             (when (array-in-bounds-p children index)
               (summing (node-value (aref children index))))))))
    (let ((root (read-node data)))
      (values
        (recursively ((node root))
          (+ (summation (metadata node))
             (summation (children node) :key #'recur)))
        (node-value root)))))


(define-problem (2018 9) (data read-file-into-string)
  (ppcre:register-groups-bind
      ((#'parse-integer players marbles))
      (#?/(\d+) players\D*(\d+) points/ data)
    (labels
        ((play (players marbles)
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
             (extremum elves '>))))
      #+sbcl (sb-ext:gc :full t)
      (values (play players marbles)
              (play players (* marbles 100))))))
