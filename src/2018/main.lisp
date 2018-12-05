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
