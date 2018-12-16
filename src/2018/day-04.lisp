(defpackage :advent/2018/04 #.cl-user::*advent-use*)
(in-package :advent/2018/04)
(named-readtables:in-readtable :interpol-syntax)

;; This problem gets much easier after you've unlocked the second question and
;; realize you can solve everything by building histograms of each guard's
;; sleeping minutes.

(defun parse-line (line)
  "Parse `line` into `(minute :event id?)`"
  (ppcre:register-groups-bind
      ((#'parse-integer minute) event)
      (#?r"\[\d+-\d+-\d+ \d+:(\d+)\] (.*)" line)
    (list* minute
           (cond
             ((string= "falls asleep" event) (list :sleep nil))
             ((string= "wakes up" event) (list :wake nil))
             (t (ppcre:register-groups-bind
                    ((#'parse-integer id))
                    (#?r"Guard #(\d+) begins shift" event)
                  (list :guard id)))))))

(defun sleep-intervals (events &aux start guard)
  "Transform `events` into a list of `(guard-id start end)`"
  (iterate
    (for (minute event id?) :in events)
    (ecase event
      (:guard (setf guard id?))
      (:wake (collect (list guard start minute)))
      (:sleep (setf start minute)))))

(defun guard-histograms (intervals)
  "Return a hash-table of histograms of the guards' sleeping minutes."
  (iterate
    (with result = (make-hash-table))
    (for (guard start end) :in intervals)
    (for histogram = (ensure-gethash guard result
                                     (make-array 60 :initial-element 0)))
    (do-range ((minute start end))
      (incf (aref histogram minute)))
    (finally (return result))))


(define-problem (2018 4) (data read-lines)
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
                 predictable-guard-time)))))


