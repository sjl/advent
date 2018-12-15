(defpackage :advent/2018/12 #.cl-user::*advent-use*)
(in-package :advent/2018/12)
(named-readtables:in-readtable :interpol-syntax)


(defstruct pots data min max)

(defmethod print-object ((o pots) s)
  (print-unreadable-object (o s :type t)
    (format s "~D to ~D: ~A" (pots-min o) (pots-max o)
            (iterate (for i :from (pots-min o) :to (pots-max o))
                     (collect (if (zerop (gethash i (pots-data o) 0)) #\. #\#)
                              :result-type 'string)))))
(defun rune-bit (rune)
  (ecase rune
    (#\# 1)
    (#\. 0)))

(defun runes-to-bits (runes)
  (map 'list #'rune-bit runes))

(defun list-to-hash-table (list)
  (iterate (for value :in list)
           (for i :from 0)
           (when (plusp value)
             (collect-hash (i value) :test 'eq))))

(defun surroundings-key (ll l x r rr)
  (declare (type bit ll l x r rr))
  (+ (* (expt 2 0) ll)
     (* (expt 2 1) l)
     (* (expt 2 2) x)
     (* (expt 2 3) r)
     (* (expt 2 4) rr)))

(defun parse-initial-line (line)
  (ppcre:register-groups-bind
      (state)
      (#?r"initial state: (\S+)" line)
    (list-to-hash-table (runes-to-bits state))))

(defun parse-rule (line)
  (ppcre:register-groups-bind
      (surroundings result)
      (#?r"(\S+) => (\S)" line)
    (values (apply #'surroundings-key (runes-to-bits surroundings))
            (rune-bit (aref result 0)))))

(defun surroundings (state i)
  (let ((data (pots-data state)))
    (surroundings-key (gethash (- i 2) data 0)
                      (gethash (- i 1) data 0)
                      (gethash i data 0)
                      (gethash (+ i 1) data 0)
                      (gethash (+ i 2) data 0))))

(defun tick (state rules)
  (with-slots (data min max) state
    (iterate
      (for i :from (- min 2) :to (+ max 2))
      (for current = (gethash i data 0))
      (for surroundings = (surroundings state i))
      (for next = (aref rules surroundings))
      (when (plusp next)
        (minimizing i :into next-min)
        (maximizing i :into next-max))
      (when (/= current next)
        (if (plusp next)
          (collect i :into add)
          (collect i :into rem)))
      (finally
        (dolist (i add) (setf (gethash i data) 1))
        (dolist (i rem) (remhash i data))
        (setf min next-min
              max next-max)
        state))))

(define-problem (2018 12) (data)
  (let* ((initial (parse-initial-line (read-line data)))
         (state (prog1 (make-pots :data initial
                                  :min (extremum (hash-table-keys initial) '<)
                                  :max (extremum (hash-table-keys initial) '>))
                  (read-line data)))
         (rules (iterate
                  (with rules = (make-array (expt 2 5) :initial-element 1))
                  (for line :in-stream data :using #'read-line)
                  (until (string= "" line))
                  (for (values key result) = (parse-rule line))
                  (setf (aref rules key) result)
                  (finally (return rules)))))
    (values
      (progn (do-repeat 20
               (tick state rules))
             (summation (hash-table-keys (pots-data state))))
      (progn (dotimes (i (- 500000 20))
               (when (dividesp i 1000)
                 (pr i))
               (tick state rules))
             (summation (hash-table-keys (pots-data state)))))))
