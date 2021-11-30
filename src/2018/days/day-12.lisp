(advent:defpackage* :advent/2018/12)
(in-package :advent/2018/12)
(named-readtables:in-readtable :interpol-syntax)


;;;; Pots ---------------------------------------------------------------------
(defstruct pots data min max)

(defmethod print-object ((o pots) s)
  (print-unreadable-object (o s :type t)
    (format s "~D to ~D: ~A" (pots-min o) (pots-max o)
            (iterate (for i :from (pots-min o) :to (pots-max o))
                     (collect (if (plusp (pot o i)) #\# #\.)
                              :result-type 'string)))))


(defun-inline pot (pots i)
  (if (hset-contains-p (pots-data pots) i)
    1
    0))

(defun add-pot (pots i)
  (hset-insert! (pots-data pots) i))

(defun rem-pot (pots i)
  (hset-remove! (pots-data pots) i))

(defun surroundings (pots i)
  (make-array 5
    :element-type 'bit
    :initial-contents (list (pot pots (- i 2))
                            (pot pots (- i 1))
                            (pot pots i)
                            (pot pots (+ i 1))
                            (pot pots (+ i 2)))))

(defun score (pots)
  (summation (hset-elements (pots-data pots))))


;;;; Input Parsing ------------------------------------------------------------
(defun rune-bit (rune)
  (ecase rune
    (#\# 1)
    (#\. 0)))

(defun runes-to-bits (runes)
  (map 'bit-vector #'rune-bit runes))

(defun vector-to-hash-set (vector &key (test #'eql))
  (iterate
    (with result = (make-hash-set :test test))
    (for value :in-vector vector :with-index i)
    (when (plusp value)
      (hset-insert! result i))))

(defun parse-initial-line (line)
  (ppcre:register-groups-bind
      (state)
      (#?r"initial state: (\S+)" line)
    (_ state
      runes-to-bits
      (positions-if #'plusp _)
      (make-hash-set :initial-contents _))))

(defun parse-rule (line)
  (ppcre:register-groups-bind
      (surroundings result)
      (#?r"(\S+) => (\S)" line)
    (values (runes-to-bits surroundings)
            (rune-bit (aref result 0)))))

(defun read-problem (stream)
  (let* ((initial (parse-initial-line (read-line stream)))
         (state (prog1 (make-pots :data initial
                                  :min (extremum (hset-elements initial) '<)
                                  :max (extremum (hset-elements initial) '>))
                  (read-line stream)))
         (rules (iterate
                  (for line :in-stream stream :using #'read-line)
                  (unless (string= "" line)
                    (for (values key result) = (parse-rule line))
                    (collect-hash (key result) :test #'equal)))))
    (values state rules)))


;;;; Solve --------------------------------------------------------------------
(defun tick (pots rules)
  (with-slots (min max) pots
    (iterate
      (for i :from (- min 2) :to (+ max 2))
      (for current = (pot pots i))
      (for surroundings = (surroundings pots i))
      (for next = (gethash surroundings rules))
      (when (plusp next)
        (minimizing i :into next-min)
        (maximizing i :into next-max))
      (when (/= current next)
        (if (plusp next)
          (collect i :into add)
          (collect i :into rem)))
      (finally
        (map nil (curry #'add-pot pots) add)
        (map nil (curry #'rem-pot pots) rem)
        (setf min next-min
              max next-max)
        pots))))

(define-problem (2018 12) (data)
    ()  ;; can't really test noninteractively :(
  (multiple-value-bind (pots rules) (read-problem data)
    (values
      (progn
        (do-repeat 20
          (tick pots rules))
        (score pots))
      (iterate
        (for tick :from 20)
        (format t "~%After ~D tick~:P:~%~A~%score: ~D~%> " tick pots (score pots))
        (force-output)
        (for input = (read-line))
        (until (string= "q" input))
        (tick pots rules)))))

(defun part-2 ()
  (let* ((score-per-tick 20)
         (starting-tick 350)
         (starting-value 7508)
         (ticks (- 50000000000 starting-tick)))
    (+ starting-value (* score-per-tick ticks))))
