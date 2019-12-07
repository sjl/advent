(defpackage :advent/2019/07 #.cl-user::*advent-use*
  (:shadow :queue))
(in-package :advent/2019/07)

(defun queue (&key initial-contents)
  (let-result (queue (make-instance 'jpl-queues:synchronized-queue
                       :queue (make-instance 'jpl-queues:unbounded-fifo-queue)))
    (dolist (el initial-contents)
      (jpl-queues:enqueue el queue))))

(defun make-amplifier (program input-queue output-queue)
  (advent/intcode:init program
                       :input (rcurry #'jpl-queues:dequeue input-queue)
                       :output (rcurry #'jpl-queues:enqueue output-queue)))

(defun make-amplifiers (program phases)
  (iterate
    (with top-queue = (queue :initial-contents (list (first phases) 0)))
    (for (nil next-phase) :on phases)
    (for output-queue = (if next-phase
                          (queue :initial-contents (list next-phase))
                          top-queue))
    (for input-queue :previous output-queue :initially top-queue)
    (collect (make-amplifier program input-queue output-queue) :into amps)
    (returning amps top-queue)))

(defun run-amplifiers (program phases)
  (multiple-value-bind (amplifiers queue) (make-amplifiers program phases)
    (-<> amplifiers
      (mapcar (lambda (amp)
                (bt:make-thread (curry #'advent/intcode:run-machine amp)
                                :name "Amplifier Thread"))
              <>)
      (map nil #'bt:join-thread <>))
    (jpl-queues:dequeue queue)))

(defun maximum-permutation (function sequence)
  (iterate (alexandria:map-permutations
             (lambda (el)
               (maximizing (funcall function el)))
             sequence)
           (finish)))

(define-problem (2019 7) (data read-numbers) (46248 54163586)
  (values
    (maximum-permutation (curry #'run-amplifiers data) '(0 1 2 3 4))
    (maximum-permutation (curry #'run-amplifiers data) '(5 6 7 8 9))))


#; Scratch --------------------------------------------------------------------

(run '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(run '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0))

(run '(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5))

(run '(3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54
       1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56
       -1 56 1005 56 6 99 0 0 0 0 10))

(defun kill-amplifiers ()
  (-<> (bt:all-threads)
    (remove "Amplifier Thread" <> :test-not #'string= :key #'bt:thread-name)
    (map nil #'bt:destroy-thread <>)))

(kill-amplifiers)
