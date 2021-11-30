(advent:defpackage* :advent/2019/15 (:shadow :step))
(in-package :advent/2019/15)

(defparameter *animate* nil)
(defparameter *directions* '(#c(0 1) #c(0 -1) #c(-1 0) #c(1 0)))

(defclass* repair-machine ()
  ((machine)
   (input)
   (output)
   (pos :initform 0)
   (world :initform (make-hash-table))
   (oxygen-position)))

(defmethod initialize-instance :after ((machine repair-machine) &key)
  (setf (gethash 0 (world machine)) #\.))

(defun make-repair-machine (program)
  (let ((input (make-queue))
        (output (make-queue)))
    (make-instance 'repair-machine
      :machine (advent/intcode:init program
                                    :input (curry #'dequeue input)
                                    :output (rcurry #'enqueue output))
      :input input
      :output output)))

(defun step (machine)
  "Run `machine` until it produces some output."
  (loop :while (queue-empty-p (output machine))
        :do (advent/intcode:step (machine machine))))

(defun print-world (machine)
  (print-hash-table-map
    (world machine)
    :default #\space
    :pad 1
    :extra (lambda (pos)
             (when (= (pos machine) pos)
               #\@))))

(defun dir->int (dir)
  (ecase dir
    (#c( 0  1) 1)
    (#c( 0 -1) 2)
    (#c(-1  0) 3)
    (#c( 1  0) 4)))

(defun int->reply (int)
  "Parse `int`, returning `(values result actually-moved found-oxygen)`."
  (ecase int
    (0 (values 'wall nil nil))
    (1 (values 'ok t nil))
    (2 (values 'oxygen t t))))

(defun result->char (reply)
  (ecase reply
    (wall #\#)
    (ok #\.)
    (oxygen #\O)))

(defun move (machine direction)
  (let ((target (+ (pos machine) direction)))
    (enqueue (dir->int direction) (input machine))
    (step machine)
    (multiple-value-bind (result actually-moved found-oxygen)
        (int->reply (dequeue (output machine)))
      (setf (gethash target (world machine))
            (result->char result))
      (when actually-moved
        (setf (pos machine) target))
      (when found-oxygen
        (setf (oxygen-position machine) target))
      actually-moved)))

(defun explore (machine)
  (labels ((seenp (pos)
             (nth-value 1 (gethash pos (world machine))))
           (seen-direction-p (pos direction)
             (seenp (+ pos direction)))
           (unseen-directions (pos)
             (remove-if (curry #'seen-direction-p pos) *directions*)))
    (iterate
      (with frontier = (list (unseen-directions 0)))
      (with path = '())
      (when *animate*
        (clear)
        (print-world machine)
        (sleep 1/60))
      (until (null frontier))
      (if (null (first frontier))
        (progn (pop frontier) ; backtrack
               (when path (move machine (pop path))))
        (progn (for dir = (pop (first frontier)))
               (when (move machine dir)
                 (push (unseen-directions (pos machine)) frontier)
                 (push (- dir) path)))))))

(defun find-oxygen (machine)
  (flet ((ref (pos)
           (gethash pos (world machine))))
    (astar :start 0
           :neighbors (lambda (pos)
                        (remove #\# (manhattan-neighbors pos) :key #'ref))
           :goalp (lambda (pos)
                    (char= #\O (ref pos)))
           :cost (constantly 1)
           :heuristic (curry #'manhattan-distance (oxygen-position machine))
           :test #'eql)))

(defun flood-oxygen (machine)
  (labels ((ref (pos)
             (gethash pos (world machine)))
           (empty-neighbors (pos)
             (remove #\. (manhattan-neighbors pos) :key #'ref :test-not #'char=))
           (oxygenate (pos)
             (setf (gethash pos (world machine)) #\O)))
    (iterate
      (with frontier = (empty-neighbors (oxygen-position machine)))
      (until (null frontier))
      (counting t)
      (map nil #'oxygenate frontier)
      (setf frontier (remove-duplicates (mapcan #'empty-neighbors frontier))))))

(define-problem (2019 15) (data read-numbers) (222 394)
  (let ((machine (make-repair-machine data)))
    (explore machine)
    (values (1- (length (find-oxygen machine)))
            (flood-oxygen machine))))

#; Scratch --------------------------------------------------------------------

(let ((*animate* t))
  (run))
