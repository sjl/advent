(advent:defpackage* :advent/2019/17)
(in-package :advent/2019/17)

(defclass* world ()
  ((tiles :initform (make-hash-table))
   robot-pos
   robot-dir))

(defun record-string (program)
  (gathering-vector (:element-type 'character)
    (advent/intcode:run program :output (compose #'gather #'code-char))))

(defun parse-char (char)
  (case char
    (#\^ (values #\# #c( 0 -1)))
    (#\v (values #\# #c( 0  1)))
    (#\< (values #\# #c(-1  0)))
    (#\> (values #\# #c( 1  0)))
    (t   (values char nil))))

(defun robot-char (dir)
  (ecase dir
    (#c( 0 -1) #\^)
    (#c( 0  1) #\v)
    (#c(-1  0) #\<)
    (#c( 1  0) #\>)))

(defun parse-map (string)
  (iterate
    (with world = (make-instance 'world))
    (for line :in (split-sequence:split-sequence #\newline string
                                                 :remove-empty-subseqs t))
    (for row :from 0)
    (iterate
      (for char :in-string line :with-index col)
      (for pos = (complex row col))
      (for (values tile dir) = (parse-char char))
      (setf (gethash pos (tiles world)) tile)
      (when dir
        (setf (robot-pos world) pos
              (robot-dir world) dir)))
    (returning world)))

(defun intersectionp (world pos)
  (every (lambda (x)
           (eql #\# (gethash x (tiles world))))
         (manhattan-neighborhood pos)))

(defun intersections (world)
  (remove-if-not (curry #'intersectionp world)
                 (alexandria:hash-table-keys (tiles world))))

(defun alignment-parameter (pos)
  (* (realpart pos)
     (imagpart pos)))

(defun draw-world (world)
  (print-hash-table-map (tiles world)
    :swap-axes t
    :flip-y t
    :extra (lambda (pos)
             (cond ((= pos (robot-pos world))
                    (robot-char (robot-dir world)))
                   ((intersectionp world pos) #\O)))))

(define-problem (2019 17) (data read-numbers) (7328)
  ;; (advent/intcode::disassemble-program (advent/intcode:init data))
  (let ((world (parse-map (record-string data))))
    ;; (draw-world world)
    (summation (intersections world) :key #'alignment-parameter)))

#; Scratch --------------------------------------------------------------------


