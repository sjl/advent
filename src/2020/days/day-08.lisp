(advent:defpackage* :advent/2020/08)
(in-package :advent/2020/08)

(defun parse-line (line)
  (ppcre:register-groups-bind ((#'ensure-keyword op) (#'parse-integer arg))
      ("^(\\w+) ([-+]\\d+)$" line)
    (cons op arg)))

(defclass* console ()
  (acc pc mem seen))

(defun make-console (program)
  (make-instance 'console
    :mem program
    :seen (make-array (length program))))

(defun run-console (console)
  (setf (acc console) 0
        (pc console) 0)
  (fill (seen console) nil)
  (iterate
    (with term = (length (mem console)))
    (with seen = (seen console))
    (for i = (pc console))
    (cond ((= i term) (return :terminated))
          ((aref seen i) (return :looped))
          (t (setf (aref seen i) t)))
    (for (op . arg) = (aref (mem console) i))
    (incf (pc console))
    (ecase op
      (:nop)
      (:acc (incf (acc console) arg))
      (:jmp (incf (pc console) (1- arg))))))

(defun part1 (program)
  (let ((console (make-console program)))
    (run-console console)
    (acc console)))

(defun part2 (program)
  (let ((console (make-console program)))
    (iterate (for inst :in-vector program)
             (for op = (car inst))
             (when (member op '(:nop :jmp))
               (setf (car inst) (ecase op
                                  (:nop :jmp)
                                  (:jmp :nop)))
               (when (eql :terminated (run-console console))
                 (return-from part2 (acc console)))
               (setf (car inst) op)))))

(define-problem (2020 8) (data read-lines) (1217 501)
  (let ((program (map 'vector #'parse-line data)))
    (values (part1 program)
            (part2 program))))


#; Scratch --------------------------------------------------------------------


