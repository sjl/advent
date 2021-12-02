(advent:defpackage* :advent/2021/02)
(in-package :advent/2021/02)

(defun parse (stream)
  (iterate (for line :in-stream stream :using #'read-line)
           (ppcre:register-groups-bind ((#'ensure-keyword command) (#'parse-integer n))
               ("(\\w+) (\\d+)" line)
             (collect (cons command n)))))

(defun horiz (pos) (realpart pos))
(defun depth (pos) (imagpart pos))

(defun part1 (course)
  (iterate (for (cmd . n) :in course)
           (summing (ecase cmd
                      (:forward (complex n 0))
                      (:down    (complex 0 n))
                      (:up      (complex 0 (- n)))))))

(defun part2 (course)
  (iterate (with pos = 0)
           (with aim = 0)
           (for (cmd . n) :in course)
           (ecase cmd
             (:forward (incf pos (complex n (* n aim))))
             (:down    (incf aim n))
             (:up      (decf aim n)))
           (returning pos)))

(define-problem (2021 2) (data) (1660158 1604592846)
  (let* ((course (parse data))
         (p1 (part1 course))
         (p2 (part2 course)))
    (values (* (horiz p1) (depth p1))
            (* (horiz p2) (depth p2)))))

#; Scratch --------------------------------------------------------------------
