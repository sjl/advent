(defpackage :advent/2019/11 #.cl-user::*advent-use*)
(in-package :advent/2019/11)

(defun run-robot (program &key (origin 'black))
  (let ((panels (make-hash-table))
        (mode '#1=(paint walk . #1#))
        (pos #c(0 0))
        (heading #c(0 1)))
    (labels ((color ()
               (gethash pos panels 0))
             (paint (color)
               (setf (gethash pos panels) color))
             (turn (direction)
               (mulf heading
                     (ecase direction
                       (0 #c(0 1))
                       (1 #c(0 -1)))))
             (walk (direction)
               (turn direction)
               (incf pos heading))
             (exec (input)
               (ecase (pop mode)
                 (paint (paint input))
                 (walk (walk input)))))
      (paint (ecase origin
               (black 0)
               (white 1)))
      (advent/intcode:run program :input #'color :output #'exec))
    panels))

(defun draw-panels (panels)
  (draw-bitmap (alexandria:hash-table-alist panels)
               "out/2019-11.pbm"
               :flip-vertically t))

(define-problem (2019 11) (data read-numbers) (1907)
  (draw-panels (run-robot data :origin 'white))
  (hash-table-count (run-robot data)))


#; Scratch --------------------------------------------------------------------
