(advent:defpackage* :advent/2019/13)
(in-package :advent/2019/13)

(defun tile (val)
  (aref #(nil wall block paddle ball) val))

(defun tile-char (tile)
  (ecase tile
    ((nil) #\Space)
    (ball #\O)
    (paddle #\=)
    (block #\B)
    (wall #\#)))

(define-condition* screen-updated () (screen score))

(defun run-game (program &key (input (constantly 0)))
  (let ((screen (make-hash-table))
        (state '#1=(x y draw . #1#))
        (score 0)
        x y ball paddle)
    (advent/intcode:run program
      :input (lambda ()
               (funcall input ball paddle))
      :output (lambda (val)
                (ecase (pop state)
                  (x (setf x val))
                  (y (setf y val))
                  (draw (if (and (= x -1) (= y 0))
                          (setf score val)
                          (let ((pos (complex x y))
                                (tile (tile val)))
                            (case tile
                              (ball (setf ball pos))
                              (paddle (setf paddle pos)))
                            (setf (gethash pos screen) (tile val))))
                        (signal 'screen-updated
                                :screen screen
                                :score score)))))
    (values screen score)))


(defun draw-screen (update)
  (clear)
  (green)
  (format t "SCORE: ~D~%" (score update))
  (reset)
  (print-hash-table-map (screen update) :key #'tile-char :default nil :flip-y t))

(defun play-interactively (program)
  (setf (elt program 0) 2)
  (flet ((input (ball paddle &aux (dir (read)))
           (declare (ignore ball paddle))
           (ccase dir
             (a -1)
             (s 0)
             (d 1))))
    (handler-bind ((screen-updated #'draw-screen))
      (run-game program :input #'input)))
  (values))

(defun play-automatically (program &key draw sleep)
  (setf (elt program 0) 2)
  (labels ((input (ball paddle)
             (when sleep (sleep sleep))
             (signum (- (x ball) (x paddle))))
           (play ()
             (nth-value 1 (run-game program :input #'input))))
    (if draw
      (handler-bind ((screen-updated #'draw-screen))
        (play))
      (play))))

(defvar *data* nil)

(define-problem (2019 13) (data read-numbers) (255)
  (setf *data* data)
  (values (count 'block (hash-table-values (run-game data)))
          (play-automatically data)))


#; Scratch --------------------------------------------------------------------

(play-interactively *data*)

