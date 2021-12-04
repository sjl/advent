(advent:defpackage* :advent/2021/04)
(in-package :advent/2021/04)


(defun cell (num) (cons num nil))
(defun num (cell) (car cell))
(defun markedp (cell) (cdr cell))
(defun unmarkedp (cell) (not (markedp cell)))
(defun mark (cell) (setf (cdr cell) t))

(defun read-bingo-numbers (stream)
  (read-numbers-from-string (read-line stream)))

(defun read-board (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (peek-char t stream nil)
  (with-eof-handled (stream eof-error-p eof-value)
    (do-array (v (make-array '(5 5)))
      (setf v (cell (parse-integer (read-word stream)))))))

(defun parse (stream)
  (values (read-bingo-numbers stream)
          (read-and-collect stream #'read-board)))

(defun copy-board (board)
  (do-array (cell (alexandria:copy-array board))
    (setf cell (cons (car cell) (cdr cell)))))

(defun print-board (board)
  (print-2d-array board :printer (lambda (cell)
                                   (when (markedp cell) (green) (bold))
                                   (format t "~3D" (num cell))
                                   (when (markedp cell) (reset)))))


(defun print-boards (boards &optional heading)
  (when heading (write-line heading))
  (dolist (board boards)
    (print-board board)
    (terpri)))

(defun mark-number-on-board (n board)
  (do-array (cell board)
    (when (= n (num cell))
      (return (mark cell)))))

(defun mark-number-on-boards (n boards)
  (dolist (board boards)
    (mark-number-on-board n board)))

(defun winning-row-p (board row)
  (iterate (for col :below 5) (always (markedp (aref board row col)))))

(defun winning-col-p (board col)
  (iterate (for row :below 5) (always (markedp (aref board row col)))))

(defun winning-board-p (board)
  (or (iterate (for row :below 5) (thereis (winning-row-p board row)))
      (iterate (for col :below 5) (thereis (winning-col-p board col)))))

(defun play-first (numbers boards)
  (iterate
    (with boards = (mapcar #'copy-board boards))
    (for n :in numbers)
    (mark-number-on-boards n boards)
    (dolist (board boards)
      (when (winning-board-p board)
        (return-from play-first (values n board))))))

(defun play-last (numbers boards)
  (iterate
    (with boards = (mapcar #'copy-board boards))
    (for n :in numbers)
    (mark-number-on-boards n boards)
    (if (null (rest boards))
      (when (winning-board-p (first boards)) ;; Run last board to completion.
        (return-from play-last (values n (first boards))))
      (setf boards (delete-if #'winning-board-p boards))))) ;; Still pruning boards.

(defun unmarked-sum (board)
  (iterate (for cell :across-flat-array board)
           (when (unmarkedp cell)
             (summing (num cell)))))

(defun score (last-number board)
  (* last-number (unmarked-sum board)))

(define-problem (2021 4) (data) (49860 24628)
  (multiple-value-bind (numbers boards) (parse data)
    (values
      (multiple-value-call #'score (play-first numbers boards))
      (multiple-value-call #'score (play-last numbers boards)))))








#; Scratch --------------------------------------------------------------------
