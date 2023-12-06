(advent:defpackage* :advent/2022/04)
(in-package :advent/2022/04)


(defclass* card ()
  (id winning contents))

(defmethod print-object ((o card) s)
  (print-unreadable-object (o s :type t)
    (format s "~D: ~S ~S" (id o) (winning o) (contents o))))

(defun parse-line (line)
  (destructuring-bind (card nums) (str:split #\: line)
    (destructuring-bind (winning contents) (str:split #\| nums)
      (make-instance 'card
        :id (parse-integer (second (str:words card)))
        :winning (mapcar #'parse-integer (str:words winning))
        :contents (mapcar #'parse-integer (str:words contents))))))

(defun win-count (card)
  (length (intersection (contents card) (winning card))))

(defun score (card)
  (let ((w (win-count card)))
    (if (zerop w)
      0
      (expt 2 (1- w)))))

(defun copies (cards)
  (iterate
    (with-result copies = (make-array (length cards) :initial-element 1))
    (for card :in cards)
    (for i :from 0)
    (for n = (aref copies i))
    (loop :repeat (win-count card)
          :for j :from (1+ i)
          :do (incf (aref copies j) n))))

(defun part-1 (cards)
  (summation cards :key #'score))

(defun part-2 (cards)
  (summation (copies cards)))

(define-problem (2023 4) (data read-lines) (23441 5923918)
  (let ((cards (mapcar #'parse-line data)))
    (values (part-1 cards)
            (part-2 cards))))

#; Scratch --------------------------------------------------------------------

