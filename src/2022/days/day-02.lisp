(advent:defpackage* :advent/2022/02)
(in-package :advent/2022/02)
(named-readtables:in-readtable losh)

(defun char->move (code)
  (ecase code
    ((#\A #\X) :rock)
    ((#\B #\Y) :paper)
    ((#\C #\Z) :scissors)))

(defun beatsp (a b)
  "Return whether `a` beats `b.`"
  (eql b (ecase a
           (:rock :scissors)
           (:paper :rock)
           (:scissors :paper))))

(defun result (them us)
  (cond ((eql them us) :draw)
        ((beatsp us them) :win)
        (t :lose)))

(defun move-score (move)
  (gethash move {:rock 1 :paper 2 :scissors 3}))

(defun result-score (result)
  (gethash result {:win 6 :draw 3 :lose 0}))

(defun score (them us)
  (+ (move-score us)
     (result-score (result them us))))

(defun parse (lines)
  (loop :for line :in lines
        :collect (cons (char->move (char line 0))
                       (char->move (char line 2)))))

(defun part-1 (data)
  (loop :for (them . us) :in data :summing (score them us)))

(define-problem (2022 2) (data read-lines) ()
  (values (part-1 (parse data))))


;; #; Scratch --------------------------------------------------------------------
