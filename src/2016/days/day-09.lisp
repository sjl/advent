(defpackage :advent/2016/09 #.cl-user::*advent-use*)
(in-package :advent/2016/09)


;;; This is ugly, but I just got bored of this one.  Tried both Smug and ESRAP
;;; and they were both miserable, so I just gave up and wrote these shitty
;;; readers.

(defun parse (stream)
  (recursively ((stream stream))
    (flet ((read-compressed-chunk ()
             (ppcre:register-groups-bind
                 ((#'parse-integer length repetitions))
                 ("(\\d+)x(\\d+)" (read-to #\right_parenthesis stream nil))
               (let ((chunk (make-string length)))
                 (read-sequence chunk stream)
                 (vector repetitions (recur (make-string-input-stream chunk)))))))
      (iterate
        (case (peek-char nil stream nil)
          ((nil #\Newline) (finish))
          (#\Left_Parenthesis (collect (read-compressed-chunk)))
          (t (collect (read-char stream))))))))

(defun score (tree)
  (etypecase tree
    (character 1)
    (list (summation tree :key #'score))
    (vector (* (aref tree 0)
               (summation (aref tree 1) :key #'score)))))

(defun decompress (stream)
  (with-output-to-string (*standard-output*)
    (flet ((read-compressed-chunk ()
             (ppcre:register-groups-bind
                 ((#'parse-integer length repetitions))
                 ("(\\d+)x(\\d+)" (read-to #\right_parenthesis stream nil))
               (let ((chunk (make-string length)))
                 (read-sequence chunk stream)
                 (str:repeat repetitions chunk)))))
      (iterate
        (case (peek-char nil stream nil)
          ((nil #\Newline) (finish))
          (#\Left_Parenthesis (write-string (read-compressed-chunk)))
          (t (write-char (read-char stream))))))))

(define-problem (2016 9) (data alexandria:read-stream-content-into-string)
    (97714 10762972461)
  (values (length (decompress (make-string-input-stream data)))
          (score (parse (make-string-input-stream data)))))


#; Scratch --------------------------------------------------------------------
