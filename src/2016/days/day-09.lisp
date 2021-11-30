(advent:defpackage* :advent/2016/09)
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

(defun .letter ()
  (smug:.is #'alpha-char-p))

(defun .digit ()
  (smug:.is #'digit-char-p))

(defun .number ()
  (smug:.let* ((s (smug:.map 'string (.digit))))
    (smug:.identity (parse-integer s))))

(defun .header-guts ()
  (smug:.let* ((length (.number))
               (_ (smug:.char= #\x))
               (repetitions (.number)))
    (smug:.identity (cons length repetitions))))

(defun .header ()
  (smug:.prog2 (smug:.char= #\left_parenthesis)
               (.header-guts)
               (smug:.char= #\right_parenthesis)))

(defun .raw% (n)
  (if (zerop n)
    (smug:.identity nil)
    (smug:.let* ((x (smug:.item))
                 (more (.raw (1- n))))
      (smug:.identity (cons x more)))))

(defun .raw (n &key (result-type 'list))
  (smug:.let* ((result (.raw% n)))
    (smug:.identity (coerce result result-type))))

(defun .v1-compressed-chunk ()
  (smug:.let* ((header (.header))
               (contents (.raw (car header) :result-type 'string)))
    (smug:.identity (vector (cdr header) contents))))

(defun .data ()
  (smug:.map 'string (.letter)))

(defun .v1 ()
  (smug:.first
    (smug:.map 'list (smug:.or (.v1-compressed-chunk)
                               (.data)))))

(defun v1 (data)
  (smug:parse (.v1) data))

(v1 "(10x2)(3x1)abcdefghijklmno")

(defun v2 (data)
  (recursively ((tree (v1 data)))
    (etypecase tree
      (list (mapcar #'recur tree))
      (string tree)
      (vector (vector (aref tree 0)
                      (recur (v1 (aref tree 1))))))))

(v2 "(10x2)(3x1)abcdefghijkl(2x10)mno")
