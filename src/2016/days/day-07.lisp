(advent:defpackage* :advent/2016/07)
(in-package :advent/2016/07)

(defun read-word (stream)
  (iterate (while (alpha-char-p (peek-char nil stream nil #\!)))
           (collect (read-char stream) :result-type 'string)))

(defun parse-address (address)
  (let ((stream (make-string-input-stream address)))
    (recursively ()
      (iterate
        (case (peek-char nil stream nil)
          (#\[ (progn (read-char stream)
                      (collect (recur))))
          (#\] (progn (read-char stream)
                      (finish)))
          ((nil) (finish))
          (t (collect (read-word stream))))))))

(defun abbap (word)
  (iterate
    (for (a b) :matching "(\\w)(\\w)\\2\\1" :against word)
    (thereis (string/= a b))))

(defun find-babs (word)
  (iterate
    (for (a b) :matching "(\\w)(\\w)\\1" :against word :overlap t)
    (when (string/= a b)
      (adjoining (concatenate 'string b a b) :test #'string=))))

(defun tree-find-if (predicate tree)
  (typecase tree
    (null nil)
    (cons (or (tree-find-if predicate (car tree))
              (tree-find-if predicate (cdr tree))))
    (t (if (funcall predicate tree)
         tree
         nil))))

(defun tlsp (address)
  (and (some #'abbap (remove-if-not #'stringp address))
       (not (tree-find-if #'abbap (remove-if #'stringp address)))))

(defun sslp (address)
  (iterate
    (with supers = (remove-if-not #'stringp address))
    (with hypers = (remove-if #'stringp address))
    (for aba :in (mapcan #'find-babs supers))
    (finding t :such-that (tree-find-if (curry #'str:containsp aba) hypers))))

(define-problem (2016 7) (data read-lines) (110 242)
  (values
    (count-if #'tlsp data :key #'parse-address)
    (count-if #'sslp data :key #'parse-address)))



#; Scratch --------------------------------------------------------------------
