(advent:defpackage* :advent/2017/09)
(in-package :advent/2017/09)

(define-problem (2017 9) (stream) (15922 7314)
  (let ((garbage-total 0))
    (labels ((read-garbage-char ()
               (if (eql #\! (peek-char nil stream))
                 (progn (read-char stream)
                        (read-char stream))
                 (progn (incf garbage-total)
                        (read-char stream))))
             (read-garbage ()
               (read-char stream) ; <
               (iterate
                 (until (eql #\> (peek-char nil stream)))
                 (read-garbage-char))
               (read-char stream) ; >
               'garbage)
             (read-group ()
               (read-char stream) ; {
               (prog1 (read-group-contents)
                 (read-char stream))) ; }
             (read-group-contents (&aux result)
               (iterate
                 (case (peek-char nil stream)
                   (#\, (read-char stream))
                   (#\} (return (nreverse result)))
                   (#\{ (push (read-group) result))
                   (#\< (read-garbage))))))
      (values
        (recursively ((group (read-group))
                      (score 1))
          (+ score (loop :for g :in group :summing (recur g (1+ score)))))
        garbage-total))))

