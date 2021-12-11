(advent:defpackage* :advent/2021/10)
(in-package :advent/2021/10)

(defun closing-char (opening-char)
  (case opening-char (#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>)))

(defun parse-line (line)
  (iterate
    (with stack = '())
    (with s = (make-string-input-stream line))
    (for next = (read-char s nil :eof))
    (ecase next
      (:eof (return (if (null stack) :ok (values :incomplete stack))))
      ((#\( #\[ #\{ #\<) (push (closing-char next) stack))
      ((#\) #\] #\} #\>) (unless (eql next (pop stack))
                           (return (values :corrupt next)))))))

(defun score1 (char)
  (ecase char (#\) 3) (#\] 57) (#\} 1197) (#\> 25137)))

(defun score2 (chars)
  (reduce (lambda (score char)
            (+ (* score 5) (ecase char (#\) 1) (#\] 2) (#\} 3) (#\> 4))))
          chars :initial-value 0))

(defun part1 (lines)
  (iterate (for line :in lines)
           (for (values status char) = (parse-line line))
           (when (eql status :corrupt)
             (summing (score1 char)))))

(defun part2 (lines)
  (_ (iterate (for line :in lines)
              (for (values status chars) = (parse-line line))
              (when (eql status :incomplete)
                (collect (score2 chars) :result-type 'vector)))
    (sort _ #'<)
    (aref _ (truncate (length _) 2))))

(define-problem (2021 10) (data read-lines) (323613 3103006161)
  (values (part1 data) (part2 data)))


#; Scratch --------------------------------------------------------------------
