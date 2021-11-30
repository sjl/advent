(advent:defpackage* :advent/2016/10)
(in-package :advent/2016/10)

(defun tree-collect (predicate tree) ; todo from quickutil, remove this
  "Returns a list of every node in the `tree` that satisfies the `predicate`. If there are any improper lists in the tree, the `predicate` is also applied to their dotted elements."
  (let ((sentinel (gensym)))
    (flet ((my-cdr (obj)
             (cond ((consp obj)
                    (let ((result (cdr obj)))
                      (if (listp result)
                        result
                        (list result sentinel))))
                   (t
                    (list sentinel)))))
      (loop :for (item . rest) :on tree :by #'my-cdr
            :until (eq item sentinel)
            :if (funcall predicate item) collect item
            :else
            :if (listp item)
            :append (tree-collect predicate item)))))

(defun parse-line (line)
  (or (ppcre:register-groups-bind
          ((#'parse-integer value bot))
          ("value (\\d+) goes to bot (\\d+)" line)
        `(input ,value (:bot . ,bot)))
      (ppcre:register-groups-bind
          ((#'parse-integer sender)
           (#'ensure-keyword lo-type) (#'parse-integer lo)
           (#'ensure-keyword hi-type) (#'parse-integer hi))
          ("bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)" line)
        `(send ,sender (,lo-type . ,lo) (,hi-type . ,hi)))
      (error "Bad line: ~S" line)))

(defun make-buckets (instructions type)
  (make-array (_ instructions
                (tree-collect (lambda (node)
                                (and (consp node) (eql (car node) type)))
                              _)
                (alexandria:extremum _ #'> :key #'cdr)
                (1+ (cdr _)))
    :initial-element nil))

(defun filter-instructions (instructions type)
  (_ instructions
    (remove type _ :test-not #'eql :key #'car)
    (mapcar #'rest _)))

(define-condition* bot-comparison () (actor lo hi))

(defun run-bots (instructions)
  (let ((bots (make-buckets instructions :bot))
        (outputs (make-buckets instructions :output))
        (input-instructions (filter-instructions instructions 'input))
        (send-instructions (alexandria:alist-hash-table
                             (filter-instructions instructions 'send))))
    (flet ((dest (type)
             (ecase type
               (:bot bots)
               (:output outputs))))
      (iterate
        (for (value (nil . id)) :in input-instructions)
        (push value (aref bots id)))
      (iterate
        (for ready = (positions 2 bots :key #'length))
        (while ready)
        (iterate
          (for id :in ready)
          (for ((lo-type . lo-id) (hi-type . hi-id)) = (gethash id send-instructions))
          (for (a b) = (aref bots id))
          (for lo = (min a b))
          (for hi = (max a b))
          (signal 'bot-comparison :actor id :lo lo :hi hi)
          (push lo (aref (dest lo-type) lo-id))
          (push hi (aref (dest hi-type) hi-id))
          (setf (aref bots id) nil))))
    outputs))

(define-problem (2016 10) (data read-lines) (157 1085)
  (let ((part1 nil))
    (handler-bind ((bot-comparison
                     (lambda (c)
                       (when (and (= (lo c) 17)
                                  (= (hi c) 61))
                         (setf part1 (actor c))))))
      (let ((outputs (run-bots (mapcar #'parse-line data))))
        (values part1
                (* (first (aref outputs 0))
                   (first (aref outputs 1))
                   (first (aref outputs 2))))))))


#; Scratch --------------------------------------------------------------------

(run '(
       "value 5 goes to bot 2"
       "bot 2 gives low to bot 1 and high to bot 0"
       "value 3 goes to bot 1"
       "bot 1 gives low to output 1 and high to bot 0"
       "bot 0 gives low to output 2 and high to output 0"
       "value 2 goes to bot 2"
       ))
