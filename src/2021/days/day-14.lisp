(advent:defpackage* :advent/2021/14)
(in-package :advent/2021/14)

(defun parse (stream &aux (template (read-line stream)))
  (values
    ;; Initial state { (a . b): 1, …}
    (frequencies (map 'list #'cons template (subseq template 1)) :test #'equal)
    ;; Last character
    (char template (1- (length template))) ; last char
    ;; Rules { pair: (new-left-pair . new-right-pair), …}
    (iterate
      (for line :in-stream stream :using #'read-line)
      (ppcre:register-groups-bind (((rcurry #'char 0) l r m))
          ("(.)(.) -> (.)" line)
        (collect-hash ((cons l r) (cons (cons l m) (cons m r))) :test #'equal)))))

(defun polymerize (polymer rules steps)
  (do-repeat steps
    (iterate
      ;; Unfortunately we can't walk the hash table because we're modifying it.
      (for (k . v) :in (alexandria:hash-table-alist polymer))
      (unless (zerop v)
        (for (l . r) = (gethash k rules))
        (incf (gethash l polymer 0) v)
        (incf (gethash r polymer 0) v)
        (decf (gethash k polymer 0) v))))
  polymer)

(defun single-counts (polymer last)
  (let ((counts (make-hash-table)))
    (maphash (lambda (k v) (incf (gethash (car k) counts 0) v)) polymer)
    (incf (gethash last counts 0))
    (alexandria:hash-table-values counts)))

(defun polydiff (polymer last)
  (multiple-value-call #'- (extrema #'> (single-counts polymer last))))

(define-problem (2021 14) (stream) (2584 3816397135460)
  (multiple-value-bind (template last rules) (parse stream)
    (values
      (polydiff (polymerize (alexandria:copy-hash-table template) rules 10) last)
      (polydiff (polymerize (alexandria:copy-hash-table template) rules 40) last))))

#; Scratch --------------------------------------------------------------------

;; old implementation with HAMTs
(defun parse (stream)
  (values
    (read-line stream)
    (iterate
      (with-result result = (make-hash-table))
      (for line :in-stream stream :using #'read-line)
      (ppcre:register-groups-bind (((rcurry #'char 0) l r m))
          ("(.)(.) -> (.)" line)
        (setf (gethash r (alexandria:ensure-gethash l result (make-hash-table))) m)))))

(defun dict-inc (dict key &optional (n 1))
  (hamt:dict-insert dict key (+ n (or (hamt:dict-lookup dict key) 0))))

(defun dict+ (dict1 dict2)
  (hamt:dict-reduce #'dict-inc dict1 dict2))

(defun polymerize (template rules steps)
  (iterate
    (with cache = (make-hash-table :test #'equal))
    (with counts = (hamt:empty-dict))
    (for l :in-string template)
    (for r :in-string template :from 1)
    (for chunk = (recursively ((l l) (r r) (steps steps))
                   (alexandria:ensure-gethash (list l r steps) cache
                     (if (zerop steps)
                       (hamt:empty-dict)
                       (let ((m (gethash r (gethash l rules))))
                         (dict-inc (dict+ (recur l m (1- steps))
                                          (recur m r (1- steps)))
                                   m))))))
    (setf counts (dict+ counts chunk))
    (returning (reduce #'dict-inc template :initial-value counts))))

(defun polydiff (polymer)
  (multiple-value-call #'- (extrema #'> (mapcar #'cdr (hamt:dict->alist polymer)))))

