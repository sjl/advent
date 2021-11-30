(advent:defpackage* :advent/2017/07)
(in-package :advent/2017/07)
(named-readtables:in-readtable :interpol-syntax)

(defun parse-line (line)
  (ppcre:register-groups-bind
      (name (#'parse-integer weight) ((curry #'str:split ", ") holding))
      (#?/(\w+) \((\d+)\)(?: -> (.+))?/ line)
    (values name weight holding)))

(defun insert-edge (digraph pred succ)
  (digraph:insert-vertex digraph pred)
  (digraph:insert-vertex digraph succ)
  (digraph:insert-edge digraph pred succ))

(defun build-tower (lines)
  (iterate
    (with tower = (digraph:make-digraph :test #'equal))
    (for line :in lines)
    (for (values name weight holding) = (parse-line line))
    (collect-hash (name weight) :into weights :test #'equal)
    (digraph:insert-vertex tower name)
    (map nil (curry #'insert-edge tower name) holding)
    (finally (return (values tower weights)))))

(defun root (digraph)
  (first (digraph:roots digraph)))

(defun compute-total-weights (digraph individual-weights)
  (let ((result (make-hash-table :test #'equal)))
    (recursively ((node (root digraph)))
      (setf (gethash node result)
            (+ (gethash node individual-weights)
               (loop :for succ :in (digraph:successors digraph node)
                     :summing (recur succ)))))
    result))

(defun find-proper-weight (digraph total-weights)
  (labels
      ((unbalanced-child (node)
         (iterate
           (with weights = (make-hash-table))
           (for child :in (digraph:successors digraph node))
           (for weight = (gethash child total-weights))
           (push child (gethash weight weights))
           (finally
             (return
               (if (<= (hash-table-count weights) 1)
                 nil
                 (values
                   (iterate
                     (for (w children) :in-hashtable weights)
                     (finding (first children) :such-that (= 1 (length children))))
                   (iterate
                     (for (w children) :in-hashtable weights)
                     (finding w :such-that (> (length children) 1))))))))))
    (recursively ((node (root digraph))
                  (target 0))
      (multiple-value-bind (child new-target) (unbalanced-child node)
        (if (null child)
          (values node target)
          (recur child new-target))))))

(define-problem (2017 7) (data read-lines) ("bsfpjtc" 529)
  (multiple-value-bind (tower individual-weights) (build-tower data)
    ;; (digraph.dot:draw tower)
    (values
      (root tower)
      (let ((total-weights (compute-total-weights tower individual-weights)))
        (multiple-value-bind (node target-weight)
            (find-proper-weight tower total-weights)
          ;; fuck this miserable problem, I just want to be done
          (+ (gethash node individual-weights)
             (- target-weight (gethash node total-weights))))))))
