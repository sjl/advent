(defpackage :advent/2018/07 #.cl-user::*advent-use*)
(in-package :advent/2018/07)
(named-readtables:in-readtable :interpol-syntax)


(defun parse-line (line)
  (ppcre:register-groups-bind
      (((rcurry #'aref 0) requirement target))
      (#?r"Step (\w) must be finished before step (\w) can begin." line)
    (list target requirement)))

(defun make-graph (edges)
  (let* ((vertices (remove-duplicates (flatten-once edges)))
         (graph (digraph:make-digraph :initial-vertices vertices)))
    (dolist (edge edges)
      (digraph:insert-edge graph (first edge) (second edge)))
    graph))

(defun char-number (char)
  (1+ (- (char-code char) (char-code #\A))))

(defun task-length (task)
  (+ 60 (char-number task)))

(defun decrement-workers (workers)
  (gathering
    (do-array (worker workers)
      (when worker
        (when (zerop (decf (cdr worker)))
          (gather (car worker))
          (setf worker nil))))))


(define-problem (2018 7) (data read-lines)
  (values
    (let ((graph (make-graph (mapcar #'parse-line data))))
      ;; (digraph.dot:draw graph)
      (recursively ((result nil))
        (if (emptyp graph)
          (coerce (nreverse result) 'string)
          (let ((next (extremum (digraph:leafs graph) 'char<)))
            (digraph:remove-vertex graph next)
            (recur (cons next result))))))
    (iterate
      (with graph = (make-graph (mapcar #'parse-line data)))
      ;; workers is a vector of (task . remaining-time), or NILs for idle workers
      (with workers = (make-array 5 :initial-element nil))
      (for elapsed :from 0)
      (for finished-tasks = (decrement-workers workers))
      (map nil (curry #'digraph:remove-vertex graph) finished-tasks)
      (for current-tasks = (remove nil (map 'list #'car workers)))
      (for available-tasks = (-<> graph
                               digraph:leafs
                               (set-difference <> current-tasks)
                               (sort <> 'char<)))
      (do-array (worker workers)
        (when (null worker)
          (when-let ((task (pop available-tasks)))
            (setf worker (cons task (task-length task))))))
      (when (and (emptyp graph) (every #'null workers))
        (return elapsed)))))


(1am:test test-2018/07
  (multiple-value-bind (part1 part2) (run)
    (1am:is (string= "BFGKNRTWXIHPUMLQVZOYJACDSE" part1))
    (1am:is (= 1163 part2))))
