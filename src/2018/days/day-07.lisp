(advent:defpackage* :advent/2018/07)
(in-package :advent/2018/07)
(named-readtables:in-readtable :interpol-syntax)


(defun parse-line (line)
  (ppcre:register-groups-bind
      (((rcurry #'aref 0) requirement target))
      (#?r"Step (\w) must be finished before step (\w) can begin." line)
    (list target requirement)))

(defun make-graph (edges)
  (let* ((vertices (remove-duplicates (loop :for edge :in edges
                                            :if (listp edge) :append edge
                                            :else :collect edge)))
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


(define-problem (2018 7) (data read-lines) ("BFGKNRTWXIHPUMLQVZOYJACDSE" 1163)
  (values
    (let ((graph (make-graph (mapcar #'parse-line data))))
      ;; (digraph.dot:draw graph)
      (recursively ((result nil))
        (if (emptyp graph)
          (coerce (nreverse result) 'string)
          (let ((next (alexandria:extremum (digraph:leafs graph) 'char<)))
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
      (for available-tasks = (_ graph
                               digraph:leafs
                               (set-difference _ current-tasks)
                               (sort _ 'char<)))
      (do-array (worker workers)
        (when (null worker)
          (when-let ((task (pop available-tasks)))
            (setf worker (cons task (task-length task))))))
      (when (and (emptyp graph) (every #'null workers))
        (return elapsed)))))
