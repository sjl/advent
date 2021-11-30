(advent:defpackage* :advent/2017/12)
(in-package :advent/2017/12)

(defun parse-line (line)
  (destructuring-bind (id others) (str:split "<->" line)
    (list (parse-integer id)
          (mapcar #'parse-integer (str:split #\, others)))))

(defun build-graph (records)
  (iterate
    (with graph = (digraph:make-digraph :initial-vertices (mapcar #'car records)))
    (for (id others) :in records)
    (dolist (id2 others)
      (digraph:insert-edge graph id id2))
    (finally (return graph))))

(defun connected-to (graph start)
  (gathering
    (digraph:mapc-depth-first #'gather graph start)))

(defun count-subgraph (graph start)
  (length (connected-to graph start)))

(defun remove-subgraph (graph start)
  (map nil (alexandria:curry #'digraph:remove-vertex graph)
       (connected-to graph start)))

(define-problem (2017 12) (data read-lines) (141 171)
  (let ((graph (build-graph (mapcar #'parse-line data))))
    (values
      (count-subgraph graph 0)
      (iterate
        (for vertex = (digraph:arbitrary-vertex graph))
        (while vertex)
        (remove-subgraph graph vertex)
        (counting t)))))
