(advent:defpackage* :advent/2021/12)
(in-package :advent/2021/12)

(defun edge (graph from to)
  (push to (gethash from graph))
  (push from (gethash to graph)))

(defun parse (stream)
  (iterate
    (with-result graph = (make-hash-table))
    (for line :in-stream stream :using #'read-line)
    (for (from to) = (str:split #\- line))
    (edge graph (intern from :advent/2021/12) (intern to :advent/2021/12))))

(defun bigp (cave)
  (upper-case-p (char (symbol-name cave) 0)))

(defun restrict-small-1 (node path)
  (member node path))

(defun used-small-twice (path)
  ;; todo do something faster than this
  (iterate (for (a . rest) :on path)
           (unless (bigp a)
             (dolist (b rest)
               (thereis (eql a b))))))

(defun restrict-small-2 (node path)
  (cond
    ((member node '(|start| |end|)) (member node path))
    ((used-small-twice path) (member node path))
    (t (member node (rest (member node path))))))

(defun neighbors (graph path &key restriction)
  (iterate (for node :in (gethash (first path) graph))
           (when (or (bigp node) (not (funcall restriction node path)))
             (collect node))))

(defun paths (graph &key restriction)
  (recursively ((path '(|start|)))
    (if (eql '|end| (first path))
      1
      (iterate (for next :in (neighbors graph path :restriction restriction))
               (summing (recur (cons next path)))))))

(define-problem (2021 12) (graph parse) (5254 149385)
  (values (paths graph :restriction #'restrict-small-1)
          (paths graph :restriction #'restrict-small-2)))




#; Scratch --------------------------------------------------------------------

