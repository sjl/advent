(defpackage :advent/2019/06 #.cl-user::*advent-use*)
(in-package :advent/2019/06)

(define-problem (2019 6) (data read-lines) (301100 547)
  (let ((graph (digraph:make-digraph :test #'equal)))
    (iterate
      (for line :in data)
      (for (mass orbiter) = (str:split ")" line))
      (digraph:insert-vertex graph mass)
      (digraph:insert-vertex graph orbiter)
      (digraph:insert-edge graph mass orbiter))
    (values
      (recursively ((n 0)
                    (node "COM"))
        (summation (digraph:successors graph node)
                   :initial-value n
                   :key (curry #'recur (1+ n))))
      (- (length
           (astar :start "YOU"
                  :neighbors (curry #'digraph:neighbors graph)
                  :goalp (curry #'equal "SAN")
                  :cost (constantly 1)
                  :heuristic (constantly 0)
                  :test #'equal))
         3))))

