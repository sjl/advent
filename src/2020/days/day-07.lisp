(advent:defpackage* :advent/2020/07)
(in-package :advent/2020/07)

(defun parse-contained (string)
  (ppcre:register-groups-bind (n color)
      ("^(\\d+) (.+) bags?$" string)
    (cons color (parse-integer n))))

(defun parse-contents (string)
  (if (string= "no other bags" string)
    (list)
    (mapcar #'parse-contained (str:split ", " string))))

(defun parse-line (line)
  (ppcre:register-groups-bind (container contents)
      ("^(.+) bags contain (.+)[.]$" line)
    (cons container (parse-contents contents))))

(defun parse-data (stream)
  ;; { container: { contained: n, … }, … }
  (iterate (for line :in-stream stream :using #'read-line)
           (for (container . contents) = (parse-line line))
           (collect-hash (container (alexandria:alist-hash-table contents :test #'equal))
                         :test #'equal)))

(defun reachablep (rules goal start)
  (astar :start start
         :neighbors (lambda (state) (alexandria:hash-table-keys (gethash state rules)))
         :goalp (curry #'equal goal)
         :cost (constantly 1)
         :heuristic (constantly 1)
         :test #'equal))

(defun count-required (rules start)
  (recursively ((current start))
    (1+ (iterate (for (child n) :in-hashtable (gethash current rules))
                 (summing (* n (recur child)))))))


(define-problem (2020 7) (rules parse-data) (192 12128)
  (values (count-if (curry #'reachablep rules "shiny gold")
                    (remove "shiny gold" (alexandria:hash-table-keys rules)
                            :test #'string=))
          (1- (count-required rules "shiny gold"))))

#; Scratch --------------------------------------------------------------------


