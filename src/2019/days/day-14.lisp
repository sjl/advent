(advent:defpackage* :advent/2019/14)
(in-package :advent/2019/14)

(defun chem-symbol (name)
  (let ((*package* (find-package :advent/2019/14)))
    (symb name)))

(defun parse-chem (chemicals)
  (iterate (for ((#'parse-integer amount) (#'chem-symbol name))
                :matching "(\\d+) (\\w+)" :against chemicals)
           (collect (cons name amount))))

(defun parse-line (line)
  (destructuring-bind (inputs output) (ppcre:split "=>" line)
    (values (first (parse-chem output))
            (parse-chem inputs))))

(defun parse-data (data)
  "Parse `data` into a hash table of reactions and a digraph.

  The resulting hash table will look like:

    RESULT ::= {element: REACTION, …}
    REACTION ::= (amount-produced . INPUTS)
    INPUTS :: (INPUT*)
    INPUT ::= (element . amount-required)

  "
  (iterate (with graph = (digraph:make-digraph))
           (for line :in data)
           (for (values (out . out-amount) inputs) = (parse-line line))
           (iterate (for (in . nil) :in inputs)
                    (ensure-edge graph in out))
           (collect-hash (out (cons out-amount inputs)) :into reactions)
           (returning reactions graph)))

(defun requirements (element amount reactions)
  "Return the requirements to produce `amount` of `element` via `reactions`.

  The result will be an alist of `(input-element . input-amount)`.

  "
  (destructuring-bind (produced-amount . inputs) (gethash element reactions)
    (let ((n (ceiling amount produced-amount)))
      (iterate
        (for (el . amount) :in inputs)
        (collect (cons el (* n amount)))))))

(defun ore-required (reactions graph &optional (fuel 1))
  "Return the amount of ore required to produce `fuel` fuel."
  (iterate
    (with needed = (make-hash-table))
    (initially (setf (gethash 'fuel needed) fuel))
    (for next :in (digraph:topological-sort graph))
    (for (values el amount found) = (pophash needed next))
    (cond
      ((not found))
      ((eql el 'ore) (return amount))
      (t (iterate
           (for (input . in-amount) :in (requirements el amount reactions))
           (incf (gethash input needed 0) in-amount))))))

(define-problem (2019 14) (data read-lines) (502491 2944565)
  (multiple-value-bind (reactions graph) (parse-data data)
    (values (ore-required reactions graph)
            (bisect-integers-left
              (lambda (fuel)
                (<= (ore-required reactions graph fuel) 1000000000000))
              0 1000000000000))))



#; Scratch --------------------------------------------------------------------

(run '(
       "10 ORE => 10 A"
       "1 ORE => 1 B"
       "7 A, 1 B => 1 C"
       "7 A, 1 C => 1 D"
       "7 A, 1 D => 1 E"
       "7 A, 1 E => 1 FUEL"
       ))

(run '(
       "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
       "17 NVRVD, 3 JNWZP => 8 VPVL"
       "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
       "22 VJHF, 37 MNCFX => 5 FWMGM"
       "139 ORE => 4 NVRVD"
       "144 ORE => 7 JNWZP"
       "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
       "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
       "145 ORE => 6 MNCFX"
       "1 NVRVD => 8 CXFTF"
       "1 VJHF, 6 MNCFX => 4 RFSQX"
       "176 ORE => 6 VJHF"
       ))


