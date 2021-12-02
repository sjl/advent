(advent:defpackage* :advent/2021/01)
(in-package :advent/2021/01)


(define-problem (2021 1) (data read-numbers) (1754 1789)
  (values
    (iterate (for d :in (nthcdr 1 data))
             (for p :in data)
             (counting (> d p)))
    ;; The window total is a red herring.  We only need to count when it
    ;; increases, and only increases when the number you're adding is larger
    ;; than the number you're subtracting.
    (iterate (for d :in (nthcdr 3 data))
             (for p :in data)
             (counting (> d p)))))

#; Scratch --------------------------------------------------------------------
