(defpackage :advent
  (:use :cl :losh :iterate :advent.quickutils)
  (:export
    :define-problem

    :read-all
    :read-lines
    :read-lines-of-numbers-and-garbage

    :ensure-string
    :ensure-stream

    :char-invertcase
    :emptyp
    :extremum+
    :extremums
    :hamming-distance
    :hash-table=
    :integral-range
    :manhattan-distance
    :manhattan-neighbors
    :nth-digit
    :unique
    :positions-if
    :digits

    :ring
    :ring-prev
    :ring-next
    :ring-data
    :map-ring
    :do-ring
    :ring-list
    :ring-length
    :ring-move
    :ring-insert-after
    :ring-insert-before
    :ring-cutf
    :ring-prevf
    :ring-nextf
    :ring-cutf
    :ring-movef
    :ring-insertf-after
    :ring-insertf-before

    :astar

    ))

(defparameter *advent-use* '(:use :cl :losh :iterate :advent :advent.quickutils))
