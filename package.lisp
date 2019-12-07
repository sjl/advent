(defpackage :advent
  (:use :cl :losh :iterate :advent.quickutils)
  (:export
    :define-problem

    :read-all
    :read-lines
    :read-lines-of-words
    :read-lines-of-numbers-and-garbage
    :read-numbers
    :read-numbers-from-string
    :read-comma-separated-values

    :ensure-string
    :ensure-stream
    :ensure-keyword

    :returning

    :char-invertcase
    :emptyp
    :extremum+
    :extremums
    :hamming-distance
    :hash-table=
    :integral-range
    :manhattan-distance
    :manhattan-neighbors
    :x
    :y
    :nth-digit
    :unique
    :positions-if
    :digits
    :fresh-vector
    :first-character
    :let-result
    :let-complex

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
