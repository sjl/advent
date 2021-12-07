(defpackage :advent
  (:use :cl :losh :iterate)
  (:export
    :define-problem

    :read-2d-array
    :read-all
    :read-and-collect
    :read-before
    :read-chunks
    :read-comma-separated-values
    :read-comma-separated-integers
    :read-digits
    :read-lines
    :read-lines-of-numbers-and-garbage
    :read-lines-of-words
    :read-numbers
    :read-numbers-from-string
    :read-to
    :read-word
    :with-eof-handled

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
    :manhattan-neighborhood
    :x
    :y
    :nth-digit
    :unique
    :positions
    :positions-if
    :digits
    :digits->number
    :fresh-vector
    :first-character
    :let-result
    :let-complex
    :queue-thunk
    :bounds
    :draw-bitmap

    :bytes->hex
    :bytes->integer

    :ring
    :ring-prev
    :ring-next
    :ring-data
    :map-ring
    :do-ring
    :ring-find
    :ring-list
    :ring-length
    :ring-move
    :ring-insert-after
    :ring-insert-before
    :ring-findf
    :ring-cutf
    :ring-prevf
    :ring-nextf
    :ring-cutf
    :ring-movef
    :ring-insertf-after
    :ring-insertf-before

    :astar

    :defpackage*

    :gethash-arbitrary
    :pophash

    :ensure-edge

    :bisect-integers-left
    :bisect-integers-right

    :print-hash-table-map
    :print-2d-array

    :bold
    :clear
    :green
    :reset
    :underline

    :mkinput

    ))

