(advent:defpackage* :advent/2021/11)
(in-package :advent/2021/11)

(defun simulate (data steps &aux (result 0))
  (destructuring-bind (rows cols) (array-dimensions data)
    (iterate
      (for step :from 0)
      (for flashed-this-round = 0)
      (for to-flash = '())
      (when steps
        (until (= step steps)))
      (labels ((inc (r c)
                 (when (array-in-bounds-p data r c)
                   (when (= 10 (incf (aref data r c)))
                     (push (cons r c) to-flash))))
               (initial-phase ()
                 (do-range ((r 0 rows)
                            (c 0 cols))
                   (inc r c)))
               (flash (row col)
                 (incf flashed-this-round)
                 (iterate
                   (for (r c) :within-radius 1 :origin (row col) :skip-origin t)
                   (inc r c)))
               (flashing-phase ()
                 (iterate (while to-flash)
                          (for (r . c) = (pop to-flash))
                          (flash r c)))
               (reset-phase ()
                 (do-array (n data)
                   (when (>= n 10)
                     (setf n 0)))))
        (initial-phase)
        (flashing-phase)
        (reset-phase))
      (if steps
        (incf result flashed-this-round) ; part 1
        (when (= flashed-this-round (* rows cols)) ; part 2
          (return-from simulate (1+ step))))))
  result)

(define-problem (2021 11) (stream) (1594 437)
  (let ((data (read-2d-array stream :key #'digit-char-p)))
    (values (simulate (alexandria:copy-array data) 100)
            (simulate (alexandria:copy-array data) nil))))


#; Scratch --------------------------------------------------------------------
