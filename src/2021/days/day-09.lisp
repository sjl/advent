(advent:defpackage* :advent/2021/09)
(in-package :advent/2021/09)

(alexandria:define-constant +adjacent-deltas+
  '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
  :test #'equal)

(defun low-point-p (data row col)
  (iterate (with n = (aref data row col))
           (for (dr . dc) :in +adjacent-deltas+)
           (for r = (+ row dr))
           (for c = (+ col dc))
           (when (array-in-bounds-p data r c)
             (always (< n (aref data r c))))))

(defun part1 (data)
  (iterate (for (n r c) :in-array data)
           (when (low-point-p data r c)
             (summing (1+ n)))))

(defun flood-fill (data row col)
  "Fill data starting from the given row and column and return the size filled."
  (iterate (with frontier = (list (cons row col)))
           (while frontier)
           (for (r . c) = (pop frontier))
           (when (array-in-bounds-p data r c)
             (for n = (aref data r c))
             (when (<= 0 n 8)
               (counting t)
               (setf (aref data r c) -1)
               (iterate (for (dr . dc) :in +adjacent-deltas+)
                        (push (cons (+ r dr) (+ c dc)) frontier))))))

(defun part2 (data &aux (data (alexandria:copy-array data)))
  (_ (iterate (for (n r c) :in-array data)
              (when (<= 0 n 8)
                (collect (flood-fill data r c))))
    (sort _ #'>)
    (subseq _ 0 3)
    product))

(define-problem (2021 9) (stream) (566 891684)
  (let ((data (read-2d-array stream :key #'digit-char-p)))
    (values (part1 data)
            (part2 data))))


#; Scratch --------------------------------------------------------------------
