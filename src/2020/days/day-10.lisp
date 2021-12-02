(advent:defpackage* :advent/2020/10)
(in-package :advent/2020/10)

(defun part1 (data)
  (iterate
    (for a :in-vector data)
    (for b :in-vector data :from 1)
    (for δ = (abs (- a b)))
    (counting (= δ 1) :into δ₁)
    (counting (= δ 3) :into δ₃)
    (returning (* δ₁ δ₃))))

(defun part2 (data)
  (iterate
    (with counts = (make-array (length data) :initial-element 1)) ; obvious dp
    (for lo :in-vector data :from (- (length data) 2) :downto 0 :with-index i)
    (setf (aref counts i)
          (iterate (for hi :in-vector data :from (1+ i) :with-index j)
                   (while (<= (- hi lo) 3))
                   (summing (aref counts j))))
    (returning (aref counts 0))))

(define-problem (2020 10) (data read-numbers) ()
  (setf data (_ data
               (cons 0 _)
               (cons (+ 3 (alexandria:extremum _ #'>)) _)
               (coerce _ 'vector)
               (sort _ #'<)))
  (values (part1 data) (part2 data)))


#; Scratch --------------------------------------------------------------------

(run (list 16 10 15 5 1 11 7 19 6 12 4))
