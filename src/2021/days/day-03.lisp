(advent:defpackage* :advent/2021/03)
(in-package :advent/2021/03)

(defun-inline bool->bit (b) (if b 1 0))
(defun-inline char->δ (ch) (ecase ch (#\0 -1) (#\1 1)))

(defun count-bits (data)
  (iterate
    (with counts = (make-array (length (first data)) :initial-element 0))
    (for line :in data)
    (iterate (for ch :in-string line :with-index i)
             (incf (aref counts i) (char->δ ch)))
    (returning counts)))

(defun rates (data)
  (let ((counts (count-bits data)))
    (values
      (digits->number counts :radix 2 :key (compose #'bool->bit #'plusp)) ; γ
      (digits->number counts :radix 2 :key (compose #'bool->bit #'minusp))))) ; ε

(defun find-rating (sorted-data count->target)
  (iterate
    (with lo = 0)
    (with hi = (1- (length sorted-data)))
    (when (= lo hi)
      (return (parse-integer (aref sorted-data lo) :radix 2)))
    (for i :from 0)
    (for count = (iterate (for candidate :in-vector sorted-data :from lo :to hi)
                          (summing (char->δ (aref candidate i)))))
    (for target = (funcall count->target count))
    ;; Could potentially bisect these instead of linearly scanning, but it's fine.
    (loop :while (char/= target (char (aref sorted-data lo) i)) :do (incf lo))
    (loop :while (char/= target (char (aref sorted-data hi) i)) :do (decf hi))))

(defun ratings (data)
  (let ((data (sort (fresh-vector data) #'string<)))
    (values
      (find-rating data (lambda (c) (if (minusp c) #\0 #\1))) ; O₂
      (find-rating data (lambda (c) (if (not (minusp c)) #\0 #\1)))))) ; CO₂

(define-problem (2021 3) (data read-lines) (3847100 4105235)
  (values (multiple-value-call #'* (rates data))
          (multiple-value-call #'* (ratings data))))

#; Scratch --------------------------------------------------------------------
