(advent:defpackage* :advent/2019/04)
(in-package :advent/2019/04)

(defun nondecreasing-digits-p (n)
  (iterate
    (for (a b) :on (digits n))
    (while b)
    (always (<= a b))))

(defun contains-duplicate-digit-p (n)
  (iterate
    (for (a b) :on (digits n))
    (thereis (eql a b))))

(defun contains-offset-duplicate-digit-p (n)
  (iterate
    (for (a b c d) :on (cons nil (digits n)))
    (while c)
    (thereis (and (eql b c)
                  (not (eql a b))
                  (not (eql c d))))))

(define-problem (2019 4) (data read-line) ()
  (destructuring-bind (lo hi) (mapcar #'parse-integer (str:split #\- data))
    (iterate
      (for i :from lo :to hi)
      (for nondecreasing = (nondecreasing-digits-p i))
      (for duplicate = (contains-duplicate-digit-p i))
      (for offset-duplicate = (contains-offset-duplicate-digit-p i))
      (counting (and nondecreasing duplicate) :into part1)
      (counting (and nondecreasing offset-duplicate) :into part2)
      (returning part1 part2))))
