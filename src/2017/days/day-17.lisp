(advent:defpackage* :advent/2017/17)
(in-package :advent/2017/17)

(defun-inline spin (ring shift n)
  (iterate (for i :from 1 :to n)
           (ring-movef ring shift)
           (ring-insertf-after ring i))
  ring)

(defun simple-part-2 (shift)
  (_ (ring 0)
    (spin _ shift 50000000)
    (ring-find _ 0)
    ring-next
    ring-data))

(defun fast-part-2 (shift)
  (declare (optimize speed (debug 1) (safety 1)))
  (check-type shift fixnum)
  ;; Hack: the only actual value we care about is the one that's after 0, so we
  ;; don't actually have to store a list of the rest.  We can just compute the
  ;; offset of where we *would* insert into the list, and keep track of the last
  ;; value we set index 1 to.
  (iterate
    (declare (iterate:declare-variables))
    (with result)
    (for (the fixnum size) :from 1)
    (for (the fixnum val) :from 1 :to 50000000)
    (for (the fixnum pos) :seed 0 :then (1+ (mod (+ pos shift) size)))
    (when (= 1 pos)
      (setf result val))
    (returning result)))

(define-problem (2017 17) (data read) (1244 11162912)
  (values
    (_ (ring 0)
      (spin _ data 2017)
      ring-next
      ring-data)
    (fast-part-2 data)))
