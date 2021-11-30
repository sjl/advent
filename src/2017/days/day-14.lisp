(advent:defpackage* :advent/2017/14)
(in-package :advent/2017/14)

(defun print-grid (grid)
  (dotimes (row 128)
    (dotimes (col 128)
      (princ (if (aref grid row col) #\# #\.)))
    (terpri)))

(defun gref (grid point)
  (let-complex ((row col point))
    (when (and (in-range-p 0 row 128)
               (in-range-p 0 col 128))
      (aref grid row col))))

(defun (setf gref) (new-value grid point)
  (setf (aref grid (realpart point) (imagpart point)) new-value))

(defun flood-region (grid row col)
  (iterate
    (with seen = (make-hash-set))
    (with frontier = (make-hash-set :initial-contents (list (complex row col))))
    (until (hset-empty-p frontier))
    (for next = (hset-pop! frontier))
    (when (not (hset-contains-p seen next))
      (hset-insert! seen next)
      (when (gref grid next)
        (counting t)
        (setf (gref grid next) nil)
        (apply #'hset-insert! frontier (manhattan-neighbors next))))))

(define-problem (2017 14) (data read-line) (8226 1128)
  (multiple-value-bind (grid total)
      (iterate
        (with grid = (make-array '(128 128)))
        (for row :from 0 :to 127)
        (for hash = (advent/knot-hash:full-knot-hash
                      (format nil "~A-~D" data row)
                      :result-type 'integer))
        (summing (logcount hash) :into total)
        (dotimes (col 128)
          (setf (aref grid row col)
                (logbitp (- 127 col) hash)))
        (returning grid total))
    (values total
            (iterate
              (for-nested ((row :from 0 :to 127)
                           (col :from 0 :to 127)))
              (counting (plusp (flood-region grid row col)))))))
