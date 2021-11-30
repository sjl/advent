(advent:defpackage* :advent/2017/06)
(in-package :advent/2017/06)


(define-problem (2017 6) (data read-all) (6681 2392)
  (let ((banks (coerce data 'vector))
        (seen (make-hash-table :test 'equalp)))
    (labels ((bank-to-redistribute ()
               (iterate (for blocks :in-vector banks :with-index bank)
                        (finding bank :maximizing blocks)))
             (redistribute ()
               (iterate
                 (with bank = (bank-to-redistribute))
                 (with blocks-to-redistribute = (aref banks bank))
                 (initially (setf (aref banks bank) 0))
                 (repeat blocks-to-redistribute)
                 (for b :modulo (length banks) :from (1+ bank))
                 (incf (aref banks b))))
             (mark-seen (banks cycles)
               (setf (gethash (copy-seq banks) seen) cycles)))
      (iterate
        (mark-seen banks cycle)
        (counting t :into cycle)
        (redistribute)
        (for last-seen = (gethash banks seen))
        (until last-seen)
        (finally (return (values cycle (- cycle last-seen))))))))


