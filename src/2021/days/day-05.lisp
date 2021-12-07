(advent:defpackage* :advent/2021/05)
(in-package :advent/2021/05)

(defun parse (stream)
  (iterate (for line :in-stream stream :using #'read-line)
           (collect (ppcre:register-groups-bind ((#'parse-integer x1 y1 x2 y2))
                        ("(\\d+),(\\d+) -> (\\d+),(\\d+)" line)
                      (cons (complex x1 y1) (complex x2 y2))))))

(defun delta (line)
  (let-complex ((x1 y1 (car line))
                (x2 y2 (cdr line)))
    (complex (signum (- x2 x1)) (signum (- y2 y1)))))

(defun orthogonalp (line)
  (= (abs (delta line)) 1.0))

(defun add-line (board line)
  (iterate (with (a . b) = line)
           (for i :from a :by (delta line))
           (incf (gethash i board 0))
           (until (= i b))))

(defun count-overlaps (data &aux (board (make-hash-table)))
  (map nil (curry #'add-line board) data)
  ;; (print-hash-table-map board :flip-y t :default #\.)
  (count-if (curry #'<= 2) (alexandria:hash-table-values board)))

(define-problem (2021 5) (data parse) (5585 17193)
  (values (count-overlaps (remove-if-not #'orthogonalp data))
          (count-overlaps data)))


#; Scratch --------------------------------------------------------------------
