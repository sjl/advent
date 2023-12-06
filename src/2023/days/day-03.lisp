(advent:defpackage* :advent/2022/03)
(in-package :advent/2022/03)


(defclass* part ()
  (part-number adjacent-symbols))

(defmethod print-object ((o part) s)
  (print-unreadable-object (o s :type t)
    (format s "#~D" (part-number o))))

(defun get-char (lines r c)
  (when (array-in-bounds-p lines r)
    (let ((line (aref lines r)))
      (when (array-in-bounds-p line c)
        (aref line c)))))

(defun adjacent-chars (lines row col-start col-end)
  ;; col-end is inclusive here for symmetry
  (gathering
    (do-irange ((r (1- row) (1+ row))
                (c (1- col-start) (1+ col-end)))
      ;; technically we don't need this because we filter digits below…
      (unless (and (= r row) (<= col-start c col-end))
        (let ((ch (get-char lines r c)))
          (when (and (not (null ch))
                     (not (char= #\. ch))
                     (not (digit-char-p ch)))
            (gather (list ch r c))))))))

(defun find-parts (lines)
  (iterate
    (for line :in-vector lines :with-index r)
    (ppcre:do-matches
        (cs ce "\\d+" line)
      (collect (make-instance 'part
                 :part-number (parse-integer line :start cs :end ce)
                 :adjacent-symbols (adjacent-chars lines r cs (1- ce)))))))

(defun gears (parts)
  (iterate
    (with-result result = (make-hash-table :test 'equal))
    (for part :in parts)
    (iterate (for symb :in (adjacent-symbols part))
      (when (char= (first symb) #\*)
        (push part (gethash symb result))))))

(defun part-1 (parts)
  (summation (remove-if-not #'adjacent-symbols parts) :key #'part-number))

(defun part-2 (parts)
  (iterate
    (for (nil parts) :in-hashtable (gears parts))
    (when (= 2 (length parts))
      (summing (product parts :key #'part-number)))))


(define-problem (2023 3) (data read-lines) (557705 84266818)
  (let ((data (find-parts (coerce data 'vector))))
    (values (part-1 data)
            (part-2 data))))

#; Scratch --------------------------------------------------------------------

(result (mapcar #'part-2
        '("two1nineight"
          "eightwothree"
          "abcone2threexyz"
          "xtwone3four"
          "4nineeightseven2"
          "zoneight234"
          "7pqrstsixteen")))



