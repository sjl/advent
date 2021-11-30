(advent:defpackage* :advent/2017/04)
(in-package :advent/2017/04)

(defun valid-hash-table-test-p (test)
  (member test `(eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp)))

(defun ensure-boolean (value)
  (if value t nil))

(defun contains-duplicates-p (sequence &key (test #'eql))
  (ensure-boolean (if (valid-hash-table-test-p test)
        (iterate
          (with seen = (make-hash-set :test test))
          (for value :in-whatever sequence)
          (thereis (hset-contains-p seen value))
          (hset-insert! seen value))
        (etypecase sequence
          (list (iterate
                  (for (value . remaining) :on sequence)
                  (thereis (position value remaining :test test))))
          (sequence
            (iterate
              (for i :from 0 :below (length sequence))
              (for value = (elt sequence i))
              (thereis (position value sequence :start (1+ i) :test test))))))))


(defun anagramp (string1 string2)
  (string= (sort (copy-seq string1) #'char<)
           (sort (copy-seq string2) #'char<)))

(define-problem (2017 4) (data read-lines-of-words) (337 231)
  (values (count-if (lambda (phrase)
                      (not (contains-duplicates-p phrase :test #'equal)))
                    data)
          (count-if (lambda (phrase)
                      (not (contains-duplicates-p phrase :test #'anagramp)))
                    data)))


