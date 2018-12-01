(in-package :advent)


(define-problem (2017 1 1) (data read-file-of-digits)
  (iterate
    (for (x . y) :pairs-of-list data)
    (when (= x y)
      (sum x))))

(define-problem (2017 1 2) (data read-file-of-digits)
  (iterate
    (with data = (coerce data 'vector))
    (with length = (length data))
    (for x :in-vector data)
    (for iy :modulo length :from (truncate length 2))
    (for y = (aref data iy))
    (when (= x y)
      (sum x))))


(define-problem (2017 2 1) (data read-file-of-lines-of-numbers)
  (flet ((checksum (line)
           (- (extremum line #'>)
              (extremum line #'<))))
    (summation data :key #'checksum)))

(define-problem (2017 2 2) (data read-file-of-lines-of-numbers)
  (labels ((validp (a b)
             (dividesp (max a b) (min a b)))
           (head-valid-p (list)
             (destructuring-bind (n . remaining) list
               (some (curry #'validp n) remaining)))
           (checksum (line)
             (somelist #'head-valid-p line)))
    (summation data :key #'checksum)))


(define-problem (2017 3 1) (data read-form-from-file)
  (labels ((manhattan-distance (a b)
             (+ (abs (- (realpart a)
                        (realpart b)))
                (abs (- (imagpart a)
                        (imagpart b)))))
           (distance-to-origin (p)
             (manhattan-distance #c(0 0) p)))
    (distance-to-origin (advent.spiral:number-coordinates data))))

(define-problem (2017 3 2) (data read-form-from-file)
  (flet ((neighbors (coord)
           (gathering
             (do-irange ((dx -1 1)
                         (dy -1 1))
               (unless (= 0 dx dy)
                 (gather (+ coord (complex dx dy))))))))
    (iterate
      (with memory = (make-hash-table))
      (initially (setf (gethash #c(0 0) memory) 1))
      (for n :from 2)
      (for coord = (advent.spiral:number-coordinates n))
      (for value = (summation (neighbors coord) :key (rcurry #'gethash memory 0)))
      (finding value :such-that (> value data))
      (setf (gethash coord memory) value))))


(define-problem (2017 4 1) (data read-file-of-lines-of-words)
  (labels ((contains-duplicates-p (list &key (test #'eql))
             (iterate (for (head . tail) :on list)
                      (thereis (member head tail :test test))))
           (validp (phrase)
             (not (contains-duplicates-p phrase :test #'string=))))
    (count-if #'validp data)))

(define-problem (2017 4 2) (data read-file-of-lines-of-words)
  (labels ((anagramp (string1 string2)
             (hash-table= (frequencies string1) (frequencies string2)))
           (contains-anagram-p (phrase)
             (iterate (for (word . tail) :on phrase)
                      (thereis (member-if (curry #'anagramp word) tail)))))
    (count-if-not #'contains-anagram-p data)))


(define-problem (2017 5 1) (data read-all-from-file)
  (iterate
    (with maze = (coerce data 'simple-vector))
    (with bound = (1- (length maze)))
    (with address = 0)
    (for steps :from 0)
    (finding steps :such-that (not (<= 0 address bound)))
    (for offset = (aref maze address))
    (incf (aref maze address))
    (incf address offset)))

(define-problem (2017 5 2) (data read-all-from-file)
  (iterate
    (with maze = (coerce data 'simple-vector))
    (with bound = (1- (length maze)))
    (with address = 0)
    (for steps :from 0)
    (finding steps :such-that (not (<= 0 address bound)))
    (for offset = (aref maze address))
    (incf (aref maze address)
          (if (>= offset 3) -1 1))
    (incf address offset)))


(define-problem (2017 6) (data read-all-from-file)
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
        (summing 1 :into cycle)
        (redistribute)
        (for last-seen = (gethash banks seen))
        (until last-seen)
        (finally (return (values cycle (- cycle last-seen))))))))
