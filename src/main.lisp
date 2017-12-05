(in-package :advent)

;;;; Utils --------------------------------------------------------------------
(defun read-file-of-digits (path)
  "Read all the ASCII digits in `path` into a list of integers.

  Any character in the file that's not an ASCII digit will be silently ignored.

  "
  (-<> path
    read-file-into-string
    (map 'list #'digit-char-p <>)
    (remove nil <>)))

(defun read-file-of-numbers (path)
  (iterate (for line :in-file path :using #'read-line)
           (appending (mapcar #'parse-integer (str:words line)))))

(defun read-file-of-lines-of-numbers (path)
  (iterate (for line :in-file path :using #'read-line)
           (collect (mapcar #'parse-integer (str:words line)))))

(defun read-file-of-lines-of-words (path)
  (iterate (for line :in-file path :using #'read-line)
           (collect (str:words line))))


(defun contains-duplicates-p (list &key (test #'eql))
  (iterate (for (head . tail) :on list)
           (thereis (member head tail :test test))))

(defun anagramp (string1 string2)
  (string= (sort (copy-seq string1) #'char<)
           (sort (copy-seq string2) #'char<)))


;;;; Problems -----------------------------------------------------------------
(defun day-1/1 ()
  (iterate (for (x . y) :pairs-of-list (read-file-of-digits "data/2017/01"))
           (when (= x y)
             (sum x))))

(defun day-1/2 ()
  (iterate
    (with data = (coerce (read-file-of-digits "data/2017/01") 'vector))
    (with length = (length data))
    (for x :in-vector data)
    (for iy :modulo length :from (truncate length 2))
    (for y = (aref data iy))
    (when (= x y)
      (sum x))))


(defun day-2/1 ()
  (flet ((checksum (line)
           (- (apply #'max line)
              (apply #'min line))))
    (summation (remove nil (read-file-of-lines-of-numbers "data/2017/02"))
               :key #'checksum)))

(defun day-2/2 ()
  (labels ((validp (a b)
             (dividesp (max a b) (min a b)))
           (head-valid-p (list)
             (some (curry #'validp (car list))
                   (cdr list)))
           (checksum (line)
             (somelist #'head-valid-p line)))
    (summation (remove nil (read-file-of-lines-of-numbers "data/2017/02"))
               :key #'checksum)))


(defun day-3/1 ()
  (labels ((manhattan-distance (a b)
             (+ (abs (- (realpart a)
                        (realpart b)))
                (abs (- (imagpart a)
                        (imagpart b)))))
           (distance-to-origin (p)
             (manhattan-distance #c(0 0) p)))
    (distance-to-origin (advent.spiral:number-coordinates 325489))))

(defun day-3/2 ()
  (flet ((neighbors (coord)
           (iterate (for-nested ((dx :from -1 :to 1)
                                 (dy :from -1 :to 1)))
                    (unless (= 0 dx dy)
                      (collect (+ coord (complex dx dy)))))))
    (iterate
      (with memory = (make-hash-table))
      (initially (setf (gethash #c(0 0) memory) 1))
      (for n :from 2)
      (for coord = (advent.spiral:number-coordinates n))
      (for value = (summation (neighbors coord) :key (rcurry #'gethash memory 0)))
      (finding value :such-that (> value 325489))
      (setf (gethash coord memory) value))))


(defun d4-valid-passphrase-p (phrase)
  (not (contains-duplicates-p phrase :test #'string=)))

(defun day-4/1 ()
  (count-if #'d4-valid-passphrase-p
            (read-file-of-lines-of-words "data/2017/04")))

(defun day-4/2 ()
  (flet ((contains-anagram-p (phrase)
           (iterate (for (word . tail) :on phrase)
                    (thereis (find-if (curry #'anagramp word) tail)))))
    (-<> (read-file-of-lines-of-words "data/2017/04")
      (remove-if-not #'d4-valid-passphrase-p <>)
      (count-if-not #'contains-anagram-p <>))))


(defun day-5/1 ()
  (iterate
    (with maze = (coerce (read-file-of-numbers "data/2017/05") 'vector))
    (with bound = (1- (length maze)))
    (with address = 0)
    (for steps :from 0)
    (finding steps :such-that (not (<= 0 address bound)))
    (for offset = (aref maze address))
    (incf (aref maze address))
    (incf address offset)))

(defun day-5/2 ()
  (iterate
    (declare (optimize speed)
             (type fixnum bound address steps offset))
    (with maze = (coerce (read-file-of-numbers "data/2017/05") 'simple-vector))
    (with bound = (1- (length maze)))
    (with address = 0)
    (for steps :from 0)
    (finding steps :such-that (not (<= 0 address bound)))
    (for offset = (aref maze address))
    (incf (aref maze address)
          (if (>= offset 3) -1 1))
    (incf address offset)))
