(advent:defpackage* :advent/2022/05)
(in-package :advent/2022/05)


(defstruct (mapping (:constructor make-mapping%))
  src-start src-end dst-start)

(defun make-mapping (src dst len)
  (make-mapping% :src-start src :src-end (+ src len) :dst-start dst))

(defun parse-seeds (string)
  ;; seeds: 79 14 55 13
  (mapcar #'parse-integer (rest (str:words string))))

(defun parse-body-range (string)
  ;; 50 98 2
  (destructuring-bind (dst src len) (mapcar #'parse-integer (str:words string))
    (make-mapping src dst len)))

(defun parse-body (strings)
  ;; 50 98 2
  ;; 52 50 48
  (sort (mapcar #'parse-body-range strings) #'< :key #'mapping-src-start))

(defun parse-map (string)
  ;; seed-to-soil map:
  ;; 50 98 2
  ;; 52 50 48
  (parse-body (rest (str:lines string))))

(defun parse (data)
  ;; Not really mentioned in the problem, but the maps are given in the correct
  ;; order.  So we don't need to bohter making a hash table of {from: to: [...]}
  ;; but can just reduce over the maps in the order they're given.
  (destructuring-bind (seeds . maps) (str:split (format nil "~2%") data)
    (values (parse-seeds seeds)
            (mapcar #'parse-map maps))))

(defun src->dst (mapping n)
  (+ (mapping-dst-start mapping)
     (- n (mapping-src-start mapping))))

(defun map-single-range (input ranges)
  ;; Take a single input range and a list of mapping ranges and produce a new
  ;; list of ranges:
  ;;
  ;;     Input:  [10-20]
  ;;     Ranges: [12-14 -> 100-102]
  ;;             [18-30 -> 200-212]
  ;;     Result: [10-11], [100-102], [15-17], [200-202]
  ;;
  ;; Input should be a cons, ranges a sorted list of mapping structs. Note that
  ;; both of these use lispy half-open intervals (unlike the example above).
  (iterate
    (with (start . end) = input)
    (with n = start)
    (with r = (pop ranges))
    (while (< n end))
    (when (null r)
      (collect (cons n end))
      (finish))
    (for rs = (mapping-src-start r))
    (for re = (mapping-src-end r))
    (cond ((< n rs)
           (let ((bound (min end rs)))
             (collect (cons n bound))
             (setf n bound)))
          ((>= n re)
           (setf r (pop ranges)))
          (t (let ((stop (min end re)))
               (collect (cons (src->dst r n)
                              (src->dst r stop)))
               (setf n stop))))))

(defun map-ranges (inputs ranges)
  (alexandria:mappend (rcurry #'map-single-range ranges) inputs))

(defun traverse (almanac seeds)
  (reduce #'map-ranges almanac :initial-value seeds))

(define-problem (2023 5) (data alexandria:read-stream-content-into-string) (324724204 104070862)
  (multiple-value-bind (seeds almanac) (parse data)
    (let ((part-1 (iterate (for seed :in seeds)
                           (collect (cons seed (1+ seed)))))
          (part-2 (iterate (for (start length) :on seeds :by #'cddr)
                           (collect (cons start (+ start length))))))
      (values (reduce #'min (mapcar #'car (traverse almanac part-1)))
              (reduce #'min (mapcar #'car (traverse almanac part-2)))))))

#; Scratch --------------------------------------------------------------------
