(advent:defpackage* :advent/2022/05)
(in-package :advent/2022/05)


;; Not really mentioned in the problem, but the maps come in sorted, i.e. the're
;; in the correct order.  So we don't need to bohter making a hash table of
;; {from: to: [...]} but can just process the maps in the order they're given.

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
  (destructuring-bind (seeds . maps) (str:split (format nil "~2%") data)
    (values (parse-seeds seeds)
            (mapcar #'parse-map maps))))

(defun src->dst (mapping n)
  (+ (mapping-dst-start mapping)
     (- n (mapping-src-start mapping))))

(defun map-single-range (input ranges)
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
  (iterate (for input :in inputs)
           (appending (map-single-range input ranges))))

(defun traverse (almanac seeds)
  (reduce #'map-ranges almanac :initial-value seeds))

;; (defun map-number (n ranges)
;;   (let ((r (bisect-right #'> ranges n :key #'mapping-src-start)))
;;     (if (or (null r) (>= n (mapping-src-end r)))
;;       n
;;       (let ((i (- n (mapping-src-start r))))
;;         (+ (mapping-dst-start r) i)))))

;; (defun traverse (almanac seed)
;;   (reduce #'map-number almanac :initial-value seed))


(define-problem (2023 5) (data alexandria:read-stream-content-into-string) (324724204)
  (multiple-value-bind (seeds almanac) (parse data)
    (let ((part-1 (iterate (for seed :in seeds)
                           (collect (cons seed (1+ seed)))))
          (part-2 (iterate (for (start length) :on seeds :by #'cddr)
                           (collect (cons start (+ start length))))))
      (values (reduce #'min (mapcar #'car (traverse almanac part-1)))
              (reduce #'min (mapcar #'car (traverse almanac part-2)))))))

#; Scratch --------------------------------------------------------------------
