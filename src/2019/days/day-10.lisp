(advent:defpackage* :advent/2019/10)
(in-package :advent/2019/10)



(defun equivalence-classes (equiv seq) ; From quickutil TODO replace this 
  "Partition the sequence `seq` into a list of equivalence classes
defined by the equivalence relation `equiv`."
  (let ((classes nil))
    (labels ((find-equivalence-class (x)
               (member-if (lambda (class)
                            (funcall equiv x (car class)))
                          classes))

             (add-to-class (x)
               (let ((class (find-equivalence-class x)))
                 (if class
                   (push x (car class))
                   (push (list x) classes)))))
      (declare (dynamic-extent (function find-equivalence-class)
                               (function add-to-class))
               (inline find-equivalence-class
                       add-to-class))

      ;; Partition into equivalence classes.
      (map nil #'add-to-class seq)

      ;; Return the classes.
      classes)))

(defun asteroid-positions (map)
  "Return a list of the asteroid positions in the 2D input `map`."
  (destructuring-bind (rows cols) (array-dimensions map)
    (iterate
      (for-nested ((row :from 0 :below rows)
                   (col :from 0 :below cols)))
      (when (char= #\# (aref map row col))
        ;; swap axes to match the problem text
        (collect (complex col row))))))

(defun slope (v)
  (let-complex (v)
    (if (zerop vy)
      nil
      (/ vx vy))))

(defun slope= (a b)
  (eql (slope a)
       (slope b)))

(defun sign= (a b)
  (and (= (signum (x a)) (signum (x b)))
       (= (signum (y a)) (signum (y b)))))

(defun colinearp (origin a b)
  (let ((va (- a origin))
        (vb (- b origin)))
    (and (sign= va vb)
         (slope= va vb))))

(defun group (asteroids origin)
  (equivalence-classes (curry #'colinearp origin)
                       (remove origin asteroids)))

(defun count-seen (asteroids pos)
  (length (group asteroids pos)))

(defun angle (a b)
  (mod (- (atan (y a) (x a))
          (atan (y b) (x b)))
       tau))

(defun distance (a b)
  (abs (- a b)))

(defun splice (list)
  "Splice `list` into a circular list."
  (setf (cdr (last list)) list))

(defun part1 (data)
  (iterate
    (with asteroids = (asteroid-positions data))
    (for pos :in asteroids)
    (finding pos :maximizing (count-seen asteroids pos) :into (best score))
    (returning best score)))

(defun part2 (data origin &optional (n 200))
  (flet ((group-angle (group)
           (angle #c(0 -1) (- (first group) origin)))
         (sort-group (group)
           (sort group #'< :key (curry #'distance origin))))
    (iterate
      (with groups = (_ data
                       asteroid-positions
                       (group _ origin)
                       (mapcar #'sort-group _)
                       (sort _ #'< :key #'group-angle)
                       (apply #'ring _)))
      (repeat n)
      (for pos = (pop (ring-data groups)))
      (if (null (ring-data groups))
        ;; We go in the opposite direction than expected because the Y axis is
        ;; annoyingly flipped in the problem.
        (ring-cutf groups :prev t)
        (ring-prevf groups))
      (returning pos))))

(define-problem (2019 10) (data read-2d-array) (284 404)
  (multiple-value-bind (origin score) (part1 data)
    (let ((lucky (part2 data origin)))
      (values score (+ (* (x lucky) 100) (y lucky))))))


#; Scratch --------------------------------------------------------------------
