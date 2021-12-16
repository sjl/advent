(advent:defpackage* :advent/2021/15)
(in-package :advent/2021/15)


(defun-inline coord (row col) (cons row col))
(defun-inline row (coord) (car coord))
(defun-inline col (coord) (cdr coord))

(defun-inline coord= (a b)
  (and (= (row a) (row b))
       (= (col a) (col b))))

(defun-inline cref (array coord)
  (aref array (row coord) (col coord)))

(defun-inline (setf cref) (value array coord)
  (setf (aref array (row coord) (col coord)) value))

(defun-inline neighbors (array coord)
  (loop :for δ in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
        :for r = (+ (row coord) (row δ))
        :for c = (+ (col coord) (col δ))
        :when (array-in-bounds-p array r c)
        :collect (coord r c)))

(defun find-path (data)
  (declare (inline astar) (optimize (speed 3) (debug 1) (safety 1)))
  (let ((seen (make-array (array-dimensions data) :initial-element nil))
        (goal (coord (1- (array-dimension data 0))
                     (1- (array-dimension data 1)))))
    (astar :test #'equal
           :start (coord 0 0)
           :neighbors (lambda (state) (neighbors data state))
           :goalp (lambda (state) (coord= goal state))
           :cost (lambda (from to) (declare (ignore from)) (cref data to))
           :get-seen (lambda (state) (cref seen state))
           :set-seen (lambda (state cost) (setf (cref seen state) cost))
           ;; Manhattan distance is the only candidate for a heuristic, but for
           ;; this problem it's not particularly helpful and slows things down.
           ;; Just use a constant and degrade to Dijkstra.
           :heuristic (constantly 0))))

(defun path-cost (data path)
  (reduce #'+ (rest path) :key (curry #'cref data)))

(defun expand (small)
  (let* ((srows (array-dimension small 1))
         (scols (array-dimension small 1))
         (brows (* 5 srows))
         (bcols (* 5 scols))
         (big (make-array (list brows bcols))))
    (dotimes (r brows)
      (dotimes (c bcols)
        (setf (aref big r c)
              (_ (aref small (mod r srows) (mod c scols))
                (+ _ (truncate r srows) (truncate c scols))
                (if (> _ 9) (- _ 9) _)))))
    big))

(define-problem (2021 15) (stream) (503 2853)
  (let* ((small (read-2d-array stream :key #'digit-char-p))
         (big (expand small)))
    (values (path-cost small (find-path small))
            (path-cost big (find-path big)))))

#; Scratch --------------------------------------------------------------------
