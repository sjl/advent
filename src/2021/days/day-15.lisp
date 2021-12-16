(advent:defpackage* :advent/2021/15)
(in-package :advent/2021/15)

(defun-inline cref (array coord)
  (aref array (realpart coord) (imagpart coord)))

(defun-inline validp (array coord)
  (array-in-bounds-p array (realpart coord) (imagpart coord)))

(defun-inline neighbors (array coord)
  (loop :for δ in '(#c(-1 0) #c(1 0) #c(0 -1) #c(0 1))
        :for n = (+ coord δ)
        :when (validp array n) :collect n))

(defun cost (data from to)
  (declare (ignore from))
  (cref data to))

(defun find-path (data)
  (declare (inline astar curry)
           (optimize (speed 3) (debug 1) (safety 1)))
  (let ((goal (complex (1- (array-dimension data 0))
                       (1- (array-dimension data 1)))))
    (astar :start #c(0 0)
           :neighbors (curry #'neighbors data)
           :goalp (curry #'= goal)
           :cost (curry #'cost data)
           :heuristic (curry #'manhattan-distance goal)
           :test #'eql)))

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
