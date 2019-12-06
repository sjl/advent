(defpackage :advent/2018/13 #.cl-user::*advent-use*)
(in-package :advent/2018/13)

;;;; Cart ---------------------------------------------------------------------
(defun left (velocity)
  (* #c(0 -1) velocity))

(defun right (velocity)
  (* #c(0 1) velocity))

(defun straight (velocity)
  velocity)

(defun horizontalp (velocity)
  (zerop (imagpart velocity)))

(defun verticalp (velocity)
  (zerop (realpart velocity)))


(defparameter *ai* '#1=(left straight right . #1#))

(defstruct cart
  position
  velocity
  (ai *ai*))


(defun turn-intersection (cart)
  (callf (cart-velocity cart) (pop (cart-ai cart))))

(defun turn-corner (cart corner)
  (callf (cart-velocity cart)
         (let ((v (verticalp (cart-velocity cart))))
           (ecase corner
             (#\\ (if v #'left #'right))
             (#\/ (if v #'right #'left))))))


(defun cart-rune (cart)
  (ecase (cart-velocity cart)
    (#c(0 -1) #\^)
    (#c(0 1) #\v)
    (#c(-1 0) #\<)
    (#c(1 0) #\>)))


;;;; Carts ---------------------------------------------------------------------
(defun make-carts (sequence)
  (iterate (for cart :in-whatever sequence)
           (collect-hash ((cart-position cart) cart))))

(defun cart-at (carts position)
  (gethash position carts))

(defun insert-cart (carts cart)
  (setf (gethash (cart-position cart) carts) cart))

(defun remove-cart (carts cart)
  (remhash (cart-position cart) carts))

(define-condition collision ()
  ((position :initarg :position :accessor collision-position)))

(defun move-cart (carts cart)
  (with-slots (position velocity) cart
    (remove-cart carts cart)
    (incf position velocity)
    (if-let ((other-cart (cart-at carts position)))
      (restart-case (error 'collision :position position)
        (remove-crashed-carts ()
          (remove-cart carts other-cart)
          (push cart *dead-carts*)
          (push other-cart *dead-carts*)))
      (insert-cart carts cart)))
  (values))

(defun remove-crashed-carts (condition)
  (declare (ignore condition))
  (invoke-restart 'remove-crashed-carts))


;;;; Track --------------------------------------------------------------------
(deftype track ()
  '(simple-array character (* *)))

(defun track-at (track position)
  (aref track (realpart position) (imagpart position)))

(defun print-track (track carts)
  (destructuring-bind (width height) (array-dimensions track)
    (dotimes (y height)
      (dotimes (x width)
        (when (zerop x)
          (terpri))
        (write-char
          (if-let ((cart (cart-at carts (complex x y))))
            (cart-rune cart)
            (aref track x y)))))
    (terpri)))

(defun cornerp (track position)
  (find (track-at track position) "\\/"))

(defun intersectionp (track position)
  (char= (track-at track position) #\+))


;;;; Input Parsing ------------------------------------------------------------
(defun cart-rune-velocity (rune)
  (ecase rune
    (#\^ #c(0 -1))
    (#\v #c(0 1))
    (#\< #c(-1 0))
    (#\> #c(1 0))))

(defun cart-rune-p (rune)
  (find rune "^v<>"))

(defun track-rune (cart-or-track-rune)
  (case cart-or-track-rune
    ((#\^ #\v) #\|)
    ((#\< #\>) #\-)
    (t cart-or-track-rune)))

(defun parse-track (lines)
  (removef lines "" :test #'string=)
  (let ((track (make-array (list (extremum (mapcar #'length lines) '>)
                                 (length lines))
                 :element-type 'character
                 :initial-element #\space))
        (carts nil))
    (iterate
      (for line :in lines)
      (for y :from 0)
      (iterate
        (for rune :in-string line)
        (for x :from 0)
        (when (cart-rune-p rune)
          (push (make-cart
                  :position (complex x y)
                  :velocity (cart-rune-velocity rune))
                carts))
        (setf (aref track x y) (track-rune rune))))
    (values track (make-carts carts))))


;;;; Simulation ---------------------------------------------------------------
(defparameter *dead-carts* nil)

(defun tick-cart (track carts cart)
  (unless (member cart *dead-carts*)
    (move-cart carts cart)
    (let ((pos (cart-position cart)))
      (cond
        ((cornerp track pos) (turn-corner cart (track-at track pos)))
        ((intersectionp track pos) (turn-intersection cart)))))
  (values))

(defun cart< (cart1 cart2)
  (let* ((p1 (cart-position cart1))
         (p2 (cart-position cart2))
         (x1 (realpart p1))
         (y1 (imagpart p1))
         (x2 (realpart p2))
         (y2 (imagpart p2)))
    ;; cart1 moves before cart2 if it's:
    (or (< y1 y2) ; higher up
        (and (= y1 y2) (< x1 x2))))) ; or further left

(defun tick-carts (track carts)
  (dolist (cart (sort (hash-table-values carts) #'cart<))
    (tick-cart track carts cart)))


;;;; Solve --------------------------------------------------------------------
(defparameter *example* (format nil "
/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/
"))

(defun format-position (position)
  (format nil "~D,~D" (realpart position) (imagpart position)))

(defun part-1 (lines)
  (multiple-value-bind (track carts) (parse-track lines)
    (handler-case (loop (tick-carts track carts))
      (collision (collision) (format-position (collision-position collision))))))

(defun part-2 (lines)
  (multiple-value-bind (track carts) (parse-track lines)
    (handler-bind ((collision #'remove-crashed-carts))
      (iterate
        (tick-carts track carts)
        (for tick :from 1)
        (for carts-remaining = (hash-table-count carts))
        (finding
          (-<> carts hash-table-values first cart-position format-position)
          :such-that (= 1 carts-remaining))))))

(define-problem (2018 13) (data read-lines) ("83,49" "73,36")
  (values (part-1 data)
          (part-2 data)))
