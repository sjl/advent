(defpackage :advent/spiral
  (:use :cl :losh :iterate :advent.quickutils)
  (:export :number-coordinates))

(in-package :advent/spiral)

(defun layer-side-length (layer)
  "Return the length of one side of `layer`."
  (1+ (* 2 layer)))

(defun layer-size (layer)
  "Return the total size of a number spiral with a final layer of `layer`."
  (square (layer-side-length layer)))

(defun layer-for-number (number)
  "Return the index of the layer containing `number`."
  (ceiling (/ (1- (sqrt number)) 2)))

(defun layer-start (layer)
  "Return the smallest number in `layer`."
  (if (zerop layer)
    1
    (1+ (layer-size (1- layer)))))

(defun layer-leg-length (layer)
  "Return the length of one \"leg\" of `layer`."
  (1- (layer-side-length layer)))


(defun leg (layer number)
  "Return the leg index and offset of `number` in `layer`."
  (if (= 1 number)
    (values 0 0)
    (let ((idx (- number (layer-start layer)))
          (legsize (layer-leg-length layer)))
      (values (floor idx legsize)
              (1+ (mod idx legsize))))))

(defun corner-coordinates (layer leg)
  "Return the coordinates of the corner starting `leg` in `layer`.

  Leg | Corner
   0  | Bottom Right
   1  | Top Right
   2  | Top Left
   3  | Bottom Left

  "

  ;; 2   1
  ;;
  ;; 3   0
  (ccase leg
    (0 (complex layer (- layer)))
    (1 (complex layer layer))
    (2 (complex (- layer) layer))
    (3 (complex (- layer) (- layer)))))

(defun leg-direction (leg)
  "Return the direction vector for the given `leg`.
  "
  ;;    <--
  ;;   11110
  ;; | 2   0 ^
  ;; | 2   0 |
  ;; v 2   0 |
  ;;   23333
  ;;    -->
  (ccase leg
    (0 (complex 0 1))
    (1 (complex -1 0))
    (2 (complex 0 -1))
    (3 (complex 1 0))))


(defun number-coordinates (number)
  (nest
    ;; Find the layer the number falls in.
    (let ((layer (layer-for-number number))))

    ;; Find which leg of that layer it's in, and how far along the leg it is.
    (multiple-value-bind (leg offset) (leg layer number))

    ;; Find the coordinates of the leg's corner, and its direction vector.
    (let ((corner (corner-coordinates layer leg))
          (direction (leg-direction leg))))

    ;; Start at the corner and add the offset in the leg's direction to find the
    ;; number's coordinates.
    (+ corner (* direction offset))))
