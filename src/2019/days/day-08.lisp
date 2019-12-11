(defpackage :advent/2019/08 #.cl-user::*advent-use*)
(in-package :advent/2019/08)

(defun read-layer (stream width height)
  (let-result (layer (make-array (list height width)))
    (do-range ((y 0 height)
               (x 0 width))
      (setf (aref layer y x) (read-char stream)))))

(defun read-image (stream width height)
  (iterate (until (char= #\newline (peek-char nil stream nil #\newline)))
           (collect (read-layer stream width height))))

(defun count-digit (layer digit)
  (iterate (for d :across-flat-array layer)
           (counting (char= digit d))))

(defun pixel-color (layers y x)
  (iterate (for layer :in layers)
           (thereis (ecase (aref layer y x)
                      (#\2) ; transparent
                      (#\1 1)
                      (#\0 0)))))

(defun decode-image (layers)
  (gathering
    (destructuring-bind (height width) (array-dimensions (first layers))
      (do-range ((y 0 height)
                 (x 0 width))
        (gather (cons (complex x y) (pixel-color layers y x)))))))

(define-problem (2019 8) (stream) (1806)
  (let ((image (read-image stream 25 6)))
    (draw-bitmap (decode-image image) "out/2019-08.pbm")
    (iterate
      (for layer :in image)
      (finding (* (count-digit layer #\1)
                  (count-digit layer #\2))
               :minimizing (count-digit layer #\0)))))


#; Scratch --------------------------------------------------------------------
