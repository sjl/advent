(advent:defpackage* :advent/2019/16)
(in-package :advent/2019/16)

(defparameter *pattern* #(0 1 0 -1))

(defun compute-element (input i)
  (iterate
    (for x :in-vector input)
    (generate p :around *pattern*)
    (if-first-time (next p)) ; initialize pattern
    (for c :modulo (1+ i) :from 1) ; skip first element in the expanded pattern
    (when (zerop c)
      (next p))
    (summing (* x p) :into result)
    (returning (mod (abs result) 10))))

(defun run-phase (input output)
  (iterate
    (for i :below (length output))
    (setf (aref output i) (compute-element input i))))

(defun fft (input &optional (n 1))
  (let ((input (fresh-vector input))
        (output (fresh-vector input)))
    (do-repeat n
      (run-phase input output)
      (rotatef input output))
    input))

(defun part2 (digits)
  ;; This is a dumb hack.
  ;;
  ;; Because the message is in the latter half of the result, we can cheat and
  ;; take advantage of the fact that for any element in the last half of the
  ;; input, the result is always just the sum of the tail of the array starting
  ;; at that element.
  ;;
  ;; This is because by the time we're in the back half of the array, the (0
  ;; 1 0 -1) input pattern repeats the 0 i times (which wipes out everything
  ;; before i) and the 1 i+1 times (which means we just sum up the rest of the
  ;; array):
  ;;
  ;;                         0  1  2  3  4  5  6  7  8  9 10 11 12
  ;;                input    a  b  c  d  e  f  g  h  i  j  k  l  m  len = 13
  ;;    pattern for i = 0    1  0 -1  0  1  0 -1  0  1  0 -1  0  1
  ;;    pattern for i = 1    0  1  1  0  0 -1 -1  0  0  1  1  0  0
  ;;    pattern for i = 2    0  0  1  1  1  0  0  0 -1 -1 -1  0  0
  ;;    pattern for i = 3    0  0  0  1  1  1  1  0  0  0  0 -1 -1
  ;;    pattern for i = 4    0  0  0  0  1  1  1  1  1  0  0  0  0
  ;;    pattern for i = 5    0  0  0  0  0  1  1  1  1  1  1  0  0
  ;;    pattern for i = 6    0  0  0  0  0  0  1  1  1  1  1  1  1
  ;;                         <---- all zeroes  all ones --------->
  ;;
  ;; Additionally: by starting at the end of the array we don't need a temporary
  ;; array, we can just keep a running sum and not worrying about destroying the
  ;; input.
  ;;
  ;; This is cheating, but whatever, I didn't really like this problem much
  ;; anyway.
  (let* ((digits (coerce (iterate (repeat 10000)
                                  (appending digits))
                         'vector))
         (offset (digits-to-number (subseq digits 0 7)))
         (data (subseq digits offset)))
    (do-repeat 100
      (iterate
        (for x :in-vector data :with-index i :from (1- (length data)) :downto 0)
        (summing x :into n)
        (setf (aref data i) (mod n 10))))
    (subseq data 0 8)))

(defun digits-string (digits)
  (map 'string #'digit-char digits))

(define-problem (2019 16) (data read-digits) ("96136976" "85600369")
  (values (_ (fft data 100)
            (subseq _ 0 8)
            digits-string)
          (digits-string (part2 data))))

#; Scratch --------------------------------------------------------------------

(part2
  '(0 3 0 3 6 7 3 2 5 7 7 2 1 2 9 4 4 0 6 3 4 9 1 5 6 5 4 7 4 6 6 4 ))
