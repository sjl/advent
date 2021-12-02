(advent:defpackage* :advent/2020/09)
(in-package :advent/2020/09)

(defun find-two-addends (numbers goal &key start end)
  "Find two numbers that sum to goal."
  (iterate
    (for x :in-vector numbers :with-index i :from start :below end)
    (iterate (for y :in-vector numbers :from (1+ i) :below end)
             (when (= goal (+ x y))
               (return-from find-two-addends (values x y))))))

(defun find-invalid (numbers window-size)
  "Find the first number that does not have valid addends in the window."
  (iterate
    (for n :in-vector numbers :from window-size)
    (for (start end) :window window-size :on numbers)
    (finding n :such-that (not (find-two-addends numbers n :start start :end end)))))

(defun find-range-addends (numbers goal)
  "Find a range of numbers that sum to goal."
  (dotimes (start (length numbers))
    (iterate
      (for n :in-vector numbers :from start :with-index end)
      (summing n :into total)
      (cond
        ((= total goal) (return-from find-range-addends (values start (1+ end))))
        ((> total goal) (finish))))))

(define-problem (2020 9) (data read-numbers) (1721308972 209694133)
  (alexandria:coercef data 'vector)
  (let* ((part1 (find-invalid data 25))
         (part2 (multiple-value-bind (i j) (find-range-addends data part1)
                  (+ (alexandria:extremum data #'< :start i :end j)
                     (alexandria:extremum data #'> :start i :end j)))))
    (values part1 part2)))


#; Scratch --------------------------------------------------------------------

(find-addends
  #(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576)
  :goal 127
  :start 9 :end 14)

(run #(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576))
