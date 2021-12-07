(advent:defpackage* :advent/2021/06)
(in-package :advent/2021/06)

(defun parse (stream)
  (mapcar #'parse-integer (str:split #\, (alexandria:read-stream-content-into-string stream))))

(defun simulate (data days)
  (let ((curr (make-array 9 :initial-element 0))
        (next (make-array 9)))
    (dolist (fish data)
      (incf (aref curr fish)))
    (do-repeat days
      (loop :for i :from 8 :downto 0
            :for n = (aref curr i)
            :do (if (zerop i)
                  (progn (setf (aref next 8) n)
                         ;; downto is important to make sure 6 is already set
                         (incf (aref next 6) n))
                  (setf (aref next (1- i)) n)))
      (rotatef curr next))
    curr))

(define-problem (2021 6) (data read-comma-separated-integers)
    (371379 1674303997472)
  (values
    (summation (simulate data 80))
    (summation (simulate data 256))))


#; Scratch --------------------------------------------------------------------
