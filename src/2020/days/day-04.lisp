(advent:defpackage* :advent/2020/04)
(in-package :advent/2020/04)


(defun parse-pair (pair)
  (destructuring-bind (k v) (str:split #\: pair)
    (cons (ensure-keyword k) v)))

(defun parse-chunk (chunk)
  (_ chunk
    (nsubstitute #\space #\newline _)
    (mapcar #'parse-pair (str:split #\space _))))

(defun parse-data (data)
  (mapcar #'parse-chunk (str:split (format nil "~2%") data)))

(defun has-field-p (field passport)
  (member field passport :key #'car))

(defun validp-1 (passport)
  (iterate (for field in '(:byr :iyr :eyr :hgt :hcl :ecl :pid))
           (always (has-field-p field passport))))

(defun valid-year-p (string min max)
  (ppcre:register-groups-bind ((#'parse-integer year))
      ("^([0-9]{4})$" string)
    (<= min year max)))

(defun valid-height-p (string)
  (ppcre:register-groups-bind ((#'parse-integer n) unit)
      ("^([0-9]+)(in|cm)$" string)
    (alexandria:eswitch (unit :test #'string=)
      ("cm" (<= 150 n 193))
      ("in" (<= 59 n 76)))))

(defun valid-pid-p (string)
  (ppcre:scan "^[0-9]{9}$" string))

(defun valid-hair-color-p (string)
  (ppcre:scan "^#[0-9a-f]{6}$" string))

(defun valid-eye-color-p (string)
  (member string '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun validp-2 (passport)
  (and (validp-1 passport)
       (valid-year-p (assocdr :byr passport) 1920 2002)
       (valid-year-p (assocdr :iyr passport) 2010 2020)
       (valid-year-p (assocdr :eyr passport) 2020 2030)
       (valid-height-p (assocdr :hgt passport))
       (valid-pid-p (assocdr :pid passport))
       (valid-hair-color-p (assocdr :hcl passport))
       (valid-eye-color-p (assocdr :ecl passport))))

(define-problem (2020 4) (data alexandria:read-stream-content-into-string) (216 150)
  (let ((passports (parse-data (str:trim-right data))))
    (values (count-if #'validp-1 passports)
            (count-if #'validp-2 passports))))

#; Scratch --------------------------------------------------------------------


