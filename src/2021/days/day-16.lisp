(advent:defpackage* :advent/2021/16)
(in-package :advent/2021/16)

(defun parse (stream)
  (let ((line (read-line stream)))
    (values (parse-integer line :radix 16)
            (* 4 (length line)))))

(defun-inline rldb (size pos byte)
  (ldb (byte size (- pos (1- size))) byte))

(defun gt (a b) (if (> a b) 1 0))
(defun lt (a b) (if (< a b) 1 0))
(defun == (a b) (if (= a b) 1 0))

(defun packets (data length &aux (i (1- length)))
  (labels ((pop-bits (size)
             (prog1 (rldb size i data) (decf i size)))
           (parse-literal ()
             (iterate (for marker = (pop-bits 1))
                      (for chunk = (pop-bits 4))
                      (collect chunk :into result)
                      (until (zerop marker))
                      (finally (return (digits->number result :radix 16)))))
           (parse-operator ()
             (ecase (pop-bits 1)
               (0 (loop :with subpacket-length = (pop-bits 15)
                        :with end = (- i subpacket-length)
                        :while (> i end)
                        :collect (parse-packet)))
               (1 (loop :with number-of-subpackets = (pop-bits 11)
                        :repeat number-of-subpackets
                        :collect (parse-packet)))))
           (op (type-id)
             (aref #(+ * min max quote gt lt ==) type-id))
           (parse-packet ()
             (let ((version (pop-bits 3))
                   (type-id (pop-bits 3)))
               (list* :version version :op (op type-id)
                      (case type-id
                        (4 (list :value (parse-literal)))
                        (t (list :contents (parse-operator))))))))
    (parse-packet)))

(defun version-sum (packet)
  (reduce #'+ (getf packet :contents)
          :key #'version-sum :initial-value (getf packet :version)))

(defun packet-sum (packet &aux (op (getf packet :op)))
  (case op
    (quote (getf packet :value))
    (t (reduce (getf packet :op) (getf packet :contents) :key #'packet-sum))))

(define-problem (2021 16) (stream) (986 18234816469452)
  (multiple-value-bind (data length) (parse stream)
    (let ((packets (packets data length)))
      (values (version-sum packets) (packet-sum packets)))))

#; Scratch --------------------------------------------------------------------
