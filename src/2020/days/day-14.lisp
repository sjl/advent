(advent:defpackage* :advent/2020/14)
(in-package :advent/2020/14)

(defun parse (stream)
  (iterate
    (for line :in-stream stream :using #'read-line)
    (collect (or (ppcre:register-groups-bind ((#'parse-integer addr val))
                     ("mem\\[(\\d+)\\] = (\\d+)" line)
                   (list :set addr val))
                 (ppcre:register-groups-bind (mask)
                     ("mask = ([X01]+)" line)
                   (list :mask mask))))))

(defun op (inst) (first inst))
(defun mask (inst) (second inst))
(defun addr (inst) (second inst))
(defun value (inst) (third inst))

(defun-inline char->bit (char)
  (ecase char (#\0 0) (#\1 1)))

(defun-inline dpbit (bit pos value)
  (dpb bit (byte 1 pos) value))

(defun mask-value (mask value)
  (iterate (for char :in-string mask :downto 0)
           (for pos :from 0)
           (unless (eql #\X char)
             (setf value (dpbit (char->bit char) pos value))))
  value)

(defun mask-address (mask addr)
  (iterate (with-result addrs = (list addr))
           (for pos :from 0)
           (for char :in-string mask :downto 0)
           (ecase char
             (#\X (setf addrs (append (mapcar (curry #'dpbit 0 pos) addrs)
                                      (mapcar (curry #'dpbit 1 pos) addrs))))
             (#\0 (progn))
             (#\1 (map-into addrs (curry #'dpbit 1 pos) addrs)))))

(defun run-program (program &key part)
  (iterate (with memory = (make-hash-table))
           (with mask = nil)
           (for inst :in program)
           (ecase (op inst)
             (:mask (setf mask (mask inst)))
             (:set (ecase part
                     (1 (setf (gethash (addr inst) memory 0)
                              (mask-value mask (value inst))))
                     (2 (dolist (addr (mask-address mask (addr inst)))
                          (setf (gethash addr memory 0)
                                (value inst)))))))
           (finally (return (reduce #'+ (alexandria:hash-table-values memory))))))

(define-problem (2020 14) (data parse) (9967721333886 4355897790573)
  (values
    (run-program data :part 1)
    (run-program data :part 2)))


#; Scratch --------------------------------------------------------------------
