;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:CURRY :EXTREMUM :RCURRY :READ-FILE-INTO-STRING) :ensure-package T :package "ADVENT.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ADVENT.QUICKUTILS")
    (defpackage "ADVENT.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "ADVENT.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:MAKE-GENSYM-LIST :ENSURE-FUNCTION
                                         :CURRY :EXTREMUM :RCURRY :ONCE-ONLY
                                         :WITH-OPEN-FILE* :WITH-INPUT-FROM-FILE
                                         :READ-FILE-INTO-STRING))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (defun extremum (sequence predicate &key key (start 0) end)
    "Returns the element of `sequence` that would appear first if the subsequence
bounded by `start` and `end` was sorted using `predicate` and `key`.

`extremum` determines the relationship between two elements of `sequence` by using
the `predicate` function. `predicate` should return true if and only if the first
argument is strictly less than the second one (in some appropriate sense). Two
arguments `x` and `y` are considered to be equal if `(funcall predicate x y)`
and `(funcall predicate y x)` are both false.

The arguments to the `predicate` function are computed from elements of `sequence`
using the `key` function, if supplied. If `key` is not supplied or is `nil`, the
sequence element itself is used.

If `sequence` is empty, `nil` is returned."
    (let* ((pred-fun (ensure-function predicate))
           (key-fun (unless (or (not key) (eq key 'identity) (eq key #'identity))
                      (ensure-function key)))
           (real-end (or end (length sequence))))
      (cond ((> real-end start)
             (if key-fun
                 (flet ((reduce-keys (a b)
                          (if (funcall pred-fun
                                       (funcall key-fun a)
                                       (funcall key-fun b))
                              a
                              b)))
                   (declare (dynamic-extent #'reduce-keys))
                   (reduce #'reduce-keys sequence :start start :end real-end))
                 (flet ((reduce-elts (a b)
                          (if (funcall pred-fun a b)
                              a
                              b)))
                   (declare (dynamic-extent #'reduce-elts))
                   (reduce #'reduce-elts sequence :start start :end real-end))))
            ((= real-end start)
             nil)
            (t
             (error "Invalid bounding indexes for sequence of length ~S: ~S ~S, ~S ~S"
                    (length sequence)
                    :start start
                    :end end)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defmacro with-open-file* ((stream filespec &key direction element-type
                                                   if-exists if-does-not-exist external-format)
                             &body body)
    "Just like `with-open-file`, but `nil` values in the keyword arguments mean to use
the default value specified for `open`."
    (once-only (direction element-type if-exists if-does-not-exist external-format)
      `(with-open-stream
           (,stream (apply #'open ,filespec
                           (append
                            (when ,direction
                              (list :direction ,direction))
                            (when ,element-type
                              (list :element-type ,element-type))
                            (when ,if-exists
                              (list :if-exists ,if-exists))
                            (when ,if-does-not-exist
                              (list :if-does-not-exist ,if-does-not-exist))
                            (when ,external-format
                              (list :external-format ,external-format)))))
         ,@body)))
  

  (defmacro with-input-from-file ((stream-name file-name &rest args
                                                         &key (direction nil direction-p)
                                                         &allow-other-keys)
                                  &body body)
    "Evaluate `body` with `stream-name` to an input stream on the file
`file-name`. `args` is sent as is to the call to `open` except `external-format`,
which is only sent to `with-open-file` when it's not `nil`."
    (declare (ignore direction))
    (when direction-p
      (error "Can't specifiy :DIRECTION for WITH-INPUT-FROM-FILE."))
    `(with-open-file* (,stream-name ,file-name :direction :input ,@args)
       ,@body))
  

  (defun read-file-into-string (pathname &key (buffer-size 4096) external-format)
    "Return the contents of the file denoted by `pathname` as a fresh string.

The `external-format` parameter will be passed directly to `with-open-file`
unless it's `nil`, which means the system default."
    (with-input-from-file
        (file-stream pathname :external-format external-format)
      (let ((*print-pretty* nil))
        (with-output-to-string (datum)
          (let ((buffer (make-array buffer-size :element-type 'character)))
            (loop
              :for bytes-read = (read-sequence buffer file-stream)
              :do (write-sequence buffer datum :start 0 :end bytes-read)
              :while (= bytes-read buffer-size)))))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(curry extremum rcurry read-file-into-string)))

;;;; END OF quickutils.lisp ;;;;
