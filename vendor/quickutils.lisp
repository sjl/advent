;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:CURRY :RCURRY :RANGE :COMPOSE :ENSURE-KEYWORD :READ-FILE-INTO-STRING) :ensure-package T :package "ADVENT.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ADVENT.QUICKUTILS")
    (defpackage "ADVENT.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "ADVENT.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:MAKE-GENSYM-LIST :ENSURE-FUNCTION
                                         :CURRY :RCURRY :RANGE :COMPOSE
                                         :ENSURE-KEYWORD :ONCE-ONLY
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
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (defun range (start end &key (step 1) (key 'identity))
    "Return the list of numbers `n` such that `start <= n < end` and
`n = start + k*step` for suitable integers `k`. If a function `key` is
provided, then apply it to each number."
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i)))
  

  (defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  

  (defun ensure-keyword (x)
    "Ensure that a keyword is returned for the string designator `x`."
    (values (intern (string x) :keyword)))
  

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
  (export '(curry rcurry range compose ensure-keyword read-file-into-string)))

;;;; END OF quickutils.lisp ;;;;
