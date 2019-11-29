(defclass auto-module (module) ())

(defmethod component-children ((self auto-module))
  (mapcar (lambda (p) (make-instance 'cl-source-file :type "lisp"
                        :pathname p
                        :name (pathname-name p)
                        :parent (component-parent self)))
          (directory-files (component-pathname self)
                           (make-pathname :directory nil :name *wild* :type "lisp"))))


(asdf:defsystem :advent
  :description "Advent of Code solutions"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"

  :depends-on (

               :1am
               :alexandria
               :beast
               :cl-digraph
               :cl-digraph.dot
               :cl-interpol
               :cl-ppcre
               :iterate
               :losh
               :named-readtables
               :pileup
               :split-sequence
               :str

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "utils")
                             (:file "number-spiral")
                             (:auto-module "2018")))))
