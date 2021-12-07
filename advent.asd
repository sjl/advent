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
               :bordeaux-threads
               :cl-digraph
               :cl-digraph.dot
               :cl-interpol
               :cl-netpbm
               :cl-ppcre
               :iterate
               :jpl-queues
               :losh
               :md5
               :named-readtables
               :pileup
               :split-sequence
               :str

               )

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:module "2016" :serial t
                              :components ((:auto-module "days")))
                             (:module "2017" :serial t
                              :components ((:file "number-spiral")
                                           (:file "knot-hash")
                                           (:auto-module "days")))
                             (:module "2018" :serial t
                              :components ((:auto-module "days")))
                             (:module "2019" :serial t
                              :components ((:file "intcode")
                                           (:auto-module "days")))
                             (:module "2020" :serial t
                              :components ((:auto-module "days")))
                             (:module "2021" :serial t
                              :components ((:auto-module "days")))))))
