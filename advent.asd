(asdf:defsystem :advent
  :description "Advent of Code solutions"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"

  :depends-on (:iterate :losh :split-sequence :str)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:module "2018" :serial t
                              :components ((:file "main")))))))
