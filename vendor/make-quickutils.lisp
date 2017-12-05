(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry
               :range
               :compose
               :ensure-keyword
               :read-file-into-string

               )
  :package "ADVENT.QUICKUTILS")
