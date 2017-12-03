(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry
               :range
               :compose
               :read-file-into-string

               )
  :package "ADVENT.QUICKUTILS")
