(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry
               :read-file-into-string

               )
  :package "ADVENT.QUICKUTILS")
