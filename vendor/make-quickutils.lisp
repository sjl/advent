(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-keyword
               :range
               :rcurry
               :read-file-into-string
               :symb

               )
  :package "ADVENT.QUICKUTILS")
