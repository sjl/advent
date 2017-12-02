(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :extremum
               :rcurry
               :read-file-into-string

               )
  :package "ADVENT.QUICKUTILS")
