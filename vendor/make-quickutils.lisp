(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-hash-table
               :curry
               :ensure-gethash
               :extremum
               :flatten-once
               :hash-table-keys
               :hash-table-values
               :rcurry
               :read-file-into-string
               :symb

               )
  :package "ADVENT.QUICKUTILS")
