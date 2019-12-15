(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-hash-table
               :curry
               :deletef
               :ensure-gethash
               :equivalence-classes
               :extremum
               :flatten-once
               :hash-table-keys
               :hash-table-values
               :once-only
               :rcurry
               :read-file-into-string
               :removef
               :symb
               :tree-collect
               :with-gensyms

               )
  :package "ADVENT.QUICKUTILS")
