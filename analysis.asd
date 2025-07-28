(defsystem :analysis
  :depends-on (
               ;; base lang tools

               :qlot ; ql qlot
               :coalton ; github coalton-lang/coalton
               ;; for coalton fset and misc-extensions must be current from repo
               :fset ; github slburson/fset
               :misc-extensions ; github slburson/misc-extensions
               :alexandria ; ql alexandria
               :access ; ql access
               :arrow-macros ; ql arrow-macros

               ;; analysis tools
               :lisp-stat ; ql lisp-stat
               :numcl ;ql numcl
               :cl-csv ; ql cl-csv
               :cl-str ; ql cl-str
               :filepaths ; ultralisp fosskers-filepaths
               :filesystem-utils ; ql filesystem-utils
               :py4cl2 ; ql py4cl2
               :rcl ;ql rcl
               )
  :serial t
  :components ((:file "analysis") ; a .lisp file
               (:static-file "README.org")))
