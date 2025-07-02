(defsystem :analysis
  :depends-on (
               ;; base lang tools
               :qlot
               :coalton ; coalton-lang/coalton
               ;; for coalton fset+me must be current from repo
               :fset ; slburson/fset/
               :misc-extensions ; slburson/misc-extensions/
               :alexandria
               :access
               :arrow-macros

               ;; analysis tools
               :lisp-stat
               :cl-csv
               :cl-str
               :filepaths ; ultralisp foskers-filepaths
               :filesystem-utils
               :py4cl2
               )
  :serial t
  :components ((:file "analysis") ; a .lisp file
               (:static-file "README.org")))
