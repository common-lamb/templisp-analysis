(defsystem :analysis
  :depends-on (
               :qlot
               :alexandria
               :serapeum
               :fset
               :misc-extensions ;; fset helper
               :access
               :arrow-macros
               :lparallel

               :lisp-stat
               :cl-csv
               :cl-str
               ;; :cl-gdal ;jl2/cl-gdal
               :filepaths ; ultralisp foskers-filepaths
               :filesystem-utils
               :py4cl2
               )
  :serial t
  :components ((:file "analysis") ; a .lisp file
               (:static-file "README.org")))
