;;;; =================================== setup

;; spc spc conda-env-activate analysis ;get conda env
;; ,' ;attach repl

;;;; ===================================  set environment

;;                                         ; imports
(ql:quickload :qlot)
(ql:quickload :coalton)
(ql:quickload :fset)
(ql:quickload :misc-extensions)
(ql:quickload :alexandria)
(ql:quickload :serapeum)
(ql:quickload :access)
(ql:quickload :arrow-macros)
(ql:quickload :lparallel)
(ql:quickload :lisp-stat)
(ql:quickload :cl-csv)
(ql:quickload :str)
(ql:quickload :filepaths); ultralisp foskers-filepaths
(ql:quickload :filesystem-utils)
(ql:quickload :py4cl2)


(defpackage :analysis
  (:use :cl)
  ;;(:local-nicknames (:py :py4cl ))
  (:local-nicknames (:py :py4cl2 ))
  (:local-nicknames (:col :coalton))
  (:local-nicknames (:csv :cl-csv))
  (:local-nicknames (:acc :access))
  (:import-from :arrow-macros :-<> :<>))

                                        ; enter package
(in-package :analysis) ; Also enter this in the REPL!

;; (py:initialize)
;; (print py4cl2:*config*) ;; this triggers the company auto complete hang on lab linux
;; (setf (config-var pycmd) "python3") ; set one field

                                        ; ensure version and sys.path is same as python in cli
(py:pyversion-info)    ; fails if python command is not resolved in system
;; (py:defpymodule "sys" nil :lisp-package "SYS")
;; (py:defpymodule "pprint" nil :lisp-package "PPRINT")
;; (py:pyexec "pprint.pprint(sys.path)")

                                        ; python imports
(py:defpymodule "sklearn" nil :lisp-package "SKLEARN")
(py:defpymodule "scipy" nil :lisp-package "SCIPY")
(py:defpymodule "osgeo" nil :lisp-package "OSGEO")
(py:defpymodule "rasterio" nil :lisp-package "RASTERIO")
