
;;;; ===================================  set environment
                                        ; imports
(ql:quickload :qlot)
(ql:quickload :alexandria)
(ql:quickload :serapeum)
(ql:quickload :fset)
(ql:quickload :misc-extensions)
(ql:quickload :access)
(ql:quickload :arrow-macros)
(ql:quickload :lparallel)
(ql:quickload :lisp-stat)
(ql:quickload :cl-csv)
(ql:quickload :str)
(ql:quickload :filepaths); ultralisp foskers-filepaths
(ql:quickload :filesystem-utils)
(ql:quickload :py4cl2)
;; (ql:quickload :click)
;;(ql:quickload :cl-gdal);jl2/cl-gdal


(defpackage :analysis
  (:use
   :cl
   :py4cl2
   ))

                                        ; enter package
(in-package :analysis) ; Also enter this in the REPL!

(initialize)
(pyversion-info)    ; fails if python command is not resolved in system
(defpymodule "sys" nil :lisp-package "SYS")
(defpymodule "pprint" nil :lisp-package "PPRINT")
(pyexec "sys.path.append('/home/holdens/miniconda3/envs/analysis/lib/python3.13/site-packages')")
(pyexec "pprint.pprint(sys.path)")
;; check file is on path
(defpymodule "os" nil :lisp-package "OS")
(os:getcwd)
(os:listdir)

(defpymodule "pip" nil :lisp-package "PIP") ; worked
(defpymodule "attr" nil :lisp-package "ATTR") ; worked
(defpymodule "joblib" nil :lisp-package "JL") ; worked

(defpymodule "sklearn" nil :lisp-package "SKLEARN")
(defpymodule "scipy" nil :lisp-package "SCIPY")
(defpymodule "osgeo" nil :lisp-package "OSGEO")
(defpymodule "rasterio" nil :lisp-package "RASTERIO")

(defpymodule "" nil :lisp-package "")
