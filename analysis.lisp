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

; set config
;; (py:initialize)
;; (print py4cl2:*config*) ;; this triggers the company auto complete hang on lab linux
;; (setf (config-var pycmd) "python3") ; set one field

                                        ; ensure version and sys.path is same as python in cli
(py:pyversion-info)    ; fails if python command is not resolved in system
;; (py:defpymodule "sys" nil :lisp-package "SYS")
;; (py:defpymodule "pprint" nil :lisp-package "PPRINT")
;; (py:pyexec "pprint.pprint(sys.path)")

                                        ; py process hard reset
(py:pystop)
(py:python-alive-p)
(py:pystart)
                                        ; python imports
(py:defpymodule "rasterio" t :lisp-package "RASTERIO") ; drivers: GTiff GPKG
(py:defpymodule "geopandas" nil :lisp-package "GEOPANDAS")
(py:defpymodule "sklearn" nil :lisp-package "SKLEARN")


; open the file
(defparameter *gtif-true* #P"/home/holdens/db/1/masters/products/predictions1percent/height.tiff")

;;py to cl: dataset = rasterio.open('some.tif')
(defparameter *dataset* (rasterio:open :fp (namestring *gtif-true*)))
(print *dataset*)
(py:pyslot-list *dataset* ) ; ie. show me what data it has
;; => ("__class__" "__dict__" "__doc__" "__firstlineno__" "__module__" "__pyx_vtable__" "__static_attributes__" "__weakref__" "_block_shapes" "_closed" "_count" "_crs" "_crs_wkt" "_descriptions" "_dtypes" "_env" "_gcps" "_nodatavals" "_offsets" "_rpcs" "_scales" "_transform" "_units" "block_shapes" "bounds" "closed" "colorinterp" "compression" "count" "crs" "descriptions" "driver" "dtypes" "files" "gcps" "height" "indexes" "interleaving" "is_tiled" "mask_flag_enums" "meta" "mode" "name" "nodata" "nodatavals" "offsets" "options" "photometric" "profile" "res" "rpcs" "scales" "shape" "subdatasets" "transform" "units" "width")
(py:pymethod-list *dataset*) ; ie show me what methods it has
;; => ("__class__" "__delattr__" "__dir__" "__enter__" "__eq__" "__exit__" "__format__" "__ge__" "__getattribute__" "__getstate__" "__gt__" "__hash__" "__init__" "__init_subclass__" "__le__" "__lt__" "__ne__" "__new__" "__reduce__" "__reduce_ex__" "__repr__" "__setattr__" "__setstate__" "__sizeof__" "__str__" "__subclasshook__" "_get_crs" "_get_rpcs" "_handle_crswkt" "_has_band" "_has_gcps_or_rpcs" "_mask_flags" "_read" "_set_all_descriptions" "_set_all_offsets" "_set_all_scales" "_set_all_units" "_set_attrs_from_dataset_handle" "_set_crs" "_set_gcps" "_set_nodatavals" "_set_rpcs" "block_size" "block_window" "block_windows" "checksum" "close" "colormap" "dataset_mask" "get_gcps" "get_nodatavals" "get_tag_item" "get_transform" "index" "lnglat" "overviews" "read" "read_crs" "read_masks" "read_transform" "sample" "start" "statistics" "stats" "stop" "tag_namespaces" "tags" "window" "window_bounds" "window_transform" "write_transform" "xy")

                                        ; operations on geotiff
;;;; slot actions
;; dataset.name
(py:pyslot-value *dataset* 'name)
;; dataset.count
(py:pyslot-value *dataset* 'count)
;; dataset.dtypes
(py:pyslot-value *dataset* 'dtypes)
;; dataset.crs
(py:pyslot-value *dataset* 'crs)
;; dataset.nodata
(py:pyslot-value *dataset* 'nodata)
;; dataset.shape
(py:pyslot-value *dataset* 'shape)
;; dataset-bounds
(py:pyslot-value *dataset* 'bounds)

;;;; method actions
(py:pymethod *dataset* 'get_nodatavals) ; works
;; get first band
;;(py4cl2:python-call "lambda ds: ds.read(1)" *dataset*)
;; (py:pymethod *dataset* "read" 1) ; WARN heap exhausted



#|


3. You can then use the dataset object to perform operations. For example:

```lisp

;; Read the first band of the raster
```

Remember to close the dataset when you're done:

```lisp
(py4cl2:python-call "lambda ds: ds.close()" *dataset*)
```

This approach should work around the "No generic type" error you're encountering, as it uses py4cl2's python-call function to directly invoke Python methods.

|#
