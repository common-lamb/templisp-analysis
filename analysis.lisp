;;;; ==================================== setup environment

;; get conda env
;; spc spc conda-env-activate "analysis"

;; attach repl
;; M-- ,' "qlot-8G-heap"

;;;; ==================================== create package

                                        ; imports
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
(in-package :analysis)

;;;; ==================================== package setup
;; set printer to limit depth of large objects
(setf *print-level* 100)
(setf *print-length* 50)

;;;; ==================================== contents

;; global variables

;;;; run-all-reports
;;;; multiplex file-name-parts/stats-calls into experimental runs
;;;; validate all filename file properties
;;;; validate all filename statscalls allowableness
;;;; validate file set geospatial properties match
;;;; call run-report for each

;;;; run-report
;;;; call create matrix
;;;; call mx-manipulations
;;;; call mx->lstat
;;;; call make-plots
;;;; call stat-tests
;;;; call compose-report

;;;; create matrix
;;;; call gpkg->array
;;;; call gtif->array
;;;; call array->mx
;;;; call validate matrix

;;;; gpkg->array
;;;; open gpkg file with geopandas
;;;; rasterize identities gpkg with rasterio.features.rasterize

;;;; gtif->array
;;;; open gtiff file with py4cl2 rasterio

;;;; array->mx
;;;; convert arrays to magicl matrix

;;;; validate-matrix
;;;; check expected properties
;;;; overlap etc

;;;; mx-manipulations (with magicl)
;;;; select matrix manipulations
;;;; abs diff, diff, masks, id masks &&&

;;;; mx->lstat
;;;; select matrix layers
;;;; convert target matrix layers to lisp-stat

;;;; make-plots
;;;; select plots
;;;; histos, bars, trends &&&

;;;; stat-tests
;;;; select stats calls
;;;; &&&

;;;; compose-report
;;;; save plots to disk
;;;; format stat-test text
;;;; save report to disk
;;;; optional display in buffer
;;;; ==================================== global variables

(defparameter *input-root* #P"/home/holdens/tempdata/predictions1percent/"
              "Pathname to a dir containing tiff files. With naming format:<TRAIT>.tiff OR PREDICTED_<TRAIT>_<objective>_<MODEL>.tiff")

(defparameter *output-root* #P"/home/holdens/tempdata/OPanalysis/")

(defparameter *models* '(
                         "TSAI"
                         "GBM"
                         )
  "List of strings naming models which are to be compared")
(defparameter *traits-reg* '(
                             "HEIGHT-CM"
                             ;; "AREA"
                             ;; "DENSITY"
                             ;; "DIAMETER"
                             ;; "SBLOTCH-LMH"
                             ;; "SBLOTCH-RATING"
                             ;; "STEM-WEIGHT"
                             ;; "WEIGHT"
                             )
  "List of strings of traits for which the prediction objective was 'regression'")
(defparameter *traits-cat* '(
                             "BARLEY-WHEAT"
                             ;; "HULLED"
                             ;; "ROWS"
                             ;; "SBLOTCH-LMH"
                             ;; "SBLOTCH-RATING"
                             )
  "List of strings of traits for which the prediction objective was 'multiclass'")

(defparameter *stats-reg-describe-true*'(
                                         test-histogram
                                         test-mean
                                         ;;&&&
                                         ))
(defparameter *stats-cat-describe-true*'(
                                         test-barchart
                                         ;;&&&
                                         ))
(defparameter *stats-reg-describe-pred*'(
                                         test-histogram
                                         test-mean
                                         ;;&&&
                                         ))
(defparameter *stats-cat-describe-pred*'(
                                         test-barchart
                                         ;;&&&
                                         ))
(defparameter *stats-reg-compare-pred*'(
                                        test-R2
                                        test-residual
                                        ;;&&&
                                        ))
(defparameter *stats-cat-compare-pred* '(
                                         test-F1
                                         test-confusion-matrix
                                         ;;&&&
                                         ))
(defparameter *stats-reg-compare-models* '(
                                           test-anova-reg
                                           ;;&&&
                                           ))
(defparameter *stats-cat-compare-models* '(
                                           test-anova-cat
                                           ;;&&&
                                           ))

;;;; ==================================== Functions

;;;; run-all-reports
;;;; multiplex file-name-parts/stats-calls into experimental runs
;;;; validate all filename file properties
;;;; validate all filename statscalls allowableness
;;;; validate file set geospatial properties match
;;;; call run-report for each

(defun run-all-reports (&key (show nil))
  (-<> (filename-parts show)
    (experiment-dictionary <> show)
    ;; (mapcar validate-files <>)
    ;; (mapcar validate-calls <>)
    ;; (mapcar validate-geospatial <>)
    ;; (mapcar run-report <>)
    ))

(defun filename-parts(&optional (show nil))
  "Create a list of filename components"
  (let ((experiments '()))
    (dolist (trait *traits-reg*)
      ;; build a p list of lists for the experiment
      (let ((experiment (list :trait (list trait)
                              :objective (list "regression")
                              :models *models*)))
        (push experiment experiments)))
    (dolist (trait *traits-cat*)
      ;; build a p list of lists for the experiment
      (let ((experiment (list :trait (list trait)
                              :objective (list "multiclass")
                              :models *models*)))
        (push experiment experiments)))
    ;; clean up and return
    (let ((clean-experiments (remove-duplicates (reverse experiments) :test #'equal)))
      (when show
          (dolist (i clean-experiments)
            (format t "~&~A" i)))
      clean-experiments)))

(defun experiment-dictionary (filename-parts &optional (show nil))
  "completes the dictionary of experiment definitions"
  (when show (print "experiment-dictionary"))
  (let ((experiments '()))
    (dolist (fp filename-parts)
      (let* ((objective (first (getf fp :objective)))
             (models (getf fp :models))
             (to-append (cond ((string= objective "regression")
                               (append *stats-reg-describe-true*
                                       *stats-reg-describe-pred*
                                       *stats-reg-compare-pred*
                                       (when (>= (length models) 2)
                                         *stats-reg-compare-models*)))
                              ((string= objective "multiclass")
                               (append *stats-cat-describe-true*
                                       *stats-cat-describe-pred*
                                       *stats-cat-compare-pred*
                                       (when (>= (length models) 2)
                                         *stats-cat-compare-models*)))
                              (t (error "Unknown objective: ~A~&" objective)))))
        (let ((experiment (copy-list fp)))
          (setf (getf experiment :stats) to-append)
          (push experiment experiments))))
    (setf experiments (nreverse experiments))
    (when show
      (dolist (exp experiments)
        (format t "~&~S~%" exp)))
    experiments))

(defun experiment-dictionary (filename-parts &optional (show nil))
  "Completes the dictionary of experiment definitions"
  (let ((completed-experiments '()))
    (dolist (experiment filename-parts)
      (let* ((objective (first (getf experiment :objective)))
             (models (getf experiment :models))
             (tests (cond
                      ((string= objective "regression")
                       (append *stats-reg-describe-true*
                               *stats-reg-describe-pred*
                               *stats-reg-compare-pred*
                               (when (>= (length models) 2)
                                 *stats-reg-compare-models*)))
                      ((string= objective "multiclass")
                       (append *stats-cat-describe-true*
                               *stats-cat-describe-pred*
                               *stats-cat-compare-pred*
                               (when (>= (length models) 2)
                                 *stats-cat-compare-models*)))
                      (t (error "Unknown objective: ~A" objective))))
             (completed-experiment (copy-list experiment)))
        (setf (getf completed-experiment :tests) tests)
        (push completed-experiment completed-experiments)))

    (setf completed-experiments (nreverse completed-experiments))

    (when show
      (dolist (exp completed-experiments)
        (format t "~&~S~%" exp)))

    completed-experiments))


(run-all-reports)


  ;;with a list whose elements are like: (TRAIT (HEIGHT-CM) OBJECTIVE (regression) MODELS (TSAI))
  ;; add a list :tests
  ;;to which we will add the stats function lists
  ;; if objective is regression add: *stats-reg-describe-true* *stats-reg-describe-pred* *stats-reg-compare-pred*
  ;; if objective is multiclass add: *stats-cat-describe-true* *stats-cat-describe-pred* *stats-cat-compare-pred*
  ;; if length of models is 2 add the correct objective of: *stats-reg-compare-models* *stats-cat-compare-models*

  ;;&&&



(run-all-reports :show t )

;;;; ==================================== FIN
;;;; ==================================== build

#|
                # set both objective and model_type are None to export only trait map
    # exports = [{'trait_name': 'HEIGHT', 'objective': None, 'model_type': None},
    #            {'trait_name': 'HEIGHT', 'objective': 'regression', 'model_type': 'TSAI'},
    #            {'trait_name': 'HEIGHT', 'objective': 'multiclass', 'model_type': 'TSAI'},
    #            {'trait_name': 'HEIGHT', 'objective': 'regression', 'model_type': 'GBM'},
    #            {'trait_name': 'HEIGHT', 'objective': 'multiclass', 'model_type': 'GBM'}]

    models = [
        "TSAI",
        "GBM"]
    # objectives = ["regression", "multiclass"]
    traits_reg = [
        'HEIGHT-CM',
        'AREA',
        'DENSITY',
        'DIAMETER',
        'SBLOTCH-LMH',
        'SBLOTCH-RATING',
        'STEM-WEIGHT',
        'WEIGHT',
    ]
    traits_cat = [
        'BARLEY-WHEAT',
        'HULLED',
        'ROWS',
        'SBLOTCH-LMH',
        'SBLOTCH-RATING',
    ]

    exports = []

    # deduplicate if repeats in reg and cat
    for m in models:
        for t in traits_reg:
            trait = {'trait_name': t, 'objective': None , 'model_type': None }
            prediction =  {'trait_name': t, 'objective': 'regression' , 'model_type': m }
            if trait not in exports:
                exports.append(trait)
            if prediction not in exports:
                exports.append(prediction)
        for t in traits_cat:
            trait = {'trait_name': t, 'objective': None , 'model_type': None }
            prediction = {'trait_name': t, 'objective': 'multiclass', 'model_type': m }
            if trait not in exports:
                exports.append(trait)
            if prediction not in exports:
                exports.append(prediction)

|#




(defparameter *temp-experiment*
  '(
    (:trait "HEIGHT")
    (:objectives () )
    (:models ())



    ))
#|
&&&
I have a python export that uses this list builder, how can I emulate it in common lisp?



To emulate the Python list builder in Common Lisp, you can use a combination of `loop` and `append`. Here's how you can create a similar structure:

```lisp
(defun build-exports ()
  (let ((exports '()))
    (loop for m in *models* do
      (loop for t in *traits-reg* do
        (let ((trait `((:trait-name ,t :objective nil :model-type nil)))
              (prediction `((:trait-name ,t :objective "regression" :model-type ,m))))
          (setf exports (append exports
                                (remove-duplicates
                                 (append trait prediction)
                                 :test #'equal)))))
      (loop for t in *traits-cat* do
        (let ((trait `((:trait-name ,t :objective nil :model-type nil)))
              (prediction `((:trait-name ,t :objective "multiclass" :model-type ,m))))
          (setf exports (append exports
                                (remove-duplicates
                                 (append trait prediction)
                                 :test #'equal))))))
    exports))

(defparameter *exports* (build-exports))
```

This function does the following:

1. It initializes an empty list `exports`.
2. It loops through each model in `*models*`.
3. For each model, it loops through traits in `*traits-reg*` and `*traits-cat*`.
4. For each trait, it creates a trait map and a prediction map.
5. It appends these maps to the `exports` list, removing duplicates.

The resulting `*exports*` will be a list of plists (property lists) that correspond to the dictionaries in the Python version.

You can then access `*exports*` to get the full list of exports.

|#

#|
&&&

|#
                                        ; X
;;;; ==================================== X
;;; ===================================== X
;; ====================================== X

                                        ; set config
;; (py:initialize)
;; (print py4cl2:*config*) ;; WARN: this triggers the company auto complete hang on lab linux
;; py4cl2:*config* ;; WARN: this triggers the company auto complete hang on lab linux

;; (setf (config-var pycmd) python3") ; set one field



                                        ; ensure version and sys.path is same as python in cli
(py:pyversion-info)    ; fails if python command is not resolved in system
;; (py:defpymodule "sys" nil :lisp-package "SYS")
;; (py:defpymodule "pprint" nil :lisp-package "PPRINT")
;; (py:pyexec "pprint.pprint(sys.path)")

                                        ; py process hard reset
;; (py:pystop)
;; (py:python-alive-p)
;; (py:pystart)
                                        ; python imports
(py:defpymodule "rasterio" t :lisp-package "RASTERIO") ; drivers: GTiff GPKG
;; (py:defpymodule "geopandas" nil :lisp-package "GEOPANDAS")
;; (py:defpymodule "sklearn" nil :lisp-package "SKLEARN")

                                        ; open the file
(defparameter *gtif-true* #P"/home/holdens/tempdata/predictions1percent/height.tiff")

;; pythonic: dataset = rasterio.open('some.tif')
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
;; dataset.shape
(py:pyslot-value *dataset* 'shape)
;; dataset-bounds
(py:pyslot-value *dataset* 'bounds)

;;;; method actions
;; get nodata value
(py:pymethod *dataset* 'get_nodatavals) ; works

;; get first band
;; pythonic: dataset.read(1)
(py:pymethod *dataset* "read" 1)
;; pythonic: read1 = dataset.read(1)
(defparameter *read1* (py:pymethod *dataset* "read" 1))
;; close method when done &&&
;; (py:pymethod *dataset* 'close)

;;;; simple array operations
(make-array '(2 2))
(make-array '(2 2) :initial-element nil)
(make-array '(2 2) :initial-contents '((1 2) (3 4)))

(defparameter *test-array* (make-array '(2 2) :initial-element 4))
(defparameter *test-tens* (make-array '(2 2 2) :initial-element 8))
(describe *test-array*)
(type-of *test-array*)

(array-dimensions *test-array*)
(aref *test-array* 0 1)
(aref *test-tens* 0 0 0)
*test-array*

(type-of *read1*) ;; => (SIMPLE-ARRAY SINGLE-FLOAT (1000 26500))
(array-dimensions *read1*) ;; => (1000 26500)
(array-total-size *read1*) ;; => 26500000 (25 bits, #x1945BA0, #o145055640, #b1100101000101101110100000)
(inspect *read1*) ;; works
(describe *read1*) ;; works

;;;; ===================================_X
