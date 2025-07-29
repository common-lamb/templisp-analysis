;;;; ==================================== setup environment

;; get conda env
;; M-x conda-env-activate "analysis"

;; attach repl
;; M-- ,' "qlot-8G-heap"

;;;; ==================================== create package

                                        ; imports
(ql:quickload :qlot)
(ql:quickload :fset)
(ql:quickload :misc-extensions)
(ql:quickload :alexandria)
(ql:quickload :serapeum)
(ql:quickload :modf)
(ql:quickload :access)
(ql:quickload :arrow-macros)
(ql:quickload :lparallel)
(ql:quickload :lisp-stat)
(ql:quickload :plot/vega)
(ql:quickload :numcl)
(ql:quickload :cl-csv)
(ql:quickload :str)
(ql:quickload :filepaths); ultralisp foskers-filepaths
(ql:quickload :filesystem-utils)
(ql:quickload :py4cl2)
(ql:quickload :rcl)

(defpackage :analysis
  (:use :cl)
  (:local-nicknames (:py :py4cl2 ))
  (:local-nicknames (:r :rcl))
  (:local-nicknames (:csv :cl-csv))
  (:local-nicknames (:acc :access))
  (:import-from :arrow-macros :-<> :<>))

                                        ; enter package
(in-package :analysis)

;;;; ==================================== package setup

;; set printer to limit depth of large objects
(setf *print-level* 100)
(setf *print-length* 50)

;;;; ==================================== python interop setup
                                        ; set config
;; (py:initialize)
;; (print py4cl2:*config*) ;; WARN: this triggers the company auto complete hang on lab linux
;; py4cl2:*config* ;; WARN: this triggers the company auto complete hang on lab linux
;; (setf (config-var pycmd) python3) ; set one field

                                        ; ensure version and sys.path is same as python in cli
(py:pyversion-info)    ; fails if python command is not resolved in system
;; (py:defpymodule "sys" nil :lisp-package "SYS")
;; (py:defpymodule "pprint" nil :lisp-package "PPRINT")
;; (py:pyexec "pprint.pprint(sys.path)") ; check python path

                                        ; py process hard reset
;; (py:pystop)
;; (py:python-alive-p)
;; (py:pystart)
                                        ; python imports
(py:defpymodule "rasterio" t :lisp-package "PYRIO") ; drivers: GTiff GPKG
(py:defpymodule "geopandas" t :lisp-package "PYGPD")
(py:defpymodule "sklearn" t :lisp-package "PYSKL")
(py:defpymodule "matplotlib.pyplot" nil :lisp-package "PYPLT")
(py:defpymodule "scikitplot" t :lisp-package "PYSKP")
(py:defpymodule "scipy" t :lisp-package "PYSCP")
(py:defpymodule "statsmodels.api" nil :lisp-package "PYSMS")
(py:defpymodule "statsmodels.stats" t :lisp-package "PYSMS")
(py:defpymodule "statsmodels.sandbox" t :lisp-package "PYSMSB")
(py:defpymodule "pingouin" t :lisp-package "PYPIN")

;;;; ==================================== contents

;; global variables

;;;; run-all-reports
;;;; multiplex file-name-parts/stats-calls into experimental run dictionaries
;;;; validate all filename file properties
;;;; add all files to experiments
;;;; validate all filename statscalls allowableness
;;;; validate file set geospatial properties match
;;;; call run-report for each experiment

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

                                        ; file locations
(defparameter *program-root* #P"/home/holdens/tempdata/predictions1percent/"
              "Pathname to a dir containing the dirs specified below" )

(defparameter *tiffs-path* (filepaths:join *program-root* "tiffs")
  "string of the dir in the input-root containing all tiff files. mandatory naming format:<TRAIT>.tiff OR PREDICTED_<TRAIT>_<objective>_<MODEL>.tiff")

(defparameter *gpkgs-path* (filepaths:join *program-root* "gpkgs"))

(defparameter *tables-path* (filepaths:join *program-root* "tables"))

(defparameter *area-geopackage* (filepaths:join *gpkgs-path* "temp-area.gpkg"))

(defparameter *identities-csv* (filepaths:join *tables-path* "temp-table.csv"))
;; &&& other input files and subdirs

(defparameter *geotiff-extension* "tiff"
  "file extension used for the geotiff files. one of \"tif\" or \"tiff\" (no dot) ")

(defparameter *output-root* (filepaths:join *program-root*  "output"))
;; &&& output subdirs

                                        ; input tif filename components
(defparameter *models* '(
                         "GBM"
                         "TSAI"
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



                                        ; categorized statistics functions
(defparameter *stats-reg-describe-true*'(
                                         stat-reg-describe-true-histo
                                         stat-reg-describe-true-mean
                                         ;; stat-reg-describe-true- &&&
                                         ))
(defparameter *stats-cat-describe-true*'(
                                         stat-cat-describe-true-barchart
                                         ;; stat-cat-describe-true- &&&
                                         ))
(defparameter *stats-reg-describe-pred*'(
                                         stat-reg-describe-pred-histo
                                         stat-reg-describe-pred-mean
                                         ;; stat-reg-describe-pred- &&&
                                         ))
(defparameter *stats-cat-describe-pred*'(
                                         stat-cat-describe-pred-barchart
                                         ;; stat-cat-describe-pred- &&&
                                         ))
(defparameter *stats-reg-compare-pred*'(
                                        stat-reg-compare-pred-R2
                                        stat-reg-compare-pred-residual
                                        ;; stat-reg-compare-pred- &&&
                                        ))
(defparameter *stats-cat-compare-pred* '(
                                         stat-cat-compare-pred-F1
                                         stat-cat-compare-pred-confusionMX
                                         ;; stat-cat-compare-pred- &&&
                                         ))
(defparameter *stats-reg-compare-models* '(
                                           stat-reg-compare-models-anova
                                           ;; stat-reg-compare-models- &&&
                                           ))
(defparameter *stats-cat-compare-models* '(
                                           stat-cat-compare-models-anova
                                           ;; stat-cat-compare-models- &&&
                                           ))

;;;; ==================================== Functions

(defun run-all-reports (&key (show nil))
  (when show (format t "~&~%In: run-all-reports"))
  (-<>
      (filename-parts show)
    (experiment-dictionary <> show)
    (validate-globals <> show)
    (validate-files <> show)
    (add-files <> show)
    ;; (validate-calls <> show)
    ;; (validate-geospatial <> show)
    ;; (run-report <> show)
    ))

(defun filename-parts (&optional (show nil))
  "Create a list of filename components"
  (when show
    (format t "~&~%In: filename-parts"))
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
          (format t "~&~S" i)))
      clean-experiments)))

(defun experiment-dictionary (filename-parts &optional (show nil))
  "Completes the dictionary of experiment definitions"
  (when show
    (format t "~&~%In: experiment-dictionary"))
  (let ((completed-experiments '()))
    (dolist (experiment filename-parts)
      (let* ((objective (first (getf experiment :objective)))
             (models (getf experiment :models))
             (selected-tests (cond
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
             (experiment-plist (copy-list experiment))
             (test-plist (list :tests selected-tests)))
        (push (append experiment-plist test-plist) completed-experiments)))
    (setf completed-experiments (nreverse completed-experiments))
    (when show
      (dolist (i completed-experiments)
        (format t "~&~S" i)))
    completed-experiments))

(defun validate-globals (experiments &optional (show nil))
  "checks global variables"
  (if show
      (format t "~&~%In: validate-globals"))
  ;; check models
  (let ((len-mod (length *models*)))
    (assert (= 2 len-mod)
        ()
        "The number of models in *models* must be 2. Found: ~A" len-mod))
  ;; check traits
  (let ((len-traits (+ (length *traits-cat*)
                       (length *traits-reg*))))
    (assert (<= 1 len-traits) () "The number of traits (in either *traits-reg* *traits-cat*) must be at least 1"))
  (let ((stats-all (append *stats-reg-describe-true* *stats-cat-describe-true* *stats-reg-describe-pred* *stats-cat-describe-pred* *stats-reg-compare-pred* *stats-cat-compare-pred* *stats-reg-compare-models* *stats-cat-compare-models*))))

  ;; check that the stats functions in all *stats-...* lists exist
  (let ((stats-all (append *stats-reg-describe-true*
                           *stats-cat-describe-true*
                           *stats-reg-describe-pred*
                           *stats-cat-describe-pred*
                           *stats-reg-compare-pred*
                           *stats-cat-compare-pred*
                           *stats-reg-compare-models*
                           *stats-cat-compare-models*)))
    (dolist (i stats-all)
      (when (not (fboundp i))
        (warn "The function ~A must exist" i))))
  ;; if all pass, then return  unmodified input
  experiments)

(defun create-filenames (experiment &optional (show nil))
  "taking an experiment dictionary return a dictionary of the expected files"
  (when show (format t "~&~%In: create-filenames"))
  (let* ((trait (first (acc:access experiment :trait)))
         (objective (first (acc:access experiment :objective)))
         (model0 (nth 0 (acc:access experiment :models)))
         (model1 (nth 1 (acc:access experiment :models)))
         (string-pred-0 (concatenate 'string "PREDICTED" "_" trait "_" objective "_" model0))
         (string-pred-1 (concatenate 'string "PREDICTED" "_" trait "_" objective "_" model1))

         ;; ground truth file
         (trait-tiff (filepaths:join *tiffs-path*
                                     (filepaths:with-extension trait *geotiff-extension*)))
         ;; prediction file
         (pred-0-tiff (filepaths:join *tiffs-path*
                                      (filepaths:with-extension string-pred-0 *geotiff-extension*)))
         ;; prediction file
         (pred-1-tiff (filepaths:join *tiffs-path*
                                     (filepaths:with-extension string-pred-1 *geotiff-extension*)))
         ;; geopackage file
         (area-gpkg *area-geopackage* )

         ;; csv file
         (table-csv *identities-csv* )

         (return-dictionary `(:true ,trait-tiff
                              :pred-0 ,pred-0-tiff
                              :pred-1 ,pred-1-tiff
                              :gpkg ,area-gpkg
                              :table ,table-csv)))
    (when show (format t "~&returning files: ~S" return-dictionary))
    return-dictionary))

(defun validate-files (experiments &optional show)
  "for all experiment dictionaries in experiments, ensure the files exist"
  (when show (format t "~&~%In: validate-files"))
  (labels (
           (validate-experiment (experiment)
             (-<>
                 (create-filenames experiment show)
               (which-files-non-exist <>)))

           (which-files-non-exist (files-dict)
             ;; (and (pathnamep i) (null (probe-file i)))
             (mapcar #'(lambda (i) (when (and (pathnamep i)
                                              (not (org.shirakumo.filesystem-utils:file-exists-p i)))
                                     i))
                     files-dict))

           (error-when-not-found (nils-or-files)
             (if (every #'null nils-or-files)
                 nil
                 (progn
                   (format t "~&~%Expected files not found: ~S" nils-or-files)
                   (remove-if #'null nils-or-files))))
           )
    (let* (
           (completed-checks (mapcar #'validate-experiment experiments))
           (reporting (mapcar #'error-when-not-found completed-checks))
           )
      ;; if everything exists, pass on experiment dictionary unchanged
      (if (every #'null reporting)
          experiments
          (error "~&~%These files were not found ~S" reporting)))))

(defun add-files (experiments &optional show)
  "adds files to each experiment in the experiment dictionary"
  (when show (format t "~&~%In: add-files"))
  (labels (
           (add-files-to-dict (experiment)
             (modf:modf (getf experiment :files) (create-filenames experiment)))
           )
    (let* (
           ;; (copied-experiments (mapcar #'(lambda (experiment) (copy-list experiment)) experiments))
           (with-files (mapcar #'add-files-to-dict experiments))
           )
      (when show (format t "~&returning dictionary: ~S" with-files))
      with-files)))
;;;; ==================================== API

(run-all-reports :show t)

;;;; ==================================== FIN

;;;; ==================================== build

;;;; run-all-reports
;;;; multiplex file-name-parts to dictionary
;;;; stats-calls into experimental run dictionary
;;;; validate all global settings
;;;; validate all filename file properties
;;;; add files to the experiment dictionary
;;;; &&& validate all filename statscalls existance/allowableness
;;;; &&& validate file set geospatial properties match
;;;; &&& call run-report for each

;;;; ==================================== scratch


;;;; ==================================== reference

;; numcl has mean
(numcl:mean)
;; numcl has standard-deviation
(numcl:standard-deviation)
;; lisp-stat has 5 num sum in summarize column
(lisp-stat:summarize-column)
;; lisp-stat has histogram in mark bar
;; lisp-stat has freq,%freq in tabulate
(lisp-stat:tabulate)
;; lisp-stat has barchart in mark bar
;; skl has %correct
(pyskl.metrics:accuracy-score)
;; skl has cohens kappa
(pyskl.metrics:cohen-kappa-score)
;; skl has jaccard
(pyskl.metrics:jaccard-score)
;; lisp-stat has scatter
;; skl has M2error, rootM2error, MAE,R2
(pyskl.metrics:mean-squared-error)
(pyskl.metrics:root-mean-squared-error)
(pyskl.metrics:mean-absolute-error)
(pyskl.metrics:r-2-score)
;; skl has F1 recall precision accuracy
(pyskl.metrics:f-1-score)
(pyskl.metrics:recall-score)
(pyskl.metrics:precision-score)
(pyskl.metrics:accuracy-score)
;; skp has confusion matrices
(pyskp.metrics:confusion-matrix)
;; pingouin has shapiro wilk test
(pypin:normality)
;; pingouin has paired t test
(pypin:ttest)
;; pingouin has wilcoxon test
(pypin:wilcoxon)
;; pingouin has mcnemars
(pypin:chi-2-mcnemar)
;; pingouin has levenes test
(pypin:homoscedasticity)
;; pingouin has 1 way anova
(pypin:anova)
;; pingouin has kruskal wallis test
(pypin:kruskal)
;; pingouin has tukey_hsd test
(pypin:pairwise-tukey)
;; pingouin has chi squared test
(pypin:chi-2-independence)
;; pingouin has 2 way anova
(pypin:mixed-anova)
;; stats models has bowker test
(pysmsb.stats.runs:symmetry-bowker)
;; pingouin has false discovery rate adjustment
(pypin:multicomp)
;; pingouin has effectsizes
(pypin:compute-effsize)

(let ((test-arg
        '(:a (:1 (:A "haha")))))
  (acc:accesses test-arg :a :1 :A))

(labels (
         (local-fun (arg)
           (print "in local fun")
           (format t "~&arg: ~A" arg))
         )
  (local-fun "haha"))

#|
&&&
|#
                                        ; X
;;;; ==================================== X
;;; ===================================== X
;; ====================================== X

(when (= 0 1)
  (print  "Begin never execute")


  ;;;; py4cl setup

                                        ; set config
  ;; (py:initialize)
  ;; (print py4cl2:*config*) ;; WARN: this triggers the company auto complete hang on lab linux
  ;; py4cl2:*config* ;; WARN: this triggers the company auto complete hang on lab linux

  ;; (setf (config-var pycmd) python3) ; set one field



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
  (py:defpymodule "rasterio" t :lisp-package "PYRIO") ; drivers: GTiff GPKG
  (py:defpymodule "geopandas" nil :lisp-package "PYGPD")
  (py:defpymodule "sklearn" nil :lisp-package "PYSKL")
  (py:defpymodule "scipy" nil :lisp-package "PYSCP")
  (py:defpymodule "statsmodels.api" nil :lisp-package "PYSMS")
  (py:defpymodule "" nil :lisp-package "PY")

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

;;;;;;;; rcl load

  (rcl:r-init)
  (rcl:r "R.Version")

  ;; cant do this
  ;; (rcl:r-quit)
  ;; (rcl:r-init)

  ;; (rcl:r "install.packages" "ggplot2")
  (rcl:r "library" "ggplot2")
  ;;visualizing categorical data
  ;; (r:r "install.packages" "vcd")
  (r:r "library" "vcd")
  ;; functions for medical statistics book
  ;; (r:r "install.packages" "fmsb")
  (r:r "library" "fmsb") ; for Kappa.test

  (rcl:r% "summary" '(1 2 3 4 5 6 7 8 9)) ; pointer
  (rcl:r "summary" '(1 2 3 4 5 6 7 8 9)) ; alist
  (rcl:r "print" (rcl:r% "summary" '(1 2 3 4 5 6 7 8 9))) ; print
  (rcl:r% "print" (rcl:r% "summary" '(1 2 3 4 5 6 7 8 9)))

  (print  "End never execute"))
