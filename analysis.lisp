;;;; ==================================== setup environment

;; get conda env
;; M-x conda-env-activate "analysis"

;; attach repl
;; M-- ,' "qlot-8G-heap"

;;;; ==================================== contents

;; setup
;; global variables

;;;; run-all-reports
;;;; multiplex file-name-parts/stats-calls into experimental run dictionaries
;;;; validate all filename file properties
;;;; add all files to experiments
;;;; validate fileset geospatial properties match
;;;; call coordinate-reports

;;;; coordinate-reports
;;;; call run-single-reports
;;;; call run-meta-reports
;;;; call produce-report-document

;;;; run-single-reports
;;;; call create matrix
;;;; call mx-manipulations
;;;; call mx->lstat
;;;; call make-plots
;;;; call stat-tests
;;;; call compose-report

;;;; run-meta-reports
;;;; &&&

;;;; produce-report-document
;;;; &&&

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
  (:import-from :arrow-macros :-<> :<>)
  (:import-from :serapeum :dict)
  (:import-from :modf :modf)
  )

                                        ; enter package
(in-package :analysis)

;;;; ==================================== package setup

;; set printer to limit depth of large objects
(setf *print-level* 100)
(setf *print-length* 100)

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


;;;; ==================================== global variables

                                        ; file locations
(defparameter *program-root* #P"/home/holdens/tempdata/predictions1percent/"
              "Pathname to a dir containing the dirs specified below" )

(defparameter *tiffs-path* (filepaths:join *program-root* "tiffs")
  "string of the dir in the input-root containing all tiff files. mandatory naming format:<TRAIT>.tiff OR PREDICTED_<TRAIT>_<objective>_<MODEL>.tiff")

(defparameter *gpkgs-path* (filepaths:join *program-root* "gpkgs"))

(defparameter *tables-path* (filepaths:join *program-root* "tables"))

(defparameter *area-geopackage* (filepaths:join *gpkgs-path* "AOI-south.gpkg"))

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

;;;; ==================================== Utilities

(defun gat (obj &rest keys)
  "transparent access to many datatypes"
  (apply #'access:accesses obj keys))

(defun sat (new obj &rest keys)
  "non mutating transparent value setting via access "
  (nth-value 1 (apply #'access:set-accesses new obj keys)))

;;;; ==================================== Functions

(defun run-all-reports (&key (show nil))
  (when show (format t "~&~%In: run-all-reports"))
  (-<>
      (filename-parts show)
    (experiment-dictionary <> show)
    (validate-globals <> show)
    (validate-files <> show)
    (add-files <> show)
    (validate-geospatial <> show)
    (coordinate-reports <> show)
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

(defun open-geopackage (gpkg-path)
  ;; handles close of file
  (pygpd:read-file :filename (namestring gpkg-path)))

(defun open-geotiff (gtif-path)
  ;; needs to be closed with: (py:pymethod python-object 'close)
  (pyrio:open :fp (namestring gtif-path)))

(defun partition-by-2 (list)
  "returns a list with neighbours made into sublists (a b 1 2)=>((a b) (b 1) (1 2))"
  (labels (
           (first-2 (list)
             (let* (
                    (a (nth 0 list))
                    (b (nth 1 list))
                    (paired (when (and a b) (list a b)))
                    )
               paired))
           )
    (let* (
           (trailing-nil (maplist #'first-2 list))
           (cleaned (butlast trailing-nil))
           )
      cleaned)))

(defun validate-geospatial (experiments &optional show)
  "Check that the geospatial properties of the files in an experiment are matched"
  (when show (format t "~&~%In: validate-geospatial"))

  ;;; &&& check crs, nodata, bounds within allowable
  (let (
        (allowable-bounds-diff 1)
        )
    (labels (
             (check-experiment (experiment)
               (let* (
                      ;; get paths from dict
                      (gpkg-path (acc:accesses experiment :files :gpkg))
                      (true-path (acc:accesses experiment :files :true))
                      (pred-0-path (acc:accesses experiment :files :pred-0))
                      (pred-1-path (acc:accesses experiment :files :pred-1))
                      ;; get py objects from paths
                      (gpkg-obj (open-geopackage gpkg-path))
                      (true-obj (open-geotiff true-path))
                      (pred-0-obj (open-geotiff pred-0-path))
                      (pred-1-obj (open-geotiff pred-1-path))
                      ;; get details
                      ;; gpkg crs left bottom right top ;; gpkg:gtif 0:left 1:bottom 2:right 3:top
                      (gpkg-crs (py:pyslot-value gpkg-obj 'crs))
                      (gpkg-left (aref (py:pyslot-value gpkg-obj "total_bounds") 0))
                      (gpkg-bottom (aref (py:pyslot-value gpkg-obj "total_bounds") 1))
                      (gpkg-right (aref (py:pyslot-value gpkg-obj "total_bounds") 2))
                      (gpkg-top (aref (py:pyslot-value gpkg-obj "total_bounds") 3))
                      ;; true crs nodatavalue left bottom right top
                      (true-crs (py:pyslot-value true-obj 'crs))
                      (true-ndv (py:pyslot-value true-obj 'nodata))
                      (true-left (py:pyslot-value true-obj 'bounds.left))
                      (true-bottom (py:pyslot-value true-obj 'bounds.bottom))
                      (true-right (py:pyslot-value true-obj 'bounds.right))
                      (true-top (py:pyslot-value true-obj 'bounds.top))
                      ;; pred-0 crs nodatavalue left bottom right top
                      (pred-0-crs (py:pyslot-value pred-0-obj 'crs))
                      (pred-0-ndv (py:pyslot-value pred-0-obj 'nodata))
                      (pred-0-left (py:pyslot-value pred-0-obj 'bounds.left))
                      (pred-0-bottom (py:pyslot-value pred-0-obj 'bounds.bottom))
                      (pred-0-right (py:pyslot-value pred-0-obj 'bounds.right))
                      (pred-0-top (py:pyslot-value pred-0-obj 'bounds.top))
                      ;; pred-1 crs nodatavalue left bottom right top
                      (pred-1-crs (py:pyslot-value pred-1-obj 'crs))
                      (pred-1-ndv (py:pyslot-value pred-1-obj 'nodata))
                      (pred-1-left (py:pyslot-value pred-1-obj 'bounds.left))
                      (pred-1-bottom (py:pyslot-value pred-1-obj 'bounds.bottom))
                      (pred-1-right (py:pyslot-value pred-1-obj 'bounds.right))
                      (pred-1-top (py:pyslot-value pred-1-obj 'bounds.top))
                      ;; aggregate check lists
                      (crss (list gpkg-crs true-crs pred-0-crs pred-1-crs))
                      (lefts (list gpkg-left true-left pred-0-left pred-1-left))
                      (bottoms (list gpkg-bottom true-bottom pred-0-bottom pred-1-bottom))
                      (rights (list gpkg-right true-right pred-0-right pred-1-right))
                      (tops (list gpkg-top true-top pred-0-top pred-1-top))
                      (ndvs (list true-ndv pred-0-ndv pred-1-ndv))
                      ;; run checks T passing
                      (checked-crss (check-crss crss))
                      (checked-lefts (check-bounds lefts))
                      (checked-bottoms (check-bounds bottoms))
                      (checked-rights (check-bounds rights))
                      (checked-tops (check-bounds tops))
                      (checked-ndvs (check-ndvs ndvs))
                      ;; aggregate test status
                      (all-tests (append checked-crss checked-lefts checked-bottoms checked-rights checked-tops checked-ndvs))
                      (all-passing (every #'(lambda (a) (eq a T)) all-tests))
                      ;; close geotiff objects
                      (true-closed (py:pymethod true-obj 'close))
                      (pred-0-closed (py:pymethod pred-0-obj 'close))
                      (pred-1-closed (py:pymethod pred-1-obj 'close))
                      )
                 (unless all-passing
                   (warn "~&~%Potential geospatial mismatch in experiment")
                   ;; (format t "~& some identifying feature")
                   (format t "~&crs: ~A" crss)
                   (format t "~&left: ~A" lefts)
                   (format t "~&bottom: ~A" bottoms)
                   (format t "~&right: ~A" rights)
                   (format t "~&tops: ~A" tops)
                   (format t "~&nodata: ~A" ndvs))
                 all-passing
                 ))
             (check-crss (crss)
               (mapcar #'check-crs (partition-by-2 crss)))
             (check-crs (pair)
               (py:pyeval (nth 0 pair) "==" (nth 1 pair)))
             ;;
             (check-bounds (bounds) ; left bottom right top
               (mapcar #'check-bound (partition-by-2 bounds)))
             (check-bound (pair)
               (<
                (abs (- (nth 0 pair) (nth 1 pair)))
                allowable-bounds-diff))
             ;;
             (check-ndvs (ndvs)
               (mapcar #'check-ndv (partition-by-2 ndvs)))
             (check-ndv (pair)
               (= (nth 0 pair) (nth 1 pair)))
             )
      (let* (
             (checked (mapcar #'check-experiment experiments))
             )
        (when show (format t "~%Experiments passing geospatial validation: ~A" checked))
        experiments ;; pass dictionary on unchanged
        ))))

(defun coordinate-reports (experiments &optional show)
  "&&& incomplete. Calls phases of report making"
  (when show (format t "~&~% In: coordinate reports"))
  (-<>
      (run-single-reports experiments show)
    ;; (run-meta-reports <> show)
    ;; (produce-report-document <> show)
    ;; &&& maybe emit something to be returned at the end
    ))

;;;; ==================================== API

(run-all-reports :show t)

;;;; ==================================== FIN

;;;; ==================================== build

(defun run-single-reports (experiments &optional show)
  "Adds the single report stats to each experiment dictionary"
  (when show (format t "~&~%In: run single reports"))
  )

;;;; ==================================== scratch

(defparameter *test-experiments* '((:FILES (:TRUE #P"/home/holdens/tempdata/predictions1percent/tiffs/HEIGHT-CM.tiff" :PRED-0 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_HEIGHT-CM_regression_GBM.tiff" :PRED-1 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_HEIGHT-CM_regression_TSAI.tiff" :GPKG #P"/home/holdens/tempdata/predictions1percent/gpkgs/AOI-south.gpkg" :TABLE #P"/home/holdens/tempdata/predictions1percent/tables/temp-table.csv") :TRAIT ("HEIGHT-CM") :OBJECTIVE ("regression") :MODELS ("GBM" "TSAI") :TESTS (STAT-REG-DESCRIBE-TRUE-HISTO STAT-REG-DESCRIBE-TRUE-MEAN STAT-REG-DESCRIBE-PRED-HISTO STAT-REG-DESCRIBE-PRED-MEAN STAT-REG-COMPARE-PRED-R2 STAT-REG-COMPARE-PRED-RESIDUAL STAT-REG-COMPARE-MODELS-ANOVA)) (:FILES (:TRUE #P"/home/holdens/tempdata/predictions1percent/tiffs/BARLEY-WHEAT.tiff" :PRED-0 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_BARLEY-WHEAT_multiclass_GBM.tiff" :PRED-1 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_BARLEY-WHEAT_multiclass_TSAI.tiff" :GPKG #P"/home/holdens/tempdata/predictions1percent/gpkgs/AOI-south.gpkg" :TABLE #P"/home/holdens/tempdata/predictions1percent/tables/temp-table.csv") :TRAIT ("BARLEY-WHEAT") :OBJECTIVE ("multiclass") :MODELS ("GBM" "TSAI") :TESTS (STAT-CAT-DESCRIBE-TRUE-BARCHART STAT-CAT-DESCRIBE-PRED-BARCHART STAT-CAT-COMPARE-PRED-F1 STAT-CAT-COMPARE-PRED-CONFUSIONMX STAT-CAT-COMPARE-MODELS-ANOVA))))

(defparameter *test-experiment* '(:FILES (:TRUE #P"/home/holdens/tempdata/predictions1percent/tiffs/HEIGHT-CM.tiff" :PRED-0 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_HEIGHT-CM_regression_GBM.tiff" :PRED-1 #P"/home/holdens/tempdata/predictions1percent/tiffs/PREDICTED_HEIGHT-CM_regression_TSAI.tiff" :GPKG #P"/home/holdens/tempdata/predictions1percent/gpkgs/AOI-south.gpkg" :TABLE #P"/home/holdens/tempdata/predictions1percent/tables/temp-table.csv") :TRAIT ("HEIGHT-CM") :OBJECTIVE ("regression") :MODELS ("GBM" "TSAI") :TESTS (STAT-REG-DESCRIBE-TRUE-HISTO STAT-REG-DESCRIBE-TRUE-MEAN STAT-REG-DESCRIBE-PRED-HISTO STAT-REG-DESCRIBE-PRED-MEAN STAT-REG-COMPARE-PRED-R2 STAT-REG-COMPARE-PRED-RESIDUAL STAT-REG-COMPARE-MODELS-ANOVA)))

;;;; ==================================== reference

;; numcl has mean
(numcl:mean)
numcl has standard-deviation
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

;;;; geopandas open gpkg
(defparameter *geopackage* #P"/home/holdens/tempdata/predictions1percent/gpkgs/AOI-south.gpkg")
(defparameter *dataset-gpkg* (pygpd:read-file :filename (namestring *geopackage*))) ; read_file handles close
(print *dataset-gpkg*)
(py:pyslot-list *dataset-gpkg*)
(py:pymethod-list *dataset-gpkg*)

(py:pyslot-value *dataset-gpkg* "total_bounds")
(py:pyslot-value *dataset-gpkg* "crs")

;;;; rasterio open tiff
(defparameter *gtif-true* #P"/home/holdens/tempdata/predictions1percent/tiffs/HEIGHT-CM.tiff")
;; pythonic: dataset = rasterio.open('some.tif')
(defparameter *dataset-gtif* (pyrio:open :fp (namestring *gtif-true*))) ; case sensitive!
(print *dataset-gtif*)
(py:pyslot-list *dataset-gtif* ) ; ie. show me what data it has
(py:pymethod-list *dataset-gtif*) ; ie show me what methods it has

                                        ; operations on geotiff
;;;; slot actions
;; dataset.bounds
(py:pyslot-value *dataset-gtif* 'bounds)
(py:pyslot-value *dataset-gtif* 'bounds.left)
;; dataset.crs
(py:pyslot-value *dataset-gtif* 'crs)
;; dataset.dtypes
(py:pyslot-value *dataset-gtif* 'dtypes)
;; dataset.nodata
(py:pyslot-value *dataset*-gtif 'nodata)
;; dataset.shape
(py:pyslot-value *dataset-gtif* 'shape)

;;;; method actions
(py:pymethod *dataset-gtif* 'get_nodatavals)
(py:pymethod *dataset-gtif* 'read-crs)
(py:pymethod *dataset-gtif* 'read-transform)

;; get first band
;; pythonic: dataset.read(1)
(py:pymethod *dataset-gtif* "read" 1)
;; pythonic: read1 = dataset.read(1)
(defparameter *read1* (py:pymethod *dataset-gtif* "read" 1)) ;; => (SIMPLE-ARRAY SINGLE-FLOAT (1000 26500))
;; close method when done &&&
(py:pymethod *dataset-gtif* 'close)

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

;;;; rcl load

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

(defparameter *test* '(:a 1 :b 2 :c (:A one)))
(serapeum:plist-keys *test*)
(serapeum:plist-values *test*)

(sat "new" *test* :c)
(sat "new" *test* :c :a)
(sat "new" *test* :c :b) ; create new b in list
(sat "new" *test* :c :b '(:new :type :plist)) ; create new nesting

(gat *test* :c)
(gat *test* :c :a)
(gat *test* :c :a)
(gat *test* :c :b) ; nil when nothing there


#|
&&&
|#
                                        ; X
;;;; ==================================== X
;;; ===================================== X
;; ====================================== X
