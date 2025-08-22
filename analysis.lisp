;;;; ==================================== setup environment

;; get conda env
;; M-x conda-env-activate "analysis"

;; attach repl
;; M-- ,' "qlot-8G-heap"

;;;; ==================================== contents

;; setup
;; global variables

;;;; run-all-reports
;;;; demultiplex file-name-parts/stats-calls into experimental run dictionaries
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
;;;; open gpkg file wh geopandas
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

;; check this rather important assumption
#+X(error () "*features* should not contain 'X' or #+X(MASKED-FORM) will not mask the forms! ")
;; set printer to limit depth of large objects
(setf *print-level* 100)
(setf *print-length* 100)
;; set worker threads
(setf lparallel:*kernel* (lparallel:make-kernel 16))

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
(py:defpymodule "pandas" t :lisp-package "PYPND")
(py:defpymodule "numpy" t :lisp-package "PYNPY")

;;;; ==================================== global variables

                                        ; file locations
(defparameter *program-root* #P"/home/holdens/tempdata/predictions1percent/"
              "Pathname to a dir containing the dirs specified below" )

(defparameter *input-path* (filepaths:parent (filepaths:join *program-root* "input" "X")))
;; &&& ensure input exists

(defparameter *tiffs-path* (filepaths:parent (filepaths:join *input-path* "tiffs" "X"))
  "string of the dir in the input-root containing all tiff files. mandatory naming format:<TRAIT>.tiff OR PREDICTED_<TRAIT>_<objective>_<MODEL>.tiff")

(defparameter *gpkgs-path* (filepaths:parent (filepaths:join *input-path* "gpkgs" "X")))

(defparameter *tables-path* (filepaths:parent (filepaths:join *input-path* "tables" "X")))

(defparameter *area-geopackage* (filepaths:join *gpkgs-path* "AOI-south.gpkg"))

(defparameter *identities-csv* (filepaths:join *tables-path* "temp-table.csv"))
;; &&& other input files and subdirs

(defparameter *geotiff-extension* "tiff"
  "file extension used for the geotiff files. one of \"tif\" or \"tiff\" (no dot) ")

(defparameter *output-root* (filepaths:parent (filepaths:join *program-root*  "output" "X")))

(defparameter *plots-path*  (filepaths:parent (filepaths:join *output-root* "plots" "X")))

(defparameter *reports-path* (filepaths:parent (filepaths:join *output-root* "reports" "X")))

(defparameter *report* (filepaths:join *reports-path* "report.txt"))

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
;; tests within a single experiment
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
;; meta tests across experiments
(defparameter *meta-stats-reg-compare-models* '(
                                                meta-stat-reg-compare-models-anova
                                                ;; stat-reg-compare-models- &&&
                                                ))
(defparameter *meta-stats-cat-compare-models* '(
                                                meta-stat-cat-compare-models-anova
                                                ;; meta-stat-cat-compare-models- &&&
                                                ))

;;;; ==================================== Utilities

;; AT
;; library for transparent operations on multi datastructures
;; site referencing works like access:accesses
;; immutability works like modf:modf
;; gat get (gets all elements at key)
;; sat set (sets all elements at key)
;; cat? check (checks for all elements existance)
;; hat? has (checks for single element existance)
;; pat push (adds single element at key)
;; rat remove (removes single element at key)
;; mat make (tests for non preexistance then runs sat)

(defun gat (obj &rest keys)
  "get at (gets all elements at key)"
  (apply #'access:accesses obj keys))

(defun sat (new obj &rest keys)
  "set at (sets all elements at key)"
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
    ;; (coordinate-reports <> show)
    ))

(defun initialize-outputs ()
  "ensure empty output dirs and files"
  (let (
        ;; must pad with x then ascend to get trailing /
        (dop (filepaths:parent (filepaths:join *output-root* "x")))
        (dplt (filepaths:parent (filepaths:join *plots-path* "x")))
        (drpt (filepaths:parent (filepaths:join *reports-path* "x")))
        (frpt *report*)
        )
    ;; op must exist
    (ensure-directories-exist dop)
    ;; plots exists and is empty
    (uiop:delete-directory-tree dplt :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist dplt)
    ;; reports path and file exists
    (ensure-directories-exist drpt)
    ;; report file is empty
    (with-open-file (stream frpt :direction :output
                                 :if-exists :supersede))
    )

  (print "Initialized output directories and report file"))

(defun filename-parts (&optional (show nil))
  "Create a list of filename components"
  (when show
    (format t "~&~%In: filename-parts"))
  (initialize-outputs)
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
                                        *stats-reg-compare-pred*))
                               ((string= objective "multiclass")
                                (append *stats-cat-describe-true*
                                        *stats-cat-describe-pred*
                                        *stats-cat-compare-pred*))
                               ;; &&& any more to add
                               (t (error "Unknown objective: ~A" objective))
                               ))
             (selected-meta-tests (cond
                                    ((string= objective "regression")
                                     (append *meta-stats-reg-compare-models*))
                                    ((string= objective "multiclass")
                                     (append *meta-stats-cat-compare-models*))
                                    ;; &&& any more to add
                                    (t (error "Unknown objective: ~A" objective))
                                    ))
             (experiment-plist (copy-list experiment))
             (test-plist (list :tests selected-tests
                               :meta-tests selected-meta-tests)))
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

  ;; check that the stats functions in all *stats-...* lists exist
  (let ((stats-all (append
                    *stats-reg-describe-true*
                    *stats-cat-describe-true*
                    *stats-reg-describe-pred*
                    *stats-cat-describe-pred*
                    *stats-reg-compare-pred*
                    *stats-cat-compare-pred*
                    *meta-stats-reg-compare-models*
                    *meta-stats-cat-compare-models*
                    ;; &&& any more to add
                    )))
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
         (return-dictionary (list
                             :true trait-tiff
                             :pred-0 pred-0-tiff
                             :pred-1 pred-1-tiff
                             :gpkg area-gpkg
                             :table table-csv)))
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
      (when show (format t "~&Returning: ~S" with-files))
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

  (let ((allowable-bounds-diff 1))
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
  "&&& Calls phases of report making"
  (when show (format t "~&~% In: coordinate reports."))
  (-<>
      (run-single-reports experiments show)
    ;; (run-meta-reports <> show)
    ;; (produce-report-document <> show)
    ;; &&& maybe emit something to be returned to call-all-reports at the end
    ))

(defun compose-name (experiment &key specifier path filetype)
  "Builds up a namestring returing string if just experiment and specifier supplied, pathname if path or filetype supplied"
  (assert (gat experiment :selected-test)() "The experiment dictionary must have a selected test")
  (assert (gat experiment :selected-model)() "The experiment dictionary must have a selected model")
  (let* (
         ;; pull apart experiment dictionary
         (model (gat experiment :selected-model))
         (trait (first (gat experiment :trait)))
         (objective (first (gat experiment :objective)))
         (test (gat experiment :selected-test))
         ;; assemble name base
         (base-name (format nil "~A_~A_~A_~A" model trait objective test))
         ;; conditionally postfix specifier
         (specifier-added (when specifier
                            (format nil "~A_~A" base-name specifier)))
         ;; conditionally postfix filetype
         (type-added (when filetype
                       (make-pathname :type filetype
                                      :name (or specifier-added
                                                base-name))))
         ;; conditionaly prefix path
         (path-added (when path
                       (make-pathname :directory (pathname-directory path)
                                      :name (or specifier-added
                                                base-name)
                                      :defaults type-added)))
         ;; responsively select return type by taking first non nil
         (return-selection (or path-added
                               type-added
                               specifier-added
                               base-name))
         )
    return-selection
    ))

(defun call-funs-on-args (funs args experiment)
  " Funcall all the funs on an args dictionary
args: a plist of args which will be passed into each of the called funs
funs: a list of symbols which are callable functions, each of which takes the args plist and returns a results plist"
  (mapcar (lambda (func)
            (funcall func args (sat func experiment :selected-test)))
          funs))

(defun interleave (l1 l2)
  "alternates items of l1 and l2, stopping with the shortest"
  (apply #'append ;; this "lifts" the first level of nesting left by (list key value)
         (map 'list
              (lambda (k v) (list k v))
              l1 l2)))

(defun simple-array->numcl-array (simple-array)
  (numcl:asarray simple-array))

(defun numcl-array->lispstat-column (numcl-array)
  (assert (numcl:numcl-array-p numcl-array) () "Must be a numcl-array")
  (assert (= 2 (length (array-dimensions numcl-array))) () "Must be 2D ie. single layered" )
  (let* (
         (col-name :unnamed)
         (flat (numcl:flatten numcl-array))
         (df (lisp-stat:make-df `(,col-name) `(,flat) ))
         (df-bound (lisp-stat:boundp 'temp-df))
         (unmade (when df-bound (lisp-stat:undef temp-df))) ;; conditionally unbind
         (made (lisp-stat:defdf temp-df df)) ;; worked and returnable
         ;; (typed (lisp-stat:set-properties temp-df :type `(,col-name ,col-type))) ; worked (must return temp-df)
         )
    made
    ))

(defun save-vega-html (plot-spec filename)
  "tests filename and writes plot-spec to disk as html"
  (let* (
         (fn-test-html (string= "html"
                                (pathname-type filename)))
         (fn-test-file (uiop:file-pathname-p filename))
         (fn-tested (and fn-test-file fn-test-html))
         )
    (assert fn-tested ()
            "The filename argument must represent a path to an html file. Received: ~S"
            filename)
    (vega:write-html plot-spec fn-test-file)))




;;;; ==================================== API

#+X(
    (run-all-reports :show t)
    (run-all-reports)
    )

;;;; ==================================== FIN

;;;; ==================================== build

;;;; run single experiment

(defun experiment-x-models (experiment models)
                     "Repeats experiment dictionary by length of models, adding :selected-model"
                     (mapcar
                      #'(lambda (model)
                          (sat model experiment :selected-model)
                          )
                      models))

(defun argument-x-models (arguments picked-model-experiments)
  "&&& repeats arguments by length of experiments
uses :selected-model to filter files"
                   (labels (
                            (get-models (experiment) (gat experiment :selected-model))
                            (filter-arguments (model) ) ; &&&
                            ;;selected-array-0
                            )
                     (let* (
                            (selected-models (mapcar #'get-models picked-model-experiments) )
                            (filtered-arguments (mapcar #'filter-arguments selected-models))
                            )
                       )))

(defun spoof-test-1 (arguments experiment)
  "&&& temp histogram caller"
  (labels (
           (run-job (experiment arguments id)
             (let* (
                    (subtitle (compose-name experiment :specifier id))
                    (2d-numcl-array (gat arguments :selected-array-0))
                    (plot-spec (def-histogram
                                   "Histogram of Raster Values"
                                 subtitle
                                 "values"
                                 "frequency"
                                 2d-numcl-array))
                    (filename (compose-name experiment
                                            :specifier id
                                            :path *plots-path*
                                            :filetype "html"))
                    (saved (save-vega-html plot-spec filename))
                    (ret-dict (list :output saved))
                    )
               ret-dict
               ))
           ;; other labels
           )
    (let* (
           ;; select model and files
           (id (gat experiment :selected-test))
           (models (gat experiment :models))
           (picked-model-experiments (experiment-x-models experiment models ))
           (picked-model-arguments (arguments-x-models arguments picked-model-experiments))
           (job-results (map #'run-job
                             picked-model-experiments
                             picked-model-arguments
                             (constantly id)))
           (result-dict (interleave (mapcar #'symbol->keyword models)
                                    job-results))
         )
    result-dict
    )))


(defun spoof-test-2 (arguments experiment)
  "&&& temp"
  (let* (
         (res (format nil "~A ~A ~A ~A ~A"
                      (gat arguments :a1)
                      "even smoller"
                      (gat experiment :selected-test)
                      (gat experiment :selected-test)
                      (gat experiment :selected-model)
                      ))
         (id "test-2")
         (ret-dict (list
                    :message res
                    :from id))
         )
    ret-dict
    ))

(defun symbol->keyword (sym)
  (intern (symbol-name sym) :keyword))

(defun where-agree (test array &key array-alt where-alt)
  "finds where 2x 2D arrays agree with a test, the second array may be a previous where result"
  ;; test xor alt args
  (assert (or (and array-alt (not where-alt))
              (and (not array-alt) where-alt))
          () "Use one of array-alt or where-alt")
  ;; test if array is 2D
  (assert (= 2 (length (array-dimensions array)))
          () "array must be 2D ie. single layered" )
  ;; test if array-alt is 2D
  (when array-alt
    (assert (= 2 (length (array-dimensions array-alt)))
            () "array-alt must be 2D ie. single layered" ))
  ;; test if where-alt is 2D ie. 2 equal length lists
  (when where-alt
    (assert (and (= 2 (length where-alt))
                 (= (length (first where-alt))
                    (length (second where-alt))))
            () "where-alt must be from a 2D array ie single layered"))
  (labels (;; convert where lists to 2D points
           (where->coords (where)
             (map 'list #'zip (first where) (second where)))
           ;; makes 1 2D point
           (zip (a b) (list a b))
           ;; undoes the point zipping, back to where lists
           (coords->where (coords)
             (list (mapcar #'first coords) (mapcar #'second coords)))
           )
    (let* (;; make and select individual where list
           (where-array-0 (numcl:where array test))
           (where-array-alt (when array-alt (numcl:where array-alt test)))
           (where-array-1 (or where-array-alt where-alt)) ;; pick one
           ;; convert where to coordinate for intersection
           (array-0-zip (where->coords where-array-0))
           (array-1-zip (where->coords where-array-1))
           ;; find shared points
           (agreement-zipped (intersection array-0-zip array-1-zip :test 'equal))
           ;; back to where list for return
           (agreement-where (coords->where agreement-zipped))
           )
      agreement-where
      )))


(defun where-aggregate (where-0 where-1)
  "foldably combines 2 where lists returning their union as a where list "
  ;; test if where lists are 2D ie. 2 equal length lists
  (assert (and (= 2 (length where-0))
               (= (length (first where-0))
                  (length (second where-0))))
          () "where-0 must be from a 2D array ie single layered")
  (assert (and (= 2 (length where-1))
               (= (length (first where-1))
                  (length (second where-1))))
          () "where-1 must be from a 2D array ie single layered")

  (labels (;; convert where lists to 2D points
           (where->coords (where)
             (map 'list #'zip (first where) (second where)))
           ;; makes 1 2D point
           (zip (a b) (list a b))
           ;; undoes the point zipping, back to where lists
           (coords->where (coords)
             (list (mapcar #'first coords) (mapcar #'second coords)))
           )
    (let* (
           ;; convert where to coordinate for intersection
           (where-lists (list where-0 where-1))
           (coordinates (lparallel:pmapcar #'where->coords where-lists))
           ;; aggregate points
           (coords-combined (union (first coordinates) (second coordinates) :test 'equal))
           ;; back to where list for return
           (where-combined (coords->where coords-combined))
           )
      where-combined
      )))

(defun where-invert (where array)
  ;; appears to be bad &&&
  "return a wherelist of all positions in the array which are not in the wherelist "
  (print "&&&in where invert creating data mask")
  ;; test if where lists are 2D ie. 2 equal length lists
  (assert (and (= 2 (length where))
               (= (length (first where))
                  (length (second where))))
          () "where must be from a 2D array ie single layered")
  ;; test if array is 2D
  (assert (= 2 (length (array-dimensions array)))
          () "array must be 2D ie. single layered" )

  (labels (
           (array->allwhere (array) (numcl:where array #'pt-always-t))
           (pt-always-t (pt) (= pt pt))
           ;; convert where lists to 2D points
           (where->coords (where)
             (map 'list #'zip (first where) (second where)))
           ;; makes 1 2D point
           (zip (a b) (list a b))
           ;; undoes the point zipping, back to where lists
           (coords->where (coords)
             (list (mapcar #'first coords) (mapcar #'second coords)))
           )
    (let* (
           (check (print "&&& convert array"))
           (where-0 (array->allwhere array))
           (where-1 where)
           ;; convert where to coordinate for intersection
           (where-lists (list where-0 where-1))
           (check (print "&&& make coordinates"))
           (coordinates (lparallel:pmapcar #'where->coords where-lists))
           ;; difference points
           (check (print "&&& make difference"))
           (coords (set-difference (nth 0 coordinates) (nth 1 coordinates) :test 'equal))
           ;; back to where list for return
           (check (print "&&& un make coordinates"))
           (where (coords->where coords))
           )
      where
      )))

(defun run-report(experiment &optional show)
  "opens preps and closes files, calls all test functions in an experiment"
  (when show (format t "~&~%In: run report"))
  (labels (
           ;; python foreign object to internal numcl array
           (py->array (obj) (simple-array->numcl-array (py:pymethod obj "read" 1)))
           ;; a function to get wherelists from arrays, which is a closure over nodataval
           (keep-nodata (nodataval)
             (lambda (arr)
               (numcl:where arr #'(lambda (pt)
                                    (= pt nodataval)))))

           (drop-nodata (nodataval)
             (lambda (arr)
               (numcl:where arr #'(lambda (pt)
                                    (/= pt nodataval)))))

           (all-data (nodataval)
             (lambda (arr)
               (numcl:where arr #'(lambda (pt)
                                    (= pt pt)))))
           )
    (let* (;; real values
           (tests (gat experiment :tests))
           (files (gat experiment :files))
           ;; open files
           (true-obj (open-geotiff (gat files :true)))
           (pred-0-obj (open-geotiff (gat files :pred-0)))
           (pred-1-obj (open-geotiff (gat files :pred-1)))
           (gpgk-obj (open-geopackage (gat files :gpkg)))
           (no-data-val (py:pyslot-value true-obj 'nodata))
           ;; pack into list
           (python-objects (list true-obj pred-0-obj pred-1-obj))
           ;; create arrays
           (check (print "creating arrays"))
           (arrays (lparallel:pmapcar #'py->array python-objects))
           ;; close files
           (true-closed (py:pymethod true-obj 'close))
           (pred-0-closed (py:pymethod pred-0-obj 'close))
           (pred-1-closed (py:pymethod pred-1-obj 'close))
           ;; &&& spoof arrays with captured arrays
           ;; (arrays (list *captured-true-array* *captured-pred-0-array* *captured-pred-1-array*))
           ;; mask
           (check (print "creating nodata mask"))
           (masks (lparallel:pmapcar (keep-nodata no-data-val) arrays)) ;; list of wherelists
           (mask-union (lparallel:preduce #'where-aggregate masks)) ;; single wherelist

           (check (print "creating data mask"))
           (notmask (lparallel:pmapcar (drop-nodata no-data-val) (list (first arrays)))) ;list of one wherelist

           ;; &&& is this breaking the run???
           ;; (check (print "creating data mask"))
           ;; (invert-mask (where-invert mask-union (first arrays)))
           ;; &&& replace with all data to test lengths
           (check (print "creating all data "))
           (all-data (first (lparallel:pmapcar (all-data no-data-val) (list (first arrays))))) ;single wherelist

           ;; combine everything and count
           (check (print "counting mask lengths"))
           (all-masks (push mask-union masks))
           ;; (all-notmasks (push invert-mask notmask))
           (all-notmasks (push all-data notmask)) ;; &&& push all datapoints into first place
           (any-masks (append all-notmasks all-masks))
           (mask-lengths (lparallel:pmapcar (lambda (wl) (length (first wl))) any-masks))
           (check (print mask-lengths))
           ;; &&& vvv
           ;; &&& recapture some stuff
           (capture (setf *captured-arrays* arrays))
           (capture (setf *captured-masks* any-masks))
           ;; &&& spoof with captured masks
           ;; (masks (list *captured-true-mask* *captured-pred-0-mask* *captured-pred-1-mask*))
           ;; &&& manipulate arrays
           ;; &&& D-pred-0
           ;; &&& D-pred-1
           ;; &&& categorical manipulations?
           ;; &&& select where not masked
           ;; &&&
           ;; &&& make an arguments plist
           ;; &&& spoof values
           ;; (tests '(spoof-test-1))
           ;; (arguments '(:A1 "hello" :A2 "smol" :A3 "frog"))
           ;; ;; use the preamble
           ;; (result-dicts (call-funs-on-args tests arguments experiment))
           ;; ;; re compose the result-maps
           ;; (keyed-result-dicts (interleave (mapcar #'symbol->keyword tests)
           ;;                                 result-dicts))
           ;; compose the return dict
           ;; (completed-experiment (sat keyed-result-dicts experiment :results))
           )
      ;; (print "check prints")
      ;; (print keyed-result-dicts)
      ;; (print completed-experiment)
      ;; return value
      mask-lengths
      ;; completed-experiment
      )))
#+X(
    (run-report *test-experiment* t)
    )

#+X(
    ;; work with captures

    ;; reset
    (defparameter *captured-arrays* nil) ; true pred0 pred1
    (defparameter *captured-masks* nil) ; all-data(of truemask) notmask(of truemask) mask-union truemask pred0mask pred1mask

    (time (cl-store:store *captured-arrays* "captured-arrays.lisp"))
    (time (cl-store:store *captured-masks* "captured-masks.lisp"))

    (setf *captured-arrays* (cl-store:restore "captured-arrays.lisp"))
    (setf *captured-masks* (cl-store:restore "captured-masks.lisp"))

    (length *captured-arrays*) ; 3
    (length *captured-masks*) ; 6

    ;; unpack
    (defparameter *captured-true-array* (first *captured-arrays*))
    (defparameter *captured-pred-0-array* (second *captured-arrays*))
    (defparameter *captured-pred-1-array* (third *captured-arrays*))
    ;; (defparameter *captured-invert-mask* (first *captured-masks*))
    (defparameter *captured-all-data* (first *captured-masks*)) ; &&& temp for subtractions
    (defparameter *captured-not-mask* (second *captured-masks*))
    (defparameter *captured-union-mask* (third *captured-masks*))
    (defparameter *captured-true-mask* (fourth *captured-masks*))
    (defparameter *captured-pred-0-mask* (fifth *captured-masks*))
    (defparameter *captured-pred-1-mask* (sixth *captured-masks*))

    ;; length extraction
    (let* (
           (mask-lengths (lparallel:pmapcar (lambda (wl) (length (first wl))) *captured-masks*))
           (all (first mask-lengths))
           (not (second mask-lengths))
           (union (third mask-lengths))
           (masked-via-diff (- all not))
           (err (- masked-via-diff union))
           )
      (format t "error: ~A" err)
      (assert (= err 0) () "expected error to be 0 found ~A" err )
      )
    )




(defun run-single-reports (experiments &optional show)
  "adds the single report stats results to each experiment dictionary"
  (when show (format t "~&~%In: run single reports"))
  (let (
        (completed-experiments (mapcar #'run-report experiments))
        )
      (when show (format t "~&Returning: ~S" completed-experiments))
      completed-experiments
      ))
#+X(
    (run-single-reports *test-experiments* t)
    )

;;;; vega plot utilities

(defun def-histogram (title subtitle xtitle ytitle 2d-numcl-array &key (bins 100))
  (let (
        (values (numcl-array->lispstat-column 2d-numcl-array))
        )
    (vega:defplot test-plot
      `(:title (:text ,title
                :subtitle ,subtitle)
        :width 400
        :height 400
        :data (:values ,values)
        :mark (:type :bar)
        :encoding (:x (
                       :title ,xtitle
                       :field :unnamed
                       :bin (:maxbins ,bins)
                       )
                   :y (
                       :title ,ytitle
                       :aggregate :count
                       ))))
    ))

(defun plot-all-vega (path &key (save-mode nil))
  "TODO lists and filters html files , mapcar plot-from-file,
   or save mode t  present 1 at a time print the filename with .png postfixed for convenient saving
on user input go to next plot or quit")


;;;; ==================================== scratch

#+X(
    ;; selection after filters &&&
    (numcl:take *test-array* (where-agree #'(lambda (x) (not (= x -9))) *test-array* :array-alt *test-subtr-array*))
    (numcl:take *test-subtr-array* (where-agree #'(lambda (x) (not (= x -9))) *test-array* :array-alt *test-subtr-array*))

    ;; filter out -9
    (where-agree #'(lambda (x) (not (= x -9))) *test-array* :array-alt *test-subtr-array*)
    (numcl:where *test-subtr-array* #'(lambda (x) (= x -9)))
    (numcl:where *test-array* #'(lambda (x) (= x -9)))

    ;; filter in out nils
    (where-agree #'null *test-array* :where-alt '((2) (2)))
    (where-agree #'null *test-array* :array-alt *test-subtr-array*)
    (where-agree (complement #'null) *test-array* :array-alt *test-subtr-array*)

    (numcl:- *test-array* *test-subtr-array*)
    (numcl:take *test-subtr-array* (numcl:where *test-subtr-array* #'null))
    (numcl:where *test-subtr-array* (complement #'null))
    (numcl:where *test-array* (complement #'null))


    (defparameter *test-subtr-array* (simple-array->numcl-array *test-subtr-array*))
    (defparameter *test-array* (simple-array->numcl-array *test-array*))

    ;; 3x3 1-9 for -9 tests
    (defparameter *test-subtr-array* (make-array '(3 3) :initial-contents '((-9.0 -9.0 1.0) (1.0 1.0 1.0) (1.0 -9.0 -9.0)) :element-type 'single-float))
    (defparameter *test-array* (make-array '(3 3) :initial-contents '((-9.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 -9.0)) :element-type 'single-float))

    ;; 3x3 1-9 for nil tests
    (defparameter *test-subtr-array* (make-array '(3 3) :initial-contents
                                                 '((nil nil 1.0) (nil 1.0 nil) (1.0 1.0 nil)) ))
    (defparameter *test-array* (make-array '(3 3) :initial-contents
                                           '((1.0 2.0 nil) (4.0 nil 6.0) (7.0 8.0 nil)) ))

    ;; 3x3 1-9
    (defparameter *test-subtr-array* (make-array '(3 3) :initial-contents '((1.0 1.0 1.0) (1.0 1.0 1.0) (1.0 1.0 1.0)) :element-type 'single-float))
    (defparameter *test-array* (make-array '(3 3) :initial-contents '((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)) :element-type 'single-float))

    ;; 100x100 all 100
    (defparameter *test-array* (make-array '(100 100) :initial-element 100.0 :element-type 'single-float))
    ;; 50x20 0-9999
    (defparameter *test-array* (numcl:reshape (numcl:arange 0 1000) '(50 -1)))
    ;; converted to numcl
    (defparameter *test-array-numcl* (simple-array->numcl-array *test-array*))

    (initialize-outputs) ;; clean up before testing output functions

    ;; vega plot spec
    (defparameter *test-plot* (def-histogram "MainTitle" "Luxurious sub title" "binned values" "frequency" 100 *test-array-numcl*))
    ;; check current plot spec
    (plot:plot *test-plot*)

    ;; placeholder for name making function
    (defparameter *test-plot-filename* (filepaths:join *plots-path* "bar.html"))

    (save-vega-html *test-plot* *test-plot-filename*)

    ;; placeholder for plot from html function
    (plot:plot-from-file *test-plot-filename* :browser :firefox)

;;;;


    (defparameter *test-experiments* '((:FILES (:TRUE #P"/home/holdens/tempdata/predictions1percent/input/tiffs/HEIGHT-CM.tiff" :PRED-0 #P"/home/holdens/tempdata/predictions1percent/input/tiffs/PREDICTED_HEIGHT-CM_regression_GBM.tiff" :PRED-1 #P"/home/holdens/tempdata/predictions1percent/input/tiffs/PREDICTED_HEIGHT-CM_regression_TSAI.tiff" :GPKG #P"/home/holdens/tempdata/predictions1percent/input/gpkgs/AOI-south.gpkg" :TABLE #P"/home/holdens/tempdata/predictions1percent/input/tables/temp-table.csv") :TRAIT ("HEIGHT-CM") :OBJECTIVE ("regression") :MODELS ("GBM" "TSAI") :TESTS (STAT-REG-DESCRIBE-TRUE-HISTO STAT-REG-DESCRIBE-TRUE-MEAN STAT-REG-DESCRIBE-PRED-HISTO STAT-REG-DESCRIBE-PRED-MEAN STAT-REG-COMPARE-PRED-R2 STAT-REG-COMPARE-PRED-RESIDUAL) :META-TESTS (META-STAT-REG-COMPARE-MODELS-ANOVA)) (:FILES (:TRUE #P"/home/holdens/tempdata/predictions1percent/input/tiffs/BARLEY-WHEAT.tiff" :PRED-0 #P"/home/holdens/tempdata/predictions1percent/input/tiffs/PREDICTED_BARLEY-WHEAT_multiclass_GBM.tiff" :PRED-1 #P"/home/holdens/tempdata/predictions1percent/input/tiffs/PREDICTED_BARLEY-WHEAT_multiclass_TSAI.tiff" :GPKG #P"/home/holdens/tempdata/predictions1percent/input/gpkgs/AOI-south.gpkg" :TABLE #P"/home/holdens/tempdata/predictions1percent/input/tables/temp-table.csv") :TRAIT ("BARLEY-WHEAT") :OBJECTIVE ("multiclass") :MODELS ("GBM" "TSAI") :TESTS (STAT-CAT-DESCRIBE-TRUE-BARCHART STAT-CAT-DESCRIBE-PRED-BARCHART STAT-CAT-COMPARE-PRED-F1 STAT-CAT-COMPARE-PRED-CONFUSIONMX) :META-TESTS (META-STAT-CAT-COMPARE-MODELS-ANOVA))))

    (defparameter *test-experiment* (first *test-experiments*))

    );; end no X

;;;; ==================================== reference

#+X(

    ;; numcl has mean
    (numcl:mean *test-array-numcl*)
    ;; numcl has standard-deviation
    (numcl:standard-deviation *test-array-numcl*)
    ;; 5 num sum is min 25 50 75 max
    (pyscp.stats:describe :a (numcl:flatten *test-array-numcl*))
    (pynpy:percentile :a *test-array-numcl* :q 50)
    ;; vvv &&&
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

    ) ;; end no X


#+X(

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
    (py:pyslot-value *dataset-gtif* 'nodata)
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
    (type-of (make-array '(2 2) :element-type 'single-float)) ;; => (SIMPLE-ARRAY SINGLE-FLOAT (2 2))

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

    ) ;; end no X
#|
&&&
|#
;;;; ==================================== X
;;; ===================================== X
;; ====================================== X
                                        ; X
