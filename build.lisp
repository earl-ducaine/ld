


(in-package :lambda-delta)

(defun setting-map-to-hash (setting-map)
  (let ((setting-map-length (length setting-map)))
  (unless (evenp setting-map-length)
    (error "setting-map must have an even number of keys"))
  (let ((hash (make-hash-table))
	;; Access in reverse order so that last item added is the
	;; final value of hash entry.  Previous value is treated as
	;; obsolete and thrown away.
	(rsetting-map (reverse setting-map)))
    ;; throw away remander
    (dotimes (i (truncate (/ (length rsetting-map) 2)))
      (let ((n1 (* i 2))
	    (n2 (+ (* i 2) 1)))
	;; map reversed. Even items are values odd are the key.
	(setf (gethash (nth n2 rsetting-map) hash)
	      (nth (- setting-map-length n1)
		   rsetting-map))))
    hash)))

(defun map-by (function type list chunk-size &optional (discard-remainder t))
  (map type function
       (iterate (for chunk on list by (lambda (list) (nthcdr chunk-size list)))
		(let ((length-chunk (length chunk)))
		  (cond
		    ((>= length-chunk chunk-size)
		     (collect (butlast chunk (- length-chunk chunk-size))))
		    (discard-remainder
		     nil)
		    (t
		     (collect chunk)))))))


;; map-list get items as list


(defun run-map-by ()
  (map-by (lambda(x) x)  '(1 2 3 4 5 6) 2 t))

;; Apply function to list by chunks of chunk-size, where chunks is a
;; list and where the remander, for lists (/= (mod (length list)
;; chunk-size) 0) is either passed to the funct as an incompletely
;; filled chunk or discarded.
;; (defun map-by (function list chunk-size &optional (discard-remainder t))
;;   (iterate (for chunk on list by (lambda (list) (butlast list (- (length list) chunk-size))))
;; 	   (collect (funcall function chunk))))

(defparameter *setting-keys* '(:nupi-scsi0-0-disk-file
			       :nupi-scsi0-1-disk-file
			       :nupi-scsi2-0-disk-file))

(defparameter *ld-projects-structure*
  '(ld-conf tapes roms rtc-ram cmos-ram lam outlogs))

(defparameter *ld-project-structure*
  '(:disks directory
    :ld-conf file
    :tapes directory
    :roms directory
    :rtc-ram file
    :cmos-ram file
    :lam file
    :outlogs directory))

;; someday validate build
;; note when roms change
;; artifacts are missing
;; outlogs are different.
;; validate ld.conf matches what's built


;; 'From folder', the ld project folder
(defparameter *source-ld-project-folder* *default-pathname-defaults*)

(defparameter *target-archieve-folder*
  (probe-file
   (merge-pathnames
    (cl-fad:pathname-parent-directory *source-ld-project-folder*)
    "build-archive")))



(let ((from-pathname-string "disks")
      (from-path (probe-file (merge-pathnames *source-ld-project-folder* from-pathname-string))))
  (unless from-path
    (error "File or directory doesn't exist: ~a~%" from-path))
  (let ((from-path-directory-p (directory-pathname-p probe-file)))
    (cond
      (from-path-directory-p


(cl-fad:copy-file
 (cl-fad:file-exists-p
  (probe-file (merge-pathnames *source-ld-project-folder* "disks")))
 *target-archieve-folder* )

;; 'To folder', where the artifacts reside
(defparamater *archive-library* "")

(defparameter *current-project*
  '(:disks "disks"
    :ld-conf "ld.conf"
    :tapes "tapes"
    :roms "romes"
    :rtc-ram "RTC.RAM"
    :cmos-ram "CMOS.RAM"
    :lambda-executable "src/lam"
    :outlogs "logs"))

(defparameter *ld-project-structure*
  '(:disks directory
    :ld-conf file
    :tapes directory
    :roms directory
    :rtc-ram file
    :cmos-ram file
    :lam file
    :outlogs directory))

(defun execute-test-case(expect-error-p test-code)
  '(lamda (chunk)
    (let ((passed-p (not expect-error-p)))
      (handler-case
	  (eval *tests*)
	(t () (setf passed-p expect-error-p)))
      passed-p))
  '(map-by (lambda(x) x) 'list chunkx4 2)
  (let ((test-cases (map-by (lambda (chunkx4)
			      (map-by (lambda (x) x) 'list chunkx4 2))
			    'list
			    *tests* 4)))
    (labels ((harness (expect-error-p test-code)
	       (let ((passed-p (not expect-error-p)))
		 (handler-case
		     (eval test-code)
		   (t () (setf passed-p expect-error-p)))
		 (format t "test-code: ~a~%" (prin1-to-string test-code))
		 passed-p)))
      (map 'list (lambda (test-item)
		   (let ((result (harness
				  (cadr (assoc :expect-error-p test-item))
				  (cadr (assoc :test-procedure test-item)))))
		     result))
	   test-cases))))

(defparameter *tests*
  '(:test-procedure '(progn
		      (build-from-path "/home/rett/dev/common-lisp/lambda-delta/ld"
		       'TO-BASE-DIRECTORY "disks" :directory))
    :expect-error-p nil
    :test-procedure '(progn
		      (build-from-path "/home/rett/dev/common-lisp/lambda-delta/ld"
		       'TO-BASE-DIRECTORY "disks" :direc))
    :expect-error-p t))



(defun run-build-from-path ()
  ;; Test header
  (format t "Results:  ~%")
  (let (passed-p)
    ;; test bad from
    (let ((from-base-directory "/home/rett/dev/common-lisp/lambda-deltaa/ld"))
      (handler-case
	  (progn
	    (build-from-path from-base-directory
			     'TO-BASE-DIRECTORY "disks" :directory))
	(t () (setf passed-p t))))
    (format t "Results:  ~%test1:  ~a~%"  (if passed-p "passed" "failed"))
    ;; test good from-base-directory
    (let ((from-base-directory "/home/rett/dev/common-lisp/lambda-delta/ld"))
      (setf passed-p t)
      (handler-case
	  (progn
	    (build-from-path from-base-directory
			     'TO-BASE-DIRECTORY "disks" :directory))
	(t () (setf passed-p nil))))
    (format t "test1:  ~a~%"  (if passed-p "passed" "failed"))))




(defmacro str (&rest args)
    `(funcall #'concatenate 'string ,@args))

(defun build-from-path (from-base-directory to-base-directory
			artifact-relative-pathname
			&optional (artifact-pathname-type :directory))
  (let ((from-pathname
	 (cond
	   ((eq artifact-pathname-type :directory)
	    (cl-fad:merge-pathnames-as-directory
	     (cl-fad:pathname-as-directory from-base-directory)
	     (cl-fad:pathname-as-directory artifact-relative-pathname)))
	   ((eq artifact-pathname-type :file)
	    (cl-fad:merge-pathnames-as-file
	     (cl-fad:pathname-as-directory from-base-directory)
	     (cl-fad:pathname-as-file artifact-relative-pathname)))
	   (t
	    (error "Unknown artifact-pathname-type: ~a~%"
		   artifact-pathname-type)))))
    (unless from-pathname
      (error (str "base-directory: ~a with  artifact-pathname: ~a"
		  "needs to describe an existing file or directory~%")))
    ;; We now know the pathname exists.  Make sure that if it's a
    ;; directory pathname type is :directory and if it's a file,
    ;; pathname type is :file.
    (unless (or (and (eq pathname-type :directory)
		     (cl-fad:directory-exists-p from-pathname))
		(and (not (cl-fad:directory-exists-p from-pathname))
		     (eq pathname-type :file)))
      (error (str "pathname (~a) either of wrong artifact-pathname-type (~a) "
		  "or if :directory needs to exist")
	     from-pathname pathname-type))))

    (cond
      ((eq pathname-type :directory)
       (let ((to-directory (cl-fad:merge-pathnames-as-directory
			    (cl-fad:pathname-as-directory base-to-directory)
			    (cl-fad:pathname-as-directory artifact-relative-pathname))))
	 (ensure-directories-exist to-directory)
	 (cl-fad:copy-file from-pathname to-directory)))
    ;; ((eq pathname-type :file)
    ;;  (let ((to-file (cl-fad:merge-pathnames-as-file
    ;; 			  (cl-fad:pathname-as-directory to-base-directory)
    ;; 			  (cl-fad:pathname-as-file to-file))))
    ;;    (ensure-directories-exist to-file)
    ;;    (cl-fad:copy-file from-pathname to-file)))
    (t
     (error (str "Something went wrong, likely a bug, with the following "
		 "unanticipated parameters: from-base-directory ~a, "
		 "to-base-directory ~a, artifact-relative-pathname ~a, "
		 "pathname-type ~a"
		 from-base-directory to-base-directory
		 artifact-relative-pathname pathname-type))))))





(defun archive-project ()
    ;; current directory or
    "~/dev/common-lisp/lambda-delta/ld/")

;; Creates any needed targets.  Target pathes should be absolute.
;; (defun ensure-targets (target-directory)
;;   (let ((target (pathname-as-directory target)))
;;     (unless (cl-fad:directory-exists-p target)
;;       (when (cl:fad:file-exists-p target)
;; 	(error "Target exists as a file not a directory, ~a" target))
;;       (ensure-directories-exist target))))
