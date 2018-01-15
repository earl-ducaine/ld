


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

(defparameter *setting-keys* '(:nupi-scsi0-0-disk-file
			       :nupi-scsi0-1-disk-file
			       :nupi-scsi2-0-disk-file))

(defparameter *ld-projects-structure*
  '(ld-conf tapes roms rtc-ram cmos-ram lam outlogs))

(defparameter *ld-projects-structure*
  '(:disks
    :ld-conf
    :tapes
    :roms
    :rtc-ram
    :cmos-ram
    :lam
    :outlogs))



;; someday validate build
;; note when roms change
;; artifacts are missing
;; outlogs are different.
;; validate ld.conf matches what's built


;; 'From folder', the ld project folder

(defparamater *ld-folder* nil)

;; 'To folder', where the artifacts reside
(defparamater *archive-library* "")


(defun archive-projects
    ;; current directory or
    "~/dev/common-lisp/lambda-delta/ld/")
