;;; -*- Package: FS; Mode: Common-lisp; Base: 10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;-*-

1;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1980, Massachusetts Institute of Technology
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.*

(defparameter 4%file-character-opcode* chaos:dat-op) 
(defparameter 4%file-binary-opcode* (logior chaos:dat-op #o100)) 
(defparameter 4%file-command-opcode* chaos:dat-op) 
(defparameter 4%file-synchronous-mark-opcode* (1+ chaos:dat-op)) 
(defparameter 4%file-asynchronous-mark-opcode* (+ chaos:dat-op 2)) 
(defparameter 4%file-notification-opcode* (+ chaos:dat-op 3)) 
(defparameter 4%file-eof-opcode* chaos:eof-op)


(defstruct 4(server-dataproc-comm (:type :list*) (:alterant nil) (:conc-name 2"SERVER-DATAPROC-COMM-"*)
  (:callable-constructors nil) (:predicate nil) (:copier nil))
  iotype
  control-proc
  data-proc
  conn
  sibling
  cell
  opening
  arg
  binp
  dinfo
  cconn
  tid)

;;;
;;; Qfile error condition handling.
;;;

1;;; An error string is as follows:
;;;  FHN<SP>ERROR<SP>Error-code<SP>Error-severity<SP>Error-description
;;; The error code is a three letter code that uniquely determines the error.  In general,
;;; this code will be ignored, but some codes may be of interest.  FNF is file not found,
;;; and NER is not enough resources.  The severity is either F (Fatal) or R (Restartable).
;;; If an error is Fatal, it can not be continued from, even if it is an asynchronous
;;; error.  If an error is Restartable, sending a CONTINUE command for the appropriate
;;; file handle will cause the file job to proceed where it left off.  In general, before
;;; the error is continued from, the error condition should be corrected, or the error
;;; will happen again immediately.
;;; The string that is passed in is expected to be "temporary" (contained in a chaos packet,
;;; for example).  Therefore, if an error handler gets called and it wants to save some
;;; of the strings, it must copy the ones it wishes to save.*


(defprop 4file-process-error-new* t :error-reporter) 

(defun 4file-process-error-new* (string &optional pathname-or-stream proceedable noerror &rest make-condition-args &aux s-p
	 error-code error-severity error-string who-for (default-cons-area background-cons-area))
  
  (cond ((typep pathname-or-stream 'pathname)
	 (setq who-for pathname-or-stream))
	
	((typep pathname-or-stream 'si:file-stream-mixin)
	 (setq who-for (funcall pathname-or-stream :pathname)))
	
	(t
	  (setq who-for pathname-or-stream)))
  
  (setq s-p (file-check-command 3"ERROR"* string))
  
  (setq error-code
	(subseq (string string) s-p
	(setq s-p (position #\Space (the string (string string)) :start s-p :test #'char-equal))) )
  (setq s-p (1+ s-p))
  (setq error-severity
	(subseq (string string) s-p
	(setq s-p (position #\Space (the string (string string)) :start s-p :test #'char-equal))) )
  (setq error-string (nsubstring string (1+ s-p) (length string)))
  (and who-for (setq error-string (string-append error-string 3" for "* (string who-for))))
  (let ((condition
	 (apply 'make-condition
		(or (get (intern error-code 3"FS"*) 'file-error) 'file-operation-failure-1) 3"~A"*
		who-for make-condition-args)))
    (setf (send condition :format-args) (list error-string))
    (if noerror
      condition
      (signal condition :proceed-types
	      (cond
		((consp proceedable) proceedable)
		(proceedable '(:retry-file-operation))))))) 


(defun 4file-check-command* (command returned-string &optional (y-or-n-p ()) &aux start end)
  2"Verify a reply from a file server using the FILE protocol.
Returns the index in the reply of the start of the data.
Gets an error if the reply's command name is not COMMAND."*
  (setq start (1+ (position #\Space (the string (string returned-string)) :test #'char-equal)))
  (setq end
	(or (string-search-set '(#\Space #\Newline) returned-string start)
	    (length returned-string)))
  (cond ((string-equal returned-string command start 0 end) (1+ end))	1;Index of character after the delimiting space*
	(y-or-n-p ())
	(t
	  (ferror ()				1;I think this is best for an internal bug in FILE protocol.*
		  3"Incorrect command name ~S in acknowledge from file computer"*
		  (nsubstring returned-string start end)))))


(defprop 4dat* data-error file-error) 
(defprop 4data-error* dat file-error) 

(defprop 4hna* host-not-available file-error) 
(defprop 4host-not-available* hna file-error) 

(defprop 4nfs* no-file-system file-error) 
(defprop 4no-file-system* nfs file-error) 


(defprop 4ner* not-enough-resources file-error) 
(defprop 4not-enough-resources* ner file-error) 

(defprop 4uop* unknown-operation file-error) 
(defprop 4unknown-operation* uop file-error) 
(defprop 4ukc* unknown-operation file-error) 

(defprop 4lip* login-problems file-error) 
(defprop 4login-problems* lip file-error) 

(defprop 4unk* unknown-user file-error) 
(defprop 4unknown-user* unk file-error) 

(defprop 4ip?* invalid-password file-error) 
(defprop 4invalid-password* ip? file-error) 

(defprop 4nli* not-logged-in file-error) 
(defprop 4not-logged-in* nli file-error) 

(defprop 4lck* file-locked file-error) 
(defprop 4file-locked* lck file-error) 

(defprop 4cir* circular-link file-error) 
(defprop 4circular-link* cir file-error) 

1;;UNIMPLEMENTED-OPTION*


(defprop 4uoo* unimplemented-option file-error) 
(defprop 4unimplemented-option* uoo file-error) 

(defprop 4ibs* invalid-byte-size file-error) 
(defprop 4invalid-byte-size* ibs file-error) 

(defprop 4ico* inconsistent-options file-error) 
(defprop 4inconsistent-options* ico file-error) 

(defprop 4nmr* no-more-room file-error) 
(defprop 4no-more-room* nmr file-error) 

(defprop 4for* filepos-out-of-range file-error) 
(defprop 4filepos-out-of-range* for file-error) 

(defprop 4nav* not-available file-error) 
(defprop 4not-available* nav file-error) 


1;;; Subclasses of FILE-OPERATION-FAILURE.*

(defprop 4fnf* file-not-found file-error) 
(defprop 4file-not-found* fnf file-error) 

(defprop 4dnf* directory-not-found file-error) 
(defprop 4directory-not-found* dnf file-error) 

(defprop 4dev* device-not-found file-error) 
(defprop 4device-not-found* dev file-error) 


(defprop 4lnf* link-target-not-found file-error) 
(defprop 4link-target-not-found* lnf file-error) 

1;ACCESS-ERROR means some kind of protection failure.*

(defprop 4acc* access-error file-error) 
(defprop 4access-error* acc file-error) 

(defprop 4atf* incorrect-access-to-file file-error) 
(defprop 4incorrect-access-to-file* atf file-error) 

(defprop 4atd* incorrect-access-to-directory file-error) 
(defprop 4incorrect-access-to-directory* atd file-error) 


1;;INVALID-PATHNAME-SYNTAX*

(defprop 4ips* invalid-pathname-syntax file-error) 
(defprop 4invalid-pathname-syntax* ips file-error) 

(defprop 4iwc* invalid-wildcard file-error) 
(defprop 4invalid-wildcard* iwc file-error) 

(defprop 4wna* wildcard-not-allowed file-error) 
(defprop 4wildcard-not-allowed* wna file-error) 

1;;WRONG-KIND-OF-FILE*

(defprop 4wkf* wrong-kind-of-file file-error) 
(defprop 4wrong-kind-of-file* wkf file-error) 

(defprop 4iod* invalid-operation-for-directory file-error) 
(defprop 4invalid-operation-for-directory* iod file-error) 

1;; CREATION-FAILUREs*

(defprop 4fae* file-already-exists file-error) 
(defprop 4file-already-exists* fae file-error) 

(defprop 4snd* superior-not-directory file-error) 
(defprop 4superior-not-directory* snd file-error) 

(defprop 4ccd* create-directory-failure file-error) 
(defprop 4create-directory-failure* ccd file-error) 

(defprop 4dae* directory-already-exists file-error) 
(defprop 4directory-already-exists* dae file-error) 

(defprop 4ccl* create-link-failure file-error) 
(defprop 4create-link-failure* ccl file-error) 

1;Should have property :NEW-PATHNAME.*

(defprop 4ref* rename-to-existing-file file-error) 
(defprop 4rename-to-existing-file* ref file-error) 

(defprop 4rad* rename-across-directories file-error) 
(defprop 4rename-across-directories* rad file-error) 

(defprop 4ukp* unknown-property file-error) 
(defprop 4unknown-property* ukp file-error) 

(defprop 4ipv* invalid-property-value file-error) 
(defprop 4invalid-property-value* ipv file-error) 

(defprop 4ipn* invalid-property-name file-error) 
(defprop 4invalid-property-name* ipn file-error) 

(defprop 4dne* directory-not-empty file-error) 
(defprop 4directory-not-empty* dne file-error) 

(defprop 4dnd* dont-delete-flag-set file-error) 
(defprop 4dont-delete-flag-set* dnd file-error) 

;;
;; Misc Qfile
;;

(defsubst 4data-handle* (data-connection direction)
  (case direction
	(:input (data-input-handle data-connection))
	(:output (data-output-handle data-connection))))

(defsubst 4data-stream* (data-connection direction)
  (cadr (member direction (data-stream-list data-connection) :test #'eq))) 


