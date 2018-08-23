;;; -*- Mode:Common-Lisp; Package:File-System; Fonts:(Cptfont Hl12b Hl12bi); Base:10 -*-

;1;;*			1      RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;*			1TEXAS INSTRUMENTS INCORPORATED.*
;1;;*				1 P.O. BOX 2909*
;1;;*			1      AUSTIN, TEXAS 78769*
;1;;*				1    MS 2151*
;1;;*
;1;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.*

;1;;===========================================================================*
;1;;                                 F T P   S T R E A M S*
;1;; *
;1;; HISTORY:*
;1;;   7/3/85 - original (adapted from chaos file streams - see NET;QFILE) [rla]*
;1;;  10/86   - modified for TCP 2.0 [pw]*
;1;;  10/86   - modified for GENI [rla]*
;1;;===========================================================================*

;1;; ----- FTP-STREAM-MIXIN*

(DEFFLAVOR ftp-stream-mixin
	   (control-connection			;1FTP control connection*
	    file-status)
	   (si:property-list-mixin si:file-stream-mixin)	;1this has pathname stuff in it*
  (:initable-instance-variables control-connection)
  (:gettable-instance-variables control-connection))

;1; required by file-stream-mixin*
(DEFMETHOD (ftp-stream-mixin :qfaslp) ()
  ;1; don't really know*
  ())                                    	


(DEFMETHOD (ftp-stream-mixin :truename) ()
  (GETF si:property-list :truename)) 


(DEFMETHOD (ftp-stream-mixin :length) ()
  (GETF si:property-list :length)) 
 

(DEFMETHOD (ftp-stream-mixin :properties) (&optional ignore)  ;1error-p*
  (SEND self :plist)) 

;1;; This is used by zmacs when deciding that a file was changed out from under you (ZWEI:SAVE-BUFFER -> ZWEI:DESCRIBE-FILE-ID).*
;1;;   1.  QFILE returns the creation date on a open-output, this is used in the compare; we have to go and get it*
;1;;   2.  ZMACS croaks on a nil creation date; return the most innocuous creation date possible if not known (1/1/00 00:00:00)*

(DEFMETHOD (ftp-stream-mixin :info) (&aux truename crdate properties)
  (SETQ truename (SEND self :truename))
  ;1; Try to get creation date for newly closed output file*
  (WHEN (AND (NOT (SETQ crdate (SEND self :get :creation-date))) (EQ file-status :closed)
	     (SETQ properties (SEND *ftp-service* :properties :tcp truename ()))
	     (NOT (ERRORP properties)))
    (SETQ crdate (GETF (CDR properties) :creation-date)))
  (CONS truename (OR crdate (ENCODE-UNIVERSAL-TIME 0 0 0 1 1 0 time::*timezone*))))
 

;1;; ----- FTP-DATA-STREAM-MIXIN*
;1;; Flavors that really have an open data connection*
;1;; FILE-STATUS is one of*
;1;;  :OPEN - a file is currently open on this channel*
;1;;  :CLOSED - no file is open, but the channel exists*
;1;;  :EOF - a file is open, but is at its end (no more data available).*
;1;;  NOT DOING ANY MARKING (SYNC/ASYNC)*

(DEFFLAVOR ftp-data-stream-mixin
	   ((file-status :open))
	   (ftp-stream-mixin)
  (:included-flavors si:file-data-stream-mixin ip:basic-stream)
  (:settable-instance-variables file-status)) 


(DEFMETHOD (ftp-data-stream-mixin :safe-to-use-p) ()
 ;1; Don't know if or how this is used*
  (EQ file-status :open)) 


;1;; QFILE lets (requires that) the rename and delete operations be done on an open file (and thats the way dired does it)*
;1;; We also need the file to be open (to guarantee an open control connection), but will close the data connection prior to doing*
;1;; the operation. *

(DEFVAR *close-data-connection-only* nil "2Bound by :delete and :rename around :close*")


(DEFMETHOD (ftp-data-stream-mixin :delete) (&optional (error-p t) &aux result)
  (ip:with-stream-whostate
    "Delete-and-expunge"
    (WHEN (EQ file-status :closed)
      (FERROR 'ftp-error "~S is closed - an illegal state for delete." self))
    (UNWIND-PROTECT
	(PROGN
	  ;1; abort the data connection*
	  (LET ((*close-data-connection-only* t))
	    (SEND self :close :abort))
	  (IF (ftp-unfavorable (SEND control-connection :delete (pathstring-for-ftp pathname)))
	      ;1; then signal or return error condition*
	      (SETQ result (check-ftp-status pathname control-connection error-p ()))
	      ;1; else return blocks freed (which we don't know) *
	      (SETQ result 0)))
      (free-ftp-connection control-connection))	;1now close it*
    result)) 



(DEFMETHOD (ftp-data-stream-mixin :rename) (new-name &optional (error-p t) &aux result)
  (ip:with-stream-whostate
    "Rename"
    (WHEN (EQ file-status :closed)
      (FERROR 'ftp-error "~S is closed - an illegal state for rename" self))
    (UNWIND-PROTECT
	(PROGN
	  ;1; abort the data connection*
	  (LET ((*close-data-connection-only* t))
	    (SEND self :close :abort))
	  (IF (ftp-unfavorable (SEND control-connection :rename (pathstring-for-ftp pathname)
				     (pathstring-for-ftp new-name)))
	      ;1; then signal or return error condition*
	      (SETQ result (check-ftp-status pathname control-connection error-p ()))	;1signal or return condition*
	      ;1; else*
	      (PROGN
		(SETQ pathname new-name)
		(SEND self :putprop (SETQ result (SEND *ftp-service* :truename :tcp new-name)) :truename))))
      (free-ftp-connection control-connection))	;1now close it*
    (VALUES result (NOT (ERRORP result))))) 



(DEFMETHOD (ftp-data-stream-mixin :change-properties) (&optional (error-p t) &rest ignore)
  (ip:with-stream-whostate
    "Change-properties"
    (LET ((condition
	    (MAKE-CONDITION 'ftp-error "Can't change properties of ~A using FTP" pathname
			    :change-properties)))
      (IF error-p
	  (SIGNAL condition :proceed-types ())
	  condition)))) 

;1;; CONTINUE method removed (related to async stuff) *


;1;; ----- FTP-INPUT-STREAM-MIXIN*
;1;; NOTE: we will be doing buffering on top of TCP (so we can use all the existing mixins)*


(DEFFLAVOR ftp-input-stream-mixin
	   ()
	   (ftp-data-stream-mixin)
  (:included-flavors si:input-file-stream-mixin ip:basic-input-stream)) 


(DEFVAR signal-ftp-close-reply ()) 		;1used for testing *


(DEFMETHOD (ftp-input-stream-mixin :around :close) (cont mt args &optional abortp)
  "2Close the stream by calling a method of ftp-control-connection.  This stream is the data-connection of
an ftp-control-connection.  Ftp-control-connection calls will recurse back through here with 
ip:*ftp-data-connection-close* bound to true.*"
  (DECLARE (SPECIAL ip:*ftp-data-connection-close*))
  (ip:with-stream-whostate
    "File Close"
    (BLOCK nil
      (COND
	;1; this is a data connection close coming back from the ftp-control-connection method*
	(ip:*ftp-data-connection-close*
	 (RETURN (AROUND-METHOD-CONTINUE cont mt args)))
	((EQ file-status :closed) (RETURN file-status)))
      ;1; drive the close through the control connection*
      (UNLESS (EQ file-status :eof) (SETF abortp t))
      (SEND (net:translated-host (PATHNAME-HOST pathname)) :deregister-stream self)
      (UNWIND-PROTECT
	  (IF abortp
	      (SEND control-connection :close :abort)
	      ;1; close normally & pay attention to errors*
	      (WHEN (OR signal-ftp-close-reply (ftp-unfavorable (SEND control-connection :close)))
		;1; Get this off the who-line before signalling since the after method won't be invoked*
		(SEND tv:who-line-file-state-sheet :delete-stream self)
		(SIGNAL
		  (MAKE-CONDITION 'ftp-error (SEND control-connection :reply-string) pathname :close)
		  :proceed-types ())))
	(SETQ file-status :closed)
	(UNLESS *close-data-connection-only* (free-ftp-connection control-connection)))
      file-status)))


;1;; REMOVED :SET-BUFFER-POINTER -- NOT SUPPORTED*

;1; Override this so we don't show an incorrect percentage*

(DEFMETHOD (ftp-input-stream-mixin :who-line-information) (&aux count length percent)
  (SETQ count (SEND self :read-pointer))
  ;1; Note: we may not know length-in-bytes and if we do it is liable to be off due to ascii translation*
  ;1; Don't show percentage unless we know some length (length-in-bytes or because eof has been reached)*
  ;1; Switch to using real length when eof has been reached so percentage will go to 100*
  (SETQ length (IF (EQ file-status :eof)
		   count
		   (SEND self :get :length-in-bytes)))
  (WHEN (AND length (NOT (ZEROP length)))
    (SETQ percent (ROUND (* 100 count) length)))
  (VALUES (SEND self :truename)
	  :input
	  count
	  percent))


(DEFMETHOD (ftp-input-stream-mixin :around :next-input-buffer) (cont mt args ignore)
  ;1; call primary method only if file is open*
  (ip:with-stream-whostate
    "File Input"
    (WHEN (AND ip:fin-received-p (ZEROP ip:pending-octet-count))
      (SETF (GETF si:property-list :length) (SEND self :read-pointer))
      (SETF file-status :eof))
    (WHEN (EQ file-status :open)
      (AROUND-METHOD-CONTINUE cont mt args))))



;1;; ----- FTP-OUTPUT-STREAM-MIXIN*

(DEFFLAVOR ftp-output-stream-mixin
	   ()
	   (ftp-data-stream-mixin)
  (:included-flavors si:output-file-stream-mixin ip:basic-output-stream)) 


(DEFMETHOD (ftp-output-stream-mixin :around :send-output-buffer) (cont mt args ignore ignore)
  (ip:with-stream-whostate
    "File Output"
    (IF (EQ file-status :open)
	(AROUND-METHOD-CONTINUE cont mt args)
	(FERROR 'stream-invalid "Attempt to output to stream ~S while in an illegal state ~S." self file-status)))) 


(DEFMETHOD (ftp-output-stream-mixin :around :close) (cont mt args &optional abortp)
  "2Close the stream by calling a method of ftp-control-connection.  This stream is the data-connection of
an ftp-control-connection.  Ftp-control-connection calls will recurse back through here with 
ip:*ftp-data-connection-close* bound to true.*"
  (DECLARE (SPECIAL ip:*ftp-data-connection-close*))
  (ip:with-stream-whostate
    "File Close"
    (BLOCK nil
      (COND
	;1; this is a data connection close coming back from the ftp-control-connection method*
	(ip:*ftp-data-connection-close*
	 (RETURN (AROUND-METHOD-CONTINUE cont mt args)))
	((EQ file-status :closed) (RETURN file-status)))
      ;1; drive the close through the control connection*
      ;1; Closing an open output channel.  Finish sending the data.*
      (WHEN (EQ file-status :open) (SEND self :eof))
      (SETQ file-status :closed)
      (SEND (net:translated-host (PATHNAME-HOST pathname)) :deregister-stream self)
      (UNWIND-PROTECT
	  (COND (abortp
		 (SEND control-connection :close :abort)
		 ;1; If aborting out of a file-writing operation before normal :CLOSE,*
		 ;1; delete the incomplete file.  Don't worry if it gets an error.*
		 ;1; Dont use the delete method of stream (get into recursive calls to close, ...)*
		 (LET ((reply-code (SEND control-connection :reply-code))
		       (reply-string (SEND control-connection :reply-string)))
		   (SEND control-connection :delete (pathstring-for-ftp pathname))
		   ;1; mask errors incurred by :delete*
		   (SETF (SEND control-connection :reply-code) reply-code
			 (SEND control-connection :reply-string) reply-string)))
		(t
		 (WHEN (OR signal-ftp-close-reply (ftp-unfavorable (SEND control-connection :close)))
		   (SEND tv:who-line-file-state-sheet :delete-stream self)
		   (SIGNAL
		     (MAKE-CONDITION 'ftp-error (SEND control-connection :reply-string) pathname :close)
		     :proceed-types ()))))
	(UNLESS *close-data-connection-only* (free-ftp-connection control-connection)))
      file-status)))


(DEFMETHOD (ftp-output-stream-mixin :who-line-information) ()
  (VALUES (SEND self :truename)
	  :output
	  (SEND self :read-pointer)
	  (WHEN (EQ file-status :closed) 100))) 


(DEFMETHOD (ftp-output-stream-mixin :eof) ()
  (ip:with-stream-whostate
    "File Finish"
    (SEND self :finish)
    (SETQ file-status :eof)
    (SETF (GETF si:property-list :length) (SEND self :read-pointer))))


;1;; ------ FTP-PROBE-STREAM-MIXIN*

(DEFFLAVOR ftp-probe-stream-mixin
	   ()
	   (ftp-data-stream-mixin)
  (:included-flavors ip:character-input-stream-mixin))


(DEFMETHOD (ftp-probe-stream-mixin :direction) ()
  ())


(DEFMETHOD (ftp-probe-stream-mixin :close) (&optional abortp)
  "2Probe streams to not have an open data connection, nor a reserved control-connection.*"
  (IGNORE abortp)
  (ip:with-stream-whostate
    "File Close"
    (COND
      ((EQ file-status :closed))
      (t
       (SETF file-status :closed)
       (free-ftp-connection control-connection)
       (SEND (net:translated-host (PATHNAME-HOST pathname)) :deregister-stream self)))
    file-status))


(DEFFLAVOR ftp-character-stream-mixin
	   ()
	   (ftp-data-stream-mixin)) 


(DEFMETHOD (ftp-character-stream-mixin :element-type) ()
  'STRING-CHAR) 


(DEFFLAVOR ftp-binary-stream-mixin
	   ()
	   (ftp-data-stream-mixin)) 

;1;; >>> REMOVED :SET-BYTE-SIZE METHOD -- REQUIRE THIS TO BE DONE AT OPEN TIME*


(DEFMETHOD (ftp-binary-stream-mixin :element-type) ()
  'unsigned-byte) 


(DEFFLAVOR ftp-input-character-stream-mixin
	   ()
	   (ftp-input-stream-mixin
	    ftp-character-stream-mixin))


(DEFFLAVOR ftp-input-binary-stream-mixin
	   ()
	   (ftp-input-stream-mixin
	    ftp-binary-stream-mixin)) 


(DEFFLAVOR ftp-input-signed-binary-stream-mixin
	   (current-byte-size)
	   (ftp-input-stream-mixin
	    ftp-binary-stream-mixin)
  (:required-flavors si:basic-buffered-input-stream)
  :inittable-instance-variables) 


(DEFMETHOD (ftp-input-signed-binary-stream-mixin :after :init) (IGNORE)
  (SETQ current-byte-size (GETF si:property-list :byte-size))) 


(DEFMETHOD (ftp-input-signed-binary-stream-mixin :element-type) ()
  `(signed-byte ,current-byte-size)) 


(DEFMETHOD (ftp-input-signed-binary-stream-mixin :around :tyi) (cont mt args &rest ignore)
  (LET ((BYTE (AROUND-METHOD-CONTINUE cont mt args)))
    (WHEN byte
      (IF (LDB-TEST (BYTE 1 (1- current-byte-size)) byte)
	  (- byte (LSH 1 current-byte-size))
	  byte)))) 


(DEFMETHOD (ftp-input-signed-binary-stream-mixin :string-in) (eof string &optional (start 0) end)
  (OR end (SETQ end (ARRAY-TOTAL-SIZE string)))
  (LOOP while (< start end) while
	(LOOP until (AND si::stream-input-buffer (< si::stream-input-index si::stream-input-limit))
						;1Out of input, get some more*
	      until (SEND self :setup-next-input-buffer) do
	      (AND eof (FERROR 'end-of-file-1 "End of file on ~S." self)) return () finally (RETURN t))
	as amt = (MIN (- end start) (- si::stream-input-limit si::stream-input-index)) do
	(COPY-ARRAY-PORTION si::stream-input-buffer si::stream-input-index
			    (SETQ si::stream-input-index (+ si::stream-input-index amt)) string
			    start (SETQ start (+ start amt)))
	;1; Sign-extend each byte.*
	(DO ((i start (1+ i))
	     (end1 (+ start amt)))
	    ((= i end1))
	  (LET ((BYTE (AREF string i)))
	    (IF (LDB-TEST (BYTE 1 (1- current-byte-size)) byte)
		(SETF (AREF string i) (- byte (LSH 1 current-byte-size))))))
	finally (AND (ARRAY-HAS-LEADER-P string) (STORE-ARRAY-LEADER start string 0))
	(RETURN (VALUES start (NULL si::stream-input-buffer))))) 


(DEFFLAVOR ftp-input-phony-character-stream-mixin
	   ()
	   (ftp-input-stream-mixin
	    ftp-character-stream-mixin)) 


(DEFMETHOD (ftp-input-phony-character-stream-mixin :element-type) ()
  'global:character) 


(DEFMETHOD (ftp-input-phony-character-stream-mixin :around :tyi) (cont mt args &rest ignore)
  (LET ((ch1 (AROUND-METHOD-CONTINUE cont mt args))
	(ch2 (OR (AROUND-METHOD-CONTINUE cont mt args) 0))
	(ch3 (OR (AROUND-METHOD-CONTINUE cont mt args) 0))
	(ch4 (OR (AROUND-METHOD-CONTINUE cont mt args) 0)))
    (WHEN ch1
      (DPB ch4 (BYTE 8 24) (DPB ch3 (BYTE 8 16) (DPB ch2 (BYTE 8 8) ch1)))))) 


(DEFFLAVOR ftp-output-character-stream-mixin
	   ()
	   (ftp-output-stream-mixin
	    ftp-character-stream-mixin)) 


(DEFFLAVOR ftp-output-phony-character-stream-mixin
	   ()
	   (ftp-output-binary-stream-mixin)) 


(DEFMETHOD (ftp-output-phony-character-stream-mixin :element-type) ()
  'global:character) 


(DEFMETHOD (ftp-output-phony-character-stream-mixin :around :tyo) (cont mt args char)
  args
  (FUNCALL-WITH-MAPPING-TABLE cont mt :tyo (LDB (BYTE 8 0) char))
  (FUNCALL-WITH-MAPPING-TABLE cont mt :tyo (LDB (BYTE 8 8) char))
  (FUNCALL-WITH-MAPPING-TABLE cont mt :tyo (LDB (BYTE 8 16) char))
  (FUNCALL-WITH-MAPPING-TABLE cont mt :tyo (LDB (BYTE 8 24) char))) 


(DEFFLAVOR ftp-output-binary-stream-mixin
	   ()
	   (ftp-output-stream-mixin
	    ftp-binary-stream-mixin)) 


;1;; >>> QFILE had different versions of character/binary send-pkt-buffer ???*

;1;; The following are the instantiable flavors*


(DEFFLAVOR ftp-input-character-stream
	   ()
	   (ftp-input-character-stream-mixin
	    ip:ascii-translating-input-stream-mixin
	    si:input-file-stream-mixin
	    ip:basic-input-stream
	    si:buffered-input-character-stream)) 

;1;; ELEMENT-TYPE = character (?)*

(DEFFLAVOR ftp-input-phony-character-stream
	   ()
	   (ftp-input-phony-character-stream-mixin
	    si:input-file-stream-mixin
	    ip:binary-input-stream-mixin
	    si:buffered-tyi-input-stream)) 


(DEFFLAVOR ftp-output-character-stream
	   ()
	   (ftp-output-character-stream-mixin
	    ip:ascii-translating-output-stream-mixin
	    si:output-file-stream-mixin
	    ip:basic-output-stream
	    si:buffered-output-character-stream)) 

;1;; ELEMENT-TYPE = character (?)*

(DEFFLAVOR ftp-output-phony-character-stream
	   ()
	   (ftp-output-phony-character-stream-mixin
	    si:output-file-stream-mixin
	    ip:binary-output-stream-mixin
	    si:buffered-tyo-output-stream)) 


(DEFFLAVOR ftp-input-binary-stream
	   ()
	   (ftp-input-binary-stream-mixin
	    si:input-file-stream-mixin
	    ip:binary-input-stream-mixin
	    si:buffered-input-stream)) 

;1;; rla 10/24/85 - for byte-sizes other than 16*

(DEFFLAVOR ftp-input-image-stream
	   ()
	   (ftp-input-binary-stream-mixin
	    si:input-file-stream-mixin
	    ip:character-input-stream-mixin
	    si:buffered-input-stream)) 

;1;; ELEMENT-TYPE = signed-byte (?)*

(DEFFLAVOR ftp-input-signed-binary-stream
	   ()
	   (ftp-input-signed-binary-stream-mixin
	    si:input-file-stream-mixin
	    ip:binary-input-stream-mixin
	    si:basic-buffered-input-stream)) 


(DEFFLAVOR ftp-input-signed-image-stream
	   ()
	   (ftp-input-signed-binary-stream-mixin
	    si:input-file-stream-mixin
	    ip:character-input-stream-mixin
	    si:basic-buffered-input-stream)) 


(DEFFLAVOR ftp-output-binary-stream
	   ()
	   (ftp-output-binary-stream-mixin
	    si:output-file-stream-mixin
	    ip:binary-output-stream-mixin
	    si:buffered-output-stream)) 


(DEFFLAVOR ftp-output-image-stream
	   ()
	   (ftp-output-binary-stream-mixin
	    si:output-file-stream-mixin
	    ip:character-output-stream-mixin
	    si:buffered-output-stream)) 


(DEFFLAVOR ftp-probe-stream
	   ()
	   (ftp-probe-stream-mixin
	    ip:character-input-stream-mixin
	    si:buffered-input-character-stream)) 	


(COMPILE-FLAVOR-METHODS ftp-input-character-stream ftp-input-binary-stream
   ftp-input-signed-binary-stream ftp-input-phony-character-stream ftp-output-character-stream
   ftp-output-binary-stream ftp-output-phony-character-stream ftp-probe-stream) 


(COMPILE-FLAVOR-METHODS ftp-input-image-stream ftp-input-signed-image-stream
   ftp-output-image-stream) 

;1;;---- FTP-OPEN *
(DEFUN ftp-open (PATHNAME &rest options &key (direction :input) (characters :default)
		 (element-type 'STRING-CHAR element-type-p) (BYTE-SIZE :default)
		 (if-exists
		   (WHEN (EQ direction :output)
		     (IF (MEMBER (PATHNAME-VERSION pathname) '(:newest :unspecific) :test #'EQ)
			 :new-version
			 :error)))
		 (if-does-not-exist
		   (SELECT direction ((:input nil) :error)	;1here is where nil and :probe differ*
			   (:output :create) (:otherwise nil)))
		 ;1; don't signal by default for a probe stream*
		 (ERROR (NOT (MEMBER direction '(:probe :probe-directory) :test #'EQ)))	;1nil,probe differ here (QFILE sets t)*
		 estimated-length
		 ;1; New keys:*
		 character-format		;1 :ascii / :ebcdic / (:ascii/:ebcdic :non-print/:telnet/:carriage-control)*
		 (file-structure :file)		;1 :file / :record / :page*
		 maximum-record-length		;1 must also give estimated-length to use this parm*
		 ;1; Keys ignored:*
		 preserve-dates deleted		;1zwei uses these*
		 temporary
		 ;1; Keys not allowed:       ACCESS-ERROR,  RAW, SUBMIT, FLAVOR, LINK-TO, ... ??? *
		 &aux phony-characters sign-extend-bytes (aborted t)
		 control-connection pseudo-direction ftp-character-format (file-format nil) truename file-did-exist
		 ftp-stream properties open-pathname)
  (DECLARE (SPECIAL *ftpint-debug*))
  (IGNORE preserve-dates deleted temporary)
  ;1;; /////////////// DEBUG*
  (WHEN *ftpint-debug*
    (PRINT
      (FORMAT () "Open ~A Direction ~A If-exists ~A If-does-not-exist ~A"
	      pathname direction if-exists if-does-not-exist)))
  ;1;;///////////////*
  ;1; Just in case *
  (SETQ pathname (MERGE-PATHNAMES (TRANSLATED-PATHNAME pathname)))
  ;1;-----------------------------------------------------------------------*
  ;1; ------ PARAMETER VALIDATION*
  ;1;-----------------------------------------------------------------------*
  (CCASE direction				;1prompts user if not one of listed values*
    ((:input :output) (SETQ pseudo-direction direction))
    (:probe-directory (SETQ pseudo-direction ()))
    ((nil :probe) (SETQ pseudo-direction ()))
    ((:io :probe-link) (FERROR 'ftp-error "Option ':DIRECTION :~A' not supported by FTP" direction)))
  (CCASE if-does-not-exist
    (:create
     (WHEN (EQ direction :input)
       (FERROR 'ftp-error "Option ':IF-DOES-NOT-EXIST :Create' not legal for input using FTP")))
    (:error
     (WHEN (EQ direction :output)
       (FERROR 'ftp-error "Option ':IF-DOES-NOT-EXIST :Error' not legal for output using FTP")))
    (nil))					;1legal option*
  (WHEN (AND if-exists (EQ direction :input))
    (FERROR 'ftp-error "Option ':IF-EXISTS' not legal for input using FTP"))
  (CCASE if-exists
    ((:rename :rename-and-delete)		;1zmacs uses on replace of existing file*
     (SETQ if-exists :rename))
    (:append
     (WHEN (NEQ direction :output)
       (FERROR 'ftp-error "Option ':IF-EXISTS :append' legal only for output"))
     (SETQ pseudo-direction :append))
    (:supersede (SETQ if-exists :truncate))
    ((:overwrite :error :truncate :new-version nil)))	;1other legal options*
  ;1; This is a QFILE routine used as-is*
  (WHEN element-type-p
    (SETF (VALUES characters byte-size phony-characters sign-extend-bytes)
	  (decode-element-type element-type byte-size)))
  (CCASE characters
    (:default					;1used by fs-utilities (e.g. dired)*
     (IF character-format
	 ;1; THEN assume  characters if character-format specified*
	 (SETQ characters t)
	 ;1; ELSE check file type*
	 (SETQ characters
	       (NOT
		 (MEMBER (SEND pathname :canonical-type) *copy-file-known-binary-types* :test
			 #'EQUAL)))))
    ((t nil)))
  ;1;-----------------------------------------------------------------------*
  ;1; ----- FTP INTERACTION*
  ;1;-----------------------------------------------------------------------*
  (ip:with-stream-whostate
    "Open"
    (UNWIND-PROTECT				;1make sure connection gets closed*
	(file-operation-retry
	  ;1; Move this up here in case byte-size property is returned *
	  (UNLESS (EQ direction :probe-directory)
	    ;1; truename may be returned as a condition object*
	    (MULTIPLE-VALUE-SETQ (truename file-did-exist properties)
	      (derive-truename pathname pseudo-direction if-exists if-does-not-exist error)))
	  (WHEN (EQ byte-size :default)
	    (SETQ byte-size (OR (GETF properties :byte-size)
				(IF characters
				    8
				    16))))
	  (IF character-format
	      ;1; THEN*
	      (PROGN
		(IF (NOT characters)
		    (FERROR 'ftp-error "Option ':CHARACTER-FORMAT ~A' not valid for binary file" character-format))
		(COND ((CONSP character-format)
		       (SETQ file-format (SECOND character-format))
		       (SETQ ftp-character-format (CAR character-format)))
		      (t (SETQ ftp-character-format character-format)))
		(CCASE ftp-character-format		;1 (changed above)*
		  ((:ascii :ebcdic))
		  (nil (SETQ ftp-character-format :ascii)))
		(ccase file-format
		  ((:non-print :telnet :carriage-control))
		  (nil (SETQ file-format :non-print))))
	      ;1; ELSE*
	      (IF characters
		  (PROGN
		    ;1; then*
		    (SETQ ftp-character-format :ascii)
		    (SETQ file-format :non-print))
		  ;1; else*
		  (SETQ ftp-character-format
			(COND
			  ((EQL byte-size 16)
			   (IF (ftp-omit-byte-size pathname)
			       :image
			       :byte-size))
			  ((EQL byte-size 8) :image)
			  (:otherwise (FERROR 'ftp-error "Byte sizes other than 8 and 16 are not supported")))))) ;1; end if*
	  (WHEN (AND characters (NEQ byte-size 8))
	    (FERROR 'ftp-error "Byte sizes other than 8 are not supported for character files"))
	  (CCASE file-structure			;1verify legal values*
	    ((:file :record :page)))
	  (WHEN maximum-record-length
	    (UNLESS estimated-length
	      (FERROR 'ftp-error
		      "MAXIMUM-RECORD-LENGTH not allowed unless ESTIMATED-LENGTH is also specified")))
	  ;1; Establish the connection (do after truename derivation, can re-use connection)*
	  (UNLESS control-connection
	    (SETQ control-connection (reserve-ftp-connection (SEND pathname :host))))	;1error will be signalled*
	  (IF (EQ direction :probe-directory)
	      ;1; then*
	      ;1; special case probe-directory: list it to see if its there and then set truename to be just directory part*
	      ;1; (This is a lot of work but I don't see how else to do it; It also doesn't guarantee anything if FTP Server*
	      ;1;  returns empty list for a non-existent directory)*
	      (UNWIND-PROTECT
		  (UNLESS
		    (ftp-unfavorable
		      (ip:with-stream-whostate
			"File data connection"
			(SEND control-connection :open
			      (dir-pathstring-for-ftp
				(SEND pathname :new-pathname :name :wild :type :wild :version :wild))
			      :nlist)))
		    (SETQ file-did-exist t)
		    (SETQ truename (SEND pathname :new-pathname :name () :type () :version ())))
		(SEND control-connection :close :abort))
	      ;1; else*
	      (PROGN
		(IF (EQ direction :output)
		    (SETQ open-pathname truename)	;1need for bumped version number*
		    (SETQ open-pathname pathname))	;1don't trust our truename derivation unless necessary*
		;1; ////////// DEBUG*
		(WHEN *ftpint-debug*
		  (PRINT (FORMAT () "Truename = ~A; File-did-exist = ~A" truename file-did-exist)))
		;1; //////////*
		;1; Do some behind-the-scene manipulations (since FTP doesn't handle these options)*
		(WHEN file-did-exist
		  (SELECT if-exists
		    (:truncate (SEND control-connection :delete (pathstring-for-ftp pathname)))
		    (:rename
		     (LET ((bp (bastardize-filename pathname)))
		       ;1; If we can't bump version number, delete old backup file (if exists)*
		       (WHEN (NEQ (SEND bp :version) :newest)
			 (SEND control-connection :delete (pathstring-for-ftp bp)))	;1ignore error *
		       (WHEN (ftp-unfavorable
			       (SEND control-connection :rename (pathstring-for-ftp truename)
				     (pathstring-for-ftp bp)))
			 (FERROR 'ftp-error "Unable to rename ~A to ~A before opening" pathname bp))))))))
	  ;1;-----------------------------------------------------------------------*
	  ;1; ----- STREAM CONSTRUCTION*
	  ;1;-----------------------------------------------------------------------*
	  ;1; issue commands to ftp (control-connection), stopping if an error is encountered*
	  (LET ((ip:*tcp-stream-instantiator*
		  ;1; ip:make-stream will use this to instantiate the data-connection*
		  #'(lambda (&optional connection timeout input-buffer-size number-of-input-buffers &aux property-list)
		      ;1; do the best we can*
		      (SETF property-list
			    (NCONC
			      (LIST :truename truename :byte-size byte-size ;1;overrides dir-list byte-size in case this is different*
				    ;1;:length 0*	1;set this to 0 even if we know :length-in-bytes (this is the remote length*
						;1and due to ascii-translation it can be off (see :who-line-information)*
				    ;1;The above is correct for Character file, but doesn't*
				    ;1;apply to binary streams since there is no ascii-translation*
				    :length (if characters
						0
						(if (and (= 16. byte-size)
							 (member (type-of truename)
								 '(unix-ucb-pathname unix-pathname MSDOS-PATHNAME)))
						         (let ((l-i-b (getf properties :length-in-bytes 0)))
							       (if (and l-i-b (= l-i-b 0))
								   0
								   (setf (getf properties :length-in-bytes 0)
									 (/ (getf properties :length-in-bytes 0) 2))
								   (getf properties :length-in-bytes 0)))
							 (getf properties :length-in-bytes 0)))
				    :characters characters
				    ;1; These are new *
				    :character-format ftp-character-format
				    :file-format file-format
				    :file-structure file-structure
				    ;1; don't set qfaslp *
				    )
			      (IF (EQ direction :output)
				  (PROGN
				    (SEND pathname :set-property-list ())
				    ())
				  properties)))	;1include any we know from directory list (unless new file)*
		      (CASE direction
			(:input
			 (MAKE-INSTANCE
			   (ftp-stream-flavor-type direction byte-size characters phony-characters sign-extend-bytes)
			   :control-connection control-connection :property-list property-list
			   :host (SEND pathname :host) :pathname pathname
			   :connection connection :timeout timeout
			   :input-buffer-size input-buffer-size :number-of-input-buffers number-of-input-buffers))
			(:output
			 (MAKE-INSTANCE
			   (ftp-stream-flavor-type direction byte-size characters phony-characters sign-extend-bytes)
			   :control-connection control-connection :property-list property-list
			   :host (SEND pathname :host) :pathname pathname
			   :connection connection :timeout timeout))
			(otherwise
			 (MAKE-INSTANCE
			   (ftp-stream-flavor-type direction byte-size characters phony-characters sign-extend-bytes)
			   :control-connection control-connection :property-list property-list
			   :host (SEND pathname :host) :pathname pathname
			   :connection connection :timeout timeout
			   ;1; workaround Unix bug, allow one byte of file to be sent (holdover from Rel 1.0)*
			   :input-buffer-size 1 :number-of-input-buffers 1))))))
	    (SETF ftp-stream
		  (BLOCK stream-construction
		    ;1; Check existence if user indicated that should be an error*
		    (WHEN (AND (EQ if-exists :error) file-did-exist)
		      (SEND control-connection :set-reply-code ())
		      (RETURN-FROM stream-construction
			(MAKE-CONDITION 'file-already-exists "Open of ~A failed because file already exists"
					truename :open)))
		    ;1; Rel 1.0 commented out this and opened probe stream for input (actually when direction*
		    ;1; was :probe-directory we did return-from stream construction here).  We now return-from at this*
		    ;1; point for all probe streams, to allow (probe-file (send pathname :directory-pathname-as-file)) to*
		    ;1; work over FTP as well as Chaos*
		    (WHEN (NOT pseudo-direction)
		      (COND ((NOT file-did-exist)
			     ;1; here the truename is a condition object*
			     (SEND control-connection :set-reply-code nil)
			     (RETURN-FROM stream-construction truename))
			    (t (RETURN-FROM stream-construction (FUNCALL ip:*tcp-stream-instantiator*)))))
		    ;1; truename is a condition object, return it*
		    (WHEN (ERRORP truename)
		      (SEND control-connection :set-reply-code nil)
		      (RETURN-FROM stream-construction truename))
		    ;1; Describe file to be opened to FTP*
		    ;1; Don't do some of these (try to speed up the open)*
		    (COMMENT (ftp-unfavorable (SEND control-connection :mode :stream)))
		    (WHEN (AND (NEQ file-structure :file)
			       (ftp-unfavorable (SEND control-connection :structure file-structure)))
		      (RETURN-FROM stream-construction nil))
		    (WHEN (ftp-unfavorable
			    (COND
			      (characters
			       (IF (EQ file-format :non-print)
				   (IF (EQ ftp-character-format :ascii)
				       t	;1do nothing - this is the default*
				       ;1; else send only character format (non-print is the default and Apollo croaks on  it)*
				       (SEND control-connection :type ftp-character-format))
				   ;1; else*
				   (SEND control-connection :type ftp-character-format file-format)))
			      ((EQ ftp-character-format :byte-size)
			       (SEND control-connection :type :byte-size byte-size))
			      (:otherwise (SEND control-connection :type :image))))
		      (RETURN-FROM stream-construction nil))
		    (WHEN (AND estimated-length
			       (ftp-unfavorable (SEND control-connection :allocate
						      estimated-length maximum-record-length)))
		      (RETURN-FROM stream-construction nil))
		    ;1; Do the open (use truename for bumped version)*
		    ;1;     Go ahead and always use the truename. Even if truename derivation didn't work*
		    ;1;     real well, at least we won't be lying about the truename of the stream.  Also probe of*
		    ;1;     a directory on the Vax doesn't work without a version number*
		    (ip:with-stream-whostate
		      "File data connection"
		      (LOOP
			(SEND control-connection :open (pathstring-for-ftp truename) pseudo-direction)
			;1; workaround a Unix 425 bug which seems to go away on the second open attempt*
			(UNLESS (AND (EQ 425 (SEND control-connection :reply-code))
				     (EQ :bsd4.2 (SEND *ftp-service* :get-ftp-implementation-type (PATHNAME-HOST pathname)))
				     (SEARCH "Can't create data socket" (SEND control-connection :reply-string))
				     (SEARCH "Address already in use" (SEND control-connection :reply-string)))
			  (RETURN-FROM stream-construction
			    (UNLESS (ftp-unfavorable (SEND control-connection :reply-code))
			      ;1; the ftp data-connection is actually an instantiated ftp-stream*
			      (SEND control-connection :data-connection))))))))
	    )					;1end let ip:*tcp-stream-instantiator**
	  ;1; NOTE: streams opened with direction nil or :probe both return an error condition if :if-does-not-exist is*
	  ;1;  :error or nil otherwise. The difference is in the default for the :if-does-not-exist parm (see header).*
	  (COND ((ftp-unfavorable (SEND control-connection :reply-code))
		 (COND ((NOT (OR pseudo-direction (EQ if-does-not-exist :error)))
			(SETF ftp-stream nil)) ;1; sometimes return nil for a probe-stream*
		       (t
			(SETF ftp-stream
			      (COND ((TYPEP ftp-stream 'condition) ftp-stream)
				    ((AND (EQ pseudo-direction :input)
					  (OR (EQL 450 (SEND control-connection :reply-code))
					      (EQL 550 (SEND control-connection :reply-code))))
				     (LET ((reply-string (SEND control-connection :reply-string))
					   directory-list)
				       (free-ftp-connection (PROG1 control-connection (SETF control-connection nil))
							    nil :abort-data)
				       (COND ((OR (SEARCH "ermission" reply-string) (SEARCH "ERMISSION" reply-string)
						  (SEARCH "rivelege" reply-string) (SEARCH "RIVELEGE" reply-string))
					      (MAKE-CONDITION 'incorrect-access-to-file reply-string pathname :open))
					     (t
					      ;1; directory-list will supply the directory-not-found condition object*
					      (SETF directory-list (SEND *ftp-service* :directory-list
									 :ftp truename '(:noerror)))
					      (IF (ERRORP directory-list)
						  directory-list
						  (MAKE-CONDITION 'fs:file-not-found "File not found for ~a"
								  truename :ftp-open))))))
				    ((AND (EQ pseudo-direction :output)
					  (OR (EQL 450 (SEND control-connection :reply-code))
					      (EQL 553 (SEND control-connection :reply-code))
					      ;1; Unix violates spec by returning 550 on STOR, workaround it*
					      (EQL 550 (SEND control-connection :reply-code))))
				     (LET ((reply-string (SEND control-connection :reply-string))
					   directory-list)
				       (free-ftp-connection (PROG1 control-connection (SETF control-connection nil))
							    nil :abort-data)
				       (COND ((OR (SEARCH "ermission" reply-string) (SEARCH "ERMISSION" reply-string)
						  (SEARCH "rivelege" reply-string) (SEARCH "RIVELEGE" reply-string))
					      (MAKE-CONDITION 'incorrect-access-to-file reply-string pathname :open))
					     (t
					      ;1; directory-list will supply the directory-not-found condition object*
					      (SETF directory-list (SEND *ftp-service* :directory-list
									 :ftp truename '(:noerror)))
					      (IF (ERRORP directory-list)
						  directory-list
						  (MAKE-CONDITION 'ftp-open-error reply-string self :open))))))
				    (t (MAKE-CONDITION 'ftp-open-error
						       (SEND control-connection :reply-string) self :open))))
			(WHEN error (SIGNAL-CONDITION ftp-stream)))))
		(t
		 (IF pseudo-direction
		     (SEND control-connection :set-in-use ftp-stream)	;1nice for display/debug*
		     (SEND ftp-stream :close))
		 (SETF aborted nil)
		 ))				;1 end cond*
	  )					;1 end file-operation-retry*
      (WHEN aborted
	(IF (AND ftp-stream (TYPEP ftp-stream 'ftp-data-stream-mixin))
	    (SEND ftp-stream :close :abort)
	    (WHEN control-connection (free-ftp-connection control-connection nil :abort-data))))))	;1 end unwind-protect *
  ;1; /////// DEBUG*
  (WHEN *ftpint-debug*
    (PRINT ftp-stream))
  ;1; ////////*
  ftp-stream					;1return value*
  )
 


(DEFUN ftp-stream-flavor-type (direction byte-size characters phony-characters sign-extend-bytes)
  (CASE direction
	(:input
	 (IF characters
	     'ftp-input-character-stream
	     (COND
	       (sign-extend-bytes
		(IF (EQL byte-size 16)
		    'ftp-input-signed-binary-stream
		    'ftp-input-signed-image-stream))
	       (phony-characters 'ftp-input-phony-character-stream)
	       (t (IF (EQL byte-size 16)
		      'ftp-input-binary-stream
		      'ftp-input-image-stream)))))
	(:output
	 (IF characters
	     'ftp-output-character-stream
	     (IF phony-characters
		 'ftp-output-phony-character-stream
		 (IF (EQL byte-size 16)
		     'ftp-output-binary-stream
		     'ftp-output-image-stream))))
	(t 'ftp-probe-stream))) 


(DEFVAR *ftp-bypass-truename* nil
  "2A flag to avoid complicated truename derivation if you really don't care about
   truenames and  properties*")

(DEFUN derive-truename (pathname direction if-exists-option if-does-not-exist-option error-p
			&aux version truename file-did-exist
			fixed-version vnum check-existence property-list)
  (BLOCK derive-truename
    (WHEN *ftp-bypass-truename* 
      (RETURN-FROM derive-truename pathname (NOT (EQ :direction :output))  nil))
    ;1; Note that :unspecific means that files are not numbered, while nil means that no version was specified*
    (IF (AND
	  (SETQ fixed-version
		(OR (EQ (SETQ version (SEND pathname :version)) :unspecific) (NUMBERP version)))
	  (EQ direction :output)
	  (NOT (SETQ check-existence (MEMBER if-exists-option '(:error :rename) :test #'EQ))))
	;1; then don't need to do truename operation or find properties*
	;1; (Note that even if properties have been saved with the pathname, they may not be up to date.  Go ahead*
	;1;  and call describe-file in that case - it will do the most efficient thing possible) *
	(VALUES pathname t property-list)	;1go ahead and say it exists for truncate*
	;1; else derive truename &/or properties (from directory-list, etc)*
	(PROGN
	  (MULTIPLE-VALUE-SETQ (truename property-list) (describe-file pathname))
	  (COND ((ERRORP truename)
		 (WHEN (AND error-p (EQ if-does-not-exist-option :error)
			    (OR (CONDITION-TYPEP truename 'file-not-found)
				(CONDITION-TYPEP truename 'directory-not-found)))
		   (SIGNAL-CONDITION truename))
		 ;1; file did not previously exist (we think)*
		 (WHEN (CONDITION-TYPEP truename 'file-not-found)
		   (IF (OR fixed-version (NEQ direction :output))
		       ;1; If direction input, we will get an error later unless the file didn't appear*
		       ;1; in the directory list for some reason but we are still able to open it.*
		       ;1; In that case, don't assume something we don't know (i.e. version #1)*
		       ;1; THEN give back what we got (number or unspecific)*
		       (SETQ truename pathname)
		       ;1; ELSE numbered files (and no number specified) - open first version of new file*
		       (SETQ truename (SEND pathname :new-pathname :version 1)))))
		(t
		 ;1; file did exist *
		 ;1;      If truename has version :newest, then wollongong didn't list the version number for :newest and*
		 ;1;      describe-file gave up before it found it.  Let it be non-existent in some cases in the sense*
		 ;1;      that it is supersedable.  This avoids some unnecessary open errors.*
		 (SETQ file-did-exist
		       (NOT (AND (EQ (SETQ vnum (SEND truename :version)) :newest) check-existence)))
		 (WHEN (AND (EQ if-exists-option :new-version) (OR (NULL version) (EQ version :newest))
			    (NUMBERP vnum))
		   ;1; bump the version number*
		   (SETQ truename (SEND truename :new-pathname :version (1+ vnum))))))
	  (VALUES truename file-did-exist property-list))))) 


;1;; 11/14/85 - RLA  *** THIS FUNCTION IS NOW OBSOLETE ****

(DEFUN nlist-match (PATHNAME control-connection &aux new-pathstring new-truename best-truename delta best-delta ov
		    nv)
  ;1; Look for the best match between the original pathstring and those returned by the directory listing.*
  ;1; (This is necessary primarily because at least one foreign FTP always lists the entire directory)*
  (UNWIND-PROTECT (UNLESS
		    (ftp-unfavorable
		      (SEND control-connection :open (dir-pathstring-for-ftp pathname) :nlist))
		    (LOOP with stream = (SEND control-connection :data-connection) while
			  (SETQ new-pathstring (READ-LINE stream ())) do
			  (SETQ new-truename (nlist-strip-and-parse pathname new-pathstring))
			  (WHEN (AND new-truename (EQUAL (SEND pathname :name) (SEND new-truename :name)))
			    (IF (EQUAL (SETQ ov (SEND pathname :type)) (SEND new-truename :type))
				(SETQ delta 1)
				;1; else*
				(IF (OR (NULL ov) (EQ ov :unspecific))
				    (SETQ delta 2)
				    (SETQ delta ())))	;1don't ever match if src type is concrete and does not match*
			    (WHEN delta
			      (IF (OR
				    (EQUAL (SETQ ov (SEND pathname :version))
					   (SETQ nv (SEND new-truename :version)))
				    (AND (NULL ov) (EQ nv :newest)))
				  (DECF delta)
				  ;1; else*
				  (WHEN (AND (NUMBERP ov) (NUMBERP nv))
				    (SETQ delta ()))))	;1don't ever match if both versions are concrete and no match*
			    (WHEN (AND delta (OR (NULL best-delta) (<= delta best-delta)))
			      (SETQ best-delta delta)
			      (SETQ best-truename new-truename)))))
    (SEND control-connection :close :abort))
  best-truename					;1last match*
  ) 



(DEFUN bastardize-filename (pathobj &aux filename sl)
  ;1; Take a file name and create a variation by replacing the last four characters by an arbitrary string.*
  ;1; If the file name is less than four characters, at least two leading characters will be retained.  *
  ;1; This routine makes the assumption that any file system allows names to be at least six characters long*
  ;1; and contain capital letters.*
  (SETQ filename (SEND pathobj :name))
  (WHEN (> (SETQ sl (LENGTH filename)) 2)
    (SETQ filename (SUBSEQ filename 0 (MAX 2 (- sl 4)))))
  ;1; for file systems with versioning, use :newest*
  (IF (EQ (SEND pathobj :version) :unspecific)
      (SEND pathobj :new-pathname :name (STRING-APPEND filename "ZBAK"
						       ))
      (SEND pathobj :new-pathname :name (STRING-APPEND filename "ZBAK"
						       ) :version :newest))) 



(DEFPARAMETER *ftp-newest-search-limit* 32
  "2The number of versions past the previous version that we will attempt to open to
   establish a truename for :newest when that is not returned in the directory list*") 

;1; Now also called from :properties method of ftp-pathname*

(DEFUN describe-file (pathname &aux previous truename properties retry-ok found path-version dir-list)
  ;1; For pathname objects where properties are known and version is >, use the :previous property as*
  ;1; a starting point for determining the version number, but be suspicious of the validity of the property list*
  ;1; since the > version number may have changed.  After a successful open, make sure that the successor*
  ;1; cannot be opened.  Otherwise dump the property list and call recursively (once) to really do a directory list.*
  ;1; Do not rely on properties stored with pathnames which have no version number.*
  (IF (AND (NEQ (SETQ path-version (SEND pathname :version)) :unspecific)
	   (SETQ properties (SEND pathname :property-list))
	   (ftp-probe pathname))
      ;1; make sure file hasn't been deleted since properties were obtained*
      ;1; THEN case where properties are already known*
      (PROGN
	(SETQ truename pathname)
	(SETQ retry-ok t))
      ;1; ELSE do a directory list*
      (WHEN (AND (CONSP (SETQ dir-list (SEND *ftp-service* :directory-list :ftp pathname '(:noerror))))
		 (SETQ truename (COND ((GETF (CDR (SECOND dir-list)) :link-to))
				      (t (CAR (SECOND dir-list))))))
	(SETQ properties (SEND truename :property-list))))
  (COND
    ((NULL truename)
     (VALUES (IF (ERRORP dir-list)
		 dir-list
		 (MAKE-CONDITION 'fs:file-not-found "File not found for ~a"
				 pathname :describe-file))))
    ((OR (EQ (SEND truename :version) :unspecific) (NUMBERP (SEND truename :version)))
     (VALUES truename properties))
    ;1; got back :newest from directory-list (not specific enough)*
    ((NUMBERP path-version)			;1asked for a number - this may or may not be it*
     (WHEN (ftp-probe pathname)
       (SEND pathname :set-property-list properties)
       (SEND truename :putprop (1- path-version) :previous)	;1keep anything we know! *
       ;1; make sure we give back the one with the number in it (more specific)*
       (VALUES pathname properties)))
    (:otherwise					;1try to figure version # for :newest*
     ;1; (directory-list leaves :previous property for us )*
     (UNLESS (SETQ previous (GETF properties :previous))
       (SETQ previous 0))
     (MULTIPLE-VALUE-SETQ (TRUENAME found)
       (ftp-probe-newest pathname previous *ftp-newest-search-limit*))
     (WHEN found
       (IF (NOT (ftp-probe (SEND pathname :new-version (1+ found))))
	   ;1; THEN got it (save info for next time)*
	   (PROGN
	     (SEND pathname :putprop (1- found) :previous)
	     (SEND truename :set-property-list properties))
	   ;1; ELSE the first success after :previous wasn't really the > (changed since last time properties were set)*
	   (PROGN
	     (SEND pathname :set-property-list ())
	     (WHEN retry-ok
	       (MULTIPLE-VALUE-SETQ (TRUENAME properties)
		 (describe-file pathname))))))
     (VALUES truename properties))))


(DEFUN ftp-probe (PATHNAME &optional open-cc &aux cc result)
  (UNWIND-PROTECT
      (PROGN
	(SETQ cc (OR open-cc (reserve-ftp-connection (SEND pathname :host) :ftp-probe)))
	(SETQ result (NOT (ftp-unfavorable (SEND cc :open (pathstring-for-ftp pathname) :input)))))
    ;1; CLEAN-UP*
    (IF open-cc
	(SEND open-cc :close :abort)
	;1; else*
	(free-ftp-connection cc () :abort-data-only)))
  result) 



(DEFUN ftp-probe-newest (PATHNAME previous search-limit &optional open-cc &aux cc truename found)
  (UNWIND-PROTECT
      (PROGN
	(SETQ cc (OR open-cc (reserve-ftp-connection (SEND pathname :host) :probe-newest)))
	(LOOP					;1 trying to open with successively bumped version numbers*
	  for i from (1+ previous) to (+ previous search-limit) do
	  (SETQ truename (SEND pathname :new-version i))
	  (WHEN (ftp-probe truename cc)
	    (RETURN (SETQ found i)))
	  finally (RETURN (SETQ truename pathname))))	;1give them back what we got *
    (UNLESS open-cc
      (free-ftp-connection cc)))
  (VALUES truename found)) 
