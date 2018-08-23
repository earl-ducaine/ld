;;; -*- Mode:Common-Lisp; Package:File-System; Base:10; Fonts:(Cptfont Hl12b Hl12bi) -*-

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

;1;;=================================================================================*
;1;;                        F T P    F I L E   S Y S T E M   I N T E R F A C E*
;1;;*
;1;;  HISTORY:*
;1;;     6/85   - original (adapted from QFILE) [RLA]*
;1;;     8/86   - converted to Common Lisp [LR]*
;1;;    10/86   - modified for GENI (release 3.0)  [RLA]*
;1;;   11.19.87 DAB - Fixed bsd4.2-directory-line-parse to attempt to determine if the file is binary.*
;1;;            If so divide :length by 2 to correct for UNIX unsigned 8 bit bytes and Explorers 16 bit. [6887]*
;1;;   12.16.87 DAB - Removed code done on  11.19.87. Will be done in FTPopen instead.*
;1;;   01-26-88 DAB - Fixed backup-token. It would error off if LINE did not begin with a SPACE.*
;1;;   02-01-88 DAB - added *new-line-delimiter* to (FTP :case :directory-list-from-ftp :bsd4.2) to comply with spec.*
;1;;==================================================================================*


(DEFFLAVOR ftp
	   ((host:name :ftp)
	    (host:desirability .30))
	   (host:service-implementation-mixin)
  (:method-combination (:case :base-flavor-last :directory-list-from-ftp)
                       (:case :base-flavor-last :parse-directory-entry)
		       (:case :base-flavor-last :get-ftp-implementation-type-internal)
                       (:case :base-flavor-last :string-for-ftp)
		       (:case :base-flavor-last :login-user-internal)
		       (:case :base-flavor-last :omit-byte-size))
  :gettable-instance-variables
  :settable-instance-variables    
  :initable-instance-variables)

(host:define-service-implementation 'FTP)

(DEFVAR *ftp-service* nil)
(DEFVAR *ftpint-debug* ())


;1;;-----------------------------*
;1;; --- FILE ACCESS METHODS ---*
;1;;*
;1;; ADVERTISED METHODS:*
;1;;        :OPEN*
;1;;        :TRUENAME*
;1;;        :RENAME*
;1;;        :DELETE-AND-EXPUNGE*
;1;;        :DELETE-AND-EXPUNGE-MULTIPLE-FILES*
;1;;        :HOMEDIR*
;1;;        :CREATE-DIRECTORY*
;1;;        :PROPERTIES*
;1;;        :SET-PROPERTIES-P*
;1;;        :GET-PROPERTIES-P*
;1;;        :ALL-DIRECTORIES*
;1;;        :COMPLETE-STRING*
;1;;        :DIRECTORY-LIST*
;1;;        :DIRECTORY-STREAM*
;1;;-----------------------------*

(DEFMETHOD (FTP :open) (medium pathname options)
  (DECLARE (IGNORE medium))
  (ip:with-stream-whostate
    "Open"
    (LET ((ftp-stream (APPLY 'ftp-open pathname options)))
      ;1; If the result is a data-stream (direction <> nil), add to list of open streams kept with host *
      (WHEN (AND (STREAMP ftp-stream) (SEND ftp-stream :direction))
	(SEND (net:translated-host (PATHNAME-HOST pathname)) :register-stream ftp-stream))
      ftp-stream)))				;1return value (stream, condition, or nil (unsuccessful probe))*

(DEFMETHOD (FTP :truename) (medium pathname &optional (error-p t) &aux result)
  (IGNORE medium)
  (ip:with-stream-whostate
    "Truename"
    (file-operation-retry
      (SETF result (describe-file pathname))
      (IF (AND (ERRORP result) error-p)
	  (SIGNAL-CONDITION result)
	  result))))

(DEFMETHOD (FTP :rename) (medium pathname new-pathname &optional (error-p t) &aux ftp finished-p ftp-result)
  (ip:with-stream-whostate
    "Rename"
    (file-operation-retry
      (UNWIND-PROTECT
	  (PROGN
	    ;1; insure the destination directory exists (allow directory creation with condition handlers)*
	    (SETF ftp-result (SEND self :directory-list medium new-pathname))
	    ;1; with ftp the rename operation takes two steps*
	    (UNLESS (ERRORP ftp-result)
	      (SETQ ftp (reserve-ftp-connection (SEND pathname :host) :rename)) ;1; last arg is nicety for peek*
	      (SEND ftp :quote (FORMAT () "RNFR ~a" (pathstring-for-ftp pathname)))
	      (SETF ftp-result (COND ((OR (EQL 450 (SEND ftp :reply-code))
					  (EQL 550 (SEND ftp :reply-code)))
				      (LET ((reply-string (SEND ftp :reply-string))
					    directory-list)
					(free-ftp-connection (PROG1 ftp (SETF ftp nil)) nil :abort-data)
					(COND ((OR (SEARCH "ermission" reply-string) (SEARCH "ERMISSION" reply-string)
						   (SEARCH "rivelege" reply-string) (SEARCH "RIVELEGE" reply-string))
					       (MAKE-CONDITION 'incorrect-access-to-file reply-string pathname :rename))
					      (t
					       ;1; directory-list will supply the directory-not-found condition object*
					       (SETF directory-list (SEND *ftp-service* :directory-list
									  :ftp pathname '(:noerror)))
					       (IF (ERRORP directory-list)
						   directory-list
						   (MAKE-CONDITION 'fs:file-not-found "File not found for ~a"
								   pathname :rename))))))
				     ((EQL 350 (SEND ftp :reply-code)) 350)
				     (t (check-ftp-status pathname ftp nil))))
	      (UNLESS (ERRORP ftp-result)
		(SEND ftp :quote (FORMAT () "RNTO ~a" (pathstring-for-ftp new-pathname)))
		;1; the spec does not allow 450 or 550 but Unix may return them*
		(SETF ftp-result
		      (COND ((OR (EQL 450 (SEND ftp :reply-code))
				 (EQL 550 (SEND ftp :reply-code)))
			     (LET ((reply-string (SEND ftp :reply-string))
				   directory-list)
			       (free-ftp-connection (PROG1 ftp (SETF ftp nil)) nil :abort-data)
			       (COND ((OR (SEARCH "ermission" reply-string) (SEARCH "ERMISSION" reply-string)
					  (SEARCH "rivelege" reply-string) (SEARCH "RIVELEGE" reply-string))
				      (MAKE-CONDITION 'incorrect-access-to-file reply-string new-pathname :rename))
				     (t
				      ;1; directory-list will supply the directory-not-found condition object*
				      (SETF directory-list (SEND *ftp-service* :directory-list
								 :ftp new-pathname '(:noerror)))
				      (IF (ERRORP directory-list)
					  directory-list
					  (MAKE-CONDITION 'ftp-open-error reply-string new-pathname :rename))))))
			    (t (check-ftp-status pathname ftp nil)))))
	      (SETF finished-p t)))
	;1; cleanup*
	(WHEN ftp (free-ftp-connection ftp (OR (TYPEP ftp-result 'condition) (NOT finished-p)))))
      (IF (ERRORP ftp-result)
	  ;1; THEN return error condition *
	  (IF error-p (SIGNAL-CONDITION ftp-result) ftp-result)
	  ;1; ELSE return new-truename, T (or some number for chaos version) (second value is ignored I think)*
	  (VALUES (SEND self :truename medium new-pathname) t)))))

(DEFMETHOD (FTP :delete) (medium path &optional (ERRORP t))
  (SEND self :delete-and-expunge medium path errorp))

(DEFMETHOD (FTP :expunge) (medium path &optional (ERRORP t))
  (DECLARE (IGNORE medium path errorp))
  ;1; return blocks freed*
  0)

(DEFMETHOD (FTP :delete-and-expunge) (medium pathname &optional (error-p t) &aux ftp finished-p ftp-result)
  (IGNORE medium)
  (ip:with-stream-whostate
    "Delete-and-expunge"
    (file-operation-retry
      (UNWIND-PROTECT
	  (PROGN
	    (SETQ ftp (reserve-ftp-connection (SEND pathname :host) :delete-and-expunge)) ;1; last arg is nicety for peek*
	    (SEND ftp :delete (pathstring-for-ftp pathname))
	    (SETF ftp-result (COND ((OR (EQL 450 (SEND ftp :reply-code))
					(EQL 550 (SEND ftp :reply-code)))
				    (LET ((reply-string (SEND ftp :reply-string))
					  directory-list)
				      (free-ftp-connection (PROG1 ftp (SETF ftp nil)) nil :abort-data)
				      (COND ((OR (SEARCH "ermission" reply-string) (SEARCH "ERMISSION" reply-string)
						 (SEARCH "rivelege" reply-string) (SEARCH "RIVELEGE" reply-string))
					     (MAKE-CONDITION 'incorrect-access-to-file
							     reply-string pathname :delete-and-expunge))
					    (t
					     ;1; directory-list will supply the directory-not-found condition object*
					     (SETF directory-list (SEND *ftp-service* :directory-list
									:ftp pathname '(:noerror)))
					     (IF (ERRORP directory-list)
						 directory-list
						 (MAKE-CONDITION 'fs:file-not-found "File not found for ~a"
								 pathname :delete-and-expunge))))))
				   (t (check-ftp-status pathname ftp nil))))
	    (SETF finished-p t))
	;1; cleanup*
	(WHEN ftp (free-ftp-connection ftp (OR (TYPEP ftp-result 'condition) (NOT finished-p)))))
      (IF (ERRORP ftp-result)
	  ;1; THEN return error condition *
	  (IF error-p (SIGNAL-CONDITION ftp-result) ftp-result)
	  ;1; ELSE return blocks freed, second value?*
	  1))))

(DEFMETHOD (FTP :delete-multiple-files) (medium files &optional (ERRORP t))
  (SEND self :delete-and-expunge-multiple-files medium files errorp))

(DEFMETHOD (FTP :delete-and-expunge-multiple-files) (medium files &optional (ERRORP t))
  (LOOP for file in files with n-blocks
	do (SETF n-blocks (SEND self :delete-and-expunge medium file errorp))
	when (NUMBERP n-blocks)
	sum n-blocks))

(DEFMETHOD (FTP :homedir) (IGNORE pathname &optional (user user-id))
  (ip:with-stream-whostate
    "Homedir"
    (SEND pathname :quiet-homedir user)))

;1;; Go ahead and try, even though most FTPs don't support this*
(DEFMETHOD (FTP :create-directory) (ignore pathname &optional (error-p t) &aux ftp finished-p result)
  (ip:with-stream-whostate
    "Create-directory"
    (file-operation-retry
      (UNWIND-PROTECT
	  (PROGN
	    (SETQ ftp (reserve-ftp-connection (SEND pathname :host) :create-directory)) ;1; last arg is nicety for peek*
	    (SETF result
		  (IF (ftp-unfavorable (SEND ftp :create-directory
					     (dir-pathstring-for-ftp (SEND pathname :new-pathname
									   :name :wild :type nil :version nil))))
		      (IF (NOT (EQL 550 (SEND ftp :reply-code)))
			  (MAKE-CONDITION 'ftp-error (SEND ftp :reply-string) pathname :create-directory)
			  (COND ((OR (SEARCH "ermission" (SEND ftp :reply-string))
				     (SEARCH "ERMISSION" (SEND ftp :reply-string))
				     (SEARCH "rivelege" (SEND ftp :reply-string))
				     (SEARCH "RIVELEGE" (SEND ftp :reply-string)))
				 (MAKE-CONDITION 'incorrect-access-to-directory (SEND ftp :reply-string)
						 pathname :create-directory))
				((OR (SEARCH "xists" (SEND ftp :reply-string))
				     (SEARCH "XISTS" (SEND ftp :reply-string))
				     ;1; workaround Unix returning two differing strings here*
				     (SEARCH "Is a directory" (SEND ftp :reply-string)))
				 (MAKE-CONDITION 'file-already-exists (SEND ftp :reply-string)
						 pathname :create-directory))
				(t (MAKE-CONDITION 'directory-not-found "Directory not found for ~a"
						   pathname :create-directory))))
		      t))
	    (SETF finished-p t))
	;1; cleanup*
	(free-ftp-connection ftp (OR (TYPEP result 'condition) (NOT finished-p))))
      (IF (AND error-p (TYPEP result 'condition))
	  (SIGNAL-CONDITION result)
	  result))))
  

(DEFMETHOD (FTP :properties) (IGNORE pathname &optional (error-p t) &aux truename properties)
  (ip:with-stream-whostate
    "Properties"
    (file-operation-retry
      ;1; => directory-list-entry, list of settable-properties*
      (MULTIPLE-VALUE-SETQ (truename properties) (describe-file pathname))
      (IF (NOT (ERRORP truename))
	  (VALUES (CONS truename properties) nil)
	  (IF error-p
	      (SIGNAL-CONDITION truename)
	      truename)))))
 

(DEFMETHOD (FTP :set-properties-p) (IGNORE)
  (ip:with-stream-whostate
    "Set-properties"
    nil))					;1for future use by copy-file*

(DEFMETHOD (FTP :get-properties-p) (IGNORE)
  (ip:with-stream-whostate
    "Get-properties"
    nil))					;1ideally this should reflect the kind of directory-list done*

(DEFMETHOD (FTP :multiple-file-plists) (medium files &optional options &aux plist plists)
  (DECLARE (IGNORE options))
  (SETF plists nil)
  (DOLIST (file (REVERSE files) plists)
    (SETF plist (SEND self :properties medium (PATHNAME file) nil))
    (PUSH (CONS file (IF (LISTP plist) (CONS :truename plist)))
	  plists)))

(DEFMETHOD (FTP :all-directories) (medium pathname options &aux ds)
;1; Returns all top-level directories on a host*
  
  (IF (ERRORP
	(SETQ ds
	      (SEND self :directory-stream
		    medium
		    (SEND pathname :new-pathname
			  :directory :root
			  :name :wild 
			  :type (SEND pathname :directory-file-type)
			  :version :wild)
		    options)))
      ;1; then return error condition*
      ds						
      ;1; else format this list like everybody else does*
      (PROGN
	(SEND ds :next-entry)			;1header*
	(LOOP for e = (SEND ds :next-entry) while e 
	      when (GETF (CDR e) :directory)
	      collecting (SEND (CAR e) :new-pathname 
			       :directory (SEND (CAR e) :name) :name :unspecific :type :unspecific :version :unspecific) 
	      ))))
;1;These two functions were moved to IO;OPEN.lisp*
;1(DEFUN leading-substring (substr s chkcase &aux sublen)*
;1  (AND (STRINGP substr) (STRINGP s) (<= (SETQ sublen (LENGTH substr)) (LENGTH s))*
;1       (IF chkcase*
;	1   (STRING= substr s :end2 sublen)*
;	1   (STRING-EQUAL substr s :end2 sublen)))) *
  

;1(DEFUN string-compare-case (s1 s2 chkcase &aux result)*
;1  (IF (NULL (SETQ result (IF chkcase*
;			1     (STRING/= s1 s2)*
;			1     (STRING-NOT-EQUAL s1 s2))))*
;1      0*
;1      (1+ result)))  *
  

(DEFMETHOD (FTP :complete-string) (medium pathname string options
				   &aux ds first-partial partial  (match-list nil)	
;1x                partial-subdir partial-dir-components   previous-subdirectories new-dir *
				   (dir-match t) new-ok nam vers typ case host)
  (SETQ host (PATHNAME-HOST pathname))
  ;1; returns an extra value = matchlist*
  ;1; self provides defaults (for host and directory only)*
  (WHEN (OR (MEMBER :print options :test 'EQ) (MEMBER :write options :test 'EQ)
	    (MEMBER :out options :test 'EQ))
    (SETQ new-ok t))
  (SETQ case (SEND host :send-if-handles :case-sensitive-when-hashing))
  ;1; list directory on host/dvc/dir/wild/wild/wild *
  (SETQ ds
	(SEND self :directory-stream
	      medium
	      (merge-pathname-defaults
		(SEND (SETQ first-partial (parse-pathname string host))
  		      :new-pathname
		      :host host :name :wild :type :wild :version :newest)
		pathname :wild :newest)
	      '(:no-extra-info :noerror)))
  (SETQ partial (merge-pathname-defaults first-partial pathname))
;1---------------------------------------------------------------------------------------*
  (COMMENT
    ;1; DON'T DO PARTIAL DIRECTORY MATCHES (FOR NOW) - PROBLEM WITH GETTING PARENT DIRECTORY*
    ;1; LISTED WITHOUT GETTING THE WORLD*
    ;1; Mimic QFILE: if a directory name is not found as given, use the FIRST one partially matched.*
    (WHEN (ERRORP ds)
      ;1; this time do a dir with directory component wild also*
      (SETQ ds
	    (SEND self
		  :directory-stream
		  medium
		  (SEND pathname :new-pathname :host host :directory :wild :name :wild :type :wild
			:version :wild)
		  '(:no-extra-info :noerror)))
      ;1; Directory components may be a list - cause even a single directory name to be treated that way for now*
      (SETQ partial-dir-components (SEND partial :directory))
      (UNLESS (LISTP partial-dir-components)
	(SETQ partial-dir-components (LIST partial-dir-components)))
      (SETQ previous-subdirectories nil)
      ;1; loop through partial components attempting a match*
      (DOLIST (partial-subdir partial-dir-components)
	(WHEN (ERRORP ds)
	  (RETURN (SETQ dir-match nil)))
	(SEND ds :next-entry)			;1header*
	(UNLESS
	  ;1; try to find a match for individual directory component*
	  (LOOP for e = (CAR (SEND ds :next-entry)) while e do
		;1; notice directory name is file name in higher level dir*
		(WHEN (leading-substring partial-subdir (SEND e :raw-name) case)
		  (IF previous-subdirectories
		      ;1; then*
		      (SETQ newdir
			    (SETQ previous-subdirectories
				  (APPEND previous-subdirectories (LIST (SEND e :raw-name)))))
		      ;1; else*
		      (SETQ previous-subdirectories (LIST (SETQ newdir (SEND e :raw-name)))))
		  (SETQ ds
			(SEND self
			      :directory-stream medium
			      (SEND pathname :new-pathname :host host :directory newdir :name :wild :type
				    :wild :version :wild)
			      '(:no-extra-info :noerror)))
		  (RETURN t)))
	  (RETURN (SETQ dir-match nil)))		;1end unless*
	)					;1end dolist*
      )						;1end when*
    )						;1 end COMMENT*
;1-------------------------------------------------------------------------------------------- *
  ;1; find all file matches*
  (UNLESS (OR (NULL dir-match) (ERRORP ds))
    (SEND ds :next-entry)			;1header*
    (LOOP for e = (CAR (SEND ds :next-entry)) while e do
	  (WHEN (AND
		  ;1; file match if wasn't specified or partial is a leading substring*
		  (OR (NOT (SEND first-partial :raw-name))
		      (leading-substring (SEND partial :raw-name) (SEND e :raw-name) case))
		  ;1; type matches if matches exactly or partial is a leading substring (this is like lm and not qfile)*
		  (OR (NOT (SETQ typ (SEND first-partial :raw-type))) (EQ typ :unspecific)
		      (leading-substring (SEND partial :raw-type) (SEND e :raw-type) case))
		  ;1; version number matches if not specified or specified and matches exactly*
		  (OR (NOT (SETQ vers (SEND first-partial :version))) (EQ vers :unspecific)
		      (EQ vers :newest) (EQUAL (SEND partial :version) (SEND e :version))))
	    (PUSH e match-list))))
  
  ;1; determine return value*
  (COND
    ((NULL match-list)				
;1x      ( (OR (NULL match-list) *
;1x            QFILE returns new pathname even if :new-ok not specified (and editor doesn't)*
;1x           AND (> (LENGTH match-list) 1) (NOT (MEMQ :new-ok options))))*
     (VALUES (STRING-APPEND (SEND host :name-as-file-computer) ":"  string)
	     nil nil))
    ((EQL (LENGTH match-list) 1)
     (IF (SEND first-partial :version)
	 ;1; then include number only if specified (might want to just edit > otherwise)*
	 (VALUES (SEND (CAR match-list) :string-for-printing) :old match-list)
	 ;1; else (clumsily) strip off version number*
	 (VALUES
	   (SEND (SEND (CAR match-list) :new-pathname :version :newest) :string-for-printing) :old
	   match-list)))
    (t
     (LET* ((hdr (CAR match-list)) (hdrname (SEND hdr :raw-name)) (namebound (LENGTH hdrname))
	    (hdrtyp (SEND hdr :raw-type)) (typebound (WHEN (STRINGP hdrtyp)
						       (LENGTH hdrtyp)))
	    (exact-name-length namebound) (exact-type-length typebound) newnamebound
	    newtypebound)
       ;1; find common subpart (note- :unspecific type always wins)*
       (VALUES
	 (DOLIST (e (CDR match-list)
		    (SEND
		      (SEND hdr :new-pathname :raw-name (SUBSEQ hdrname 0 namebound) :raw-type
			    (IF (STRINGP hdrtyp)
				(SUBSEQ hdrtyp 0 typebound)
				hdrtyp)
			    :version (IF (NUMBERP (SEND partial :version))
					 (SEND partial :version)
					 :newest))
		      :string-for-printing))
	   (SETQ newnamebound (string-compare-case hdrname (SETQ nam (SEND e :raw-name)) case))
	   (IF (EQ (SETQ typ (SEND e :raw-type)) :unspecific)
	       (PROGN
		 (SETQ hdrtyp :unspecific)
		 (SETQ exact-type-length nil))
	       ;1; else*
	       (SETQ newtypebound (string-compare-case hdrtyp typ case)))
	   (SETQ exact-name-length (MIN exact-name-length (LENGTH nam)))
	   (WHEN exact-type-length
	     (SETQ exact-type-length (MIN exact-type-length (LENGTH typ))))
	   (UNLESS (ZEROP newnamebound)
	     (SETQ namebound (MIN namebound (1- newnamebound))))
	   (UNLESS (OR (NULL exact-type-length) (ZEROP newtypebound))
	     (SETQ typebound (MIN typebound (1- newtypebound)))))
	 (IF (AND (= namebound exact-name-length)
		  (OR (NULL exact-type-length) (= typebound exact-type-length)))
	     ;1; then this string represents a file name that could be opened for input*
	     :old
	     :new)
	 match-list))))) 
  
  
;1;; --- DIRECTORY STUFF ---*
;1;; :directory-stream - creates and returns a directory stream*
;1;; :read-directory-stream-entry - *
;1;; :default-directory-stream-parser - ??? (used by read-directory-stream-entry, chaos-pathname defines*
;1;;    :directory-stream-default-parser - possibly misspelled and never actually used).  I DON'T THINK WE NEED THIS.*
  
  
(DEFVAR *ftp-fast-dir-list* nil
    "2Set t to limit remote all directory listings via FTP to names only - a faster choice*")
  
;1;;*********************
;1;; The default way of processing FTP directory lists is to do an NLIST (to get file names) and a LIST (to get descriptive*
;1;;   information).  The two lists are then "merged" (sometimes less than perfectly) to match up the file names with*
;1;;   their descriptions.*
;1;;*
;1;; There are two ways to do "smarter" processing of FTP directory lists:*
;1;;   (A) define (:METHOD FTP :CASE :PARSE-DIRECTORY-ENTRY) for a system type.*
;1;;       This is given a pathname object and a descriptive*
;1;;       string from which the pathname has been removed (hopefully correctly).  This method generates properties*
;1;;       from the descriptive string, most importantly :dired-echo, and :directory.*
;1;;       [Example: (:METHOD FTP :CASE :PARSE-DIRECTORY-ENTRY :UNIX)]*
;1;;*
;1;;   (B) define (METHOD FTP :CASE :DIRECTORY-LIST-FROM-FTP <IMPLEMENTATION-NAME>).  *
;1;;      This is given an established FTP control connection*
;1;;       and a newly-created directory stream object and is responsible for opening one or more data connections, reading*
;1;;       data and processing it to return a list of sublists, each sublist containing a pathname object and associated property/*
;1;;       value pairs.  The who-line must also be maintained by storing an open data stream object in the directory stream*
;1;;       object.  (The last data connection will be closed by the caller (closing before hand can destroy status information)*
;1;;       and the directory-stream removed from the who-line).*
;1;;       To cause this method to be invoked:*
;1;;         (1) for all instances of a particular system type, define*
;1;;                (:METHOD FTP :CASE :GET-FTP-IMPLMENTATION-TYPE-INTERNAL <system-type>) to return <IMPLEMENTATION-NAME>*
;1;;                [Example: (:METHOD FTP :CASE :DIRECTORY-LIST-FROM-FTP :TOPS20] &*
;1;;                          (:METHOD FTP :CASE :GET-FTP-IMPLEMENTATION-TYPE  :LISPM)]*
;1;;         (2) for a particular-host, set the property :FTP-SERVER-IMPLEMENTATION-TYPE on that host to *
;1;;             <IMPLEMENATION-NAME>*
;1;;                [Example: (:METHOD :FTP :CASE :DIRECTORY-LIST-FROM-FTP :WOLLONGONG)]*
;1;;*********************


(DEFFLAVOR ftp-directory-stream
	   (pathname				;1 necessary for :pathname message*
	    string-for-printing
	    (short-string nil)
	    (data-stream nil)
	    (done nil)
	    (COUNT 0)	;1count nlist and list*
	    (dir-list nil)
	    state)		;1 :newly-opened | :got-first | :eof | :closed   [NO LONGER USED]*
	   ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(DEFMETHOD (ftp-directory-stream :who-line-information) ()
  (VALUES self :input (+ count (IF data-stream
				   (SEND data-stream :read-pointer)
				   0))
	  (WHEN done
	    100)))

(DEFMETHOD (ftp-directory-stream :string-for-wholine) (&optional limit)
  ;1; This method is called when the string-for-printing plus percent and count are too long for the wholine*
  (IF (AND limit
	   short-string
	   (EQL (LENGTH short-string) limit))
      short-string
      ;1; else*
      (SETQ short-string (SEND pathname :string-for-wholine limit))))
  
(DEFMETHOD (ftp-directory-stream :next-entry) ()
    (POP dir-list)) 

(DEFMETHOD (ftp-directory-stream :close) (&optional ignore)
  (SETQ dir-list nil)
  (SETQ state :closed)
  (SEND tv:who-line-file-state-sheet :delete-stream self)) 
  
(DEFMETHOD (ftp-directory-stream :abort) ()
   (SEND self :close)) 
  

;1;; Redefine this so we don't needlessly create another list when our stream is really just a list anyway*
(DEFMETHOD (FTP :directory-list) (medium pathname &optional options &aux stream) 
  (SETQ stream (SEND self :directory-stream medium pathname options))
  (IF (ERRORP stream)
      stream
      ;1; else*
      (SEND stream :dir-list)))


(DEFVAR *ftp-directory-link* nil
  "2This is bound to the truename of the directory if it is a link.  Should only be bound and not setf'd.
Workaround the Unix bug of not chasing links with the ftp LIST command.*")


(DEFMETHOD (FTP :directory-stream) (medium pathname &optional options
				    &aux (error-p t) ftp ftp-list stream result fast? finished-p reply-code link-name)
  ;1; Process options:*
  ;1;   only supported options are :noerror, :no-extra-info (probably not exactly the same), :sorted*
  ;1;   other options of directory-list =  :deleted*
  ;1;   other options of chaos-pathname :directory-stream = :fast, :directories-only*
  (IGNORE medium)
  (ip:with-stream-whostate
    "Directory"
    (file-operation-retry
      (WHEN (MEMBER :noerror options :test 'EQ)
	(SETQ error-p nil))
      (SETQ fast? (OR *ftp-fast-dir-list* (MEMBER :no-extra-info options :test 'EQ)))
      (UNWIND-PROTECT
	  (PROGN
	    (SETF stream (MAKE-INSTANCE 'ftp-directory-stream :state :newly-opened
					:pathname pathname
					:string-for-printing (SEND pathname :string-for-printing)))
	    ;1; Loop to retry when we open a link and the operating system (e.g. Unix) did not resolve the link.*
	    ;1; Whenever a throw is done to :link-error the value thrown is the name of the link.*
	    (LOOP
	      (SETF ftp-list
		    (PROG1
		      (CATCH :directory-link-error
			;1; unless a throw occurs, this return will exit the loop*
			(RETURN
			  (LET ((*ftp-directory-link* link-name))
			    (SETQ ftp (reserve-ftp-connection (PATHNAME-HOST pathname) stream)) ;1; last arg is nicety for peek*
			    (SETQ ftp-list (SEND self :directory-list-from-ftp
						 (SEND self :get-ftp-implementation-type (PATHNAME-HOST pathname))
						 pathname ftp stream fast?))
			    (SEND stream :set-done t))))
		      ;1; Exit the loop when we have already tried what we thought was the truename.*
		      ;1; Aborting this second retry attempt may occur with a circular link, invalid link, etc.*
		      (WHEN ftp-list
			(SETF (SEND ftp :reply-code) 450)
			(SETF (SEND ftp :reply-string) (FORMAT nil "Directory not found for ~a" pathname))
			(RETURN))))
	      ;1; reset to try again with directory link resolved*
	      (UNWIND-PROTECT
		  (free-ftp-connection ftp nil :abort-data-conn)
		(SETF ftp nil))
	      (SEND stream :close)
	      ;1; resolve the directory link portion of the pathname by chasing links*
	      (SETF ftp-list (modify-dir-list (FIRST (FIRST ftp-list)) ftp-list nil))
	      (SETF link-name (GETF (CDR (FIRST ftp-list)) :link-to))
	      (SETF link-name (SEND pathname :new-directory
				    (APPEND (SEND link-name :directory)
					    (LIST (STRING-APPEND (IF (STRINGP (SEND link-name :name))
								     (SEND link-name :name)
								     "")
								 (IF (STRINGP (SEND link-name :type))
								     (STRING-APPEND "." (SEND link-name :type))
								     "")))))))
	    ;1; NOTE - this assumes that the final data connection hasn't been closed so that the control connection*
	    ;1;          still contains the status*
	    (SETF result
		  (COND
		    ;1; some Unix-like systems will violate the spec and return a 550 here*
		    ((OR (EQL 450 (SETF reply-code (SEND ftp :reply-code)))
			 (EQL 550 (SETF reply-code (SEND ftp :reply-code))))
		     (IF (OR (SEARCH "ermission" (SEND ftp :reply-string)) (SEARCH "ERMISSION" (SEND ftp :reply-string))
			     (SEARCH "rivelege" (SEND ftp :reply-string)) (SEARCH "RIVELEGE" (SEND ftp :reply-string)))
			 (MAKE-CONDITION 'incorrect-access-to-directory (SEND ftp :reply-string)
					 pathname :directory-stream)
			 (MAKE-CONDITION 'directory-not-found "Directory not found for ~a"
					 pathname :directory-stream))) 
		    ((NOT (ftp-unfavorable reply-code))
		     ;1; release the control connection so that modify-dir-list can use it*
		     (UNWIND-PROTECT
			 (free-ftp-connection ftp)
		       (SETF ftp nil))
		     (SETQ ftp-list (modify-dir-list pathname ftp-list (MEMBER :sorted options :test 'EQ)))
		     (SETF (SEND stream :dir-list) (PUSH (LIST nil :pathname pathname) ftp-list))
		     stream)
		    (t (check-ftp-status pathname ftp nil))))
	    (SETF finished-p t)
	    )			;1; end progn*
	(WHEN ftp
	  ;1; close data-connection with :abort in case all data hasn't been read*
	  (free-ftp-connection ftp (OR (TYPEP result 'condition) (NOT finished-p)) :abort-data-conn)) 
	(SEND tv:who-line-file-state-sheet :delete-stream stream))	;1; end unwind protect*
      (IF (AND error-p (TYPEP result 'condition))
	  (SIGNAL-CONDITION result)
	  result))))				;1; return stream or error condition*


(DEFSUBST nth-from-end (n list)
  ;1; one-relative (last element is 1 from end)*
  (NTH (- (LENGTH list) n) list)) 
  
  
(DEFPARAMETER *ftp-dir-newest-search-limit* 0
  "2Number of opens to try on a directory-list to resolve :newest version*")


(DEFVAR *ftp-ancestors-list* nil
  "2Used by modify dir-list to maintain ancestors when chasing links.  Must not be setf'd only bound within a let.*")


(DEFUN modify-dir-list (dirobj dir-list sort?
			&aux previous pathobj link-name found ftp seen-newest new-pathobj finished-p)
  ;1;; This used to be done on the fly by read-directory-entry*
  (UNWIND-PROTECT 
      (PROGN
	;1; Destructively throw out all the entries that don't match the directory spec and*
	;1; copy the properties into the pathname object (see ftp-open)*
	;1; NOTE: go ahead and copy the properties into the pathname object even if discarding for non-match*
	;1; (the cached copy of the pathname will be updated)*
	(LOOP with trailer = nil
	      with current = dir-list
	      while current do
	      (WHEN (SETQ pathobj (CAAR current))
		(SEND pathobj :set-property-list (COPY-LIST (CDAR current))))
	      (COND
		((SEND *ftp-service* :get-directory-entry dirobj pathobj)
		 (SETQ seen-newest (OR seen-newest (EQ (SEND pathobj :version) :newest)))
		 (SETQ trailer current))
		(trailer (RPLACD trailer (CDR current)))
		(:otherwise (SETQ dir-list (CDR current))))
	      (SETQ current (CDR current)))
	;1; chase links to determine the proper values of the :directory and :link-to properties (update cached pathnames also)*
	(DOLIST (item dir-list)
	  (WHEN (SETF link-name (GETF (CDR item) :link-to))
	    (LET ((*ftp-ancestors-list* (PUSH dirobj *ftp-ancestors-list*))
		  (pathobj (FIRST item))
		  link-dirlist new-link-to)
	      ;1; if problems occur return the last link*
	      (UNLESS (OR
			;1; circular link*
			(MEMBER link-name *ftp-ancestors-list* :test #'pathname-equal)
			;1; link-to directory-not-found or some other error*
			(ERRORP (SETF link-dirlist (SEND *ftp-service* :directory-list :ftp link-name '(:noerror))))
			;1; link-to non-existant*
			(NOT (SECOND link-dirlist)))
		;1; promote the :link-to property, if it exists*
		(WHEN (SETF new-link-to (GETF (CDR (SECOND link-dirlist)) :link-to))
		  (SETF (GET pathobj :link-to) new-link-to)
		  (SETF (GETF (CDR item) :link-to) new-link-to))
		;1; promote the :directory property*
		(COND ((SETF (GET pathobj :directory) (GETF (CDR (SECOND link-dirlist)) :directory))
		       (SETF (GETF (CDR item) :directory) (GETF (CDR (SECOND link-dirlist)) :directory)))
		      ;1; delete the directory property rather than setting it to nil*
		      (t (REMPROP pathobj :directory)
			 (REMF (CDR item) :directory)))))))
	;1; Sort if necessary (after discarding non-matches)*
	(WHEN (AND (> (LENGTH dir-list) 1) (OR seen-newest sort?))
	  (SETQ dir-list (SORT dir-list 'pathname-lessp :key 'CAR)))
	;1; Save :previous version if number not included (VAX) and throw out all but newest if that's what was requested*
	;1; This additional loop is unfortunate, but it requires a sort and we don't want to penalize other server types*
	;1; or sort more stuff than we have to*
	;1; Note that throwing out all but newest is required at least for directory list of non-wild file spec (like used on open)*
	;1;  and for open of a pathname with a concrete version number which may or may not be >*
	(WHEN seen-newest
	  (LOOP with trailer = nil with save-trailer nil with current = dir-list while
		current do
		(COND
		  ;1; Save :previous version if number not included (VAX)*
		  ((EQ (SEND (SETQ pathobj (CAAR current)) :version) :newest)
		   (SETQ previous
			 (OR
			   (AND save-trailer
				(SEND pathobj :pathname-match-no-version (CAAR save-trailer)
				      nil)
				(NUMBERP (SETQ previous (SEND (CAAR save-trailer) :version)))
				previous)
			   0))
		   ;1; [Optionally] have a try at establishing the real version number  *
		   (UNLESS (OR (NULL *ftp-dir-newest-search-limit*)
			       (ZEROP *ftp-dir-newest-search-limit*))
		     (UNLESS ftp (SETQ ftp (reserve-ftp-connection (SEND dirobj :host) :modify-dir-list)))
		     (MULTIPLE-VALUE-SETQ (new-pathobj found)
					  (ftp-probe-newest pathobj previous *ftp-dir-newest-search-limit*
							    ftp)))
		   (IF found
		       (PROGN
			 (RPLACA (CAR current) new-pathobj)
			 (SEND new-pathobj :set-property-list
			       (COPY-LIST (SEND pathobj :property-list))))
		       ;1; else *
		       (SEND pathobj :putprop previous :previous))
		   (SETQ trailer current))
		  ((AND trailer (EQ (SEND dirobj :version) :newest))
		   ;1; delete current item (not :newest)*
		   (RPLACD trailer (CDR current)))
		  ((EQ (SEND dirobj :version) :newest)
		   ;1; delete head item (not newest)*
		   (SETQ dir-list (CDR current))))
		(SETQ save-trailer current) (SETQ current (CDR current))))
	(SETF finished-p t))
    (WHEN ftp
      (free-ftp-connection ftp (NOT finished-p))))
  dir-list)


(DEFMETHOD (FTP :get-directory-entry) (dirobj pathobj &aux match-dir)
  ;1; hook for doing special things to get files to match the directory spec*
  (SETQ match-dir 
;1x (OR (SEND SELF :SEND-IF-HANDLES :PATH-FOR-FTP-DIR-MATCH DIROBJ) DIROBJ))*
	dirobj)       
  ;1; don't include bad pathname objects*
  (NOT
    (OR (NULL pathobj) (NULL (SEND pathobj :name)) (EQ (SEND pathobj :name) :wild)
	;1; want :newest to match # and # to match :newest (for case of Vax)*
	(AND (NOT (SEND match-dir :pathname-match-no-device pathobj nil))
	     (NOT (SEND pathobj :pathname-match-no-device match-dir)))))) 

;1; This is the default directory lister. Other cases are handled by creating a :case method*
;1; that selects on the ftp-implementation type.*
(DEFMETHOD (FTP :directory-list-from-ftp) (IGNORE pathname ftp stream fast?)
  (SEND self :directory-list-from-ftp :basic pathname ftp stream fast?)) 

(DEFVAR *ftp-implementation-types* '(:basic :minimal :explorer :MicroExplorer :tops20 :bsd4.2 :wollongong :vms-excelan))

;1; NOTE: could set implementation-type to this to :BASIC to undo a host-specific parsing method for a particular host*
(DEFMETHOD (FTP :case :directory-list-from-ftp :basic) (PATHNAME ftp stream fast? &aux ftp-nlst ftp-list)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :nlist))
    (SEND tv:who-line-file-state-sheet :add-stream stream)
    (SETQ ftp-nlst (construct-dir-list ftp stream))
    (IF (OR fast?
	    (PROGN
	      (SEND ftp :close)
	      (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))))
	(PROGN
	  (SEND stream :set-done t)
	  (ftp-dir-merge pathname ftp-nlst nil))
	;1; else*
	(PROGN
	  (SETQ ftp-list (construct-dir-list ftp stream))
	  (SEND stream :set-done t)
	  (ftp-dir-merge pathname ftp-nlst ftp-list))))) 

;1;; CONSTRUCT-DIR-LIST *

(DEFUN construct-dir-list (FTP &optional dir-stream &aux data-stream)
  ;1; ftp is an ftp-instance with an established data connection which is a stream of lines*
  ;1; Return a list having the lines (w/o terminators) as members*
  (SETQ data-stream (SEND ftp :data-connection))
  (WHEN dir-stream
    (SEND dir-stream :set-data-stream data-stream))
  (PROG1
    (LOOP for entry = (READ-LINE data-stream nil) while entry collecting entry)
    (WHEN dir-stream
      (WITHOUT-INTERRUPTS (SEND dir-stream :set-data-stream nil)
			  (SEND dir-stream :set-count
				(+ (SEND dir-stream :count) (SEND data-stream :read-pointer))))))) 
  
  
;1;; FTP-DIR-MERGE*
;1;; Take a list of pathstrings and a list of arbitrary strings containing those pathstrings  + descriptive stuff*
;1;; and create a new list of (pathobj, descrip-string).  Don't assume order, equivalent list length, or*
;1;; position of the pathstring in the descriptive string.  If an element in the list of pathstrings has*
;1;; no corresponding entry in the list of descriptive strings include it alone.  Ignore any extra entries*
;1;; in the descriptive list.*
  
(DEFUN ftp-dir-merge (dirobj ftp-nlst ftp-list &aux found-at-head found-ix found-len descrip-substring pathobj
		      matched-file dir-entry)
  (WHEN *ftpint-debug*
    (PRINT
      (FORMAT nil "-----~%FTP-DIR-MERGE for ~A~%  Nlist=~S~%  List=~S~%-----" dirobj
	      ftp-nlst ftp-list)))
  (LOOP for pathstring in ftp-nlst when
	(SETQ dir-entry
	      (PROGN
		(SETQ found-at-head t)
		(MULTIPLE-VALUE-SETQ (pathobj pathstring)
				     (nlist-strip-and-parse dirobj pathstring))
		(SETQ descrip-substring
		      (IF (NULL pathobj)
			  ""
			  ;1; else*
			  (STRING-TRIM '(#\Space)
				       (DOLIST (descrip-string ftp-list "")
					 (COND
					   ;1; try to throw out things like header lines *
					   ((AND (NOT matched-file)
						 (NOT (contains-number descrip-string)))
					    (IF found-at-head
						(SETQ ftp-list (CDR ftp-list))
						;1; else*
						(SETQ found-at-head nil)))
					   ;1; try to match full pathstring*
					   ((MULTIPLE-VALUE-SETQ (found-ix found-len)
								 (ftp-dir-search pathstring descrip-string)))
					   ;1;; rla 10/22/85 - try string-for-host*
					   ((MULTIPLE-VALUE-SETQ (found-ix found-len)
								 (ftp-dir-search (SEND pathobj :string-for-host)
										 descrip-string)))
					   ;1; get a little more heuristic:*
					   ;1;  get file name from pathobj, must match from file name on with no trailing junk*
					   ((MULTIPLE-VALUE-SETQ (found-ix found-len)
								 (ftp-dir-search (SEND pathobj :string-for-dired)
										 descrip-string)))
					   ;1; no match yet*
					   (t (SETQ found-at-head nil)))
					 (WHEN found-ix
					   (SETQ matched-file t)
					   ;1; clean up the list*
					   (IF found-at-head
					       (SETQ ftp-list (CDR ftp-list))
					       ;1; else*
					       (SETQ ftp-list
						     (DELETE descrip-string (THE list ftp-list) :count
							     1 :test 'STRING-EQUAL)))
					   (RETURN
					     (STRING-APPEND
					       ;1; leading stuff*
					       (IF (ZEROP found-ix)
						   ""
						   (SUBSEQ descrip-string 0 found-ix))
					       " "
					       ;1; trailing stuff*
					       (IF (< (+ found-ix found-len) (LENGTH descrip-string))
						   (SUBSEQ descrip-string (+ found-ix found-len))
						   ""))))))))
		(WHEN pathobj
		  (SEND dirobj :parse-directory-entry pathobj descrip-substring))))
	collecting dir-entry)) 

(DEFUN ftp-dir-search (partial-path descrip-string &aux found-len found-ix)
  (SETQ found-len (LENGTH partial-path))
  (SETQ found-ix 0)
  (LOOP
    (SETQ found-ix
	  (SEARCH (THE string (STRING partial-path)) (THE string (STRING descrip-string)) :start2
		  found-ix :end2 nil :start1 0 :end1 nil :test 'CHAR=))	;1consider case*
    (UNLESS found-ix
      (RETURN nil))
    ;1; must have leading and trailing white space*
    (IF (AND (OR (ZEROP found-ix) (MEMBER (AREF descrip-string (1- found-ix)) '(#\Space #\Tab) :test 'CHAR-EQUAL))
	     (OR (EQL (+ found-ix found-len) (LENGTH descrip-string))
		 (MEMBER (AREF descrip-string (+ found-ix found-len)) '(#\Space #\Tab) :test 'CHAR-EQUAL)))
	(RETURN (VALUES found-ix found-len))
	(INCF found-ix)))) 
  
(DEFUN contains-number (str &aux strlen found-num ch is-digit (delim-set '(#\( #\) #\: #\/ #\Space)))
  ;1; Check to see if a string contains a number separated by delimiters*
  (SETQ strlen (LENGTH str))
  (LOOP for i from 0 to (1- strlen) do (SETQ is-digit (DIGIT-CHAR-P (SETQ ch (AREF str i))))
	(IF found-num
	    (IF is-digit
		(WHEN (EQL i (1- strlen))
		  (RETURN t))
		;1; else *
		(PROGN
		  (WHEN (MEMBER ch delim-set :test 'EQL)
		    (RETURN t))
		  (SETQ found-num nil)))
	    ;1; else*
	    (WHEN (AND is-digit (OR (EQL i 0) (MEMBER (AREF str (1- i)) delim-set :test 'EQL)))
	      (WHEN (EQL i (1- strlen))
		(RETURN t))
	      (SETQ found-num t))))) 
  

(DEFUN nlist-strip (pathstring &aux start end)
  (LOOP for i from 0 to (1- (LENGTH pathstring)) do
	(IF (EQL (AREF pathstring i) #\Space)
	    (WHEN start
		  (IF end
		      (RETURN)
		      (SETQ end i)))		;1allow one embedded space*
	    ;1; else*
	    (PROGN
	      (SETQ end nil)
	      (UNLESS start
		      (SETQ start i)))))
  (IF (AND (OR (NULL start) (ZEROP start)) (NULL end))
      pathstring
      (SUBSEQ pathstring start end))) 
  

(DEFUN nlist-strip-and-parse (PATHNAME nlist-pathstring)
  (IF (STRINGP nlist-pathstring)
      (PROGN
	(SETQ nlist-pathstring (nlist-strip nlist-pathstring))
	;1; merge in leading parts of pathname, but leave type alone, default version = newest*
	(VALUES
	  ;1; return nil if unparseable*
	  (IGNORE-ERRORS
	    (merge-pathname-defaults nlist-pathstring
				     (SEND pathname :new-pathname :name :wild :type nil :version
					   :newest)
				     nil :newest))
	  nlist-pathstring))
      ;1; else*
      nlist-pathstring))				;1might already be a pathname object*
  

;1;;>>> ???*
;1x(DEFMETHOD (FTP-PATHNAME :READ-DIRECTORY-STREAM-ENTRY) (STREAM)*
;1x   (SEND STREAM :NEXT-ENTRY)) *

;1;; This isn't fool-proof, but will just result in an error signalled by dired on attempt to descend to*
;1;;  a non-directory file having this extension. *
(DEFUN ftp-is-directory (PATHNAME &aux dir-type self-type)
  (WHEN (AND (STRINGP (SETQ dir-type (SEND pathname :send-if-handles :directory-file-type)))
	     (STRINGP (SETQ self-type (SEND pathname :type))))
    (STRING-EQUAL self-type dir-type))) 
  

;1;; RESULT FORMAT IS (<property/value pairs>).*
(DEFMETHOD (FTP :parse-directory-entry) (pathobj descrip-string)
  ;1; default is no parsing (just echo)*
  (IF (ftp-is-directory pathobj)
      (LIST pathobj :dired-echo descrip-string :directory t)
      (LIST pathobj :dired-echo descrip-string))) 

(DEFMETHOD (FTP :case :parse-directory-entry :unix) (pathobj descrip-string &aux index directory?)
;1;; KPK 10/9/85*
;1; This will parse the string looking for the 'd' in the mode to set the :directory property*
  (SETF index (STRING-SEARCH-SET "dbcp-" descrip-string))	;1 Get start of mode*
  (IF (AND index				;1 think we found mode*
	   (< (+ index 3) (LENGTH descrip-string))	;1RLA - don't run off the end*
	   (OR (EQUAL (AREF descrip-string (+ index 1)) #\r)	;1 next character is r or*
	       (EQUAL (AREF descrip-string (+ index 1)) #\-))	;1 a dash.*
	   (OR (EQUAL (AREF descrip-string (+ index 2)) #\w)	;1 next character is w or*
	       (EQUAL (AREF descrip-string (+ index 2)) #\-))	;1 a dash.*
	   (OR (EQUAL (AREF descrip-string (+ index 3)) #\x)	;1 next character is x or*
	       (EQUAL (AREF descrip-string (+ index 3)) #\-))	;1 a dash.*
	   (EQUAL (AREF descrip-string index) #\d))	;1 d is for directory*
      (SETQ directory? t)
      ;1; else*
      (IF (SEARCH " dir " descrip-string)	;1 else try for text "dir"*
	  (SETQ directory? t)			;1 this is for apollo*
	  ;1; else*
	  (SETQ directory? (ftp-is-directory pathobj))))	;1 just in case*
  (IF directory?
      (LIST pathobj :dired-echo descrip-string :directory t)
      (LIST pathobj :dired-echo descrip-string)))
  

;1;;--------------------------------------------------------------*
;1;; IMPLEMENTATION-SPECIFIC DIRECTORY PARSING METHODS*
;1;;--------------------------------------------------------------*

(DEFUN add-directory-stream (directory-stream data-stream)
  (SEND tv:who-line-file-state-sheet :add-stream directory-stream)
  (SEND directory-stream :set-data-stream data-stream)) 
  

(DEFMETHOD (FTP :case :directory-list-from-ftp :minimum) (PATHNAME ftp stream
							  &optional ignore &aux data-stream entry pathobj)
  ;1;; FOR IMPLEMENTATIONS WHICH SEND ONLY SIMPLE STRINGS FOR BOTH NLIST & LIST*
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :nlist))
    (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
    (LOOP for line = (READ-LINE data-stream nil) while line when
	  (SETQ entry
		(WHEN (SETQ pathobj (nlist-strip-and-parse pathname line))
		  (IF (ftp-is-directory pathobj)
		      (LIST pathobj :dired-echo "" :directory t)
		      ;1; else*
		      (LIST pathobj :dired-echo ""))))
	  collecting entry))) 
  
  

(DEFMETHOD (FTP :case :directory-list-from-ftp :explorer) (PATHNAME ftp stream fast? &aux data-stream entry)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))
	 (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
	 (LOOP for line = (READ-LINE data-stream nil) while line when
	       (SETQ entry (lispm-directory-line-parse pathname line fast?)) collecting entry)))
  

(DEFUN lispm-directory-line-parse (dirobj line fast? &aux pathobj cursor cursor2 result month day year hours minutes seconds
				   length-in-blocks length-in-bytes byte-size)
  ;1;; FORMAT:*
  ;1;; {d}  <pathname>  #-blocks  #-bytes(byte-size)  {!}{@}{$}{@} mm/dd//yy hh:mm:ss {author}*

  ;1;;                           ""    DIRECTORY         ""*
  (WHEN (CHAR-EQUAL (AREF line 0) #\d)
    (PUSH t result)
    (PUSH :deleted result))
  ;1; Careful - there may be multiple blanks embedded in the pathname and pathname parsing doesn't handle :Junk-allowed very well*
  (SETQ cursor (POSITION #\Space (THE string (STRING line)) :start 1 :test-not 'CHAR-EQUAL))
  (SETQ cursor2
	(POSITION #\Space (THE string (STRING line)) :start
		  (POSITION #\# (THE string (STRING line)) :start cursor :test 'CHAR-EQUAL)
		  :test 'CHAR-EQUAL))
;1; Make the pathname to merge with instead of parsing it. *BJ**
  (SETQ pathobj (MERGE-PATHNAMES
		  (parse-pathname (SUBSEQ line cursor cursor2) nil dirobj)
		  (fs:make-PATHNAME :host (send dirobj :host)
				    :type :unspecific)))
  (UNLESS fast?
    (MULTIPLE-VALUE-SETQ (length-in-blocks cursor)
			 (PARSE-INTEGER line :start cursor2 :junk-allowed t))
    (PUSH length-in-blocks result)
    (PUSH :length-in-blocks result)
    (IF (MULTIPLE-VALUE-SETQ (length-in-bytes cursor)
			     (PARSE-INTEGER line :start cursor :junk-allowed t))
	(PROGN
	  (MULTIPLE-VALUE-SETQ (BYTE-SIZE cursor)
			       (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	  (PUSH byte-size result)
	  (PUSH :byte-size result)
	  (PUSH length-in-bytes result)
	  (PUSH :length-in-bytes result))
	;1; ELSE*
	(PROGN
	  (PUSH t result)
	  (PUSH :directory result)
	  (INCF cursor (LENGTH "DIRECTOR"))))	;1leave at "Y" (will be incremented)*
    (LOOP
      (SETQ cursor
	    (POSITION #\Space (THE string (STRING line)) :start (1+ cursor) :test-not
		      'CHAR-EQUAL))
      (SELECTOR (AREF line cursor) char-equal (#\! (PUSH t result) (PUSH :not-backed-up result))
		(#\# (PUSH t result) (PUSH :dont-supersede result))
		(#\$ (PUSH t result) (PUSH :dont-reap result))
		(#\@ (PUSH t result) (PUSH :dont-delete result)) (:otherwise (RETURN))))
    (MULTIPLE-VALUE-SETQ (month cursor)
			 (PARSE-INTEGER line :start cursor :junk-allowed t))
    (MULTIPLE-VALUE-SETQ (day cursor)
			 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
    (MULTIPLE-VALUE-SETQ (year cursor)
			 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
    (MULTIPLE-VALUE-SETQ (hours cursor)
			 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
    (MULTIPLE-VALUE-SETQ (minutes cursor)
			 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
    (MULTIPLE-VALUE-SETQ (seconds cursor)
			 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
    (PUSH (ENCODE-UNIVERSAL-TIME seconds minutes hours day month year) result)
    (PUSH :creation-date result)
    (IF (SETQ cursor
	      (POSITION #\Space (THE string (STRING line)) :start cursor :test-not 'CHAR-EQUAL))
	(PROGN
	  (SETQ cursor2
		(POSITION #\Space (THE string (STRING line)) :start cursor :test 'CHAR-EQUAL))
	  (PUSH (SUBSEQ line cursor cursor2) result))
	;1; else*
	(PUSH (CAR (SEND dirobj :directory)) result))
    (PUSH :author result))
  (PUSH pathobj result)) 


(DEFMETHOD (FTP :case :directory-list-from-ftp :MicroExplorer) (PATHNAME ftp stream fast? &aux data-stream entry)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))
	 (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
	 (LOOP for line = (READ-LINE data-stream nil) while line when
	       (SETQ entry (microExplorer-directory-line-parse pathname line fast?)) collecting entry)))

(DEFUN MicroExplorer-directory-line-parse (dirobj line fast? &aux cursor cursor2 result creation-date-end
					   month day year hours minutes seconds directoryp)
  (declare (ignore fast?)) 
  ;1;; FORMAT:*
  ;1;;    <pathname>  #-bytes(byte-size)  {!}{@}{$}{@} mm/dd//yy hh:mm:ss {author} DIRECTORY*
  ;1;;     *
  (setf cursor (position #\space line :test #'char-equal :from-end t))
  
  ;1; Try to find the creation date. This test may be overkill but might as well be sure.*
  (loop
    (setf cursor (position #\/ line :test #'char-equal :end cursor :from-end t))
    (when (and
	    
	    ;1; Look for the year and closing paren.*
	    (digit-char-p (char line (+ cursor 1)))
	    (digit-char-p (char line (+ cursor 2)))
	    (char-equal (char line (+ cursor 3)) #\))
	    
	    ;1; Look for the day and /*
	    (digit-char-p (char line (- cursor 1)))
	    (digit-char-p (char line (- cursor 2)))
	    (char-equal (char line (- cursor 3)) #\/)
	    
	    ;1; Look for the month and )*
	    (digit-char-p (char line (- cursor 4)))
	    (digit-char-p (char line (- cursor 5)))
	    (char-equal (char line (- cursor 6)) #\())
      
      (setf creation-date-end (+ cursor 4))
      (return)))
  
  ;1; Now get the author.*
  (setf cursor (+ cursor 4))
  
  (cond ((SETQ cursor
	       (POSITION #\Space line :start cursor :test-not 'CHAR-EQUAL))
	 (SETQ cursor2
	       (POSITION #\Space line :start cursor :test 'CHAR-EQUAL))
	 (PUSH (SUBSEQ line cursor cursor2) result))
	;1; else*
	(t
	 (PUSH (CAR (SEND dirobj :directory)) result)))
  (PUSH :author result)
  
  ;1; Now find the directory property*
  (when (SETQ cursor
	      (POSITION #\Space (THE string (STRING line)) :start cursor2 :test-not 'CHAR-EQUAL))
    (SETQ cursor2
	  (POSITION #\Space (THE string (STRING line)) :start cursor :test 'CHAR-EQUAL))
    
    (when (string-equal "DIRECTORY" line :start2 cursor :end2 cursor2)
      (setf directoryp t)
      (push t result)
      (PUSH :directory result)))
  
  ;1; Start going backwards through the line. Creation date.*
  (setf year (PARSE-INTEGER line :start (- creation-date-end 3) :junk-allowed t))
  (setf day (PARSE-INTEGER line :start (- creation-date-end 6) :junk-allowed t))
  (setf month (PARSE-INTEGER line :start (- creation-date-end 9) :junk-allowed t))
  (push (encode-universal-time 0 0 0 day month year) result)
  (push :creation-date result)

  ;1; Last modification date.*
  (setf cursor (position #\space line
			 :from-end t
			 :test-not #'char-equal
			 :end (position #\space line :from-end t :end creation-date-end :test #'char-equal)))
  
  (setf seconds (parse-integer line :start (- cursor 1) :junk-allowed t))
  (setf minutes (parse-integer line :start (- cursor 4) :junk-allowed t))
  (setf hours   (parse-integer line :start (- cursor 7) :junk-allowed t))

  (setf cursor (position #\space line
			 :from-end t
			 :test-not #'char-equal
			 :end (- cursor 8)))
  
  (setf year    (parse-integer line :start (- cursor 1) :junk-allowed t))
  (setf day     (parse-integer line :start (- cursor 4) :junk-allowed t))
  (setf month   (parse-integer line :start (- cursor 7) :junk-allowed t))

  (push (encode-universal-time seconds minutes hours day month year) result)
  (push :modification-date result)

  ;1; get byte size if it is there.*
  (setf cursor (position #\space line
			 :from-end t
			 :test-not #'char-equal
			 :end (position #\space line :from-end t :end (- cursor 8) :test #'char-equal)))
  
  (setf cursor2 cursor)
  (when (char-equal (char line cursor) #\))
    (setf cursor2 (position #\( line :from-end t :end cursor :test #'char-equal))
    (unless directoryp
      (push (parse-integer line :start (1- cursor) :junk-allowed t) result)
      (push :byte-size result)))

  (setf cursor (position #\space line :from-end t :test #'char-equal :end cursor2))
  (push (parse-integer line :start cursor :junk-allowed t) result)
  (cond (directoryp
	 (push :length-in-blocks result))
	(t
	 (push :length-in-bytes result)))
  
  ;1; Last of all put the pathname on the entry.*
  (setf cursor2 (position #\space line :test-not #'char-equal :from-end t :end cursor))
  (setf cursor  (position #\space line :test-not #'char-equal))
  
  (push (MERGE-PATHNAMES (ip:ftp-parse-for-server (SUBSEQ line cursor (+ cursor2 1)) dirobj) (PATHNAME ".")) result))


;1;; NOTE:*
;1;;        Always including a dired-echo property if there is a chance that any entry may generate that property*
;1;;         will help dired alignment.*


(DEFMETHOD (FTP :case :directory-list-from-ftp :wollongong) (PATHNAME ftp stream &optional fast?
							     &aux data-stream entry)
  (COND ((ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))
	 ;1; Wollongong returns an error rather than an empty listing on nonexistant file.  Return an empty listing*
	 (WHEN (AND (EQL 550 (SEND ftp :reply-code)) (SEARCH "file not found" (SEND ftp :reply-string)))
	   (SETF (SEND ftp :reply-code) 200.)
	   nil))
	(t
	 (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
	 (DOTIMES (i 3) (READ-LINE data-stream nil)) ;1; skip first three lines*
	 (LOOP for line = (READ-LINE data-stream nil)
	       while line do
	       (SETQ entry (wollongong-3.x-directory-line-parse pathname line fast?))
	       when entry collecting entry))))


(DEFSUBST translate-alphabetic-month (name)
  (LOOP for m in time::*months* with found = 1 do
     (IF (STRING-EQUAL (CAR m) name)
       (RETURN found)
       (INCF found))))


(DEFUN wollongong-3.x-directory-line-parse (dirobj line &optional fast?
					    &aux pathobj cursor result month day year hours minutes bytes)
  ;1; FORMAT:*
  ;1; <20 character pathname> MMM DD HH:MM YYYY blocks protection*
  ;1; tcp.lisp;1           Nov  5 15:43 1985   1024 (,RWE,RWE,RE) *
  (WHEN (SETQ pathobj
	      (IGNORE-ERRORS (parse-pathname (SUBSEQ line 0
						     (SETF cursor (POSITION #\Space (THE string (STRING line))
									    :start cursor :test 'CHAR-EQUAL)))
					     (SEND dirobj :host) nil)))
    (UNLESS (SEND pathobj :type)
      (SETQ pathobj (SEND pathobj :new-type :unspecific)))
    (UNLESS (SEND pathobj :version)
      (SETQ pathobj (SEND pathobj :new-version :unspecific)))
    (SETQ pathobj (MERGE-PATHNAMES pathobj dirobj)))
  (COND
    ((NULL pathobj) (VALUES nil))
    (fast? (VALUES (LIST pathobj)))
    (t
     (WHEN (STRING-EQUAL (SEND pathobj :type) (SEND pathobj :directory-file-type))
       (SETF (GETF result :directory) t))
     (SETQ cursor (POSITION #\Space (THE string (STRING line)) :start cursor :test-not 'CHAR-EQUAL))
     (SETQ month (translate-alphabetic-month (SUBSEQ line cursor (INCF cursor 3))))
     (MULTIPLE-VALUE-SETQ (day cursor)
       (PARSE-INTEGER line :start cursor :junk-allowed t))
     (MULTIPLE-VALUE-SETQ (hours cursor)
       (PARSE-INTEGER line :start cursor :junk-allowed t))
     (MULTIPLE-VALUE-SETQ (minutes cursor)
       (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
     (MULTIPLE-VALUE-SETQ (year cursor)
       (PARSE-INTEGER line :start cursor :junk-allowed t))
     (SETF (GETF result :creation-date) (ENCODE-UNIVERSAL-TIME 0 minutes hours day month year))
     (MULTIPLE-VALUE-SETQ (bytes cursor)
       (PARSE-INTEGER line :start cursor :junk-allowed t))
     (SETF (GETF result :length-in-bytes) bytes)
     (SETF (GETF result :dired-echo) (SUBSEQ line cursor)) 
     (PUSH pathobj result))))


(DEFMETHOD (FTP :case :directory-list-from-ftp :wollongong-2.x) (PATHNAME ftp stream &optional fast?
								 &aux data-stream entry)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))
    (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
    (LOOP for line = (READ-LINE data-stream nil)
	  while line do
	  (SETQ entry (wollongong-2.x-directory-line-parse pathname line fast?))
	  when entry collecting entry)))     


(DEFUN wollongong-2.x-directory-line-parse (dirobj line &optional fast?
					    &aux pathobj pathstring cursor cursor2 entry month day year hours
					    minutes seconds)
 ;1;; FORMATS:*
 ;1;;  <pathname>    (directory)*
 ;1;;  <pathname>   Tue Nov  5 15:43:13 1985,   1024 bytes *
 ;1;;  <pathname>    -- Can't access file --*
  (SETQ cursor (POSITION #\Space (THE string (STRING line)) :test-not 'CHAR-EQUAL))
  (SETQ cursor2 (POSITION #\Space (THE string (STRING line)) :start cursor :test 'CHAR-EQUAL))
  (WHEN (AND (NOT (OR (STRING-EQUAL pathstring ".") (STRING-EQUAL pathstring "..")))
      (SETQ pathobj (IGNORE-ERRORS (parse-pathname pathstring (SEND dirobj :host) nil))))
   ;1; Make sure no type gets merged in if pathname has no type.  This will at least allow these files to be accessed from*
   ;1; dired (use of the raw pathname will yield a nil type and merge will always stick something in)*
    (UNLESS (SEND pathobj :type)
      (SETQ pathobj (SEND pathobj :new-type :unspecific)))
    (SETQ pathobj (MERGE-PATHNAMES pathobj dirobj))
    (WHEN (SETQ cursor
	   (POSITION #\Space (THE string (STRING line)) :start cursor2 :test-not 'CHAR-EQUAL))
      (SETQ cursor2
	    (POSITION #\Space (THE string (STRING line)) :start cursor :test 'CHAR-EQUAL))
      (COND
       ;1; merge DIR as type before kicking out because of fast? flag*
       ((SEARCH (THE string (STRING "(directory)")) (THE string (STRING line)) :start2
		cursor :test 'CHAR-EQUAL)
	(SETQ pathobj (SEND pathobj :new-type (SEND pathobj :directory-file-type)))
	(UNLESS fast?
	  (PUSH t entry)
	  (PUSH :directory entry)
	  (PUSH "" entry)
	  (PUSH :dired-echo entry)))
       (fast?)
       ((ASSOC (SUBSEQ line cursor cursor2) time::*days-of-the-week* :test 'EQUAL)
	(SETQ cursor
	      (POSITION #\Space (THE string (STRING line)) :start cursor2 :test-not 'CHAR-EQUAL))
	(SETQ month (translate-alphabetic-month (SUBSEQ line cursor (+ cursor 3))))
	(MULTIPLE-VALUE-SETQ (day cursor)
	  (PARSE-INTEGER line :start (+ cursor 3) :junk-allowed t))
	(MULTIPLE-VALUE-SETQ (hours cursor)
	  (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	(MULTIPLE-VALUE-SETQ (minutes cursor)
	  (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	(MULTIPLE-VALUE-SETQ (seconds cursor)
	  (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	(MULTIPLE-VALUE-SETQ (year cursor)
	  (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	(PUSH (ENCODE-UNIVERSAL-TIME seconds minutes hours day month year) entry)
	(PUSH :creation-date entry)
	(PUSH (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t) entry)
	(PUSH :length-in-bytes entry) (PUSH "" entry) (PUSH :dired-echo entry))
       (t
	(SETQ cursor2
	      (POSITION #\Space (THE string (STRING line)) :from-end t :test-not 'CHAR-EQUAL))
	(PUSH (SUBSEQ line cursor (1+ cursor2)) entry) (PUSH :dired-echo entry))))
    (PUSH pathobj entry))) 


(DEFMETHOD (FTP :case :directory-list-from-ftp :vms-excelan) (PATHNAME ftp stream &optional fast?)
  ;1; If version number is newest, doesn't return version number.  Get them all, and the greatest*
  ;1; will be used when deriving a truename*
  (LET ((dirobj (IF (EQ (SEND pathname :version) :newest)
		    (SEND pathname :new-version :wild)
		    pathname))
	data-stream
	entry)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp dirobj) :list))
    (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
    (LOOP
      for line = (READ-LINE data-stream nil)
      while line
      when (SETQ entry (vms-excelan-directory-line-parse dirobj line fast?))
      collecting entry))))

(DEFUN vms-excelan-directory-line-parse (dirobj line &optional fast? &aux entry)
  
  (CONDITION-CASE (condition)
    (LET (path-start
	  path-end
	  cursor
	  cursor2
	  value
	  pathstring
	  pathobj)
      
      (IF (SETQ cursor (POSITION #\% line))
	  ;1; THEN FORMAT IS <PATHNAME>: %<ERROR MESSAGE>*
	  (PROGN
	    (SETQ path-start 0)
	    (SETQ path-end (POSITION #\: line :end cursor :from-end t))
	    (SETQ entry (LIST :dired-echo (SUBSEQ line cursor))))
	  
	  ;1; ELSE FORMAT IS "RWED  [GROUP,OWNER]  BLKS-USED/ALLOC  DD-MON-YY HH:MM   PATHNAME"*
	  (SETF cursor (STRING-SEARCH-SET '(#\R #\-) line))
	  (WHEN cursor
	    (PUSH (SUBSEQ line cursor (+ cursor 4)) entry)
	    (PUSH :dired-echo entry))
	  
	  (SETQ cursor (POSITION #\[ line :start (+ cursor 4)))
	  (SETQ cursor2 (1+ (POSITION #\] line :start cursor)))
	  (PUSH (SUBSEQ line cursor cursor2) entry)
	  (PUSH :author entry)
	  
	  (SETQ cursor (STRING-SEARCH-NOT-SET '(#\Space #\Tab) line cursor2))
	  (MULTIPLE-VALUE-SETQ (value cursor) (PARSE-INTEGER line :start cursor :junk-allowed t))
	  (PUSH value entry)
	  (PUSH :length-in-blocks entry)
	  
	  (SETQ cursor (POSITION #\- line :start cursor))
	  (LET*
	    ((day (PARSE-INTEGER line :start (- cursor 2) :end cursor :junk-allowed t))
	     (month (translate-alphabetic-month (SUBSEQ line (1+ cursor) (+ cursor 4))))
	     year hr min)
	    (MULTIPLE-VALUE-SETQ (year cursor) (PARSE-INTEGER line :start (+ cursor 5) :junk-allowed t))
	    (MULTIPLE-VALUE-SETQ (hr cursor) (PARSE-INTEGER line :start cursor :junk-allowed t))
	    (MULTIPLE-VALUE-SETQ (MIN cursor) (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))
	    (WHEN (AND (NUMBERP day) (NUMBERP month) (NUMBERP year) (NUMBERP hr) (NUMBERP min))
	      (PUSH (time:encode-universal-time 0 min hr day month year) entry)
	      (PUSH :creation-date entry)))
	  
	  (SETQ path-start (STRING-SEARCH-NOT-SET '(#\Space #\Tab) line cursor)))
      
      (WHEN fast? (SETQ entry nil))		;1cheating a bit*
      (SETQ pathstring (SUBSEQ line path-start path-end))
      (WHEN (SETQ pathobj (IGNORE-ERRORS (parse-pathname pathstring (SEND dirobj :host) nil)))
	;1; Make sure no type gets merged in if pathname has no type.  This will at least allow these *
	;1; files to be accessed from dired (use of the raw pathname will yield a nil type and *
	;1; merge will always stick something in)*
	(UNLESS (SEND pathobj :type)
	  (SETQ pathobj (SEND pathobj :new-type :unspecific)))
        (SETQ pathobj (MERGE-PATHNAMES pathobj dirobj))
	(WHEN (STRING-EQUAL (SEND pathobj :type) (SEND pathobj :directory-file-type))
	   (PUSH t entry)
	   (PUSH :directory entry))
	(PUSH pathobj entry)))
    
    (ERROR 
       (WHEN *ftpint-debug*
          (FORMAT t "~&FTP ERROR PARSING [~A]:~% ~A~% RESULT SO FAR = ~A" line (SEND condition :report-string) entry))
       nil)))



(DEFVAR *ftp-recursive-directory-list-from-ftp* nil
  "2Indicates when recursive calls are made to directory-list-from-ftp.  Should only be bound within a let.*")


(DEFMETHOD (FTP :case :directory-list-from-ftp :bsd4.2) (pathname ftp stream fast?
							 &aux data-stream line entry entry-list
							 in-subdirectory total-line-p open-name directory-name)
  (let ((sys:*new-line-delimiter* '(#\newline #\linefeed))) ;101-27-88 DAB*
    (declare (special sys:*new-line-delimiter* ))
    (BLOCK bsd4.2
    (LOOP
      ;1; open-name is equivalent to parameter pathname considering directory link translation*
      ;1; directory-name is the pathname actually passed across the control connection*
      (SEND ftp :open
	    (PROG1 (SETF directory-name (dir-pathstring-for-ftp (SETF open-name (OR *ftp-directory-link* pathname))))
		   (SETF directory-name (parse-pathname directory-name nil pathname)))
	    ;1; Unix nlist returns a null listing for both nonexistant and empty directories*
	    ;1; Don't use nlist to speed up the listing on a recursive entry, or empty directories will appear as nonexistant*
	    :list)
      ;1; workaround a Unix 425 bug which seems to go away on the second open attempt*
      (UNLESS (AND (EQ 425 (SEND ftp :reply-code))
		   (SEARCH "Can't create data socket" (SEND ftp :reply-string))
		   (SEARCH "Address already in use" (SEND ftp :reply-string)))
	(RETURN)))
    (UNLESS (or (ftp-unfavorable (SEND ftp :reply-code))
		(not (SEND ftp :data-connection)))   ;103-03-88 DAB*
      (SETF data-stream (SEND ftp :data-connection))
      ;1; work around Unix bug of giving favorable status for a nonexistant directory*
      (WHEN (NULL (SETF line (READ-LINE data-stream nil)))
	(COND
	  ;1; if FTP list command on parent directory, then directory not found*
	  ((OR *ftp-recursive-directory-list-from-ftp*
	       (NOT (pathname-equal directory-name open-name)))
	   (SETF (SEND ftp :reply-code) 450)
	   (SETF (SEND ftp :reply-string) (FORMAT nil "Directory not found for ~a" pathname))
	   (RETURN-FROM bsd4.2 nil))
	  ;1; if FTP list command on root directory, then file not found *
	  ((EQL :root (SEND pathname :directory))
	   (RETURN-FROM bsd4.2 nil))
	  ;1; must check existence of parent directory to make decision*
	  (t (LET ((*ftp-recursive-directory-list-from-ftp* t))
	       (SEND ftp :close)
	       (SEND self :directory-list-from-ftp :bsd4.2
		     (SEND pathname :directory-pathname-as-file) ftp stream fast?)
	       ;1; reply code is set by the recursive call*
	       (RETURN-FROM bsd4.2 nil)))))
      ;1; work around Unix bug of giving favorable status for a protected directory*
      (WHEN (SEARCH (STRING-APPEND (SEND directory-name :string-for-host) " unreadable") line)
	(SETF (SEND ftp :reply-code) 450)
	(SETF (SEND ftp :reply-string) (FORMAT nil "~a: Permission denied" pathname))
	(RETURN-FROM bsd4.2 nil))
      ;1; when recursively entered, we were only checking the existance of the parent directory, so exit.*
      ;1; (Note also that Unix will chase links with the NLST comand, so we don't have to worry about links.)*
      (IF *ftp-recursive-directory-list-from-ftp*
	  (RETURN-FROM bsd4.2 nil)
	  (add-directory-stream stream data-stream))
      (MULTIPLE-VALUE-SETQ (entry in-subdirectory total-line-p)
	(bsd4.2-directory-line-parse pathname directory-name open-name line fast? in-subdirectory))
      ;1; They gave us /d1/d2.  Unix lists d2 if it is a directory.*
      ;1; We considered d2 as the file spec and thus don't match anything if d2 is a directory.*
      ;1; Assumption is based on the fact that we saw the total line first (e.g. total n) when only a single file was requested.*
      (WHEN (AND total-line-p (pathname-equal directory-name open-name))
	(RETURN-FROM bsd4.2 (LIST (LIST pathname :directory t))))
      ;1; Workaround Unix feature of not chasing the link path with the LIST command (and returning the link information instead),*
      ;1; when we were listing the parent directory.  We throw out with a single entry, which needs its link chased.*
      (WHEN (AND (GETF (CDR entry) :link-to)
		 (NOT (pathname-equal directory-name open-name))
		 (pathname-equal (FIRST entry) directory-name))
	(THROW :directory-link-error (LIST entry)))
      (LOOP
	(WHEN entry (PUSH entry entry-list))
	(UNLESS (SETF line (READ-LINE data-stream nil))
	  (RETURN-FROM bsd4.2 entry-list))
	(MULTIPLE-VALUE-SETQ (entry in-subdirectory total-line-p)
	  (bsd4.2-directory-line-parse pathname directory-name open-name line fast? in-subdirectory)))))))

(DEFUN bsd4.2-directory-line-parse (dirobj directory-name open-name line fast? in-subdirectory
				    &aux pathobj result start cursor cursor2 year day month
				    (minutes 0) (hours 0) (seconds 0) time-or-year link-ix device)
  "2Open-name is equivalent to parameter pathname considering directory link translation.
Directory-name is the pathname actually passed across the control connection.*"
  ;1; FORMATS:*
  ;1;   total n*
  ;1;   {n}  [d|-|]rwxrwxr-- n kari   1152 Apr 10 1985  chess*
  ;1;          "                            Jun 7  11:48  foo.bar *
  ;1;   {n}  l              "                                       -> link translation*
  ;1;   {n}  [b|c]rw-rw-rw- n xxxx   m,n   "*
  ;1;   directory-pathname:*
  (SETQ start (POSITION #\Space (THE string (STRING line)) :test-not 'CHAR-EQUAL))
  (COND
    ;1; throw out ignored lines*
    ((NULL start)  ;1; ignore blank lines*
     (VALUES nil in-subdirectory))
    ((AND (> (LENGTH line) 5)  ;1; the totals line is ignored*
	  (STRING-EQUAL (SUBSEQ line start (+ start 6)) "total "))
     (VALUES nil in-subdirectory t))
    ;1; top-level directory name (followed by subordinate files)*
    ((CHAR-EQUAL (AREF line (SETQ cursor2 (1- (LENGTH line)))) #\:)
     (SETF (AREF line cursor2) #\Space) (SETQ in-subdirectory t)
     (LET ((p1 (IGNORE-ERRORS (parse-pathname line nil dirobj))))
       (IF (PATHNAMEP p1)
	   (PROGN
	     (OR (PATHNAME-NAME p1) (SETQ p1 (SEND p1 :new-name :unspecific)))
	     (OR (PATHNAME-TYPE p1) (SETQ p1 (SEND p1 :new-type :unspecific)))
	     (SETQ pathobj (IGNORE-ERRORS (merge-pathname-defaults p1 dirobj))))
	   (SETQ pathobj nil)))
     (VALUES (WHEN pathobj
	       (IF fast?
		   (LIST pathobj)
		   (LIST pathobj :directory t :dired-echo "")))
	     in-subdirectory))
    ;1; don't include subordinate files (they don't make any sense to dired at this point)*
    (in-subdirectory (VALUES nil t))
    ;1; regular lines *
    (:otherwise
     (WHEN (AND (SETF cursor (STRING-SEARCH-SET "dbcp-l" line))	;1 Get start of mode*
		(< (+ cursor 3) (LENGTH line))	;1 don't run off the end*
		(OR (EQUAL (AREF line (+ cursor 1)) #\r)	;1 next character is r or*
		    (EQUAL (AREF line (+ cursor 1)) #\-))	;1 a dash.*
		(OR (EQUAL (AREF line (+ cursor 2)) #\w)	;1 next character is w or*
		    (EQUAL (AREF line (+ cursor 2)) #\-))	;1 a dash.*
		(OR (EQUAL (AREF line (+ cursor 3)) #\x)	;1 next character is x or*
		    (EQUAL (AREF line (+ cursor 3)) #\-)))	;1 a dash.*
       (SELECTOR (AREF line cursor) char-equal
	 (#\d (SETF (GETF result :directory) t))
	 (#\l
	  ;1; This link really needs to be chased - do it when the control connection is free in modify-dir-list.*
	  ;1; When the link is chased, we will insert the :directory property, if necessary.  Note that the*
	  ;1; :directory and :link-to properties are the only ones that refer to the end of the link chain.*
	  ;1; All other properties refer to the link itself.*
	  (SETF cursor2 (SEARCH " ->" (STRING line) :from-end t :test 'CHAR-EQUAL))
	  (SETF link-ix (STRING-SEARCH-NOT-SET #\SPACE line (+ 3 cursor2))))
	 ((#\b #\c) (SETQ device t))))
     (MULTIPLE-VALUE-SETQ (cursor cursor2)
       (backup-token line (WHEN link-ix cursor2)))
     (LET ((p1 (parse-pathname (SUBSEQ line cursor cursor2) nil dirobj)))
		  (OR (PATHNAME-NAME p1) (SETQ p1 (SEND p1 :new-name :unspecific)))
		  (OR (PATHNAME-TYPE p1) (SETQ p1 (SEND p1 :new-type :unspecific)))
		  (SETQ pathobj (merge-pathname-defaults p1 dirobj)))
     ;1; we cannot determine the link-name until pathobj has been determined*
     ;1; merge-pathnames is necessary here in case link-name has a :relative or :up directory component*
     (WHEN (AND pathobj link-ix)
       (SETF (GETF result :link-to)
	     (MERGE-PATHNAMES
	       (parse-pathname (SUBSEQ line link-ix (STRING-SEARCH-SET #\SPACE line link-ix)) nil dirobj)
	       (IF (AND (NOT (pathname-equal directory-name open-name))
			(pathname-equal pathobj directory-name))
		   directory-name
		   open-name))))
     (COND
       ((NULL pathobj) (VALUES nil in-subdirectory))
       (fast? (VALUES (LIST pathobj) in-subdirectory))
       (:otherwise
	;1; CREATION DATE*
	(SETQ cursor (backup-token line cursor))
	(MULTIPLE-VALUE-SETQ (time-or-year cursor2)
	  (PARSE-INTEGER line :start cursor :junk-allowed t))
	(IF (CHAR-EQUAL #\: (AREF line cursor2))
	    (PROGN
	      (SETQ hours time-or-year)
	      (SETQ minutes (PARSE-INTEGER line :start (1+ cursor2) :junk-allowed t)))
	    ;1; else*
	    (SETQ year time-or-year))
	(SETQ cursor (backup-token line cursor))
	(SETQ day (PARSE-INTEGER line :start cursor :junk-allowed t))
	(SETQ cursor (backup-token line cursor))
	(SETQ month (translate-alphabetic-month (SUBSEQ line cursor (+ cursor 3))))
	(WHEN (AND (NUMBERP seconds) (NUMBERP minutes) (NUMBERP hours) (NUMBERP day) (NUMBERP month))
	  (MULTIPLE-VALUE-BIND (this-sec this-min this-hour this-day this-month this-year) (time:get-decoded-time)
	    (IGNORE this-sec this-min this-hour this-day)
	    ;1; Year appears only if date is > 180 days old.  Otherwise decide whether assumed year is last year or this year.*
	    ;1; Note that July 1st is either day 181 (non-leap year) or day 182 (leap year).*
	    (UNLESS year
	      (SETF year (IF (>= month (+ this-month 6))
			     (1- this-year)
			     this-year)))
	    (SETF (GETF result :creation-date) (time:encode-universal-time seconds minutes hours day month year)))) 
	;1; LENGTH-IN-BYTES *
	(UNLESS device
	  (SETQ cursor (backup-token line cursor))
	  (SETF (GETF result :length-in-bytes) (PARSE-INTEGER line :start cursor :junk-allowed t)))
	;1; DIRED-ECHO*
	(SETF (GETF result :dired-echo) (SUBSEQ line 0 cursor))
	(PUSH pathobj result))))))

 


(DEFUN backup-token (line cursor &aux start end)
  (SETQ end
	(POSITION #\Space (THE string (STRING line)) :from-end t :end cursor :test-not
		  'CHAR-EQUAL))
  (SETQ start
	(POSITION #\Space (THE string (STRING line)) :from-end t :end end :test 'CHAR-EQUAL))
  (VALUES (1+ (or start 0)) (1+ end)))     ;101-26-88 DAB *


(DEFPARAMETER *ftp-extended-tops20-dired* nil
   "2When set to T, does additional work to determine properties of files in directory (substantially slower);
   When set to nil, will show properties only for a single file;
   When set to :ask, will query each time;
   When set to :choose, will present a cvv window to set variable value.*") 


(DEFMETHOD (FTP :case :directory-list-from-ftp :tops20) (PATHNAME ftp stream fast?
							 &aux list-type data-stream entry dir-list line)
 ;1; TOPS-20 only gives directory information for a single file.  If requesting more than one file*
 ;1; NLIST is formatted better (one file per line, complete pathnames)*
  (SETQ list-type (IF (AND (SEND pathname :name) (NOT (SEND pathname :wild-p)))
		    :list
		    :nlist))
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) list-type))
    (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
    (SETQ dir-list
	  (LOOP for line = (READ-LINE data-stream nil) while line when
	     (SETQ entry (tops20-directory-line-parse pathname line fast?)) collecting entry))
    (WHEN (AND (NOT fast?) (EQ list-type :nlist) (check-extend-option pathname))
     ;1; Do  a LIST for each NLIST entry*
      (SEND ftp :close)
      (SETQ dir-list
	    (LOOP for fspec in dir-list collecting
	       (OR
		(IF (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp (CAR fspec)) :list))
		  (PROGN
		    (SEND ftp :set-reply-code t);1don't flag an error when we return*
		    nil)
		  ;1; else *
		  ;1;  make who line show cumulative byte count*
		  (PROGN
		    (WITHOUT-INTERRUPTS
		     (SEND stream :set-count
			(+ (SEND stream :count) (SEND data-stream :read-pointer)))
		     (SEND stream :set-data-stream
			(SETQ data-stream (SEND ftp :data-connection))))
		    (SETQ line (READ-LINE data-stream nil))
		    (SEND ftp :close :abort)
		    (AND line (SETQ entry (tops20-directory-line-parse pathname line nil))
		       (SEND (CAR fspec) :pathname-match-no-device (CAR entry) nil) entry)))
		fspec))))
    dir-list)) 


(DEFUN check-extend-option (dirobj)
  (WHEN (EQ *ftp-extended-tops20-dired* :choose)
    (SETQ *ftp-extended-tops20-dired* nil)
    (tv:choose-variable-values
     '("" " This variable can be set to control a TOPS-20 directory listing via FTP."
       " File properties are normally included only when listing a single file."
       " Including these for multiple files can take substantially longer." "" ""
       (*ftp-extended-tops20-dired* "   FS:*Ftp-Extended-TOPS20-Dired*" :documentation
	"NIL => never do an extended list, :ASK => query each time, T => always do an extended list"
	:choose (nil :ask t)) "")
     :margin-choices '("Done []") :label "TOPS-20 FTP/Dired Option"))
  (IF (EQ *ftp-extended-tops20-dired* :ask)
    (Y-OR-N-P "Show extended directory list for ~A?" dirobj)
    *ftp-extended-tops20-dired*)) 


(DEFUN tops20-directory-line-parse (dirobj line &optional fast? &aux entry (dired-echo "") cursor cursor2 save-cursor pathobj
  blocks day month year hour minute second ch new-version)
 ;1; FORMAT:*
 ;1;   (a) <ATP.USER.JFK>GC.LISP.55 or*
 ;1;   (b) <ATP.USER.JFK>GC.LISP.55;P777700;AMAED304, 9, 11-Nov-85 11:43:39, {some number of additional dates} + ???*
 ;1;                                  protection account  blks creation-date        update, reference, tape-written, on-line-exp, off-line-exp*
 ;1;*
 ;1; First format is from NLIST of more than one file; second is from LIST of one file.  NLIST of one file still tacks on protection*
 ;1; and account information.  LIST of more than one file may do things like this (supposedly avoided above)*
 ;1;      <ATP.USER.JFK>*
 ;1;      X.TEMP.13,14*
 ;1;      X.FOO*
 ;1;      Z.TEXT.1,15*
  (SETQ cursor (POSITION #\Space (THE string (STRING line)) :test-not 'CHAR-EQUAL))
  (UNLESS (OR (NULL cursor) (CHAR-EQUAL (AREF line cursor) #\?))
    (SETQ cursor2 (STRING-SEARCH-SET "; ," line cursor))
    (SETQ pathobj (parse-pathname (SUBSEQ line cursor cursor2) (SEND dirobj :host)))
    (WHEN (AND pathobj (SEND pathobj :name));1make sure not just a directory *
      (SETQ pathobj (MERGE-PATHNAMES pathobj dirobj))
      (UNLESS fast?
	(COND
	  ((NULL cursor2))
	  ((CHAR-EQUAL (SETQ ch (AREF line cursor2)) #\,)
	   ;1;this shouldn't really happen (list of version numbers) - use greatest*
	   (WHEN (NUMBERP
	     (SETQ new-version
		   (PARSE-INTEGER line :start
				  (1+
				   (POSITION #\, (THE string (STRING line)) :from-end t :test
					     'CHAR-EQUAL))
				  :junk-allowed t)))
	     (SETQ pathobj (SEND pathobj :new-version new-version))))
	  ((CHAR-EQUAL ch #\Space)
	   ;1; This isn't a known case, but it is a pretty reasonable one *
	   (WHEN (SETQ cursor
		  (POSITION #\Space (THE string (STRING line)) :start cursor2 :test-not
			    'CHAR-EQUAL))
	     (SETQ dired-echo (SUBSEQ line cursor))))
	  ((CHAR-EQUAL ch #\;)
	   (SETQ cursor
		 (POSITION #\, (THE string (STRING line)) :start cursor2 :test 'CHAR-EQUAL))
	   (SETQ dired-echo (SUBSEQ line cursor2 cursor))
	   (SETQ save-cursor cursor);1used for recovery from unsuccessful parse*
	   (WHEN (AND cursor
	       (NUMBERP
		(MULTIPLE-VALUE-SETQ (blocks cursor)
		  (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t))))
	     (PUSH blocks entry)
	     (PUSH :length-in-blocks entry)
	     (SETQ save-cursor (1+ cursor)))
	   (WHEN (AND cursor
	       (SETQ cursor
		     (POSITION #\Space (THE string (STRING line)) :start (1+ cursor) :test-not
			       'CHAR-EQUAL))
	       (< (+ cursor 18)));1room for date & time*
	     (WHEN (AND
	       (MULTIPLE-VALUE-SETQ (day cursor)
		 (PARSE-INTEGER line :start cursor :junk-allowed t))
	       (< day 0))
	      ;1; trailing dash can make it look negative*
	       (SETQ day (ABS day))
	       (DECF cursor))
	     (WHEN (AND cursor (< (SETQ cursor2 (+ cursor 4)) (LENGTH line)))
	       (SETQ month (translate-alphabetic-month (SUBSEQ line (1+ cursor) cursor2))))
	     (WHEN cursor2
	       (MULTIPLE-VALUE-SETQ (year cursor)
		 (PARSE-INTEGER line :start (1+ cursor2) :junk-allowed t)))
	     (WHEN cursor
	       (MULTIPLE-VALUE-SETQ (hour cursor)
		 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t)))
	     (WHEN cursor
	       (MULTIPLE-VALUE-SETQ (minute cursor)
		 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t)))
	     (WHEN cursor
	       (MULTIPLE-VALUE-SETQ (SECOND cursor)
		 (PARSE-INTEGER line :start (1+ cursor) :junk-allowed t)))
	     (WHEN (AND (NUMBERP month) (NUMBERP day) (NUMBERP year) (NUMBERP hour) (NUMBERP minute)
		 (NUMBERP second))
	       (PUSH (ENCODE-UNIVERSAL-TIME second minute hour day month year) entry)
	       (PUSH :creation-date entry)
	       (SETQ save-cursor (1+ cursor))))
	   (WHEN save-cursor
	     (SETQ dired-echo (STRING-APPEND (SUBSEQ line save-cursor) " " dired-echo)))))
	(WHEN (ftp-is-directory pathobj)
	  (PUSH t entry)
	  (PUSH :directory entry))
	(PUSH dired-echo entry)
	(PUSH :dired-echo entry));1 end unless fast*
      (PUSH pathobj entry))))


(DEFMETHOD (FTP :case :directory-list-from-ftp :msdos) (PATHNAME ftp stream fast?
				   &aux data-stream entry in-subdirectory)
  (UNLESS (ftp-unfavorable (SEND ftp :open (dir-pathstring-for-ftp pathname) :list))
    (add-directory-stream stream (SETQ data-stream (SEND ftp :data-connection)))
    (LET* ((first-line (READ-LINE data-stream nil))
	   (directory-line-parser
	     (IF (STRING-EQUAL "" first-line)
		 'msdos-directory-line-parse-karn  ;1;The first line of the Karn listing is blank*
		 'msdos-directory-line-parse-romkey)))
      (LOOP
	for line first first-line then (READ-LINE data-stream nil)
	while line
	do
	(MULTIPLE-VALUE-SETQ (entry in-subdirectory)
			     (FUNCALL directory-line-parser pathname line fast? in-subdirectory))
	when entry collecting entry))
    )
  )


(DEFUN msdos-directory-line-parse-karn (dirobj line fast? in-subdirectory
				   &aux pathobj result 
				   directory-p
				   )
  (DECLARE (STRING line))
  
  ;1; MSDOS format: (KARN version)*
  ;1; foobarrr txt  1899  1-01-86  12:34a  *
  
  
  ;1;For now, ignore "." link to parent (or whatever)*
  
  (COND ((OR (STRING-EQUAL line "")
	     (CHAR-EQUAL (CHAR line 0) #\space)
	     (CHAR-EQUAL (CHAR line 0) #\.)	;1don't mess with silly ms-dos alias pointers*
	     )
	 ;1;Info line is ignored*
	 (VALUES nil in-subdirectory))
	;1;Case of empty directory*
	((STRING-EQUAL line "No Files")
	 (VALUES nil in-subdirectory))
	(t
	 (WHEN (SEARCH "<DIR>" line)
	   (SETQ directory-p t))
	 (UNLESS fast?
	   ;1;get the creation date*	1   *
	   (PUSH (time:parse-universal-time (STRING-APPEND line "m") ;1this kludge brought to you by MSDOS*
					    22)
		 result)
	   (PUSH :creation-date result)
	   ;1;length-in bytes*
	   (WHEN (NOT directory-p)
	     (PUSH (PARSE-INTEGER line :start 13 :junk-allowed t) result)
	     (PUSH :length-in-bytes result))
	   ;1;directory*
	   (WHEN directory-p
	     (PUSH t result)
	     (PUSH :directory result))
	   ;1;dired-echo*
	   (PUSH "" result) ;1;No useful information there*
	   (PUSH :dired-echo result))
	 ;1;get actual pathname*
	 (SETQ pathobj (MAKE-PATHNAME :defaults dirobj
				      :name (SUBSEQ line 0 (POSITION #\space line))
				      :type (LET ((type-spec (SUBSEQ line 9 (POSITION #\space line :start 9))))
					      (IF (STRING-EQUAL "" type-spec)
						:unspecific
						type-spec))))
	 (PUSH pathobj result)
	 (VALUES result in-subdirectory))
	))

(DEFUN msdos-directory-line-parse-romkey (dirobj line fast? in-subdirectory
				   &aux pathobj result 
				   directory-p
				   )
  (DECLARE (STRING line))
  
  ;1; MSDOS format: (ROMKEY version)*
  ;1; 2998  foo.bat  Wed Dec 04 16:40:18 1986*
  
  
  ;1;(SETQ start (STRING-SEARCH-NOT-CHAR #\space line))*
  ;1;;(PUSH line *lines*)*
  
  ;1;For now, ignore "." link to parent (or whatever)*
  
  (COND ((STRING-EQUAL line "No files")		;1 Case of empty directory*
	 (VALUES nil in-subdirectory))
	((CHAR-EQUAL (CHAR line 10) #\.)	;1 don't mess with silly ms-dos alias pointers*
	 (VALUES nil in-subdirectory))
	(t
	 (WHEN (STRING-EQUAL "<dir>" line :end2 5)
	   (SETQ directory-p t))
	 (UNLESS fast?
	   ;1;get the creation date*	1   *
	   (PUSH (time:parse-universal-time line 34) result)
	   (PUSH :creation-date result)
	   ;1;length-in bytes*
	   (WHEN (NOT directory-p)
	     (PUSH (PARSE-INTEGER line :junk-allowed t) result)
	     (PUSH :length-in-bytes result))
	   ;1;directory*
	   (WHEN directory-p
	     (PUSH t result)
	     (PUSH :directory result))
	   ;1;dired-echo*
	   (PUSH "" result) ;1;No useful information there*
	   (PUSH :dired-echo result))
	 ;1;get actual pathname*
	 (SETQ pathobj (LET* ((pathname-end (POSITION #\space  line :start 10))
			      (line-pathname (NSUBSTRING line 10 pathname-end)))
			 (LET* ((period-location (POSITION #\. (THE string line-pathname)))
				(line-name (IF period-location
					       (NSUBSTRING line-pathname 0 period-location)
					       line-pathname))
				(line-type (IF period-location
					       (NSUBSTRING line-pathname (1+ period-location))
					       :unspecific)))
			   (MAKE-PATHNAME :defaults dirobj 
					  :name line-name :type line-type))
			 ))
	 (PUSH pathobj result)
	 (VALUES result in-subdirectory))
	))


;1;; --- Methods dealing with FTP-IMPLEMENTATION-TYPE (used in directory list parsing)*
(DEFUN ftp-canonicalize-system-type (system-type)
  (SELECTOR system-type eq
    (:unix-ucb :unix)
    (:otherwise system-type)))

(DEFUN ftp-system-type (host)
   (ftp-canonicalize-system-type (SEND host :system-type)))

(DEFMETHOD (FTP :set-ftp-implementation-type) (host type)
  (host:set-host-attribute host :ftp-implementation-type type))

(DEFMETHOD (FTP :get-ftp-implementation-type) (host)
   (OR (host:get-host-attribute host :ftp-implementation-type)
       (SEND self :get-ftp-implementation-type-internal (ftp-system-type host) host)))

(DEFMETHOD (FTP :get-ftp-implementation-type-internal) (&rest ignore) nil)

(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :vms4) (IGNORE) :wollongong)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :unix) (IGNORE) :bsd4.2)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :tops20) (IGNORE) :tops20)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :lispm) (IGNORE) :explorer)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :lmfs) (IGNORE) :minimum)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :msdos) (IGNORE) :msdos)
(DEFMETHOD (FTP :case :get-ftp-implementation-type-internal :microExplorer) (IGNORE) :microExplorer)

;1;; --- Methods & functions dealing with names actually passed to FTP*
;1;; These functions are called extensively in FTP-STREAM*
(DEFUN hoststring-for-ftp (path)
  (SEND (PATHNAME-HOST path) :name-as-file-computer))

(DEFUN dir-pathstring-for-ftp (path)
  ;1; This syntax must be acceptable to the remote host*
  (OR (SEND *ftp-service* :string-for-ftp (ftp-system-type (PATHNAME-HOST path)) path)
      (SEND path :string-for-host))) 

(DEFUN pathstring-for-ftp (path)
 ;1; This syntax must be acceptable to the remote host*
  (SEND path :string-for-host)) 

(DEFMETHOD (FTP :string-for-ftp) (IGNORE ignore)
   nil)

(DEFMETHOD (FTP :case :string-for-ftp :unix) (pathname &aux result stop-ix save-name save-type)
  ;1; Careful of oddities in pathname caused by the fact that x.y is really just the name although the :type method*
  ;1; will return y as the type.*
  (SETQ result (SEND pathname :string-for-host))
  ;1; Unix wildcarding cannot be trusted.  Consider "unix:/usr/a/b/c*" where b is a nonexistant directory (directory a exists).*
  ;1; (send ftp :list "/usr/a/b/c*") will return the root directory for Sun and will abort the control connection for Ultrix.*
  (COND
    ;1; do our own wildcarding in the presence of any wildcard character*
    ((OR (EQ :wild (SETF save-name (SEND pathname :name)))
	 (EQ :wild (SETF save-type (SEND pathname :type)))
	 (STRING-SEARCH-SET "*" save-name)
	 (STRING-SEARCH-SET "*" save-type))
     (IF (SETF stop-ix (POSITION #\/ (THE string (STRING result)) :from-end t :test 'CHAR-EQUAL))
	 (SUBSEQ result 0 (IF (ZEROP stop-ix) 1 stop-ix))
	 result))
    (t result)))

(DEFMETHOD (FTP :case :string-for-ftp :vms4) (PATHNAME)
  (CASE (SEND *ftp-service* :get-ftp-implementation-type (PATHNAME-HOST pathname))
    (:wollongong-2.x
     ;1; This is required because FTP (wollongong) gives nothing for a :list of a single file and*
     ;1; because :nlist of a directory with any kind of file spec lists the home directory instead of what was requested*
     (SEND pathname :real-string-for-directory))
    (otherwise (SEND pathname :string-for-host))))
 
;1;; -- Miscellaneous system-type dependent stuff*
(DEFUN ftp-omit-byte-size (path)
  (SEND *ftp-service* :omit-byte-size (ftp-system-type (PATHNAME-HOST path))))

(DEFMETHOD (FTP :omit-byte-size) (IGNORE)
   nil)

(DEFMETHOD (FTP :case :omit-byte-size :unix) ()
;1; (sigh) Some unix servers don't like a byte-size other than 8*
   :unix)


;1;;----------------------------------------------------------*
;1;; --- Methods & functions dealing with getting user login information*
;1;;----------------------------------------------------------*

(DEFPARAMETER *ftp-account-prompt* "Account (optional): "
  "2The string used by FTP when prompting for account on login (or NIL to skip this prompt)*")

(DEFMETHOD (FTP :login-user) (host)
  ;1;; Default login - will prompt if user-id/password is not known for this host*
  ;1; Some systemt types may not require this*
  (SEND self :login-user-internal (ftp-system-type host) host))

(DEFMETHOD (FTP :login-user-internal) (IGNORE host)
  (get-user-information host nil))

(DEFMETHOD (FTP :case :login-user-internal :lispm) (IGNORE)
  (UNLESS user-id
    (force-user-to-login))
  (VALUES user-id ""))

(DEFMETHOD (FTP :case :login-user-internal :lmfs) (IGNORE)
  (UNLESS user-id
    (force-user-to-login))
  (VALUES user-id ""))

(DEFMETHOD (FTP :case :login-user-internal :MicroExplorer) (IGNORE)
  (UNLESS user-id
    (force-user-to-login))
  (VALUES user-id ""))

;1;; Login utility functions*
(DEFUN reset-user-information (host)
  (get-user-information host :always-prompt)
  "User information updated") 

(DEFUN get-user-information (host &optional (always-prompt t))
  (UNLESS (TYPEP host 'si:basic-host)
    (SETQ host (si:parse-host host)))
  (IF (AND *ftp-account-prompt* (host:get-host-attribute host :ftp-prompt-for-account))
    ;1; THEN ask for account *
    (file-get-password-etc host nil nil always-prompt *ftp-account-prompt*)
    ;1; ELSE just default account*
    (MULTIPLE-VALUE-BIND (user pass)
       (file-get-password-etc host nil nil always-prompt)
       (VALUES user pass ""))))

(DEFUN add-user-information (host user &optional (password "") (account ""))
  "2Non-interactive way of setting user information for a given host (to be used on subsequent file-system access)*"
  (file-host-user-id user host)
  (store-password-etc user host password (LIST *ftp-account-prompt* account))
  host) 					;1return value*

(DEFUN delete-user-information (host user &rest ignore)
  (delete-password-etc user host)
  :delete-user-information)



;1;;----------------------------------------------------------------*
;1;;-------- PEEK*
;1;;----------------------------------------------------------------*

(DEFMETHOD (FTP :peek-file-system) (host)
  (peek-file-system-ftp host))

;1;;----------------------------------------------------------------*
;1;;-------- CONTROL CONNECTION MAINTENANCE*
;1;;----------------------------------------------------------------*

(DEFMETHOD (FTP :reset) (medium host)
  (DECLARE (IGNORE medium))
   (SEND self :close-all-control-connections host))

(DEFMETHOD (FTP :close-all-control-connections) (host)
   (DOLIST (conn (GET host :active-ftp-control-connections))      
      (IGNORE-ERRORS (free-ftp-connection conn :abort-all)))
   (SETF (GET host :active-ftp-control-connections) nil))

(DEFMETHOD (FTP :add-control-connection) (host conn)
   (WITHOUT-INTERRUPTS
       (PUSH conn (GET host :active-ftp-control-connections))))

(DEFMETHOD (FTP :delete-control-connection) (host conn)
   (WITHOUT-INTERRUPTS
     (SETF (GET host :active-ftp-control-connections)
	   (DELETE conn (GET host :active-ftp-control-connections) :test 'EQ))))

(DEFMETHOD (FTP :active-control-connections) (host)
  (GET host :active-ftp-control-connections))

(ADD-INITIALIZATION "Close FTP connections" '(reset-ftp-service :enable) '(:logout :normal)) 

(DEFVAR *ftp-gc-on* t) 				;1T to kill old control connections*

(DEFPARAMETER *ftp-gc-interval* 30) 		;1Time in minutes dormant connection will live (1t < TTL  < 2t)*

(DEFPARAMETER *ftp-gc-notify* nil) 		;1T for debug notifications*

;1;;-------------------*
;1;; Clean up old FTP control connections as follows:*
;1;;   Every time a reserve-ftp-connection is done set the GC-MARK in the control connection to nil*
;1;;   Every time the GC process wakes up, test the GC-MARK in active control connections:*
;1;;     If nil, the connection has been used since last wake-up, set T for next time around*
;1;;     If T, the connection has not been used since last wake-up, close and delete the connection*
;1;;-------------------*


(DEFUN ftp-gc (&aux kill free? control-tcp-connection)
  (LOOP
    (PROCESS-SLEEP (* *ftp-gc-interval* 3600))
    (WHEN *ftp-gc-notify*
      (tv:notify nil "FTP GC: On=~A  Interval=~A" *ftp-gc-on* *ftp-gc-interval*))
    (WHEN *ftp-gc-on*
      (DOLIST (host host:*host-list*)
	(DOLIST (cc (SEND *ftp-service* :active-control-connections host))
	  ;1; If the connection is dead, delete it regardless of GC status*
	  (UNLESS (SETQ kill (OR (NOT (SETQ control-tcp-connection (SEND cc :control-connection)))
				 (EQ :closed (SEND control-tcp-connection :status))))
	    (WITHOUT-INTERRUPTS
	      (IF (AND (SETQ free? (NOT (SEND cc :in-use))) (SEND cc :send-if-handles :gc-mark))
		  ;1; THEN hasn't been reserved since last gc*
		  (SEND cc :set-in-use (SETQ kill "GC"))	;1this will keep it from being grabbed while we kill it*
		  ;1; else set to T for next time (unless still in use)*
		  (SEND cc :send-if-handles :set-gc-mark free?))))
	  (WHEN kill
	    (WHEN *ftp-gc-notify*
	      (tv:notify nil (FORMAT nil "FTP GC: Killing ~A @ ~A" cc host)))
	    (IGNORE-ERRORS (SEND cc :close-connections :abort))
	    (SEND *ftp-service* :delete-control-connection host cc))))))) 


(DEFVAR *ftp-gc-process* nil)  


(DEFVAR *ftp-reuse-control-connections* t) 

;1;; RESERVE-FTP-CONNECTION*

(DEFUN reserve-ftp-connection (host-object &optional (in-use-reason t) &aux (conn nil))
  (WHEN *ftp-reuse-control-connections*
    (LOOP
      ;1; critical section to find an available connection*
      (WITHOUT-INTERRUPTS
	(SETQ conn
	      (DOLIST (c (SEND *ftp-service* :active-control-connections host-object))
		(UNLESS (SEND c :in-use)
		  (SEND c :set-in-use in-use-reason)
		  ;1; Indicate to GC that we have used during current interval*
		  (SEND c :send-if-handles :set-gc-mark nil)
		  (RETURN c)))))
      (UNLESS conn
	(RETURN))
      ;1; validate connection and remove from list if bad*
      (WHEN (validate-ftp-connection host-object conn)
	(RETURN))))
  (UNLESS conn
    (LET ((aborted t))
      (UNWIND-PROTECT
	  (PROGN
	    (SETQ conn (connect-to-ftp host-object))
	    (login-to-ftp host-object conn t)
	    (WHEN *ftp-reuse-control-connections*
	      (SEND conn :set-in-use in-use-reason)
	      (SEND *ftp-service* :add-control-connection host-object conn))
	    (SETQ aborted nil))
	(WHEN (AND aborted conn)
	  (SEND conn :close-connections :abort)))))
  conn)						;1return the connection*


;1;; VALIDATE-FTP-CONNECTION*

(DEFUN validate-ftp-connection (host conn &aux control-connection (good-connection nil))
  (WHEN (SETF control-connection (SEND conn :control-connection))
    (CONDITION-BIND ((nil 'validate-ftp-connection-error-handler))
      (CATCH 'ftp-validation-error
	(WHEN (AND conn (EQ (SEND control-connection :status) :established))
	  ;1; validate control connection & reset mode to default (e.g. for directory-listings)*
	  (UNLESS (ftp-unfavorable (SEND conn :type :ascii))	;1TCP may also signal error here*
	    (SETQ good-connection t))))))
  (UNLESS good-connection
    (SEND *ftp-service* :delete-control-connection host conn))
  good-connection) 


(DEFUN validate-ftp-connection-error-handler (condition)
  (UNLESS (SEND condition :dangerous-condition-p)
    (THROW 'ftp-validation-error
	   nil)))  

;1;; FREE-FTP-CONNECTION*

(DEFUN free-ftp-connection (conn &optional abort-all abort-data-only &aux host)
  (ip:with-stream-whostate
    "Close"
    (WHEN conn
      (UNWIND-PROTECT
	  (COND (abort-all (SEND conn :close-connections :abort))
		(t
		 (SEND conn :close abort-data-only)	;1close the data connection (with optional abort to discard pending input)*
		 (UNLESS *ftp-reuse-control-connections* (SEND conn :quit))))
      (IF *ftp-reuse-control-connections*
	  (SETF (SEND conn :in-use) nil)
	  (WHEN (SETQ host (SEND conn :host)) (SEND *ftp-service* :delete-control-connection host conn)))))))


;1;; RESET-FTP-SERVICE*

(DEFUN reset-ftp-service (&optional enable-p)
  (WHEN (TYPEP *ftp-service* 'FTP)
    (WHEN *ftp-gc-process*
      (WITHOUT-INTERRUPTS
	(SEND *ftp-gc-process* :kill t)
	(SETF *ftp-gc-process* nil)))
    (DOLIST (h host:*host-list*)
      (WHEN (ftp-host-p h)
	(SEND *ftp-service* :reset :ftp h))))
  (COND ((NOT enable-p)
	 (SETF *ftp-service* #'(lambda (&rest ignore)
				 (FERROR 'ftp-error "FTP Service is not enabled."))))
	(t
	 (SETF *ftp-gc-process* (PROCESS-RUN-FUNCTION
				  '(:name "Dormant FTP Connection GC" :priority 0) 'ftp-gc))
	 (SETF *ftp-service* (host:find-service-implementation :ftp))))
  "FTP Reset Complete")



;1;;--------------------------*
;1;;----- FTP INTERFACE ROUTINES*
;1;;--------------------------*

(DEFUN fs-call-ftp (pathobj error-p msg &rest args &aux ftp hostobj ftp-result finished-p)
  ;1; issue main-cmd in between login and quit, check status and return error-condition or nil *
  ;1; also return info-list*
  (SETQ hostobj (SEND pathobj :host))
  (UNWIND-PROTECT
      (PROGN
	;1; Establish the connection*
	(SETQ ftp (reserve-ftp-connection hostobj msg))	;1last arg is nicety for peek*
	;1; issue the main msg*
	(APPLY ftp msg args)
	;1; Check status, signal error if error-p or return condition *
	(SETQ ftp-result (check-ftp-status pathobj ftp error-p))
	(SETF finished-p t))
    ;1; cleanup*
    (free-ftp-connection ftp (OR (TYPEP ftp-result 'condition) (NOT finished-p))))
  ftp-result) 


(DEFUN connect-to-ftp (hostobj &aux ftp)
  (ip:with-stream-whostate
    "Connect"
    (SEND (SETQ ftp (MAKE-INSTANCE 'ip::ftp-control-connection)) :get-connection  hostobj)
    (check-ftp-status hostobj ftp t)		;1signal error no matter what*
    ftp))					;1return connection object*


(DEFUN login-to-ftp (host control-connection &optional (error-p nil) &aux uid pw acct ftp-result (try-max 3))
  (ip:with-stream-whostate
    "Login"
    (LOOP for try from 1 to try-max do
	  ;1; If a host requires login, will check the host/password alist or prompt user.*
	  (MULTIPLE-VALUE-SETQ (uid pw acct)
	    (SEND *ftp-service* :login-user host))
	  (IF (ftp-unfavorable (SETQ ftp-result (SEND control-connection :login uid pw acct)))
	      (PROGN
		;1; error on login - don't remember user-id (next time will prompt)*
		(delete-user-information host uid)
		(IF (EQL try try-max)
		    (RETURN (check-ftp-status host control-connection error-p))
		    ;1; else*
		    (tv:notify nil (SEND control-connection :reply-string))))
	      ;1; else*
	      (RETURN ftp-result))))) 



;1;;------------------------------*
;1;; ----- ERROR HANDLING ----------*
;1;;------------------------------*

(DEFFLAVOR ftp-base-error-flavor
	   ()
	   (file-error ip:tcp-stream-error)) 


(DEFSIGNAL ftp-error ftp-base-error-flavor (PATHNAME operation)) 


(DEFSIGNAL ftp-open-error (ftp-base-error-flavor file-not-found file-lookup-error)
   (PATHNAME operation)) 


(DEFSIGNAL ftp-create-error (ftp-base-error-flavor file-already-exists creation-error)
   (PATHNAME operation)) 



(DEFCONSTANT *ftp-unfavorable-code* 300) 

;1;; FTP-UNFAVORABLE*

(DEFUN ftp-unfavorable (status-code);1 arg may be a scalar (t, nil, #) or a list (with code at head)*
  (WHEN (CONSP status-code)
    (SETQ status-code (CAR status-code)))
  (OR (NULL status-code)
     (AND (NUMBERP status-code) (OR (ZEROP status-code) (>= status-code *ftp-unfavorable-code*))))) 

;1;; CHECK-FTP-STATUS*

(DEFUN check-ftp-status (context ftp-conn error-p &optional (free-before-signal nil))
  ;1; CONTEXT is usually a pathname object, but may sometimes be a host object (probably not used*
  ;1; in the format string of the error message*
  (WHEN (ftp-unfavorable (SEND ftp-conn :reply-code))
    (LET ((condition
	   (MAKE-CONDITION 'ftp-error (SEND ftp-conn :reply-string) context (SEND ftp-conn :in-use))))
      (WHEN error-p
	(WHEN free-before-signal
	  (free-ftp-connection ftp-conn :abort-all))
	(SIGNAL condition :proceed-types nil))
      condition))) 



;1;;--------------------------------------------------------------*
;1;;-------- HOST MANIPULATION*
;1;;--------------------------------------------------------------*

;1; Replaces the variable *ftp-hosts*.*
(DEFUN all-ftp-hosts ()
  (LOOP for host in host:*host-list*
	when (ftp-host-p host)
	collect host))

(DEFUN ftp-host-p (host)
  (UNLESS (TYPEP host 'si:basic-host)
     (SETQ host (si:parse-host host)))
  (SEND host :send-if-handles :service-p :file :tcp :ftp))


;1;; CREATE-FTP-PATHNAME-HOST*
;1;;    --- user callable*

(DEFUN create-ftp-pathname-host (name ip-addresses &key system-type
				 &optional (MACHINE-TYPE :unknown) file-system-type aliases
                                           ip-gateway-addresses override ftp-server-implementation-type 
                                 &aux usable-aliases old-host)
  "2Create a *TEMPORARY* FTP host for use in pathnames:
     REQUIRED:
       NAME is the primary name for the new host
       IP-ADDRESSES is an address or a list of addresses in any ip address format
       :SYSTEM-TYPE is :LISPM, :UNIX, :VMS, :VMS4, :TENEX, :TOPS-20, :MULTICS, :ITS, etc. 

     OPTIONAL:
       :ALIASES is a name or list of alternate names
       :OVERRIDE is T to override an existing host of the same name (not allowed for the local-host)
       :FTP-SERVER-IMPLEMENTATION-TYPE is a keyword used for implementation-specific directory parsing
       :MACHINE-TYPE is documentary (included in host object)

    OBSOLETE:
       :FILE-SYSTEM-TYPE only for Symbolics (must be :LMFS)
       :IP-GATEWAY-ADDRESSES *"
  (IGNORE ip-gateway-addresses)      
  (COND
    ((EQ file-system-type :lmfs) (SETQ system-type :lmfs))
    ((NULL system-type) (FERROR 'ftp-error "Parameter :SYSTEM-TYPE is required"))
;1x    ((NULL (GET SYSTEM-TYPE 'SI::SYSTEM-TYPE-FLAVOR))*
;1x     (FERROR 'FTP-ERROR "~A is not a known system type" SYSTEM-TYPE))*
    )

  (UNLESS (STRINGP name)
    (SETQ name (FORMAT nil "~A" name)))

  ;1; Note: probably safest to store a copy of the list even if all strings*
  (COND
    ((NULL aliases))
    ((CONSP aliases)
     (DOLIST (a aliases)
       (IF (STRINGP a)
	 (PUSH a usable-aliases)
	 (PUSH (FORMAT nil "~A" a) usable-aliases))))
    ((STRINGP aliases) (PUSH aliases usable-aliases))
    (:otherwise (PUSH (FORMAT nil "~A" aliases) usable-aliases)))
  (PUSH name usable-aliases)
  (DOLIST (a usable-aliases)
    (COND
      ((NULL (SETQ old-host (si:parse-host a t))))
      ((EQ old-host si:local-host) (FERROR 'ftp-error "Can't override the local host"))
      (override)
      (:otherwise (FERROR 'ftp-error "Host ~A is already defined, use :OVERRIDE T to replace." a))))

  (name:add-object
     name :host
     :attributes (LIST :alias-list usable-aliases
                       :system-type system-type
     :machine-type machine-type
     :site si:site-name
     '(:services :group) '((:file :tcp :ftp))
     :address (CONS :ip (IF (LISTP ip-addresses)
			    (MAPCAR 'net:ip-address-parser ip-addresses)
			    (LIST (net:ip-address-parser ip-addresses))))
     :ftp-implementation-type  ftp-server-implementation-type)
     :pre-delete t
     :local t)

  (si:parse-host name :no-error))
 

;1;;------------------*
(COMPILE-FLAVOR-METHODS ftp ftp-directory-stream) 


