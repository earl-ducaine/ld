
;;;-*- Mode:COMMON-LISP; Package:FS; BASE:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*-

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
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.*
;;1; Copyright (C) 1980, Massachusetts Institute of Technology*

;;; Revision:
;;;     04-20-88 DAB    Rolled back out fixes on 04-20-88. It was causing chaos hangs on Explorer II.
;;;     04-06-88 DAB    Fixes the problem of version limiting erroring off during close when the
;;;                     file is delete protected.
;;;	03.08.88 MBC	Make LMFS-PARSE-FOR-SERVER handle :WILD directory to properly support
;;;			MAC pathname's real :DEVICE.
;;;	02.08.88 MBC	Make LMFS-PARSE-FOR-SERVER not break "LM:dir;foo.bar" parse (w/ host).
;;;	02.03.88 MBC	Make LMFS-PARSE-FOR-SERVER use :parse-namestring and always parse against
;;;			si:local-host instead of blindly letting merge-pathname-defaults.
;;;     01-20-88 DAB    Fixed 4server-convert-known-file-property* to pass the proper *merge-timezone*
;;;                     to parse-directrory-date-property. 
;;;     11-30-87 DAB    Fixed foreign-pname-string and 4server-dirlist-single* expect TI-LISPM pathname types.
;;;     11-16-87 DAB    Added package binding to *system-package* if rfile-server.
;;;     09-10-87 DAB    Added code to support (OPEN "...." :Direction :IO) in QFILE.
;;;	04.14.87 MBC	Make :Delete-And-Expunge pathname method use old calling
;;;			args. ie. pass on pathname, just optional error-p.
  
1;;; Toplevel and parsers

;;; This is separate to allow debugging by recompiling rfile-server.*



(defun 4file-server* (&rest ignore)
  (rfile-server))  



(proclaim '(special *local-server-via-net*))  



(defvar 4server-login-id* 2"File Server"*)  


(defvar 4server-login* t)  


(defvar 4trace-server-enabled* ())  


(defvar 4server-traces* ())  


(defvar 4*lmfs-server-dont-answer-logins** ())  


(defvar 4server-protocol-version* 0.)  


(defvar 4server-openings)*  


(defvar 4alldatas)*  				1;all dataconn cells*


(defvar 4tid)*  					1;running transaction id*


(defvar 4conn-stream)*  				1;connection stream*


(defvar 4conn)*  					1;connection*


(defvar 4server-instance)*  			1;tag for props*


(defvar 4uname)*  


(defvar 4lmfs-server-lossages* ())  


(defvar 4lmfs-debug-server* ()
   2"T means file server processes do not handle most errors, so you can debug them.
Note that changing this may not take affect in existing servers."*)  


1;(DEFVAR LOCAL-HOST-PATHNAME)  *



(defvar 4*server-shutdown-message** ())  


(defvar 4*server-shutdown-time** 0.)  



(defun 4cv-time* (x)
  (time:print-universal-time x ()))  



(defun 4trace-server* (&optional (onoff t))
  (setq trace-server-enabled onoff))  



(defvar 4trap-error* ())  



(defmacro 4trap-lossage* ((condition-names id) code &body errors)
  `(condition-case-if (not lmfs-debug-server) (trap-error) ,code
      (,condition-names
       (notify-nicely () 3"File server got an error: ~a"* (send trap-error ':report-string)) ;07-13-88 DAB
       (push (list (cv-time (get-universal-time)) ',id (send trap-error ':report-string))
	  lmfs-server-lossages)
       ,@errors)))  

(defun foreign-pname-string (conn strings)
  ;; This functions allows the EXPLORER file server to work with a 3600.
  ;;This code was removed on 11-30-87 DAB. Symbolics need to have Explorer of host type
  ;; TI-LISPM. The old method was a klunge.
  ;(let ((host (si:get-host-from-address (chaos:foreign-address conn) ':chaos))
  ;	(sa))
  ; (when (and host (eq (send host :system-type) :lmfs))
  ;	  
  ;	  (setf sa (pathname (string-append (send host :name)  ":" (first strings))))
  ;	  
  ;	  (setf strings (namestring (make-pathname :host "lm"
  ;						   :directory (pathname-directory sa)
  ;						   :name (pathname-name sa)
  ;						   :type (pathname-type sa)
  ;						   :version (pathname-version sa))))
  ;	  (setf strings (list (subseq strings
  ;					 (1+ (position (int-char #\space) strings)))))))
  strings)

(defun rfile-server ()
  
  (if (and server-login (or (null user-id)
			    (zerop (length user-id))))
      (trap-lossage (error "Server Login")
		    (progn (login server-login-id si:local-host)
			   (print-server-login-exegesis))))
  (let ((*print-case* :upcase)
	(*print-length* nil)
	(*print-level* nil)
	(*package* *system-package*)   ;11-16-87 DAB
	(time:*default-date-print-mode* :mm/dd/yy)    
	tid conn-stream
	conn
	alldatas
	server-openings
	(server-instance (gensym))
	(user-id server-login-id)
	(server-protocol-version server-protocol-version)
	(*local-server-via-net* nil))
    
    (unwind-protect
	(trap-lossage
	  (error "Server Top Level")
	  (catch
	    'server-chaos-disappear
	    (setq conn (chaos:listen "FILE"))
	    (when *lmfs-server-dont-answer-logins*
	      (chaos:reject (prog1 conn
				   (setq conn nil))
			    *lmfs-server-dont-answer-logins*)
	      (ferror nil *lmfs-server-dont-answer-logins*))
	    (let* ((pkt (chaos:read-pkts conn))
		   (result (server-parse-rfc pkt)))
	      (cond ((integerp result)
		     (setq server-protocol-version result))
		    (t
		     (chaos:reject (prog1 conn
					  (setq conn nil))
				   result)
		     (ferror nil result))))
	    (chaos:accept conn)
	    (funcall tv:who-line-file-state-sheet
		     :add-server
		     conn
		     "FILE"
		     current-process
		     'lmfs-peek-server
		     (process-stack-group current-process))
	    (setq conn-stream (chaos:make-stream conn))
	    (if *server-shutdown-message* (send-single-shutdown-message conn))
	    (error-restart-loop
	      ((system:abort error) "Return to server command-reading loop.")
	      (let
		(pkt op)
		(setq
		  pkt
		  (trap-lossage
		    (error "Server Reading packets")
		    (condition-bind
		      ((system:host-stopped-responding
			 (function (lambda (&rest ignore)
				     (throw 'server-chaos-disappear nil))))
		       (system:connection-lost (function (lambda (&rest ignore)
							   (throw 'server-chaos-disappear
								  nil)))))
		      (chaos:get-next-pkt conn))
		    (ferror 'server-control-conn-network-lossage "Control connection lost")))
		(setq op (chaos:pkt-opcode pkt))
		(cond ((or (= op chaos:eof-op)
			   (= op chaos:cls-op))
		       (funcall conn-stream :force-output)
		       (chaos:return-pkt pkt)
		       (throw 'server-chaos-disappear nil))
		      ((not (= op chaos:dat-op))
		       (ferror nil "Unrecognized packet opcode: ~S" op)))
		(let*
		  ((string (chaos:pkt-string pkt))
		   (strings (get-strings-from-pktstring string)))
		  (if trace-server-enabled
		      (without-interrupts (push (string-append string) server-traces)))
		  (destructuring-bind
		    (tid fh cmd . rest)
		    (parse-cmd-string string)
		    (if *lmfs-server-dont-answer-logins*
			(format conn-stream
				"~A ~A ERROR HNA F Host not available - ~A "
				tid
				(or fh "")
				*lmfs-server-dont-answer-logins*)
			(case cmd
			  (:login  (setq user-id (file-server-login rest)))
			  (:open  (file-server-open fh rest (first (foreign-pname-string conn strings))))
			  (:open-for-lispm  (apply
					      'file-server-open-for-lispm
					      fh
					      (first strings)
					      (let
						((*print-base* 10)
						 (*read-base* 10)
						 (*package* si:pkg-user-package)
						 (*readtable* si:initial-readtable))
						(read-from-string
						  string
						  nil
						  nil
						  :start
						  (+ (position #\Newline
							       (the string (string string))
							       :test
							       (function char=))
						     (length (first strings))
						     2)))))
			  (:direct-output (apply 'file-server-direct-output   ;new commands to support OPEN :IO
						 fh                           ; 09-02-87 DAB IO Support
						 rest
						 ))	;direct write
			  (:read  (apply 'file-server-direct-read              ;new commands to support OPEN :IO
					 fh                                    ; 09-02-87 DAB IO Support
					 rest
					 ))	;direct read
			  (:extended-command  (apply
						'file-server-extended-command
						fh
						(first rest)
						(first strings)
						(let
						  ((*print-base* 10)
						   (*read-base* 10)
						   (*package* si:pkg-user-package)
						   (*readtable* si:initial-readtable))
						  (read-from-string
						    string
						    nil
						    nil
						    :start
						    (+ (position #\Newline
								 (the string (string string))
								 :test
								 (function char=))
						       (length (first strings))
						       2)))))
			  (:data-connection  (file-server-data-connection fh rest))
			  (:undata-connection  (file-server-undata-connection fh))	
			  (:close  (file-server-close-connection fh rest))
			  (:filepos  (file-server-filepos fh rest))
			  (:delete  (file-server-delete fh (foreign-pname-string conn strings)))
			  (:rename  (file-server-rename fh (list
							     (first (foreign-pname-string conn (list (first strings))))
							     (first (foreign-pname-string conn (rest strings)))
							     )))
			  (:expunge  (file-server-expunge fh (foreign-pname-string conn strings)))
			  (:complete  (file-server-complete fh rest (foreign-pname-string conn strings)))
			  (:continue  (file-server-continue fh))
			  (:directory  (file-server-directory fh rest (foreign-pname-string conn strings)))
			  (:change-properties  (file-server-change-props fh (foreign-pname-string conn strings)))
			  (:create-directory  (file-server-create-directory fh
									    (foreign-pname-string conn strings)))
			  (:create-link  (file-server-create-link fh (foreign-pname-string conn strings)))
			  (:delete-and-expunge  (file-server-delete-and-expunge fh
										(foreign-pname-string conn strings)))
			  (otherwise  (format conn-stream
					      "~A ~A ERROR UKC F Unknown command: ~A"
					      tid
					      (or fh "")
					      cmd)))))
		  (funcall conn-stream :force-output)
		  (chaos:return-pkt pkt))))))
      (cond (conn
	     (funcall tv:who-line-file-state-sheet :delete-server conn)
	     (trap-lossage (error "Server Top Level close")
			   (chaos::close-conn conn
					      (or *lmfs-server-dont-answer-logins*
						  "Server error")))
	     (chaos:remove-conn conn)))
      (if server-openings
	  (trap-lossage (error "Server finish closing remaining openings")
			(dolist (opening  server-openings)
			  (funcall opening :close :abort))))
      (trap-lossage
	(error "Closeout undata")
	(dolist
	  (data alldatas)
	  (rplaca
	    (server-dataproc-comm-cell (get (server-dataproc-comm-sibling data)
					    server-instance))
	    'undata)
	  (rplaca (server-dataproc-comm-cell data) 'undata)
	  (chaos:remove-conn (server-dataproc-comm-conn data)))))))  



(defun 4server-parse-rfc* (pkt &aux s fx version)
  (setq s (chaos:pkt-string pkt))
  (setq fx (search 3"FILE"* (the string (string s)) :test #'char-equal))
  (cond
    ((null fx) 3"Unparseable RFC"*)
    (t (setq fx (position #\Space (the string (string s)) :start (+ fx 4.) :test-not #'char=))
     (cond
       ((null fx) 0.)
       ((null (setq version (parse-number s fx))) 3"Unparseable version number in RFC"*)
       ((or (= version 0.) (= version 1.)) version)
       (t (format () 3"Unsupported FILE protocol version: ~D"* version))))))  



(defun 4parse-cmd-string* (string &aux answers)
  (let ((nlx (position #\Newline (the string (string string)) :test #'char=)))
    (do ((start 0.)
	 (lim (or nlx (length string))))
	((>= start lim)
	 (nreverse answers))
      (if (char= (aref string start) #\Space)
	(progn
	  (push () answers)
	  (incf start))
	(let ((endx
	       (or
		(position #\Space (the string (string string)) :start start :end lim :test
			  #'char=)
		lim)))
	  (push
	   (or
	    (and
	     (loop for x from start below endx finally (return t) unless
		(member (aref string x) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'eq)
		return ())1;parse number is a dead bear*
	     (parse-number string start endx))
	    (si::intern1 (subseq (string string) start endx)  3""*))
	   answers)
	  (setq start (1+ endx)))))))  



(defun 4get-strings-from-pktstring* (string &aux answer)
  (do ((start (position #\Newline (the string (string string)) :test #'char=)))
      ((null start)
       nil)
    (let ((ix (position #\Newline (the string (string string)) :start (1+ start) :test #'char=)))
      (if (null ix)
	(progn
	  (if (not (= (1+ start) (length string)))
	    (push (subseq (string string) (1+ start))  answer))
	  (return (nreverse answer))))
      (push (subseq (string string) (1+ start) ix)  answer)
      (setq start ix))))  



(defun 4file-server-login* (rest)
  (let ((uname (first rest)))
    (if (null uname)
      (format conn-stream 3"~A  ERROR LIP F Invalid Login syntax"* tid)
      (progn
	(format conn-stream 3"~A  ~A ~A ~%~A~%"* tid 3"LOGIN"* uname
		(send (sample-pathname si:local-host) :new-pathname :device :unspecific :directory
		   (string uname) :name () :type () :version ()))
	(string uname)))))  

1;;; Opening files. *



(defun 4file-server-open* (fh rest filename &aux answer binp (characters t) direction directionp if-exists
  if-does-not-exist (byte-size :default) estimated-length1;Common lisp open option *
  deleted preserve-dates inhibit-links)
  (let ((losep
	 (catch 'open-opt-lost
	   (progn
	     (loop for olist on rest do
		(let ((opt (first olist)))
		  (case opt
		    (:binary (setq characters ()))
		    (:character (setq characters t))
		    (:default (setq characters :default))
		    (:read (setq direction :input
				 directionp t))
		    (:write (setq direction :output
				  directionp t))
		    (:probe (setq direction ()
				  directionp t))
		    (:probe-directory (setq direction :probe-directory
					    directionp t))
		    (:inhibit-links (setq inhibit-links t))
		    ((:temporary :raw :super-image))
		    (:deleted (setq deleted t))
		    (:preserve-dates (setq preserve-dates t))
		    (:byte-size (setq byte-size (cadr olist)) (pop olist))
		    (:if-exists (setq if-exists (cadr olist)) (pop olist))
		    (:if-does-not-exist (setq if-does-not-exist (cadr olist)) (pop olist))
		    (:estimated-length (setq estimated-length (cadr olist)) (pop olist))
		    (t (open-err 3"UOO F Unknown option: "* opt)))))
	     (if (null fh)
	       (if (member direction '(:input :output) :test #'eq)
		 (open-err 3"ICO F Inconsistent open options for probe opening"*))
	       1;; FHN given. must be real read or write.*
	       (let* ((comdata (get fh server-instance))
		      (type
		       (case (server-dataproc-comm-iotype comdata)
			 (input :input)
			 (output :output))))
		 (if (null comdata)
		   (open-err 3"UFH F No open data channel for this file handle: "* fh))
		 (if directionp
		   (unless (eq direction type)
		     (open-err 3"ICO F File handle type inconsistent with open mode."*))
		   (setq direction type))))
	     (let ((pathname (lmfs-parse-for-server filename)))
	       (if (errorp pathname)
		 (open-err 3"IPS F Bad filename syntax: "* pathname))
	       (let ((opening
		      (open pathname :direction direction :characters characters
			    :if-does-not-exist
			    (or if-does-not-exist
			       (case direction
				 ((:input nil) :error)
				 (:output :create)))
			    :if-exists
			    (or if-exists
			       (if (member (pathname-version pathname) '(:unspecific :newest) :test
					#'eq)
				 :new-version
				 :supersede))
			    :error () :inhibit-links inhibit-links :deleted deleted
			    :preserve-dates preserve-dates :byte-size byte-size)))
		 (if (errorp opening)
		   (throw 'open-opt-lost
			  (lmfs-error-string
			   opening)))
		 (setq binp
		       (case characters
			 (:default (not (funcall opening :characters)))
			 (t (not characters))))
		 (setq answer
		       (case server-protocol-version
			 (0.
			  (format () 3"~D ~A ~D ~S~%~A~%"*
				  (funcall (funcall opening :truename) :version)
				  (cv-time (funcall opening :creation-date))
				  (funcall opening :length)
				  (funcall opening :send-if-handles :qfaslp)
				  (server-print-pathname (funcall opening :truename))))
			 (1.
			  (format () 3"~A ~D ~S ~S~%~A~%"*
				  (time:print-universal-time (funcall opening :creation-date) ())
				  (funcall opening :length) binp1;qfaslp, needed for compatibility*
				  (not binp) (server-print-pathname (funcall opening :truename))))))
		 (if (null direction)
		   (funcall opening :close)
		   (let ((servi (get fh server-instance)))
		     (push opening server-openings)
		     (setf (server-dataproc-comm-binp servi) binp)
		     (setf (server-dataproc-comm-tid servi) tid)
		     (setf (server-dataproc-comm-opening servi) opening)
		     (rplaca (server-dataproc-comm-cell servi)
			     (if (eq direction :input)
			       'read
			       'write))))
		 ()))))))
    (if (null losep)
      (format conn-stream 3"~A ~A OPEN ~A"* tid (or fh 3""*) answer)
      (format conn-stream 3"~A ~A ERROR ~A"* tid (or fh 3""*) losep))))  



(defun 4lmfs-error-string* (error)
  (let* ((pn (send error :send-if-handles :pathname))
	 (pnn (and pn (typep pn 'pathname) (send pn :string-for-printing)))
	 (en (send error :report-string)))
	 1;; Drop a period off the end of the error message.*
    (and (char= (aref en (1- (length en))) #\.) (setq en (subseq (string en) 0 (1- (length en))) ))
    1;; Drop " for lm: foo" off the end.*
    (and pnn (string-equal en pnn (- (length en) (length pnn)))
       (setq en (subseq (string en) 0 (- (length en) (length pnn) 5)) ))
    (string-append
     (dolist (cn (send error :condition-names))
       (when (get cn 'file-error)
	 (return (get cn 'file-error))))
     3" F "* en)))  



(defun 4open-err* (&rest args)
  (throw 'open-opt-lost
	 (apply
	  'string-append
	  args)))  



1;(DEFVAR LOCAL-HOST-PATHNAME () "A pathname whose host is SI:LOCAL-HOST.")  



;(DEFUN INIT-LOCAL-HOST-PATHNAME ()
;  (SETQ LOCAL-HOST-PATHNAME (DEFAULT-PATHNAME () SI:LOCAL-HOST () () T)))  



;(ADD-INITIALIZATION 'INIT-LOCAL-HOST-PATHNAME '(INIT-LOCAL-HOST-PATHNAME) '(:WARM))  *
	

(defun lmfs-parse-for-server (string)
  (let* ((sample-pathname  (sample-pathname "boot|lm"))   
	 (local-boot-host (send sample-pathname :host))
	 pathname)
    (condition-case (result)		   ;2.03.88
	(if (eq (send local-boot-host :pathname-flavor) 'fs:lispm-pathname)	   ;2.08.88
	    (merge-pathname-defaults string (sample-pathname "boot|lm") :unspecific :newest)
	    (MULTIPLE-VALUE-BIND (DEVICE DIRECTORY NAME TYPE VERSION ignore QUOTED-STRING)
		(send sample-pathname :PARSE-NAMESTRING NIL string)
	      (unless directory (setf directory (list :WILD)))	;3.08.88 MBC
	      (setf pathname		   ;2.03.88
		    (MAKE-PATHNAME-INTERNAL QUOTED-STRING local-boot-host DEVICE DIRECTORY NAME TYPE VERSION)))
	    (merge-pathname-defaults pathname sample-pathname :unspecific :newest))
      (pathname-error (send result :report-string)))))

(defun 4lmfs-parse-no-merge-for-server* (string)
  (condition-case (result)
      (parse-namestring string si:local-host)
    (pathname-error
     (send result :report-string))))

(defun 4server-print-pathname* (pathname)
  (if (eq (send pathname :host) si:local-host)
    (send pathname :string-for-host)
    (send pathname :string-for-printing)))  

1;;; Special open command that handles arbitrary open options.*



(defparameter 4unmentioned-standard-stream-ops*
   '(:advance-input-buffer :break :characters :clear-input :clear-output :close :describe
     :direction :discard-current-input-buffer :discard-current-output-buffer
     :discard-input-buffer :discard-output-buffer :eof :eval-inside-yourself :finish
     :force-output :fresh-line :funcall-inside-yourself :get-handler-for :get-input-buffer :init
     :last-char-output :line-in :line-out :listen :new-output-buffer :next-input-buffer
     :operation-handled-p :plist :print-self :qfaslp :read-input-buffer :read-pointer
     :read-until-eof :rewind :send-current-output-buffer :send-if-handles :send-output-buffer
     :set-buffer-pointer :set-pointer :setup-new-output-buffer :setup-next-input-buffer
     :stream-input-buffer :stream-input-index :stream-input-limit :stream-output-buffer
     :stream-output-index :stream-output-limit :string-in :string-out :truename :tyi
     :tyi-no-hang :tyipeek :tyo :untyi :which-operations :who-line-information))  

1;These stream operations of the local file stream,
;plus all standard stream operations, do not get mentioned
;in the WHICH-OPERATIONS list we send to the remote system
;because they are either implemented specially over there
;or are not supposed to be available there.*


(defparameter 4unmentioned-stream-ops*
   `(:init :get :getl :get-location :putprop :remprop :push-property :plist :property-list
     :set-property-list :setplist :info :pathname :generic-pathname :status :delete-and-expunge
     :delete :rename :undelete :expunge :open :change-properties :all-directories
     :directory-list :directory-list-stream :peek-file-system :set-byte-size :byte-size
     :creation-date :length :author :pdp10-format :must-explicitly-close :force-close :node
     :its-directory-stream ,@unmentioned-standard-stream-ops))  



(defun file-server-open-for-lispm (fh filename &rest modes &key &optional (direction :input) &allow-other-keys)
  (let ((losep
	  (catch 'open-opt-lost
	    (progn
	      (if (null fh)
		  (unless (member direction '(nil :IO) :test #'eq)	; 09-02-87 DAB IO Support
		    ;; :direction missing or not nil.
		    (open-err "ICO F Inconsistent open options for probe opening"))
		  ;; FHN given. must be real read or write.
		  (let* ((comdata (get fh server-instance))
			 (type (server-dataproc-comm-iotype comdata)))
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (unless (eq type (case direction
				       (:input 'input)
				       (:output 'output)))
		      (open-err "ICO F File handle type inconsistent with open mode"))))
	      
	      (let ((pathname (lmfs-parse-for-server filename)))
		(if (errorp pathname)
		    (open-err "IPS F Bad filename syntax: " pathname))
		(let ((opening (apply 'open pathname :error () modes)))
		  (if (errorp opening)
		      (throw 'open-opt-lost
			     (lmfs-error-string
			       opening)))
		  (format conn-stream "~A ~A OPEN " tid (or fh ""))
		  (format conn-stream "~A ~D ~S ~S ~S ~S~%~A~%"
			  (time:print-universal-time (funcall opening :creation-date) ())
			  (funcall opening :length) (funcall opening :send-if-handles :qfaslp)
			  (funcall opening :characters) (funcall opening :get :author)
			  (funcall opening :byte-size)
			  (server-print-pathname (funcall opening :truename)))
		  (let ((*print-base* 10.)
			(*read-base* 10.)
			(*readtable* si:initial-readtable)
			(*package* si:pkg-user-package))
		    (send conn-stream :tyo #\()
		    (dolist (op (funcall opening :which-operations))
		      (unless (member op unmentioned-stream-ops :test #'eq)
			(prin1 op conn-stream)
			(send conn-stream :tyo #\Space)))
		    (send conn-stream :tyo #\))
		    (print
		      (or (send opening :send-if-handles :file-contents-plist)
			  (send opening :send-if-handles :file-plist))
		      conn-stream))
		  (case direction
		    (null (funcall opening :close))
		    ((:output :input)
		     (let ((servi (get fh server-instance)))
		       (push opening server-openings)
		       (setf (server-dataproc-comm-binp servi) (not (funcall opening :characters)))
		       (setf (server-dataproc-comm-tid servi) tid)
		       (setf (server-dataproc-comm-opening servi) opening)
		       (rplaca (server-dataproc-comm-cell servi)
			       (case direction
				 (:input 'read)
				 (:output 'write)
				 (t (ferror "direction is not :input or :output"))))))
		    (:IO (file-server-direct-file-id opening (getf modes :direct-file-id))))	; 09-02-87 DAB IO Support
		  ()))))))
    (if losep
	(format conn-stream "~A ~A ERROR ~A" tid (or fh "") losep))))  

(defun file-server-direct-file-id (opening direct-file-id)     ; 09-02-87 DAB IO Support
  "Create a variable directory-file-id and assign it to opening."
  (when opening
      (let ((FIle-var (intern-local direct-file-id)))
	(setf (symbol-value file-var)  opening))))

1;;; Allow Lispm machine to do arbitrary stream and pathname operations
;;; with arbitrary Lisp data as arguments and as values.*


(defun 4file-server-extended-command* (fh command pathname &rest args &aux target)
 1;; Either FH or PATHNAME, but not both, should be non-nil.*
  (unless
   1;; This returns t if pathname or stream is not suitable*
   (cond
     ((null fh) (setq target (lmfs-parse-for-server pathname))
      (when (errorp pathname)
	(format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid)
	t))
     (t
      (let* ((data (get fh server-instance))
	     (opening (server-dataproc-comm-opening data)))
	(setq target opening)
	(when (or (null data) (null opening) (symbolp opening))1;yes, I know NIL is a symbol, thx*
	  (format conn-stream 3"~A ~A ERROR UFH F No opening for handle ~A"* tid fh fh)
	  t))))
   (condition-case (results) (multiple-value-list (apply target command args))
      (ferror
       (format conn-stream 3"~A ~A ERROR ~A"* tid (or fh 3""*) (lmfs-error-string (first results))))
      (:no-error (format conn-stream 3"~A ~A ~A~%"* tid (or fh 3""*) command)
       (let ((*print-base* 10.)
	     (*read-base* 10.)
	     (*readtable* si:initial-readtable)
	     (*package* si:pkg-user-package))
	 (prin1 results conn-stream))))))  

1;;; Data connection stuff.*



(defun 4null-car* (x)
  (null (first x)))  



(defun 4send-sync-mark* (dconn)
  (chaos:send-pkt dconn (chaos:get-pkt) %file-synchronous-mark-opcode))  



(defun 4file-server-data-connection* (fh rest)
  (let ((ifh (first rest))
	(ofh (cadr rest))
	(default-cons-area working-storage-area))
    (if (not (and ifh ofh (symbolp ifh) (symbolp ofh)))
      (format conn-stream 3"~A ~A ERROR IRF F Ill-formed data-connection request"* tid (or fh 3""*))
      (condition-case (dconn) (chaos:connect (chaos:foreign-address conn) (string ofh))
	 (ferror
	  (format conn-stream 3"~A ~A ERROR NWL F Reverse data connection (~A) lost:~%~A"* tid
		  (or fh 3""*) ofh dconn))
	 (:no-error
	  (setf (get ifh server-instance)
		(make-server-dataproc-comm iotype 'input conn dconn cconn conn))
	  (setf (get ofh server-instance)
		(make-server-dataproc-comm iotype 'output conn dconn cconn conn))
	  (let ((ocell (cons () ofh))
		(icell (cons () ifh))
		(idata (get ifh server-instance))
		(odata (get ofh server-instance)))
	    (setf (server-dataproc-comm-data-proc idata)
		  (process-run-function (string-append 3"File Server Data "* ifh)
					'file-server-data-top-level server-instance icell ifh))
	    (setf (server-dataproc-comm-data-proc odata)
		  (process-run-function (string-append 3"File Server Data "* ofh)
					'file-server-data-top-level server-instance ocell ofh))
	    (setf (server-dataproc-comm-sibling idata) ofh)
	    (setf (server-dataproc-comm-sibling odata) ifh)
	    (push odata alldatas)1;one side's good enough.*
	    (setf (server-dataproc-comm-cell idata) icell)
	    (setf (server-dataproc-comm-cell odata) ocell)
	    (format conn-stream 3"~A ~A DATA-CONNECTION"* tid (or fh 3""*))))))))  




(defun 4file-server-undata-connection* (fh)
  (let ((data (get fh server-instance)))
    (cond
      ((null data) (format conn-stream 3"~A ~A ERROR UFH F Unknown file handle: ~A"* tid fh fh))
      (t
       (dolist (fh (list fh (server-dataproc-comm-sibling data)))
	 (let* ((data (get fh server-instance))
		(cell (server-dataproc-comm-cell data)))
	   (await-data-process cell 'undata)
	   1;; We can't predict which side we'll be told to undata, so make sure*
	   1;; that all sides get removed from alldatas.*


	   
	   (setq alldatas (delete data (the list alldatas) :test #'eq :count 1.))))
       (chaos:remove-conn (server-dataproc-comm-conn data))
       (format conn-stream 3"~A ~A UNDATA-CONNECTION"* tid fh)))))  



(defun file-server-data-top-level (server-instance cell handle &aux &special (*local-server-via-net* nil))
  (let ((*print-case* :upcase)        ;01-22-88 DAB 
	(*package* *system-package*))
    (unwind-protect 
	(trap-lossage
	  (error "File Server Data Connection")
	  (loop
	    (process-wait "Data Conn Cmd" #'car cell)
	    (let* ((data (get handle server-instance))
		   (celloc (locf (first cell)))
		   (opening (server-dataproc-comm-opening data))
		   (dconn (server-dataproc-comm-conn data))
		   (binp (server-dataproc-comm-binp data)))
	      (case (first cell)
		(undata			   ;Gute Nacht, O Wesen.
		 (rplaca cell ()) (return ()))
		((fpsync wsync) (send-sync-mark dconn) (rplaca cell ()))
		(directory (server-dataproc-hack-directory data handle)
			   (%store-conditional celloc 'directory ()))
		(write (if (null opening)
			   (ferror () "file-server-data-top-level - no opening"))
		       (condition-bind
			 ((no-more-room
			    (let-closed
			      ((server-instance server-instance) (cell1 cell) (handle1 handle))
			      'server-disk-full-handler)))
			 (catch 'async-abort
			   (do ()
			       (nil)
			     (if (not (eq (first cell) 'write))
				 (return ()))
			     (let* ((pkt
				      (if (server-window-write-check cell dconn 'write)
					  (chaos:get-next-pkt dconn)
					  (return ()))))
			       (select (chaos:pkt-opcode pkt)
				 (chaos:eof-op (chaos:return-pkt pkt)
					       (setq pkt
						     (if (server-window-write-check cell dconn 'write)
							 (chaos:get-next-pkt dconn)
							 (return ())))
					       (or (= (chaos:pkt-opcode pkt) %file-synchronous-mark-opcode)
						   (ferror "Unrecognized Opcode in data server: ~O"
							   (chaos:pkt-opcode pkt)))
					       (chaos:return-pkt pkt) (%store-conditional celloc 'write ())
					       (when (eq (funcall opening :direction) :io)
						 (send-sync-mark dconn))
					       
					       (return ()))
				 (%file-synchronous-mark-opcode (chaos:return-pkt pkt)
								(%store-conditional celloc 'write ())
								(return ()))
				 (%file-binary-opcode
				  (unwind-protect (funcall
						    opening
						    :string-out
						    pkt
						    chaos:first-data-word-in-pkt
						    (+ (truncate (chaos:pkt-nbytes pkt) 2.)
						       chaos:first-data-word-in-pkt))
				    (chaos:return-pkt pkt)))
				 (%file-character-opcode
				  (unwind-protect (funcall
						    opening
						    :string-out
						    (chaos:pkt-string pkt)
						    0.
						    (chaos:pkt-nbytes pkt))
				    (chaos:return-pkt pkt)))
				 (otherwise
				  (ferror () "Unknown pkt opcode: ~O" (chaos:pkt-opcode pkt))))
			       (when (eq (funcall opening :direction) :io) ; 09-02-87 DAB IO Support
				 (send-sync-mark dconn)
				 (%store-conditional celloc 'write ())
				 (return ())
				 )
			       )))))
		(read (if (null opening)
			  (ferror () "file-server-data-top-level - no opening"))
		      (do (last
			   eofp)
			  (nil)
			(if (server-window-read-check cell dconn)
			    (return ()))
			(let ((pkt (chaos:get-pkt)))
			  (cond
			    (binp
			     (multiple-value-setq (last eofp)
			       (funcall opening :string-in () pkt chaos:first-data-word-in-pkt
					chaos:max-data-words-per-pkt))
			     (setf (chaos:pkt-opcode pkt) %file-binary-opcode)
			     (setf (chaos:pkt-nbytes pkt)
				   (* 2. (- last chaos:first-data-word-in-pkt))))
			    (t
			     (multiple-value-setq (last eofp)
			       (funcall opening :string-in () (chaos:pkt-string pkt) 0.
					chaos:max-data-bytes-per-pkt))
			     (setf (chaos:pkt-opcode pkt) %file-character-opcode)
			     (setf (chaos:pkt-nbytes pkt) last)))
			  (if (plusp (chaos:pkt-nbytes pkt))
			      (chaos:send-pkt dconn pkt (chaos:pkt-opcode pkt))	   ;don't let SEND dft it
			      (chaos:return-pkt pkt)
			      )
			  (when eofp (if (server-window-read-check cell dconn)
					 (return ()))
				(chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
				(%store-conditional celloc 'read ())
				(when (eq (funcall opening :direction) :io)	   ; 09-02-87 DAB IO Support
				  (send-sync-mark dconn))
				(return ()))
			  (when (eq (funcall opening :direction) :io)	   ; 09-02-87 DAB IO Support
			    (send-sync-mark dconn)
			    (%store-conditional celloc 'read ())
			    (return ())
			    )
			  )))
		(t (ferror () "Bogus com-cell value: ~S" (first cell)))))))
      (remprop handle server-instance))))




(defun 4server-disk-full-handler* (condition)
  (declare (special server-instance cell1 handle1)
	   1;;; (UNSPECIAL TID) ;;; RLA - I believe this is  a NOP*
	   )
  (let* ((data (get handle1 server-instance))
	 (celloc (locf (first cell1)))
	 (tid (server-dataproc-comm-tid data))
	 (dconn (server-dataproc-comm-conn data))
	 (cconn (server-dataproc-comm-cconn data)))
    (%store-conditional celloc 'write 'async-mark)
    1;; Send an async pkt on the control connection to advertise our woes.*
    (let ((pkt (chaos:get-pkt)))
      (chaos:set-pkt-string pkt tid 3" "* handle1 3" ERROR NMR R "* (send condition :report-string))
      (chaos:send-pkt cconn pkt %file-asynchronous-mark-opcode))
    1;; Now wait for the control connection to fix us.*
    (process-wait 3"Disk Full"* #'(lambda (x)
				  (neq (first x) 'async-mark))
		  cell1)
    (case (first cell1)
      (continue (rplaca cell1 'write) (values :retry-file-operation ()))
      (async-abort
       (loop for pkt = (chaos:get-next-pkt dconn) as op = (chaos:pkt-opcode pkt) do
	  (chaos:return-pkt pkt) when (= op %file-synchronous-mark-opcode) return ())
       (rplaca cell1 (quote nil)) (throw 'async-abort
					 ()))
      (otherwise (ferror () 3"Cell in odd state in async recover - ~S"* (first cell1))))))  



(defun 4server-window-write-check* (cell dconn val)
  (do ()
      (nil)
    (if (neq (first cell) val)
      (return ()))
    (if (chaos:read-pkts dconn)
      (return t))
    (process-wait 3"Net In or Cmd"*
		  #'(lambda (cell dconn val)
		      (or (neq (first cell) val) (chaos:read-pkts dconn)))
		  cell dconn val)))  




(defun 4server-window-read-check* (cell dconn &optional (cstate 'read))
  (do ()
      (nil)
    (if (neq (first cell) cstate)
      (return t))
    (if (chaos:may-transmit dconn)
      (return ()))
    (process-wait 3"Net Out or Cmd"*
		  #'(lambda (cell dconn cstate)
		      (or (neq (first cell) cstate) (chaos:may-transmit dconn)))
		  cell dconn cstate)))  



(defun 4await-data-process* (cell flag)
  (rplaca cell flag)
  (process-wait 3"Await Data Conn"* #'null-car cell))  



(defun 4file-server-continue* (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
      (format conn-stream 3"~A ~A ERROR UFH F Unknown file handle: ~A"* tid fh fh)
      (let ((opening (server-dataproc-comm-opening data))
	    (cell (server-dataproc-comm-cell data)))
	(cond
	  ((null opening)
	   (format conn-stream 3"~A ~A ERROR UFH F No opening on handle ~A"* tid fh fh))
	  ((neq (first cell) 'async-mark)
	   (format conn-stream 3"~A ~A ERROR LOS F Channel not in async marked state"* tid fh))
	  (t (%store-conditional (locf (first cell)) 'async-mark 'continue)
	   (format conn-stream 3"~A ~A CONTINUE"* tid fh)))))))  


(defun file-server-close-connection (fh rest)
  (let* ((direct-file-id (find-symbol fh)) ; 09-02-87 DAB IO Support
	 (data (unless direct-file-id (get fh server-instance)))
	 direction opening cell)
    (if direct-file-id			   ; 09-02-87 DAB IO Support
	(setq opening direct-file-id
	      direction :io)
	(if (null data)
	    (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	    (setf direction (server-dataproc-comm-iotype data))
	    (setf opening (server-dataproc-comm-opening data))
	    (setf cell (server-dataproc-comm-cell data))
	    ))
    (when (or direct-file-id data)
      (cond
	((null opening)
	 (format conn-stream "~A ~A ERROR UFH F No opening on handle ~A" tid fh (or direct-file-id fh)))
	(direct-file-id			   ; 09-02-87 DAB IO Support
	 (case server-protocol-version
	      (0.
	       (format conn-stream "~A ~A CLOSE ~D ~A ~D~%~A~%" tid fh
		       (funcall (funcall (symbol-value opening) :truename) :version)
		       (time:print-universal-time (funcall (symbol-value opening) :creation-date) ())
		       (funcall (symbol-value opening) :length) (funcall (symbol-value opening) :truename)))
	      (1.
	       (format conn-stream "~A ~A CLOSE ~A ~D~%~A~%" tid fh
		       (cv-time (funcall (symbol-value opening) :creation-date))
		       (funcall (symbol-value opening) :length)
		       (funcall (symbol-value opening) :truename))))
	 (funcall conn-stream :force-output)
	 (condition-case (results)	   ;catch close error,like check Deleting hogs with delete bit set.
	     (funcall (symbol-value opening) :close (if (first rest)  :abort))
	   (:no-error ())
	   (error
	    ;(format conn-stream "~A ~A ERROR ~A" tid (or fh "") (lmfs-error-string (first results)))
	    ))
	 (setq server-openings
	       (delete (symbol-value opening) (the list server-openings) :test #'eq :count 1.))
	 (unintern direct-file-id))
	(t (if (eq direction 'input)
	       (rplaca cell 'wsync))
	   (%store-conditional (locf (first cell)) 'async-mark 'async-abort)
	   (cond
	     ((eq opening 'directory) (format conn-stream "~A ~A CLOSE" tid fh))
	     (t
	      (case server-protocol-version
		   (0.
		    (format conn-stream "~A ~A CLOSE ~D ~A ~D~%~A~%" tid fh
			    (funcall (funcall opening :truename) :version)
			    (time:print-universal-time (funcall opening :creation-date) ())
			    (funcall opening :length) (funcall opening :truename)))
		   (1.
		    (format conn-stream "~A ~A CLOSE ~A ~D~%~A~%" tid fh
			    (cv-time (funcall opening :creation-date)) (funcall opening :length)
			    (funcall opening :truename))))
	      ;;03-31-88 DAB
	      ))
	   (funcall conn-stream :force-output)
	   (if (eq direction 'input)
	       (process-wait "Read Finish" #'null-car cell)
	       (process-wait "Write Finish" #'null-car cell))
	   (setf (server-dataproc-comm-opening data) ())
	   (cond
	     ((not (eq opening 'directory))
	      (condition-case (results)	   ;catch close error,like check Deleting hogs with delete bit set.
		  (when (not (eq opening 'directory)) (funcall opening :close (if (first rest) :abort)))
		(:no-error ())
		(error
	           ;(format conn-stream "~A ~A ERROR ~A" tid (or fh "") (lmfs-error-string  results))
		 ))
	      (setq server-openings
		    (delete opening (the list server-openings) :test #'eq :count 1.))))
	   )))))


(defun file-server-direct-read (fh direct-file-id &optional BIND? filepos &rest ignore)	; 09-02-87 DAB IO Support
  (let ((losep
	  (catch 'open-opt-lost
	    (progn
	      (if (null fh)
		  (open-err "ICO F Inconsistent file direction for File IO direct read")
		  ;; FHN given. must be real read or write.
		  (let* ((comdata (get fh server-instance))
			 (type (server-dataproc-comm-iotype comdata))
			 )
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (unless (eq type 'input)
		      (open-err "ICO F File handle type inconsistent with open mode"))
		    ))
	      
	      (let ((opening (find-symbol direct-file-id)))
		(unless opening
		  (open-err "IPS F Bad filename syntax: " opening))
		(when filepos
		  (funcall (symbol-value opening) :set-pointer filepos))
		(format conn-stream "~A ~A READ" tid (or fh ""))
		(let ((servi (get fh server-instance)))
		  (if BIND?
		      (progn
			(push opening server-openings)
			(setf (server-dataproc-comm-binp servi) (not (funcall (symbol-value opening) :characters)))
			(setf (server-dataproc-comm-tid servi) tid)
			(setf (server-dataproc-comm-opening servi) (symbol-value opening))
			(rplaca (server-dataproc-comm-cell servi) 'read))
		      (rplaca (server-dataproc-comm-cell servi) 'wsync)
		      )
		  )))
	    ())))
    (if losep (format conn-stream "~A ~A ERROR ~A" tid (or fh "") losep)))    
  )



1;;; Random commands.*



(defun file-server-filepos (fh rest)		; 09-02-87 DAB IO Support
  (let* ((position (first rest))
	 (direct-file-id (find-symbol (second rest)))
	 (data (get fh server-instance))
	 )
    (cond (direct-file-id
	   (format conn-stream "~A ~A FILEPOS" tid (or fh ""))
	   (send (symbol-value direct-file-id) :set-pointer position))
	  (data
	   (let ((direction (server-dataproc-comm-iotype data))
		 (opening (server-dataproc-comm-opening data))
		 (cell (server-dataproc-comm-cell data))
		 )
	     (format conn-stream "~A ~A FILEPOS" tid fh)
	     (funcall conn-stream :force-output)
	     (await-data-process cell 'fpsync)
	     (funcall opening :set-pointer (first rest))
	     (rplaca cell (if (eq direction 'input)
			      'read
			      'write))))
	  (t 	
	   (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh))
	  )))  



(defun 4file-server-delete-multiple-files* (fh strings)
  (if fh
    (format conn-stream 3"~A  ERROR IRF F Inconsistent command options"* tid)
    (let (results)
      (dolist (s strings)
	(let ((path (lmfs-parse-for-server s)))
	  (if (errorp path)
	    (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid)
	    (push (funcall path :delete ()) results))))
      (if (some 'identity results)
	(progn
	  (format conn-stream 3"~A  DELETE-MULTIPLE-FILES~%"* tid)
	  (dolist (r (reverse results))
	    (if (errorp r)
	      (format conn-stream 3"~A  ERROR ~A~%"* tid (lmfs-error-string r))
	      (format conn-stream 3"~%"*))))
	(format conn-stream 3"~A  DELETE-MULTIPLE-FILES"* tid)))))  



(defun 4file-server-undelete-multiple-files* (fh strings)
  (if fh
    (format conn-stream 3"~A  ERROR IRF F Inconsistent command options"* tid)
    (let (results)
      (dolist (s strings)
	(let ((path (lmfs-parse-for-server s)))
	  (if (errorp path)
	    (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid)
	    (push (funcall path :undelete ()) results))))
      (if (some 'identity results)
	(progn
	  (format conn-stream 3"~A  UNDELETE-MULTIPLE-FILES~%"* tid)
	  (dolist (r (reverse results))
	    (if (errorp r)
	      (format conn-stream 3"~A  ERROR ~A~%"* tid (lmfs-error-string r))
	      (format conn-stream 3"~%"*))))
	(format conn-stream 3"~A  UNDELETE-MULTIPLE-FILES"* tid)))))  



(defun 4file-server-delete* (fh strings)
  (cond
    ((null fh)1;must be string, delete random file*
     (if (not (= (length strings) 1.))
       (format conn-stream 3"~A  ERROR IRF F Inconsistent command options"* tid)
       (let ((path (lmfs-parse-for-server (first strings))))
	 (if (errorp path)
	   (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid)
	   (let ((result (funcall path :delete ())))
	     (if (errorp result)
	       (format conn-stream 3"~A  ERROR ~A"* tid (lmfs-error-string result))
	       (format conn-stream 3"~A  DELETE"* tid)))))))
    (t1;delete while open*
     (if strings
       (format conn-stream 3"~A ~A ERROR IRF F Inconsistent delete command options"* tid fh)
       (let ((data (get fh server-instance)))
	 (if (or (null data) (null (server-dataproc-comm-opening data)))
	   (format conn-stream 3"~A ~A ERROR UFH F No opening for handle ~A"* tid fh fh)
	   (progn
	     (funcall (server-dataproc-comm-opening data) :delete)
	     (format conn-stream 3"~A ~A DELETE"* tid fh))))))))  



(defun file-server-delete-and-expunge (fh strings &aux result)
  (cond
    ((null fh);must be string, delete-and-expunge random file
     (if (not (= (length strings) 1.))
       (format conn-stream "~A  ERROR IRF F Inconsistent command options" tid)
       (let ((path (lmfs-parse-for-server (first strings))))
	 (if (errorp path)
	   (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)

	;;; *** Add path arg here ***
	   (let ((result (funcall path :delete-and-expunge)))	   ;4.14.87
	     (if (errorp result)
	       (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result))
	       (setq result (format conn-stream "~A  :DELETE-AND-EXPUNGE ~A" tid result))))))))
    (t
     (if strings
       (format conn-stream "~A ~A ERROR IRF F Inconsistent delete and expunge command options"
	       tid fh)
       (let ((data (get fh server-instance)))
	 (if (or (null data) (null (server-dataproc-comm-opening data)))
	   (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A" tid fh fh)
	   (progn
	     (funcall (server-dataproc-comm-opening data) :delete-and-expunge)
	     (format conn-stream "~A  :DELETE-AND-EXPUNGE ~A" tid result))))))))

(defun 4file-server-directory* (fh rest strings &aux data parsed)
  (cond
    ((or (null fh) (not (= (length strings) 1.)))
     (format conn-stream 3"~A ~A ERROR IRF F Inconsistent DIRECTORY command"* tid (or fh 3""*)))
    ((or (null (setq data (get fh server-instance)))
	(not (eq (server-dataproc-comm-iotype data) 'input)))
     (format conn-stream 3"~A ~A ERROR UFH F Bad file handle for DIRECTORY command"* tid fh fh))
    ((errorp (setq parsed (lmfs-parse-for-server (first strings))))
     (format conn-stream 3"~A ~A ERROR IPS F Bad pathname syntax - ~A"* tid fh (first strings)))
    (t (setf (server-dataproc-comm-arg data) (cons parsed rest))
     (setf (server-dataproc-comm-tid data) tid)
     (setf (server-dataproc-comm-opening data) 'directory)1;make close work*
     (setf (server-dataproc-comm-dinfo data) (cons (format () 3"~A ~A "* tid fh) conn))
     1;;let dataproc do the answerage*
     (setf (first (server-dataproc-comm-cell data)) 'directory))))  



(defun 4server-dataproc-hack-directory* (data handle &aux ok (conn (server-dataproc-comm-conn data))
  (cell (server-dataproc-comm-cell data)))
  (trap-lossage (error 3"Directory lister toplevel"*)
     (let* ((conn-stream (chaos:make-stream conn))
	    (arg (server-dataproc-comm-arg data))
	    (path (first arg))
	    (opts (rest arg)))
       (let ((dirlist (funcall path :directory-list (cons :noerror opts)))
	     (gopkt (chaos:get-pkt))
	     (dinfo (server-dataproc-comm-dinfo data)))
	 (cond
	   ((errorp dirlist)
	    (chaos:set-pkt-string gopkt (first dinfo) 3"ERROR "* (lmfs-error-string dirlist)))
	   (t (chaos:set-pkt-string gopkt (first dinfo) 3"DIRECTORY"*)))
	 (chaos:send-pkt (rest dinfo) gopkt)
	 (cond
	   ((not (errorp dirlist)) (server-dirlist-single (cdar dirlist) () conn-stream)
	    (dolist (file (rest dirlist))
	      (if (server-window-read-check cell conn 'directory)
		(return ()))
	      (server-dirlist-single (rest file) (first file) conn-stream))
	    (funcall conn-stream :tyo #\Newline) (setq ok t))))
       (funcall conn-stream :force-output)
       (if ok
	 (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op)))
     (send-data-async-lossage conn 3"System error during dir list processing"* handle)))  



(defun 4send-data-async-lossage* (conn msg handle)
  (let ((pkt (chaos:get-pkt)))
    (chaos:set-pkt-string pkt 3" "* handle 3" ERROR LOS F "* msg)
    (chaos:send-pkt conn pkt %file-asynchronous-mark-opcode)))  



(defun 4server-dirlist-single* (props pn conn-stream)
  (let ((*print-base* 8.) (*read-base* 8.) (*nopoint t))
    (format conn-stream 3"~%"*)
    (if pn
	(format conn-stream "~A~%"  
		;;Removed this hack on 11-30-87 DAB.
					   ;(let ((host (si:get-host-from-address (chaos:foreign-address conn) ':chaos)))
		;; This is a hack to allow symbolics pathname syntax from 3600's
					   ;  (if (and host (eq (send host :system-type) :lmfs))
		
		;; Make a 3600 pathname to send back.
					   ;      (send (make-pathname
					   ;	       :host host  
					   ;	       :directory (pathname-directory pn)
					   ;	       :name (pathname-name pn)
					   ;	       :type (pathname-type pn)
					   ;	       :version (pathname-version pn)) :string-for-host)
		
		(send pn ':string-for-host)
					   ;      ))
		))
    
    (loop for (ind prop) on props by #'cddr
	  do (if (eq ind :settable-properties)
		 (progn
		   (format conn-stream 3"~a "* ind)
		   (loop for x on prop do (princ (first x) conn-stream)
			 (if (rest x)
			     (write-char #\Space conn-stream))))
		 (or
		   (dolist (spec *known-directory-properties*)
		     (if (member ind (rest spec) :test #'eq)
			 (progn
			   (format conn-stream 3"~a "* ind)
			   (if (cadar spec)
			       (funcall (cadar spec) prop conn-stream)
			       (princ (format () 3"~s"* prop) conn-stream))
			   (return t))))
		   (progn
		     (format conn-stream 3"~s "* ind)
		     (princ (format () 3"~s"* prop) conn-stream))))
	  (format conn-stream 3"~%"*))))

(defun 4file-server-change-props* (fh strings)
  (trap-lossage (error 3"Change properties toplevel"*)
     (cond
       ((null fh)
	(if (not (> (length strings) 1.))
	  (format conn-stream 3"~A  ERROR IRF F No pathname given."* tid)
	  (let ((path (lmfs-parse-for-server (first strings))))
	    (if (errorp path)
	      (format conn-stream 3"~A  ERROR IPS F Syntax error in supplied path: ~A"* tid)
	      (change-props-1 path 3""* (rest strings))))))
       (t
	(let ((data (get fh server-instance)))
	  (if (or (null data) (null (server-dataproc-comm-opening data)))
	    (format conn-stream 3"~A ~A ERROR UFH F No opening for handle ~A"* tid fh fh)
	    (change-props-1 (server-dataproc-comm-opening data) fh strings)))))
     (format conn-stream 3"~A ~A ERROR SYS F Internal error:~% ~A"* tid (or fh 3""*) trap-error)))  



(defun 4change-props-1* (actor fh strings)
  (loop with sym for string in strings as spacex =
     (position #\Space (the string (string string)) :test #'char=) unless spacex do
     (format conn-stream 3"~A ~A ERROR STX F Ill formated property spec: ~A"* tid fh string) nconc
     (list* (setq sym (si::intern1 (subseq (string string) 0 spacex)  pkg-keyword-package))
	    (server-convert-known-file-property string (1+ spacex) sym) ())
     into plist finally
     (trap-lossage (error 3"Change properties"*)
	(let ((m (apply actor :change-properties () plist)))
	  (if (errorp m)
	    (format conn-stream 3"~A ~A ERROR LOS F ~A"* tid fh m)
	    (format conn-stream 3"~A ~A CHANGE-PROPERTIES"* tid fh)))
	(format conn-stream 3"~A ~A ERROR SYS F Internal error:~% ~A"* tid fh trap-error))))  



(defun 4server-convert-known-file-property* (string index ind)
  (let ((*read-base* 8.)
	(*merge-timezone* time:*timezone*))  ;01-20-88 DAB Used by parse-directory-date-property to control
                                             ;baising the time by daylight savings.
     (declare (special *merge-timezone* ))   ;01-20-88 DAB 
     (loop for ((fcn) . propnames) in *known-directory-properties* if
       (member ind propnames :test #'eq) return (funcall fcn string index) finally
       (return (read-from-substring string index ())))))  



(defun 4file-server-rename* (fh strings)
  (cond
    ((null fh)1;must be string, delete random file*
     (if (not (= (length strings) 2.))
       (format conn-stream 3"~A  ERROR IRF F Inconsistent RENAME command options"* tid)
       (let ((path1 (lmfs-parse-for-server (first strings)))
	     (path2 (lmfs-parse-for-server (second strings))))
	 (if (errorp path1)
	   (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid)
	   (if (errorp path2)
	     (format conn-stream 3"~A  ERROR IPS F Syntax error in rename pathname"* tid)
	     (trap-lossage (error 3"Rename 2 args"*)
		(progn
		  (if (null (funcall path1 :version))
		    (setq path1 (funcall path1 :new-version :newest)))
		  (if (null (funcall path2 :version))
		    (setq path2 (funcall path2 :new-version :newest)))
		  (let ((result (funcall path1 :rename path2 ())))
		    (if (errorp result)
		      (format conn-stream 3"~A  ERROR ~A"* tid (lmfs-error-string result))
		      (format conn-stream 3"~A  RENAME~%~A"* tid
			      (server-print-pathname (send path2 :truename))))))
		(format conn-stream 3"~A  ERROR SYS F System error renaming"* tid)))))))
    (t1;rename while open*
     (if (not (= (length strings) 1.))
       (format conn-stream 3"~A ~A ERROR IRF F Inconsistent rename command options"* tid fh)
       (let ((path (lmfs-parse-for-server (first strings))))
	 (if (errorp path)
	   (format conn-stream 3"~A ~A ERROR IPS F Syntax error in pathname"* tid fh)
	   (let* ((data (get fh server-instance))
		  (opening (server-dataproc-comm-opening data)))
	     (if (or (null data) (null opening) (symbolp opening))1;yes, I know NIL is a symbol, thx*
	       (format conn-stream 3"~A ~A ERROR UFH F No opening for handle ~A"* tid fh fh)
	       (trap-lossage (error 3"Rename while open"*)
		  (progn
		    (if (null (funcall path :version))
		      (setq path (funcall path :new-version :newest)))
		    (let ((result (funcall opening :rename path ())))
		      (if (errorp result)
			(format conn-stream 3"~A ~A ERROR ~A"* tid fh (lmfs-error-string result))
			(format conn-stream 3"~A ~A RENAME~%~A"* tid fh
				(server-print-pathname (send opening :truename))))))
		  (format conn-stream 3"~A ~A ERROR SYS F System error while renaming"* tid fh))))))))))  




(defun 4file-server-expunge* (fh strings &aux path result)
  (cond
    (fh (format conn-stream 3"~A ~A ERROR IRF File handle given in EXPUNGE command."* tid fh))
    ((null strings)
     (format conn-stream 3"~A  ERROR IRF F No pathname given to EXPUNGE command."* tid))
    ((rest strings)
     (format conn-stream 3"~A  ERROR IRF F Extra junk given to EXPUNGE command."* tid))
    ((errorp (setq path (lmfs-parse-for-server (first strings))))
     (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid))
    ((errorp (setq result (funcall path :expunge :error ())))
     (format conn-stream 3"~A  ERROR ~A"* tid (lmfs-error-string result)))
    (t (format conn-stream 3"~A  EXPUNGE ~D"* tid result))))  



(defun 4file-server-create-directory* (fh strings &aux path result)
  (cond
    (fh
     (format conn-stream 3"~A ~A ERROR IRF File handle given in CREATE-DIRECTORY command."* tid fh))
    ((null strings)
     (format conn-stream 3"~A  ERROR IRF F No pathname given to CREATE-DIRECTORY command."* tid))
    ((rest strings)
     (format conn-stream 3"~A  ERROR IRF F Extra junk given to CREATE-DIRECTORY command."* tid))
    ((errorp (setq path (lmfs-parse-for-server (first strings))))
     (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid))
    ((errorp (setq result (funcall path :create-directory :error ())))
     (format conn-stream 3"~A  ERROR ~A"* tid (lmfs-error-string result)))
    (t (format conn-stream 3"~A  CREATE-DIRECTORY ~D"* tid result))))  



(defun 4file-server-create-link* (fh strings &aux path path2)
  (cond
    (fh (format conn-stream 3"~A ~A ERROR IRF File handle given in CREATE-LINK command."* tid fh))
    ((null (second strings))
     (format conn-stream 3"~A  ERROR IRF F Insufficient arguments given to CREATE-LINK command."*
	     tid))
    ((cddr strings)
     (format conn-stream 3"~A  ERROR IRF F Extra junk given to CREATE-LINK command."* tid))
    ((errorp (setq path (lmfs-parse-for-server (first strings))))
     (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid))
    ((errorp (setq path2 (lmfs-parse-for-server (second strings))))
     (format conn-stream 3"~A  ERROR IPS F Syntax error in pathname"* tid))
    (t
     (condition-case (result) (funcall path :create-link path2)
	(ferror (format conn-stream 3"~A  ERROR ~A"* tid (lmfs-error-string result)))
	(:no-error (format conn-stream 3"~A  CREATE-LINK ~D"* tid result))))))  



(defun 4file-server-complete* (fh args strings &aux path result success)
  (cond
    (fh (format conn-stream 3"~A ~A ERROR IRF File handle given to COMPLETE command."* tid fh))
    ((not (= (length strings) 2))
     (format conn-stream 3"~A  ERROR IRF Wrong number of strings given in COMPLETE command."* tid))
    (t (setq path (lmfs-parse-no-merge-for-server (first strings)))
       1;;string result means an error*
       (if (errorp path)			1;ZMACS will supply semibogus paths....!!*
	   (setq path (user-homedir si:local-host)))
       (multiple-value-setq (result success)
			    (funcall path :complete-string (second strings)
				     (list* (if (member :write args :test #'eq)
						:write
						:read)
					    (if (member :new-ok args :test #'eq)
						:new-ok
						:old)
					    (if (member :deleted args :test #'eq)
						'(:deleted)))))
       (let ((x (and result (position #\: (the string (string result)) :test #'char-equal))))	1;strip out host*
	 (if x
	     (setq result (subseq (string result) (1+ x)) )))
       (format conn-stream 3"~A  COMPLETE ~A~%~A~%"* tid success result))))   



(defun 4print-server-login-exegesis* ()
  (notify-nicely () 3"This machine has been invoked as a remote file server, but is otherwise free."*)) ;07-13-88



(defun 4print-server-lossages* ()
  (if (null lmfs-server-lossages)
    (format t 3"~&No lossages."*)
    (dolist (l lmfs-server-lossages)
      (destructuring-bind (time key (err . args)) l
	 (format t 3"~&~A  ~A~45T ~S~%~10T~A"* time key err
		 (if (errorp (first args))
		   (apply #'format () args)
		   (format () 3"~S"* args))))
      (cond
	((funcall *standard-input* :listen)
	 (let ((c (funcall *standard-input* :tyi)))
	   (or (= c #\Space) (funcall *standard-input* :untyi c)))
	 (return ()))))))  



(defun 4file-server-shutdown* (message &optional (in-minutes 5.))
  (setq *server-shutdown-message* message
	*server-shutdown-time* (+ (get-universal-time) (* in-minutes 60.)))
  (process-run-function 3"Server shutdown"* 'file-server-shutdown-1 message
			(time-increment (time) (* in-minutes 3600.))))  



(defun 4file-server-cancel-shutdown* (&optional (key :cancel))
  (let ((sdval *server-shutdown-message*))
    (or (null sdval) (%store-conditional (locf *server-shutdown-message*) sdval key))))  
  


(defun 4file-server-reschedule-shutdown* (message &optional (in-minutes 5.))
  (file-server-cancel-shutdown :reschedule)
  (process-wait 3"Unshut"* #'null-car (locf *server-shutdown-message*))
  (file-server-shutdown message in-minutes))  



(defun 4file-server-shutdown-state* ()
  (values *server-shutdown-message* *server-shutdown-time*))  



(defvar 4*server-notify-times** '(60. 30. 15. 5. 2. 1. 0.))  



(defun 4file-server-shutdown-1* (message at-time)
  (loop with time-to-go and notify-interval do
     (if (time-lessp at-time (time))1;Already passed
;Already passed*
       (setq time-to-go ()
	     notify-interval ())
       (setq time-to-go (time-difference at-time (time))
	     notify-interval
	     (loop for times on *server-notify-times* when (<= (* (first times) 3600.) time-to-go)
		return times)))
     unless (eq message *server-shutdown-message*) do
     (cond
       ((eq *server-shutdown-message* :reschedule)
	(blast-message-to-file-servers 3"File Server shutdown rescheduled"*))
       (t (notify-nicely () 3"File Server shutdown cancelled."*) ;07-13-88 DAB
	(blast-message-to-file-servers 3"File Server shutdown cancelled."*)))
     (setq *server-shutdown-message* ()) (return :cancelled) unless
     (eq notify-interval *server-notify-times*) do
     (let* ((minutes-to-go (and time-to-go (round time-to-go 3600.)))
	    (current-message
	     (format () 3"File server shutting down~*
						3~@[ in ~D minute~:P~] - ~A"*
		     minutes-to-go message)))
       (notify-nicely () 3"~A"* current-message)  ;07-13-88 DAB
       (cond
	 ((null minutes-to-go) (setq chaos::chaos-servers-enabled ())
	  (tv:close-all-servers current-message) (setq *server-shutdown-message* ()) (return t))
	 (t (blast-message-to-file-servers current-message))))
     do
     (process-wait 3"Shutdown Msg"*
		   #'(lambda (until-time mval)
		       (or (neq *server-shutdown-message* mval) (time-lessp until-time (time))))
		   (time-increment at-time (* (first notify-interval) -3600.)) message)))  



(defun 4send-single-shutdown-message* (conn)
  (let ((pkt (chaos:get-pkt)))
    (chaos:set-pkt-string pkt
			  (format () 3"File Server shutting down in ~D minute~:P - ~A"*
				  (round (- *server-shutdown-time* (get-universal-time)) 60.)
				  *server-shutdown-message*))
    (chaos:send-pkt conn pkt %file-notification-opcode)))  



(defun 4blast-message-to-file-servers* (message)
  (loop for server in (funcall tv:who-line-file-state-sheet :servers) when
     (equalp (tv::server-desc-contact-name server) 3"FILE"*) do
     (let ((pkt (chaos:get-pkt)))
       (chaos:set-pkt-string pkt message)
       (chaos:send-pkt (tv::server-desc-connection server) pkt %file-notification-opcode))))  



(defun 4lmfs-peek-server* (sg)
  (let ((itag (symeval-in-stack-group 'server-instance sg)))
    (list (quote nil)
	  (tv:scroll-parse-item 3"    User: "*
				`(:function ,(function symeval-in-stack-group) (user-id ,sg) 15.)
				3"    Server Tag: "* (string itag))
	  (tv:scroll-maintain-list `(lambda ()
				      (symeval-in-stack-group 'alldatas ',sg))
				   `(lambda (x)
				      (lmfs-peek-data-process (server-dataproc-comm-cell x)
							      ',itag))))))  



(defun 4lmfs-peek-data-process* (cell itag)
  (let* ((handle (rest cell))
	 (data (get handle itag)))
    (cond
      ((null data)
       (tv:scroll-parse-item (format () 3"      Vanished process ~A, instance ~A."* handle itag)))
      (t
       (let* ((sib (server-dataproc-comm-sibling data))
	      (sibdata (get sib itag)))
	 (list () (lmfs-peek-data-process-half data) (lmfs-peek-data-process-half sibdata)))))))  



(defun 4lmfs-peek-data-process-half* (data)
  (let ((process (server-dataproc-comm-data-proc data))
	(cell (server-dataproc-comm-cell data)))
    (list
     (list :pre-process-function 'lmfs-peek-server-preprocess 'lmfs-cdata data 'cur-display
	   (cons () ()))
     (tv:scroll-parse-item 3"      "*
			   `(:mouse
			     (nil :eval (tv::peek-process-menu ',process) :documentation
			      3"Menu of useful things to do to this process."*)
			     :string ,(format () 3"~A"* (process-name process)))
			   3"    "* `(:function ,(function tv::peek-whostate) (,process))
			   3", sibling "* (string (server-dataproc-comm-sibling data))
			   (format () 3", ~A"* (server-dataproc-comm-iotype data)) 3", cmd: "*
			   `(:function ,(function (lambda (x)
						    (or (first x) 3"(Idle)"*))) (,cell)))
     ())))  



(defun 4lmfs-peek-server-preprocess* (list-item)
  (let* ((plist (locf (first list-item)))
	 (data (get plist 'lmfs-cdata))
	 (cell (get plist 'cur-display))
	 (curdisp (first cell))
	 (opening (server-dataproc-comm-opening data)))
    (cond
      ((eq curdisp opening))
      ((null opening) (setf (third list-item) ()))
      ((eq opening 'directory) (rplaca cell opening)
       (setf (third list-item) (tv:scroll-parse-item 3"         Directory state."*)))
      ((setf (third list-item) (funcall opening :peek-file-system 9.)) (rplaca cell opening)))))  

1;;; This oughtta go at THE BOTTOM.....

;; This gets slightly higher priority because the control connection doesn't have much
;; to do, but when asked it should perform promptly.
;; ** Bogus: in fact the control process does all opens, and when doing probes
;; really hard can use up the whole machine.  So, let it compete with the rest. ***


(add-initialization 3"FILE"* '(process-run-function 2"File Server"* 'file-server) ()
		    'chaos:server-alist)  
