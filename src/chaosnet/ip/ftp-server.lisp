;;; -*- Mode:Common-Lisp; Package: Ip; Fonts:(COURIER MEDFNB TR12BI); Base:10 -*-

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
;1;; Copyright (C) 1986, 1988 Texas Instruments Incorporated. All rights reserved.*

(IN-PACKAGE 'ip) 

(DEFPARAMETER *ftp-server-debug* ()
   "2if true, the server will not handle errors and will notify of commands received and replys sent*") 

(DEFPARAMETER ftp-server-timeout 30 "2timeout in seconds on opening active data connections*") 

(DEFCONSTANT ftp-default-port 21 "2see TCP/IP RFC 790 (assigned numbers)*") 

(DEFCONSTANT ftp-data-port 20 "2see TCP/IP RFC 790 (assigned numbers)*") 


(DEFCONSTANT ftp-server-command-list
   '(("abor" ftp-server-abor "(abort data connection")
     ("acct" ftp-server-unimplemented "unimplemented")
     ("allo" ftp-server-allo "<sp> file-size [<sp> R <sp> record-size]")
     ("appe" ftp-server-appe "<sp> file-name")
     ("cdup" ftp-server-xcup "(change to parent directory)")
     ("cwd" ftp-server-cwd "<sp> directory-name")
     ("dele" ftp-server-dele "<sp> file-name")
     ("help" ftp-server-help "[<sp> command-name")
     ("list" ftp-server-list "[<sp> path-name]")
     ("mail" ftp-server-unimplemented "unimplemented")
     ("mkd" ftp-server-xmkd "<sp> path-name")
     ("mlfl" ftp-server-unimplemented "unimplemented")
     ("mode" ftp-server-mode "<sp>S")
     ("mrcp" ftp-server-unimplemented "unimplemented")
     ("mrsq" ftp-server-unimplemented "unimplemented")
     ("msam" ftp-server-unimplemented "unimplemented")
     ("msnd" ftp-server-unimplemented "unimplemented")
     ("msom" ftp-server-unimplemented "unimplemented")
     ("nlst" ftp-server-list "[<sp> path-name]")
     ("noop" ftp-server-noop "(null command)")
     ("pass" ftp-server-unimplemented "unimplemented")
     ("pasv" ftp-server-pasv "(listen on data connection)")
     ("port" ftp-server-port "<sp> a1,a2,a3,a4,p1,p2")
     ("pwd" ftp-server-xpwd "(print current directory)")
     ("quit" ftp-server-quit "(terminate FTP service)")
     ("rein" ftp-server-rein "(reinitialize FTP server)")
     ("rnfr" ftp-server-rnfr "<sp> file-name")
     ("rnto" ftp-server-rnto "<sp> file-name")
     ("rest" ftp-server-unimplemented "unimplemented")
     ("retr" ftp-server-retr "<sp> file-name")
     ("rmd" ftp-server-xrmd "<sp> path-name")
     ("site" ftp-server-unimplemented "unimplemented")
     ("smnt" ftp-server-unimplemented "unimplemented")
     ("stat" ftp-server-stat "[<sp> path-name]")
     ("stor" ftp-server-stor "<sp> file-name")
     ("stou" ftp-server-stou "(store in file 'ftp-temporary')")
     ("stru" ftp-server-stru "<sp> (F or R)")
     ("syst" ftp-server-syst "(return FTP Server system type)")
     ("type" ftp-server-type "<sp> (A [<sp> N] or I or L <sp> byte-size)")
     ("user" ftp-server-user "<sp> user-name")
     ("xcup" ftp-server-xcup "(change to parent directory)")
     ("xmkd" ftp-server-xmkd "<sp> path-name")
     ("xpwd" ftp-server-xpwd "(print current directory)")
     ("xrmd" ftp-server-xrmd "<sp> path-name"))
   "2list of FTP commands.
First entry is the command name, second is the function name to handle the 
command and the last is the documentation string for the command.*") 


(DEFUN ftp-server (&aux command fn index)
  "2Main function of the FTP Server process.
The process is started by the TCP code when it receives a packet
to the FTP default port.*"
  (LET ((default-cons-area *ip-area*)	   ;1 cons only in *ip-area**
	(default-directory nil)		   ;1 user's default directory*
	(user-name nil)			   ;1 records the user id at login*
	(control-connection nil)	   ;1 telnet connection for commands*
	(data-connection nil)		   ;1 TCP connection for data*
	(file-stream nil)		   ;1 file associated with data connection*
	(data-port-number 0)		   ;1 user data port*
	(data-port-address 0)		   ;1 user's IP address*
	(type-code :ascii)		   ;1 data transfer type*
	(BYTE-SIZE 8)			   ;1 data transfer byte size*
	(FORMAT :non-print)		   ;1 data transfer format*
	(mode :stream)			   ;1 data transfer mode*
	(structure :file)		   ;1 data transfer structure*
	(cmd nil)			   ;1 current command*
	(ARG ())			   ;1 current command's argument list*
	(previous-cmd nil)		   ;1 previous command*
	(previous-arg nil))		   ;1 previous command's argument list*
    (DECLARE (SPECIAL default-cons-area	default-directory user-name control-connection
		      data-connection file-stream data-port-number data-port-address
		      type-code byte-size format mode structure cmd arg previous-cmd previous-arg))
    ;1; close the control connection always*
    (UNWIND-PROTECT
	(CONDITION-BIND
	  ((nil 'ftp-server-error-handler))
	  ;1; handle errors*
	  (CATCH 'errors
	    ;1; open the control connection*
	    (SETQ control-connection
		  (open-telnet-stream () :local-port ftp-default-port
				      :transmit-mode :line
				      :remote-echo-preference :never
				      :timeout ftp-server-timeout
				      :timeout-after-open nil
				      :negotiate-options-on-open ()))
	    (WHEN (OR (NULL control-connection)
		      ;1; could not open*
		      (TYPEP control-connection 'condition))
	      (IF *ftp-server-debug*
		  (tv:notify () "FTP-Server: make-stream error: ~a" control-connection))
	      (RETURN-FROM ftp-server ()))	    	    
	    ;1; add server information to wholine*
	    (SEND tv:who-line-file-state-sheet :add-server
		  control-connection "FTP" si:current-process
		  'ip:peek-servers-ftp (process-stack-group current-process))
	    ;1; initialize the server to defaults*
	    (ftp-server-initialize)
	    ;1; reply to user*
	    (FORMAT control-connection "220 ~a FTP server (Version ~d.~d) at "
		    (SEND si:local-host :name-as-file-computer)
		    (si:get-system-version "IP")
		    (NTH-VALUE 1 (si:get-system-version "IP")))
	    (time:print-current-time control-connection)
	    (FORMAT control-connection "-~a; default directory: ~a~%"
		    (time:timezone-string) default-directory)
	    ;1; loop forever*
	    (LOOP
	      ;1; get next command*
	      (SETQ command (IGNORE-ERRORS (READ-LINE control-connection () ())))
	      (WHEN (NULL command)
		;1; something went wrong*
		(IF *ftp-server-debug*
		    (tv:notify () "FTP-Server: read-line returned nil"))
		(IGNORE-ERRORS (CLOSE control-connection :abort t)
			       (SEND tv:who-line-file-state-sheet :delete-server control-connection))
		(SETQ control-connection ())
		(RETURN ()))
	      ;1; parse the command*
	      (SETQ cmd (SUBSEQ command 0 (SETQ index (POSITION #\Space command))))
	      ;1; is there an argument?*
	      (IF index
		  (SETQ arg (SUBSEQ command (1+ index) ()))
		  (SETQ arg ""))
	      (IF *ftp-server-debug*
		  (tv:notify () "FTP-Server: ~a~%Parsed: ~a, ~a" command cmd arg))
	      ;1; try to find the command*
	      (SETQ fn (CDR (ASSOC cmd ftp-server-command-list :test 'STRING-EQUAL)))
	      (IF fn
		  ;1; found the command so handle it*
		  (FUNCALL (CAR fn))
		  (FORMAT control-connection
			  "500 Syntax error, ~a command unrecognized, try HELP~%" cmd))
	      ;1; if it is quit, quit*
	      (WHEN (EQUAL (CAR fn) 'ftp-server-quit)
		(IGNORE-ERRORS (CLOSE control-connection)
			       (SEND tv:who-line-file-state-sheet :delete-server control-connection))
		(SETQ control-connection ())
		(IF *ftp-server-debug*
		    (tv:notify () "FTP-Server: Successful close"))
		(RETURN t))
	      ;1; get ready for next command*
	      (SETQ previous-cmd cmd) (SETQ previous-arg arg))))
      (WHEN control-connection
	(IGNORE-ERRORS (CLOSE control-connection :abort t)
		       (SEND tv:who-line-file-state-sheet :delete-server control-connection)))
      ))) 
    
;1; add ftp server to TCP's server list*

(add-to-server-list ftp-default-port 'ftp-server "2FTP Server*") 


(DEFUN ftp-server-error-handler (condition)
  "2FTP Server's error handler.
This handles all miscellaneous errors, it just shuts down the server.
Condition is the error passed to it.*"
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL cmd))
  (DECLARE (SPECIAL previous-cmd))
  ;1; notify local user*
  (tv:notify () "FTP Server got an error: ~a" condition)
  ;1; do not handle the error if debugging*
  (IF (NULL *ftp-server-debug*)
      ;1; do not handle dangerous or debug errors*
      (UNLESS (OR (SEND condition :dangerous-condition-p) (SEND condition :debugging-condition-p))
	;1; close data connection if necessary*
	(WHEN data-connection
	  (IGNORE-ERRORS (CLOSE data-connection :abort t))
	  (SETQ data-connection ()))
	;1; if we still have a control connection*
	(WHEN (AND control-connection (EQUAL (SEND control-connection :status) :established))
	  ;1; send closing reply*
	  (FORMAT control-connection "421 Error: ~a; closing connection~%" condition)
	  ;1; else just shut down*
	  (IGNORE-ERRORS (CLOSE control-connection)
			 (SEND tv:who-line-file-state-sheet :delete-server control-connection))
	  (SETQ control-connection ()))
	(THROW 'errors
	       ())))) 


(DEFUN ftp-server-initialize ()
  "2FTP Server's initialization function.
It sets all instance variables to their defaults.*"
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL user-name))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (DECLARE (SPECIAL format))
  (DECLARE (SPECIAL mode))
  (DECLARE (SPECIAL structure))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL data-port-number))
  (DECLARE (SPECIAL data-port-address))
  (SETQ default-directory
	(MAKE-PATHNAME :host 'lm :directory :root :name :wild :type :unspecific :version :newest))
  (SETQ user-name ())			   ;1 not logged in yet*
  (SETQ type-code :ascii)		   ;1 for defaults, see the RFC*
  (SETQ byte-size 8)
  (SETQ format :non-print)
  (SETQ mode :stream)
  (SETQ structure :file)
  (WHEN data-connection			   ;1 this is for reinitialize*
    (IGNORE-ERRORS (CLOSE data-connection :abort t))
    (SETQ data-connection ()))
  ;1; get the remote socket*
  (SETQ data-port-number (NTH-VALUE 1 (SEND control-connection :status)))
  (SETQ data-port-address (NTH-VALUE 2 (SEND control-connection :status)))
  (WHEN (OR (NULL user-id)		   ;1 if no one is logged in*
	    (EQUAL user-id ""))
    (LOGIN "File Server" 'lm t)		   ;1 login as File Server*
    (fs::print-server-login-exegesis))	   ;1 and notify local user*
  )


(DEFUN ftp-server-abor ()
  "2FTP Server's handler for 'ABOR<CRLF>'*"
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  ;1; close data connection if necessary*
  (WHEN data-connection
    (IGNORE-ERRORS (CLOSE data-connection :abort t))
    (SETQ data-connection ()))
  (WRITE-LINE "226 data connection closed; no data transfers in progress" control-connection)) 


(DEFUN ftp-server-abort-check (&aux command)
  "2FTP Server's check for ABOR command during data transfer.*"
  (DECLARE (SPECIAL control-connection))
  ;1; if listen returns nil, we return nil*
  (WHEN (LISTEN control-connection)
    (SETQ command (IGNORE-ERRORS (READ-LINE control-connection () ())))
    (WHEN (NULL command)
      (IF *ftp-server-debug*
	  (tv:notify () "FTP-Server: read-line returned nil"))
      (IGNORE-ERRORS (CLOSE control-connection :abort t)
		     (SEND tv:who-line-file-state-sheet :delete-server control-connection))
      (SETQ control-connection ())
      (RETURN-FROM ftp-server-abort-check ()))
    (IF (STRING-EQUAL command "ABOR")
	(WRITE-LINE "426 data connection closed; data transfer aborted" control-connection)
	(WRITE-LINE "422 ignoring commands until data transfer complete" control-connection))
    (VALUES t))) 


(DEFUN ftp-server-allo ()
  "2FTP Server's handler for 'ALLO<SP><decimal-integer>
                                      [<SP>R<SP><decimal-integer>]<CRLF>'*"
  (DECLARE (SPECIAL control-connection))
  (WRITE-LINE "202 No storage/record-size allocation necessary" control-connection)) 


(DEFUN ftp-server-appe ()
  "2FTP Server's handler for 'APPE<SP><pathname><CRLF>'*"
  (ftp-server-store :append)) 

(DEFUN ftp-server-cwd (&aux path result)
  "2FTP Server's handler for 'CWD<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(IF (EQUAL arg "")
	    (SETQ path
		  (SEND default-directory :new-pathname :host 'lm :directory user-name :name :wild
			:type :unspecific :version :newest))
	    (SETQ path (FTP-PARSE-FOR-SERVER arg default-directory)))
	(IF (TYPEP (SETQ result (fs:directory-list path :noerror)) 'condition)
	    (FORMAT control-connection "550 Cannot change to ~a: ~a~%" path result)
	    (PROGN
	      (SETQ default-directory path)
	      (FORMAT control-connection "250 Directory changed to ~a~%" path))))))
 


(DEFUN ftp-server-data-conn (path &aux type)
  "2FTP Server's internal function to open a data connection*"
  (DECLARE (PATHNAME path))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL previous-cmd))
  (DECLARE (SPECIAL data-port-number))
  (DECLARE (SPECIAL data-port-address))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (PROGN
	(WRITE-LINE "530 Please login first" control-connection)
	(VALUES ()))
      (PROGN
	(ECASE type-code
	  (:ascii (SETQ type :ascii))
	  (:image (SETQ type t))
	  (:byte-size (IF (EQUAL byte-size 8)
			  (SETQ type t)
			  (SETQ type ()))))
	(IF (AND data-connection
		 (OR (EQUAL (SEND data-connection :status) :established)
		     (STRING-EQUAL previous-cmd "pasv")))
	    (PROGN
	      (FORMAT control-connection "125 Using existing data connection for ~a~%" path)
	      (VALUES data-connection))
	    ;1; ELSE*
	    (PROGN
	      (WHEN data-connection
		(IGNORE-ERRORS (CLOSE data-connection :abort t))
		(SETQ data-connection ()))
	      (FORMAT control-connection "150 Opening data connection for ~a~%" path)
	      (SETQ data-connection
		    (open-stream data-port-address
				 :local-port ftp-data-port
				 :remote-port data-port-number
				 :characters type
				 :input-buffer-size ftp-input-buffer-size
				 :number-of-input-buffers ftp-number-of-input-buffers
				 :timeout ftp-server-timeout
				 :timeout-after-open nil
				 :error ()))
	      (IF *ftp-server-debug*
		  (tv:notify () "FTP-Server: data-connection: ~a" data-connection))
	      (IF (OR (NULL data-connection) (TYPEP data-connection 'condition))
		  (PROGN
		    (FORMAT control-connection "425 Cannot open data connection for ~a: ~a~%" path
			    data-connection)
		    (SETQ data-connection ())
		    (VALUES ()))
		  (VALUES data-connection))))))) 

(DEFUN ftp-server-list (&aux path dir done)
  "2FTP Server's handler for 'LIST[<SP><pathname>]<CRLF>' and 'NLST[<SP><pathname>]<CRLF>'*"
  (DECLARE (ATOM done))
  (DECLARE (SPECIAL cmd))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (SETQ path arg)
  (IF (EQUAL path "")
      (SETQ path default-directory))
  (IF *ftp-server-debug*
      (tv:notify () "FTP-Server-list: path ~a" path))
  (SETQ dir (fs:directory-list (FTP-PARSE-FOR-SERVER path (SEND default-directory :new-pathname :version :wild))
						:noerror))
  (IF (TYPEP dir 'condition)
      (FORMAT control-connection "450 Cannot list ~a: ~a~%" path dir)
      (PROGN
	(IF (ftp-server-data-conn path)
	    (UNWIND-PROTECT
		(BLOCK list
		  (SETQ done ())
		  (IF (STRING-EQUAL cmd "list")
		      (DOLIST (file (CDR dir))
			(WHEN (SECOND
				(MULTIPLE-VALUE-LIST
				  (IGNORE-ERRORS
				    (zwei::default-list-one-file file data-connection
				      :string-for-host))))
			  (RETURN-FROM list))
			(IF (ftp-server-abort-check)
			    (RETURN-FROM list)))
		      (DOLIST (file (CDR dir))
			(UNLESS (IGNORE-ERRORS
				  (WRITE-LINE (SEND (CAR file) :string-for-host) data-connection))
			  (RETURN-FROM list))
			(IF (ftp-server-abort-check)
			    (RETURN-FROM list))))
		  (SETQ done t))
	      (IF done
		  (IGNORE-ERRORS (CLOSE data-connection))
		  (IGNORE-ERRORS (CLOSE data-connection :abort t)))
	      (SETQ data-connection ())
	      (IF done
		  (FORMAT control-connection "226 ~a listed~%" path)
		  (FORMAT control-connection "226 listing ~a aborted~%" path)))))))

(DEFUN ftp-server-dele (&aux path result)
  "2FTP Server's handler for 'DELE<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(IF (NULL (SETQ path (PROBE-FILE (FTP-PARSE-FOR-SERVER arg default-directory))))
	    (FORMAT control-connection "550 ~a does not exist.~%" arg)
	    (PROGN
	      (SETQ result (SEND path :delete-and-expunge ()))
	      (IF (TYPEP result 'condition)
		  (FORMAT control-connection "550 Cannot delete ~a: ~a~%" path result)
		  (FORMAT control-connection "250 ~a deleted~%" path)))))))


(DEFUN ftp-server-help (&aux help-string)
  "2FTP Server's handler for 'HELP[<SP><string>]<CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL control-connection))
  (WHEN (EQUAL arg "")
    (WRITE-LINE "211-The following commands are recognized (* == unimplemented)"
		control-connection)
    (WRITE-LINE "    ABOR ACCT* ALLO APPE CDUP CWD DELE HELP LIST MAIL* MKD MLFL*"
		control-connection)
    (WRITE-LINE "    MODE MRCP* MRSQ* MSAM* MSND* MSOM* NLST NOOP PASS* PASV PORT"
		control-connection)
    (WRITE-LINE "    PWD QUIT REIN RMD RNFR RNTO REST* RETR SITE* SMNT* STAT STOR"
		control-connection)
    (WRITE-LINE "    STOU STRU SYST TYPE USER XCUP XMKD XPWD XRMD" control-connection)
    (WRITE-LINE "211 Direct comments to Texas Instruments Explorer Customer Service"
		control-connection)
    (RETURN-FROM ftp-server-help))
  (SETQ help-string (CDR (ASSOC arg ftp-server-command-list :test 'STRING-EQUAL)))
  (IF (NULL help-string)
      (FORMAT control-connection "501 Unknown command: ~a~%" arg)
      (FORMAT control-connection "214 Syntax: ~a ~a~%" arg (SECOND help-string)))) 
  

  

(DEFUN ftp-server-mode ()
  "2FTP Server's handler for 'MODE<SP><mode-code><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL mode))
  (IF (EQUAL arg "")
      (WRITE-LINE "501 Missing MODE argument" control-connection)
      (IF (STRING-EQUAL arg "S")
	  (PROGN
	    (SETQ mode :stream)
	    (WRITE-LINE "200 MODE S accepted" control-connection))
	  (WRITE-LINE "504 Only MODE S implemented" control-connection)))) 


(DEFUN ftp-server-noop ()
  "2FTP Server's handler for 'NOOP<CRLF>'*"
  (DECLARE (SPECIAL cmd))
  (DECLARE (SPECIAL control-connection))
  (FORMAT control-connection "200 ~a command accepted~%" cmd)) 


(DEFUN ftp-server-pasv (&aux type addr port)
  "2FTP Server's handler for 'PASV<CRLF>'*"
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-port-number))
  (DECLARE (SPECIAL data-port-address))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (PROGN
	(WRITE-LINE "530 Please login first" control-connection)
	(VALUES ()))
      (PROGN
	(ECASE type-code
	  (:ascii (SETQ type :ascii))
	  (:image (SETQ type t))
	  (:byte-size (IF (EQUAL byte-size 8)
			  (SETQ type t)
			  (SETQ type ()))))
	(WHEN data-connection
	  (IGNORE-ERRORS (CLOSE data-connection :abort t))
	  (SETQ data-connection ()))
	(SETQ data-connection
	      (open-stream () :characters type :input-buffer-size ftp-input-buffer-size
			   :number-of-input-buffers ftp-number-of-input-buffers
			   :wait-for-establishment ()))
	(SETQ addr (get-ip-address si:local-host))
	(SETQ port (SEND (SEND data-connection :connection) :source-port))
	(FORMAT control-connection "227 Entering Passive Mode.  ~d,~d,~d,~d,~d,~d~%"
		(LDB (BYTE 8 24) addr) (LDB (BYTE 8 16) addr) (LDB (BYTE 8 8) addr)
		(LDB (BYTE 8 0) addr) (LDB (BYTE 8 8) port) (LDB (BYTE 8 0) port))))) 



(DEFUN ftp-server-port (&aux byte (value 0) (index 0) addr hostname)
  "2FTP Server's handler for 'PORT<SP><host-port><CRLF>'*"
  (DECLARE (integer value))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL data-port-number))
  (DECLARE (SPECIAL data-port-address))
  (DECLARE (SPECIAL control-connection))
  (DO ((bytes 0 (1+ bytes)))
      ((> bytes 3))
    (MULTIPLE-VALUE-SETQ (BYTE index)
			 (PARSE-INTEGER arg :start index :junk-allowed t))
    (WHEN (OR (NOT (INTEGERP byte)) (< byte 0) (> byte 255))
      (FORMAT control-connection "501 Invalid port ~a~%" arg)
      (RETURN-FROM ftp-server-port ()))
    (SETQ value (+ (* value 256) byte))
    (INCF index))
  (SETQ addr value)
  (SETQ value 0)
  (DO ((bytes 0 (1+ bytes)))
      ((> bytes 1))
    (MULTIPLE-VALUE-SETQ (BYTE index)
			 (PARSE-INTEGER arg :start index :junk-allowed t))
    (WHEN (OR (NOT (INTEGERP byte)) (< byte 0) (> byte 255))
      (FORMAT control-connection "501 Invalid port ~a~%" arg)
      (RETURN-FROM ftp-server-port ()))
    (SETQ value (+ (* value 256) byte))
    (INCF index))
  (SETQ data-port-number value)
  (SETQ data-port-address addr)
  (FORMAT control-connection "200 Port ~a (~a port ~d) accepted~%" arg
	  (IF (SETQ hostname (si:get-host-from-address data-port-address :ip))
	      hostname
	      "UNKNOWN")
	  data-port-number)) 


(DEFUN ftp-server-quit ()
  "2FTP Server's handler for 'QUIT<CRLF>'*"
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (WHEN data-connection
    (IGNORE-ERRORS (CLOSE data-connection :abort t))
    (SETQ data-connection ()))
  (FORMAT control-connection "221 ~a FTP server (Version ~d.~d) closing TELNET connection at "
	  (SEND si:local-host :name-as-file-computer) (si:get-system-version "IP")
	  (NTH-VALUE 1 (si:get-system-version "IP")))
  (time:print-current-time control-connection)
  (FORMAT control-connection "-~a~%" (time:timezone-string))) 


(DEFUN ftp-server-rein ()
  "2FTP Server's handler for 'REIN<CRLF>'*"
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (ftp-server-initialize)
  (FORMAT control-connection "220 ~a FTP server (Version ~d.~d) at "
	  (SEND si:local-host :name-as-file-computer) (si:get-system-version "IP")
	  (NTH-VALUE 1 (si:get-system-version "IP")))
  (time:print-current-time control-connection)
  (FORMAT control-connection "-~a; default directory: ~a~%" (time:timezone-string)
	  default-directory)) 


(DEFUN ftp-server-rnfr (&aux path)
  "2FTP Server's handler for 'RNFR<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(IF (PROBE-FILE (SETQ path (FTP-PARSE-FOR-SERVER arg default-directory)))
	    (FORMAT control-connection "350 Rename ~a to:~%" path)
	    (FORMAT control-connection "550 ~a does not exist.~%" path))))) 


(DEFUN ftp-server-rnto (&aux old-path new-path)
  "2FTP Server's handler for 'RNTO<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL previous-cmd))
  (DECLARE (SPECIAL previous-arg))
  (WHEN (NOT (STRING-EQUAL previous-cmd "rnfr"))
    (WRITE-LINE "503 Missed RNFR" control-connection)
    (RETURN-FROM ftp-server-rnto))
  (RENAME-FILE (SETQ old-path (FTP-PARSE-FOR-SERVER previous-arg default-directory))
	       (SETQ new-path (FTP-PARSE-FOR-SERVER arg default-directory)))
  (IF (PROBE-FILE new-path)
      (FORMAT control-connection "250 File ~a renamed to ~a~%" old-path new-path)
      (FORMAT control-connection "553 File ~a not renamed~%" old-path))) 


(DEFUN ftp-server-retr (&aux mode path done)
  "2FTP Server's handler for 'RETR<SP><pathname><CRLF>'*"
  (DECLARE (ATOM mode))
  (DECLARE (ATOM done))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (IF (EQUAL type-code :ascii)
      (SETQ mode t)
      (SETQ mode ()))
  (SETQ path (FTP-PARSE-FOR-SERVER arg default-directory))
  (WITH-OPEN-FILE (fromfile path :if-does-not-exist :error :characters mode
			    :byte-size byte-size :error nil)
    (IF (OR (NULL fromfile) (TYPEP fromfile 'condition))
	(FORMAT control-connection "550 Cannot access ~a: ~a~%" arg fromfile)
	(IF (ftp-server-data-conn path)
	    (LET ((file-stream fromfile))
	      (DECLARE (SPECIAL file-stream))
	      (UNWIND-PROTECT
		  (BLOCK retrieve
		    (SETQ done ())
		    (DO ((buf)
			 (offset)
			 (limit))
			(nil)
		      (MULTIPLE-VALUE-SETQ (buf offset limit)
			(SEND fromfile :read-input-buffer))
		      (IF (NULL buf)
			  (RETURN ()))
		      (WHEN (SECOND
			      (MULTIPLE-VALUE-LIST
				(IGNORE-ERRORS (SEND data-connection :string-out buf offset limit))))
			(RETURN-FROM retrieve))
		      (SEND fromfile :advance-input-buffer)
		      (IF (ftp-server-abort-check)
			  (RETURN-FROM retrieve)))
		    (SETQ done t))
		(IF done
		    (IGNORE-ERRORS (CLOSE data-connection))
		    (IGNORE-ERRORS (CLOSE data-connection :abort t)))
		(SETQ data-connection ())
		(IF done
		    (FORMAT control-connection "226 ~a retrieved~%" arg)
		    (FORMAT control-connection "226 retrieving ~a aborted~%" arg))))))))  


(DEFUN ftp-server-stat (&aux path dir)
  "2FTP Server's handler for 'STAT[<SP><pathname>]<CRLF>'*"
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL data-port-number))
  (DECLARE (SPECIAL data-port-address))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (DECLARE (SPECIAL format))
  (DECLARE (SPECIAL mode))
  (DECLARE (SPECIAL structure))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL previous-cmd))
  (DECLARE (SPECIAL user-name))
  (IF (EQUAL arg "")
      (PROGN
	(FORMAT control-connection "211-User: ~a, Directory: ~a~%" (IF user-name
								       user-name
								       "none")
		default-directory)
	(FORMAT control-connection " Data-port ~d, Data-address ~d~%" data-port-number
		data-port-address)
	(FORMAT control-connection " Type: ~a, Byte-size: ~d, Previous cmd: ~a~%" type-code
		byte-size previous-cmd)
	(FORMAT control-connection " Format: ~a, Mode: ~a, Structure: ~a~%" format mode structure)
	(FORMAT control-connection "211 Control status: ~a, Data status: ~a~%"
		(SEND control-connection :status)
		(IF data-connection
		    (SEND data-connection :status)
		    "No connection")))
      (IF (NULL user-name)
	  (WRITE-LINE "530 Please login first" control-connection)
	  (PROGN
	    (SETQ path (FTP-PARSE-FOR-SERVER arg default-directory))
	    (SETQ dir (fs:directory-list path :noerror))
	    (IF (TYPEP dir 'condition)
		(FORMAT control-connection "450 Cannot list ~a: ~a~%" arg dir)
		(CASE (LENGTH dir)
		      (1 (WRITE-LINE "212-Directory listing:" control-connection)
			 (zwei::default-list-one-file (CAR dir) control-connection :string-for-printing)
			 (FORMAT control-connection "212 ~a listed~%" arg))
		      (2 (WRITE-LINE "213-File listing:" control-connection)
			 (zwei::default-list-one-file (CADR dir) control-connection :string-for-printing)
			 (FORMAT control-connection "213 ~a listed~%" path))
		      (t (WRITE-LINE "212-Directory listing:" control-connection)
			 (DOLIST (file (CDR dir))
			   (zwei::default-list-one-file file control-connection :string-for-printing))
			 (FORMAT control-connection "212 ~a listed~%" path)))))))) 


(DEFUN ftp-server-stor ()
  "2FTP Server's handler for 'STOR<SP><pathname><CRLF>'*"
  (ftp-server-store :truncate)) 


(DEFUN ftp-server-store (creation-mode &aux mode path done)
  "2FTP Server's internal routine used by STOR, APPE and STOU*"
  (DECLARE (keyword creation-mode))
  (DECLARE (ATOM mode))
  (DECLARE (ATOM done))
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (DECLARE (SPECIAL data-connection))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (IF (EQUAL type-code :ascii)
      (SETQ mode t)
      (SETQ mode ()))
  (SETQ path (FTP-PARSE-FOR-SERVER arg default-directory))
  (IF (AND (EQUAL creation-mode :truncate) (EQUAL (SEND path :version) :newest))
      (SETQ creation-mode :new-version))
  (WITH-OPEN-FILE (tofile path :direction :output :if-exists creation-mode :if-does-not-exist :create
			  :characters mode :byte-size byte-size :error nil)
    (IF (OR (NULL tofile) (TYPEP tofile 'condition))
	(FORMAT control-connection "553 Cannot access ~a: ~a~%" arg tofile)
	(IF (ftp-server-data-conn path)
	    (LET ((file-stream tofile))
	      (DECLARE (SPECIAL file-stream))
	      (UNWIND-PROTECT
		  (BLOCK store
		    (SETQ done ())
		    (IF user-name
			(SEND tofile :change-properties () :author user-name))
		    (DO ((buf)
			 (offset)
			 (limit))
			(nil)
		      (MULTIPLE-VALUE-SETQ (buf offset limit)
			(SEND data-connection :read-input-buffer))
		      (IF (NULL buf)
			  (RETURN ()))
		      (SEND tofile :string-out buf offset limit)
		      (SEND data-connection :advance-input-buffer)
		      (IF (ftp-server-abort-check)
			  (RETURN-FROM store)))
		    (SETQ done t))
		(IF done
		    (IGNORE-ERRORS (CLOSE data-connection))
		    (IGNORE-ERRORS (CLOSE data-connection :abort t)))
		(SETQ data-connection ())
		(IF done
		    (FORMAT control-connection "226 ~a ~a~%" arg
			    (IF (EQUAL creation-mode :append)
				"appended"
				"stored"))
		    (FORMAT control-connection "226 ~a ~a aborted~%"
			    (IF (EQUAL creation-mode :append)
				"appending"
				"storing")
			    arg))))))))  


(DEFUN ftp-server-stou ()
  "2FTP Server's handler for 'STOU<CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (IF (EQUAL arg "")
      (SETQ arg
	    (IF (EQUAL byte-size 16)
		"ftp-temporary.xfasl#>"
		(IF (EQUAL type-code :ascii)
		    "ftp-temporary.text#>"
		    "ftp-temporary.bin#>")))
      (FORMAT control-connection "501 Extra argument: ~a to STOU~%" arg))
  (ftp-server-store :new-version)) 


(DEFUN ftp-server-stru ()
  "2FTP Server's handler for 'STRU<SP><structure-code><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL structure))
  (IF (EQUAL arg "")
      (WRITE-LINE "501 Missing STRU argument" control-connection)
      (COND
	((STRING-EQUAL arg "F") (SETQ structure :file)
				(WRITE-LINE "200 STRU F accepted" control-connection))
	(t (WRITE-LINE "504 Only STRU F implemented." control-connection))))) 


(DEFUN ftp-server-syst ()
  "2FTP Server's handler for 'SYST<CRLF>'*"
  (DECLARE (SPECIAL control-connection))
  (WRITE-LINE "215 Explorer System (Texas Instruments Inc.)" control-connection)) 


(DEFUN ftp-server-type (&aux size)
  "2FTP Server's handler for 'TYPE<SP><type-code><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL type-code))
  (DECLARE (SPECIAL byte-size))
  (DECLARE (SPECIAL format))
  (IF (EQUAL arg "")
      (WRITE-LINE "501 Missing TYPE argument" control-connection)
      (COND
	((STRING-EQUAL arg "A" :end1 1)
	 (IF (OR (STRING-EQUAL arg "" :start1 1) (STRING-EQUAL arg " N" :start1 1))
	     (PROGN
	       (SETQ type-code :ascii)
	       (SETQ byte-size 8)
	       (SETQ format :non-print)
	       (FORMAT control-connection "200 TYPE ~a accepted~%" arg))
	     (WRITE-LINE "504 Only N implemented for TYPE A." control-connection)))
	((STRING-EQUAL arg "I")
	 (SETQ type-code :image)
	 (SETQ byte-size 8)
	 (WRITE-LINE "200 TYPE I accepted" control-connection))
	((STRING-EQUAL arg "L" :end1 1)
	 (SETQ size (PARSE-INTEGER arg :start 2 :junk-allowed t))
	 (IF (AND (INTEGERP size) (OR (= size 8) (= size 16)))
	     (PROGN
	       (SETQ type-code :byte-size)
	       (SETQ byte-size size)
	       (FORMAT control-connection "200 TYPE L ~d accepted~%" size))
	     (WRITE-LINE "504 Only 8 and 16 allowed for TYPE L." control-connection)))
	(t
	 (WRITE-LINE "504 Only TYPE A [N],TYPE I and TYPE L (8|16) implemented" control-connection))))) 


(DEFUN ftp-server-unimplemented ()
  "2FTP Server's handler for all unimplemented commands*"
  (DECLARE (SPECIAL cmd))
  (DECLARE (SPECIAL control-connection))
  (FORMAT control-connection "502 ~a not implemented, try HELP~%" cmd)) 


(DEFUN ftp-server-user (&aux path)
  "2FTP Server's handler for 'USER<SP><username><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (EQUAL arg "")
      (WRITE-LINE "501 Missing user name" control-connection)
      (PROGN
	(SETQ user-name arg)
	(SETQ path
	      (MAKE-PATHNAME :host "boot|lm"
			     :directory user-name
			     :name :wild
			     :type :unspecific
			     :version :newest))
	(IF (NOT (TYPEP (fs:directory-list path :noerror) 'condition))
	    (SETQ default-directory path))
	(FORMAT control-connection "230 User ~a logged in; default directory: ~a~%" user-name
		default-directory)))) 


(DEFUN ftp-server-xcup (&aux path)
  "2FTP Server's handler for 'XCUP<CRLF>' and 'CDUP<CRLF>'*"
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(IF (EQUAL (SEND default-directory :directory) :root)
	    (FORMAT control-connection "550 Already in :root directory: ~a~%" default-directory)
	    (PROGN
	      (SETQ path (SEND default-directory :directory-pathname-as-file))
	      (SETQ default-directory
		    (SEND path :new-pathname :name :wild :type :unspecific :version :newest))
	      (FORMAT control-connection "250 Directory changed to ~a~%" default-directory)))))) 


(DEFUN ftp-server-xmkd (&aux path result)
  "2FTP Server's handler for 'XMKD<SP><pathname><CRLF>' and 'MKD<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(SETQ path (FTP-PARSE-FOR-SERVER arg default-directory))
	(IF (PROBE-FILE (SEND path :directory-pathname-as-file))
	    (FORMAT control-connection "550 Directory ~a already exists.~%" path)
	    (IF (EQUAL (SETQ result (fs:create-directory path :error ())) t)
		(PROGN
		  (SETQ path (SEND path :directory-pathname-as-file))
		  (fs:change-file-properties path () :author user-name)
		  (FORMAT control-connection "257 #\"~a#\" directory created~%" path))
		(FORMAT control-connection "550 Cannot create ~a: ~a~%" path result)))))) 


(DEFUN ftp-server-xpwd ()
  "2FTP Server's handler for 'XPWD<CRLF>' and 'PWD<CRLF>'*"
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (FORMAT control-connection "250 ~a~%" default-directory))) 


(DEFUN ftp-server-xrmd (&aux path result)
  "2FTP Server's handler for 'XRMD<SP><pathname><CRLF>' and 'RMD<SP><pathname><CRLF>'*"
  (DECLARE (SPECIAL arg))
  (DECLARE (SPECIAL default-directory))
  (DECLARE (SPECIAL control-connection))
  (DECLARE (SPECIAL user-name))
  (IF (NULL user-name)
      (WRITE-LINE "530 Please login first" control-connection)
      (PROGN
	(SETQ path (SEND (FTP-PARSE-FOR-SERVER arg default-directory) :directory-pathname-as-file))
	(IF (PROBE-FILE path)
	    (PROGN
	      (SETQ result (SEND path :delete-and-expunge))
	      (IF (TYPEP result 'condition)
		  (FORMAT control-connection "550 Cannot delete directory ~a: ~a~%" path result)
		  (FORMAT control-connection "250 Directory ~a deleted~%" path)))
	    (FORMAT control-connection "550 Directory ~a does not exist.~%" path))))) 

(defun ftp-parse-for-server
       (string &optional (default-pathname (fs:sample-pathname "boot|lm")) &aux pathname)
  (condition-case (result)		
      (if (typep default-pathname 'fs:lispm-pathname)
	  (fs:merge-pathname-defaults string default-pathname :unspecific :newest)
	  (multiple-value-bind (device directory name type version ignore quoted-string)
	      (send default-pathname :parse-namestring nil string)
	    (setf pathname
		  (fs:make-pathname-internal
		    quoted-string (pathname-host default-pathname)
		    device directory name type version)))
	  (fs:merge-pathname-defaults pathname default-pathname :unspecific :newest))
    (pathname-error (send result :report-string))))








