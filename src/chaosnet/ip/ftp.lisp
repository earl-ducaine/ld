;;; -*- Mode:COMMON-LISP; Base: 10.; Fonts: (COURIER MEDFNB TR12BI); Package: ip -*-

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

(DEFFLAVOR ftp-error
	   ()
	   (tcp-stream-error))

(DEFSIGNAL control-connection-error (tcp-stream-error control-connection-error) (condition)
  "A condition was signaled in the context of the control connection")

(DEFPARAMETER ftp-timeout 10 "2FTP server's timeout period in seconds*") 

(DEFPARAMETER ftp-input-buffer-size 8192 "2FTP data connection input buffer size*") 

(DEFPARAMETER ftp-number-of-input-buffers 9) 


(DEFPARAMETER *new-ftp-commands* ()
   "2If not nil, will send the new FTP spec format directory commands instead of Berkeley format.*") 

(DEFCONSTANT ftp-server-default-port 21 "2FTP server's default port*") 

(DEFCONSTANT ftp-positive-preliminary-reply 1 "2action initiated; expect another reply*") 

(DEFCONSTANT ftp-positive-completion-reply 2 "2action successfully completed*") 

(DEFCONSTANT ftp-positive-intermediate-reply 3 "2command accepted; send more info*") 

(DEFCONSTANT ftp-negative-transient-reply 4 "2command not accepted; try again later*") 

(DEFCONSTANT ftp-negative-completion-reply 5 "2command not accepted; error*") 

(DEFCONSTANT ftp-server-close-reply 421 "2server got an error; closing connection*") 

(DEFCONSTANT ftp-server-passive-reply 227 "2successful reply to PASSIVE command*") 


(DEFFLAVOR ftp-control-connection
	   ((host nil);1 remote host*
	    (in-use nil);1 Connection in use, used by FTP streams*
	    (control-connection nil);1 FTP control stream*
	    (data-connection nil);1 FTP data stream*
	    (default-directory nil);1 User default directory*
	    (data-port-address 0);1 FTP data port address*
	    (data-port-number 0);1 FTP data port number*
	    (BYTE-SIZE 8);1 byte length for :byte-size (8 (default) or 16)*
	    (format-code :non-print);1 format code (only :non-print allowed)*
	    (mode-code :stream);1 mode code (only :stream allowed)*
	    (structure-code :file);1 structure code (:file (default) or :record)*
	    (type-code :ascii);1 type code (:ascii (default) :image or :byte-size)*
	    (passive-mode nil);1 true if :passive has been issued*
	    (reply-code nil);1 FTP reply code (from 100 to 599 or 0 or nil)*
	    (reply-string nil);1 FTP reply string (the rest after the reply code)*
	    (gc-mark nil);1 Marked for GC on next go-around, used by FS xface to FTP*
)
	   ()
  :settable-instance-variables) 


(DEFMETHOD (ftp-control-connection :get-connection) (remote-host &aux host-object)
  (SETQ host-object (parse-ip-host-spec remote-host))
  (CONDITION-CASE (condition)
      (IF (NULL host-object)
	  (PROGN
	    (SETQ reply-code ())
	    (SETQ reply-string (FORMAT () "Unknown host ~a" remote-host)))
	  (IF (AND control-connection (EQUAL host host-object)
		   (EQUAL (SEND control-connection :status) :established))
	      (PROGN
		(SETQ reply-code t)
		(SETQ reply-string "Connection already exists"))
	      (PROGN
		(IF (AND control-connection (EQUAL (SEND control-connection :status) :established))
		    (SEND self :quit))
		(SETQ host host-object)
		(IF (NULL default-directory)
		    (SETQ default-directory (MAKE-PATHNAME :host 'lm)))
		(UNWIND-PROTECT
		    (PROGN
		      (SETQ reply-code ())
		      (SETQ control-connection
			    (open-telnet-stream host
						:remote-port ftp-server-default-port
						:transmit-mode :line
						:remote-echo-preference :never
						:timeout ftp-timeout
						:timeout-after-open nil
						:negotiate-options-on-open ()
						:error t))
		      ;1; abort the data-connection when control-connection gets a condition*
		      (SETF (SEND (SEND control-connection :connection) :condition-handler)
			    (LET ((ftp-control-connection self))
			      #'(lambda (condition-object &aux dconn)
				  (SETF dconn (SEND ftp-control-connection :data-connection))
				  (WITHOUT-INTERRUPTS
				    (WHEN (AND dconn (NOT (EQ :closed (SEND dconn :status))))
				      (CATCH-ERROR (CLOSE dconn :abort t))
				      (SEND dconn :condition-handler
					    (MAKE-CONDITION
					      'control-connection-error
					      "~a~:* condition signaled in FTP control connection"
					      condition-object))))
				  (SEND (SEND ftp-control-connection :control-connection)
					:condition-handler condition-object))))
		      (SETQ type-code :ascii)
		      (SETQ byte-size 8)
		      (SETQ format-code :non-print)
		      (SETQ mode-code :stream)
		      (SETQ structure-code :file)
		      (SETQ passive-mode ())
		      (WHEN data-connection
			(IGNORE-ERRORS (CLOSE data-connection :abort t))
			(SETQ data-connection ()))
		      (SETQ data-port-number
			    (SEND (SEND control-connection :connection) :source-port))
		      (SETQ data-port-address
			    (SEND (SEND control-connection :connection) :source-address))
		      (SEND self :get-reply ftp-positive-preliminary-reply))
		  (IF (NOT
			(AND (INTEGERP reply-code)
			     (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply)))
		      (IF (AND control-connection (STREAMP control-connection))
			  (CLOSE control-connection)))))))
    (sys:network-error (UNLESS reply-code (SETF reply-string (FORMAT nil "~a" condition)))))
  (VALUES reply-code reply-string))


(DEFMETHOD (ftp-control-connection :abort) (&aux msg)
  (IF (NULL control-connection)
      (PROGN
	(SETQ reply-code ())
	(SETQ reply-string "No control connection"))
      (BLOCK abort
	(LOOP
	  (IF (NULL (LISTEN control-connection))
	      (RETURN)
	      (PROGN
		(SETQ msg (READ-LINE control-connection () ()))
		(IF msg
		    (PROGN
		      (SETQ reply-code ())
		      (SETQ reply-string (FORMAT () "From FTP Server: ~a" msg)))
		    (PROGN
		      (SETQ reply-code ())
		      (SETQ reply-string "Control connection abnormally closed")
		      (CLOSE control-connection)
		      (SETQ control-connection ())))
		(RETURN-FROM abort))))
	(WRITE-LINE "ABOR" control-connection)
	(WHEN data-connection
	  (IGNORE-ERRORS (CLOSE data-connection :abort t))
	  (SETQ data-connection ()))
	(SEND self :get-reply)))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :account) (account)
  (DECLARE (STRING account))
  (SEND self :quote (FORMAT () "ACCT ~a" account))) 


(DEFMETHOD (ftp-control-connection :get-reply) (&optional (ignore-reply -1) &aux code)
  (DECLARE (integer ignore-reply))
  (IF (NULL control-connection)
      (PROGN
	(SETQ reply-code ())
	(SETQ reply-string "control connection closed"))
      (PROGN
	(LOOP (SEND self :read-reply)
	      (IF (INTEGERP reply-code)
		  (SETQ code (FLOOR reply-code 100))
		  (RETURN))
	      (IF (NOT (EQUAL code ignore-reply))
		  (RETURN)))
	(IF (EQUAL reply-code ftp-server-close-reply)
	    (SEND self :close-connections))))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :read-reply) (&aux reply multi-line-reply)
  (DECLARE (ATOM multi-line-reply))
  (SETQ multi-line-reply ())
  (LOOP (SETQ reply (READ-LINE control-connection () ()))
	(WHEN (NULL reply)
	  (SEND self :close-connections :abort)
	  (SETQ reply-code ())
	  (SETQ reply-string "control connection closed")
	  (RETURN))
	(SETQ reply-code (PARSE-INTEGER reply :end 3 :junk-allowed t))
	(IF (OR (NULL reply-code) (NOT (INTEGERP reply-code)) (< reply-code 100) (> reply-code 599))
	    (SETQ reply-code 0))
	(IF (NULL multi-line-reply)
	    (IF (EQUAL reply-code 0)
		(SETQ reply-string reply)
		(SETQ reply-string (SUBSEQ reply 4)))
	    (IF (AND (EQUAL (CHAR reply 3) #\Space) (> reply-code 0))
		(PROGN
		  (SETQ multi-line-reply ())
		  (SETQ reply-string (CONCATENATE 'STRING reply-string "" (SUBSEQ reply 4))))
		(IF (EQUAL reply-code 0)
		    (SETQ reply-string (CONCATENATE 'STRING reply-string "" reply))
		    (SETQ reply-string (CONCATENATE 'STRING reply-string "" (SUBSEQ reply 4))))))
	(IF (AND (EQUAL (CHAR reply 3) #\-) (> reply-code 0))
	    (SETQ multi-line-reply t))
	(IF (NULL multi-line-reply)
	    (RETURN)))) 


(DEFMETHOD (ftp-control-connection :allocate) (alloc &optional record)
  (IF (AND (INTEGERP alloc) (> alloc 0))
    (IF (AND (INTEGERP record) (> record 0))
      (SEND self :quote (FORMAT () "ALLO ~d R ~d" alloc record))
      (SEND self :quote (FORMAT () "ALLO ~d" alloc)))
    (PROGN
      (SETQ reply-code ())
      (SETQ reply-string "Usage: :allocate <positive integer> &opt <positive integer")))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :change-directory) (&optional directory)
  (IF (OR (NULL directory) (EQUAL (STRING directory) ""))
    (SEND self :quote "CWD")
    (SEND self :quote (FORMAT () "CWD ~a" directory)))) 


(DEFMETHOD (ftp-control-connection :change-to-parent-directory) ()
  (IF *new-ftp-commands*
    (SEND self :quote "CDUP")
    (SEND self :quote "XCUP"))) 


(DEFVAR *ftp-data-connection-close* nil
  "2Used by ftp-streams to discern closes from ftp-control-connection.*")


(DEFMETHOD (ftp-control-connection :close) (&optional abort-p (get-reply-p t))
  (WHEN data-connection
    (LET ((*ftp-data-connection-close* t))
      (IF abort-p
         (IGNORE-ERRORS (CLOSE data-connection :abort t))
	 (CLOSE data-connection)))
    (SETQ data-connection ())
    (WHEN get-reply-p (SEND self :get-reply)))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :close-connections) (&optional abort-p)
  (WHEN data-connection
    (LET ((*ftp-data-connection-close* t))
      (IF abort-p
	  (IGNORE-ERRORS (CLOSE data-connection :abort t))
	  (CLOSE data-connection)))
    (SETQ data-connection ()))
  (WHEN control-connection
    (IF abort-p
      (IGNORE-ERRORS (CLOSE control-connection :abort t))
      (CLOSE control-connection))
    (SETQ control-connection ()))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :create-directory) (path)
  (DECLARE (PATHNAME path))
  (IF *new-ftp-commands*
    (SEND self :quote (FORMAT () "MKD ~a" path))
    (SEND self :quote (FORMAT () "XMKD ~a" path)))) 


(DEFMETHOD (ftp-control-connection :delete) (file)
  (DECLARE (PATHNAME file))
  (SEND self :quote (FORMAT () "DELE ~a" file))) 


(DEFMETHOD (ftp-control-connection :delete-directory) (path)
  (DECLARE (PATHNAME path))
  (IF *new-ftp-commands*
    (SEND self :quote (FORMAT () "RMD ~a" path))
    (SEND self :quote (FORMAT () "XRMD ~a" path)))) 


(DEFMETHOD (ftp-control-connection :help) (&optional help)
  (IF (OR (NULL help) (EQUAL (STRING help) ""))
    (SEND self :quote "HELP")
    (SEND self :quote (FORMAT () "HELP ~a" help)))) 


(DEFMETHOD (ftp-control-connection :list) (&optional file list-stream &aux stream done)
  (COND
    ((EQUAL list-stream t) (SETQ stream *standard-output*))
    ((NULL list-stream) (SETQ stream (MAKE-STRING-OUTPUT-STREAM)))
    (t (SETQ stream list-stream)))
  (WITH-OPEN-STREAM (tofile stream)
    (SEND self :open file :list)
    (IF (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
      (UNWIND-PROTECT (PROGN
		       (SETQ done ())
		       (STREAM-COPY-UNTIL-EOF data-connection tofile)
		       (SETQ done t))
	(IF done
	  (SEND self :close)
	  (PROGN
	    (WRITE-LINE "ABOR" control-connection)
	    (SEND self :close :abort)
	    (SEND self :get-reply))))))
  (IF list-stream
    (VALUES reply-code reply-string)
    (VALUES reply-code reply-string (GET-OUTPUT-STREAM-STRING stream))))


(DEFMETHOD (ftp-control-connection :nlst) (&optional file list-stream &aux stream done)
  (COND
    ((EQUAL list-stream t) (SETQ stream *standard-output*))
    ((NULL list-stream) (SETQ stream (MAKE-STRING-OUTPUT-STREAM)))
    (t (SETQ stream list-stream)))
  (WITH-OPEN-STREAM (tofile stream)
    (SEND self :open file :nlist)
    (IF (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
      (UNWIND-PROTECT (PROGN
		       (SETQ done ())
		       (STREAM-COPY-UNTIL-EOF data-connection tofile)
		       (SETQ done t))
	(IF done
	  (SEND self :close)
	  (PROGN
	    (WRITE-LINE "ABOR" control-connection)
	    (SEND self :close :abort)
	    (SEND self :get-reply))))))
  (IF list-stream
    (VALUES reply-code reply-string)
    (VALUES reply-code reply-string (GET-OUTPUT-STREAM-STRING stream)))) 


(DEFMETHOD (ftp-control-connection :login) (user &optional passw account)
  (DECLARE (STRING user))
  (SEND self :quote (FORMAT () "USER ~a" user))
  (WHEN (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-intermediate-reply)
      (NOT (NULL passw)))
    (SEND self :quote (FORMAT () "PASS ~a" passw))
    (WHEN (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-intermediate-reply)
	(NOT (NULL account)))
      (SEND self :quote (FORMAT () "ACCT ~a" account))))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :mode) (mode)
  (DECLARE (keyword mode))
  (IF (EQUAL mode :stream)
    (SEND self :quote "MODE S")
    (PROGN
      (SETQ reply-code ())
      (SETQ reply-string "Usage: only :stream mode supported")))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :nlist) (&optional file list-stream &aux stream done)
  (COND
    ((EQUAL list-stream t) (SETQ stream *standard-output*))
    ((NULL list-stream) (SETQ stream (MAKE-STRING-OUTPUT-STREAM)))
    (t (SETQ stream list-stream)))
  (WITH-OPEN-STREAM (tofile stream)
    (SEND self :open file :nlist)
    (IF (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
      (UNWIND-PROTECT (PROGN
		       (SETQ done ())
		       (STREAM-COPY-UNTIL-EOF data-connection tofile)
		       (SETQ done t))
	(IF done
	  (SEND self :close)
	  (PROGN
	    (WRITE-LINE "ABOR" control-connection)
	    (SEND self :close :abort)
	    (SEND self :get-reply))))))
  (IF list-stream
    (VALUES reply-code reply-string)
    (VALUES reply-code reply-string (GET-OUTPUT-STREAM-STRING stream)))) 


(DEFMETHOD (ftp-control-connection :noop) ()
  (SEND self :quote "NOOP")) 


(DEFMETHOD (ftp-control-connection :open) (file mode &aux type)
  (DECLARE (SPECIAL *tcp-stream-whostate*))
  (DECLARE (keyword mode))
  (BLOCK open
    (WHEN data-connection
      (CLOSE data-connection :abort t)
      (SETQ data-connection ()))
    (CASE type-code
	  (:ascii (SETQ type :ascii))
	  (:image (SETQ type t))
	  (:byte-size (IF (EQUAL byte-size 8)
			  (SETQ type t)
			  (SETQ type ())))
	  (t (SETQ reply-code ())
	     (SETQ reply-string (FORMAT () "Invalid type-code: ~a" type-code))
	     (RETURN-FROM open)))
    (WHEN (NULL data-connection)
      (SEND self :port (generate-tcp-port))
      (UNWIND-PROTECT
	  (SETQ	data-connection
		(open-stream ()
			     :local-port data-port-number
			     :remote-port (- ftp-server-default-port 1)
			     :direction (CASE mode
					  ((:input :list :nlist) :input)
					  ((:output :append) :output)
					  (otherwise :bidirectional))
			     :characters type
			     :input-buffer-size (IF type
						    ftp-input-buffer-size
						    (TRUNCATE ftp-input-buffer-size 2))
			     :number-of-input-buffers ftp-number-of-input-buffers
			     :timeout ftp-timeout
			     :timeout-after-open nil
			     :wait-for-establishment ()))
	(unreserve-tcp-port data-port-number)))
    (CASE mode
	  (:input (SEND self :quote (FORMAT () "RETR ~a" file)))
	  (:output (IF file
		       (SEND self :quote (FORMAT () "STOR ~a" file))
		       (SEND self :quote "STOU")))
	  (:append (SEND self :quote (FORMAT () "APPE ~a" file)))
	  (:list
	   (IF (OR (NULL file) (EQUAL (STRING file) ""))
	       (SEND self :quote "LIST")
	       (SEND self :quote (FORMAT () "LIST ~a" file))))
	  (:nlist
	   (IF (OR (NULL file) (EQUAL (STRING file) ""))
	       (SEND self :quote "NLST")
	       (SEND self :quote (FORMAT () "NLST ~a" file))))
	  (t (SETQ reply-code ())
	     (SETQ reply-string "Only :input, :output, :append, :list and :nlist modes accepted")
	     (RETURN-FROM open)))
    (IF (AND data-connection (INTEGERP reply-code)
	     (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
	(UNWIND-PROTECT
	    (PROCESS-WAIT (OR *tcp-stream-whostate* "FTP Open")
			  #'(lambda (c-conn d-conn)
			      (OR (EQ (SEND c-conn :status) :closed)
				  (LISTEN c-conn)
				  (CASE (SEND d-conn :status)
				    ((:closed :established :close-wait) t)
				    (otherwise nil))))
			  control-connection data-connection)
	  (IF (EQ (SEND data-connection :status) :listen)
	      (IF (LISTEN control-connection)
		  (SEND self :get-reply)
		  (PROGN
		    (SETQ reply-code ())
		    (SETQ reply-string "data connection open failed")))))))
  (WHEN (OR (NULL reply-code)
	    (AND (INTEGERP reply-code)
		 (NOT (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))))
    (WHEN data-connection
      ;1; if the data connection open fails, don't get a reply*
      (SEND self :close :abort nil)
      (SETQ data-connection ())))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :passive) (&aux (value 0) byte (index 0) (addr 0) (port 0))
  (DECLARE (integer value))
  (DECLARE (integer index))
  (DECLARE (integer addr))
  (DECLARE (integer port))
  (SEND self :quote "PASV")
  (IF (EQUAL reply-code ftp-server-passive-reply)
    (WHEN (SETQ index (POSITION-IF 'DIGIT-CHAR-P reply-string))
      (SETQ passive-mode t)
      (DO ((bytes 0 (1+ bytes)))
	  ((> bytes 3))
	(MULTIPLE-VALUE-SETQ (BYTE index)
	  (PARSE-INTEGER reply-string :start index :junk-allowed t))
	(WHEN (OR (NOT (INTEGERP byte)) (< byte 0) (> byte 255))
	  (SETQ value 0)
	  (RETURN))
	(SETQ value (+ (* value 256) byte))
	(INCF index))
      (SETQ addr value)
      (SETQ value 0)
      (DO ((bytes 0 (1+ bytes)))
	  ((> bytes 1))
	(MULTIPLE-VALUE-SETQ (BYTE index)
	  (PARSE-INTEGER reply-string :start index :junk-allowed t))
	(WHEN (OR (NOT (INTEGERP byte)) (< byte 0) (> byte 255))
	  (SETQ value 0)
	  (RETURN))
	(SETQ value (+ (* value 256) byte))
	(INCF index))
      (SETQ port value)))
  (VALUES reply-code reply-string port addr)) 


(DEFMETHOD (ftp-control-connection :password) (passw)
  (DECLARE (STRING passw))
  (SEND self :quote (FORMAT () "PASS ~a" passw))) 


(DEFMETHOD (ftp-control-connection :port) (&optional port hostname &aux addr host-object)
  (IF (AND (NULL port) control-connection (SEND control-connection :connection))
    (SETQ port (SEND (SEND control-connection :connection) :source-port)))
  (IF (OR (NOT (INTEGERP port)) (< port 0) (> port 65535))
    (PROGN
      (SETQ reply-code ())
      (SETQ reply-string (FORMAT () "Invalid port ~a" port)))
    (PROGN
      (IF (NULL hostname)
	(SETQ host-object (parse-ip-host-spec si:local-host))
	(IF (INTEGERP hostname)
	  (SETQ addr hostname)
	  (SETQ host-object (parse-ip-host-spec hostname))))
      (IF host-object
	(SETQ addr (SEND (SEND control-connection :connection) :source-address)))
      (IF (NULL addr)
	(PROGN
	  (SETQ reply-code ())
	  (SETQ reply-string (FORMAT () "unknown host ~a" hostname)))
	(PROGN
	  (SEND self :quote
	     (FORMAT () "PORT ~d,~d,~d,~d,~d,~d" (LDB (BYTE 8 24) addr) (LDB (BYTE 8 16) addr)
		     (LDB (BYTE 8 8) addr) (LDB (BYTE 8 0) addr) (LDB (BYTE 8 8) port)
		     (LDB (BYTE 8 0) port)))
	  (WHEN (AND (INTEGERP reply-code)
	      (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply))
	    (SETQ data-port-number port)
	    (SETQ data-port-address addr)
	    (SETQ passive-mode ())
	    (WHEN data-connection
	      (CLOSE data-connection)
	      (SETQ data-connection ())))))))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :get-directory) ()
  (IF *new-ftp-commands*
    (SEND self :quote "PWD")
    (SEND self :quote "XPWD"))) 


(DEFMETHOD (ftp-control-connection :quote) (cmd &aux msg)
  (DECLARE (STRING cmd))
  (IF (NULL control-connection)
    (PROGN
      (SETQ reply-code ())
      (SETQ reply-string "No control connection"))
    (BLOCK quote
      (LOOP
       (IF (NULL (LISTEN control-connection))
	 (RETURN)
	 (PROGN
	   (SETQ msg (READ-LINE control-connection () ()))
	   (IF msg
	     (PROGN
	       (SETQ reply-code ())
	       (SETQ reply-string (FORMAT () "From FTP Server: ~a" msg)))
	     (PROGN
	       (SETQ reply-code ())
	       (SETQ reply-string "Control connection abnormally closed")
	       (CLOSE control-connection)
	       (SETQ control-connection ())))
	   (RETURN-FROM quote))))
      (WRITE-LINE cmd control-connection)
      (SEND self :get-reply)))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :quit) ()
  (SEND self :quote "QUIT")
  (SEND self :close-connections)
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :reinitialize) ()
  (SEND self :quote "REIN")
  (IF (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
    (SEND self :get-reply ftp-positive-preliminary-reply))
  (SETQ type-code :ascii)
  (SETQ byte-size 8)
  (SETQ format-code :non-print)
  (SETQ mode-code :stream)
  (SETQ structure-code :file)
  (SETQ passive-mode ())
  (WHEN data-connection
    (CLOSE data-connection)
    (SETQ data-connection ()))
  (SETQ data-port-number (SEND (SEND control-connection :connection) :source-port))
  (SETQ data-port-address (SEND (SEND control-connection :connection) :source-address))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :rename) (frompath topath)
  (DECLARE (STRING frompath))
  (DECLARE (STRING topath))
  (SEND self :quote (FORMAT () "RNFR ~a" frompath))
  (IF (AND (INTEGERP reply-code)
      (OR (EQUAL (FLOOR reply-code 100) ftp-positive-intermediate-reply)
	 (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply)))
    (SEND self :quote (FORMAT () "RNTO ~a" topath)))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :retrieve) (frompath topath &optional open-mode &aux type bytes time done)
  (DECLARE (STRING frompath))
  (DECLARE (PATHNAME topath))
  (DECLARE (ATOM done))
  (IF (EQUAL type-code :ascii)
    (SETQ type :ascii)
    (SETQ type ()))
  (IF (NULL open-mode)
    (SETQ open-mode
	  (IF (EQUAL (SEND (MERGE-PATHNAMES topath default-directory) :version) :newest)
	    :new-version
	    :truncate)))
  (SETQ bytes 0)
  (SETQ time 0)
  (WITH-OPEN-FILE (tofile (MERGE-PATHNAMES topath default-directory) :direction :output :characters type
    :byte-size byte-size :if-does-not-exist :create :if-exists open-mode
    :error nil)
    (IF (OR (NULL tofile) (TYPEP tofile 'condition))
      (PROGN
	(SETQ reply-code ())
	(SETQ reply-string (FORMAT () "Error opening ~a: ~a" topath tofile)))
      (PROGN
	(SEND self :open frompath :input)
	(WHEN (AND (INTEGERP reply-code)
	    (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
	  (SETQ time (TIME))
	  (UNWIND-PROTECT (PROGN
			   (SETQ done ())
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
			     (SETQ bytes (+ bytes (- limit offset))))
			   (SETQ done t))
	    (IF done
	      (SEND self :close)
	      (PROGN
		(WRITE-LINE "ABOR" control-connection)
		(SEND self :close :abort)
		(SEND self :get-reply))))
	  (SETQ time (TIME-DIFFERENCE (TIME) time))))))
  (VALUES reply-code reply-string bytes (FLOOR time 60)
	  (IF (AND time (NOT (EQUAL time 0)))
	    (ROUND (* bytes 480) time)
	    0))) 


(DEFMETHOD (ftp-control-connection :site) (siteinfo)
  (DECLARE (STRING siteinfo))
  (SEND self :quote (FORMAT () "SITE ~a" siteinfo))) 


(DEFMETHOD (ftp-control-connection :status) (&optional path)
  (IF (OR (NULL path) (EQUAL (STRING path) ""))
    (SEND self :quote "STAT")
    (SEND self :quote (FORMAT () "STAT ~a" path)))) 


(DEFMETHOD (ftp-control-connection :store) (frompath &optional topath open-mode &aux type bytes time done)
  (DECLARE (PATHNAME frompath))
  (DECLARE (ATOM done))
  (IF (EQUAL type-code :ascii)
    (SETQ type :ascii)
    (SETQ type ()))
  (IF (NULL open-mode)
    (SETQ open-mode :output))
  (SETQ bytes 0)
  (SETQ time 0)
  (WITH-OPEN-FILE (fromfile (MERGE-PATHNAMES frompath default-directory) :direction :input :characters type
    :byte-size byte-size :if-does-not-exist :error :error nil)
    (IF (OR (NULL fromfile) (TYPEP fromfile 'condition))
      (PROGN
	(SETQ reply-code ())
	(SETQ reply-string (FORMAT () "Error: ~a" fromfile)))
      (PROGN
	(SEND self :open topath open-mode)
	(WHEN (AND (INTEGERP reply-code)
	    (EQUAL (FLOOR reply-code 100) ftp-positive-preliminary-reply))
	  (SETQ time (TIME))
	  (UNWIND-PROTECT (PROGN
			   (SETQ done ())
			   (DO ((buf)
				(offset)
				(limit))
			       (nil)
			     (MULTIPLE-VALUE-SETQ (buf offset limit)
			       (SEND fromfile :read-input-buffer))
			     (IF (NULL buf)
			       (RETURN ()))
			     (SEND data-connection :string-out buf offset limit)
			     (SEND fromfile :advance-input-buffer)
			     (SETQ bytes (+ bytes (- limit offset))))
			   (SETQ done t))
	    (IF done
	      (SEND self :close)
	      (PROGN
		(WRITE-LINE "ABOR" control-connection)
		(SEND self :close :abort)
		(SEND self :get-reply))))
	  (SETQ time (TIME-DIFFERENCE (TIME) time))))))
  (VALUES reply-code reply-string bytes (FLOOR time 60)
	  (IF (AND time (NOT (EQUAL time 0)))
	    (ROUND (* bytes 480) time)
	    0))) 


(DEFMETHOD (ftp-control-connection :structure) (type)
  (CASE type
    (:file (SEND self :quote "STRU F")
     (IF (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply))
       (SETQ structure-code :file)))
    (t (SETQ reply-code ()) (SETQ reply-string "Usage only :file structure supported")))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :system) ()
  (SEND self :quote "SYST")) 


(DEFMETHOD (ftp-control-connection :type) (type &optional format-or-length)
  (CASE type
    (:ascii
     (IF (NULL format-or-length)
       (SEND self :quote "TYPE A")
       (IF (EQUAL format-or-length :non-print)
	 (SEND self :quote "TYPE A N")
	 (PROGN
	   (SETQ reply-code ())
	   (SETQ reply-string "Usage: only :non-print format supported for :ascii"))))
     (WHEN (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply))
       (SETQ type-code :ascii)
       (SETQ format-code :non-print)
       (SETQ byte-size 8)))
    (:image (SEND self :quote "TYPE I")
     (WHEN (AND (INTEGERP reply-code) (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply))
       (SETQ type-code :image)
       (SETQ byte-size 8)))
    (:byte-size (IF (NULL format-or-length)
		  (SETQ format-or-length 8))
     (IF (AND (INTEGERP format-or-length)
	 (OR (EQUAL format-or-length 8) (EQUAL format-or-length 16)))
       (PROGN
	 (SEND self :quote (FORMAT () "TYPE L ~d" format-or-length))
	 (WHEN (AND (INTEGERP reply-code)
	     (EQUAL (FLOOR reply-code 100) ftp-positive-completion-reply))
	   (SETQ type-code :byte-size)
	   (SETQ byte-size format-or-length)))
       (PROGN
	 (SETQ reply-code ())
	 (SETQ reply-string "Only 8 or 16 allowed for byte-size"))))
    (t (SETQ reply-code ())
     (SETQ reply-string "Only :ascii [:non-print], :image and :byte-length [8 or 16] allowed")))
  (VALUES reply-code reply-string)) 


(DEFMETHOD (ftp-control-connection :user) (user)
  (DECLARE (STRING user))
  (SEND self :quote (FORMAT () "USER ~a" user))) 