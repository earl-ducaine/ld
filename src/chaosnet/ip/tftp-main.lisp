;;; -*- Mode: COMMON-LISP; Package: IP; Base: 10.; Fonts:(medfnt MEDFNB TR12BI) -*-

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

;1; Resource management functions.*

;1; TFTP packets.*


(DEFRESOURCE tftp-packets () :constructor
  (MAKE-ARRAY tftp-maximum-packet-size :type tftp-packet-type :area *ip-area*) :matcher
  #'(lambda ()
      t)
  :initial-copies 2 :initializer (ARRAY-INITIALIZE object 0)) 


(DEFMACRO deallocate-tftp-packet (pkt)
  `(IF ,pkt
       (DEALLOCATE-RESOURCE 'tftp-packets ,pkt))) 


(DEFRESOURCE tftp-xfasl-packets (pkt) :constructor
  (MAKE-ARRAY (/ tftp-maximum-packet-size 2) :type :art-16b :area *ip-area* :displaced-to pkt
	      :displaced-index-offset 0)
  :matcher #'(lambda ()
	       t)
  :free-list-size 1 :initializer
  (si:change-indirect-array object 'art-16b (LIST (/ tftp-maximum-packet-size 2)) pkt 0)) 
			     


(DEFMACRO allocate-xfasl-packet (pkt)
  `(ALLOCATE-RESOURCE 'tftp-xfasl-packets ,pkt)) 


(DEFMACRO deallocate-xfasl-packet (pkt)
  `(IF ,pkt
       (DEALLOCATE-RESOURCE 'tftp-xfasl-packets ,pkt))) 

;1; TFTP CONNECTIONS *
			       

(DEFRESOURCE tftp-connections (port remote-port remote-addr local-filename remote-filename
				    remote-hostname operation mode proc np npl)
  :constructor (MAKE-INSTANCE 'tftp-connection)
  :initializer (SEND object :initialize port remote-port remote-addr local-filename remote-filename
		     remote-hostname operation mode proc np npl)
  :initial-copies 0
  :deallocator (PROGN
		 (SETF (SEND object :local-filename) nil)
		 (SETF (SEND object :remote-filename) nil)
		 (SETF (SEND object :remote-hostname) nil)
		 (SETF (SEND object :network-port) nil)
		 (SETF (SEND object :process) nil)
		 (SETF (SEND object :stream) nil)
		 (SETF (SEND object :current-packet) nil)
		 (SETF (SEND object :network-packet) nil)
		 (SETF (SEND object :xfasl-packet) nil)))


(DEFUN allocate-tftp-connection (port remote-port remote-addr &key (local-filename "") (remote-filename "")
				 (remote-hostname "") (operation nil) (mode *tftp-default-mode*) (process nil)
				 (network-packet (ALLOCATE-RESOURCE 'tftp-packets))
				 (network-packet-length tftp-maximum-packet-size))
  (ALLOCATE-RESOURCE 'tftp-connections port remote-port remote-addr local-filename
		     remote-filename remote-hostname operation mode process network-packet
		     network-packet-length)) 


(DEFUN deallocate-tftp-connection (connection)
  (WHEN connection
    (SEND *udp-handler* :return-port (SEND connection :network-port))
    (deallocate-tftp-packet (SEND connection :current-packet))
    (deallocate-tftp-packet (SEND connection :network-packet))
    (deallocate-xfasl-packet (SEND connection :xfasl-packet))
    (DEALLOCATE-RESOURCE 'tftp-connections connection))) 


;1; TFTP MAIN FUNCTIONS*
;1;*

(DEFUN tftp-window (&optional s d)
  "2Window routine for TFTP file transfer.
    RETURNS:
       FROM is the file from which the data is read
       TO   is the file to which the data is written
       MODE is either :NETASCII or :BINARY.*"
  
  (LET* ((mode *tftp-default-mode*)
	 (null-string "     ")
	 (source (IF s
		     s
		     null-string))
	 (destination (IF d
			  d
			  null-string)))
    (DECLARE (SPECIAL source destination mode))
    (IF (CATCH 'abort
	  (tv:choose-variable-values *tftp-default-window* :margin-choices
				     '("Do it [”]" ("Abort [‘]" (THROW 'abort t))) :label
				     "TFTP--Trivial File Transfer Protocol" :extra-width 35))
	(SETF mode :abort))		   ;1 Abort*
    (LIST (IF (PLUSP (LENGTH (SETF source (STRING-TRIM '(#\Space #\Tab) source))))
	      source
	      ())
	  (IF (PLUSP (LENGTH (SETF destination (STRING-TRIM '(#\Space #\Tab) destination))))
	      destination
	      ())
	  mode))) 

;1;; TFTP-GET-HOST*
;1;;*

(DEFUN tftp-get-host (path)
  (COND
    ((STREAMP path) (SEND (SEND (PATHNAME path) :host) :name))
    ((PATHNAMEP path) (SEND (SEND path :host) :name))
    (t
     ;1; If no host name is specified then use local machine name*
     (IF (POSITION #\: (THE string (STRING path)) :test #'CHAR-EQUAL)
	 (SUBSEQ path 0 (POSITION #\: (THE string (STRING path)) :test #'CHAR-EQUAL))	   ;1rla 9/18/86*
	 si:local-host-name)))) 

;1;; KNOWN-SOFTWARE-TYPE-P*
;1;;*

(DEFUN known-software-type-p (host &aux host-obj)
  (SETF host-obj (si:parse-host host t))
  (OR (STRING-EQUAL "LM" host)
      (AND host-obj (SEND host-obj :system-type)
	   (NOT (STRING-EQUAL (SEND host-obj :system-type) 'other))))) 

;1;; TFTP-GET_TRANSFER-SPECIFIC-INFORMATION*
;1;; Return values: local-file, remote-file, remote-host-name, operation*

;1;; rla 9/22/86 - :HOST-NAMES is no longer supported, just test for host object eq-ness*
(DEFUN tftp-get-transfer-specific-information (source destination mode
					       &aux local-file remote-file operation remote remote-host-name)
  (LET* ((source-host (tftp-get-host source))
	 (destination-host (tftp-get-host destination))
	 (source-host-object (si:parse-host source-host))
	 (destination-host-object (si:parse-host destination-host)))
    ;1; Get the direction of the transfer*
    (SETF operation
	  (COND
	    ;1; Both files on the local machine.*
	    ;1; Allow loop back mode and specify source as local file. *
	    ((AND (EQ source-host-object si:local-host)
		  (EQ destination-host-object si:local-host))
	     :read)
	    ;1; The source file is on the local machine. *	1    *
	    ((EQ source-host-object si:local-host) :read)
	    ;1; The destination file is on the local machine.*
	    ((EQ destination-host-object si:local-host) :write)
	    ;1;Neither file on the local machine.*
	    (:otherwise
	     (FERROR :terminate "~&Neither file is on the local machine!~&"))))
    ;1; Get local filename*
    
    (SETF local-file (fs:parse-pathname (IF (EQ operation :read)
					    source
					    destination)
					() "LM:"))
    ;1; Get remote filename*
    
    (SETF remote (IF (EQ operation :read)
		     destination
		     source))
    (SETF remote-file
	  (IF (known-software-type-p (tftp-get-host remote))
	      (fs:parse-pathname remote () "LM:")
	      (tftp-get-filename-string remote)))
    ;1; Get remote host name*
    
    (SETF remote-host-name (IF (EQ operation :read)
			       destination-host
			       source-host))
    ;1; Check the mode, local and remote file names for validity*
    (COND
      ((NOT (tftp-valid-local-file-p local-file))
       (FERROR :terminate "~&The local file, ~A, must be a string, pathname, or stream."
	       local-file))
      ((NOT (tftp-valid-remote-file-p remote-file))
       (FERROR :terminate "~&The remote file, ~A, must be a string, or pathname." remote-file))
      ((NOT (tftp-mode-p mode))
       (FERROR :terminate (tftp-bad-mode-message mode))))
    (VALUES local-file remote-file remote-host-name operation))) 

;1;; TFTP-TRANSFER-FILE*
;1;;*
;1;; If the parameters are valid, instanciate a TFTP connection.*
;1;; Proceed with the file transfer.*
;1;;*

(DEFUN tftp-transfer-file (source destination mode)
  "2Check the parameters and if valid, instanciate a TFTP connection and proceed with 
    the file transfer.*"
  (CONDITION-CASE (condition)
      (LET (connection operation local-filename remote-filename remote-host)
	;1;kludge for performance... dont use multiple-value-bind*
	(MULTIPLE-VALUE-SETQ (local-filename remote-filename remote-host operation)
			     (tftp-get-transfer-specific-information source destination mode))
	(UNWIND-PROTECT
	    (PROGN
	      (SETF connection
		    (allocate-tftp-connection
		      (SEND *udp-handler* :get-port)	   ;1network-port*
		      tftp-port		   ;1Remote port*
		      (get-ip-address remote-host) ;1 remote address*
		      :operation operation
		      :local-filename local-filename
		      :remote-filename remote-filename
		      :remote-hostname remote-host
		      :mode mode))
	      (SETF *tftp-return* ())
	      (PRINT
		(tftp-start-transfer-message local-filename remote-filename operation mode))
	      (SEND connection :primary-transfer)
	      (FORMAT () "TFTP:  Successful transfer."))
	  (deallocate-tftp-connection connection)))
    (error (FORMAT () "~&TFTP: Aborting transfer due to the following:~%~A~&" condition)))) 

;1;; TFTP *
;1;;*
;1;; TFTP is the user function which initiates a TFTP file transfer.*
;1;; There are three optional parameters, the source file name, the *
;1;; destination file name and the transfer mode.  If the function*
;1;; is initiated without the source and destination file names then*
;1;; the TFTP window function is called.  If no mode is specified then*
;1;; the file is transfered with the default transfer mode.*
;1;;*

(DEFUN tftp (&optional source destination (mode *tftp-default-mode*) &aux v-list)
  "2TFTP is the user function call to initiate a TFTP file transfer.
    Source  -- This is the source file name.
    Destination  -- This is the destination file name.
    Mode  -- This is the transfer mode. The TFTP default mode is used if no mode is specified.*"
  ;1; Setup consing area*
  (LET ((default-cons-area *ip-area*))
    ;1; If no arguments then call the window.*
    (SETF mode (CAR (MEMBER mode *tftp-valid-modes-list* :test 'STRING-EQUAL)))
    (IF (AND source destination mode)
	(tftp-transfer-file source destination mode)
	;1; ELSE*
	(PROGN
	  (SETF v-list (tftp-window source destination))
	  (UNLESS (EQUAL (THIRD v-list) :abort)
	    (tftp-transfer-file (FIRST v-list) (SECOND v-list) (THIRD v-list))))))) 


;1;; rla 9/18/86 - not supported in vm2*
;1;; (GLOBALIZE 'TFTP) *

;1;*
;1; tftp server*
;1;*
(DEFPARAMETER *tftp-server-process* ())


(DEFUN tftp-server (&aux connection)
  (LET ((default-cons-area *ip-area*))
    (UNWIND-PROTECT
	(PROGN
	  (SETF connection
		(allocate-tftp-connection
		  (SEND *udp-handler* :get-port tftp-port) ;1network port*
		  ()			   ;1remote port*
		  ()			   ;1remote address*
		  ))
	  (SEND connection (IF *tftp-debug-mode*
			       :debug-listen-on-tftp-port
			       :listen-on-tftp-port)))
      (WHEN connection (deallocate-tftp-connection connection))
      (SETF *tftp-server-process* nil))))



(DEFUN reset-tftp-service (&optional (enable-p nil) (debug-mode nil))
  (delete-server tftp-port '*udp-server-alist*) 
  (WHEN *tftp-server-process*
    (WITHOUT-INTERRUPTS
      (SEND *tftp-server-process* :kill t)
      (SETF *tftp-server-process* nil)))
  (WHEN enable-p
    (SETF *tftp-debug-mode* debug-mode)
    (add-server tftp-port
		'(SETF *tftp-server-process*
		       (PROCESS-RUN-FUNCTION `(:name ,ip:tftp-process-name :priority 5) 'tftp-server))
		'*udp-server-alist*))
  "TFTP Reset Complete")


;1;; TFTP-SECONDARY*
;1;; Instantiate the connection and complete the file transfer.*

(DEFUN tftp-secondary (pkt pkt-length opcode pkt-filename pkt-mode rem-port rem-addr)
  "2Function that handles a TFTP transfer after the initial request is received by the TFTP server listening to 
    the default TFTP port.*"
  (CONDITION-CASE (condition)
      (LET ((operation (IF (EQUAL opcode tftp-wrq)
			   :write
			   :read))
	    connection)
	(IF *tftp-debug-mode*
	    (tv:notify () "SECONDARY: MODE: ~A OPERATION: ~A" pkt-mode operation))
	(UNWIND-PROTECT (PROGN
			  (SETF connection
				(allocate-tftp-connection (SEND *udp-handler* :get-port)	   ;1port*
							  rem-port ;1Remote port*
							  rem-addr ;1Remote addr*
							  :local-filename pkt-filename	   ;1Local file name*
							  :operation operation
							  :mode pkt-mode	   ;1Mode*
							  :process current-process
							  :network-packet pkt
							  :network-packet-length pkt-length))
			  (SETF *tftp-return* ())
			  (SEND connection :secondary-transfer))
	  (deallocate-tftp-connection connection)))
    ;1;Right now do nothing with the errors.*
    (:terminate
     (WHEN *tftp-debug-mode*
       (tv:notify () "~&TFTP: Aborting transfer due to the following:~%~A~&" condition)))
    (:error
     (WHEN *tftp-debug-mode*
       (tv:notify () "~&TFTP: Aborting transfer due to the following:~%~A~&" condition))))) 


;1;; TFTP-GET-STRING*
;1;; *
;1;; Extract a string from a TFTP packet.*
;1;; Nill if no string.*
;1;;*

(DEFUN tftp-get-string (pkt &optional (start 0) (delimiter tftp-string-delimiter))
  "2This extracts a string from an 8bit array given a starting point and a string delimiter.*"
  (LET* ((pkt-length (ARRAY-TOTAL-SIZE pkt))
	 (pkt-string (MAKE-STRING (- pkt-length start)))
	 (STRING-LENGTH
	   (LOOP for i from start below pkt-length when (EQUAL (AREF pkt i) delimiter) return
		 (- i start) do
					   ;1(TRANSL::WARNING*
					   ;1 " The arguments have been swapped during the translation,*
;1make sure it can be done."*
		 (SETF (AREF pkt-string (- i start)) (COERCE (AREF pkt i) 'CHARACTER)))))
    (SUBSEQ pkt-string 0 string-length)))  ;1RLA 9/18/86*

;1;; TFTP-PROCESS-ERROR-PACKET*
;1;;*
;1;; Create an error message for human consumption from the error text in the packet.*
;1;; If the packet hasn't any error text, use the packet error code to derive the*
;1;; correct default error message.  Return the error message.*
;1;;*

(DEFUN tftp-process-error-packet (pkt host)
  "2Return formatted error string from the error packet or a default error string if the packet has no error text.*"
  (LET ((error-code (tftp-extract-error-code pkt))
	(error-text (tftp-get-string pkt tftp-data-offset)))
    ;1; Use the packets message when available*
    ;1; a default error string otherwise.*
    (FORMAT () "     Error message from ~A:~%     ~A~%" host
	    (IF error-text
		error-text
		(tftp-default-error-string error-code))))) 


;1;; TFTP-GET-LOCAL-FILE-NAME*
;1;;*
;1;; Returns the filename string in a TFTP request packet.*
;1;;*

(DEFUN tftp-get-local-file-name (pkt)
  "2Returns the filename string in a TFTP request packet.*"
  (LET ((name (tftp-get-string pkt tftp-request-header-size))
	(default-host (tftp-get-host tftp-default-pathname)))
    ;1; If the local machine has no file system mounted*
    ;1; and the default pathname specifies no host or the*
    ;1; local file system, use the associated machine as the host. *
    ;1; Otherwise, use the default pathname.*
    (MERGE-PATHNAMES name
		     (IF (AND (NOT (VARIABLE-BOUNDP fs::disk-configuration))
			      (OR (STRING-EQUAL default-host "LM")
				  (EQ (si:parse-host default-host) si:local-host)))
			 (MAKE-PATHNAME :host (SEND si:associated-machine :name) :directory "SITE"
					:name "TFTP-LOST-FILE" :type "LISP")
			 tftp-default-pathname)))) 


;1;; TFTP-GET-MODE*
;1;;*
;1;; Returns the mode string in a TFTP request packet.*
;1;; Nil if no mode.*
;1;;*

(DEFUN tftp-get-mode (pkt)
  "2Pulls out and returns the mode for this TFTP access.*"
  (LET* ((offset
	   (LOOP for i from 2 to (ARRAY-TOTAL-SIZE pkt) when
		 (= (AREF pkt i) tftp-string-delimiter) return (1+ i)))
	 ;1;Offset to mode.*
	 (mode (tftp-get-string pkt offset)))
    (IF mode
	(SETF mode (host::intern-as-keyword mode))))) 


;1;; READ-DATA-FROM-STREAM*
;1;;*
;1;; Read data from the local file.*
;1;; Return the packet length.*
;1;;*

(DEFUN tftp-read-data-from-stream (pkt &aux ch size)
  "2Read data from the local file stream. *"
  (DECLARE (:self-flavor tftp-connection))
  (SELECT mode
    (:netascii
     ;1; We have split the newline sequence across two packets*
     ;1; make the first character a NL*
     (LET ((start tftp-data-offset)
	   (end (+ tftp-data-offset tftp-maximum-data-size)))
       (WHEN *tftp-return*
	 (SETF (AREF pkt start) 10)
	 (INCF start)
	 (SETF *tftp-return* ()))
       (LOOP for index = start
	     then (INCF index) when
	     (OR (>= index end) (NULL (SEND stream :tyipeek)))
					   ;1 Quit at end of file or when reach maximum data size*
	     return index		   ;1 Return packet length!*
	     do
	     (CASE (SETQ ch (SEND stream :tyi))
	       (136 (SETF (AREF pkt index) 8))	   ;1OVERSTRIKE*
	       (137 (SETF (AREF pkt index) 9))	   ;1TAB*
	       (138 (SETF (AREF pkt index) 10))	   ;1LINE*
	       (140 (SETF (AREF pkt index) 12))	   ;1FF*
	       (141
		(IF (= (1+ index) end)
					   ;1 newline sequence is split across packets*
		    (AND (SETF *tftp-return* t) (SETF (AREF pkt index) 13))
		    ;1; ELSE*
		    (PROGN
		      (SETF (AREF pkt index) 13)
		      (INCF index)
		      (SETF (AREF pkt index) 10))))	   ;1RETURN*
	       (135 (SETF (AREF pkt index) 177))   ;1RUBOUT*
	       (t (SETF (AREF pkt index) ch))))))
    (:octet (SEND stream :string-in () pkt tftp-data-offset))
    (t (SETF size (SEND stream :string-in () xfasl-packet (/ tftp-data-offset 2))) (* 2 size)))) 
    
    
;1;; TFTP-WRITE-DATA-TO-STREAM*
;1;; *
;1;; Write data to the local file.*
;1;; If the data is of maximum data size, return :CONTINUE*
;1;; Otherwise, return :DONE.*
;1;;*

(DEFUN tftp-write-data-to-stream (pkt pkt-length)
  "2Write the data to the local file stream.*"
  (DECLARE (:self-flavor tftp-connection))
  (IF (OR (< pkt-length tftp-data-header-size)
	  (> pkt-length (+ tftp-data-header-size tftp-maximum-data-size)))
      (FERROR :terminate "~&TFTP-WRITE-DATA-TO-STREAM: ~A~&"
	      (tftp-bad-packet-size-message pkt-length)))
  (IF (> pkt-length tftp-data-header-size)
      (SELECT mode
	(:netascii
	 ;1; If the return sequence is split accross the packet boundaries, *
	 ;1; *TFTP-RETURN* is set to t and the first character in the packet*
	 ;1; should be a line feed or a null. Throw it away. *
	 (LET ((data-start
		 (IF (AND *tftp-return*
			  (OR (SETF *tftp-return* ()) (SYS:%STRING-SEARCH-CHAR 0 pkt 0 1)
			      (SYS:%STRING-SEARCH-CHAR 10 pkt 0 1)))
		     (1+ tftp-data-offset)
		     tftp-data-offset))
	       r-list lisp-ch)
	   ;1; translate special ascii characters to lisp machine characters*
	   (DOLIST (ch translation-list)
	     (SETF lisp-ch (+ 128 ch))
	     (LOOP with index = 0 do (SETF index (SYS:%STRING-SEARCH-CHAR ch pkt index pkt-length))
		   until (NULL index) do (SETF (AREF pkt index) lisp-ch)))
	   ;1; translate ascii return to lisp RETURN*
	   ;1; and record all the locations of the RETURN*
	   (LOOP with index = 0 do (SETF index (SYS:%STRING-SEARCH-CHAR 13 pkt index pkt-length))
		 until (NULL index) do (SETF (AREF pkt index) 141) do
		 (IF (< index (1- pkt-length))
		     (WHEN (OR (SYS:%STRING-SEARCH-CHAR 128 pkt index (+ 2 index))
			       (SYS:%STRING-SEARCH-CHAR 138 pkt index (+ 2 index)))
		       (PUSH (1+ index) r-list))
		     ;1; the return sequence is split between to buffers*
		     ;1; special case.*
		     (SETF *tftp-return* t))
		 finally (SETF r-list (NREVERSE r-list)))
	   ;1; Write the pkt to the stream*
	   (DO* ((from data-start (INCF to))
		 (to (POP r-list) (POP r-list)))
		((NULL to)
		 (IF (< from pkt-length)
		     (SEND stream :string-out pkt from pkt-length)))
	     (SEND stream :string-out pkt from to))))
	(:octet (SEND stream :string-out pkt tftp-data-header-size pkt-length))
	(t
	 ;1;;XFASL FILES*
	 (LET ((end
		 (IF (ODDP pkt-length)
		     (PROGN
		       (SETF (AREF pkt pkt-length) 0)
		       (/ (1+ pkt-length) 2))
		     (/ pkt-length 2))))
	   (SEND stream :string-out xfasl-packet (/ tftp-data-header-size 2) end)))))
  ;1; If data is the maximum data size then we expect another data packet.*
  ;1; If the data length is less than the maximum, then we are done.*
  (IF (EQL pkt-length tftp-maximum-packet-size)
      :continue				   ;1 We expect to receive another packet.*
      :done)				   ;1 We are done transfering the file.*
  ) 
