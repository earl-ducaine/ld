;;; -*- Mode:COMMON-LISP; Package: FILE-SYSTEM; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  Base:10 -*-

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
;;; 12-14-88 DAB   The function fs:login-host-unit fails to "guess" the password of Symbolics file servers
;;;                correctly when the server is using Symbolics' simple file protection scheme.  Should try
;;;                using the empty string before prompting user. [6828]
;;; **************** Rel 5.0 OS. 13-12-88 DAB
;;; 09-14-88 DAB - Changes open-chaos to OPEN-FOR-LISPM when host is an Explorer or a microExplorer.
;;; 08-10-88 DAB - Changed directory-chaos to ignore the propety :short. This was added for MX.
;;; 07-13-88 DAB - Added a new function: notify-nicely. this function will notify the user of message
;;;                but will not lock up the system.
;;; 04-06-88 DAB - Fixes the error "failed on all implementation ...".
;;;                The problem happen because a error object is added to the property list and is not
;;;                remove until some timeout value occurs. The patch will force this value to be cleared.
;;;   25 MAR 86 MMG - Conversion to Common Lisp.
1;;;      JUL 86 BJ  - Conversion for GENI
;;;   16 OCT 86 RLA - changed direct accesses to USER-UNAMES & USER-HOST-PASSWORD-ALIST*
;;;    10 OCT 86  BJ  - Moved stream code to file QFILE-STREAMS.
;;;   09-10-87  DAB  Added code to support (OPEN "...." :Direction :IO) in QFILE.
;;;   11-16-87  DAB - Fixed 4read-file-property-list-string* to bind *package* to *system-package*.
;;;                   Read-from-string would fail if LISP:NIL if *system-package* is not binded. [6823]
;;;   11-16-87 DAB  - Fixed decode-element-type to intern element-type to the local package so CASE will
;;;                   work. Also changes CHARACTER type to return T and 8.
;;;   12-01-87 DAB    Move Decode-element-type from io; open.
;;;   12-10-87 DAB  - Fixed complete-chaos to return NIL not :NIL if unsuccessful.

1;Disembodied plists whose property names are host objects
;and whose values are pathnames connected to or accessed to.
;Used so that when a new host unit connection is made
;it can be told to connect or access as specified.*

(proclaim '(special *qfile-service*))

(defvar 4chaos-host-connect-plist* (cons () ())) 
(defvar 4chaos-host-access-plist* (cons () ())) 
(defvar 4chaos-host-capabilities-plist* (cons () ())) 

(defun 4clear-connect-and-access-memory* ()
  2"Clear the records of what remote access and connects have been done to what hosts.*
   2Also clears out records of enabled capabilities."*
  (setq chaos-host-connect-plist (cons () ()))
  (setq chaos-host-access-plist (cons () ()))
  (setq chaos-host-capabilities-plist (cons () ()))) 

1;;; One HOST-UNIT is associated with each control connection*

(defflavor 4host-unit*
	   (host				1;Host object*
	    (control-connection nil)		1;Control connection for this host*
	    (data-connections nil)		1;List of DATA-CONNECTION's*
	    (max-data-connections 37777777)	1;Maximum number of data connections*
	    (lock nil)				1;Lock to insure no timing screws*
	    (last-use-time (time)))
	   ()
  :ordered-instance-variables
  (:gettable-instance-variables control-connection)
  :outside-accessible-instance-variables
  (:initable-instance-variables host)) 

1;;; A DATA-CONNECTION is associated with each data connection.
;;; The two directions in the connection itself are used independently.*

(defstruct 4(data-connection (:conc-name data-*)
			    (:alterant nil)
			    (:constructor make-data-connection (connection input-handle output-handle))
			    (:callable-constructors nil)
			    (:predicate nil)
			    (:copier nil)
			    (:type :list*))
  connection1;The chaos connection*
  input-handle
  output-handle
  (last-use-time (time))
  (stream-list (list :input () :output ()))) 


1;; Max-data-connections now defaults. Lets see what kind of error happens.
;;(DEFMETHOD (HOST-UNIT :INIT) (IGNORE)
;;  (SETQ MAX-DATA-CONNECTIONS (SEND HOST :MAX-DATA-CONNECTIONS))) 

;;; Lock a host unit around BODY*
(defmacro 4lock-host-unit* ((host-unit) &body body)
  (let ((lock (gensym))
	(locked-p (gensym)))
    `(let ((,lock (locf (host-unit-lock ,host-unit)))
	   (,locked-p nil))
       (unwind-protect
	   (progn
	     (cond ((neq (car ,lock) current-process)
		    (process-lock ,lock)
		    (setq ,locked-p t)))
	     ,@body)
	 (setq last-use-time (time))
	 (and ,locked-p (process-unlock ,lock)))))) 

(defmethod 4(host-unit :reset*) (&optional dont-unlock-lock-p)
  
  (when control-connection
    (chaos:remove-conn control-connection)
    (setq control-connection ()))

  (dolist (data-conn data-connections)    
    (do ((list (data-stream-list data-conn) (cddr list))
	 (stream))
	((null list))
      (unless (symbolp (setq stream (cadr list)))
	      (send tv:who-line-file-state-sheet :delete-stream stream)
	      (setf (send stream :status) :closed)))
    (chaos:remove-conn (data-connection data-conn)))
  
  (setq data-connections ())
  (unless dont-unlock-lock-p
    (setq lock ()))) 

(defparameter 4*file-contact-name** 2"FILE 1"* "Contact name and protocol version number") 
(defparameter 4*file-control-window-size** 810) 
(defvar 4*host-file-contact-name-alist** () 2"Alist of host primary names and the contact name for the file."*) 

1;;; Check that connection hasn't gone away, making a new one if necessary*

(defmethod 4(host-unit :validate-control-connection*) (&optional no-error-p)
  (condition-call (cond)
      (lock-host-unit (self)
	(cond ((and control-connection
		    (eq (chaos:state control-connection) 'chaos:open-state)
		    (loop for data-conn in data-connections always
			  (eq (chaos:state (data-connection data-conn)) 'chaos:open-state)))
	       t)
	      (t
	       (send self :reset t)	   1;Arg of T means don't unlock lock*

	       (setq control-connection
		     (host:open-connection-on-medium
		       host :chaos 3"file"*
		       :window-size *file-control-window-size*))
	       
	       (setf (chaos:interrupt-function control-connection)
		     (let-closed ((host-unit self)) 'host-chaos-interrupt-function))
	       
	       (send *qfile-service* :login-unit self t)
	       
	       t)
	      ))
    ((condition-typep cond 'sys:network-error)
     (unless no-error-p
       (signal cond)))))

1;;; Transaction management*

(defstruct 4(file-transaction-id*
	     4(:alterant nil*)
	     (:constructor make-file-transaction-id-internal (id simple-p))
	     (:conc-name file-transaction-id-)
	     (:callable-constructors nil)
	     (:predicate nil)
	     (:copier nil)
	     (:type :list))
  id
  simple-p
  (pkt nil)) 


(defvar 4*file-unique-number** 259 "Number user by file-gensym.") 

(defun 4file-gensym* (leader)
  2"Create a string which is LEADER followed by a unique number (printed out)."*
  (let ((number
	 (without-interrupts
	  (setq *file-unique-number* (rem (1+ *file-unique-number*) 10000)))))
    (string-append leader (+ #\0 (rem (truncate number 1000) 10))
		   (+ #\0 (rem (truncate number 100) 10))
		   (+ #\0 (rem (truncate number 10) 10)) (+ #\0 (rem number 10))))) 


(defvar 4*file-pending-transactions** ())
(defun 4file-make-transaction-id* (&optional (simple-p nil) &aux id (default-cons-area background-cons-area))
  2"Return a new transaction ID string, and perhaps record a pending transaction.*
   2If SIMPLE-P is not non-NIL, a pending transaction is recorded forthis transaction ID."*
  (without-interrupts
    (setq id (file-gensym 3"T"*))
    (push (make-file-transaction-id-internal id simple-p) *file-pending-transactions*))
  id) 

;; Try 1 min for now.
(defparameter *Qfile-transaction-wait-time* (* 60 60 60 1)
  "The wait time for a qfile file transaction to be completed.")

(defun 4file-wait-for-transaction* (tid &optional conn (whostate 2"File Transaction"*) &aux id)
  2"Wait for completion of transaction with id TID, which should be on the pending list.
CONN is the connection the reply should arrive on (the control connection).
WHOSTATE is what to say in the who-line while waiting. This will time-out if the transaction
is not comleted in 20. minutes."*
  (if (null (setq id (assoc tid *file-pending-transactions* :test #'equal)))
    (ferror () 3"Transaction ID ~A not found on pending list"* tid)
    (progn
      (when (null
	(process-wait-with-timeout whostate *Qfile-transaction-wait-time*
				   #'(lambda (id conn) (or (file-transaction-id-pkt id)
							   (neq (chaos:state conn) 'chaos:open-state)))
				   id conn))
	(setf (chaos:state conn) 'chaos:host-down-state)
	(chaos:report-bad-connection-state conn (format () 3"process a ~a command from"* whostate))
	(chaos:remove-conn conn))
      (cond ((neq (chaos:state conn) 'chaos:open-state)
	     1;; Get an error of the appropriate sort.*
	     (chaos:report-bad-connection-state conn 3"read file command reply from"*))
	    (t
	      (without-interrupts
		(setq *file-pending-transactions*
		      (delete id (the list *file-pending-transactions*) :test #'eq))
		(file-transaction-id-pkt id))))))) 

1;This is the interrupt function we put into the control connection*


(defun 4host-chaos-interrupt-function* (reason conn &rest ignore)
  (declare (special host-unit))
  (case reason
    (:input
     (do ((pkt (chaos:get-next-pkt conn t) (chaos:get-next-pkt conn t))
	  (string)
	  (tem))
	 ((null pkt))
       (setq string (chaos:pkt-string pkt))
       (select (chaos:pkt-opcode pkt)
	  (%file-asynchronous-mark-opcode
	   (setq string
		 (nsubstring string
			     (1+
			      (position #\Space (the string (string (chaos:pkt-string pkt)))
					:test #'char-equal))))
	   (do ((data-conns (host-unit-data-connections host-unit) (cdr data-conns))
		(handle-len
		 (or (position #\Space (the string (string string)) :test #'char-equal)
		    (length string)))
		(stream))
	       ((null data-conns)
		(chaos:return-pkt pkt))
	     (cond
	       ((string-equal string (data-handle (car data-conns) :output) :END1 handle-len)
		(setq stream (data-stream (car data-conns) :output))
		(send stream :async-mark pkt) (return ())))))
	  (%file-command-opcode
	   (setq string
		 (subseq (string string) 0 (position #\Space (the string (string string)) :test #'char-equal)) )
	   (setq tem (assoc string *file-pending-transactions* :test #'equal))
	   (return-array (prog1
			   string
			   (setq string ())))1;Don't cons*
	   (cond
	     ((null tem)
	      (process-run-function 3"Unknown file-system-command"*
				    #'(lambda (pkt)
					(unwind-protect (ferror
							 ()
							 3"File system error, unknown transaction id in ~S"*
							 (chaos:pkt-string pkt))
					  (chaos:return-pkt pkt)))
				    pkt))
	     ((file-transaction-id-simple-p tem)
	      1;;If simple transaction, make sure no error*
	      (let ((string
		     (nsubstring (chaos:pkt-string pkt)
				 (1+
				  (position #\Space (the string (string (chaos:pkt-string pkt)))
					    :test #'char-equal))))
		    (from))
		(setq from (1+ (string-search-set '(#\Space #\Newline) string)))
		1;; If simple transaction fails, display error in another process*
		(or
		 (not
		  (string-equal 3"ERROR"* string :start2 from :end1 5 :end2
				(string-search-set '(#\Space #\Newline) string from)))
		 (process-run-function 3"File System Transaction Error"* #'file-process-error-new
				       (prog1
					 (string-append string)
					 (chaos:return-pkt pkt)))))
	      (setq *file-pending-transactions*
		    (delete tem (the list *file-pending-transactions*) :test #'eq)))
	     (t (setf (file-transaction-id-pkt tem) pkt))))
	  (%file-notification-opcode
	   (tv:notify () 3"File server ~A: ~A"* (host-unit-host host-unit) string)
	   (chaos:return-pkt pkt))
	  (otherwise (chaos:return-pkt pkt))))))) 

 
1;;; Get a data connection for this unit.  Makes a new one if there is room in within the
;;; maximum number.  We are assumed to have recently been checked for validity.*

(defmethod (host-unit :get-data-connection) (direction)
  (lock-host-unit (self)
    (do ((data-conns data-connections (cdr data-conns))
	 (data-conn))
	(nil)
      (setq data-conn
	    (cond
	      (data-conns (car data-conns))
	      ((= (length data-connections) max-data-connections) (return ()))
	      (t (send self :new-data-connection))))
      (if (eq direction :io)			; 09-02-87 DAB IO Support
	  (cond ((and (null (data-stream data-conn :input))  ;need a connection with both data channels available
		      (null (data-stream data-conn :output)))
		 (setf (data-stream data-conn :input) t)	;Mark as allocated
		 (setf (data-stream data-conn :output) t)
		 (return data-conn)))
	  (cond ((null (data-stream data-conn direction))
		 (setf (data-stream data-conn direction) t)	;Mark as allocated
		 (return data-conn))))))) 

1;;; Called when done with a DATA-CONNECTION for DIRECTION.
;;; If free in both directions for long enough, it is flushed for being dormant.*

(defmethod 4(host-unit :free-data-connection*) (data-connection direction)
  (setf (data-stream data-connection direction) ())
  (setf (data-last-use-time data-connection) (time))
  (setq last-use-time (time))) 


(defvar 4*file-data-window-size** 13 2"Window size used on file data connections."*) 

1;;; Allocate a new data connection*

(defmethod 4(host-unit :new-data-connection*) ()
  (let* ((default-cons-area background-cons-area)
	 (input-handle (file-gensym 3"I"*))
	 (output-handle (file-gensym 3"O"*))
	 connection
	 success)	1;T => don't remove-conn the connection.*
    (unwind-protect
	(prog nil
	   retry
	      (setq connection (chaos:listen output-handle *file-data-window-size* ()))
	      (let ((pkt (chaos:get-pkt))
		    (id (file-make-transaction-id))
		    (data-conn))
		(chaos:set-pkt-string pkt id 3"  DATA-CONNECTION "* input-handle 3" "*
				      output-handle)
		(chaos:send-pkt control-connection pkt)
		(unless (chaos:wait connection 'chaos:listening-state (* 60 30)
				    3"File Data Connection"*)
			1;; Attempt to establish connection timed out -- give reasonable error*
			(cerror :retry-file-operation () 'network-lossage
				3"Attempt to establish file data connection timed out."*)
			1;; It lost; tell the connection we gave up,*
			(chaos::close-conn connection)
			1;; wait for the server to report the failure on our side,*
			1;; or say why it failed, or something.*
			(chaos:return-pkt
			  (file-wait-for-transaction id control-connection
						     3"File Data Connection"*))
			1;; then try again*
			(go retry))
		(chaos:accept connection)
		(setq pkt (file-wait-for-transaction id control-connection 3"File Data Connection"*))
		(unwind-protect
		    (let ((string (chaos:pkt-string pkt)))
		      (setq string (nsubstring string
					       (1+ (position #\Space (the string (string string)) :test #'char-equal))))
		      (cond ((file-check-command 3"DATA-CONNECTION"* string t)
			     (setq data-conn
				   (make-data-connection connection input-handle
							 output-handle))
			     (push data-conn data-connections))
			    (t (file-process-error-new string))))	1;not proceedable*
		  (chaos:return-pkt pkt))
		(setq success t)
		(return data-conn)))
      1;; If we are not putting CONNECTION into the host unit, free it.*
      (unless success
	      (when connection
		    (send self :command () input-handle t 3"UNDATA-CONNECTION"*)
		    (chaos::close-conn connection 3"Aborted"*)
		    (chaos:remove-conn connection))))))

1;;; Send a command over the control connection.
;;; MARK-P means writing or reading (expecting) a synchronous mark.
;;; STREAM-OR-HANDLE is a stream whose file handle should be used, or the handle itself.
;;;  if MARK-P, this had better really be a stream.
;;; SIMPLE-P means do not wait for a response, get an asynchronous error if any.*

(defmethod 4(host-unit :command*) (mark-p stream-or-handle simple-p &rest commands &aux handle stream)
  (declare (values pkt success string))
  (when control-connection
    (setq last-use-time (time))
    (cond
      ((stringp stream-or-handle) (setq handle stream-or-handle))
      (stream-or-handle (setq stream stream-or-handle
			      handle (send stream :file-handle))
			(and mark-p (setq mark-p (send stream :direction)))))
    (let ((pkt (chaos:get-pkt)) (transaction-id (file-make-transaction-id simple-p)) success
	  whostate string)
      1;; Make up a packet containing the command to be sent over*
      (apply #'chaos:set-pkt-string pkt transaction-id 3" "* (or handle 3""*) 3" "* commands)
      (let ((string (chaos:pkt-string pkt))
	    (from 0))
	(setq from
	      (position #\Space (the string (string string)) :start
			(1+ (position #\Space (the string (string string)) :test #'char-equal))
			:test #'char-equal))
	(setq whostate
	      (subseq (string string) (1+ from) (string-search-set '(#\Space #\Newline) string (1+ from))) )
	1;; Make the whostate look pretty.*
	(do ((i 1 (1+ i))
	     (len (length whostate)))
	    ((= i len))
	  (setf (aref whostate i) (char-downcase (aref whostate i)))))
      (chaos:send-pkt control-connection pkt %file-command-opcode)
      (and (eq mark-p :output) (send stream :write-synchronous-mark))
      1;; Get the portion of the response after the transaction ID.*
      (cond
	(simple-p (and (eq mark-p :input) (send stream :read-until-synchronous-mark))
		  (values () t 3""*))
	((setq pkt (file-wait-for-transaction transaction-id control-connection whostate))
	 (setq string
	       (nsubstring (chaos:pkt-string pkt)
			   (1+
			     (position #\Space (the string (string (chaos:pkt-string pkt))) :test
				       #'char-equal))))
	 (setq success
	       (let ((from
		       (if handle
			   (file-check-handle handle string)
			   (1+ (string-search-set '(#\Space #\Newline) string)))))
		 (not
		   (string-equal 3"ERROR"* string :start2 from :end1 5 :end2
				 (string-search-set '(#\Space #\Newline) string from)))))
	 (and success (eq mark-p :input) (send stream :read-until-synchronous-mark))
	 (values pkt success string))))))


(defun 4file-check-handle* (handle string)
  2"Validate a reply STRING received supposedly for file HANDLE.*
   2Gets an error if the string does not start with HANDLE.*
   2If it does, returns the index of the first character in STRING after the handle."*
  (let ((handle-end (string-search-set '(#\Space #\Newline) string)))
    (and (null handle-end)
       (ferror () 3"Response over control connection was incorrectly formatted"*))
    (or (string-equal string handle :END1 handle-end)
       (ferror () 3"Response over control connection was for wrong file handle"*))
    (1+ handle-end)))


(DEFUN mac-host-p (host)	;; TUNG
  (AND (EQ (SEND host :system-type) :unix-ucb)
       (EQ (SEND host :pathname-flavor) 'fs:mac-pathname)) )

(defun 4login-host-unit* (unit login-p uname-host)
  2"Log the host unit UNIT in or out.  LOGIN-P = NIL means log out, otherwise log in.
Note that logging in must be done on each host unit before it can be used,
whether or not this is the host that the user actually specified when
he said \"log in\".  UNAME-HOST should be the host that the user actually logged in on."*
  (declare (special chaos:give-finger-saved-user-id))
  (let* ((host (host-unit-host unit))
	 (conn (host-unit-control-connection unit))
	 success
	 pkt
	 enable-capabilities
	 (default-cons-area background-cons-area))
    
    1;;first thing we should do is check to see *
    1;;if the connection is in a valid state, and then logout.*
    (if (and (null login-p)
	     conn
	     (eq (chaos:state conn) 'chaos:open-state))
	(setq conn (chaos::close-conn conn 3"I'm loggin out."*)))
    
    1;;nil*
    (when (and conn
	       (eq (chaos:state conn) 'chaos:open-state))
	  (unwind-protect
	      (do ((id (file-make-transaction-id) (file-make-transaction-id))
		   (password 3""*)
		   (account 3""*)
		   (need-password nil)
		   new-user-id)
		  (success)
		
		(setq pkt (chaos:get-pkt))
		1;; Only hack user name or password if logging in, not out.*
		(cond (login-p
			(setq new-user-id (uname-on-host uname-host (eq uname-host 'its)))	1;*RLA**
			(cond ((eq uname-host 'its)
			       (cond ((null new-user-id)
				      1;; This is an ITS; ask for the user name for all ITSes.*
				      (format *query-io* 3"~&ITS uname (default ~A): "* user-id)
				      (let ((nid (readline-trim *query-io*)))
					(setq new-user-id (if (equal nid 3""*)
							      user-id
							      nid))))))
			      1;; Not an ITS: if we don't know user id or if password failed,*
			      1;; ask for one or both.*
			      ((or need-password
				   (null new-user-id))
			       (if (mac-host-p host)	;; Tung
				   (SETQ password "MX")
				   (multiple-value-setq (new-user-id password enable-capabilities)
				     (file-get-password user-id uname-host))) )
			      1;; We know the user id; use remembered password if any.*
			      ((equal password 3""*)
			       (setq password
				     (or
				       (first (lookup-password-etc new-user-id host))	1;*RLA**
				       
				       1;; None remembered => guess, except on Multics*
				       1;; since multics would hassle the guy if it's wrong.*
				       (if (eq (send host :system-type) :multics)
					   3""*)

				       (if (EQ (SEND host :system-type) :lmfs) ;12-14-88 DAB [6828]
					   "")
				       
				       1;; Try guessing password same as on some other host.*
				       (first-password)	1; *RLA**
				       
				       1;; Try guessing password same as uname or last part of it.*
				       (prog1
					 (setq password
					       (subseq (string new-user-id)
						       (1+
							 (or
							   (search (the string (string #\.))
								   (the string (string new-user-id)) :from-end t :test
								   #'char-equal)
							   -1))))
					 (store-password-etc new-user-id host password)	1;*RLA**
					 )))))
			(or new-user-id (setq new-user-id user-id))
			(file-host-user-id new-user-id host)))
		1;; If the connection got closed while we waited for input, reconnect.*
		(cond ((not (eq (chaos:state conn) 'chaos:open-state))
		       (chaos::close-conn conn)
		       
		       (let ((conn1 (host:open-connection-on-medium
				      host :chaos 3"file"* :window-size *file-control-window-size*)))
			 (cond ((errorp conn1)
				(ferror 'file-server-connect-error
					3"Error while trying to open connection ~a"*
					(send conn1 :report-string)))
			       (t
				 (setf (host-unit-control-connection unit) conn1)
				 (setq conn conn1)
				 (setf (chaos:interrupt-function conn)
				       (let-closed ((host-unit unit))
						   'host-chaos-interrupt-function)))))))
		1;; Send the login command.*
		(chaos:set-pkt-string pkt id 3"  LOGIN "* (or new-user-id 3""*) 3" "* password 3" "* account)
		(chaos:send-pkt conn pkt)
		
		1;; Avoid doing RETURN-PKT on a PKT that has been returned already by SEND-PKT.*
		(setq pkt ())
		(setq pkt (file-wait-for-transaction id conn 3"Login"*))
		(if login-p
		    (let ((str (chaos:pkt-string pkt))
			  idx
			  hsname-pathname
			  personal-name
			  group
			  personal-name-1
			  item)
		      (setq str
			    (nsubstring str
					(1+
					  (position #\Space (the string (string str)) :test
						    #'char-equal))))
		      (setq idx (file-check-command 3"LOGIN"* str t))
		      (cond (idx
			      (setq idx
				    (position #\Space (the string (string str)) :start idx :test
					      #'char-equal))
			      (multiple-value-setq (hsname-pathname personal-name group personal-name-1)
						   (send *qfile-service* :hsname-information
							 unit str idx))
			      1;; Record info about this user*
			      1;; only if host login name equals name given to LOGIN.*
			      (and (equal user-id new-user-id)
				   (equal user-personal-name 3""*)
				   (setq user-personal-name personal-name
					 user-group-affiliation group
					 user-personal-name-first-name-first personal-name-1))
			      (setq chaos:give-finger-saved-user-id t)	1;Clear cache*
			      
			      1;; If this is the user's login host*
			      1;; but the host user id is not the one specified in LOGIN,*
			      1;; do not accept the file server's suggested home dir*
			      1;; since it is based on the file server login id.*
			      (and (eq host user-login-machine)
				   (not (equal user-id new-user-id))
				   (setq hsname-pathname (quiet-user-homedir host)))
			      
			      1;; Record homedir for this host.*
			      (if (setq item (assoc host user-homedirs :test #'eq))
				  (rplacd item hsname-pathname)
				  (push (cons host hsname-pathname) user-homedirs))
			      
			      1;; If we have done remote connect or access on this host,*
			      1;; tell the new file server about it.*
			      (if (get chaos-host-connect-plist host)
				  (cwd-chaos host (get chaos-host-connect-plist host) t () unit))
			      (if (get chaos-host-access-plist host)
				  (cwd-chaos host (get chaos-host-access-plist host) t t unit))
			      (if enable-capabilities
				  (send host :enable-capabilities)
				  (if (get chaos-host-capabilities-plist host)
				      (change-capabilities-chaos host
								 (get chaos-host-capabilities-plist
								      host)
								 t unit)))
			      (setq success t))
			    1;; If user or password is invalid, force getting it (again).*
			    (t
			      (condition-case ()
				  (file-process-error-new str)
				(login-problems
				  1;; Since this password is wrong, flush it from list of remembered ones.*
				  (delete-password-etc new-user-id host)	1;*RLA**
				  (setq need-password t))))))
		    (setq success t))
		(chaos:return-pkt pkt)
		(setq pkt ()))
	    (cond ((not success)
		   (when pkt
			 (chaos:return-pkt pkt))
		   (chaos::close-conn conn 3"Login failed"*)))))
    t))


 
1;;; Functions to be called by pathname interface.
;;; Commands without associated streams.*

(defun 4delete-chaos* (host pathname error-p)
  (let (host-unit
	pkt
	success
	string)
    (file-operation-retry
      (setq host-unit (send *qfile-service* :get-host-unit host))
      (multiple-value-setq (pkt success string)
			   (send host-unit :command () () () 3"DELETE"* #\Newline (file-print-pathname pathname)
				    #\Newline))
      (unwind-protect
	  (or success
	      (file-process-error-new string pathname () (not error-p) :delete))
	(chaos:return-pkt pkt))))) 


(defun 4delete-and-expunge-chaos* (host pathname error-p)

  (cond ((send pathname :undeletable-p)
	 (let (host-unit
	       pkt
	       success
	       string)
	   (file-operation-retry
	     (setq host-unit (send *qfile-service* :get-host-unit host))
	     (multiple-value-setq (pkt success string)
	       (send host-unit :command () () () 3"DELETE-AND-EXPUNGE"* #\Newline
		     (file-print-pathname pathname) #\Newline))
	     (unwind-protect (cond
			       (success
				(let ((start (file-check-command 3":DELETE-AND-EXPUNGE"* string)))
				  (parse-integer string :start start :junk-allowed t)))
			       (t
				(let ((return-value (delete-chaos host pathname error-p)))
				  (if (errorp return-value)
				      return-value
				      (directory-operation-chaos :expunge host pathname error-p)))))
	       (chaos:return-pkt pkt)))))
	(t
	 (delete-chaos host pathname error-p))))

(defun 4rename-chaos* (host old-pathname new-pathname error-p)
  (let (host-unit
	pkt
	success
	string)
    (file-operation-retry
      (setq host-unit (send *qfile-service* :get-host-unit host))
      (multiple-value-setq (pkt success string)
			   (send host-unit :command () () () 3"RENAME"* #\Newline
				    (file-print-pathname old-pathname) #\Newline (file-print-pathname new-pathname)
				    #\Newline))
      (unwind-protect (if
			success
			1;; If there is a second line coming from the file server,*
			1;; it is the new truename.*
			(let* ((from
				 (search (the string (string #\Newline))
					 (the string (string string)) :test #'char-equal))
			       truename-string)
			  (cond
			    (from
			     (setq truename-string
				   (subseq (string string) (1+ from)
					   (search (the string (string #\Newline))
						   (the string (string string)) :start2 (1+ from)
						   :test #'char-equal)) )
			     (parse-pathname truename-string (send old-pathname :host)))
			    (t (truename new-pathname))))
			(file-process-error-new string old-pathname () (not error-p) :rename))
	(chaos:return-pkt pkt))))) 


(defun 4complete-chaos* (host pathname string options)
  (let (host-unit
	pkt
	file-string
	success
	deleted-p
	write-p
	new-ok
	string-origin
	(default-cons-area background-cons-area))
    (dolist (key options)
      (case key
	    (:deleted (setq deleted-p t))
	    ((:read :in) (setq write-p ()))
	    ((:print :out :write) (setq write-p t))
	    (:old (setq new-ok ()))
	    (:new-ok (setq new-ok t))
	    (otherwise (ferror () 3"~S is not a recognized option."* key :complete-string))))
    (setq host-unit (send *qfile-service* :get-host-unit host))
    (multiple-value-setq (pkt success file-string)
			 (send host-unit :command () () ()
				  (format () 3"COMPLETE~:[ DELETED~]~:[ WRITE~]~:[ NEW-OK~]~%~A~%~A~%"* (not deleted-p)
					  (not write-p) (not new-ok) (file-print-pathname pathname) string)))
    (cond
      (success
       (or
	 (setq string-origin
	       (position #\Newline (the string (string file-string)) :test #'char-equal))
	 (ferror () 3"Illegally formatted string ~S from file server."* file-string))
       (setq success
	     (pkg-bind pkg-keyword-package
		       (read-from-string file-string () () :start
					 (file-check-command 3"COMPLETE"* file-string))))
       (when (eq success :nil) (setf success nil))  ;If nil, return nil not :NIL 12-10-87 DAB
       
       (Setq string
	     (subseq (string file-string) (setq string-origin (1+ string-origin))
	(position #\Newline (the string (string file-string)) :start string-origin :test
		  #'char-equal)) )))
    (chaos:return-pkt pkt)
    (if (eq success (quote nil))
	(setq success ()))
    (values string success))) 


(defun 4change-properties-chaos* (host pathname error-p properties)
  (let (host-unit
	pkt
	success
	string)
    (file-operation-retry
      (setq host-unit (send *qfile-service* :get-host-unit host))
      (setq string (change-properties-string properties pathname))
      (multiple-value-setq (pkt success string)
			   (send host-unit :command () () () string))
      (unwind-protect (or
			success
			(file-process-error-new string pathname () (not error-p)
						:change-properties))
	(chaos:return-pkt pkt))))) 


(defun change-properties-string (properties &optional pathname)
  (let ((*read-base* 8)
	(*print-base* 8))
    (with-output-to-string (stream)
      (format stream "CHANGE-PROPERTIES~%")
      (and pathname (format stream "~A~%" (file-print-pathname pathname)))
      (loop for (ind prop) on properties by #'cddr
	    do
	    (let ((*package* pkg-keyword-package)
		  (*print-case* :upcase))
	      (format stream "~a " ind)
	      (if prop
		  (send
		    (do ((l *known-directory-properties* (cdr l)))
			((null l) 'prin1)
		      
		      (if (member ind (cdar l) :test #'eq)
			  (return (cadaar l))))
		    
		    prop stream)
		  (princ " NIL" stream))
	      (send stream :tyo #\Newline))))))

(defun 4create-link-chaos* (host link link-to error-p)
  (let (host-unit
	pkt
	success
	string)
    (file-operation-retry
      (setq host-unit (send *qfile-service* :get-host-unit host))
      (multiple-value-setq (pkt success string)
			   (send host-unit :command () () () 3"CREATE-LINK"* #\Newline (file-print-pathname link)
				    #\Newline (file-print-pathname link-to)))
      (unwind-protect (unless
			success
			(file-process-error-new string link () (not error-p) :create-link))
	(chaos:return-pkt pkt)))))




1;; Added pathname as an argument and removed self-flavor reference. Replaces homedir-chaos *BJ**
(defun 4homedir-chaos-generic* (pathname &optional (user user-id))
  (let ((-host- (pathname-host pathname)))
    (or (cdr (assoc -host- user-homedirs :test 'eq))
	(when (not (member user-id '(nil 3""*)))
	  1;; Try logging in a file connection, in case that works.*
	  (send *qfile-service* :get-host-unit -host- t)
	  (cdr (assoc -host- user-homedirs :test 'eq)))
	1;; If we fail to establish a connection and don't already know a homedir,*
	1;; return something innocuous.  Don't get an error.*
	(send pathname :quiet-homedir user))))


(compiler:make-obsolete homedir-chaos 2"Use homedir-chaos-generic"*)
(defun 4homedir-chaos* (-host- &optional (user user-id))
  (declare (:self-flavor chaos-pathname))
  (or (cdr (assoc -host- user-homedirs :test #'eq))
      (when (not (member user-id '(nil 3""*) :test #'equal))
	1;; Try logging in a file connection, in case that works.*
	(send -host- :get-host-unit t)
	(cdr (assoc -host- user-homedirs :test #'eq)))
      1;; If we fail to establish a connection and don't already know a homedir,*
      1;; return something innocuous.  Don't get an error.*
      (send self :quiet-homedir user))) 


(defun 4directory-operation-chaos* (operation host pathname error-p)
  (let (host-unit
	pkt
	success
	file-string)
    (file-operation-retry
      (setq host-unit (send *qfile-service* :get-host-unit host))
      (multiple-value-setq (pkt success file-string)
			   (send host-unit :command () () () (string operation) #\Newline
				    (file-print-pathname pathname) #\Newline))
      (unwind-protect (cond
			(success
			 (let ((start (file-check-command (string operation) file-string)))
			   (parse-integer file-string :start start :junk-allowed t)))
			(t
			 (file-process-error-new file-string pathname () (not error-p) operation)))
	(chaos:return-pkt pkt)))))


(defun 4change-capabilities-chaos* (host capabilities enable-p &optional (host-unit nil host-unit-specd) &aux pkt success
  file-string command cap-string current-capabilities)
  (setq current-capabilities (get chaos-host-capabilities-plist host))
  (setq command (if enable-p
		  3"ENABLE-CAPABILITIES"*
		  3"DISABLE-CAPABILITIES"*))
  (cond
    ((not capabilities)
     (if enable-p
       (return-from change-capabilities-chaos)
       (setq capabilities current-capabilities)))
    ((atom capabilities) (setq capabilities (list capabilities))))
  (setq cap-string 3""*)
  (dolist (cap capabilities)
    (setq cap-string (string-append (string-upcase cap) 3" "* cap-string)))
  (or host-unit-specd (setq host-unit (send *qfile-service* :get-host-unit host)))
  (multiple-value-setq (pkt success file-string)
    (send host-unit :command () () () command #\Space cap-string))
  (unwind-protect (when
		   success
		   1;; Succeeded on one host unit.*
		   1;; Record what our capabilities are for this host.*
		   (if enable-p
		     (setq current-capabilities
			   (nunion current-capabilities capabilities :test #'equal))
		     (setq current-capabilities
			   (remove-if #'(lambda (c)
					  (member c capabilities :test #'equal))
				      current-capabilities)))
		   (setf (get chaos-host-capabilities-plist host) current-capabilities)
		   1;; Also inform any other host units that are connected now.*
		   (or host-unit-specd
		      (let ((units (send *qfile-service* :host-units host)))
			(dolist (unit units)
			  (and (neq unit host-unit) (send unit :validate-control-connection t)
			     (send unit :command () () () command #\Space cap-string))))))
    (chaos:return-pkt pkt))) 


(defun 4cwd-chaos* (host pathname error-p access-p &optional (host-unit nil host-unit-specd) &aux pkt success
  file-string command need-password enable-capabilities (default-cons-area background-cons-area))
  (setq command (if access-p
		  3"ACCESS"*
		  3"CWD"*))
  (loop
   (let ((dir (file-print-directory pathname)) (password 3""*))
	 1;; If we have failed once, ask for a new password.*
	 1;; The first time, if we remember a password, use it.*
     (cond
       (need-password
	(multiple-value-setq (dir password enable-capabilities)
	  (file-get-password dir host t))
	(when enable-capabilities
	  (send *qfile-service* :enable-capabilities host))
	(setq pathname (parse-pathname dir host)) (setq dir (file-print-directory pathname)))
       1;; We know the user id; use remembered password if any.*
       ((equal password 3""*)
	(setq password
	      (or
               (car (lookup-password-etc dir host))	1;*RLA**
	       3""*))))
     (or host-unit-specd (send *qfile-service* :get-host-unit host))
     (multiple-value-setq (pkt success file-string)
       (send host-unit :command () () () command #\Newline dir #\Newline password #\Newline))
     (unwind-protect (cond
		      (success
		       1;; Succeeded on one host unit.*
		       1;; Record what our connected or accessed directory is for this host.*
		       (setf
			(get (if access-p
			       chaos-host-access-plist
			       chaos-host-connect-plist)
			     host)
			pathname)
		       1;; Also inform any other host units that are connected now.*
		       (or host-unit-specd
			  (let ((units (send *qfile-service* :host-units host)))
			    (dolist (unit units)
			      (and (neq unit host-unit)
				 (send unit :validate-control-connection t)
				 (send unit :command () () () command #\Newline dir #\Newline
					  password #\Newline)))))
		       (return t))
		      (t
		       (condition-case-if (not error-p) (error-object)
			  (condition-case () (file-process-error-new file-string)
			     (login-problems
			      1;; Since this password is wrong, flush it from list of remembered ones.*
                              (delete-password-etc dir host)	1;*RLA**
			      (setq need-password t)))
			  (error error-object))))
       (chaos:return-pkt pkt))))) 



1;;; Stream generating functions*

(defun open-chaos (host pathname &rest options &key
		   (direction :input)
		   (characters t)
		   (error t)
		   (access-error (not error))
		   (element-type 'string-char element-type-p)
		   (if-exists
		     (if (member (pathname-version pathname)
				 ;; :UNSPECIFIC here is to prevent lossage
				 ;; writing ITS files with no version numbers.
				 '(:newest :unspecific) :test #'eq)
			 :new-version
			 :error))
		   (if-does-not-exist
		     (cond ((member direction
				    '(:probe :probe-link :probe-directory) :test #'eq) nil)
			   ((and (eq direction :output)
				 (not (member if-exists '(:overwrite :truncate :append) :test #'eq)))
			    :create)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (t :error)))
		   temporary
		   deleted
		   raw
		   super-image
		   (byte-size :default)
		   preserve-dates
		   inhibit-links
		   submit
		   estimated-length
		   &allow-other-keys)
  (let (host-unit
	data-conn
	pkt
	success
	string
	not-aborted
	phony-characters
	sign-extend-bytes
	if-exists-p
	direct-file-id
	(*package* *system-package*)
	(default-cons-area background-cons-area))
    
    (ccase direction
      ((:input :output :probe-directory :probe-link))
      (:io (setq direct-file-id (file-gensym "IO"))	; 09-02-87 DAB IO Support
	   )
      ((nil :probe)
       (setf (getf options :direction) nil)
       (setq direction ())))
    
    (check-type if-exists
		(member :error :new-version :rename :rename-and-delete :overwrite :append :truncate
			:supersede ()))
    
    (check-type if-does-not-exist (member :error :create ()))
    
    ;; IF-EXISTS-P is T if we need to give the IF-EXISTS to the server.
    (setq if-exists-p
	  (not
	    (member if-exists
		    (case (pathname-version pathname)
		      (:newest '(:new-version))
		      (:unspecific '(:new-version :supersede)))
		    :test #'eq)))
    
    (when element-type-p
      (setf (values characters byte-size phony-characters sign-extend-bytes)
	    (decode-element-type element-type byte-size))
      
      (setf (getf options :characters) characters)
      (setf (getf options :byte-size) byte-size))
    
    (file-operation-retry
      (condition-case-if access-error (error-object)
	  (progn
	   (if (member direction '(nil :probe-directory :probe-link) :test #'eq)
	       ;;PROBE mode implies no need for data connection
	       (setq host-unit (send *qfile-service* :get-host-unit host))
	       (multiple-value-setq (data-conn host-unit)
		 (send *qfile-service* :get-data-connection host direction))))
	
	(remote-network-error error-object)
	(:no-error
	 (unwind-protect
	     (progn
	       (multiple-value-setq (pkt success string)
		 
		 ;; If the destination is another Explorer.
		 (if (or (typep pathname 'new-lm-parsing-mixin)  ;09-14-88 DAB 
			 (typep pathname 'mac-pathname)
			 (eq (send host :system-type) :lispm))
		     
		     (send host-unit :command ()
			   (case direction
			     (:input (data-input-handle data-conn))
			     (:output (data-output-handle data-conn))
			     (:io ""))		; 09-02-87 DAB IO Support
			   ()
			   "OPEN-FOR-LISPM "
			   #\Newline
			   (file-print-pathname pathname)
			   #\Newline
			   (let ((*print-base* 10)
				 (*nopoint t)
				 (*package* si:pkg-user-package)
				 (*print-length* nil)
				 (*readtable* si::common-lisp-readtable))
			     
			     (when (and (eq direction :output)
					(null if-exists))
			       (setq options (list* :if-exists :error options)))
			     
			     (when (and (not if-exists-p)
					(get-location-or-nil (locf options) :if-exists))
			       (setq options (copy-list options))
			       (remprop (locf options) :if-exists))
			     
			     (when (null if-does-not-exist)
			       (setq options (list* :if-does-not-exist :error options)))
			     (when (eq direction :io)	; 09-02-87 DAB IO Support
			       (setq options (list* :direct-file-id direct-file-id options)))
			     (format nil "~S" options)))
		     
		     (send host-unit :command ()
			   (case direction
			     (:input (data-input-handle data-conn))
			     (:output (data-output-handle data-conn)))
			   ()
			   "OPEN "
			   (case direction
			     ((nil) "PROBE")
			     (:probe-directory "PROBE-DIRECTORY")
			     (:probe-link "PROBE INHIBIT-LINKS")
			     (:input "READ")
			     (:output "WRITE"))
			   " "
			   (case characters
			     ((nil) "BINARY")
			     (:default "DEFAULT")
			     (t "CHARACTER"))
			   
			   (if (eq :CHAOS-COMMON (send host :send-if-handles :server-type)) ;;LSS
                                   ;no since sending this on a probe
				(string-append
				 (if (and (eq direction :output) if-exists-p)
				     (string-append " IF-EXISTS "
						    (if (eq if-exists ())
							:error
							if-exists))
				     "")
				 (if (or if-exists-p
					 (neq if-does-not-exist
					      (case direction
						((:input nil :probe-directory :probe-link) :error)
						(:output :create))))
				     
				       (if direction   ;02-05-88 DAB
					   (string-append " IF-DOES-NOT-EXIST "
						      (if (eq if-does-not-exist ())
							  :error
							  if-does-not-exist))
					   "")
				       "")
				 )
			       "")
			   
			   (if inhibit-links
			       " INHIBIT-LINKS"
			       "")
			   (format ()
				   "~:[ BYTE-SIZE ~D~;~*~]~:[~; TEMPORARY~]~:[~; DELETED~]~
				~:[~; RAW~]~:[~; SUPER~]~:[~; PRESERVE-DATES~]~
				~:[~; SUBMIT~]~@[ ESTIMATED-LENGTH ~D~]~%~A~%"
				   (eq byte-size :default) byte-size temporary deleted raw
				   super-image preserve-dates submit estimated-length
				   (file-print-pathname pathname)))))
	       
	       (cond ((not success)
		      (setq not-aborted t)
		      (setq string (string-append string))
		      (and pkt (chaos:return-pkt pkt))
		      (or (null data-conn) (if (eq direction :io)	; 09-02-87 DAB IO Support
					       (progn (setf (data-stream data-conn :input) ())
						      (setf (data-stream data-conn :output) ()))
					       (setf (data-stream data-conn direction) ())))
		      
		      (condition-case-if (not if-does-not-exist) ()
			  (condition-case-if (not if-exists) ()
			      (file-process-error-new string pathname () (not error) :open)
			    (file-already-exists nil))
			(file-not-found nil)))
		     (t
		      (let ((properties
			      (read-file-property-list-string string "OPEN" pathname)))
			(chaos:return-pkt pkt)
			(and (eq characters :default)
			     (setq characters (getf properties :characters)))
			(unless (or (eq byte-size :default) (getf properties :byte-size))
			  (setf (getf properties :byte-size) byte-size))
			(when (eq direction :io)	; 09-02-87 DAB IO Support
			  (setf (getf properties :direct-file-id) direct-file-id))
			;; *BJ* For *DAB*.
			;; Check for case where a specific version number was specified
			;; and :if-does-not-exist is :new-version. This will make sure that
			;; the user will have the correct version number.
			(if (and (eq if-exists :new-version)
				 (eq direction :output)
				 (integerp (pathname-version pathname)))
			    (unless (eq (pathname-version pathname)
					(send (getf properties :truename) :version))
			      (setf pathname (send pathname :new-version :newest))))
			
			(prog1
			  (make-instance
			    (case direction
			      (:input
			       (if characters
				   'file-input-character-stream
				   (cond
				     (sign-extend-bytes 'file-input-signed-binary-stream)
				     (phony-characters 'file-input-phony-character-stream)
				     (t 'file-input-binary-stream))))
			      (:output
			       (if characters
				   'file-output-character-stream
				   (if phony-characters
				       'file-output-phony-character-stream
				       'file-output-binary-stream)))
			      (:io		; 09-02-87 DAB IO Support
			       (if characters
				   'file-IO-character-stream
				   'file-IO-binary-stream))
			      (t 'file-probe-stream))
			    :host-unit host-unit :data-connection data-conn
			    :property-list
			    properties :pathname pathname)
			  (setq not-aborted t))))))
	   
	   (unless (or not-aborted (null data-conn) (null (send host-unit :control-connection)))
	     ;; Here if aborted out of it and server may have file open.
	     (condition-case ()
		 (progn
		  (and (eq direction :output)
		       (send host-unit :command () (data-output-handle data-conn) () "DELETE"))
		  (multiple-value-bind (nil close-success)
		      (send host-unit :command ()
			    (case direction
			      (:input (data-input-handle data-conn))
			      (:output (data-output-handle data-conn))
			      (:io direct-file-id))		; 09-02-87 DAB IO Support
			    ()
			    "CLOSE")

		    (when close-success
		      (case direction
			(:input (read-until-synchronous-mark (data-connection data-conn)))
			(:output
			 (chaos:send-pkt (data-connection data-conn) (chaos:get-pkt)
					 %file-synchronous-mark-opcode)))))
		  (if (eq direction :io)	; 09-02-87 DAB IO Support
		      (progn
			(send host-unit :free-data-connection data-conn :input)
			(send host-unit :free-data-connection data-conn :output))
		      (send host-unit :free-data-connection data-conn direction)))
	       (host-stopped-responding nil)))
	   )					;unwind
	 )))))


(defun 4read-until-synchronous-mark* (conn)
  2"Discard data from chaosnet connection CONN up thru synchronous mark.
Used on file data connections when there is no stream yet."*
  (do (pkt
       done)
      (done)
    (setq pkt (chaos:get-next-pkt conn () 3"File Input"*))
    (select (chaos:pkt-opcode pkt)
	    1;; No data, but a synchronous mark*
       (%file-synchronous-mark-opcode (chaos:return-pkt pkt) (return ()))
       1;; Received an asynchronous mark, meaning some sort of error condition*
       ((%file-asynchronous-mark-opcode %file-eof-opcode %file-binary-opcode
	 %file-character-opcode)
	nil)
       1;; Connection closed or broken with message*
       ((chaos:cls-op chaos:los-op)
	(chaos:report-bad-connection-state conn 3"read file data from"*))
       1;; Not a recognized opcode, huh?*
       (otherwise
	(ferror () 3"Receieved data packet (~S) with illegal opcode for file data conn."* pkt)))
    (chaos:return-pkt pkt))) 


(defun 4file-print-pathname* (pathname)
  2"Return namestring for PATHNAME, including the host if it isn't an actual machine."*
  (let ((hn (send (send pathname :host) :send-if-handles :remote-host-name))
	sfh
	(qstring (send pathname :operation-handled-p :real-string-for-host))
	(default-cons-area pathname-area))
    (setq sfh
	  (if qstring
	    (send (send pathname :translated-pathname) :real-string-for-host)
	    (send (send pathname :translated-pathname) :string-for-host)))
    (if hn
      (string-append hn 3": "* sfh)
      sfh))) 


(defun 4file-print-directory* (pathname)
  2"Return namestring for PATHNAME's dir, including the host if it isn't an actual machine."*
  (let ((hn (send (send pathname :host) :send-if-handles :remote-host-name))
	sfd
	(qstring (send pathname :operation-handled-p :real-string-for-directory))
	(default-cons-area pathname-area))
    (setq sfd
	  (if qstring
	    (send (send pathname :translated-pathname) :real-string-for-directory)
	    (send (send pathname :translated-pathname) :string-for-directory)))
    (if hn
      (string-append hn 3": "* sfd)
      sfd))) 

1;;; PATHNAME is only used as a source of a host with respect to which to parse*
(defun read-file-property-list-string (string operation pathname &optional
	 (properties-to-read
	  '((:creation-date) (:creation-time) (:length t) (:qfaslp t) (:characters t)
	    (:author t)))
	 &aux pathname-origin property-list (default-cons-area background-cons-area))

  
  (unless
    (setq pathname-origin (position #\Newline (the string (string string)) :test #'char-equal))   
    (ferror () "Illegally formatted string ~S." string))
  
  (do ((i (file-check-command operation string))
       (prop properties-to-read (cdr prop))
       (*read-base* 10)
       (*package* *system-package*)   ;11-16-87 DAB
       (type)
       (date-start))
      ((or (null i) (> i pathname-origin) (null prop)))
    
    (setq type (caar prop))
    
    (case type
      (:creation-date (setq date-start i))
      
      (:length
	(setf (getf property-list :creation-date)
	      (if (not (fboundp 'time:parse-universal-time))
		  
		  ;;When bootstrapping, dates are recorded as strings.
		  ;;Discard zero at front of month, so the format
		  ;;matches that produced by PRINT-UNIVERSAL-TIME.
		  (string-left-trim #\0 (subseq (string string) date-start i) )
		  (parse-directory-date-property string date-start i)))))
    
    (cond ((cadar prop)
	   (multiple-value-bind (propval endpos)
	       (read-from-string string () () :start i)
	     (setq i endpos)
	     (setf (getf property-list type) propval)))
	  (t
	    (setq i (position #\Space (the string (string string)) :start (1+ i) :test #'char-equal)))))
  
  (let* ((truename-string (subseq (string string)
				  (setq pathname-origin (1+ pathname-origin))
				  (position #\Newline (the string (string string))
					    :start pathname-origin :test #'char-equal)))
	 (truename (send pathname :parse-truename truename-string
			 (net:translated-host (pathname-host pathname))))) ;6.16.87 MBC
    
    (setf (getf property-list :truename) truename))
  
  property-list) 


(defun 4make-file-property-list-stream-chaos* (host command string-arg token-args pathname noerror-p)
  (let (data-conn
	 host-unit
	 pkt
	 success
	 not-aborted string
	 (*package* *system-package*)
	 (default-cons-area background-cons-area))
    
    (multiple-value-setq (data-conn host-unit)
			 (send *qfile-service* :get-data-connection host :input))
    
    (unwind-protect
	(progn
	  (multiple-value-setq (pkt success string)
			       (send host-unit :command () (data-input-handle data-conn) () command
				     token-args #\Newline string-arg #\Newline))
	  (cond ((not success)
		 (setq not-aborted t)
		 (setq string (string-append string))
		 (chaos:return-pkt pkt)
		 (setf (data-stream data-conn :input) ())
		 (file-process-error-new string pathname () noerror-p :directory-stream))
		
		(t
		  (file-check-command command string)
		  (chaos:return-pkt pkt)
		  (prog1
		    (make-instance 'file-directory-stream :host-unit host-unit
				   :data-connection data-conn :pathname pathname)
		    (setq not-aborted t)))))
      
      1;; Both success and failure set NOT-ABORTED once they get past critical section.*
      (unless (or not-aborted (null data-conn)
		  (null (send host-unit :control-connection)))
	      1;; Here if aborted out of it and server may have directory stream open.*
	      (condition-case ()
		  (multiple-value-bind (nil close-success)
		      (send host-unit :command () (data-input-handle data-conn) () 3"CLOSE"*)
		    (when close-success
			  (read-until-synchronous-mark (data-connection data-conn)))
		    (send host-unit :free-data-connection data-conn :input))
		(host-stopped-responding nil))))))


(defun 4multiple-plists-chaos* (host pathnames options &aux file-list connection (characters t)
  (default-cons-area background-cons-area))
  
  (loop for (ind opt) on options by 'cddr do
     (case ind
       (:characters (setq characters opt))
       (otherwise (ferror () 3"~S is not a known MULTIPLE-FILE-PLISTS option"* ind))))
  
  (setq connection (host-unit-control-connection (send *qfile-service* :get-host-unit host)))
  (setq file-list (loop for pathname in pathnames collect (list pathname ())))
  
  (do ((list-to-do file-list (cdr list-to-do))
       (pending-list (copy-list file-list))
       (elem-to-do))
      ((null pending-list))
    (setq elem-to-do (car list-to-do))
    (do ((p-l pending-list (cdr p-l))
	 (elem))
	((or (null p-l)
	    (and elem-to-do (not (chaos:data-available connection))
	       (chaos:may-transmit connection))))
      (setq elem (car p-l))
      (let ((transaction-id (second elem)))
	(and transaction-id
	   (let* ((pkt (file-wait-for-transaction transaction-id connection 3"Probe"*))
		  (pkt-string (chaos:pkt-string pkt))
		  (string
		   (nsubstring pkt-string
			       (1+
				(position #\Space (the string (string pkt-string)) :test
					  #'char-equal))))
		  (from (1+ (string-search-set '(#\Space #\Newline) string)))
		  (success
		   (not
		    (string-equal 3"ERROR"* string :start2 from :end1 5 :end2
				  (string-search-set '(#\Space #\Newline) string from))))
		  (property-list nil))
	     (and success
		(setq property-list (read-file-property-list-string string 3"OPEN"* (first elem))))
	     (chaos:return-pkt pkt)
	     (setf (cdr elem) property-list)
	     (setq pending-list (delete elem (the list pending-list) :test #'eq))))))
    (and elem-to-do
       (let ((mode (case characters
		     ((nil) :binary)
		     (:default :default)
		     (t :character)))
	     (pkt (chaos:get-pkt))
	     (transaction-id (file-make-transaction-id ())))
	 (chaos:set-pkt-string pkt transaction-id 3"  OPEN PROBE "* mode #\Newline
			       (file-print-pathname (first elem-to-do)) #\Newline)
	 (chaos:send-pkt connection pkt %file-command-opcode)
	 (setf (second elem-to-do) transaction-id))))
  file-list) 


(defun 4directory-chaos* (host pathname options)
  (let ((noerror-p nil)
	(deleted-p nil)
	(fast-p nil)
	(dirs-only-p nil)
	(no-extra-info nil)
	(sorted-p nil))
    
  (file-operation-retry
   (do ((l options (cdr l)))
       ((null l))
     
     (case (car l)
       (:noerror (setq noerror-p t))
       
       (:fast (setq fast-p t))
       
       (:no-extra-info (setq no-extra-info t))
       
       (:sorted (setq sorted-p t))
       
       1;; This is for the :ALL-DIRECTORIES message*
       (:directories-only (setq dirs-only-p t))
       
       1;; This is for TOPS-20*
       (:deleted (setq deleted-p t))
   
       ;; This is for NFS 08-10-88 DAB
       (:short nil)
       
       (otherwise (ferror () 3"~S is not a known DIRECTORY option"* (car l)))))   
   
   (make-file-property-list-stream-chaos host 3"DIRECTORY"*
					 (file-print-pathname pathname)
					 (format () 3"~:[~; DELETED~]~
                                                     ~:[~; FAST~]~
                                                     ~:[~; DIRECTORIES-ONLY~]~*
		3                                     ~:[~; NO-EXTRA-INFO~]~
                                                     ~:[~; SORTED~]"*
						 deleted-p
						 fast-p
						 dirs-only-p
						 no-extra-info
						 sorted-p)
					 pathname noerror-p))))


(defun 4properties-chaos* (type thing error-p &aux (pathname (case type
				      (:file thing)
				      (:stream (send thing :truename))))
  settable-properties got-error plist)
  2"TYPE is either :FILE or :STREAM."*
  (declare (values plist settable-properties))
  (with-open-stream-case
   (s
    (make-file-property-list-stream-chaos (send pathname :host) 3"PROPERTIES"*
					  (if (eq type :file)
					    (file-print-pathname thing)
					    3""*)
					  (if (eq type :stream)
					    (send thing :file-handle)
					    3""*)
					  pathname ()))
   (error (if (not error-p)
	    (setq got-error s)
	    (signal-condition s)))
   (:no-error (setq settable-properties (parse-settable-properties (send s :line-in) 0))
    (setq plist (send pathname :read-directory-stream-entry s))))
  (or got-error (values plist settable-properties))) 



;;;
1;;; Initializations* 
;;;

(add-initialization 3"File Login"* '(file-login t) '(login)) 
(add-initialization 3"File Logout"* '(file-login ()) '(logout))
(add-initialization 3"File System Init"* '(file-system-init) nil 'chaos:net-system-init-list)

(defun 4file-login* (login-p)
  2"Log all open host units in or out.  LOGIN-P = NIL means log out, otherwise log in."*
  (clear-connect-and-access-memory)
  (dolist (host host:*host-list*)
    (dolist (unit (send *qfile-service* :host-units host))
      (send *qfile-service* :login-unit unit login-p))))

(defun 4file-system-init* ()
  "Abort pending file transactions, reinitialize Qfile hosts, and start host-unit gc."
  ;; Abort pending transactions.
  (without-interrupts
    (do ((element (pop *file-pending-transactions*)
		  (pop *file-pending-transactions*)))
	((null element))
	    (let ((pkt (file-transaction-id-pkt element)))
	      (when (and pkt
			 (eq (chaos:pkt-status pkt) 'chaos:released))
		(chaos:return-pkt pkt)))))
  
  (dolist (host host:*host-list*)
	  (send host :send-if-handles :reset))
  
  (init-dormant-host-gc-process))


1;;; These functions take care of closing connections that have not been used for a while.*

(defun 4reset-dormant-host-units* ()
   (dolist (host host:*host-list*)
	   (dolist (host-unit (send *qfile-service* :host-units host))
		   
		   1(if (host-unit-data-connections h*ost-1u*nit1)*
		       1(*send1 h*ost-1u*nit1 :close-dormant-data-connections))*

		      1(without-interrupts*

			 ;; Don't reset it if it is already reset (no control-connections)
			 ;;  or if it is not dormant.
			 (when (and 1(*send1 h*ost-1u*nit1 :control-connection)*
				    1(host-unit-dormant h*ost-1u*nit1)*)

			       1(setf (host-unit-lock h*ost-1u*nit1) 'locked-for-suicide)*
			       1(*send1 h*ost-1u*nit1 :reset)*)))))

(defparameter 4host-unit-lifetime* 72000 "Time in 60th's of a second for a qfile server connection 
                                          to remain idle before it is closed.")    1;20 MINUTES.*

(defparameter 4data-connection-lifetime* 3600) 

(defun 4host-unit-dormant* (host-unit)
  (and (not (host-unit-lock host-unit))
       
       1;; Don't kill a host unit if it has more than one data connection still,*
       (null (rest (host-unit-data-connections host-unit)))
     
       1;; or if that data connection is doing anything.*
       (let ((data-connection (first (host-unit-data-connections host-unit))))
	 (or (null data-connection)
	     (and (null (data-stream data-connection :input))
		  (null (data-stream data-connection :output)))))
       
       (> (time-difference (time) (host-unit-last-use-time host-unit)) host-unit-lifetime))) 

(defun 4data-connection-dormant* (data-connection)
  (and (null (data-stream data-connection :input))
       (null (data-stream data-connection :output))
       (> (time-difference (time) (data-last-use-time data-connection)) data-connection-lifetime))) 1 *

(defmethod 4(host-unit :close-dormant-data-connections*) ()

  ;; Just use with-lock here so that we do not update last-use-time.
  (with-lock (lock)
    (dolist (data-connection data-connections)
      (cond ((data-connection-dormant data-connection)
	     (condition-case ()		   
		 (let ((old-last-use-time last-use-time))
		   (send self :command () (data-input-handle data-connection) () 3"UNDATA-CONNECTION"*)
		   
		   ;; Make unit look like it has not been used. 
		   ;; This will allow control connection to die earlier.
		   (setf last-use-time old-last-use-time))
	       
	       (sys:network-error))
	     (let ((conn (data-connection data-connection)))
	       (condition-case () (chaos::close-conn conn 3"Done"*) (sys:network-error))
	       (condition-case () (chaos:remove-conn conn) (sys:network-error)))
	     (setq data-connections (delete data-connection (the list data-connections) :test #'eq))))))) 

1;;; The following takes care of the process to do the above stuff every *thirty 1minute*s1.*
;;(defvar 4dormant-host-connection-gc-wait-arg-list* (list () (* 30 3600)))
(defvar 4dormant-host-connection-gc-wait-*interval (* 30 3600))
(defvar 4dormant-host-gc-process* nil4)* 

(add-initialization "Kill dormant-host-gc-process" '(init-dormant-host-gc-process nil)
		    '(:before-cold :full-gc)) 

(defparameter *debug-Qfile-connection-gc* nil "Don't signal an error if this Var is either nil or :notify.")

(defun 4dormant-host-connection-gc-top-level* ()
  (loop
    (process-sleep 4dormant-host-connection-gc-wait-*interval "Qfile GC Sleep")
    (condition-case-if (or (null *debug-Qfile-connection-gc*)
			   (eq *debug-qfile-connection-gc* :notify))
		       (condition)
	(reset-dormant-host-units)
      (error
       (when (eq *debug-qfile-connection-gc* :notify)
	 (tv:notify nil "Error from Qfile connection GC: ~s" (send condition :report-string)))))))

(defun 4init-dormant-host-gc-process* (&optional (enable-p t))
  (cond ((and enable-p dormant-host-gc-process)
	 (process-reset dormant-host-gc-process))
	
	(enable-p
	 (setf dormant-host-gc-process
	       (process-run-function '(:name 3"Dormant FILE connection GC"*
					     :restart-after-reset t
					     :restart-after-boot t
					     :priority -2)
				     'dormant-host-connection-gc-top-level)))
	
	(dormant-host-gc-process
	 (send dormant-host-gc-process :kill)
	 (setf dormant-host-gc-process nil))))


1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Qfile access functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*

(defflavor 4qfile*
	   ((host:name :qfile)
	    (host:desirability .50))
	   (host:service-implementation-mixin)
  (:method-combination (:case :base-flavor-last :enable-capabilities-internal)
		       (:case :base-flavor-last :disable-capabilities-internal)
		       (:case :base-flavor-last :max-data-connections-internal)
		       (:case :base-flavor-last :login-unit-internal)
		       (:case :base-flavor-last :hsname-information-internal))
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(host:define-service-implementation 'qfile)
(host:define-logical-contact-name 3"file"* '((:chaos 2"FILE 1"*)))

(defvar 4*qfile-service** (host:find-service-implementation :qfile))

(defmethod 4(qfile :open*) (medium pathname options)
  (declare (ignore medium))
  (force-user-to-login)
  (apply #'open-chaos (net:translated-host (pathname-host pathname)) pathname options))

(defmethod 4(qfile :delete*) (medium pathname &optional (error-p t))
  (declare (ignore medium))
  (delete-chaos (net:translated-host (pathname-host pathname)) pathname error-p))

(defmethod 4(qfile :delete-and-expunge*) (medium pathname &optional (error-p t))
  (declare (ignore medium))
  (delete-and-expunge-chaos (net:translated-host (pathname-host pathname)) pathname error-p))

(defmethod 4(qfile :undelete*) (medium pathname &optional (error-p t))
  (declare (ignore medium))
  (change-file-properties pathname error-p :deleted nil))

(defmethod 4(qfile :expunge*) (medium pathname &optional (error-p t))
  (declare (ignore medium))
  (directory-operation-chaos :expunge (net:translated-host (pathname-host pathname)) pathname error-p))

(defmethod 4(qfile :delete-multiple-files*)(medium pathnames &optional (error-p t))
  (dolist (pathname pathnames)
    (send self :delete medium pathname error-p)))

(defmethod 4(qfile :delete-and-expunge-multiple-files*) (medium pathnames &optional (error-p t))
  (declare (values number-of-blocks-freed))
  (loop for pathname in pathnames
	with n-blocks
	do (setf n-blocks (send self :delete-and-expunge medium pathname error-p))
	when (numberp n-blocks)
	sum n-blocks))


(defmethod 4(qfile :rename*) (medium pathname new-pathname &optional (error-p t))
  (declare (ignore medium))
  (rename-chaos (net:translated-host (pathname-host pathname)) pathname new-pathname error-p))

(defmethod 4(qfile :directory-list*) (medium pathname &optional options)
  (let (dir-list
	(*merge-timezone* time:*timezone*))  ;01-20-88 DAB Used by parse-directory-date-property to control
                                             ;baising the time by daylight savings.

    (declare (special *merge-timezone* ))    ;01-20-88 DAB
    (with-open-stream (stream (send self :directory-stream medium pathname (remove :sorted (the list options) :test 'eq)))
      (if (errorp stream)
	  stream
	  (setq dir-list
		(loop as entry = (send stream :read-directory-stream-entry)
		      until (null entry)
		      collecting entry))
	  (if (member :sorted options :test 'eq)
	      (let ((null-elem (assoc nil dir-list :test 'eq)))
		(and null-elem (setq dir-list (delete null-elem (the list dir-list) :test 'eq)))
		(setq dir-list (sort dir-list #'pathname-lessp :key #'car))
		(and null-elem (push null-elem dir-list))))
	  dir-list))))

(defmethod 4(qfile :directory-stream*) (medium pathname &optional options)
  (declare (ignore medium))
  (directory-chaos (net:translated-host (pathname-host pathname)) pathname options))

(defmethod 4(qfile :properties*) (medium pathname &optional (error-p t))
  (let ((dir (send self :directory-list medium pathname (if error-p '(:deleted) '(:noerror :deleted)))))
    (cond ((consp dir)
	   (values (cadr dir) (get (car dir) ':settable-properties)))
	  (t dir))))

(defmethod 4(qfile :all-directories*) (medium pathname &optional options)
  (let ((dirs (send self :directory-list medium pathname (cons :directories-only options))))
    (if (errorp dirs) dirs
      (setq dirs (cdr dirs))
      (dolist (x dirs)
	(rplaca x (send (car x) :new-pathname :name :unspecific :type :unspecific
			   :version :unspecific)))
      dirs)))

(defmethod 4(qfile :multiple-file-plists*) (medium files &optional options)
  (declare (ignore medium))
  (let ((host (send (pathname (first files)) :host)))
    (multiple-plists-chaos host files options)))

(defmethod 4(qfile :change-properties*) (medium pathname plist &optional (error-p t))
  (declare (ignore medium))
  (change-properties-chaos (net:translated-host (pathname-host pathname)) pathname error-p plist))

(defmethod 4(qfile :complete-string*) (medium pathname string options)
  (declare (ignore medium))
  (let ((host (net:translated-host (pathname-host pathname)))
	success)
    (multiple-value-setq (string success)
			 (complete-chaos host pathname string options))
    (let ((default-cons-area background-cons-area))
      (values (string-append (send host :name-as-file-computer) 3": "* string) success))))

(defmethod 4(qfile :create-directory*) (medium pathname &optional (error-p t))
  (declare (ignore medium))
  (directory-operation-chaos :create-directory
			     (net:translated-host (pathname-host pathname)) pathname error-p))

(defmethod 4(qfile :create-link*) (medium pathname link-to &optional (error-p t))
  (declare (ignore medium))
  (create-link-chaos (net:translated-host (pathname-host pathname)) pathname link-to error-p))

(defmethod 4(qfile :truename*) (medium pathname error-p)
  ;(with-open-stream (stream (send self :open medium pathname `(:error ,error-p :direction nil)))
   (with-open-stream (stream (send self :open medium pathname (list :error error-p :direction nil)))
	  (if (errorp stream)
	      stream
	      (send stream :truename))))

(defmethod (qfile :reset) (medium host)
  "Reset all host units for a host."
  (declare (ignore medium))
  (dolist (host-unit (send self :host-units host))
	  (send host-unit :reset)))

(defmethod 4(qfile :homedir*) (medium pathname &optional (user user-id))
  (declare (ignore medium))
  (homedir-chaos-generic pathname user))

1;;;
;;; Internal only methods
;;;*

(defmethod 4(qfile :login-unit*) (unit login-p)
  (send self :login-unit-internal (send (host-unit-host unit) :system-type) unit login-p))

(defmethod 4(qfile :new-host-unit*) (host &optional (noerror-p nil))
  
  (let ((default-cons-area background-cons-area)
	unit)
    
    (setq unit (make-instance 'host-unit :host host))
    (send self :register-host-unit host unit)
    (and (send unit :validate-control-connection noerror-p)
	 unit)))


(defmethod (qfile :get-host-unit) (host &optional noerror-p)
  ;;Clear connection failures,otherwise we get implementation errors.  03-11-88 DAB
  (let ((host-units (send self :host-units host)))
    (cond ((null host-units)
	   (send self :new-host-unit host noerror-p))
	  ((loop for unit in host-units
		 when (send unit :validate-control-connection t)
		 return unit))
	  ((not noerror-p)
	   (let ((unit (car host-units)))
	     (send unit :validate-control-connection)
	     unit)))))

(defmethod (qfile :get-data-connection) (host direction &optional noerror-p)
  (let ((host-units (send self :host-units host)))
    (block top
      (do ((error-p nil t)) (nil)
	;;Clear connection failures,otherwise we get implementation errors.  03-11-88 DAB
	(do ((units host-units (cdr units))	   
	     (unit) (data-conn))
	    ((null units))
	  (setq unit (car units))
	  (and (send unit :validate-control-connection (or noerror-p (not error-p)))
	       (setq data-conn (send unit :get-data-connection direction))
	       (return-from top data-conn unit)))
	(and noerror-p
	     (not (send self :get-host-unit host t))	   ;If you can't get a valid connection,
	     (return-from top nil nil))	   ;then you have a loosing host.
	(and error-p
	     (let* ((unit (send self :new-host-unit host))
		    (data-conn (send unit :get-data-connection direction)))
	       (or data-conn
		   (ferror nil "New unit failed to allocate data connection"))
	       (return-from top data-conn unit)))))))



(defmethod 4(qfile :enable-capabilities*) (host capabilities)
  (send self :enable-capabilities-internal (send host :system-type) host capabilities))

(defmethod 4(qfile :disable-capibilities*) (host capabilities)
  (send self :disable-capibilities-internal (send host :system-type) capabilities))

(defmethod 4(qfile :hsname-pathname*) (string host)
  (fs:parse-pathname (format nil "~a:~a " (send (net:translated-host host) :fully-qualified-name) string)))

(defmethod 4(qfile :hsname-information*) (unit str idx)
  (send self :hsname-information-internal (send (host-unit-host unit) :system-type) unit str idx))

(defmethod 4(qfile :host-units*) (host)
  (get host :host-units))

(defmethod 4(qfile *:register-4host-unit*) (host unit)
  (push unit (get host :host-units)))

1;;;
;;; System-type dependent methods
;;;*

(defmethod 4(qfile :hsname-information-internal*) (system-type unit str idx)
  (declare (ignore system-type))
  (let (pathname)
    (setf pathname
	  (ignore-errors
	    (send self :hsname-pathname
	      (subseq (string str) (setq idx (1+ (position #\Newline str :start idx)))
		      (position #\Newline str :start idx))
	      
	      (host-unit-host unit))))
    (unless pathname
	    (setf pathname (quiet-user-homedir (host-unit-host unit))))
    (values
      pathname
      user-personal-name
      user-group-affiliation
      user-personal-name-first-name-first)))

(defun tenex-family-hsname-information (foo system-type unit str idx)
  (declare (ignore system-type foo))
  (let* ((default-cons-area fs:background-cons-area)
	 (hsname (subseq (string str) (setq idx (1+ idx)) (setq idx (position #\Newline str :start idx))) )
	 (hsname-pathname (send self :hsname-pathname hsname (fs:host-unit-host unit)))
	 (personal-name (subseq (string str) (setq idx (1+ idx))
				(setq idx (position #\Newline str :start idx))) )
	 (group-affiliation (if (or (null idx)
				    (eq idx (1- (length str))))
				#\Sp
				(aref str (1+ idx)))))
    (setq idx (search 3", "* personal-name)
	  str (nsubstring personal-name 0 idx))
    (and idx (setq str (string-append (nsubstring personal-name (+ idx 2)) #\Sp str)))
    (values
      hsname-pathname
      personal-name
      group-affiliation
      str)))

(defmethod 4(qfile :case :hsname-information-internal :vms4*) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :vms*) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*unix) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*unix-ucb) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*its) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*tops20) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*tenex) tenex-family-hsname-information)

(defmethod 4(qfile :case :hsname-information-internal :*multics) tenex-family-hsname-information)

(defmethod (qfile :case :hsname-information-internal :lmfs) (unit str idx)
  (values
    (send self :hsname-pathname
	  (subseq str
		  (setq idx (1+ (setq idx (position #\Newline str :start idx))))
		  (position #\Newline str :start idx))
	  (host-unit-host unit))
    user-personal-name user-group-affiliation
    user-personal-name-first-name-first))

;; Note that MSDOS does not currently support chaosnet.
;;;
;;; Login-unit
;;;

(defmethod 4(qfile :login-unit-internal*) (system-type unit login-p)
  (declare (ignore system-type))
  (let ((default-cons-area background-cons-area)
	(conn (host-unit-control-connection unit))
	(host (host-unit-host unit))
	tem)
    
    1;; Don't confuse the user by asking for UNAME and PASSWORD if he's logged in elsewhere.*
    (and (setq tem (cond ((not (equal user-id 3""*)) user-id)
                         ((uname-on-host nil 'its))	1;*RLA**
			 ((first-uname))))	1;*RLA**
         (file-host-user-id tem host))		1;*RLA**
    
    1;; Connection is used up when logging out*
    (and conn (eq (chaos:state conn) 'chaos:open-state)
	 (if login-p
	     (login-host-unit unit login-p host)
	     (setf (host-unit-control-connection unit) nil)
	     (chaos:close-conn conn 3"Logging out"*)))
    t))

(defun tenex-login-unit (foo system-type unit login-p)
  (declare (ignore system-type foo))
  (let ((default-cons-area background-cons-area)
	(conn (host-unit-control-connection unit)))
    1;; Connection is used up when logging out*
    (and conn (eq (chaos:state conn) 'chaos:open-state)
	 (if login-p
	     (login-host-unit unit login-p (host-unit-host unit))
	     (setf (host-unit-control-connection unit) nil)
	     (chaos:close-conn conn 3"Logging out"*)))
    t))

(defmethod 4(qfile :case :login-unit-internal :vms4*) tenex-login-unit)

(defmethod 4(qfile :case :login-unit-internal :vms*) tenex-login-unit)

(defmethod 4(qfile :case :login-unit-internal :*unix) tenex-login-unit)

(defmethod 4(qfile :case :login-unit-internal :*unix-ucb) tenex-login-unit)

(defmethod (qfile :case :login-unit-internal :its) (unit login-p)
  (login-host-unit unit login-p 'its))

(defmethod (qfile :case :login-unit-internal :tops20) tenex-login-unit)

(defmethod (qfile :case :login-unit-internal :tenex) tenex-login-unit)

(defmethod (qfile :case :login-unit-internal :multics) tenex-login-unit)

;(defmethod (qfile :case :login-unit-internal :lmfs) tenex-login-unit)

;; Note that MSDOS does not currently support chaosnet.

;;;
;;; Enable/disable capabilities
;;;

(defmethod 4(qfile :enable-capabilities-internal*) (system-type host &rest capabilities)
  (declare (ignore system-type host capabilities))
  nil)

(defmethod 4(qfile :case :enable-capabilities-internal :vms4*) (host &rest capabilities)
  (fs:change-capabilities-chaos host (or capabilities '(3"SYSPRV"*)) t))

(defmethod 4(qfile :case :enable-capabilities-internal *:vms) (host &rest capabilities)
  (fs:change-capabilities-chaos host (or capabilities '(3"SYSPRV"*)) t))

(defmethod (qfile :case :enable-capabilities-internal :unix) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

(defmethod (qfile :case :enable-capabilities-internal :unix-ucb) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

(defmethod (qfile :case :enable-capabilities-internal :its) (host &rest capabilities)
  (declare (ignore host capabilities))
  nil)

(defmethod (qfile :case :enable-capabilities-internal :tops20) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

(defmethod (qfile :case :enable-capabilities-internal :tenex) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

(defmethod (qfile :case :enable-capabilities-internal :multics) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

(defmethod (qfile :case :enable-capabilities-internal :lmfs) (host &rest capabilities)
  (change-capabilities-chaos host (or capabilities '("OPERATOR" "WHEEL")) t))

;; Note that MSDOS does not currently support chaosnet.

;;;
;;; Disable capabilities
;;;

(defmethod 4(qfile :disable-capabilities-internal*) (system-type host &rest capabilities)
  (declare (ignore system-type  host capabilities))
  nil)

(defmethod 4(qfile :case :disable-capabilities-internal :vms4*) (host &rest capabilities)
  (fs:change-capabilities-chaos host capabilities nil))

(defmethod 4(qfile :case :disable-capabilities-internal *:vms) (host &rest capabilities)
  (fs:change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :unix) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :unix-ucb) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :its) (host &rest capabilities)
  (declare (ignore host capabilities))
  nil)

(defmethod (qfile :case :disable-capabilities-internal :tops20) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :tenex) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :multics) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

(defmethod (qfile :case :disable-capabilities-internal :lmfs) (host &rest capabilities)
  (change-capabilities-chaos host capabilities nil))

;; Note that MSDOS does not currently support chaosnet.

(compile-flavor-methods qfile)


(defun notify-nicely (Window-of-interest Message &rest args)    ;new function  07-11-88 DAB
  " Display a message to the user, but do it nicely. Do not lock the system if the user does not 
    respond."
  (let ((tv:wait-for-notifications-flag T)) (apply 'TV:NOTIFY Window-of-interest  message args)))
