;;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(MEDFNT HL12B TR12BI) -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986,1988 Texas Instruments Incorporated.  All rights reserved.*

;1;;------*
;1;;  TCP USER INTERFACE*
;1;;------*
;1;;  These functions constitute the user interface to the Transport Control Protocol,*
;1;;  as specified by the DARPA Internet Program Protocol Specification (RFC793).*
;1;;*
;1;;  open-tcp-connection*
;1;;*
;1;;  methods of tcp-connection       interact with connection object*
;1;;    :open*
;1;;    :send*
;1;;    :receive*
;1;;    :close*
;1;;    :abort*
;1;;    :status*
;1;;    :get-octet*
;1;;    :get-multiple-octets*
;1;;  add-to-server-list                register network service*
;1;;  add-server*
;1;;  delete-server*
;1;;*
;1;;  The following functions are provided for creating highly specialized applications.*
;1;;  TCP will manage the non-well known port number space and the application*
;1;;  need not be explicitly involved.*
;1;;*
;1;;  generate-tcp-port                generate non-well known port number*
;1;;  reserve-tcp-port                 reserve port number*
;1;;  unreserve-tcp-port               unreserve port number*


(DEFUN open-tcp-connection (source-port destination-port destination-address mode buffer-handler urgent-handler
			    receive-fin-handler close-complete-handler user-timeout-handler condition-handler
			    &key process-fin-handler (timeout (* 5 60)) (precedence 0) (security 0))
  "2Open a TCP connection and return the connection object as the return value.  Required arguments are:
SOURCE-PORT If zero, an valid non-well known source port is generated.
  Otherwise must be a valid port number.
DESTINATION-PORT If zero and mode is :passive, signifies unspecified.
  Otherwise must be a valid port number.
DESTINATION-ADDRESS If zero and mode is :passive, signifies unspecified.
  Otherwise must be a valid IP address specification.
MODE :active or :passive.  Any other value is assumed :passive.
The following parameters are the event handling functions for TCP.  WARNING: None of these handler
functions should call, directly or indirectly, the methods of tcp-connection, excepting :get-octet and
:get-multiple-octets.
BUFFER-HANDLER  A function of three arguments - octet count, push flag, urgent flag.  When the
  urgent flag is non-nil, it is actually the number of octets of urgent data in octet count.
URGENT-HANDLER  A function of no arguments, called when entering 'urgent mode'. 
RECEIVE-FIN-HANDLER  A function of no arguments, called to process the 'connection closing' event
  (not error condition).  After RECEIVE-FIN-HANDLER is called, no further calls will be made to BUFFER-HANDLER, 
  and an error will be signaled if a :receive is done.
CLOSE-COMPLETE-HANDLER  A function of no arguments called when entering the :closed or :time-wait states.
  When this function is called the connection object has been deleted and should not be accessed again.
  Therefore, all references to the connection object must be replaced by the value returned from
  (SEND *tcp-handler* :dummy-connection).
USER-TIMEOUT-HANDLER  A function of no arguments called to process the 'user timeout' event.  If nil,
  the condition-handler will be called to handle the 'user timeout' event.
CONDITION-HANDLER  Condition handler for the TCP signals. A function of one argument, a condition object.
  The return value from this function must be a valid proceed type, or nil meaning the condition was not handled.
  When this function is called the connection object has been deleted and should not be accessed again.
  Therefore, all references to the connection object must be replaced by the value returned from
  (SEND *tcp-handler* :dummy-connection condition-object).
  The possible condition-names include: connection-does-not-exist, connection-already-exists,
  securitycompartment-not-allowed, precedence-not-allowed, illegal-connection, connection-reset,
  connection-aborted-due-to-user-timeout, connection-closing, connection-refused, foreign-socket-unspecified.
Options available:
:PROCESS-FIN-HANDLER (Optional keyword) A function of no arguments, called to proceess the arrival of the fin.
  The RECEIVE-FIN-HANDLER will be called later when the fin is received in sequence number order.
:TIMEOUT (Optional keyword) 'User timeout' in seconds (300 default).  Nil indicates no timeout.
  The 'user timeout' is a timeout on the acknowlegment of data sent.
:PRECEDENCE (Optional keyword) Not used at this time.
:SECURITY (Optional keyword) Not used at this time.*"
  (LET ((connection nil)
	(open-complete nil))
    (UNWIND-PROTECT
	(PROGN
	  (SETF connection (SEND *tcp-handler* :make-connection
				 buffer-handler urgent-handler process-fin-handler receive-fin-handler
				 close-complete-handler user-timeout-handler condition-handler))
	  (SEND connection :open source-port destination-port destination-address mode timeout
		precedence security)
	  (SETF open-complete t)
	  connection)
      (WHEN (AND connection (NOT open-complete))
	(SEND *tcp-handler* :delete-tcb connection)))))



(DEFMETHOD (tcp-connection :open) (tcp-source-port tcp-destination-port tcp-destination-address mode
				   &optional (tcp-timeout (* 5 60)) (tcp-precedence 0) (tcp-security 0))
  "2Open a TCP connection (RFC793 p45, p54). Options available:
TCP-SOURCE-PORT If zero, an valid non-well known source port is generated.
  Otherwise must be a valid port number.
TCP-DESTINATION-PORT If zero and mode is :passive, signifies unspecified.
  Otherwise must be a valid port number.
TCP-DESTINATION-ADDRESS If zero and mode is :passive, signifies unspecified.
  Otherwise must be a valid IP address specification.
MODE :active or :passive.  Any other value is assumed :passive.
TCP-TIMEOUT (Optional) 'User timeout' in seconds (300 default).
  The 'user timeout' is a timeout on the acknowlegment of data sent.
TCP-PRECEDENCE (Optional) Not used at this time.
TCP-SECURITY (Optional) Not used at this time.*"
  (drive-connection-exclusively
    :norecursive
    (CASE state
	  (:closed)
	  (:listen
	   ;1; insure connection object is not registered*
	   (WITHOUT-INTERRUPTS
	     (SEND *tcp-handler* :deregister-tcb self)
	     (unreserve-tcp-port source-port)
	     (SETF source-port ())))
	  (otherwise (FERROR 'connection-already-exists "connection already exists" self)))
    ;1; perform mode, state, and destination socket validation*
    (CASE mode
	  (:active
	   (WHEN (NOT (AND (INTEGERP tcp-destination-port) (PLUSP tcp-destination-port)
			   (< tcp-destination-port 65536)))
	     (FERROR 'invalid-destination-port "connection illegal for this process" self))
	   (WHEN (OR (ZEROP tcp-destination-address) (ZEROP tcp-destination-port))
	     (FERROR 'foreign-socket-unspecified "foreign socket unspecified" self)))
	  (otherwise
	   (WHEN (NOT (EQ state :closed))
	     (FERROR 'connection-already-exists "connection already exists" self))
	   (WHEN (AND (NOT (ZEROP tcp-destination-port))
		      (NOT
			(AND (INTEGERP tcp-destination-port) (PLUSP tcp-destination-port)
			     (< tcp-destination-port 65536))))
	     (FERROR 'invalid-destination-port "connection illegal for this process" self))))
    ;1; perform source-port validation*
    (COND
      ((ZEROP tcp-source-port)
       (SETF tcp-source-port (generate-tcp-port)))
      ((NOT (AND (INTEGERP tcp-source-port) (PLUSP tcp-source-port) (< tcp-source-port 65536)))
       (FERROR 'invalid-source-port "connection illegal for this process" self))
      (t (reserve-tcp-port tcp-source-port)))
    ;1; security and precedence should be verified here*
    ;1; place connection object in table, if connection is unique*
    (WITHOUT-INTERRUPTS
      (COND
	((AND (NOT (ZEROP tcp-destination-address))
	      (NOT (ZEROP tcp-destination-port))
	      (SEND *tcp-handler* :find-tcb tcp-destination-port tcp-destination-address
		    tcp-source-port (closest-local-address tcp-destination-address)))
	 (unreserve-tcp-port tcp-source-port)
	 (FERROR 'connection-already-exists "connection illegal for this process" self))
	(t (SETF source-port tcp-source-port)
	   (WHEN (EQ mode :active)
	     (SETF source-address (closest-local-address tcp-destination-address)))
	   (SETF destination-port tcp-destination-port)
	   (SETF destination-address tcp-destination-address)
	   (SETF state (CASE mode
			     (:active :syn-sent)
			     (otherwise :listen)))
	   (SETF user-timeout tcp-timeout)
	   (SETF precedence tcp-precedence)
	   (SETF security tcp-security)
	   (SETF open-mode mode)
	   (SETF open-destination-port destination-port
		 open-destination-address destination-address)
	   (SEND *tcp-handler* :register-tcb self)
	   (SEND *tcp-handler* :scan-syn-list self))))
    ;1; send syn segment*
    (WHEN (EQ mode :active)
      (SETF initial-send-sequence-# (LOGAND (1- *tcp-full-seq-range*) (GET-UNIVERSAL-TIME)))
      (SETF send-unack initial-send-sequence-#)
      ;1; this differs from spec because send-syn increments send-next*
      (SETF send-next initial-send-sequence-#)
      (send-syn)))) 	   
    


(DEFMETHOD (tcp-connection :send) (data length push urgent &optional timeout)
  "2Perform TCP send operation (RFC793 p46, p56).
DATA ART-8B data buffer array.
LENGTH Length of data buffer in octets.
PUSH If non-nil, a push will be done.
URGENT If non-nil, data is urgent.
TIMEOUT (Optional) If specified, this is a new value for the 'user timeout'.
  The 'user timeout' is a timeout on the acknowlegment of data sent.*"
  (drive-connection-exclusively
    :norecursive
    (WHEN timeout (SETF user-timeout timeout))
    (CASE state
	  (:closed (FERROR 'connection-does-not-exist "connection does not exist" self))
	  (:listen
	   (COND
	     ((OR (ZEROP destination-port) (ZEROP destination-address))
	      (FERROR 'foreign-socket-unspecified "foreign socket unspecified" self))
	     (t (SETF open-mode :active) (SETF state :syn-sent)
		(SETF source-address (closest-local-address destination-address))
		(segmentize-buffer data length push urgent)
		(SETF initial-send-sequence-#
		      (LOGAND (1- *tcp-full-seq-range*) (GET-UNIVERSAL-TIME)))
		(SETF send-unack initial-send-sequence-#)
		;1; this differs from spec because send-syn increments send-next*
		(SETF send-next initial-send-sequence-#) (send-syn))))
	  ((:syn-sent :syn-received :established :close-wait)
	   (segmentize-buffer data length push urgent))
	  ((:fin-wait-1 :fin-wait-2 :closing :last-ack :time-wait)
	   (FERROR 'connection-closing "connection closing" self))))) 
  


(DEFMETHOD (tcp-connection :receive) (length &aux push-p urgent-p)
  "2Perform TCP receive operation (RFC793 p58).  LENGTH is length of data in octets.
The return values are: 
OCTET COUNT, PUSH FLAG, URGENT FLAG - If the data is present to satisfy the receive.
NIL - If the data is not present to satisfy the receive, the and the receive is queued.
  The receive data is returned when available by calling the user-supplied 'buffer-handler' function.
  The arguments to the buffer-handler function are: octet count, push flag, urgent flag.*"
  (drive-connection-exclusively
    :norecursive
    ;1; receive size is limited by the maximum defined window size in the tcp header.*
    (WHEN (> length #.(1- (EXPT 2 16)))
      (SETF length #.(1- (EXPT 2 16))))
    ;1; connection never opened*
    (WHEN (AND (EQ state :closed) (NULL source-port))
      (FERROR 'connection-does-not-exist "connection does not exist" self))
    (CASE state
	  ((:listen :syn-sent :syn-received)
	   (add-rd length)
	   nil)
	  ((:established :fin-wait-1 :fin-wait-2)
	   (add-rd length)
	   (scan-rd-q)
	   ;1; return receive information only if in order*
	   (WHEN (EQ rd-q rd-q-end)
	     (receive-match)))
	  ;1; deviation from spec in including closing, last-ack, time-wait, closed states here is necessitated by buffer handling*
	  ((:close-wait :closing :last-ack :time-wait :closed)
	   (add-rd length)
	   (scan-rd-q)
	   (COND
	     ;1; receive can be satisfied*
	     ((MULTIPLE-VALUE-SETQ (length push-p urgent-p) (receive-match))
	      (FUNCALL buffer-handler length push-p urgent-p)
	      ;1; pass fin to user (if in order)*
	      (WHEN (= return-sequence fin-sequence)
		(FUNCALL receive-fin-handler))
	      nil)
	     ;1; current receive cannot be satisfied*
	     ((EQ state :closed)
	      (FERROR 'connection-does-not-exist "connection does not exist" self))
	     (t (FERROR 'connection-closing "connection closing" self))))))) 



(DEFMETHOD (tcp-connection :close) (&aux segment)
  "2Close TCP connection (RFC793 p60).*"
  (drive-connection-exclusively
    :norecursive
    (CASE state
	  (:closed (FERROR 'connection-does-not-exist "connection does not exist" self))
	  ((:listen :syn-sent)
	   (COND
	     ((OR send-q rd-q) (FERROR 'connection-closing "connection closing" self))
	     (t (FUNCALL close-complete-handler)
		(SEND *tcp-handler* :delete-tcb self))))
	  (:syn-received
	   (COND
	     (send-q-end
	      (SETF (tcp-header-fin-p send-q-end) t))
	     (t (SETF segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
		(SETF (tcp-header-fin-p segment) t)
		(SETF (tcp-header-ack-p segment) t)
		(SETF (tcp-header-data-offset segment) 5)
		(queue segment send-q send-q-end)))
	   (WHEN (AND segment (NULL retransmit-q))
	     (SETF state :established))
	   ;1; process-send-q will make the transition into fin-wait-1 state*
	   )
	  (:established
	   (COND
	     (send-q-end (SETF (tcp-header-fin-p send-q-end) t))
	     (t (SETF segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
		(SETF (tcp-header-fin-p segment) t)
		(SETF (tcp-header-ack-p segment) t)
		(SETF (tcp-header-data-offset segment) 5)
		(queue segment send-q send-q-end)))
	   ;1; process-send-q will make the transition into fin-wait-1 state*
	   )
	  ((:fin-wait-1 :fin-wait-2))
	  (:close-wait
	   (COND
	     (send-q-end (SETF (tcp-header-fin-p send-q-end) t))
	     (t (SETF segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
		(SETF (tcp-header-fin-p segment) t)
		(SETF (tcp-header-ack-p segment) t)
		(SETF (tcp-header-data-offset segment) 5)
		(queue segment send-q send-q-end)))
	   ;1; process send-q will make the transition into last-ack state*
	   )
	  ((:closing :last-ack :time-wait)
	   (FERROR 'connection-closing "connection closing" self))))) 



(DEFMETHOD (tcp-connection :abort) ()
  "2Abort the TCP connection (RFC793 p62).*"
  (drive-connection-exclusively
    :norecursive
    (CASE state
	  (:closed (FERROR 'connection-does-not-exist "connection does not exist" self))
	  ((:listen :syn-sent :closing :last-ack :time-wait)
	   (FUNCALL close-complete-handler)
	   (SEND *tcp-handler* :delete-tcb self))
	  ((:syn-received :established :fin-wait-1 :fin-wait-2 :close-wait)
	   (tcp-send-reset source-address source-port destination-address destination-port
			   send-next)
	   (FUNCALL close-complete-handler)
	   (SEND *tcp-handler* :delete-tcb self))))) 
  


(DEFMETHOD (tcp-connection :status) ()
  "2Return the state, destination address, and destination port of the TCP connection (RFC793 p63).*"
  ;1; Warning: this method must not call drive-connection, since it is used in process-wait functions of TCP clients*
  (WITHOUT-INTERRUPTS
    ;1; Also, we do not follow the spec and signal an error in closed state, to allow use in process-wait*
    (VALUES state destination-port destination-address))) 



(DEFMETHOD (tcp-connection :get-octet) (&aux out)
  "2Return one octet from the data buffers. NIL is returned when no octet is available.*"
  (drive-connection-exclusively
    ;1; allow recursive locking*
    nil
    (CASE state
	  ((:listen :syn-sent :syn-received) nil)
	  (otherwise
	   ;1; insure data available*
	   (WHEN (NOT (= out-sequence return-sequence))
	     (SETF out (segment-index return-q))	     
	     (SETF out-sequence (+seq out-sequence 1))
	     (INCF (segment-index return-q))
	     (SETF (segment-sequence-# return-q) out-sequence)
	     (PROG1
	       (AREF return-q out)
	       (WHEN (= (segment-index return-q) (segment-size return-q))
		 (DEALLOCATE-RESOURCE 'tcp-segment
				      (PROG1
					return-q
					(SETF return-q (segment-link return-q))
					(IF return-q
					    (SETF (segment-back-link return-q) ())
					    (SETF return-q-end ())))))
	       ;1; connection is closed and all data is returned*
	       (CASE state
		     (:closed
		      (WHEN (= out-sequence fin-sequence)
			(FUNCALL close-complete-handler)
			(SEND *tcp-handler* :delete-tcb self)))
		     (:time-wait
		      (WHEN (= out-sequence fin-sequence)
			(FUNCALL close-complete-handler)
			(SEND *tcp-handler* :link-tcb-to-time-wait-list self)))))))))) 



(DEFMETHOD (tcp-connection :get-multiple-octets) (buffer length &aux (total-length 0))
  "2Copy up to length octets from the data buffers into an art-8b buffer.
The return value is the actual number of octets copied.*"
  (drive-connection-exclusively
    ;1; allow recursive locking*
    nil
    (CASE state
	  ((:listen :syn-sent :syn-received) 0)
	  (otherwise
	   (SETF length (MIN length (-seq return-sequence out-sequence)))
	   (DO ((buffer-index 0 (+ buffer-index copy-length))
		copy-length)
	       ((ZEROP length))
	     (SETF copy-length (MIN length (- (segment-size return-q) (segment-index return-q))))
	     (COPY-ARRAY-PORTION return-q (segment-index return-q)
				 (+ (segment-index return-q) copy-length) buffer buffer-index
				 (+ buffer-index copy-length))
	     (INCF total-length copy-length)
	     (DECF length copy-length)
	     (SETF out-sequence (+seq out-sequence copy-length))
	     (INCF (segment-index return-q) copy-length)
	     (SETF (segment-sequence-# return-q) out-sequence)
	     (WHEN (= (segment-index return-q) (segment-size return-q))
	       (DEALLOCATE-RESOURCE 'tcp-segment
				    (PROG1
				      return-q
				      (SETF return-q (segment-link return-q))
				      (IF return-q
					  (SETF (segment-back-link return-q) ())
					  (SETF return-q-end ()))))))
	   (PROG1
	     total-length
	     ;1; connection is closed and all data is returned*
	     (CASE state
		   (:closed
		    (WHEN (= out-sequence fin-sequence)
		      (FUNCALL close-complete-handler)
		      (SEND *tcp-handler* :delete-tcb self)))
		   (:time-wait
		    (WHEN (= out-sequence fin-sequence)
		      (FUNCALL close-complete-handler)
		      (SEND *tcp-handler* :link-tcb-to-time-wait-list self))))))))) 
    


(DEFUN add-to-server-list (port-number server-function process-name)
  "2Add a server to the TCP server association list.
PORT-NUMBER - Must be in the range 1..65535.
SERVER-FUNCTION - Top level server function.
PROCESS-NAME - String naming the server process.*"
  (PROG1
    (add-server port-number `(PROCESS-RUN-FUNCTION ,process-name ',server-function) '*tcp-server-alist*)
    (WHEN (TYPEP *tcp-handler* 'tcp-handler)
      (SEND *tcp-handler* :reserve-port port-number))))



(DEFUN add-server (name form list-name)
  "2Add a server to an IP server alist.
NAME is the port number.  This port number should be reserved.
FORM is the form to be executed to instantiate the server.
LIST-NAME is either '*tcp-server-alist* or '*udp-server-alist*.*"
  ;1; code borrowed from add initialization (with string-equal changed to equalp)*
  (DO ((l (SYMBOL-VALUE list-name) (CDR l)))
      ((NULL l)
       (COND ((NULL (SYMBOL-VALUE list-name))
	      (CAR (SET list-name (CONS (si:make-init-list-entry name form nil si:fdefine-file-pathname)
					(SYMBOL-VALUE list-name)))))
	     (t (CADR (RPLACD (LAST (SYMBOL-VALUE list-name))
			      (LIST (si:make-init-list-entry name form nil si:fdefine-file-pathname)))))))
    (WHEN (EQUALP (si:init-name (CAR l)) name)
      (SETF (si:init-form (CAR l)) form)
      (SETF (si:init-source-file (CAR l)) si:fdefine-file-pathname)
      (RETURN (CAR l)))))



(DEFUN delete-server (name list-name)
  "2Delete a server from an IP server alist.
NAME is the port number.  This port number should be unreserved following delete-server.
LIST-NAME is either '*tcp-server-alist* or '*udp-server-alist*.*"
  ;1; code borrowed from delete-initialization (with string-equal changed to equalp)*
  (DO ((l (SYMBOL-VALUE list-name) (CDR l))
       (flag nil))
      ((NULL l) flag)
    (WHEN (EQUALP (si:init-name (CAR l)) name)
      (SET list-name (DELETE (CAR l) (SYMBOL-VALUE list-name) :test #'EQ))
      (SETQ flag t))))



(DEFUN generate-tcp-port ()
  "2Generate and reserve unique non-well known TCP port number.  Return value is the port number.
Warning: It is the responsiblity of the caller of this function to unreserve the port number.*"
  (SEND *tcp-handler* :generate-port)) 



(DEFUN reserve-tcp-port (port-number)
  "2Increment use count on TCP port number.*"
  (SEND *tcp-handler* :reserve-port port-number)) 



(DEFUN unreserve-tcp-port (port-number)
  "2Decrement use count on TCP port number.*"
  (SEND *tcp-handler* :unreserve-port port-number)) 