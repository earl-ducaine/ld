;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

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

(DEFVAR *udp-handler* nil)


(DEFUN reset-udp-service (&optional (enable-p nil) (debug-p nil) &aux background-process)
  "2Reset all UDP data structures.
ENABLE-P (Optional) Enable UDP server when t (nil default).
DEBUG-P (Optional) Enable UDP debugging when t (nil default).*"
  (WHEN (TYPEP *udp-handler* 'udp-handler)
    (WITHOUT-INTERRUPTS
      (MAPC #'(lambda (entry)
		(SEND *udp-handler* :return-port (CDR entry)))
	    *udp-receive-list*)
      (SETF *udp-receive-list* ()))
    (WHEN (SETF background-process (SEND *udp-handler* :background-process))
      (SEND background-process :kill t)))
  (CLEAR-RESOURCE 'udp-packet nil nil)
  (CLEAR-RESOURCE 'udp-ports nil nil)
  (COND ((NOT enable-p)
	 (SETF *udp-handler* #'(lambda (operation &rest ignore)
				 (CASE operation
				   (:receive-data nil)
				   (otherwise
				    (FERROR 'udp-error "UDP Service is not enabled."))))))
	(t (SETF *udp-handler* (MAKE-INSTANCE (IF (TYPEP *udp-handler* 'udp-handler)
						  *udp-handler*
						  'udp-handler)
					      :background-process nil
					      :background-event nil
					      :background-requests nil
					      :packets-received 0
					      :checksum-errors 0
					      :server-packet-list nil
					      :server-packet-timeouts 0
					      :server-packet-claims 0
					      :nonexistant-ports 0))
	   (SETF (SEND *udp-handler* :background-process)
		 (PROCESS-RUN-FUNCTION '(:name "UDP Background" :priority 15)
				       *udp-handler* :run-function))
	   (SETF *udp-debug-mode* debug-p)))
  "UDP Reset Complete") 


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*

(DEFUN print-udp-header (header &optional (STREAM *standard-output*))
  (FORMAT stream "~& Source-Port:     ~16,4,'0r" (udp-header-source-port header))
  (FORMAT stream "   Destination-Port:~16,4,'0r" (udp-header-destination-port header))
  (FORMAT stream "   Header-Length:   ~16,4,'0r" (udp-header-length header))
  (FORMAT stream "   Header-Checksum: ~16,4,'0r" (udp-header-checksum header))) 


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP-UNIQUE-PORT-NUMBER-P examines the port number passed to it and returns T as the*
;1;; value of the function if the port number does not exist in the machine local world of*
;1;; UDP.  If it does exist, the value of the method is NIL.  This is done by checking the*
;1;; UDP receivers list for an entry with a car equal to the port number passed as input.*
;1;;*

(DEFSUBST udp-unique-port-number-p (port-number)
  (WITHOUT-INTERRUPTS
    (NOT (ASSOC port-number *udp-receive-list* :test #'EQUAL)))) 




;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; VALID-UDP-PORT-P determines if the value passed is a valid UDP port number.  In order*
;1;; to be considered valid, it must be an integer less than #x10000. *
;1;;*

(DEFSUBST valid-udp-port-p (port-number)
  (AND (INTEGERP port-number) (PLUSP port-number) (< port-number 65536))) 



(DEFFLAVOR udp-handler
	   (background-process
	    background-event
	    background-requests
	    packets-received
	    checksum-errors
	    server-packet-list
	    server-packet-timeouts
	    server-packet-claims
	    nonexistant-ports)
	   ()
  :settable-instance-variables)


(DEFMETHOD (udp-handler :run-function) (&aux tasks)
  "2Run function for tcp background process.*"
  (LOOP
    (PROCESS-WAIT "Background Task"
		  #'(lambda (self)
		      (OR
			(SEND self :background-requests)
			(AND (SEND self :background-event)
			     (NOT (TIME-LESSP (TIME) (SEND self :background-event))))))
		  self)
    (WITHOUT-INTERRUPTS
      (SETF tasks (NREVERSE background-requests))
      (SETF background-requests nil))
    (MAPC 'eval tasks)
    (WHEN (NOT (TIME-LESSP (TIME) (SEND self :background-event)))
      (process-udp-server-packet-list))))


(DEFUN process-udp-server-packet-list ()
  (DECLARE (:self-flavor udp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p server-packet-list (CDR p)))
	((OR (NULL p)
	     (TIME-LESSP (TIME)
			 (TIME-INCREMENT (udp-packet-time (FIRST p)) *udp-server-packet-timeout*)))
	 (SETF server-packet-list p))
      (INCF server-packet-timeouts))
    (SETF background-event (WHEN server-packet-list
			     (TIME-INCREMENT (udp-packet-time (FIRST server-packet-list))
					     *udp-server-packet-timeout*)))))


(DEFMETHOD (udp-handler :connection-pending-p) (destination-port)
  "2Predicate to determine if a pending packet has arrived that has destination port.*"
  (WITHOUT-INTERRUPTS
    (MEMBER destination-port server-packet-list
	    :test #'(lambda (destination-port packet)
		      (EQL destination-port (udp-header-destination-port packet))))))


(DEFMETHOD (udp-handler :receive-data) (ip-packet start length source-address destination-address
					&aux packet packet-checksum computed-checksum 
					port-number port server-entry)
  (DECLARE (inline copy-pkt))
  (LET* ((default-cons-area *ip-area*))
    (INCF packets-received)
    ;1; copy ip-packet to udp-packet*
    (SETF length (MIN *udp-maximum-packet-size* length))
    (SETF packet (ALLOCATE-RESOURCE 'udp-packet length
				    :source-address source-address
				    :destination-address destination-address))
    (SETF (udp-packet-time packet) (TIME))
    (copy-pkt ip-packet packet length start)
    (WHEN *udp-debug-mode*
      (FORMAT t "~&UDP Packet Received:")
      (print-udp-header packet))
    (SETF port-number (udp-header-destination-port packet))
    (WITHOUT-INTERRUPTS
      (SETF port (CDR (ASSOC port-number *udp-receive-list*))))
    (SETF packet-checksum (udp-header-checksum packet))
    (SETF (udp-header-checksum packet) 0)
    (SETF computed-checksum (IF (ZEROP packet-checksum)
				0
				(ip-ones-complement-checksum packet length 
							     destination-address source-address 
							     length *udp-protocol*)))
    (COND ((NOT (EQL packet-checksum computed-checksum))
	   (INCF checksum-errors)
	   (UNLESS (NULL port) (INCF (udp-port-checksum-errors port)))
	   (WHEN *udp-debug-mode*
	     (FORMAT t "~&UDP checksum error.  Packet = #x~16,4,'0r, Computed = #x~16,4,'0r."
		     packet-checksum computed-checksum))
	   (DEALLOCATE-RESOURCE 'udp-packet packet))
	  (t
	   (WHEN *udp-debug-mode* 
	     (tv:notify nil 
			"~&UPD-RECEIVE-DATA HANDLER: Port number = ~A  Port = ~A"
			port-number port))
	   (WITHOUT-INTERRUPTS
	     (COND ((NULL port)
		    (COND ((SETF server-entry
				 (ASSOC (udp-header-destination-port packet) *udp-server-alist* :test 'eql))
			   (PUSH (si:init-form server-entry) background-requests)
			   (WHEN (NOT background-event)
			     (SETF background-event (TIME-INCREMENT (TIME) *udp-server-packet-timeout*)))
			   (SETF server-packet-list (NREVERSE (CONS packet (NREVERSE server-packet-list)))))
			  (t
			   (DEALLOCATE-RESOURCE 'udp-packet packet)
			   (INCF nonexistant-ports))))
		   ((SEND port :received-packet)
		    (DEALLOCATE-RESOURCE 'udp-packet packet)
		    (INCF (udp-port-receive-overruns port)))
		   (t
		    (SETF (SEND port :received-packet) packet)
		    (SETF (SEND port :last-buffered-time) (time:get-universal-time))
		    (SETF (SEND port :sender-port) (udp-header-source-port packet))
		    (SETF (SEND port :sender-address) source-address)
		    (SETF (SEND port :receiver-address) destination-address)))))))) 	  

1*
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; :GET-PORT is called to have a UDP-PORT object generated and initialized.  An optional*
;1;; port number argument may be passed.  If it is, it is verified to be a valid UDP*
;1;; port number.  The PORT-NUMBER and HANDLER-OBJECT instance variables are set.  *
;1;; PORT-NUMBER is the number of the genereated port and HANDLER-OBJECT is the   *
;1;; UDP-HANDLER instance object used by the UDP-PORT routines to interface with the*
;1;; handler. *
;1;; *
;1;; The value of the method is the port object.*
;1;;*
;1;; FERRORS possible from this routine. PORT-IS-INVALID*
;1;;                                     PORT-IN-USE  *
;1;;*

(DEFMETHOD (udp-handler :get-port) (&optional (port-number 0)
				    (source-address 0)
				    (destination-port 0)
				    (destination-address 0)
				    &aux port)
  (LET* ((default-cons-area *ip-area*))
    ;1; If the port number is not specified, generate one.  Otherwise, be sure the specified*
    ;1; one is unique and will fit in two bytes (less than #X10000).*
    ;1;*
    (IF (ZEROP port-number)
	(SETF port-number (udp-get-port-number))
	(IF (valid-udp-port-p port-number)
	    (UNLESS (udp-unique-port-number-p port-number)
	      (FERROR 'port-in-use "The specified source port, ~a, is in use" port-number))
	    (FERROR 'port-is-invalid "The specified source port, ~a, is not a valid UDP port"
		    port-number)))
    (SETF port (ALLOCATE-RESOURCE 'udp-ports))
    (SETF (SEND port :port-number) port-number
	  (SEND port :who-state) "UDP Input"
	  (SEND port :sender-port) destination-port
	  (SEND port :sender-address) destination-address
	  (SEND port :receiver-address) (IF (ZEROP source-address)
					     (closest-local-address destination-address)
					     source-address))
    (WITHOUT-INTERRUPTS
      ;1; claim any waiting packets for server*
      (DO ((p server-packet-list (CDR p))
	   (q nil p))
	  ((NULL p))
	(WHEN (EQL (udp-header-destination-port (FIRST p)) port-number)
	  (SETF (SEND port :received-packet) (FIRST p))
	  (SETF (SEND port :last-buffered-time) (time:get-universal-time))
	  (SETF (SEND port :sender-port) (udp-header-source-port (FIRST p)))
	  (SETF (SEND port :sender-address) (udp-packet-source-address (FIRST p)))
	  (IF q
	      (RPLACD q (CDR p))
	      (SETF server-packet-list (CDR p)))
	  (INCF server-packet-claims)))
      (PUSH (CONS port-number port) *udp-receive-list*))
    (VALUES port))) 



(DEFMETHOD (udp-handler :return-port) (port &optional (dont-deregister-p nil))
  ;1; deallocate the port, such that it can be allocated again*
  (WHEN *udp-debug-mode*
    (FORMAT t "~&Deallocating port ~a" (SEND port :port-number)))
  (UNLESS dont-deregister-p
    (WITHOUT-INTERRUPTS
      (SETQ *udp-receive-list*
	    (delete (RASSOC port *udp-receive-list* :test #'EQUAL) (THE list *udp-receive-list*)
		    :test #'EQUAL))))
  (SETF (SEND port :reset-occured-p) t)
  (ignore-errors (DEALLOCATE-RESOURCE 'udp-ports port)) ;112-04-87 DAB Don't worry if has already been*
                                                        ;1deallocated (by a background server).*
  )


(COMPILE-FLAVOR-METHODS udp-handler)