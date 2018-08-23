;;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(MEDFNT HL12B TR12BI) -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986, 1988 Texas Instruments Incorporated.  All rights reserved.*

;1------*
;1 Switches which modify the behavior of the protocol*
;

(DEFPARAMETER *window-kludge-switch* t
   "2Modify window handling to communicate with implementations like which violate
the specification by not implementing the zero send window probe (RFC793 p42).
Warning:  Setting this switch will avoid the zero window probing logic on the remote host, and
may have performance impact or disallow testing of the remote window probing logic.*") 


(DEFPARAMETER *octet-sized-probe-switch* nil
   "2When this switch is t, zero send window probes are of text size one octet.
When this switch is nil, zero send window probes are the size of the entire pending segment text.
A one octet sized probe is always safe; a full segment probe will lead to higher performance provided
the remote implementation will not choke.*") 

;1------*
;1 Retransmission/delayed acknowledgment parameters*
;

(DEFPARAMETER *alpha* 0.8 "2Retransmission timeout smoothing factor.*") 


(DEFPARAMETER *beta* 2 "2Retransmission timeout delay variance.*") 


(DEFPARAMETER *ubound* (* 60 60) "2Upper bound on retransmission timeout in 1/60 seconds.*") 


(DEFPARAMETER *lbound* (* 1 60) "2Lower bound on retransmission timeout in 1/60 seconds.*") 


(DEFPARAMETER *initial-retransmission-timeout* (* 10 60)
   "2Retransmission timeout used for SYN segment.*") 


(DEFPARAMETER *delayed-ack-alpha* 0.8 "2Acknowledgement delay smoothing factor.*") 


(DEFPARAMETER *delayed-ack-beta* 2 "2Acknowledgement delay variance.*") 


(DEFPARAMETER *delayed-ack-fudge* 1
   "2Fudge factor increasing delayed ack beyond smoothed intersegment arrival time in 1/60 seconds.*")


(DEFPARAMETER *piggybacked-ack-fudge* 3
  "2Fudge factor used in awaiting new text to be sent or new receive window information.*")

;1------*
;1 other protocol parameters*
;
(DEFCONSTANT *tcp-protocol* 6  "2TCP protocol number*")


(DEFPARAMETER *max-tcp-ports* 256 "2Maximum allowed non-well known ports*")


(DEFCONSTANT *tcp-minimum-header-size* 20 "2Size of the TCP header information*")


(DEFPARAMETER *tcp-maximum-segment-size* *max-data-bytes*
   "2If remote host does not specify a maximum segment size option, this size will be used.
This value is also specified as the local maximum segment size option.*") 


(DEFPARAMETER *tcp-minimum-segment-size* 576
  "2When no maximum segment size is provided in the received SYN segment, this size will be infered.*")


(DEFCONSTANT *tcp-maximum-segment-lifetime* (* 2 60 60)
   "2Maximum time a segment should be in the network (1/60 second units).*")


(DEFPARAMETER *tcp-handler-syn-timeout* (* 30 60)
   "2SYN segment timeout for server response in 1/60 second units.*")


(DEFPARAMETER *tcp-dormant-probe-interval* (* 1 60 60)
  "2Interval between probes of dormant connections in 1/60 second units.
If this parameter is nil, no connection probing will be done.
When this parameter is modified, a (ip:reset-tcp-service t) must be done.*")


(DEFPARAMETER *tcp-dormant-connection-gc-threshold* 20
  "2Number of unaswered probes before connection is aborted.*")

;1------*
;1 global items*
;
(DEFVAR *tcp-handler* () "2TCP protocol state object*")


(DEFVAR *tcp-server-alist* ()
  "2Alist of (CONTACT-NAME FORM-TO-EVALUATE) for creating TCP servers.
   Entries are put on with ADD-INITIALIZATION, and removed with DELETE-INITIALIZATION.
   The form is evaluated in the background task when an incoming SYN matches the port number
   and there is no pending listening connection for it.*")

;1------*
;1 Sequence number manipulation macros*
;

(DEFCONSTANT *tcp-full-seq-range* #.(EXPT 2 32) "The size of the sequence number range") 


(DEFMACRO <seq (a b)
  "2Perform less than comparison modulo full-seq-range.*"
  `(LDB-TEST (BYTE 1 31) (- ,a ,b)))


(DEFMACRO >seq (a b)
  "2Perform greater than comparison modulo full-seq-range.*"
  `(<seq ,b ,a))


(DEFMACRO <=seq (a b)
  "2Perform less than or equal comparison modulo full-seq-range.*"
  `(NOT (>seq ,a ,b)))


(DEFMACRO >=seq (a b)
  "2Perform greater than or equal comparison modulo full-seq-range.*"
  `(<=seq ,b ,a))


(DEFMACRO maxseq (a b)
  "2Perform max function modulo full-seq-range.*"
  `(COND
     ((<seq ,a ,b) ,b)
     (t ,a))) 


(DEFMACRO minseq (a b)
  "2Perform min function modulo full-seq-range.*"
  `(COND
     ((>seq ,a ,b) ,b)
     (t ,a))) 


(DEFMACRO +seq (&rest a)
  "2Perform sequence number addition modulo full-seq-range.
May also be used to incrementdecrement a sequence number.*"
  `(LOGAND (1- *tcp-full-seq-range*) (+ . ,a))) 


(DEFMACRO -seq (a b)
  "2Compute sequence number difference modulo full-seq-range.
Warning: to decrement sequence numbers use (+seq sequence-number -decrement-value).*"
  `(LET ((pos-p (<=seq ,b ,a))
	 (diff (- ,a ,b)))
     (COND
       ((AND pos-p (MINUSP diff)) (+ diff *tcp-full-seq-range*))
       ((AND (NOT pos-p) (NOT (MINUSP diff))) (- diff *tcp-full-seq-range*))
       (t diff)))) 

;1------*
;1 Segment manipulation macros*
;

(DEFMACRO segment-size (segment)
  "2Location of the array size in the array leader.*"
  `(ARRAY-LEADER ,segment 0)) 


(DEFMACRO segment-index (segment)
  "2Location of the array index in the array leader.
For outbound segments this is the next octet to be filled.
For inbound segments this is the next octet to be consumed.*"
  `(ARRAY-LEADER ,segment 1)) 


(DEFMACRO segment-link (segment)
  "2Location of the queue link in the array leader.*"
  `(ARRAY-LEADER ,segment 2)) 


(DEFMACRO segment-back-link (segment)
  "2Location of the reverse queue link in the array leader (receive queues only).*"
  `(ARRAY-LEADER ,segment 3)) 


(DEFMACRO segment-time (segment)
  "2Time the segment was placed on the retransmission or receive queues.*"
  `(ARRAY-LEADER ,segment 4)) 


(DEFMACRO segment-source-address (segment)
  "2IP source address (receive queues only).*"
  `(ARRAY-LEADER ,segment 5)) 


(DEFMACRO segment-destination-address (segment)
  "2IP destination-address (receive queues only).*"
  `(ARRAY-LEADER ,segment 6))


(DEFMACRO segment-sequence-# (segment)
  "2Sequence number of this segment (retransmit queue and receive queues only).
Note that the tcp-header-sequence-# is not used with inbound segments.*"
  `(ARRAY-LEADER ,segment 7))


(DEFMACRO segment-ack-# (segment)
  "2Acknowlegement number of this segment (receive queues only).
Note that the tcp-header-ack-# is not used with inbound segments.*"
  `(ARRAY-LEADER ,segment 8))


(DEFMACRO segment-ack-p (segment)
  "2Substitute for tcp-header-ack-p (receive queues only).*"
  `(NOT (NULL (segment-ack-# ,segment))))


(DEFMACRO segment-retransmission-timeout (segment)
  "2Time of next retransmission for this segment (retransmit-queue only).*"
  `(segment-back-link ,segment)) 


(DEFMACRO segment-user-timeout (segment)
  "2Time of user timeout event for this segment (retransmit queue only).*"
  `(segment-source-address ,segment))


(DEFMACRO segment-sequence-length (segment)
  "2Sequence space of segment, includes text, syn, and fin (retransmit queue only).*"
  `(segment-ack-# ,segment))

;1------*
;1 Segment queue manipulation macros*
;

(DEFMACRO queue (segment q-head q-end)
  "2Add a segment to the specified queue.*"
  `(WITHOUT-INTERRUPTS (SETF (segment-link ,segment) ())
		       (IF ,q-head
			   (SETF (segment-link ,q-end) ,segment)
			   (SETF ,q-head ,segment))
		       (SETF ,q-end ,segment))) 		   


(DEFMACRO dequeue (q-head q-end)
  "2Remove the first segment from the specified queue.*"
  `(WITHOUT-INTERRUPTS
     (PROG1
       ,q-head
       (WHEN ,q-head
	 (SETF ,q-head (PROG1
			 (segment-link ,q-head)
			 (SETF (segment-link ,q-head) ())))
	 (WHEN (NOT ,q-head)
	   (SETF ,q-end ()))))))
;
;1 connection locking macros*
;
(DEFMACRO with-tcb-lock (tcb &body body)
  "2Insure all received segments are processed before releasing the connection lock.
Warning: this macro should only be used in functions which are NOT methods of tcp-connection.*"
  (LET ((pointer (GENSYM))
	(already-mine (GENSYM)))
    `(CONDITION-BIND ((system:network-error #'(lambda (condition tcb)
						(SEND tcb :error condition))
					    ,tcb))
       (LET* ((,pointer (SEND ,tcb :locf-lock))
	      (,already-mine (EQ (CAR ,pointer) current-process)))
	 (UNWIND-PROTECT
	     (PROGN
	       (IF ,already-mine
		   (FERROR 'tcp-error "Attempt to lock connection recursively.")
		   (UNLESS (SYS:%STORE-CONDITIONAL ,pointer nil current-process)
		     (PROCESS-LOCK ,pointer)))
	       (MULTIPLE-VALUE-PROG1
		 ;1; body may return multiple values*
		 (PROGN ,@body)
		 ;1; drive connection at least once before exit*
		 (DO (once)
		     ;1; atomically unlock only when receive-q is empty*
		     ((WITHOUT-INTERRUPTS
			(NOT (COND ((NOT (TYPEP ,tcb 'tcp-connection))
				    nil)
				   ((NOT once))
				   ((SEND ,tcb :receive-q))
				   (t (UNLESS ,already-mine
					(SYS:%STORE-CONDITIONAL ,pointer current-process nil))
				      nil)))))
		   (SEND ,tcb :drive-locked-connection)
		   (SETF once t))))
	   (UNLESS ,already-mine (SYS:%STORE-CONDITIONAL ,pointer current-process nil)))))))

  
(DEFMACRO drive-connection-exclusively (norecursive &body body)
  "2Insure all received segments are processed before releasing the connection lock.
Warning: this macro should only be used WITHIN methods of tcp-connection.*"
  (LET ((pointer (GENSYM))
	(already-mine (GENSYM)))
    `(CONDITION-BIND ((system:network-error #'(lambda (condition tcb)
						(SEND tcb :error condition))
					    self))
       (LET* ((,pointer (LOCF lock))
	      (,already-mine (EQ (CAR ,pointer) current-process)))
	 (UNWIND-PROTECT
	     (PROGN
	       (IF ,already-mine
		   ,(WHEN norecursive
			'(FERROR 'tcp-error "Attempt to lock connection recursively."))
		   (UNLESS (SYS:%STORE-CONDITIONAL ,pointer nil current-process)
		     (PROCESS-LOCK ,pointer)))
	       (MULTIPLE-VALUE-PROG1
		 ;1; body may return multiple values*
		 (PROGN ,@body)
		 ;1; drive connection at least once before exit (disallow recursive entry to drive connection)*
		 (UNLESS ,already-mine
		   (DO (once)
		       ;1; atomically unlock only when receive-q is empty*
		       ((AND once
			     (NOT (WITHOUT-INTERRUPTS
				    (COND (receive-q)
					  (t (UNLESS ,already-mine
					       (SYS:%STORE-CONDITIONAL ,pointer current-process nil))
					     nil))))))
		     (drive-connection)
		     (SETF once t)))))
	   (UNLESS ,already-mine (SYS:%STORE-CONDITIONAL ,pointer current-process nil)))))))


;
;1 TCP errors*
;

(DEFFLAVOR tcp-error
	   ()
	   (system:network-error)) 


(DEFSIGNAL connection-does-not-exist tcp-error ()
   "Operation attempted to non-existant connection") 
 

(DEFSIGNAL connection-already-exists tcp-error ()
   "OPEN operation has already been performed on this connection.") 


(DEFSIGNAL security-or-compartment-not-allowed tcp-error ()
   "caller does not have the correct security level to use this channel") 


(DEFSIGNAL precedence-not-allowed tcp-error ()
   "caller does not have the correct precedence level to use this channel") 


(DEFSIGNAL connection-reset tcp-error ()
   "connection has been reset") 


(DEFSIGNAL connection-closing tcp-error ()
   "connection is closing") 


(DEFSIGNAL connection-refused tcp-error ()
   "connection has been refused") 


(DEFSIGNAL foreign-socket-unspecified tcp-error ()
   "destination address andor port is not defined") 


(DEFSIGNAL user-timeout tcp-error ()
   "user timeout has ocurred")


(DEFSIGNAL host-stopped-responding
   (tcp-error host-stopped-responding host-not-responding sys:connection-error) (connection)
   "The foreign host stopped responding while we were connected.") 

;1------*
;1 the following are all special cases fo the 'illegal connection' condition*
;

(DEFSIGNAL illegal-connection tcp-error ()
   "connection illegal for this process") 


(DEFSIGNAL invalid-destination-port (tcp-error illegal-connection invalid-destination-port) ()
   "invalid destination port") 


(DEFSIGNAL invalid-destination-address
   (tcp-error illegal-connection invalid-destination-address) ()
   "invalid destination address") 


(DEFSIGNAL invalid-source-port (tcp-error illegal-connection invalid-source-port) ()
   "invalid source port") 


(DEFSIGNAL access-denied (tcp-error illegal-connection access-denied) ()
   "access denied") 


;
;1 tcp-connection flavor (Note: all times and timeouts are 1/60 second units, except user timeout)*
;

(DEFFLAVOR tcp-connection
	   (retransmit-link
	    time-wait-link
	    delayed-ack-link
	    port-link			   ;1 link to next TCB with same local port but different destination port/address*
	    lock			   ;1 connection lock*
					   ;1 WARNING: This lock must only be manipulated with either*
	                                   ;1 with-tcb-lock, drive-connection-exclusively,*
	                                   ;1 or (:method tcp-connection :external-drive-connection).*
	                                   ;1 Any other method will compromise segment reception.*

	    state
	    source-address
	    source-port
	    destination-address
	    destination-port
	    security
	    precedence
	    user-timeout		   ;1 maximum seconds for data to be acknowledged*
	    open-mode
	    open-destination-port	   ;1 used for listening connections only*
	    open-destination-address	   ;1 used for listening connections only*

	    send-q
	    send-q-end
	    send-unack   ;1 ref RFC793 p19*
	    send-next
	    send-window
	    send-wl1
	    send-wl2
	    window-probe-active-p	   ;1 zero send window probing in progress*
	    initial-send-sequence-#
	    maximum-send-size		   ;1 determined by remote peer*
	    maximum-text-sent
	    total-text-sent
	    number-of-text-segments-sent
	    number-of-segments-sent

	    retransmit-q
	    retransmit-q-end
	    retransmit-timestamp	   ;1 time of next retransmission*
	    retransmission-timeout	   ;1 interval between retransmissions*
	    smoothed-round-trip-time
	    number-of-retransmissions

	    last-receive-time		   ;1 time of last reception*

	    receive-q			   ;1 received segments waiting to be processed*
	    receive-q-end
	    received-segment		   ;1 segment currently being processed*
	    out-of-order-q		   ;1 segments held for in order processing*
	    out-of-order-q-end
	    return-q			   ;1 segments being returned to user*
	    return-q-end
	    rd-q			   ;1 receive descriptors*
	    rd-q-end
	    rd-registered-length	   ;1 sum length of registered receive descriptors*
	    initial-receive-sequence-#	   ;1 ref RFC793 p19*
	    maximum-receive-size
	    receive-urgent-pointer
	    receive-next
	    receive-window
	    remote-probe-segment	   ;1 segment used by remote host as a zero receive window probe*

	    fin-sequence		   ;1 sequence number of fin*
	    out-sequence		   ;1 sequence number of available user data*
	    return-sequence		   ;1 sequence number of unreturned user data*
	    latest-leading-edge		   ;1 most recent window information sent in ack segment*
	    latest-trailing-edge
	    not-push-time-p		   ;1 time last segment arrived (if not push segment)*
	    smoothed-intersegment-arrival-time
	    delayed-ack-timestamp	   ;1 time delayed ack requested*
	    ack-due-p			   ;1 piggyback ack due*
	    time-wait-timestamp		   ;1 time time-wait state expires*
	    segments-out-of-sequence
	    segments-rejected
	    maximum-text-received
	    total-text-received
	    number-of-text-segments-received
	    number-of-segments-received

	    buffer-handler		   ;1 user function to handle return buffers*
	    urgent-handler		   ;1 user function to handle entering urgent mode event*
            process-fin-handler		   ;1 user function to handle fin arrival*
	    receive-fin-handler		   ;1 user function to handle fin reception in sequence number order*
	    close-complete-handler	   ;1 user function to handle completion of close/abort request*
	    user-timeout-handler	   ;1 user function to handle user timeout condition*
	    condition-handler)		   ;1 user function to handle TCP error conditions*
	   ()
  :settable-instance-variables
  (:documentation
    "Transport Control Protocol connection object (sometimes referred to as TCB)"
    )) 

;1------*
;1 Resource of TCP connection objects (TCBS)*
;
(DEFRESOURCE tcbs (buffer-handler urgent-handler process-fin-handler receive-fin-handler
				  close-complete-handler user-timeout-handler condition-handler)
  :constructor (MAKE-INSTANCE 'tcp-connection)
  :initializer (SEND object :initialize buffer-handler urgent-handler process-fin-handler receive-fin-handler
		     close-complete-handler user-timeout-handler condition-handler)
  :matcher t
  :checker (AND (NOT in-use-p) (NULL (SEND object :lock)))) 



;
;1 receive descriptor resource*
;
;1 link               - :receive call issued next in time order*
;1 state             - :pending, not represented in receive window*
;1                     :registered, represented by a hole (or holes) in ring buffer*
;1 length            - number of octets requested*
;

(DEFSTRUCT (rd (:conc-name rd-) (:callable-constructors nil) (:alterant alter-rd) (:predicate nil)
  (:copier nil) (:type :array))
  link
  state
  length) 


(DEFRESOURCE tcp-rd-resource (LENGTH)
  :constructor (make-rd)
  :initializer (PROGN
		 (SETF (rd-link object) ())
		 (SETF (rd-state object) :pending)
		 (SETF (rd-length object) length))
  :matcher (PROGN object)) 
;
;1 displaced array resource - used when resegmentizing*
;
(DEFRESOURCE tcp-displaced-array (segment)
  :constructor  (LOCALLY
		  (DECLARE (SPECIAL *ip-area*))
		  (MAKE-ARRAY (- (segment-index segment) *tcp-minimum-header-size*)
			      :displaced-to segment
			      :displaced-index-offset *tcp-minimum-header-size*
			      :element-type '(unsigned-byte 8)
			      :area *ip-area*))
  :initializer  (si:change-indirect-array object (array-type object)
					  (- (segment-index segment) *tcp-minimum-header-size*)
					  segment *tcp-minimum-header-size*)
  :matcher (PROGN object)) 


;
;1 tcp-header structure - since the header is declared as art-8b, multiple octet fields require special processing*
;

(DEFSTRUCT (tcp-header (:type :array '(MOD 256)) (:conc-name tcp-header-) (:callable-constructors nil)
		       (:alterant alter-tcp-header) (:predicate nil) (:copier nil))
  source-port-msb
  source-port-lsb
  destination-port-msb
  destination-port-lsb
  sequence-#-msb
  sequence-#-2nd-sb
  sequence-#-3rd-sb
  sequence-#-lsb
  ack-#-msb
  ack-#-2nd-sb
  ack-#-3rd-sb
  ack-#-lsb
  ((data-offset #o0404)
   (reserve-1 #o0004))
  ((reserve-2 #o0602)
   (urgent-flag #o0501)
   (ack-flag #o0401)
   (push-flag #o0301)
   (reset-flag #o0201)
   (syn-flag #o0101)
   (fin-flag #o0001))
  window-msb
  window-lsb
  checksum-msb
  checksum-lsb
  urgent-pointer-msb
  urgent-pointer-lsb
  options) 


(DEFCONSTANT options-position (THIRD (MACROEXPAND-1 '(tcp-header-options dummy)))
   "2Array positon of TCP-HEADER-OPTIONS*") 
;
;1 the following functions facilitate obtaining the value of multiple octet and flag fields in the tcp-header*
;

(DEFMACRO tcp-header-source-port (ARRAY)
  `(DPB (tcp-header-source-port-msb ,array) (BYTE 8 8) (tcp-header-source-port-lsb ,array))) 


(DEFMACRO tcp-header-destination-port (ARRAY)
  `(DPB (tcp-header-destination-port-msb ,array) (BYTE 8 8) (tcp-header-destination-port-lsb ,array))) 


;1; WARNING: for inbound segments use segment-sequence-# rather than tcp-header-sequence-#*
(DEFMACRO tcp-header-sequence-# (ARRAY)
  `(DPB (tcp-header-sequence-#-msb ,array) (BYTE 8 24)
	(DPB (tcp-header-sequence-#-2nd-sb ,array) (BYTE 8 16)
	     (DPB (tcp-header-sequence-#-3rd-sb ,array) (BYTE 8 8)
		  (tcp-header-sequence-#-lsb ,array))))) 


;1; WARNING: for inbound segments use segment-ack-# rather than tcp-header-ack -#*
(DEFMACRO tcp-header-ack-# (ARRAY)
  `(DPB (tcp-header-ack-#-msb ,array) (BYTE 8 24)
	(DPB (tcp-header-ack-#-2nd-sb ,array) (BYTE 8 16)
	     (DPB (tcp-header-ack-#-3rd-sb ,array) (BYTE 8 8) (tcp-header-ack-#-lsb ,array))))) 


(DEFMACRO tcp-header-urgent-p (ARRAY)
  `(= (tcp-header-urgent-flag ,array) 1)) 


;1; WARNING: for inbound segments use segment-ack-p rather than tcp-header-ack-p*
(DEFMACRO tcp-header-ack-p (ARRAY)
  `(= (tcp-header-ack-flag ,array) 1)) 


(DEFMACRO tcp-header-push-p (ARRAY)
  `(= (tcp-header-push-flag ,array) 1)) 


(DEFMACRO tcp-header-reset-p (ARRAY)
  `(= (tcp-header-reset-flag ,array) 1)) 


(DEFMACRO tcp-header-syn-p (ARRAY)
  `(= (tcp-header-syn-flag ,array) 1)) 


(DEFMACRO tcp-header-fin-p (ARRAY)
  `(= (tcp-header-fin-flag ,array) 1)) 


(DEFMACRO tcp-header-window (ARRAY)
  `(DPB (tcp-header-window-msb ,array) (BYTE 8 8) (tcp-header-window-lsb ,array))) 


(DEFMACRO tcp-header-checksum (ARRAY)
  `(DPB (tcp-header-checksum-msb ,array) (BYTE 8 8) (tcp-header-checksum-lsb ,array))) 


(DEFMACRO tcp-header-urgent-pointer (ARRAY)
  `(DPB (tcp-header-urgent-pointer-msb ,array) (BYTE 8 8) (tcp-header-urgent-pointer-lsb ,array))) 


;
;1 the following macros facilitate setf'ing multiple octet and flag fields in the tcp-header*
;

(DEFSETF tcp-header-source-port (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-source-port-msb ,array) (ASH ,value -8))
     (SETF (tcp-header-source-port-lsb ,array) ,value))) 


(DEFSETF tcp-header-destination-port (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-destination-port-msb ,array) (ASH ,value -8))
     (SETF (tcp-header-destination-port-lsb ,array) ,value))) 


(DEFSETF tcp-header-sequence-# (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-sequence-#-msb ,array) (ASH ,value -24))
     (SETF (tcp-header-sequence-#-2nd-sb ,array) (ASH ,value -16))
     (SETF (tcp-header-sequence-#-3rd-sb ,array) (ASH ,value -8))
     (SETF (tcp-header-sequence-#-lsb ,array) (LOGAND 255 ,value))
     ,value)) 


(DEFSETF tcp-header-ack-# (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-ack-#-msb ,array) (ASH ,value -24))
     (SETF (tcp-header-ack-#-2nd-sb ,array) (ASH ,value -16))
     (SETF (tcp-header-ack-#-3rd-sb ,array) (ASH ,value -8))
     (SETF (tcp-header-ack-#-lsb ,array) (LOGAND 255 ,value))
     ,value)) 


(DEFSETF tcp-header-urgent-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-urgent-flag ,array) (IF ,value 1 0))
     ,value)) 


(DEFSETF tcp-header-ack-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-ack-flag ,array) (IF ,value 1 0))
     ,value)) 
  

(DEFSETF tcp-header-push-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-push-flag ,array) (IF ,value 1 0))
     ,value)) 
  

(DEFSETF tcp-header-reset-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-reset-flag ,array) (IF ,value 1 0))
     ,value)) 


(DEFSETF tcp-header-syn-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-syn-flag ,array) (IF ,value 1 0))
     ,value)) 


(DEFSETF tcp-header-fin-p (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-fin-flag ,array) (IF ,value 1 0))
     ,value)) 


(DEFSETF tcp-header-window (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-window-msb ,array) (ASH ,value -8))
     (SETF (tcp-header-window-lsb ,array) ,value))) 


(PROCLAIM '(inline set-tcp-header-checksum))
(DEFUN set-tcp-header-checksum (ARRAY value)
  (SETF (tcp-header-checksum-msb array) (ASH value -8))
  (SETF (tcp-header-checksum-lsb array) value))

(DEFSETF tcp-header-checksum set-tcp-header-checksum) 


(DEFSETF tcp-header-urgent-pointer (ARRAY) (value)
  `(PROGN
     (SETF (tcp-header-urgent-pointer-msb ,array) (ASH ,value -8))
     (SETF (tcp-header-urgent-pointer-lsb ,array) ,value))) 
;
;1 tcp-segment resource*
;

(DEFRESOURCE tcp-segment (length)
  :constructor
  (LOCALLY
    (DECLARE (SPECIAL *ip-area*))
    (MAKE-ARRAY *tcp-maximum-segment-size* :element-type '(unsigned-byte 8) :area *ip-area*
		:leader-length 9))
  :matcher (PROGN object)
  :initializer
  (PROGN
    (WHEN (> length *tcp-maximum-segment-size*)
      (DEALLOCATE-RESOURCE 'tcp-segment object)
      (FERROR 'tcp-error "segment too large"))
    (SETF (segment-size object) length)
    (SETF (segment-index object) *tcp-minimum-header-size*)
    (SETF (segment-link object) ())
    (SETF (segment-back-link object) ())
    (SETF (segment-time object) ())
    (SETF (segment-source-address object) ())
    (SETF (segment-destination-address object) ())
    (SETF (segment-sequence-# object) ())
    (SETF (segment-ack-# object) ())
    (DO ((i 0 (1+ i)))
	((= i *tcp-minimum-header-size*))
      (SETF (AREF object i) 0))
    (SETF (tcp-header-data-offset object) 5))) 