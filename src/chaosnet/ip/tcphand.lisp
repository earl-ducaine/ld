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

;
;1 tcp handler*
;1 *

(DEFFLAVOR tcp-handler
	   (background-process
	    background-event
	    probe-event
	    retransmit-list-event
	    retransmit-list
	    delayed-ack-list-event   
	    delayed-ack-list
	    time-wait-list-event   
	    time-wait-list
	    syn-list
	    background-requests
	    tcb-table
	    port-table
	    segments-received
	    checksum-errors
	    nonexistant-connections
	    ports-currently-reserved
	    retransmissions
	    delayed-acks
	    idle-probes
	    syn-listings
	    syn-timeouts
	    syn-claims)
	   ()
  :settable-instance-variables) 



(DEFUN reset-tcp-service (&optional (enable-p nil) &aux background-process)
  "2Reset all TCP data structures.
ENABLE-P (Optional) Enable TCP service when t (nil default).*"
  (DECLARE (SPECIAL *ip-area*))
  ;1; disable tcp*
  (WHEN (TYPEP *tcp-handler* 'tcp-handler)
    (MAPHASH
      #'(lambda (IGNORE tcb)
	  (DO ((p tcb q)
	       q)
	      ((NULL p))
	    (SETF q (SEND p :port-link))
	    (SEND *tcp-handler* :delete-tcb p :dont-deregister)))
      (SEND *tcp-handler* :tcb-table))
    ;1; nail the background process and make sure its unwinds complete*
    (WHEN (SETF background-process (SEND *tcp-handler* :background-process))
      (SEND background-process :kill t)))
  (CLEAR-RESOURCE 'tcbs nil nil)
  (CLEAR-RESOURCE 'tcp-segment nil nil)
  (CLEAR-RESOURCE 'tcp-stream-binary-segment nil nil)
  (CLEAR-RESOURCE 'tcp-stream-ascii-buffer nil nil)
  (COND ((NOT enable-p)
	 (SETF *tcp-handler* #'(lambda (operation &rest ignore)
				 (CASE operation
				   (:receive-data nil)
				   (otherwise
				    (FERROR 'tcp-error "TCP Service is not enabled."))))))
	(t (SETF *tcp-handler*
		 (MAKE-INSTANCE (IF (TYPEP *tcp-handler* 'tcp-handler)
				    *tcp-handler*
				    'tcp-handler)
				:background-process ()
				:background-event ()
				:probe-event (WHEN  *tcp-dormant-probe-interval*
					       (TIME-INCREMENT (TIME) *tcp-dormant-probe-interval*))
				:retransmit-list-event () :retransmit-list ()
				:delayed-ack-list-event () :delayed-ack-list ()
				:time-wait-list-event () :time-wait-list ()
				:syn-list ()
				:background-requests ()
				:tcb-table
				(MAKE-HASH-TABLE :compare-function #'EQ
						 :hash-function ()
						 :size 123)
				:port-table
				(MAKE-ARRAY *max-tcp-ports*
					    :element-type 'integer
					    :initial-element 0
					    :area *ip-area*)
				:segments-received 0
				:checksum-errors 0 :nonexistant-connections 0
				:ports-currently-reserved 0 :retransmissions 0 :delayed-acks 0
				:idle-probes 0 :syn-listings 0 :syn-timeouts 0 :syn-claims 0))
	   (DOLIST (server-entry *tcp-server-alist*)
	     (SEND *tcp-handler* :reserve-port (FIRST server-entry)))
	   (SETF (SEND *tcp-handler* :background-process)
		 (PROCESS-RUN-FUNCTION '(:name "TCP Background" :priority 15)
				       *tcp-handler* :run-function))))
  "TCP Reset Complete")



(DEFMETHOD (tcp-handler :run-function) (&aux tasks)
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
    (WHEN (WITHOUT-INTERRUPTS
	    (AND retransmit-list-event (NOT (TIME-LESSP (TIME) retransmit-list-event))))
      (process-retransmit-list))
    (WHEN (WITHOUT-INTERRUPTS
	    (AND delayed-ack-list-event (NOT (TIME-LESSP (TIME) delayed-ack-list-event))))
      (process-delayed-ack-list))	  
    (WHEN (WITHOUT-INTERRUPTS
	    (AND time-wait-list-event (NOT (TIME-LESSP (TIME) time-wait-list-event))))
      (process-time-wait-list))
    (WHEN (WITHOUT-INTERRUPTS
	    (AND syn-list
		 (NOT
		   (TIME-LESSP (TIME) (TIME-INCREMENT (segment-time syn-list) *tcp-handler-syn-timeout*)))))
      (process-syn-list))
    (WHEN (NOT (TIME-LESSP (TIME) probe-event))
      (probe-connections)))) 



(DEFUN calculate-background-event ()
  "2Calculate the time of the next event occurence in the background process.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (SETF background-event probe-event)
    (WHEN (AND syn-list (TIME-LESSP background-event
				    (TIME-INCREMENT (segment-time syn-list) *tcp-handler-syn-timeout*)))
       (SETF background-event (TIME-INCREMENT (segment-time syn-list) *tcp-handler-syn-timeout*)))
    (WHEN (AND retransmit-list-event (TIME-LESSP retransmit-list-event background-event))
       (SETF background-event retransmit-list-event))
    (WHEN (AND delayed-ack-list-event (TIME-LESSP delayed-ack-list-event background-event))
       (SETF background-event delayed-ack-list-event))
    (WHEN (AND time-wait-list-event (TIME-LESSP time-wait-list-event background-event))
       (SETF background-event time-wait-list-event)))) 


;
;1 syn segment management*
;
(DEFUN link-to-syn-list (syn-segment server-form)
  "2Add a syn segment to the syn list, duplicate syns are deleted.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p syn-list (segment-link p))
	 (q nil p))
	((NULL p)
	 (INCF syn-listings)
	 (SETF (segment-link syn-segment) ())
	 (IF (NULL q)
	     (SETF syn-list syn-segment)
	     (SETF (segment-link q) syn-segment))
	 (WITHOUT-INTERRUPTS
	     (PUSH server-form background-requests)))
      (WHEN (AND (EQL (segment-source-address syn-segment) (segment-source-address p))
		 (EQL (tcp-header-source-port syn-segment) (tcp-header-source-port p))
		 (EQL (segment-destination-address syn-segment) (segment-destination-address p))
		 (EQL (tcp-header-destination-port syn-segment) (tcp-header-destination-port p)))
	(DEALLOCATE-RESOURCE 'tcp-segment syn-segment)
	(RETURN)))
    (calculate-background-event))) 



(DEFUN process-syn-list (&aux old-syns)
  "2Remove SYN segments from the syn-list which have not been claimed in *tcp-handler-syn-timeout* 1/60 second units.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p syn-list (segment-link p))
	 (q nil p))
	((NULL p))
      (WHEN (NOT (TIME-LESSP (TIME) (TIME-INCREMENT (segment-time p) *tcp-handler-syn-timeout*)))
	(IF (NULL q)
	    (SETF syn-list (segment-link p))
	    (SETF (segment-link q) (segment-link p)))
	(SETF (segment-link p) old-syns)
	(SETF old-syns p)))
    (calculate-background-event))
  ;1; return a reset segment to the remote end*
  (DO ()
      ((NULL old-syns))
    (INCF syn-timeouts)
    (UNLESS (tcp-header-reset-p old-syns)
      (COND
	((segment-ack-p old-syns)
	 (tcp-send-reset (closest-local-address (segment-source-address old-syns))
			 (tcp-header-destination-port old-syns) (segment-source-address old-syns)
			 (tcp-header-source-port old-syns) (segment-ack-# old-syns)))
	(t
	 (tcp-send-reset (closest-local-address (segment-source-address old-syns))
			 (tcp-header-destination-port old-syns) (segment-source-address old-syns)
			 (tcp-header-source-port old-syns) 0
			 (+seq (segment-sequence-# old-syns) (segment-index old-syns))))))   
    (DEALLOCATE-RESOURCE 'tcp-segment (PROG1 old-syns (SETF old-syns (segment-link old-syns))))))



(DEFMETHOD (tcp-handler :scan-syn-list) (tcb)
  "2Scan the syn-list looking for a SYN segment that matches current-process and matches tcb, 
and queue the segment to the tcb if one is found.*"
  (WITHOUT-INTERRUPTS
    (DO ((p syn-list (segment-link p))
	 (q nil p))
	((COND
	   ((NULL p))
	   ((AND (EQL (SEND tcb :source-port) (tcp-header-destination-port p))
		 (OR (AND (EQL (SEND tcb :destination-address) (segment-source-address p))
			  (EQL (SEND tcb :destination-port) (tcp-header-source-port p)))
		     (AND (EQL (SEND tcb :destination-address) (segment-source-address p))
			  (ZEROP (SEND tcb :destination-port)))
		     (AND (ZEROP (SEND tcb :destination-address))
			  (EQL (SEND tcb :destination-port) (tcp-header-source-port p)))
		     (AND (ZEROP (SEND tcb :destination-address))
			  (ZEROP (SEND tcb :destination-port)))))
	    (IF (NULL q)
		(SETF syn-list (segment-link p))
		(SETF (segment-link q) (segment-link p)))
	    (SETF (SEND tcb :last-receive-time) (TIME))
	    (queue p (SEND tcb :receive-q) (SEND tcb :receive-q-end))
	    (INCF syn-claims)
	    t))))
    (calculate-background-event)))



(DEFMETHOD (tcp-handler :connection-pending-p) (destination-port &optional (source-port 0) (source-address 0))
  "2Predicate to determine if a pending syn segment has arrived that has 
destination port, source port, and source address that match the arguments.*"
  (WITHOUT-INTERRUPTS
    (DO ((p syn-list (segment-link p)))
	(nil)
      (COND ((NULL p) (RETURN nil))
	    ((AND (EQL destination-port (tcp-header-destination-port p))
		  (OR (AND (EQL source-address (segment-source-address p))
			   (EQL source-port (tcp-header-source-port p)))
		      (AND (EQL source-address (segment-source-address p))
			   (ZEROP source-port))
		      (AND (ZEROP source-address)
			   (EQL source-port (tcp-header-source-port p)))
		      (AND (ZEROP source-address)
			   (ZEROP source-port)))) 
	     (RETURN t))))))


;
;1 packet reception*
;
(PROCLAIM '(inline tcp-copy-packet-to-segment))
(DEFUN tcp-copy-packet-to-segment (from-pkt to-segment nbytes &optional (from-offset 0))
  "2Copy from an inbound IP packet to a TCP segment.*"
  (LET ((si:inhibit-scavenging-flag t))
    (WITHOUT-INTERRUPTS
      (SYS:%BLT
	(SYS:%MAKE-POINTER-OFFSET si:dtp-fix from-pkt
				  (+ (CEILING from-offset 2) (si:array-data-offset from-pkt)))
	(SYS:%MAKE-POINTER-OFFSET si:dtp-fix to-segment (si:array-data-offset to-segment))
	(CEILING nbytes 4)
	1))))



(DEFMETHOD (tcp-handler :receive-data) (ip-packet start length source-address destination-address
					&aux tcb checksum server-entry segment)
  "2Perform initial receive processing, called directly from IP.
SEGMENT Inbound TCP segment.
LENGTH Includes header and data.
SOURCE-ADDRESS Sending machine.
DESTINATION-ADDRESS Address used for local machine.*"
  (BLOCK receive
    (INCF segments-received)
    ;1; copy into segment*
    (SETF length (MIN length *tcp-maximum-segment-size*))
    (SETF segment (ALLOCATE-RESOURCE 'tcp-segment length))
    (tcp-copy-packet-to-segment ip-packet segment length start)
    ;1; Perform checksum validation*
    (SETF checksum (tcp-header-checksum segment))
    (SETF (tcp-header-checksum segment) 0)
    (WHEN (NOT (= checksum
		  (ip-ones-complement-checksum segment length destination-address source-address
					       *tcp-protocol* length)))
      (INCF checksum-errors)
      (DEALLOCATE-RESOURCE 'tcp-segment segment)
      (RETURN-FROM receive))
    ;1; setup array leader*
    (SETF (segment-source-address segment) source-address)
    (SETF (segment-destination-address segment) destination-address)
    (SETF (segment-index segment) (* 4 (tcp-header-data-offset segment)))
    (SETF (segment-time segment) (TIME))
    (SETF (segment-sequence-# segment) (tcp-header-sequence-# segment))
    (SETF (segment-ack-# segment) (WHEN (tcp-header-ack-p segment) (tcp-header-ack-# segment)))
    ;1; Perform TCB lookup and queueing atomically*
    (WITHOUT-INTERRUPTS
      (SETF tcb (SEND self :find-tcb (tcp-header-source-port segment) source-address
		      (tcp-header-destination-port segment) destination-address
		      :syn-segment-p (tcp-header-syn-p segment)))
      (WHEN tcb
	(COND ((EQ (SEND tcb :state) :closed)
	       (SETF tcb nil))
	      (t (queue segment (SEND tcb :receive-q) (SEND tcb :receive-q-end))
		 (SETF (SEND tcb :last-receive-time) (TIME))))))
    (COND (tcb
	   ;1; be sure to not block waiting on connection lock, screen out connection-oriented errors*
	   (CONDITION-CASE ()
	       (SEND tcb :external-drive-connection nil)
	     (system:network-error)))
	  ;1; signal the background task to spawn a server*
	  ((AND (NOT tcb)
		(tcp-header-syn-p segment)
		(WITHOUT-INTERRUPTS
		  ;1; the server port is in the car of an init list entry*
		  (SETF server-entry
			(ASSOC (tcp-header-destination-port segment) *tcp-server-alist* :test 'eql)))
		(access-permitted source-address))
	   (link-to-syn-list segment (si:init-form server-entry)))
	  ;1; Process inbound segment for closed or non-existant connection (RFC793 p65).  *
	  (t
	   (INCF nonexistant-connections)
	   (UNLESS (tcp-header-reset-p segment)
	     (COND
	       ((tcp-header-ack-p segment)
		(tcp-send-reset destination-address (tcp-header-destination-port segment)
				source-address (tcp-header-source-port segment)
				(tcp-header-ack-# segment)))
	       (t
		(tcp-send-reset destination-address (tcp-header-destination-port segment)
				source-address (tcp-header-source-port segment) 0
				(+seq (tcp-header-sequence-# segment) (tcp-header-syn-flag segment)
				      (tcp-header-fin-flag segment)
				      (- length (* 4 (tcp-header-data-offset segment))))))))
	   (DEALLOCATE-RESOURCE 'tcp-segment segment)))))



;
;1 segment retransmission*
;
;1 The tcb retransmit-timestamp is used in the following manner.  When the retransmit-timestamp is non-nil,*
;1 the tcb is guaranteed to be linked on the retransmit list.  When the retransmit-timestamp is nil, the tcb*
;1 may or may not be on the retransmit-list.*
;
(DEFUN calculate-retransmit-list-event ()
  "2Calculate the time of the next retransmission in 1/60 seconds.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p retransmit-list)
	 q
	 retransmit-timestamp
	 new-retransmit-list-event)
	((NULL p)
	 (SETF retransmit-list-event new-retransmit-list-event))
      ;1; remove tcbs with nil retransmit-timestamps from the retransmit list*
      (COND ((NOT (SETF retransmit-timestamp (SEND p :retransmit-timestamp)))
	     (DO ((r (SEND p :retransmit-q) s)
		 s)
		((NULL r)
		 (SETF (SEND p :retransmit-q) ())
		 (SETF (SEND p :retransmit-q-end) ()))
	      (SETF s (segment-link r))
	      (DEALLOCATE-RESOURCE 'tcp-segment r))
	     (SETF p (PROG1 (SEND p :retransmit-link)
			    (SETF (SEND p :retransmit-link) nil)))
	     (IF q
		 (SETF (SEND q :retransmit-link) p)
		 (SETF retransmit-list p)))
	    (t (WHEN (OR (NOT new-retransmit-list-event)
			 (TIME-LESSP retransmit-timestamp new-retransmit-list-event))
		 (SETF new-retransmit-list-event retransmit-timestamp))
	       (SETF q p)
	       (SETF p (SEND p :retransmit-link)))))
    (calculate-background-event))) 

  
		
(DEFMETHOD (tcp-handler :link-tcb-to-retransmit-list) (tcb timestamp)
  "2Place the TCB on the retransmit-list, if not already there.  Update the retransmit-timestamp.
Note that if the timestamp argument is nil, this will have the effect of removing the tcb from the
list, provided it was already there.*"
  (WITHOUT-INTERRUPTS
    (WHEN (AND timestamp (NOT (SEND tcb :retransmit-timestamp)))
      (DO ((p retransmit-list (SEND p :retransmit-link))
	   (q nil p))
	  ((COND ((EQ p tcb))
		 ((NULL p)
		  (SETF (SEND tcb :retransmit-link) nil)
		  (IF q
		      (SETF (SEND q :retransmit-link) tcb)
		      (SETF retransmit-list tcb))
		  t)))))
    (SETF (SEND tcb :retransmit-timestamp) timestamp)
    (calculate-retransmit-list-event)))



(DEFMETHOD (tcp-handler :delink-tcb-from-retransmit-list) (tcb)
  "2If on retransmit-list, remove the TCB from the retransmit-list and remove all segments on retransmit-q.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (SETF (SEND tcb :retransmit-timestamp) nil)
    (calculate-retransmit-list-event))) 



(DEFUN process-retransmit-list ()
  "2Drive any connection which has an expired retransmit-timestamp.*"
  (DECLARE (:self-flavor tcp-handler))
  (DO (tcb)
      (nil)
    (SETF tcb (WITHOUT-INTERRUPTS
		(DO ((p retransmit-list (SEND p :retransmit-link)))
		    ((NULL p) nil)
		  (WHEN (AND (SEND p :retransmit-timestamp)
			     (NOT (TIME-LESSP (TIME) (SEND p :retransmit-timestamp))))
		    ;1; return a single tcb*
		    (RETURN p)))))
    ;1; do not drive connection without-interrupts*
    (COND (tcb
	   (INCF retransmissions)
	   (CONDITION-CASE ()
	       (SEND tcb :external-drive-connection)
	     (sys:network-error)))
	  ;1; exit when no tcb is found*
	  (t (RETURN))))) 
 


;
;1 delayed acknowledgement management*
;
;1 The tcb delayed-ack-timestamp is used in the following manner.  When the delayed-ack-timestamp is non-nil,*
;1 the tcb is guaranteed to be linked on the delayed-ack list.  When the delayed-ack-timestamp is nil, the tcb*
;1 may or may not be on the delayed-ack-list.*
;
(DEFUN calculate-delayed-ack-list-event ()
  "2Calculate the time of the next delayed-ack expiration in 1/60 seconds.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p delayed-ack-list)
	 q
	 delayed-ack-timestamp
	 new-delayed-ack-list-event)
	((NULL p)
	 (SETF delayed-ack-list-event new-delayed-ack-list-event))
      ;1; remove tcbs with not delayed-ack-timestamps from the delayed-ack list*
      (COND ((NOT (SETF delayed-ack-timestamp (SEND p :delayed-ack-timestamp)))
	     (SETF p (PROG1 (SEND p :delayed-ack-link)
			    (SETF (SEND p :delayed-ack-link) nil)))
	     (IF q
		 (SETF (SEND q :delayed-ack-link) p)
		 (SETF delayed-ack-list p)))
	    (t (WHEN (OR (NOT new-delayed-ack-list-event)
			 (TIME-LESSP delayed-ack-timestamp new-delayed-ack-list-event))
		 (SETF new-delayed-ack-list-event delayed-ack-timestamp))
	       (SETF q p)
	       (SETF p (SEND p :delayed-ack-link)))))
    (calculate-background-event)))



(DEFMETHOD (tcp-handler :link-tcb-to-delayed-ack-list) (tcb timestamp)
  "2Place the TCB on the delayed-ack-list, if not already there.  Update the delayed-ack-timestamp.
Note that if the timestamp argument is nil, this will have the effect of removing the tcb from the
list, provided it was already there.*"
  (WITHOUT-INTERRUPTS
    (WHEN (AND timestamp (NOT (SEND tcb :delayed-ack-timestamp)))
      (DO ((p delayed-ack-list (SEND p :delayed-ack-link))
	   (q nil p))
	  ((COND ((EQ p tcb))
		 ((NULL p)
		  (SETF (SEND tcb :delayed-ack-link) nil)
		  (IF q
		      (SETF (SEND q :delayed-ack-link) tcb)
		      (SETF delayed-ack-list tcb))
		  t)))))
    (SETF (SEND tcb :delayed-ack-timestamp) timestamp)
    (calculate-delayed-ack-list-event))) 



(DEFUN delink-tcb-from-delayed-ack-list (tcb)
  "2If on delayed-ack-list, remove the TCB from the delayed-ack-list.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p delayed-ack-list (SEND p :delayed-ack-link))
	 (q nil p))
	((COND
	   ((NULL p))
	   ((EQ p tcb)
	    (SETF (SEND tcb :delayed-ack-timestamp) nil)
	    (IF q
		(SETF (SEND q :delayed-ack-link) (SEND tcb :delayed-ack-link))
		(SETF delayed-ack-list (SEND tcb :delayed-ack-link)))
	    (SETF (SEND tcb :delayed-ack-link) ())
	    t))))
    (calculate-delayed-ack-list-event))) 


(DEFUN process-delayed-ack-list ()
  "2Drive any connection which has an expired delayed-ack-timestamp.*"
  (DECLARE (:self-flavor tcp-handler))
  (DO (tcb)
      (nil)
    (SETF tcb (WITHOUT-INTERRUPTS
		(DO ((p delayed-ack-list (SEND p :delayed-ack-link)))
		    ((NULL p) nil)
		  (WHEN (AND (SEND p :delayed-ack-timestamp)
			     (NOT (TIME-LESSP (TIME) (SEND p :delayed-ack-timestamp))))
		    ;1; return a single tcb*
		    (RETURN p)))))
    ;1; do not drive connection without-interrupts*
    (COND (tcb
	   (INCF delayed-acks)
	   (CONDITION-CASE ()
	       (SEND tcb :external-drive-connection)
	     (sys:network-error)))
	  ;1; exit when no tcb is found*
	  (t (RETURN))))) 



;
;1 time-wait state management*
;
;1 The time-wait-timestamp in the tcb is set when the connection enters the time-wait state.*
;1 However, the connection object will not be linked onto the time-wait list until all data segments*
;1 have been removed by the user.*
;
(DEFUN calculate-time-wait-list-event ()
  "2Calculate the time of the next time-wait expiration in 1/60 seconds.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (SETF time-wait-list-event (WHEN time-wait-list
				 (SEND time-wait-list :time-wait-timestamp)))
    (calculate-background-event))) 



(DEFMETHOD (tcp-handler :link-tcb-to-time-wait-list) (tcb)
  "2Place the TCB on the time-ordered time-wait list.*"
  ;1; replace all handlers with benign defaults*
  (SETF (SEND tcb :buffer-handler) #'(lambda (ignore ignore ignore)))
  (SETF (SEND tcb :urgent-handler) #'(lambda ()))
  (SETF (SEND tcb :receive-fin-handler) #'(lambda ()))
  (SETF (SEND tcb :close-complete-handler) #'(lambda ()))
  (SETF (SEND tcb :user-timeout-handler) ())
  (SETF (SEND tcb :condition-handler) #'(lambda (ignore)))
  (WITHOUT-INTERRUPTS
    (DO ((p time-wait-list (SEND p :time-wait-link))
	 (q nil p))
	((OR (NULL p)
	     (TIME-LESSP (SEND tcb :time-wait-timestamp) (SEND p :time-wait-timestamp)))
	 (IF q
	     (SETF (SEND q :time-wait-link) tcb)
	     (SETF time-wait-list tcb))
	 (SETF (SEND tcb :time-wait-link) (COND
					    (p)
					    (nil)))))
    (calculate-time-wait-list-event))) 



(DEFUN delink-tcb-from-time-wait-list (tcb)
  "2If on time-wait-list, remove the TCB from the time-wait-list.*"
  (DECLARE (:self-flavor tcp-handler))
  (WITHOUT-INTERRUPTS
    (DO ((p time-wait-list (SEND p :time-wait-link))
	 (q nil p))
	((COND
	   ((NULL p))
	   ((EQ p tcb)
	    (SETF (SEND tcb :time-wait-timestamp) nil)
	    (IF q
		(SETF (SEND q :time-wait-link) (SEND tcb :time-wait-link))
		(SETF time-wait-list (SEND tcb :time-wait-link)))
	    (SETF (SEND tcb :time-wait-link) ())
	    t))))
    (calculate-time-wait-list-event))) 



(DEFUN process-time-wait-list ()
  "2Remove all TCBs from the time-wait list that have completed the time-wait-timeout.*"
  (DECLARE (:self-flavor tcp-handler))
  (DO (tcb)
      (nil)
    (SETF tcb nil)
    (WITHOUT-INTERRUPTS
      (WHEN (AND time-wait-list
		 (NOT (TIME-LESSP (TIME) (SEND time-wait-list :time-wait-timestamp))))
	(SETF time-wait-list (PROG1 (SEND time-wait-list :time-wait-link)
				    (SETF (SEND time-wait-list :time-wait-link) ())
				    (SETF tcb time-wait-list)))))
    (UNLESS tcb (RETURN (calculate-time-wait-list-event)))
    ;1; delete those tcbs with no pending data (must be done under lock to avoid race condition)*
    (with-tcb-lock
      tcb
      (WHEN (EQ :time-wait (SEND tcb :state))
	(IF (EQL (SEND tcb :out-sequence) (SEND tcb :fin-sequence))
	    (SEND self :delete-tcb tcb)
	    (SETF (SEND tcb :state) :closed))))))



;
;1 probing of idle connections*
;
(DEFUN probe-connections (&aux probe-list)
  "2Probe all connections that have been idle for the probe interval.*"
  (DECLARE (:self-flavor tcp-handler))
  (WHEN (SETF probe-event (WHEN *tcp-dormant-probe-interval*
			    (TIME-INCREMENT probe-event *tcp-dormant-probe-interval*)))
    ;1; warning maphash locks the tcb-table*
    (MAPHASH
      #'(lambda (ignore tcb)
	  (WITHOUT-INTERRUPTS
	    (DO ((p tcb (SEND p :port-link)))
		((NULL p))
	      (CASE (SEND p :state)
		((:established :fin-wait-1 :fin-wait-2 :closing :close-wait :last-ack)		   
		 (WHEN
		   (AND *tcp-dormant-probe-interval*
			(SEND p :last-receive-time)
			(NOT (TIME-LESSP (TIME)
					 (TIME-INCREMENT (SEND p :last-receive-time)
							 *tcp-dormant-probe-interval*))))
		   (PUSH p probe-list)))))))
      (SEND *tcp-handler* :tcb-table))
    ;1; to avoid deadlock, drive the connection outside maphash*
    (DOLIST (p probe-list)
      (CASE (SEND p :state)
	((:established :fin-wait-1 :fin-wait-2 :closing :close-wait :last-ack)		   
	 (WHEN
	   (AND *tcp-dormant-probe-interval*
		(SEND p :last-receive-time)
		(NOT (TIME-LESSP (TIME)
				 (TIME-INCREMENT (SEND p :last-receive-time)
						 *tcp-dormant-probe-interval*))))
	   (INCF idle-probes)
	   (CONDITION-CASE ()
	       (SEND p :external-drive-connection :block :send-probe)
	     (sys:network-error)))))))
  (calculate-background-event))



;
;1 connection object management*
;
(DEFMETHOD (tcp-handler :dummy-connection)
	   (&optional (condition (MAKE-CONDITION 'illegal-connection "connection illegal for this process")))
  "2Generate a dummy connection that signals CONDITION error for most operations.*"
  #'(lambda (operation &rest ignore)
      (CASE operation
	    ((:state :status) :closed)
	    (otherwise (SIGNAL-CONDITION condition)))))


(DEFMETHOD (tcp-handler :make-connection)
	   (buffer-handler urg-handler process-fin-handler receive-fin-handler
	    close-complete-handler user-timeout-handler condition-handler)
  "2Allocate a TCP connection object (TCB).  See the documentation for open-tcp-connection.*"
  (ALLOCATE-RESOURCE 'tcbs buffer-handler urg-handler process-fin-handler receive-fin-handler
		     close-complete-handler user-timeout-handler condition-handler)) 



(DEFMETHOD (tcp-handler :delete-tcb) (tcb &optional dont-deregister-p)
  "2Cleanup all connection-oriented resources.
The optional dont-deregister-p argument is used during reset-tcp-service to disallow modification of the hash table.*"
  (SETF (SEND tcb :state) :closed)
  (UNLESS dont-deregister-p (SEND self :deregister-tcb tcb))
  (delink-tcb-from-delayed-ack-list tcb)
  (delink-tcb-from-time-wait-list tcb)
  (WHEN (SEND tcb :source-port)
    (SEND self :unreserve-port (SEND tcb :source-port)))
  (DO ((p (SEND tcb :send-q) q)
       q)
      ((NULL p)
       (SETF (SEND tcb :send-q) ())
       (SETF (SEND tcb :send-q-end) ()))
    (SETF q (segment-link p))
    (DEALLOCATE-RESOURCE 'tcp-segment p))
  (SEND self :delink-tcb-from-retransmit-list tcb)
  (DO ((p (SEND tcb :receive-q) q)
       q)
      ((NULL p)
       (SETF (SEND tcb :receive-q) ())
       (SETF (SEND tcb :receive-q-end) ()))
    (SETF q (segment-link p))
    (DEALLOCATE-RESOURCE 'tcp-segment p))
  (WHEN (SEND tcb :received-segment)
    (DEALLOCATE-RESOURCE 'tcp-segment (SEND tcb :received-segment))
    (SETF (SEND tcb :received-segment) nil))
  (DO ((p (SEND tcb :out-of-order-q) q)
       q)
      ((NULL p)
       (SETF (SEND tcb :receive-q) ())
       (SETF (SEND tcb :receive-q-end) ()))
    (SETF q (segment-link p))
    (DEALLOCATE-RESOURCE 'tcp-segment p))
  (DO ((p (SEND tcb :return-q) q)
       q)
      ((NULL p)
       (SETF (SEND tcb :receive-q) ())
       (SETF (SEND tcb :receive-q-end) ()))
    (SETF q (segment-link p))
    (DEALLOCATE-RESOURCE 'tcp-segment p))
  (WHEN (SEND tcb :remote-probe-segment)
    (DEALLOCATE-RESOURCE 'tcp-segment (SEND tcb :remote-probe-segment)))
  (SEND tcb :clean-up-rd-q)
  (DEALLOCATE-RESOURCE 'tcbs tcb)) 



;
;1 tcp-handler port management methods. *
;
(LET ((last-port-table-index 0))
  ;1; this method is memoized*
  (DEFMETHOD (tcp-handler :generate-port) (&aux port (table-size (ARRAY-DIMENSION port-table 0)) random-part)
    "Generates and reserves a unique TCP port not in the well-known range."
    (WITHOUT-INTERRUPTS
      ;1; find a table slot*
      (DO ((i (MOD (1+ last-port-table-index) table-size) (MOD (1+ i) table-size)))
	  ((IF (EQL i last-port-table-index)
	       (FERROR 'insufficient-resources "insufficient resources for connection")
	       (ZEROP (AREF port-table i)))
	   (SETF last-port-table-index i)))
      ;1; generate a unique port number*
      (DO (unique-port-p
	   (index-bits (CEILING (LOG table-size 2))))
	  (unique-port-p)
	(SETF port (+ last-port-table-index
		      (LSH (MOD (IF (ZEROP (SETF random-part (time:microsecond-time)))
				    1
				    random-part)
				(EXPT 2 (- 16 index-bits)))
			   index-bits)))
	(SETF unique-port-p t)
	(MAPHASH
	  #'(lambda (ignore tcb)
	      (DO ((p tcb q)
		   q)
		  ((NULL p))
		(SETF q (SEND p :port-link))
		(WHEN (OR (EQL port (SEND p :source-port))
			  (AND (MEMBER (SEND p :destination-address) my-addresses)
			       (EQL port (SEND p :destination-port))))
		  (SETF unique-port-p nil))))
	  tcb-table))
      ;1; reserve it before anyone else can get it*
      (SEND self :reserve-port port)
      port))) 


(DEFMETHOD (tcp-handler :reserve-port) (port-number &aux (table-size (ARRAY-DIMENSION port-table 0)))
  "2Increment the use count on a port number.*"
  (WITHOUT-INTERRUPTS
    (WHEN (> port-number 255)
      (WHEN (EQL 1 (INCF (AREF port-table (MOD port-number (EXPT 2 (CEILING (LOG table-size 2)))))))
	(INCF ports-currently-reserved))))) 


(DEFMETHOD (tcp-handler :unreserve-port) (port-number &aux (table-size (ARRAY-DIMENSION port-table 0)))
  "2Decrement the use count on a port number.*"
  (WITHOUT-INTERRUPTS
    (WHEN (> port-number 255)
      (WHEN (ZEROP (DECF (AREF port-table (MOD port-number (EXPT 2 (CEILING (LOG table-size 2)))))))
	(DECF ports-currently-reserved))))) 


;
;1 connection registration methods*
;
(DEFMETHOD (tcp-handler :register-tcb) (tcb)
  "2Register connection for packet reception.  The TCB should be verified to be unique prior to registration.*"
  (si:with-lock-fast ((si:hash-table-lock tcb-table))
    (SETF (SEND tcb :port-link) (GETHASH (SEND tcb :source-port) tcb-table))
    (SETF (GETHASH (SEND tcb :source-port) tcb-table) tcb))) 



(DEFMETHOD (tcp-handler :find-tcb) (source-port source-address destination-port
				    destination-address &key (syn-segment-p nil)
				    &aux (unattached-tcb nil) (inexact-match-type :both))
  "2Return a TCB that represents the specified connection.
If :syn-segment-p is non-nil, return a connection in the listen state (with possibly partially specified destination socket),
  if an exact match cannot be found.  Note that source-address is only used if syn-segment-p is non-nil.
Otherwise, only the unique connection which matches the specified parameters will be returned.
The match hierarchy is as follows (in decreasing order): exact, inexact address, inexact port, both.
Warning: This method must be called from within a WITHOUT-INTERRUPTS form.*"
  ;1; listening tcbs are not available when a syn segment is on the receive-q, execpt for duplicate syns *
  (DO ((tcb (GETHASH destination-port tcb-table) (SEND tcb :port-link)))
      ((OR (NOT tcb)
	   (COND
	     ((AND (EQL (SEND tcb :destination-port) source-port)
		   (EQL (SEND tcb :destination-address) source-address)
		   (EQL (SEND tcb :source-address) destination-address)))
	     ((NOT syn-segment-p) nil)
	     ((AND (EQL (SEND tcb :destination-port) source-port)
		   (ZEROP (SEND tcb :destination-address)) (EQ (SEND tcb :state) :listen))
	      (COND
		((SEND tcb :receive-q)
		 (AND (tcp-header-syn-p (SEND tcb :receive-q))
		      (EQL (tcp-header-source-port (SEND tcb :receive-q)) source-port)
		      (EQL (segment-source-address (SEND tcb :receive-q)) source-address)
		      (EQL (tcp-header-destination-port (SEND tcb :receive-q)) destination-port)
		      (EQL (segment-destination-address (SEND tcb :receive-q)) destination-address)))
		(t (SETF unattached-tcb tcb)
		   (SETF inexact-match-type :inexact-destination) nil)))
	     ((AND (EQL (SEND tcb :destination-address) source-address)
		   (ZEROP (SEND tcb :destination-port)) (EQ (SEND tcb :state) :listen))
	      (COND
		((SEND tcb :receive-q)
		 (AND (tcp-header-syn-p (SEND tcb :receive-q))
		      (EQL (tcp-header-source-port (SEND tcb :receive-q)) source-port)
		      (EQL (segment-source-address (SEND tcb :receive-q)) source-address)
		      (EQL (tcp-header-destination-port (SEND tcb :receive-q)) destination-port)
		      (EQL (segment-destination-address (SEND tcb :receive-q)) destination-address)))
		((EQ inexact-match-type :both)
		 (SETF unattached-tcb tcb)
		 (SETF inexact-match-type :inexact-port) nil)))
	     ((AND (ZEROP (SEND tcb :destination-port)) (ZEROP (SEND tcb :destination-address))
		   (EQ (SEND tcb :state) :listen))
	      (COND
		((SEND tcb :receive-q)
		 (AND (tcp-header-syn-p (SEND tcb :receive-q))
		      (EQL (tcp-header-source-port (SEND tcb :receive-q)) source-port)
		      (EQL (segment-source-address (SEND tcb :receive-q)) source-address)
		      (EQL (tcp-header-destination-port (SEND tcb :receive-q)) destination-port)
		      (EQL (segment-destination-address (SEND tcb :receive-q)) destination-address)))
		((EQ inexact-match-type :both)
		 (SETF unattached-tcb tcb) nil)))))
       (COND
	 (tcb)
	 (syn-segment-p unattached-tcb))))) 
	      


(DEFMETHOD (tcp-handler :deregister-tcb) (tcb)
  "2Deregister connection, no longer available for packet reception.*"
  (si:with-lock-fast ((si:hash-table-lock tcb-table))
    (DO ((p (GETHASH (SEND tcb :source-port) tcb-table) (SEND p :port-link))
	 (q nil p))
	((COND
	   ((EQ p tcb)
	    (COND
	      ((NULL q)
	       (IF (SEND tcb :port-link)
		   (SETF (GETHASH (SEND tcb :source-port) tcb-table) (SEND tcb :port-link))
		   (REMHASH (SEND tcb :source-port) tcb-table)))
	      (t (SETF (SEND q :port-link) (SEND tcb :port-link))))
	    t)
	   ((NULL p))))))) 


(COMPILE-FLAVOR-METHODS tcp-handler)