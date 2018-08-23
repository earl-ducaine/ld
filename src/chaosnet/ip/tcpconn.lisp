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
;1 TCP connection management methods*
;
(DEFMETHOD (tcp-connection :external-drive-connection) (&optional (block-p t) send-probe-p)
  "2This method is used by the tcp-handler and tcp-stream to drive the connection externally.
That is, it is not called from within a method of tcp-connection.  Arguements are:
BLOCK-P (optional) - specifies whether the caller allows blocking awaiting the connection lock.
  When nil (default), this method will return immediately if the lock is set.
SEND-PROBE-P (optional) - specifies that an idle connection probe should be sent, if necessary.
  Also, the user timeout on connection probes is checked.*"
  (LET* ((pointer (LOCF lock))
	 (already-mine (EQ (CAR pointer) current-process))
	 lock-set)
    ;1; disallow recursive entry*
    (UNLESS already-mine
      (UNWIND-PROTECT
	  (PROGN
	    (SETF lock-set (SYS:%STORE-CONDITIONAL pointer nil current-process))
	    (WHEN (AND (NOT lock-set) block-p) 
	      (PROCESS-LOCK pointer)
	      (SETF lock-set t))
	    (WHEN lock-set
	      (CONDITION-BIND ((system:network-error #'(lambda (condition tcb)
							 (SEND tcb :error condition))
						     self))
		;1; connection probing is done here*
		(WHEN send-probe-p
		  (IF (TIME-LESSP (TIME) (TIME-INCREMENT last-receive-time
							 (* *tcp-dormant-connection-gc-threshold*
							    *tcp-dormant-probe-interval*)))
		      (send-idle-probe)
		      (FERROR 'host-stopped-responding "Foreign host stopped responding." self)))
		(DO (once)
		    ;1; atomically unlock only when receive-q is empty*
		    ((AND once
			  (NOT (WITHOUT-INTERRUPTS
				 (COND (receive-q)
				       (t (SYS:%STORE-CONDITIONAL pointer current-process nil)
					  nil))))))
		  (drive-connection)
		  (SETF once t)))))
	(WHEN lock-set (SYS:%STORE-CONDITIONAL pointer current-process nil))))))


(DEFMETHOD (tcp-connection :drive-locked-connection) ()
  "2Drive a connection that has already been locked.  Allows access to drive-connection outside of 
a method of tcp-connection, as long as the connection lock has been set.*"
  (drive-connection))


(DEFUN drive-connection ()
  "2Process all segments on the receive-q, followed by all segments on the send-q.
Process user timeouts, retransmissions, delayed/piggybacked acknowledgements.
Warning: all callers of this method must have an condition handler for sys:network-error.
          Also, all callers of this method must have non-recursively locked the connection lock.*"
  (DECLARE (:self-flavor tcp-connection))
  ;1; this order, receive-q then send-q, is important for zero window probing logic and piggybacked acknowledgement*
  (WHEN receive-q (process-receive-q))
  (WHEN send-q (process-send-q))
  ;1; check for user timeout*
  (WHEN (AND retransmit-q (segment-user-timeout retransmit-q)
	     (NOT (TIME-LESSP (TIME) (segment-user-timeout retransmit-q))))
    (COND (user-timeout-handler
	   (SETF (segment-user-timeout retransmit-q)
		 (AND user-timeout (TIME-INCREMENT (TIME) (* 60. user-timeout))))
	   (FUNCALL user-timeout-handler))
	  (t (FERROR 'user-timeout "connection aborted due to user timeout" self))))
  ;1; handle retransmissions*
  (WHEN (AND retransmit-timestamp (NOT (TIME-LESSP (TIME) retransmit-timestamp)))
    (retransmit-segment))
  ;1; handle acknowledgments that were not piggybacked and delayed acknowledgements*
  (WHEN (OR ack-due-p (AND delayed-ack-timestamp (NOT (TIME-LESSP (TIME) delayed-ack-timestamp))))
    (send-ack)))



(DEFMETHOD (tcp-connection :locf-lock) ()
  "2Return the locative of the connection lock.*"
  (LOCF lock))



(DEFMETHOD (tcp-connection :error) (condition)
  "2Condition handler for tcp connections, used within condition-bind.*"
  (UNWIND-PROTECT
      ;1; call the user's condition handler*
      (FUNCALL condition-handler condition)
    (SEND *tcp-handler* :delete-tcb self)))



;
;1 transmission methods*
;
(DEFUN process-send-q ()
  "2Transmit segments on send-q which have been filled, pushed, or have an implied push.
When the window is not large enough to accomodate the current segment, the segment is reduced.
When the window is zero, window probing logic is entered as described in RFC793 pp. 42-43.  
Note that SYNs and RSTs are not queued to the send-q.  Also, ACKs without text or control are not queued.*"
  (DECLARE (:self-flavor tcp-connection))
  (CASE state
	((:established :close-wait)
	 ;1; terminate window probe when probe is acked*
	 (SETF window-probe-active-p (AND window-probe-active-p (<seq send-unack send-next)))
	 (DO (useable-window segment-sequence-space)
	     ;1; termination conditions*
	     ((OR
		;1; empty queue*
		(NOT send-q)
		;1; segment is not full (excepting pushes or implied pushes)*
		(NOT (OR (tcp-header-push-p send-q) (tcp-header-fin-p send-q)
			 (EQL (segment-size send-q) (segment-index send-q))))
		;1; window probe in progress*
		window-probe-active-p
		;1; useable window is less than 25% of the offered send window as per RFC813*
		;1; (except when zero send window or pushed segment fits useable window)*
		(PROGN
		  (SETF segment-sequence-space
			(+ (tcp-header-fin-flag send-q)
			   (- (segment-index send-q) *tcp-minimum-header-size*)))
		  (SETF useable-window (-seq (+seq send-unack send-window) send-next))
		  (AND (PLUSP send-window)
		       (< (* 4 useable-window) send-window)		       
		       (< useable-window segment-sequence-space)))))	   
	   (COND
	     ;1; enter zero window probing logic only when all data has been acked*
	     ((ZEROP send-window)
	      (WHEN retransmit-q (RETURN))
	      (SETF window-probe-active-p t)
	      ;1; send either the entire segment or single octet of text according to switch setting*
	      (WHEN (AND *octet-sized-probe-switch* (< 1 segment-sequence-space))
		(restructure-send-q 1)))
	     ;1; restructure segment to smaller size only when all data has been acked, according to swith setting*
	     ((< useable-window segment-sequence-space)
	      (WHEN retransmit-q (RETURN))
	      (WHEN *octet-sized-probe-switch*
		(restructure-send-q useable-window))))
	   ;1; out-of-band state transition required by queued fin segments*
	   (WHEN (tcp-header-fin-p send-q)
	     (CASE state
		   (:established (SETF state :fin-wait-1))
		   (:close-wait (SETF state :last-ack))))
	   (transmit-segment (dequeue send-q send-q-end)))))) 



(DEFUN transmit-segment (segment &aux text-size)
  "2Pass segment to IP protocol (RFC791) for transmission.*"
  (DECLARE (:self-flavor tcp-connection) (SPECIAL *ip-handler*))
  (SETF (tcp-header-checksum segment) 0)
  ;1; WARNING: tcp-header-sequence-# and segment-sequence-# must agree for all segments on retransmit queue*
  (SETF (tcp-header-sequence-# segment) send-next
	(segment-sequence-# segment) send-next)
  (SETF (tcp-header-source-port segment) source-port)
  (SETF (tcp-header-destination-port segment) destination-port)
  (WHEN (tcp-header-ack-p segment)
    (SETF ack-due-p nil)
    (WHEN delayed-ack-timestamp
      (SEND *tcp-handler* :link-tcb-to-delayed-ack-list self nil))
    (SETF (tcp-header-ack-# segment) receive-next
          latest-leading-edge receive-next)
    (SETF latest-trailing-edge (+seq receive-next
				     (SETF (tcp-header-window segment)
					   (IF (EQ state :syn-received)
					       (prescan-rd-q)
					       receive-window)))))
  (SETF text-size (- (segment-index segment) (* (tcp-header-data-offset segment) 4)))
  (SETF (segment-sequence-length segment)
	(+ (tcp-header-fin-flag segment) (tcp-header-syn-flag segment) text-size))
  (INCF total-text-sent text-size)
  (WHEN (PLUSP text-size) (INCF number-of-text-segments-sent))
  (SETF maximum-text-sent (MAX maximum-text-sent text-size))
  (INCF number-of-segments-sent)
  (SETF (tcp-header-checksum segment)
	(ip-ones-complement-checksum segment (segment-index segment) source-address
				     destination-address *tcp-protocol* (segment-index segment)))
  (SEND *ip-handler* :transmit segment :tcp destination-address (segment-index segment)
	source-address)
  ;1; if segment occupies sequence number space, update pointers and queue for possible retransmission*
  (COND
    ((PLUSP (segment-sequence-length segment))
     (SETF send-next (+seq send-next (segment-sequence-length segment)))
     (SETF (segment-time segment) (TIME))
     (SETF (segment-retransmission-timeout segment) (TIME-INCREMENT (TIME) retransmission-timeout))
     (SETF (segment-user-timeout segment)
		   (AND user-timeout (TIME-INCREMENT (TIME) (* 60 user-timeout))))
     (queue segment retransmit-q retransmit-q-end)
     (WHEN (NULL retransmit-timestamp)
       (SEND *tcp-handler* :link-tcb-to-retransmit-list self (segment-retransmission-timeout segment))))
    (t (DEALLOCATE-RESOURCE 'tcp-segment segment))))

 

(DEFUN retransmit-segment (&aux (segment retransmit-q))
  "2Retransmit the first segment on the retransmit queue.*"
  ;1; since zero send window probes are put on the retransmit queue, and such probes must carry the*
  ;1; latest window information, we update retransmitted segments with the latest window information*
  (DECLARE (:self-flavor tcp-connection) (SPECIAL *ip-handler*))
  (INCF number-of-retransmissions)
  (SETF (segment-retransmission-timeout segment) (TIME-INCREMENT (TIME) retransmission-timeout))
  (SEND *tcp-handler* :link-tcb-to-retransmit-list self (segment-retransmission-timeout segment))
  (WHEN (tcp-header-ack-p segment)
    (SETF (tcp-header-ack-# segment) receive-next
	  latest-leading-edge receive-next)
    (SETF latest-trailing-edge (+seq receive-next
				     (SETF (tcp-header-window segment)
					   (IF (EQ state :syn-received)
					       (prescan-rd-q)
					       receive-window)))))
  (SETF (tcp-header-checksum segment) 0)
  (SETF (tcp-header-checksum segment)
	(ip-ones-complement-checksum segment (segment-index segment) source-address
				     destination-address *tcp-protocol* (segment-index segment)))
  (SEND *ip-handler* :transmit segment :tcp destination-address
	(segment-index segment) source-address))



(DEFUN update-send-unack (ack-number &aux rtt)
  "2Update send-unack, remove acked segments, and update the retransmission timeout (RFC793, p41).*"
  (DECLARE (:self-flavor tcp-connection))
  (SETF send-unack ack-number)
  (DO ((p retransmit-q retransmit-q))
      ((OR (NULL p)
	   (<seq send-unack (+seq (segment-sequence-# p) (segment-sequence-length p)))))
    (WHEN (NULL rtt)
      (SETF rtt (TIME-DIFFERENCE (TIME) (segment-time p))))
    (DEALLOCATE-RESOURCE 'tcp-segment (dequeue retransmit-q retransmit-q-end)))
  ;1; a non-nil rtt indicates that at least one segment was removed from the retransmit-q*
  (WHEN rtt
    (SEND *tcp-handler* :link-tcb-to-retransmit-list self
	  (WHEN retransmit-q (segment-retransmission-timeout retransmit-q)))
    (SETF smoothed-round-trip-time
	  (IF (NULL smoothed-round-trip-time)
	      rtt
	      (ROUND (+ (* *alpha* smoothed-round-trip-time) (* (- 1 *alpha*) rtt)))))
    (SETF retransmission-timeout
	  (MIN *ubound* (MAX *lbound* (* *beta* smoothed-round-trip-time))))))



(DEFUN send-idle-probe (&aux probe-sequence-#)
  "2Transmit an invalid segment that is guaranteed to be rejected by the remote end, causing an ack to be returned.*"
  (DECLARE (:self-flavor tcp-connection) (SPECIAL *ip-handler*))
  (CASE state
    ((:established :close-wait :fin-wait-2)
     (USING-RESOURCE (segment tcp-segment (+ *tcp-minimum-header-size* 1))
       ;1; invalid sequence number with byte of garbage text*
       (SETF probe-sequence-# (+seq send-unack -1))
       
       (INCF (segment-index segment))
       (SETF (tcp-header-checksum segment) 0)
       (SETF (tcp-header-sequence-# segment) probe-sequence-#)
       (SETF (tcp-header-source-port segment) source-port)
       (SETF (tcp-header-destination-port segment) destination-port)
       (SETF (tcp-header-ack-p segment) t)
       (SETF (tcp-header-ack-# segment) receive-next)
       (SETF (tcp-header-window segment) receive-window)
       (INCF number-of-segments-sent)
       (SETF (tcp-header-checksum segment)
	     (ip-ones-complement-checksum segment (segment-index segment) source-address
					  destination-address *tcp-protocol* (segment-index segment)))
       (SEND *ip-handler* :transmit segment :tcp
	     destination-address (segment-index segment) source-address)))))



(DEFUN send-syn (&optional send-ack-p &aux segment maximum-segment-option)
  "2Send a SYN segment with an optional acknowlegment.*"
  (DECLARE (:self-flavor tcp-connection))
  ;1; the following code is neccesary only until IP fragmentation is fixed*
  (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (+ *tcp-minimum-header-size* 4)))
  (SETF (segment-index segment) (+ *tcp-minimum-header-size* 4))
  (SETF (tcp-header-data-offset segment) 6)
  ;1; if on a directly attached IP network, use big segments else use the maximum Arpanet segment*
  (SETF maximum-segment-option
	(+ 33816576 (IF (EQ :direct (ip-routing-address (get-routing-entry destination-address)))
			*tcp-maximum-segment-size*
			(- *max-arpanet-packet-bytes* *min-header-bytes*))))
  (SETF (AREF segment options-position) (ASH maximum-segment-option -24))
  (SETF (AREF segment (1+ options-position)) (ASH maximum-segment-option -16))
  (SETF (AREF segment (+ options-position 2)) (ASH maximum-segment-option -8))
  (SETF (AREF segment (+ options-position 3)) (LOGAND 255 maximum-segment-option))
  (SETF (tcp-header-syn-p segment) t)
  (SETF (tcp-header-ack-p segment) send-ack-p)
  (transmit-segment segment)) 



(DEFUN send-ack (&aux segment)
  "2Send an ACK segment.*"
  (DECLARE (:self-flavor tcp-connection))
  (SETF segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
  (SETF (tcp-header-ack-p segment) t)
  (transmit-segment segment)) 



(DEFUN tcp-send-reset (rst-source-address rst-source-port rst-destination-address rst-destination-port
		       sequence-number &optional ack-number)
  "2Send a RST segment with the given destination socket, sequence number, and optional ack number.
This operation is handled in a special way to allow resets to be sent on non-open connections.*"
  (DECLARE (SPECIAL *ip-handler*))
  (USING-RESOURCE (segment tcp-segment *tcp-minimum-header-size*)
    (SETF (tcp-header-source-port segment) rst-source-port)
    (SETF (tcp-header-destination-port segment) rst-destination-port)
    (SETF (tcp-header-sequence-# segment) sequence-number)
    (SETF (tcp-header-reset-p segment) t)
    (WHEN ack-number
      (SETF (tcp-header-ack-p segment) t)
      (SETF (tcp-header-ack-# segment) ack-number)
      (SETF (tcp-header-window segment) 0))
    (SETF (tcp-header-checksum segment) 0)
    (SETF (tcp-header-checksum segment)
	  (ip-ones-complement-checksum segment (segment-index segment) rst-source-address
				       rst-destination-address *tcp-protocol* (segment-index segment)))
    (SEND *ip-handler* :transmit segment :tcp rst-destination-address (segment-index segment)
	  rst-source-address))) 



(DEFUN segmentize-buffer (user-buffer user-buffer-length push-flag urgent-flag)
  "2Copy user send buffer into TCP segment text (RFC793 p56).*"
  (DECLARE (:self-flavor tcp-connection))
  (DO ((user-index 0 (+ user-index min-length))
       segment
       index
       length
       min-length)
      ((ZEROP user-buffer-length)
       ;1; set push-flag only in last segment*
       (WHEN send-q-end
	 (SETF (tcp-header-push-p send-q-end) push-flag)))
    ;1; use send-q-end segment or new segment*
    (SETF segment
	  (COND
	    ((OR
	       ;1; empty send-q*
	       (NOT send-q-end)
	       ;1; send-q-end segment is full*
	       (= (segment-size send-q-end) (segment-index send-q-end))
	       ;1; send-q-end segment is pushed*
	       (tcp-header-push-p send-q-end))
	     ;1; until maximum-send-size is defined, use *tcp-maximum-segment-size* for queued segments*
	     (ALLOCATE-RESOURCE 'tcp-segment
				(CASE state
				      ((:syn-sent :syn-received) *tcp-maximum-segment-size*)
				      (otherwise maximum-send-size))))
	    (t send-q-end)))
    ;1; copy in data*
    (SETF index (segment-index segment))
    (SETF length (- (segment-size segment) index))
    (SETF min-length (MIN length user-buffer-length))
    (COPY-ARRAY-PORTION user-buffer user-index (+ user-index min-length) segment index
			(+ index min-length))
    (SETF (segment-index segment) (+ index min-length))
    (DECF user-buffer-length min-length)
    ;1; if new segment, format it*
    (WHEN (NOT (EQ segment send-q-end))
      (queue segment send-q send-q-end)
      (SETF (tcp-header-ack-p segment) t)
      (SETF (tcp-header-push-p segment) ()))
    ;1; setup urgent pointer in new and old segments*
    (WHEN urgent-flag
      (SETF (tcp-header-urgent-p segment) t)
      (SETF (tcp-header-urgent-pointer segment) (- (segment-index segment) *tcp-minimum-header-size*))))) 



(DEFUN resegmentize (&aux new-segment)
  "2This method is called when entering the :established state to guarantee adherence to maximum-send-size.*"
  (DECLARE (:self-flavor tcp-connection))
  (DO ((seg (PROG1
	      send-q
	      (SETF send-q ()
		    send-q-end ()))
	    (PROG1
	      (segment-link seg)
	      (DEALLOCATE-RESOURCE 'tcp-segment seg))))
      ((NULL seg))
    (USING-RESOURCE (displaced-array tcp-displaced-array seg)
      (segmentize-buffer displaced-array (ARRAY-DIMENSION displaced-array 0)
			 (tcp-header-push-p seg) (tcp-header-urgent-p seg))
      (WHEN (tcp-header-fin-p seg)
	(COND
	  (send-q-end (SETF (tcp-header-fin-p send-q-end) t))
	  (t (SETF new-segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
	     (SETF (tcp-header-fin-p new-segment) t) (SETF (tcp-header-ack-p new-segment) t)
	     (SETF (tcp-header-data-offset new-segment) 5) (queue new-segment send-q send-q-end))))))) 



(DEFUN restructure-send-q (size &aux segment)
  "2The first segment on the send-q is reduced to a segment of the indicated size.*"
  (DECLARE (:self-flavor tcp-connection))
  (WHEN (PLUSP size)
    ;1; format new segment by extracting from old segment*
    (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (+ *tcp-minimum-header-size* size)))
    (SETF (segment-link segment) send-q)
    (SETF (segment-index segment) (+ *tcp-minimum-header-size* size))
    (WHEN (SETF (tcp-header-urgent-p segment) (tcp-header-urgent-p send-q))
      (SETF (tcp-header-urgent-pointer segment) (tcp-header-urgent-pointer send-q)))
    (SETF (tcp-header-ack-p segment) (tcp-header-ack-p send-q))
    (COPY-ARRAY-PORTION send-q *tcp-minimum-header-size* (+ *tcp-minimum-header-size* size)
			segment *tcp-minimum-header-size* (+ *tcp-minimum-header-size* size))
    ;1; revise old segment*
    (WHEN (AND (tcp-header-urgent-p send-q)
	       (NOT (PLUSP (DECF (tcp-header-urgent-pointer send-q) size))))
      (SETF (tcp-header-urgent-pointer send-q) 0)
      (SETF (tcp-header-urgent-p send-q) ()))
    (COPY-ARRAY-PORTION send-q (+ *tcp-minimum-header-size* size) (segment-size send-q) send-q
			*tcp-minimum-header-size* (- (segment-size send-q) size))
    (DECF (segment-index send-q) size)
    (DECF (segment-size send-q) size)
    (SETF send-q segment))) 



;
;1 reception methods*
;
;1 WARNING: for inbound segments the accessors tcp-header-sequence-#, tcp-header-ack-#, and tcp-header-ack-p*
;1           should not be used.  Rather, use segment-sequence-#, segment-ack-#, and segment-ack-p, respectively.*
;1           The later accessors were designed for performance reasons, and their use is required.*
;1 *
(DEFUN process-receive-q ()
  "2Process inbound segments on receive-q through receive state machine.*"
  (DECLARE (:self-flavor tcp-connection))
  (DO (text-length segment-end-sequence intersegment-arrival-time)
      ((NULL receive-q))
    (SETF received-segment receive-q)
    ;1; update receive statistics*
    (SETF text-length (- (segment-size received-segment) (segment-index received-segment)))
    (SETF segment-end-sequence (+seq (segment-sequence-# received-segment) text-length))
    (INCF total-text-received text-length)
    (WHEN (PLUSP text-length) (INCF number-of-text-segments-received))
    (SETF maximum-text-received (MAX maximum-text-received text-length))
    (INCF number-of-segments-received)
    ;1; update delayed-ack timeout (RFC813 p21)*
    (WHEN not-push-time-p
      (SETF intersegment-arrival-time (TIME-DIFFERENCE (segment-time received-segment) not-push-time-p))
      (WHEN (< intersegment-arrival-time (* *delayed-ack-beta* smoothed-intersegment-arrival-time))
	(SETF smoothed-intersegment-arrival-time
	      (ROUND
		(+ (* *delayed-ack-alpha* smoothed-intersegment-arrival-time)
		   (* (- 1 *delayed-ack-alpha*) intersegment-arrival-time))))))
    (SETF not-push-time-p
	  (WHEN (AND (NOT (tcp-header-push-p received-segment)) (NOT (tcp-header-fin-p received-segment)))
	    (segment-time received-segment)))
    ;1; enter automaton*
    (DO ((automaton-state
	   (CASE state
		 ;1; to prevent race conditions in matching syns and listening connections,*
		 ;1; received-segment is not removed from the receive-q during handler-listen*
		 (:listen
		  :handler-listen)
		 (:syn-sent (PROGN
			      (dequeue receive-q receive-q-end)
			      :handler-syn-sent))
		 (otherwise (PROGN
			      (dequeue receive-q receive-q-end)
			      :sequence-check)))))
	(nil)
      (SETF automaton-state
	    (CASE automaton-state
		  (:handler-listen
		   (PROG1
		     (handler-listen)
		     (dequeue receive-q receive-q-end)))
		  (:handler-syn-sent (handler-syn-sent))
		  ;1; warning: sequence-check may modify segment-size in the array leader*
		  (:sequence-check (sequence-check))
		  (:reset-check (reset-check))
		  ;1; should check security and precedence here*
		  (:syn-check (syn-check))
		  (:ack-check (ack-check))
		  (:urgent-check (urgent-check))
		  (:remote-probe-check
		   (IF (ZEROP receive-window)
		       (IF (> (+ (segment-size received-segment) (tcp-header-fin-flag received-segment))
			      (segment-index received-segment))
			   :remote-probe
			   :exit)
		       :process-segment-text))
		  (:process-segment-text
		   (SETF received-segment
			 (PROG1
			   (process-segment-text)
			   ;1; use the ending sequence originally computed*
			   (fin-check segment-end-sequence)))
		   :exit)
		  (:remote-probe
		   (SETF received-segment (PROG1
				   remote-probe-segment
				   (SETF remote-probe-segment received-segment)))
		   :exit)
		  (:exit
		   (WHEN received-segment
		     (DEALLOCATE-RESOURCE 'tcp-segment received-segment)
		     (SETF received-segment nil))
		   (RETURN))))))) 



(DEFUN handler-listen ()
  "2Perform actions required on incoming segment when the TCB is in the LISTEN state (RFC793 p65).*"
  (DECLARE (:self-flavor tcp-connection))
  (BLOCK listen
    (WHEN (tcp-header-reset-p received-segment)
      (RETURN-FROM listen :exit))
    (WHEN (segment-ack-p received-segment)
      (tcp-send-reset source-address source-port (segment-source-address received-segment)
		      (tcp-header-source-port received-segment) (segment-ack-# received-segment))
      (RETURN-FROM listen :exit))
    (WHEN (tcp-header-syn-p received-segment)
      ;1; should check security/precedence here*
      (check-syn-option)
      (SETF initial-receive-sequence-# (segment-sequence-# received-segment))
      (SETF receive-next (+seq (segment-sequence-# received-segment) 1))
      (SETF out-sequence receive-next)
      (SETF return-sequence out-sequence)
      (SETF initial-send-sequence-# (LOGAND (1- *tcp-full-seq-range*) (GET-UNIVERSAL-TIME)))
      (SETF send-unack initial-send-sequence-#)
      ;1; this differs from spec because :send-syn increments send-next*
      (SETF send-next initial-send-sequence-#)
      (SETF state :syn-received)
      (SETF destination-port (tcp-header-source-port received-segment))
      (SETF destination-address (segment-source-address received-segment))
      (SETF source-address (segment-destination-address received-segment))
      (send-syn :ack)
      (WHEN (PLUSP
	      (+ (tcp-header-fin-flag received-segment)
		 (- (segment-size received-segment) (segment-index received-segment))))
	(RETURN-FROM listen :remote-probe)))
    (RETURN-FROM listen :exit))) 



(DEFUN check-syn-option ()
  "2Determine the maximum segment size if the option is set in a SYN segment.*"
  (DECLARE (:self-flavor tcp-connection))
  ;1; if no maximum segment size option is supplied, we assume 556 (workaround for MSDOS)*
  (SETF maximum-send-size (- *tcp-minimum-segment-size* *tcp-minimum-header-size*))
  (DO ((index options-position (1+ index)))
      ((COND
	 ((= index (segment-index received-segment)))
	 ((= (AREF received-segment index) 0))
	 ((= (AREF received-segment index) 2)
	  ;1; maximum segment size option*
	  (WHEN (= (AREF received-segment index) 2)
	    (SETF maximum-send-size
		  (MIN  *tcp-maximum-segment-size*
		       (+ (* 256 (AREF received-segment (+ index 2))) (AREF received-segment (+ index 3)))))
	    ;1; insure proper memory alignment*
	    (DECF maximum-send-size (MOD maximum-send-size 4)))
	  t))))) 

    

(DEFUN handler-syn-sent ()
  "2Perform actions required by incoming segment when the TCB is in the SYN-SENT state (RFC793 p66).*"
  (DECLARE (:self-flavor tcp-connection))
  (BLOCK syn-sent
    (WHEN (segment-ack-p received-segment)
      (WHEN (OR (<=seq (segment-ack-# received-segment) initial-send-sequence-#)
		(>seq (segment-ack-# received-segment) send-next))
	(WHEN (NOT (tcp-header-reset-p received-segment))
	  (tcp-send-reset source-address source-port (segment-source-address received-segment)
			  (tcp-header-source-port received-segment) (segment-ack-# received-segment)))
	(RETURN-FROM syn-sent :exit)))
    (WHEN (tcp-header-reset-p received-segment)
      (IF (segment-ack-p received-segment)
	  (FERROR 'connection-reset "connection reset" self)
	  (RETURN-FROM syn-sent :exit)))
    ;1; should check security and precedence here*
    (WHEN (tcp-header-syn-p received-segment)
      (check-syn-option)
      (SETF initial-receive-sequence-# (segment-sequence-# received-segment))
      (SETF receive-next (+seq (segment-sequence-# received-segment) 1))
      (SETF out-sequence receive-next)
      (SETF return-sequence out-sequence)
      (WHEN (segment-ack-p received-segment)
	(update-send-unack (segment-ack-# received-segment))
	(SETF send-window (tcp-header-window received-segment))
	(SETF send-wl1 (segment-sequence-# received-segment))
	(SETF send-wl2 (segment-ack-# received-segment)))
      (COND
	((<seq initial-send-sequence-# send-unack)
	 (SETF state :established)
	 (scan-rd-q)
	 (resegmentize)
	 (SETF ack-due-p t)
	 (trim-segment)
	 (RETURN-FROM syn-sent :urgent-check))
	(t (SETF state :syn-received)
	   (SEND *tcp-handler* :link-tcb-to-retransmit-list self nil)
	   ;1; this differs from spec because send-syn increments send-next*
	   (SETF send-next initial-send-sequence-#) (send-syn t)
	   (WHEN (PLUSP
		   (+ (tcp-header-fin-flag received-segment)
		      (- (segment-size received-segment) (segment-index received-segment))))
	     (RETURN-FROM syn-sent :remote-probe)))))
    (RETURN-FROM syn-sent :exit))) 



(DEFUN sequence-check (&aux sequence-length ok)
  "2Perform the initial checking for acceptability of the incoming segment (RFC793 p69).
Segments which lie outside the window are modified to represent only the acceptable portions.*"
  (DECLARE (:self-flavor tcp-connection))
  (SETF sequence-length (+ (tcp-header-syn-flag received-segment) (tcp-header-fin-flag received-segment)
			   (- (segment-size received-segment) (segment-index received-segment))))
  (WHEN (OR (>seq (segment-sequence-# received-segment) receive-next)
	    (>seq receive-next
		  (+ (segment-sequence-# received-segment)
		     (IF (ZEROP sequence-length)
			 0
			 (1- sequence-length)))))
    (INCF segments-out-of-sequence))
  (SETF ok
	(IF (ZEROP sequence-length)
	    (IF (ZEROP receive-window)
		(= (segment-sequence-# received-segment) receive-next)
		(AND (<=seq receive-next (segment-sequence-# received-segment))
		     (<seq (segment-sequence-# received-segment) (+seq receive-next receive-window))))
	    (WHEN (NOT (ZEROP receive-window))
	      (IF (<=seq receive-next (segment-sequence-# received-segment))
		  (<seq (segment-sequence-# received-segment) (+seq receive-next receive-window))
		  (<=seq receive-next (+seq (segment-sequence-# received-segment) sequence-length -1))))))
  (COND
    ((OR ok (AND (ZEROP receive-window) (= (segment-sequence-# received-segment) receive-next)))
     (WHEN (AND (NOT ok) (NOT (tcp-header-reset-p received-segment)))
       (SETF ack-due-p t))
     (trim-segment)
     :reset-check)
    (t (WHEN (AND (NOT (tcp-header-reset-p received-segment)))
	 (SETF ack-due-p t))
       (INCF segments-rejected)
       :exit))) 



(DEFUN trim-segment (&aux index-increment size-decrement)
  "2Trim a segment to the receive window by adjusting segment-index and segment-size.*"
  (DECLARE (:self-flavor tcp-connection))
  ;1; segment-index/sequence-# must be increased to indicate text/control only on or after leading edge of window*
  (WHEN (PLUSP (SETF index-increment (-seq receive-next (segment-sequence-# received-segment))))
    (WHEN (tcp-header-syn-p received-segment)
      (SETF (tcp-header-syn-p received-segment) ())
      (DECF index-increment))
    (SETF (segment-sequence-# received-segment)
	  (+seq (segment-sequence-# received-segment) index-increment))
    (WHEN (tcp-header-urgent-p received-segment)
      (WHEN (ZEROP
	      (SETF (tcp-header-urgent-pointer received-segment)
		    (MAX 0 (- (tcp-header-urgent-pointer received-segment) index-increment))))
	(SETF (tcp-header-urgent-p received-segment) ())))
    (INCF (segment-index received-segment) index-increment))
  ;1; segment-size must be reduced to indicate text/control only before trailing edge of window*
  ;1; Note: zero receive window probes do not go through trailing edge trimming*
  (WHEN (AND (PLUSP receive-window)
	     (PLUSP (SETF size-decrement
			  (-seq
			    (+seq (segment-sequence-# received-segment)
				  (tcp-header-syn-flag received-segment)
				  (- (segment-size received-segment) (segment-index received-segment))
				  (tcp-header-fin-flag received-segment))
			    (+seq receive-next receive-window)))))
    (WHEN (tcp-header-fin-p received-segment)
      (SETF (tcp-header-fin-p received-segment) ())
      (DECF size-decrement))
    (SETF (tcp-header-push-p received-segment) ())
    (WHEN (PLUSP size-decrement)
      (SETF (tcp-header-push-p received-segment) ()))
    (DECF (segment-size received-segment) size-decrement)))



(DEFUN reset-check ()
  "2Perform actions required by reset segment arrival (RFC793 p70).*"
  (DECLARE (:self-flavor tcp-connection))
  (COND
    ((tcp-header-reset-p received-segment)
     (CASE state
	   (:syn-received
	    (CASE open-mode
		  (:active
		   (FERROR 'connection-refused "connection refused" self))
		  (otherwise
		   (SETF state :listen)
		   (SETF maximum-send-size *tcp-maximum-segment-size*)
		   (SETF source-address ())
		   (SETF destination-port open-destination-port)
		   (SETF destination-address open-destination-address)
		   (WHEN remote-probe-segment
		     (DEALLOCATE-RESOURCE 'tcp-segment remote-probe-segment)
		     (SETF remote-probe-segment nil))
		   (SEND *tcp-handler* :link-tcb-to-retransmit-list self nil))))
	   ((:established :fin-wait-1 :fin-wait-2 :close-wait)
	    (FERROR 'connection-reset "connection reset" self))
	   ((:closing :last-ack :time-wait)
	    (COND
	      ((NOT (= out-sequence fin-sequence))
	       (FERROR 'connection-reset "connection reset" self))
	      ((EQ state :time-wait) (SEND *tcp-handler* :delete-tcb self))
	      (t (FUNCALL close-complete-handler)
		 (SEND *tcp-handler* :delete-tcb self)))))
     :exit)
    (t :syn-check))) 



(DEFUN syn-check ()
  "2Perform actions required by syn segment arrival (RFC793 p71).*"
  (DECLARE (:self-flavor tcp-connection))
  (WHEN (tcp-header-syn-p received-segment)
    (CASE state
	  ((:syn-received :established :fin-wait-1 :fin-wait-2 :close-wait :closing :last-ack
			  :time-wait)
	   (FERROR 'connection-reset "connection reset" self))))
  :ack-check) 



(DEFUN ack-check ()
  "2Perform actions required by ack segment arrival (RFC793 p72).*"
  (DECLARE (:self-flavor tcp-connection))
  (BLOCK ack
    (WHEN (NOT (segment-ack-p received-segment))
      (RETURN-FROM ack :exit))
    ;1; note state may change and processing continue*
    (WHEN (EQ state :syn-received)
      (COND
	((AND (<=seq send-unack (segment-ack-# received-segment))
	      (<=seq (segment-ack-# received-segment) send-next))
	 (SETF state :established)
	 (scan-rd-q)
	 (SETF send-window (tcp-header-window received-segment))
	 (SETF send-wl1 (segment-sequence-# received-segment))
	 (SETF send-wl2 (segment-ack-# received-segment))
	 (resegmentize))
	(t
	 (tcp-send-reset source-address source-port (segment-source-address received-segment)
			 (tcp-header-source-port received-segment) (segment-ack-# received-segment))
	 (RETURN-FROM ack :exit))))
    (CASE state
	  ((:established :fin-wait-1 :fin-wait-2 :close-wait :closing)
	   ;1; this logic is clarified by the MIL-STD 1778 spec p. 111, 121, 159*
	   (WHEN (<seq send-next (segment-ack-# received-segment))
	     (SETF ack-due-p t)
	     (RETURN-FROM ack :exit))
	   (WHEN (<seq send-unack (segment-ack-# received-segment))
	     (update-send-unack (segment-ack-# received-segment)))
	   (WHEN (OR (<seq send-wl1 (segment-sequence-# received-segment))
		     (AND (= send-wl1 (segment-sequence-# received-segment))
			  (<seq send-wl2 (segment-ack-# received-segment))))
	     (SETF send-window (tcp-header-window received-segment))
	     (SETF send-wl1 (segment-sequence-# received-segment))
	     (SETF send-wl2 (segment-ack-# received-segment)))))
    (WHEN (AND (EQ state :fin-wait-1) (= send-next (segment-ack-# received-segment)))
      (SETF state :fin-wait-2))
    (WHEN (EQ state :closing)
      (COND
	((= send-next (segment-ack-# received-segment))
	 (SETF state :time-wait)
	 (SETF time-wait-timestamp (TIME-INCREMENT (TIME) (* 2 *tcp-maximum-segment-lifetime*)))
	 (SEND *tcp-handler* :delink-tcb-from-retransmit-list self)
	 (WHEN (= out-sequence fin-sequence)
	   (FUNCALL close-complete-handler)
	   (SEND *tcp-handler* :link-tcb-to-time-wait-list self)))
	(t (RETURN-FROM ack :exit))))
    (WHEN (EQ state :last-ack)
      (WHEN (= send-next (segment-ack-# received-segment))
	(SETF state :closed)
	(SEND *tcp-handler* :delink-tcb-from-retransmit-list self)
	(WHEN (= out-sequence fin-sequence)
	  (FUNCALL close-complete-handler)
	  (SEND *tcp-handler* :delete-tcb self)))
      (RETURN-FROM ack :exit))
    (WHEN (EQ state :time-wait)
      (WHEN (tcp-header-fin-p received-segment)
	(SETF time-wait-timestamp (TIME-INCREMENT (TIME) (* 2 *tcp-maximum-segment-lifetime*)))
	(SETF ack-due-p t))
      (RETURN-FROM ack :exit))
    (RETURN-FROM ack :urgent-check))) 



(DEFUN urgent-check ()
  "2Perform actions required by urgent segment (RFC793 p73).*"
  (DECLARE (:self-flavor tcp-connection))
  (WHEN (tcp-header-urgent-p received-segment)
    (CASE state
	  ((:established :fin-wait-1 :fin-wait-2)
	   (WHEN (NOT receive-urgent-pointer)
	     (FUNCALL urgent-handler))
	   (SETF receive-urgent-pointer
		 (IF (NOT receive-urgent-pointer)
		     (+seq (segment-sequence-# received-segment)
			   (tcp-header-urgent-pointer received-segment))
		     (maxseq receive-urgent-pointer
			     (+seq (segment-sequence-# received-segment)
				   (tcp-header-urgent-pointer received-segment))))))))
  :remote-probe-check) 



(DEFUN process-segment-text (&aux seg-start seg-end p new-receive-next old-receive-window invariant)
  "2Place segment text in buffer management scheme (RFC793 p74).  This method returns either segment or nil,
nil signifying that the segment was added to the out-of-order queue.*"
  (DECLARE (:self-flavor tcp-connection))
  (BLOCK process-segment-text
    (CASE state
	  ((:established :fin-wait-1 :fin-wait-2))
	  (otherwise (RETURN-FROM process-segment-text received-segment)))
    ;1; process only non-empty segments*
    (WHEN (NOT (> (segment-size received-segment) (segment-index received-segment)))
      (RETURN-FROM process-segment-text received-segment))
    (SETF seg-start (segment-sequence-# received-segment))
    (SETF seg-end
	  (+seq (segment-sequence-# received-segment)
		(- (segment-size received-segment) (segment-index received-segment))))
    ;1; scan backward across segments on the out-of-order queue, to find position*
    (SETF p
	  (DO ((r out-of-order-q-end (segment-back-link r))
	       (s () r))
	      ((OR (NULL r)
		   (>=seq seg-start
			  (+seq (segment-sequence-# r) (- (segment-size r) (segment-index r)))))
	       s)))
    ;1; scan forward across segments on the out-of-order queue, to eliminate redundant data*
    (DO (p-start
	 p-end)
	((NULL p))
      (SETF p-start (segment-sequence-# p))
      (SETF p-end (+seq (segment-sequence-# p) (- (segment-size p) (segment-index p))))
      (COND
	;1; p is consumed by segment: release p, move p forward, and iterate*
	((<seq p-end seg-end)
	 (SETF p
	       (PROG1
		 (segment-link p)
		 (IF (segment-back-link p)
		     (SETF (segment-link (segment-back-link p)) (segment-link p))
		     (SETF out-of-order-q (segment-link p)))
		 (IF (segment-link p)
		     (SETF (segment-back-link (segment-link p)) (segment-back-link p))
		     (SETF out-of-order-q-end (segment-back-link p)))
		 (DEALLOCATE-RESOURCE 'tcp-segment p))))
	;1; decrement segment size (prevent overlap)*
	((>seq seg-end p-start) (DECF (segment-size received-segment) (-seq seg-end p-start)) (RETURN))
	(t (RETURN))))
    ;1; segment consumed by pre-existing segments on the out-of-order queue*
    (WHEN (NOT (> (segment-size received-segment) (segment-index received-segment)))
      (RETURN-FROM process-segment-text received-segment))
    ;1; link segment on out-of-order queue*
    (COND
      ((NULL p) (SETF (segment-link received-segment) ())
		(IF (SETF (segment-back-link received-segment) out-of-order-q-end)
		    (SETF (segment-link out-of-order-q-end) received-segment)
		    (SETF out-of-order-q received-segment))
		(SETF out-of-order-q-end received-segment))
      (t (SETF (segment-link received-segment) p)
	 (IF (SETF (segment-back-link received-segment) (segment-back-link p))
	     (SETF (segment-link (segment-back-link p)) received-segment)
	     (SETF out-of-order-q received-segment))
	 (SETF (segment-back-link p) received-segment)))
    ;1; advance receive-next and update receive-window as per RFC813*
    (DO ((r out-of-order-q out-of-order-q)
	 (s-end receive-next))
	((OR (NULL r) (NOT (= s-end (segment-sequence-# r))))
	 (SETF new-receive-next (WHEN (NOT (= s-end receive-next))
				  s-end)))
      ;1; move in order segments to the return queue*
      (SETF s-end (+seq (segment-sequence-# r) (- (segment-size r) (segment-index r))))
      (IF (SETF out-of-order-q (segment-link r))
	  (SETF (segment-back-link out-of-order-q) ())
	  (SETF out-of-order-q-end ()))
      (SETF (segment-link r) ())
      (SETF (segment-back-link r) return-q-end)
      (IF return-q
	  (SETF (segment-link return-q-end) r)
	  (SETF return-q r))
      (SETF return-q-end r))
    (WHEN new-receive-next
      (SETF invariant (+seq receive-next receive-window))
      (SETF old-receive-window receive-window)
      (SETF receive-next new-receive-next)
      (SETF receive-window
	    (IF (<= (* 2 (-seq invariant receive-next))
		    (- rd-registered-length (-seq receive-next return-sequence)))
		(- rd-registered-length (-seq receive-next return-sequence))
		(-seq invariant receive-next)))
      ;1; when receive-window opens, process queued remote-probe-segment*
      (WHEN (AND (ZEROP old-receive-window) (PLUSP receive-window) remote-probe-segment)
	(queue remote-probe-segment receive-q receive-q-end)
	(SETF remote-probe-segment ())))
    ;1; handle acknowledgment of in order segment*
    (WHEN (NOT (= latest-leading-edge receive-next))
      (COND
	;1; no new window information, do a full delay or attempt to piggyback acknowlegment*
	((= latest-trailing-edge (+seq receive-next receive-window))
	 (WHEN (NOT delayed-ack-timestamp)
	   (SEND *tcp-handler* :link-tcb-to-delayed-ack-list self
		 (TIME-INCREMENT (TIME) (IF (NOT (tcp-header-push-p received-segment))
					    ;1; delay acknowledgement as per RFC813*
					    (+ smoothed-intersegment-arrival-time *delayed-ack-fudge*)
					    ;1; attempt to piggyback ack with text or new window information*
					    *piggybacked-ack-fudge*)))))
	(t (SETF ack-due-p t))))
    ;1; return data to user*
    (LOOP
      (MULTIPLE-VALUE-BIND (LENGTH push-p urgent-p)
	  (receive-match)
	(IF length
	    (FUNCALL buffer-handler length push-p urgent-p)
	    (RETURN))))
    (RETURN-FROM process-segment-text ()))) 



(DEFUN fin-check (segment-end-sequence)
  "2Perform actions required by inbound segment with FIN (RFC793 p75).*"
  (DECLARE (:self-flavor tcp-connection))
  (WHEN (tcp-header-fin-p received-segment)
    (CASE state
	  ((:closed :listen :syn-sent))
	  (otherwise (SETF fin-sequence segment-end-sequence))))
  ;1; process fin only in order*
  (WHEN (AND fin-sequence (= receive-next fin-sequence))
    (CASE state
	  ((:established :syn-received)
	   (process-fin)
	   (SETF state :close-wait))
	  (:fin-wait-1
	   (process-fin)
	   (COND
	     ((= send-next send-unack)
	      (SETF state :time-wait)
	      (SETF time-wait-timestamp (TIME-INCREMENT (TIME) (* 2 *tcp-maximum-segment-lifetime*)))
	      (SEND *tcp-handler* :delink-tcb-from-retransmit-list self)
	      (WHEN (= out-sequence fin-sequence)
		(FUNCALL close-complete-handler)
		(SEND *tcp-handler* :link-tcb-to-time-wait-list self)))
	     (t (SETF state :closing))))
	  (:fin-wait-2
	   (SETF state :time-wait)
	   (SETF time-wait-timestamp (TIME-INCREMENT (TIME) (* 2 *tcp-maximum-segment-lifetime*)))
	   (process-fin)
	   (SEND *tcp-handler* :delink-tcb-from-retransmit-list self)
	   (WHEN (= out-sequence fin-sequence)
	     (FUNCALL close-complete-handler)
	     (SEND *tcp-handler* :link-tcb-to-time-wait-list self)))))) 



(DEFUN process-fin ()
  (DECLARE (:self-flavor tcp-connection))
  ;1; advance receive-next past the fin*
  (SETF receive-window 0)
  (SETF receive-next (+seq receive-next 1))
  (SETF ack-due-p t)
  (WHEN process-fin-handler (FUNCALL process-fin-handler))
  (MULTIPLE-VALUE-BIND (LENGTH push-p urgent-p)
      (receive-match)
    (WHEN length
      (FUNCALL buffer-handler length push-p urgent-p)))
  ;1; pass fin to user (in order)*
  (WHEN (= return-sequence fin-sequence)
    (FUNCALL receive-fin-handler))
  (SEND self :clean-up-rd-q)) 


;
;1 receive descriptor management*
;

(DEFUN add-rd (length &aux new-rd)
  "2Add a receive descriptor (rd) to the end of receive descriptor queue.*"
  (DECLARE (:self-flavor tcp-connection))
  (SETF new-rd (ALLOCATE-RESOURCE 'tcp-rd-resource length))
  (IF rd-q-end
      (SETF (rd-link rd-q-end) new-rd)
      (SETF rd-q new-rd))
  (SETF rd-q-end new-rd)) 



(DEFUN prescan-rd-q ()
  "2This method is used in syn-received state to determine the size of the receive-window to advertise,
prior to the actual registering of the receive descriptors.*"
  (DECLARE (:self-flavor tcp-connection))
  (DO ((rd rd-q (rd-link rd))
       (syn-window 0))
      ((OR (NULL rd)
	   (NOT (EQ (rd-state rd) :pending))
	   ;1; don't consider more than the window can hold*
	   (> (+ syn-window (rd-length rd)) #.(1- (EXPT 2 16))))
       ;1; return value*
       syn-window)
    (WHEN (EQ (rd-state rd) :pending)
      (INCF syn-window (rd-length rd))))) 
    


(DEFUN scan-rd-q ()
  "2Scan rd-q register all possible receive descriptors.*"
  (DECLARE (:self-flavor tcp-connection))
  (DO ((rd rd-q (rd-link rd))
       (old-receive-window receive-window))
      ((OR (NULL rd)
	   ;1; don't register more than the window can hold*
	   (AND (EQ (rd-state rd) :pending)
		(> (+ rd-registered-length (rd-length rd)) #.(1- (EXPT 2 16)))))
       ;1; update receive-window as per RFC813*
       (WHEN (<= (* 2 receive-window) (- rd-registered-length (-seq receive-next return-sequence)))
	 (SETF receive-window (- rd-registered-length (-seq receive-next return-sequence))))
       ;1; send delayed acknowledgement immediately when window opens*
       (WHEN (AND delayed-ack-timestamp (NOT (EQUAL old-receive-window receive-window)))
	 (SETF ack-due-p t))
       ;1; when receive-window opens, process queued remote-probe-segment*
       (WHEN (AND (ZEROP old-receive-window) (PLUSP receive-window))
	 ;1; according to the spec this new window information will be rejected, unless the seq or ack numbers increase*
	 (WHEN *window-kludge-switch* (SETF ack-due-p t))
	 (WHEN remote-probe-segment
	   (queue remote-probe-segment receive-q receive-q-end)
	   (SETF remote-probe-segment ()))))
    (WHEN (EQ (rd-state rd) :pending)
      (INCF rd-registered-length (rd-length rd))
      (SETF (rd-state rd) :registered)))) 




(DEFUN dequeue-rd (&aux return-rd)
  "2Dequeue first element from the receive descriptor queue.  Also register pending rds.*"
  (DECLARE (:self-flavor tcp-connection))
  (WHEN rd-q
    (SETF return-rd rd-q)
    (WHEN (NULL (SETF rd-q (rd-link rd-q)))
      (SETF rd-q-end ()))
    (WHEN (EQ (rd-state return-rd) :registered)
      (DECF rd-registered-length (rd-length return-rd)))
    (scan-rd-q)
    return-rd)) 



(DEFMETHOD (tcp-connection :clean-up-rd-q) ()
  "2Remove all receive descriptors.*"
  (DO ((p rd-q))
      ((NULL p)
       (SETF rd-q ())
       (SETF rd-q-end ())
       (SETF rd-registered-length 0))
    (SETF p (PROG1
	      (rd-link p)
	      (DEALLOCATE-RESOURCE 'tcp-rd-resource p))))) 



(DEFUN receive-match (&aux p sequence-number length push-p)
  "2Match data on the return queue with receive descriptors. Return values are:
NIL - no match was made.
LENGTH PUSH-P URGENT-P - a match was made.*"
  (DECLARE (:self-flavor tcp-connection))
  (BLOCK receive-match
    (WHEN (OR (NULL rd-q) (NOT (EQ (rd-state rd-q) :registered)))
      (RETURN-FROM receive-match ()))
    ;1; scan backwards to find segment containing return-sequence*
    (SETF p
	  (DO ((p return-q-end (segment-back-link p))
	       (q nil p))
	      ((OR (NULL p)
		   (>=seq return-sequence
			  (+seq (segment-sequence-# p) (- (segment-size p) (segment-index p)))))
	       q)))
    ;1; scan forward acrosss segments on return queue*
    (SETF sequence-number return-sequence)
    (DO ()
	;1; no match right now*
	((NULL p)
	 (RETURN-FROM receive-match ()))
      (SETF length
	    (-seq (+seq (segment-sequence-# p) (- (segment-size p) (segment-index p)))
		  return-sequence))
      (COND
	;1; receive fully satisfied*
	((<= (rd-length rd-q) length)
	 (SETF length (rd-length rd-q))
	 (SETF push-p (tcp-header-push-p p))
	 (RETURN))
	;1; receive partially satisfied*
	((OR (tcp-header-push-p p) (tcp-header-fin-p p))
	 (SETF push-p t)
	 (RETURN)))
      (SETF p (segment-link p)))
    ;1; return match values*
    (DEALLOCATE-RESOURCE 'tcp-rd-resource (dequeue-rd))
    (SETF return-sequence (+seq return-sequence length))
    (VALUES length push-p
	    (WHEN receive-urgent-pointer
	      (COND
		((<=seq (+seq sequence-number length) receive-urgent-pointer)
		 (-seq (minseq receive-urgent-pointer (+seq sequence-number length))
		       sequence-number))
		(t (SETF receive-urgent-pointer ()))))))) 


;
;1 initialization and termination*
;

(DEFMETHOD (tcp-connection :initialize) (new-buffer-handler new-urgent-handler
					 new-process-fin-handler new-receive-fin-handler 
					 new-close-complete-handler new-user-timeout-handler
					 new-condition-handler)
  (SETF retransmit-link ())
  (SETF delayed-ack-link ())
  (SETF time-wait-link ())
  (SETF port-link ())
  (SETF lock ())
  (SETF state :closed)
  (SETF source-address ())
  (SETF source-port ())
  (SETF destination-address ())
  (SETF destination-port ())
  (SETF user-timeout (* 5 60))
  (SETF send-q ())
  (SETF send-q-end ())
  (SETF send-unack ())
  (SETF send-next ())
  (SETF send-window ())
  (SETF send-wl1 ())
  (SETF send-wl2 ())
  (SETF window-probe-active-p ())
  (SETF initial-send-sequence-# ())
  (SETF maximum-send-size *tcp-maximum-segment-size*)
  (SETF number-of-text-segments-sent 0)
  (SETF number-of-segments-sent 0)
  (SETF maximum-text-sent 0)
  (SETF total-text-sent 0)
  (SETF retransmit-q ())
  (SETF retransmit-q-end ())
  (SETF retransmit-timestamp nil)
  (SETF retransmission-timeout *initial-retransmission-timeout*
	smoothed-round-trip-time ())
  (SETF number-of-retransmissions 0)
  (SETF last-receive-time nil)
  (SETF receive-q ()
	receive-q-end ())
  (SETF received-segment ())
  (SETF out-of-order-q ()
	out-of-order-q-end ())
  (SETF return-q ()
	return-q-end ())
  (SETF rd-q ()
	rd-q-end ())
  (SETF rd-registered-length 0)
  (SETF initial-receive-sequence-# ())
  (SETF maximum-receive-size *tcp-maximum-segment-size*)
  (SETF receive-urgent-pointer ())
  (SETF receive-next ())
  (SETF fin-sequence ())
  (SETF receive-window 0)
  (SETF remote-probe-segment ())
  (SETF not-push-time-p ())
  (SETF smoothed-intersegment-arrival-time 15)
  (SETF delayed-ack-timestamp ())
  (SETF ack-due-p nil)
  (SETF time-wait-timestamp ())
  (SETF segments-out-of-sequence 0)
  (SETF segments-rejected 0)
  (SETF maximum-text-received 0)
  (SETF total-text-received 0)
  (SETF number-of-text-segments-received 0)
  (SETF number-of-segments-received 0)
  (SETF buffer-handler new-buffer-handler)
  (SETF urgent-handler new-urgent-handler)
  (SETF process-fin-handler new-process-fin-handler)
  (SETF receive-fin-handler new-receive-fin-handler)
  (SETF close-complete-handler new-close-complete-handler)
  (SETF user-timeout-handler new-user-timeout-handler)
  (SETF condition-handler new-condition-handler)) 


(COMPILE-FLAVOR-METHODS tcp-connection)
