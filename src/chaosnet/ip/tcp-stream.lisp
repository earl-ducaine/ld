;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(CPTFONT HL12B TR12BI) -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986, 1988 Texas Instruments Incorporated.  All rights reserved.*

;1;;-------------------------------------------------------------------------------------*
;1;;                       T C P   S T R E A M S*
;1;;*
;1;; ADVERTISED FUNCTIONS:*
;1;;   open-stream*
;1;;*
;1;; ADVERTISED METHODS OF STREAMS:*
;1;;    All streams:       :close, :foreign-host, :status*
;1;;    Input streams:    :next-input-buffer, :discard-input-buffer*
;1;;    Output streams:  :new-output-buffer, :send-output-buffer, :discard-output-buffer,*
;1;;                      :turn-on-urgent-mode, :turn-off-urgent-mode*
;1;;*
;1;;-------------------------------------------------------------------------------------*

(DEFVAR *tcp-stream-whostate* nil
  "2Whostate used by process wait.
This must be nil, and only bound within a let.*")


(DEFVAR *tcp-stream-instantiator* nil
  "2Function used to instantiate streams in ip:make-stream.
This must be nil, and only bound within a let.*")


(DEFMACRO with-stream-whostate (whostate-string &body body)
  "2Modify the process-wait whostate during execution of body.*"
  `(LET ((ip:*tcp-stream-whostate* ,whostate-string))
     ,@body))


(DEFMACRO with-connection-lock (&body body)
  "<Lock the connection nonrecursively during body.*"
  `(with-tcb-lock connection ,@body))


(DEFPARAMETER *default-tcp-stream-input-buffer-size* 2048) 

;1;------*
;1; signals and condition handlers*
;1;*

(DEFFLAVOR tcp-stream-error
	   ()
	   (tcp-error)) 


(DEFSIGNAL host-not-responding-during-connection
   (tcp-stream-error host-not-responding-during-connection sys:connection-error) (connection)
   "open timed out before connection became established")

;1;------*
;1; tcp-stream-binary-segment resource - used for 16 bit characters in binary streams*
;1;*
(DEFMACRO tcp-stream-actual-array (binary-segment)
  `(si:array-indirect-to ,binary-segment))


(DEFUN allocate-tcp-stream-binary-segment (*tcp-stream-segment* *tcp-stream-segment-beg* *tcp-stream-segment-end*)
  "2Displace a binary segment on top of a TCP segment.*"
  (DECLARE (SPECIAL *tcp-stream-segment* *tcp-stream-segment-beg* *tcp-stream-segment-end*))
  (ALLOCATE-RESOURCE 'tcp-stream-binary-segment))


(DEFUN deallocate-tcp-stream-binary-segment (binary-segment &optional *tcp-stream-deallocate-displaced-array-only*)
  "2Deallocate a binary segment displaced over a TCP segment, and conditionally deallocate the TCP segment.*"
  (DECLARE (SPECIAL *tcp-stream-deallocate-displaced-array-only*))
  (DEALLOCATE-RESOURCE 'tcp-stream-binary-segment binary-segment))


(DEFRESOURCE tcp-stream-binary-segment ()
  :constructor (LOCALLY
		 (DECLARE (SPECIAL *tcp-stream-segment* *tcp-stream-segment-beg* *tcp-stream-segment-end*))
		 (MAKE-ARRAY (TRUNCATE (- *tcp-stream-segment-end* *tcp-stream-segment-beg*) 2)
			   :displaced-to *tcp-stream-segment*
			   :displaced-index-offset (TRUNCATE *tcp-stream-segment-beg* 2)
			   :element-type '(unsigned-byte 16)
			   :area *ip-area*))
  :initializer (LOCALLY
		 (DECLARE (SPECIAL *tcp-stream-segment* *tcp-stream-segment-beg* *tcp-stream-segment-end*))
		 (si:change-indirect-array object
					   (ARRAY-TYPE object)
					   (TRUNCATE (- *tcp-stream-segment-end* *tcp-stream-segment-beg*) 2)
					   *tcp-stream-segment*
					   (TRUNCATE *tcp-stream-segment-beg* 2)))
  :matcher t
  :deallocator (LOCALLY
		 (DECLARE (SPECIAL *tcp-stream-deallocate-displaced-array-only*))
		 (UNLESS *tcp-stream-deallocate-displaced-array-only*
		   (DEALLOCATE-RESOURCE 'tcp-segment (tcp-stream-actual-array object)))))

;1;------*
;1; tcp-stream-ascii-buffer resource - used by ascii translating output streams*
;1;*

(DEFMACRO buffer-push-p (ascii-buffer)
  `(ARRAY-LEADER ,ascii-buffer 1)) 


(DEFRESOURCE tcp-stream-ascii-buffer ()
  :constructor (MAKE-ARRAY (- *tcp-maximum-segment-size* *tcp-minimum-header-size*)
			   :element-type '(unsigned-byte 8)
			   :leader-length 2
			   :area *ip-area*)
  :initializer (SETF (buffer-push-p object) ())
  :matcher t) 
			     

;1;------*
;1; This is included in all tcp streams, input or output*
;1;*
(DEFFLAVOR basic-stream
	   ((host nil)
	    connection
	    (fin-received-p nil)
	    (condition-handler-entered-p nil)
	    ;1; used during the established phase of the connection (seconds)*
	    (timeout (* 5 60)))
	   ()
  (:included-flavors stream)
  (:inittable-instance-variables host connection)
  (:gettable-instance-variables connection fin-received-p)
  (:settable-instance-variables timeout))


(DEFMETHOD (basic-stream :before :set-timeout) (new-timeout)
  (SETF (SEND connection :user-timeout) new-timeout))


(DEFMETHOD (basic-stream :network-type) ()
  :tcp)


(DEFMETHOD (basic-stream :foreign-host) ()
  (COND
    (host)
    (t (SETF host (si:get-host-from-address (SEND connection :destination-address) :ip)))))


(DEFMETHOD (basic-stream :close) (&optional abort-p)
  ;1; allow multiple closes without error*
  (UNLESS (EQ :closed (SEND self :status))
    (COND
      (abort-p (SEND connection :abort))
      (t ;1; queue a receive for the fin*
	 (SEND connection :receive 1)
	 (SEND connection :close)
	 ;1; wait for fin acknowledgment (rely on tcp user timeout)*
	 (PROCESS-WAIT (OR *tcp-stream-whostate* "TCP Close")
		       #'(lambda (stream)
			   (CASE (SEND stream :status)
			     ((:fin-wait-2 :closed) t)
			     (otherwise nil)))
		       self)
	 ;1; if :condition-handler method entered in background, cause error to be signaled*
	 (WHEN condition-handler-entered-p (SEND connection :abort))
	 ;1; insure that a fin is received*
	 (UNLESS fin-received-p
	   (UNLESS (PROCESS-WAIT-WITH-TIMEOUT (OR *tcp-stream-whostate* "TCP Close")
					      (WHEN timeout (* timeout 60))
					      #'(lambda (stream)
						  (SEND stream :fin-received-p))
					      self)
	     (SEND self :timeout-handler))))))) 


(DEFMETHOD (basic-stream :load-receives) ()) 


(DEFMETHOD (basic-stream :buffer-handler) (ignore ignore ignore)) 


(DEFMETHOD (basic-stream :urgent-handler) ()) 


(DEFMETHOD (basic-stream :process-fin-handler) ())


(DEFMETHOD (basic-stream :receive-fin-handler) ()
  (SETF fin-received-p t)) 


(DEFMETHOD (basic-stream :close-complete-handler) ()
  "2Return illegal-connection errors for most operations to a closed connection.*"
  (SETF connection (SEND *tcp-handler* :dummy-connection)))


(DEFMETHOD (basic-stream :timeout-handler) (&optional user-timeout-p)
  (let ((foreign-host (SEND self :foreign-host)))   ;get the foreign-host before it gets cleared. 12-07-87 DAB
    (UNLESS user-timeout-p (SEND connection :abort))
    (FERROR 'host-stopped-responding
	    "Foreign host ~*~a stopped responding.~%Timeout on ~:[receive~;acknowledgment of sent data~]."
	    connection foreign-host user-timeout-p)))

  
(DEFMETHOD (basic-stream :condition-handler) (condition)
  ;1; returning nil from this handler will decline to handle the condition*
  (SETF condition-handler-entered-p t)
  (SETF connection (SEND *tcp-handler* :dummy-connection condition))
  ()) 


(DEFMETHOD (basic-stream :status) ()
  "2Returns state, destination address, and destination port of the connection (or :closed).*"
  ;1; At the stream layer, we map :time-wait state into :closed state.*
  (MULTIPLE-VALUE-BIND (state dest-addr dest-port) (SEND connection :status)
    (CASE state
      ((:closed :time-wait) :closed)
      (otherwise (VALUES state dest-addr dest-port)))))


(DEFMETHOD (basic-stream :server-current-p) ()
  (NOT (EQ :closed (SEND self :status))))


;1;------*
;1; This is included in all tcp input streams, character and binary*
;1;*

(DEFFLAVOR basic-input-stream
	   ((input-buffer-size *default-tcp-stream-input-buffer-size*)
	    (number-of-input-buffers 1)
	    (pending-receives 0)
	    (pending-octet-count 0)
	    (urgent-input-index nil))
	   (basic-stream)
  (:included-flavors si:basic-buffered-input-stream)
  (:gettable-instance-variables pending-octet-count urgent-input-index)
  (:inittable-instance-variables input-buffer-size number-of-input-buffers))


(DEFMETHOD (basic-input-stream :process-fin-handler) (&aux segment)
  (WHEN (AND (EQ :input (SEND self :direction))
	     ;1; state machine has not transitioned into :close-wait state at this point*
	     (EQ :established (SEND self :status)))
    ;1; we cannot (send connection :close) because this would recursively enter the state machine*
    (COND ((SEND connection :send-q-end) (SETF (tcp-header-fin-p (SEND connection :send-q-end)) t))
	  (t (SETF segment (ALLOCATE-RESOURCE 'tcp-segment *tcp-minimum-header-size*))
	     (SETF (tcp-header-fin-p segment) t)
	     (SETF (tcp-header-ack-p segment) t)
	     (SETF (tcp-header-data-offset segment) 5)
	     (queue segment (SEND connection :send-q) (SEND connection :send-q-end))))
    ;1; process send-q will make the transition into last-ack state*
    ))


(DEFMETHOD (basic-input-stream :close) (&optional abort-p)
  ;1; allow multiple closes without error*
  (UNLESS (EQ :closed (SEND self :status))
    (COND (abort-p
	   (CASE (SEND self :status)
	     (:closed-with-data-available
	      (SEND *tcp-handler* :delete-tcb connection)
	      (SEND self :close-complete-handler))
	     (otherwise (SEND connection :abort)))
	   ;1; allow further calls to the :status method to properly return :closed status,*
	   ;1; even though the condition handler may not have been entered*
	   (SETF condition-handler-entered-p t))
	  (t (CASE (SEND self :status)
	       ((:established :close-wait) (SEND connection :close)))
	     ;1; wait for fin acknowledgement (rely on tcp user timeout)*
	     (PROCESS-WAIT (OR *tcp-stream-whostate* "TCP Close")
			   #'(lambda (stream)
			       (CASE (SEND stream :status)
				 ((:fin-wait-2 :closed :closed-with-data-available) t)
				 (otherwise nil)))		       
			   self)
	     ;1; if :condition-handler method entered in background, cause error to be signaled*
	     (WHEN condition-handler-entered-p (SEND connection :abort))
	     ;1; insure that all data is consumed*
	     (with-stream-whostate
	       "TCP Close"
	       (SEND self :read-until-eof)))))) 


(DEFMETHOD (basic-input-stream :buffer-handler) (length ignore ignore)
  ;1; atomically set buffer attributes*
  (WITHOUT-INTERRUPTS
    (DECF pending-receives input-buffer-size)
    (INCF pending-octet-count length)))


(DEFMETHOD (basic-input-stream :status) ()
  "2Returns state, destination address, and destination port of the connection (or :closed).*"
  ;1; At the stream layer, we map :time-wait state into :closed state.*
  ;1; We also indicate when data remains to be received even though the connection is closed.*
  (MULTIPLE-VALUE-BIND (state dest-addr dest-port) (SEND connection :status)
    (CASE state
      ((:closed :time-wait)
       (IF (OR condition-handler-entered-p
	       (AND fin-received-p (ZEROP pending-octet-count)))
	   :closed
	   :closed-with-data-available))
      (otherwise (VALUES state dest-addr dest-port)))))


(DEFMETHOD (basic-input-stream :tyi-no-hang) (&optional eof)
  (LOOP until (AND si::stream-input-buffer
		   (< si::stream-input-index si::stream-input-limit))
	;1;Out of input, get some more*
	unless (SEND self :setup-next-input-buffer t)
	;1;Reached end of file*
	return (WHEN eof
		 (CASE (SEND self :status)
		   ((:close-wait :last-ack :closed)
		    (FERROR 'sys:end-of-file-1 "End of file on ~S." self))
		   (otherwise
		    nil)))
	;1;Here we have a character available*
	finally (RETURN (PROG1 (AREF si::stream-input-buffer si::stream-input-index)
			       (INCF si::stream-input-index)))))


;1;------*
;1; This is included in all tcp output streams, character and binary*
;1;*

(DEFFLAVOR basic-output-stream
	   ((send-urgent-mode-p nil)
	    (urgent-output-index nil))
	   (basic-stream)
  (:included-flavors si:basic-buffered-output-stream)
  (:settable-instance-variables send-urgent-mode-p urgent-output-index)) 


(DEFMETHOD (basic-output-stream :flush-buffer) ()
  ;1; we do not timeout this wait, the TCP user-timeout will expire and signal an error*
  (PROCESS-WAIT (OR *tcp-stream-whostate* "TCP Send")
		#'(lambda (stream)
		    (OR (EQ (SEND stream :status) :closed)
			(NULL (SEND (SEND stream :connection) :send-q))))
		self)
  ;1; if error occurred in background, make it appear now*
  (WHEN condition-handler-entered-p (SEND connection :abort))) 


(DEFMETHOD (basic-output-stream :turn-on-urgent-mode) ()
  (SETF send-urgent-mode-p t
	urgent-output-index ())) 


(DEFMETHOD (basic-output-stream :turn-off-urgent-mode) ()
  (WHEN send-urgent-mode-p
    (SETF urgent-output-index si::stream-output-index
	  send-urgent-mode-p ()))) 


;1;------*

(DEFFLAVOR character-input-stream-mixin
	   ()
	   (basic-input-stream)
  (:included-flavors basic-stream si:basic-buffered-input-stream)) 


(DEFMETHOD (character-input-stream-mixin :element-type) ()
  'STRING-CHAR) 


(DEFMETHOD (character-input-stream-mixin :load-receives)
	   (&optional (high-water-mark (* (1- number-of-input-buffers) input-buffer-size)))
  (DO ()
      ((OR fin-received-p
	   (> (+ pending-octet-count pending-receives) high-water-mark)))
    ;1; prevent a race condition between fin arrival and issuing a receive*
    (CONDITION-CASE ()
	(PROGN
	 (INCF pending-receives input-buffer-size)
	 (MULTIPLE-VALUE-BIND (length push-p urgent-p) (SEND connection :receive input-buffer-size)
	   (WHEN length (SEND self :buffer-handler length push-p urgent-p))))
      (connection-closing))))


(DEFMETHOD (character-input-stream-mixin :next-input-buffer) (no-hang-p
							      &aux segment beg end text-space octet-deficit first-rd)
  (BLOCK next-input-buffer
    (IF (AND fin-received-p (ZEROP pending-octet-count))
	(RETURN-FROM next-input-buffer ())
	(SEND self :load-receives))
    (WHEN (AND (ZEROP pending-octet-count) (NOT no-hang-p))
      (PROCESS-WAIT-WITH-TIMEOUT (OR *tcp-stream-whostate* "TCP Input")
				 (WHEN timeout (* timeout 60))
				 #'(lambda (stream)
				     (OR (EQ :closed (SEND stream :status))
					 (SEND stream :fin-received-p)
					 (PLUSP (SEND stream :pending-octet-count))))
				 self))
    (COND
      ;1; force error that occurred in background to appear now*
      (condition-handler-entered-p (SEND connection :abort))
      ((ZEROP pending-octet-count)
       (UNLESS (OR no-hang-p fin-received-p (EQ :closed (SEND self :status)))
	 (SEND self :timeout-handler)))
      (t
       (SETF segment (SEND connection :return-q))
       (SETF beg (segment-index segment))
       (SETF end (segment-size segment))
       ;1; insure segment is covered by receives*
       (WHEN (< pending-octet-count (SETF text-space (- end beg)))
	 (SEND self :load-receives (1- text-space)))
       (with-connection-lock
	 ;1; insure segment is fully returned*
	 (WHEN (PLUSP (SETF octet-deficit (- text-space pending-octet-count)))
	   (INCF pending-octet-count octet-deficit)
	   (DECF (rd-length (SETF first-rd (SEND connection :rd-q))) octet-deficit)
	   (WHEN (EQ :registered (rd-state first-rd))
	     (DECF (SEND connection :rd-registered-length) octet-deficit))
	   (SETF (SEND connection :return-sequence) (+seq (SEND connection :return-sequence) octet-deficit)))
	 (IF (SETF (SEND connection :return-q) (segment-link segment))
	     (SETF (segment-back-link (SEND connection :return-q)) ())
	     (SETF (SEND connection :return-q-end) ()))
	 (DECF pending-octet-count text-space)
	 (WHEN (EQL (SETF (SEND connection :out-sequence) (+seq (SEND connection :out-sequence) text-space))
		    (SEND connection :return-sequence))
	   (CASE (SEND connection :state)
	     (:closed
	      (SEND *tcp-handler* :delete-tcb connection)
	      (SEND self :close-complete-handler))
	     (:time-wait 
	      (SEND *tcp-handler* :link-tcb-to-time-wait-list connection)
	      (SEND self :close-complete-handler))))
	 (SETF urgent-input-index (WHEN (tcp-header-urgent-p segment)
				    (MAX end (+ beg (tcp-header-urgent-pointer segment)))))
	 (VALUES segment beg end)))))) 
  

(DEFMETHOD (character-input-stream-mixin :discard-input-buffer) (segment)
  (DEALLOCATE-RESOURCE 'tcp-segment segment)) 


;1;------*

(DEFFLAVOR ascii-translating-input-stream-mixin
	   ((last-char-cr-p nil))
	   (basic-input-stream)
  (:included-flavors character-input-stream-mixin)) 


(DEFMETHOD (ascii-translating-input-stream-mixin :around :next-input-buffer) (cont mt args ignore)
  "2RFC854 pp. 11-12 contains the Netascii definition.  On input, CR NUL -> CR and CR LF -> #\newline.
CR should *not* appear in any other context, i.e. not followed by either NUL or LF.*"
  (MULTIPLE-VALUE-BIND (buffer start end) (AROUND-METHOD-CONTINUE cont mt args)
    (WHEN buffer
      (DO ((i start (1+ i))
	   (j start (1+ j))
	   (old-last-char-cr-p (PROG1
				 last-char-cr-p
				 (SETF last-char-cr-p ())))
	   look)
	  ((EQL i end)
	   (SETF end j))
	(SETF (AREF buffer j)
	      (CASE (AREF buffer i)
		(0 (IF (AND (EQL i start) old-last-char-cr-p)
		       13
		       0))
		(8 (CHAR-CODE #\Backspace))
		(9 (CHAR-CODE #\Tab))
		(10 (IF (AND (EQL i start) old-last-char-cr-p)
			(CHAR-CODE #\Newline)
			(CHAR-CODE #\Linefeed)))
		(12 (CHAR-CODE #\Page))
		(13 (COND ((EQL (SETF look (1+ i)) end)
			   (SETF last-char-cr-p t)
			   (DECF j)
			   ;1; dummy value to insert in array.  Translation not completed until next buffer.*
			   (CHAR-CODE #\Newline))
			  (t (SETF i look)
			     (IF (EQL (AREF buffer look) 10)
				 (CHAR-CODE #\Newline)
				 13))))
		(127 (CHAR-CODE #\Rubout))
		(t (AREF buffer i))))))
    (VALUES buffer start end))) 


;1;------*
;1; binary streams have an effective character size of 2 octets*
;1;*

(DEFFLAVOR binary-input-stream-mixin
	   ((saved-octet nil))
	   (basic-input-stream)
  (:included-flavors basic-stream si:basic-buffered-input-stream)) 


(DEFMETHOD (binary-input-stream-mixin :element-type) ()
  '(unsigned-byte 8))


(DEFMETHOD (binary-input-stream-mixin :buffer-handler) (length ignore ignore)
  ;1; atomically set buffer attributes*
  (WITHOUT-INTERRUPTS
    (DECF pending-receives (* input-buffer-size 2))
    (INCF pending-octet-count length)))


(DEFMETHOD (binary-input-stream-mixin :load-receives)
	   (&optional (high-water-mark (* (1- number-of-input-buffers) (* 2 input-buffer-size))))
  (DO ()
      ((OR fin-received-p
	   (> (+ pending-octet-count pending-receives) high-water-mark)))
    ;1; prevent a race condition between fin arrival and issuing a receive*
    (CONDITION-CASE ()
	(PROGN
	 (INCF pending-receives (* 2 input-buffer-size))
	 (MULTIPLE-VALUE-BIND (length push-p urgent-p) (SEND connection :receive (* 2 input-buffer-size))
	   (WHEN length (SEND self :buffer-handler length push-p urgent-p))))
      (connection-closing))))


(DEFMETHOD (binary-input-stream-mixin :next-input-buffer) (no-hang-p
							   &aux segment beg end text-space octet-deficit first-rd)
  (BLOCK next-input-buffer
    (IF (AND fin-received-p (ZEROP pending-octet-count))
	(RETURN-FROM next-input-buffer ())
	(SEND self :load-receives))
    (WHEN (AND (ZEROP pending-octet-count) (NOT no-hang-p))
      (PROCESS-WAIT-WITH-TIMEOUT (OR *tcp-stream-whostate* "TCP Input")
				 (WHEN timeout (* timeout 60))
				 #'(lambda (stream)
				     (OR (EQ :closed (SEND stream :status))
					 (SEND stream :fin-received-p)
					 (PLUSP (SEND stream :pending-octet-count))))
				 self))
    (COND
      ;1; force error that occurred in background to appear now*
      (condition-handler-entered-p (SEND connection :abort))
      ((ZEROP pending-octet-count)
       (UNLESS (OR no-hang-p fin-received-p (EQ :closed (SEND self :status)))
	 (SEND self :timeout-handler)))
      (t
       (SETF segment (SEND connection :return-q))
       ;1; insure segment is covered by receives*
       (WHEN (< pending-octet-count (SETF text-space (- (segment-size segment) (segment-index segment))))
	 (SEND self :load-receives (1- text-space)))
       (with-connection-lock
	 ;1; insure segment is fully returned*
	 (WHEN (PLUSP (SETF octet-deficit (- text-space pending-octet-count)))
	   (INCF pending-octet-count octet-deficit)
	   (DECF (rd-length (SETF first-rd (SEND connection :rd-q))) octet-deficit)
	   (WHEN (EQ :registered (rd-state first-rd))
	     (DECF (SEND connection :rd-registered-length) octet-deficit))
	   (SETF (SEND connection :return-sequence) (+seq (SEND connection :return-sequence) octet-deficit)))
	 (IF (SETF (SEND connection :return-q) (segment-link segment))
	     (SETF (segment-back-link (SEND connection :return-q)) ())
	     (SETF (SEND connection :return-q-end) ()))
	 ;1; permute segment into even number of text octets, on even octet boundary (inserting saved-octet)*
	 (WHEN saved-octet
	   (COPY-ARRAY-PORTION segment
			       (segment-index segment)
			       (segment-size segment)
			       segment
			       (1- (segment-index segment))
			       (1- (segment-size segment)))
	   (DECF (segment-size segment))
	   (SETF (segment-sequence-# segment) (+seq (segment-sequence-# segment) -1))
	   (DECF (segment-index segment) 2)
	   (SETF (AREF segment (segment-index segment)) saved-octet)
	   (SETF saved-octet ()))
	 (SETF beg (segment-index segment))
	 (SETF end (segment-size segment))
	 (WHEN (ODDP (- end beg))
	   (DECF end)
	   (DECF (segment-size segment))
	   (SETF saved-octet (AREF segment (segment-size segment))))
	 (DECF pending-octet-count text-space)
	 (WHEN (EQL (SETF (SEND connection :out-sequence) (+seq (SEND connection :out-sequence) text-space))
		    (SEND connection :return-sequence))
	   (CASE (SEND connection :state)
	     (:closed
	      (SEND *tcp-handler* :delete-tcb connection)
	      (SEND self :close-complete-handler))
	     (:time-wait
	      (SEND *tcp-handler* :link-tcb-to-time-wait-list connection)
	      (SEND self :close-complete-handler))))
	 (SETF urgent-input-index (WHEN (tcp-header-urgent-p segment)
				    (TRUNCATE (MAX end (+ beg (tcp-header-urgent-pointer segment)))
					      2)))
	 (VALUES
	   (allocate-tcp-stream-binary-segment segment beg end)
	   0
	   (TRUNCATE (- end beg) 2))))))) 


(DEFMETHOD (binary-input-stream-mixin :discard-input-buffer) (displaced-buffer)
  (deallocate-tcp-stream-binary-segment displaced-buffer)) 


;1;-----*

(DEFFLAVOR character-output-stream-mixin
	   ()
	   (basic-output-stream)
  (:included-flavors basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (character-output-stream-mixin :element-type) ()
  'STRING-CHAR) 


(DEFMETHOD (character-output-stream-mixin :new-output-buffer) (&aux segment)
  (WHEN (SEND connection :send-q)
    (SEND self :flush-buffer))
  (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (SEND connection :maximum-send-size)))
  (SETF (tcp-header-ack-p segment) t)
  (WITHOUT-INTERRUPTS
    (SETF (SEND connection :send-q) segment)
    (SETF (SEND connection :send-q-end) segment))
  (VALUES segment (segment-index segment) (segment-size segment))) 


(DEFMETHOD (character-output-stream-mixin :send-output-buffer) (segment character-index)
  (SETF (segment-index segment) character-index)
  (COND
    (urgent-output-index
     (SETF (tcp-header-urgent-p segment) t)
     (SETF (tcp-header-urgent-pointer segment)
	   (- urgent-output-index (* 4 (tcp-header-data-offset segment))))
     (SETF urgent-output-index ()))
    (send-urgent-mode-p
     (SETF (tcp-header-urgent-p segment) t)
     (SETF (tcp-header-urgent-pointer segment)
	   (- character-index (* 4 (tcp-header-data-offset segment))))))
  (SEND connection :external-drive-connection)) 


(DEFMETHOD (character-output-stream-mixin :force-output) ()
  (WHEN (NULL si::stream-output-buffer)
    (SEND self :setup-new-output-buffer))
  (SETF (tcp-header-push-p si::stream-output-buffer) t)
  (SEND self :send-current-output-buffer)) 


(DEFMETHOD (character-output-stream-mixin :discard-output-buffer) (ignore)
  nil) 

;1------*

(DEFFLAVOR ascii-translating-output-stream-mixin
	   ()
	   (basic-output-stream)
  (:included-flavors basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (ascii-translating-output-stream-mixin :new-output-buffer) (&aux buffer)
  (SETF buffer (ALLOCATE-RESOURCE 'tcp-stream-ascii-buffer))
  (VALUES buffer 0 (ARRAY-DIMENSION buffer 0))) 


(DEFMETHOD (ascii-translating-output-stream-mixin :send-output-buffer) (buffer character-index &aux segment j)
  "2RFC854 pp. 11-12 contains the Netascii definition.  On output, CR -> CR NUL  and #\newline -> CR LF.*"
  ;1; find segment to receive translated data*
  (WHEN (AND (SETF segment (SEND connection :send-q-end))
	     (OR (= (segment-index segment) (segment-size segment)) (tcp-header-push-p segment)))
    (SEND self :flush-buffer))
  (WHEN (NOT (SETF segment (SEND connection :send-q-end)))
    (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (SEND connection :maximum-send-size)))
    (SETF (tcp-header-ack-p segment) t)
    (WITHOUT-INTERRUPTS
      (IF (SEND connection :send-q-end)
	  (SETF (segment-link (SEND connection :send-q-end)) segment)
	  (SETF (SEND connection :send-q) segment))
      (SETF (SEND connection :send-q-end) segment)))
  (SETF j (segment-index segment))
  ;1; translate entire buffer*
  (DO ((i 0 (1+ i)))
      (nil)
    (WHEN (OR (= i character-index) (= j (segment-size segment)))
      ;1; complete current segment*
      (SETF (segment-index segment) j)
      (COND
	((OR send-urgent-mode-p (AND urgent-output-index (< i urgent-output-index)))
	 (SETF (tcp-header-urgent-p segment) t)
	 (SETF (tcp-header-urgent-pointer segment)
	       (- j (* 4. (tcp-header-data-offset segment)))))
	(urgent-output-index (SETF (tcp-header-urgent-p segment) t)
			     (SETF (tcp-header-urgent-pointer segment)
				   (- (- j (* 4 (tcp-header-data-offset segment))) (- i urgent-output-index)))
			     (SETF urgent-output-index ())))
      (WHEN (= i character-index)
	(RETURN))
      ;1; allocate new segment*
      (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (SEND connection :maximum-send-size)))
      (SETF (tcp-header-ack-p segment) t)
      (WITHOUT-INTERRUPTS
	(IF (SEND connection :send-q-end)
	    (SETF (segment-link (SEND connection :send-q-end)) segment)
	    (SETF (SEND connection :send-q) segment))
	(SETF (SEND connection :send-q-end) segment))
      (SETF j (segment-index segment)))
    ;1; translate one character*
    (CASE (AREF buffer i)
      (13
       ;1; insure room for <cr>/<nul>*
       (COND ((= (1+ j) (segment-size segment))
	      (DECF (segment-size segment))
	      (DECF i)
	      (DECF j))
	     (t (SETF (AREF segment j) 13)
		(SETF (AREF segment (INCF j)) 0))))
      (#.(CHAR-CODE #\Backspace) (SETF (AREF segment j) 8))
      (#.(CHAR-CODE #\Tab) (SETF (AREF segment j) 9))
      (#.(CHAR-CODE #\Linefeed) (SETF (AREF segment j) 10))
      (#.(CHAR-CODE #\Page) (SETF (AREF segment j) 12))
      (#.(CHAR-CODE #\Newline)
	 ;1; insure room for <cr>/<lf>*
	 (COND ((= (1+ j) (segment-size segment))
		(DECF (segment-size segment))
		(DECF i)
		(DECF j))
	       (t (SETF (AREF segment j) 13)
		  (SETF (AREF segment (INCF j)) 10))))
      (#.(CHAR-CODE #\Rubout) (SETF (AREF segment j) 127))
      (otherwise (SETF (AREF segment j) (AREF buffer i))))
    (INCF j))
  (WHEN (buffer-push-p buffer)
    (SETF (tcp-header-push-p (SEND connection :send-q-end)) t))
  (DEALLOCATE-RESOURCE 'tcp-stream-ascii-buffer buffer)
  (SEND connection :external-drive-connection)) 


(DEFMETHOD (ascii-translating-output-stream-mixin :force-output) ()
  (WHEN (NULL si::stream-output-buffer)
    (SEND self :setup-new-output-buffer))
  (SETF (buffer-push-p si::stream-output-buffer) t)
  (SEND self :send-current-output-buffer)) 


(DEFMETHOD (ascii-translating-output-stream-mixin :discard-output-buffer) (buffer)
  (DEALLOCATE-RESOURCE 'tcp-stream-ascii-buffer buffer)) 

;1;-----*
;1; binary streams have an effective character size of 2 octets*
;1;*

(DEFFLAVOR binary-output-stream-mixin
	   ()
	   (basic-output-stream)
  (:included-flavors basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (binary-output-stream-mixin :element-type) ()
  '(unsigned-byte 8)) 


(DEFMETHOD (binary-output-stream-mixin :new-output-buffer) (&aux segment)
  (WHEN (SEND connection :send-q)
    (SEND self :flush-buffer))
  (SETF segment (ALLOCATE-RESOURCE 'tcp-segment (SEND connection :maximum-send-size)))
  (SETF (tcp-header-ack-p segment) t)
  (WITHOUT-INTERRUPTS (SETF (SEND connection :send-q) segment)
		      (SETF (SEND connection :send-q-end) segment))
  (VALUES (allocate-tcp-stream-binary-segment segment 0 (segment-size segment))
	  (TRUNCATE (segment-index segment) 2)
	  (TRUNCATE (segment-size segment) 2))) 


(DEFMETHOD (binary-output-stream-mixin :send-output-buffer) (binary-segment character-index &aux segment)
  (SETF segment (tcp-stream-actual-array binary-segment))
  (SETF (segment-index segment) (* character-index 2))
  (deallocate-tcp-stream-binary-segment binary-segment :dont-release-segment)
  (COND
    (urgent-output-index (SETF (tcp-header-urgent-p segment) t)
			 (SETF (tcp-header-urgent-pointer segment)
			       (- (* 2 urgent-output-index) (* 4 (tcp-header-data-offset segment))))
			 (SETF urgent-output-index ()))
    (send-urgent-mode-p (SETF (tcp-header-urgent-p segment) t)
			(SETF (tcp-header-urgent-pointer segment)
			      (- (* 2 character-index) (* 4 (tcp-header-data-offset segment))))))
  (SEND connection :external-drive-connection)) 


(DEFMETHOD (binary-output-stream-mixin :force-output) ()
  (WHEN (NULL si::stream-output-buffer)
    (SEND self :setup-new-output-buffer))
  (SETF (tcp-header-push-p (tcp-stream-actual-array si::stream-output-buffer)) t)
  (SEND self :send-current-output-buffer)) 


(DEFMETHOD (binary-output-stream-mixin :discard-output-buffer) (binary-segment)
  (deallocate-tcp-stream-binary-segment binary-segment :dont-release-segment)) 


;1;------*
;1; Now the instantiable flavors*

(DEFFLAVOR input-character-stream
	   ()
	   (character-input-stream-mixin
	    basic-input-stream
	    si:buffered-input-character-stream)) 


(DEFFLAVOR output-character-stream
	   ()
	   (character-output-stream-mixin
	    basic-output-stream
	    si:buffered-output-character-stream)) 


(DEFFLAVOR character-stream
	   ()
	   (character-input-stream-mixin
	    character-output-stream-mixin
	    basic-input-stream
	    basic-output-stream
	    si:buffered-character-stream)) 

;1; This is to make the EVAL server work*

(DEFMETHOD (character-stream :beep) (&optional ignore)) 


(COMPILE-FLAVOR-METHODS input-character-stream output-character-stream character-stream) 

;1;------*

(DEFFLAVOR ascii-translating-input-character-stream
	   ()
	   (ascii-translating-input-stream-mixin
	    basic-input-stream
	    si:buffered-input-character-stream)) 


(DEFFLAVOR ascii-translating-output-character-stream
	   ()
	   (ascii-translating-output-stream-mixin
	    basic-output-stream
	    si:buffered-output-character-stream)) 


(DEFFLAVOR ascii-translating-character-stream
	   ()
	   (ascii-translating-input-stream-mixin
	    ascii-translating-output-stream-mixin
	    basic-input-stream
	    basic-output-stream
	    si:buffered-character-stream)) 


(COMPILE-FLAVOR-METHODS ascii-translating-input-character-stream
   ascii-translating-output-character-stream ascii-translating-character-stream) 

;1;------*

(DEFFLAVOR input-binary-stream
	   ()
	   (binary-input-stream-mixin
	    basic-input-stream
	    si:buffered-input-stream)) 


(DEFFLAVOR output-binary-stream
	   ()
	   (binary-output-stream-mixin
	    basic-output-stream
	    si:buffered-output-stream)) 


(DEFFLAVOR binary-stream
	   ()
	   (binary-input-stream-mixin
	    binary-output-stream-mixin
	    basic-input-stream
	    basic-output-stream
	    si:buffered-stream)) 


(COMPILE-FLAVOR-METHODS input-binary-stream output-binary-stream binary-stream) 


;1;------*

(DEFUN open-stream (host &key &optional (local-port 0) (remote-port 0) (direction :bidirectional)
		    (characters t) (wait-for-establishment t) (timeout 10) (timeout-after-open (WHEN timeout 300)) 
		    (error t) (input-buffer-size *default-tcp-stream-input-buffer-size*) (number-of-input-buffers 2)
		    &aux (host-name "Unknown") connection mode destination-address stream normal-exit
		    default-timeout-handler timeout-occurred-p)
  "2Establish a TCP connection and return a stream object for accessing that connection.
HOST The host to connect to.  May be a name, IP address, or host object.  If nil, the open is passive (listen).
:LOCAL-PORT (Optional) The local TCP port.  If 0 (default), a non-well known port will be generated and used.
:REMOTE-PORT (Optional) The remote TCP port on the specified host.  May be 0 (default) for passive opens.
:DIRECTION (Optional) :Input, :output, or :bidirectional (default).
:CHARACTERS (Optional) Either t (characters, the default), nil (binary), or :ascii (ascii characters).
:WAIT-FOR-ESTABLISHMENT (Optional) If nil, don't wait for connection establishment (default t).
:TIMEOUT  (Optional) The timeout (in seconds) used in open-stream when waiting for establishment.  
  Nil indicates no timeout.  Default is 10 seconds.
:TIMEOUT-AFTER-OPEN (Optional) The timeout used after the connection has been established.  Nil indicates no timeout.  
  The default for user-timeout is 300 seconds when :TIMEOUT is non-nil, and nil when :TIMEOUT is nil.  
  This timeout is used for the TCP 'user timeout' (the timeout on acknowledgement of data sent), and also used as 
  the timeout on :next-input-buffer.  This timeout can be changed after open-stream by sending the stream a :set-timeout message.
:ERROR (Optional) If t (default), error conditions will be signaled in open-stream.  Otherwise, the error condition will be returned.
:INPUT-BUFFER-SIZE (Optional) The size of the receive window in characters (default 2048).  For binary streams
  each character occupies two octets.
:NUMBER-OF-INPUT-BUFFERS (Optional) The number of input buffers supplied to TCP (default 2).*"
  (FLET
    ;1; internal function*
    ((make-stream (connection direction characters timeout input-buffer-size number-of-input-buffers
			      &aux stream)
		  ;1; ftp provides its own stream instantiator*
		  (LET ((*tcp-stream-instantiator*
			  (OR *tcp-stream-instantiator*
			      #'(lambda (connection timeout input-buffer-size number-of-input-buffers)
				  (CASE direction
				    (:input
				     (MAKE-INSTANCE
				       (COND
					 ((EQ characters :ascii) 'ascii-translating-input-character-stream)
					 (characters 'input-character-stream)
					 (t 'input-binary-stream))
				       :connection connection :timeout timeout :input-buffer-size input-buffer-size
				       :number-of-input-buffers number-of-input-buffers))
				    (:output
				     (MAKE-INSTANCE
				       (COND
					 ((EQ characters :ascii) 'ascii-translating-output-character-stream)
					 (characters 'output-character-stream)
					 (t 'output-binary-stream))
				       :connection connection :timeout timeout))
				    (:bidirectional
				     (MAKE-INSTANCE
				       (COND
					 ((EQ characters :ascii) 'ascii-translating-character-stream)
					 (characters 'character-stream)
					 (t 'binary-stream))
				       :connection connection :timeout timeout :input-buffer-size input-buffer-size
				       :number-of-input-buffers number-of-input-buffers)))))))
		    (SETF stream
			  (FUNCALL *tcp-stream-instantiator* connection timeout input-buffer-size number-of-input-buffers)))
		  ;1; insert actual handlers now that stream is known*
		  (SETF (SEND connection :buffer-handler) #'(lambda (LENGTH push-p urgent-p)
							      (SEND stream :buffer-handler length push-p urgent-p)))
		  (SETF (SEND connection :urgent-handler) #'(lambda ()
							      (SEND stream :urgent-handler)))
		  (SETF (SEND connection :process-fin-handler) #'(lambda ()
								   (SEND stream :process-fin-handler)))
		  (SETF (SEND connection :receive-fin-handler) #'(lambda ()
								   (SEND stream :receive-fin-handler)))
		  (SETF (SEND connection :close-complete-handler) #'(lambda ()
								      (SEND stream :close-complete-handler)))
		  (SETF (SEND connection :user-timeout-handler) #'(lambda ()
								    (SEND stream :timeout-handler :user-timeout)))
		  (SETF (SEND connection :condition-handler) #'(lambda (condition-object)
								 (SEND stream :condition-handler condition-object)))
		  stream))
    ;1; body of open-stream*
    (CONDITION-CASE-IF (NOT error) (error-object)
	(PROGN
	 (COND (host
		(SETF mode :active)
		(SETF host (parse-ip-host-spec host))
		(SETF host-name (SEND host :short-name))	      
		(SETF destination-address (FIRST (closest-addresses-to-network (get-ip-addresses host)))))
	       (t (SETF mode :passive)
		  (SETF destination-address 0)))
	 (SETF connection (SEND *tcp-handler* :make-connection nil nil nil nil nil nil nil))
	 (SETF stream (make-stream connection direction characters timeout input-buffer-size number-of-input-buffers))
	 ;1; use special handler until connection is established*
	 (SETF default-timeout-handler (SEND connection :user-timeout-handler))
	 (SETF (SEND connection :user-timeout-handler) #'(lambda ()
							   (SETF timeout-occurred-p t)))
	 (UNWIND-PROTECT
	     (PROGN
	       (SEND connection :open local-port remote-port destination-address mode timeout)
	       (SEND stream :load-receives)
	       (IF (NOT wait-for-establishment)
		   (SETF normal-exit t)
		   (DO ()
		       ((CATCH-ERROR-RESTART-EXPLICIT-IF error
							 (host-not-responding-during-connection
							   :retry-connection "Try the connection again.")
			  (SETF timeout-occurred-p nil)
			  (WHEN (NOT (PROCESS-WAIT-WITH-TIMEOUT
				       (COND
					 (*tcp-stream-whostate*)
					 ((NULL host) "TCP Listen")
					 (t
					  (FORMAT () "TCP Connect: ~A" host-name)))
				       ;1; when waiting for active establishment we depend on the tcp user timeout*
				       (IF (OR (EQ mode :active) (NULL timeout))
					   ()
					   (* timeout 60))
				       #'(lambda (connection)
					   (CASE (SEND connection :state)
					     ((:listen :syn-sent :syn-received) timeout-occurred-p)
					     (otherwise t)))
				       connection))
			    ;1; when timing out passive establishment we must explicitly call the user-timeout-handler*
			    (FUNCALL (SEND connection :user-timeout-handler)))
			  (IF timeout-occurred-p
			      (FERROR 'host-not-responding-during-connection "Host ~*~A not responding."
				      connection host-name)
			      t))
			(SETF normal-exit t))
		     (WHEN (AND timeout (<= timeout 60))
		       (SETF timeout 60)
		       (SETF (SEND connection :user-timeout) timeout)))))
	   (WHEN (NOT (OR normal-exit (EQ (SEND connection :status) :closed)))
	     (SEND connection :abort)))
	 ;1; restore user timeout to be used with established connection*
	 (SETF (SEND connection :user-timeout-handler) default-timeout-handler)
	 (SETF (SEND stream :timeout) timeout-after-open))
      (ERROR error-object)
      (:no-error stream)))) 
