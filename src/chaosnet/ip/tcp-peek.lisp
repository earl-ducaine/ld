;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT HL12B TR12BI); Base:10 -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986, 1988 Texas Instruments Incorporated.  All rights reserved.*

(DEFUN tcp-peek (IGNORE)
  "2Top level of the PEEK display of TCP*"
  (LIST ()
	(LIST '(:pre-process-function extended-tcp-meters)
	      (tv:scroll-parse-item
		:leader 1
		:mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
				  :documentation "Insert/remove display of global TCP meters.")		
		'(:string "TCP activity at ")
		'(:function time:print-current-time (nil))))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item "TCP connections")
	(tv:scroll-parse-item "")
	(tv:scroll-maintain-list #'tcp-init #'tcp-item)))

 

(DEFUN extended-tcp-meters (item)
  "2Insert/remove extended TCP meters.*"
  (COND
    ;1; leave state alone*
    ((NOT (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset)))
    ;1; remove display*
    ((CDR (tv::scroll-item-component-items item)) (RPLACD (tv::scroll-item-component-items item) ()))
    ;1; insert display*
    (t
     (RPLACD (tv::scroll-item-component-items item)
	     (CONS
	       (LIST ()
		     (tv:scroll-parse-item "")
		     (LET ((tcp-peek-window self))
		       (tv:scroll-parse-item
			 :mouse-self `(nil :eval
					   (LET ((terminal-io typwin))
					     (SEND ,tcp-peek-window :force-kbd-input
						   '(INSPECT ,ip:*tcp-handler*)))
					   :documentation "Inspect the TCP Handler object."
					   :bindings ((typwin ',(FUNCALL tcp-peek-window :typeout-window))))
			 '(:string "Global TCP Meters")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*tcp-handler* (:segments-received) nil ("  ~d. segment~:p received, "))
		       `(:function ,ip:*tcp-handler* (:checksum-errors) nil ("~d. checksum error~:p, "))
		       `(:function ,ip:*tcp-handler* (:nonexistant-connections) nil
				   ("~d. nonexistant connection~:p")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*tcp-handler* (:retransmissions) nil ("  ~d. retransmission~:p, "))
		       `(:function ,ip:*tcp-handler* (:delayed-acks) nil ("~d. delayed acknowledgment~:p, "))
		       `(:function ,ip:*tcp-handler* (:idle-probes) nil ("~d. idle connection probe~:p")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*tcp-handler* (:syn-listings) nil ("  ~d. syn segment~:p queued, "))
		       `(:function ,ip:*tcp-handler* (:syn-claims) nil ("~d. syn segment~:p claimed, "))
		       `(:function ,ip:*tcp-handler* (:syn-timeouts) nil ("~d. syn segment timeout~:p")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*tcp-handler* (:ports-currently-reserved) nil
				   ("  ~d. port number~:p currently reserved"))))
	       ()))))
  (SETF (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ()))



(DEFUN tcp-init (&aux tcb-list)
  "2Return a list of all registered TCBs.*"
  (MAPHASH
    #'(lambda (IGNORE tcb)
	(DO ()
	    ((NULL tcb))
	  (PUSH tcb tcb-list)
	  (SETF tcb (SEND tcb :port-link))))
    (SEND *tcp-handler* :tcb-table))
  tcb-list) 


;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Reformatted text so any one line would not be longer than 83 characters - 11/24/87 CAT*
(DEFUN tcp-item (tcb)
  "2Create the PEEK display for the specified connection.*"
  (LIST ()
	;1; top level connection information*
	(LET ((tcp-peek-window self))
	  (tv:scroll-parse-item
	    :leader 4
	    :mouse-self `(nil
			   :menu-choose ("Connection Operations"
					 ("Abort" :eval
					  (CONDITION-CASE ()
					      (SEND ,tcb :abort)
					    (sys:network-error))
					  :documentation "Abort this connection")
					 ("Close" :eval
					  (CONDITION-CASE ()
					      (SEND ,tcb :close)
					    (sys:network-error))
					  :documentation "Close this connection")
					 ("Probe" :eval
					  (CONDITION-CASE ()
					      (SEND ,tcb :external-drive-connection :block :send-probe)
					    (sys:network-error))
					  :documentation "Probe this connection")
					 ("Describe" :eval
					  (LET ((terminal-io typwin))
					    (SEND ,tcp-peek-window :force-kbd-input '(DESCRIBE ,tcb)))
					  :documentation "Describe this connection.")
					 ("Inspect" :eval
					  (LET ((terminal-io typwin))
					    (SEND ,tcp-peek-window :force-kbd-input '(INSPECT ,tcb)))
					  :documentation "Inspect this connection."))
			   :documentation "Menu of things to do to this connection."
			   :bindings ((typwin ',(FUNCALL tcp-peek-window :typeout-window))))
	    `(:function
	       ,#'(lambda (tcb)
		    (si:get-host-from-address (SEND tcb :destination-address) :ip))
	       (,tcb) nil ("Connection to: ~:[Unknown Host        ~;~:*~20a~] "))
	    `(:function ,tcb (:destination-port) nil ("(port ~d., "))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 24) (COND ((SEND tcb :destination-address))
						    (t 0))))
			(,tcb) nil ("address ~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 16) (COND ((SEND tcb :destination-address))
						    (t 0))))
			(,tcb) nil ("~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 8) (COND ((SEND tcb :destination-address))
						   (t 0))))
			(,tcb) nil ("~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 0) (COND ((SEND tcb :destination-address))
						   (t 0))))
			(,tcb) nil ("~d) "))
	    `(:function ,#'(lambda (tcb)
			     (si:get-host-from-address (SEND tcb :source-address) :ip))
			(,tcb) nil ("~%From:          ~20a "))
	    `(:function ,tcb (:source-port) nil ("(port ~d., "))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 24) (COND ((SEND tcb :source-address))
						    (t 0))))
			(,tcb) nil ("address ~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 16) (COND ((SEND tcb :source-address))
						    (t 0))))
			(,tcb) nil ("~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 8) (COND ((SEND tcb :source-address))
						   (t 0))))
			(,tcb) nil ("~d."))
	    `(:function ,#'(lambda (tcb)
			     (LDB (BYTE 8 0) (COND ((SEND tcb :source-address))
						   (t 0))))
			(,tcb) nil ("~d) "))))
	(tv:scroll-parse-item
	  `(:function ,tcb (:state) nil ("State:         ~a")))
	;1; transmit information*
	(LIST `(:pre-process-function extended-tcp-transmit-info :connection ,tcb)
	      (tv:scroll-parse-item
		:leader 1
		:mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
				  :documentation "Insert/remove display of extended transmit information.")
		`(:function ,tcb (:number-of-segments-sent) nil ("Segments sent:           ~10d."))
		`(:function ,#'(lambda (tcb)
				 (TRUNCATE (SEND tcb :total-text-sent)
					   (MAX 1 (SEND tcb :number-of-text-segments-sent))))
			    (,tcb) nil ("   Average text sent:     ~10d. octet~:p"))
		`(:function ,tcb (:number-of-retransmissions) nil ("~%Segments retransmitted:  ~10d."))
		`(:function ,#'(lambda (tcb)
				 (DO ((segment (SEND tcb :send-q)
					       (segment-link segment))
				      (total 0))
				     ((NOT segment)
				      total)
				   (INCF total
					 (- (segment-index segment)
					    (* 4 (tcp-header-data-offset segment))))))
			    (,tcb) nil ("   # in transmit buffers: ~10d. octet~:p"))))
	;1; receive information*
	(LIST `(:pre-process-function extended-tcp-receive-info :connection ,tcb)
	      (tv:scroll-parse-item
		:leader 1
		:mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
				  :documentation "Insert/remove display of extended receive information.")
		`(:function ,tcb (:number-of-segments-received) nil ("Segments received:       ~10d."))
		`(:function ,#'(lambda (tcb)
				 (TRUNCATE (SEND tcb :total-text-received)
					   (MAX 1 (SEND tcb :number-of-text-segments-received))))
			    (,tcb) nil ("   Average text received: ~10d. octet~:p"))
		`(:function ,#'(lambda (tcb)
				 (CASE (SEND tcb :state)
				   ((:listen :syn-sent :syn-received) 0)
				   ((:close-wait :last-ack :closing :time-wait)
				    (-seq (SEND tcb :fin-sequence)
					  (SEND tcb :return-sequence)))
				   (:closed
				    ;1; check if connection ever opened*
				    (IF (SEND tcb :fin-sequence)
					(-seq (SEND tcb :fin-sequence)
					      (SEND tcb :return-sequence))
					0))
				   (otherwise
				    (-seq (SEND tcb :receive-next)
					  (SEND tcb :return-sequence)))))
			    (,tcb) nil ("~%Rec'd but not available: ~10d."))
		`(:function ,#'(lambda (tcb)
				 (CASE (SEND tcb :state)
				   ((:listen :syn-sent :syn-received) 0)
				   (:closed
				    ;1; check if connection ever opened*
				    (IF (SEND tcb :fin-sequence)
					(-seq (SEND tcb :return-sequence)
					      (SEND tcb :out-sequence))
					0))
				   (otherwise
				    (-seq (SEND tcb :return-sequence)
					  (SEND tcb :out-sequence)))))
			    (,tcb) nil ("   # available:           ~10d. octet~:p"))))
	(tv:scroll-parse-item ""))) 


;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Reformatted text so any one line would not be longer than 83 characters - 11/24/87 CAT*
(DEFUN extended-tcp-transmit-info (item &aux tcb)
  "2Insert/remove extended TCP transmit information.*"
  (SETF tcb (GETF (tv::scroll-item-plist item) :connection))
  (COND
    ;1; leave state alone*
    ((NOT (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset)))
    ;1; remove display*
    ((CDR (tv::scroll-item-component-items item)) (RPLACD (tv::scroll-item-component-items item) ()))
    ;1; insert display*
    (t
     (RPLACD (tv::scroll-item-component-items item)
	     (CONS
	       (LIST ()
		     (tv:scroll-parse-item
		       `(:function ,tcb (:send-unack) nil ("  snd.una: ~14d."))
		       `(:function ,tcb (:send-next) nil ("   snd.nxt: ~14d."))
		       `(:function ,tcb (:send-window) nil ("   snd.wnd: ~14d.")))
		     (tv:scroll-parse-item
		       `(:function ,tcb (:send-wl1) nil ("  snd.wl1: ~14d."))
		       `(:function ,tcb (:send-wl2) nil ("   snd.wl2: ~14d."))
		       `(:function ,tcb (:initial-send-sequence-#) nil ("   iss:     ~14d.")))
		     (tv:scroll-parse-item
		       `(:function ,tcb (:maximum-text-sent) nil
				   ("  maximum text sent:     ~10d. octet~:p"))
		       `(:function ,tcb (:user-timeout) nil ("   user timeout:        ~d. sec~:p, ")))
		     (tv:scroll-parse-item
		       `(:function ,#'(lambda (tcb)
					(ROUND (SEND tcb :retransmission-timeout) 60))
				   (,tcb) nil ("  retransmission timeout:~10d. sec~:p"))
		       `(:function ,#'(lambda (tcb)
					(ROUND
					 (* 1000
					    (+ (SEND tcb :smoothed-intersegment-arrival-time)
					       *delayed-ack-fudge*))
					 60))
				   (,tcb) nil ("      delayed ack timeout: ~d. msec~:p")))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :retransmit-q)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					   "Insert/remove display of segments on retransmit queue.")
			 `(:function ,tcb (:retransmit-q) nil
				     ("  retransmit queue ~:[empty~;contains segments~]"))))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :send-q)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
					   :documentation "Insert/remove display of segments on send queue.")
			 `(:function ,tcb (:send-q) nil ("  send queue ~:[empty~;contains segments~]")))))
	       ()))))
  (SETF (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ()))

;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Reformatted text so any one line would not be longer than 83 characters - 11/24/87 CAT*
(DEFUN extended-tcp-receive-info (item &aux tcb)
  "2Insert/remove extended TCP receive information.*"
  (SETF tcb (GETF (tv::scroll-item-plist item) :connection))
  (COND
    ;1; leave state alone*
    ((NOT (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset)))
    ;1; remove display*
    ((CDR (tv::scroll-item-component-items item)) (RPLACD (tv::scroll-item-component-items item) ()))
    ;1; insert display*
    (t
     (RPLACD (tv::scroll-item-component-items item)
	     (CONS
	       (LIST ()
		     (tv:scroll-parse-item
		       `(:function ,tcb (:receive-next) nil ("  rcv.nxt: ~14d."))
		       `(:function ,tcb (:receive-window) nil ("   rcv.wnd: ~14d."))
		       `(:function ,tcb (:receive-urgent-pointer) nil ("   rcv.up: ~14d.")))
		     (tv:scroll-parse-item
		       `(:function ,tcb (:initial-receive-sequence-#) nil ("  irs:     ~14d.")))
		     (tv:scroll-parse-item
		       `(:function ,tcb (:segments-out-of-sequence) nil ("  out of order segments: ~10d."))
		       `(:function ,tcb (:segments-rejected) nil ("   rejected segments: ~10d.")))
		     (tv:scroll-parse-item
		       `(:function ,tcb (:maximum-text-received) nil
				   ("  maximum text received: ~10d. octet~:p")))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :remote-probe-segment)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
					   :documentation "Insert/remove display of remote probe segment.")
			 `(:function ,tcb (:remote-probe-segment) nil
				     ("  ~:[no remote probe segment~;remote probe segment exists~]"))))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :receive-q)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					   "Insert/remove display of segments on receive queue.")
			 `(:function ,tcb (:receive-q) nil ("  receive queue ~:[empty~;contains segments~]"))))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :received-segment)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					   "Insert/remove display of inbound segment being processed.")
			 `(:function ,tcb (:received-segment) nil
				     ("  ~:[no ~;~]segment currently being processed"))))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :out-of-order-q)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					   "Insert/remove display of segments on out-of-order queue.")
			 `(:function ,tcb (:out-of-order-q) nil
				     ("  out-of-order queue ~:[empty~;contains segments~]"))))
		     (LIST
		       `(:pre-process-function display-tcb-queue :connection ,tcb :queue :return-q)
		       (tv:scroll-parse-item
			 :leader 1
			 :mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
					   :documentation "Insert/remove display of segments on return queue.")
			 `(:function ,tcb (:return-q) nil ("  return queue ~:[empty~;contains segments~]")))))
	       ()))))
  (SETF (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ())) 



(DEFUN display-tcb-queue (item &aux connection queue)
  "2Insert/remove display of segments on a given queue.*"
  (SETF connection (GETF (tv::scroll-item-plist item) :connection))
  (SETF queue (GETF (tv::scroll-item-plist item) :queue))
  (COND
    ;1; leave state alone*
    ((NOT (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset)))
    ;1; remove display*
    ((CDR (tv::scroll-item-component-items item)) (RPLACD (tv::scroll-item-component-items item) ()))
    ;1; insert display*
    (t
     (RPLACD (tv::scroll-item-component-items item)
	     (CONS
	       (LIST ()
		     (tv:scroll-maintain-list
		       `(global:lambda ()
			  (tcp-q-init (SEND ,connection ,queue)))
		       `(global:lambda (segment)
			  (format-tcp-segment segment ,queue))))
	       ()))))
  (SETF (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ())) 



(DEFUN tcp-q-init (queue-header)
  "2Return a list of segments on the specified queue.*"
  (WITHOUT-INTERRUPTS
    (DO ((segment-list nil (PUSH segment segment-list))
	 (segment queue-header (segment-link segment)))
	((NULL segment)
	 (NREVERSE segment-list))))) 



(DEFUN format-tcp-segment (segment queue)
  "2Format display of a segment.*"
  (LIST ()
	(LET ((tcp-peek-window self))
	  (tv:scroll-parse-item
	    :leader 4
	    :mouse-self `(nil :eval (LET ((terminal-io typwin))
				      (SEND ,tcp-peek-window :force-kbd-input '(INSPECT ,segment)))
			      :documentation "Inspect this segment."
			      :bindings ((typwin ',(FUNCALL tcp-peek-window :typeout-window))))
	    `(:function ,#'(lambda (segment)
			     (CASE queue
			       ((:retransmit-q :send-q)
				(tcp-header-sequence-# segment))
			       (otherwise
				(segment-sequence-# segment))))
			(,segment) nil ("    seq#: ~d., "))
	    `(:function ,#'(lambda (segment)
			     (CASE queue
			       ((:retransmit-q :send-q)
				(tcp-header-ack-# segment))
			       (otherwise
				(segment-ack-# segment))))
			(,segment) nil ("ack#: ~d., "))
	    `(:function tcp-header-data-offset (,segment) nil ("offset: ~d., "))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-urgent-p segment))
			(,segment) nil ("flags: ~:[-~;u~]"))
	    `(:function ,#'(lambda (segment)
			     (CASE queue
			       ((:retransmit-q :send-q)		    
				(tcp-header-ack-p segment))
			       (otherwise
				(segment-ack-p segment))))
			(,segment) nil ("~:[-~;a~]"))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-push-p segment))
			(,segment) nil ("~:[-~;p~]"))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-reset-p segment))
			(,segment) nil ("~:[-~;r~]"))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-syn-p segment))
			(,segment) nil ("~:[-~;s~]"))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-fin-p segment))
			(,segment) nil ("~:[-~;f~], "))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-window segment))
			(,segment) nil ("win: ~d., "))
	    `(:function ,#'(lambda (segment)
			     (tcp-header-urgent-pointer segment))
			(,segment) nil ("urg ptr: ~d., "))
	    `(:function ,#'(lambda (segment)
			     (CASE queue
			       ((:retransmit-q :send-q)
				(- (segment-index segment) (* (tcp-header-data-offset segment) 4)))
			       (otherwise
				(- (segment-size segment) (segment-index segment)))))
			(,segment) nil ("text size: ~d."))))))
