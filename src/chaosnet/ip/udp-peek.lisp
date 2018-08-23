;;; -*- Mode:COMMON-LISP; Package:IP; Base:10; Fonts: MEDFNT,HL12B -*-

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

;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Made everything output to a single column - 11/24/87 CAT*
(DEFUN ip-peek (IGNORE)
  "Top level of the PEEK display of IP"
  (LIST ()
	(tv:scroll-parse-item "IP Activity at "
			      '(:function time:print-current-time (nil)))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent (,*ip-handler*) nil (" ~12D. IP Packets Sent ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received (,*ip-handler*) nil
		      (" ~12D. IP Packets Received ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-ip-broadcast-pkts-received (,*ip-handler*) nil
		      (" ~12D. IP broadcast packets received ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-bytes-sent (,*ip-handler*) nil
		      (" ~12D. Bytes Sent As IP Data,    ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-average-bytes-per-packet-sent (,*ip-handler*) nil
		      (" ~12D. Average Bytes of Data per IP packet Sent")))
	(tv:scroll-parse-item
	  `(:function ip-handler-bytes-received (,*ip-handler*) nil
		      (" ~12D. Bytes Received As IP Data,")))
	(tv:scroll-parse-item
	  `(:function ip-handler-average-bytes-per-packet-received (,*ip-handler*) nil
		      (" ~12D. Average Bytes of Data per IP packet Received")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-average-time-sending (,*ip-handler*) nil
		      (" ~12D. Average Milliseconds Processing Per IP Packet Transmission ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-average-time-receiving (,*ip-handler*) nil
		      (" ~12D. Average Milliseconds Processing Per IP Packet Reception ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-allocated (,*ip-handler*) nil
		      (" ~12D. Total Ip Packet Allocations ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-deallocated (,*ip-handler*) nil
		      (" ~12D. Total Ip Packet Deallocations ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-in-use (,*ip-handler*) nil
		      (" ~12D. Total Ip Packets Currently In Use ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-fragmented (,*ip-handler*) nil
		      (" ~12D. IP Packets Fragmented Before Transmission ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packet-fragments-sent (,*ip-handler*) nil
		      (" ~12D. IP Packet Fragments Sent ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-reassembled (,*ip-handler*) nil
		      (" ~12D. IP Packets Reassembled During Reception ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packet-fragments-received (,*ip-handler*) nil
		      (" ~12D. IP Packet Fragments Received ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-rerouted (,*ip-handler*) nil
		      (" ~12D. IP Packets Rerouted ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-from-other-net (,*ip-handler*) nil
		      (" ~12D. IP Packets Received From Net Not Attached to ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-to-other-net (,*ip-handler*) nil
		      (" ~12D. IP Packets Transmitted To Net Not Attached to ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-not-for-this-host (,*ip-handler*) nil
		      (" ~12D. IP Packets Discarded Due To Not For This Host ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-with-bad-checksums (,*ip-handler*) nil
		      (" ~12D. IP Packets Discarded Due To Incorrect Header Checksums ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-with-unknown-gateway-for-destination (,*ip-handler*) nil
		      (" ~12D. IP Packets Discarded Due To No Known Gateway For Destination Network ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-with-expired-time-to-live (,*ip-handler*) nil
		      (" ~12D. IP Packets Discarded Due To Expired Time To Live ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-security (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Security Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-lsr (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Loose Source And Record Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-ssr (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Strict Source And Record Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-record-route (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Record Route Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-stream-id (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Stream Id Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-received-with-timestamp (,*ip-handler*) nil
		      (" ~12D. IP Packets Received With Timestamp Option ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-security (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Security Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-lsr (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Loose Source And Record Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-ssr (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Strict Source And Record Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-record-route (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Record Route Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-stream-id (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Stream Id Option ")))
	(tv:scroll-parse-item
	  `(:function ip-handler-packets-sent-with-timestamp (,*ip-handler*) nil
		      (" ~12D. IP Packets Sent With Timestamp Option ")))
	(tv:scroll-parse-item "")
	(tv:scroll-parse-item
	  '(:function symbol-value (ethernet::*ip-arp-requests-sent*) nil
		      (" ~12D. ARP Requests Sent involving an IP address ")))
	(tv:scroll-parse-item
	  '(:function symbol-value (ethernet::*ip-arp-replies-received*) nil
		      (" ~12D. ARP Replies Received to requests involving an IP address ")))
	(tv:scroll-parse-item
	  '(:function symbol-value (ethernet::*ip-arp-requests-received*) nil
		      (" ~12D. ARP Requests Received involving an IP address ")))
	(tv:scroll-parse-item
	  '(:function symbol-value (ethernet::*ip-arp-replies-sent*) nil
		      (" ~12D. ARP Replies Sent involving an IP address "))))) 



(DEFUN ip-handler-average-time-sending (handler)
  (IF (AND (NUMBERP (ip-handler-time-sending handler)) (NUMBERP (ip-handler-packets-sent handler))
	   (NOT (ZEROP (ip-handler-packets-sent handler))))
      (ROUND (ip-handler-time-sending handler) (ip-handler-packets-sent handler))
      0)) 


(DEFUN ip-handler-average-time-receiving (handler)
  (IF (AND (NUMBERP (ip-handler-time-receiving handler))
	   (NUMBERP (ip-handler-packets-received handler))
	   (NOT (ZEROP (ip-handler-packets-received handler))))
      (ROUND (ip-handler-time-receiving handler) (ip-handler-packets-received handler))
      0)) 


(DEFUN ip-handler-average-bytes-per-packet-sent (handler)
  (IF (AND (NUMBERP (ip-handler-bytes-sent handler)) (NUMBERP (ip-handler-packets-sent handler))
	   (NOT (ZEROP (ip-handler-packets-sent handler))))
      (ROUND (ip-handler-bytes-sent handler) (ip-handler-packets-sent handler))
      0)) 


(DEFUN ip-handler-average-bytes-per-packet-received (handler)
  (IF (AND (NUMBERP (ip-handler-bytes-received handler))
	   (NUMBERP (ip-handler-packets-received handler))
	   (NOT (ZEROP (ip-handler-packets-received handler))))
      (ROUND (ip-handler-bytes-received handler) (ip-handler-packets-received handler))
      0)) 

;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP-PEEK is the top level function of the code used to display the TCP connections*
;1;; in a PEEK display.  It is called when the user clicks on the TCP prompt in the *
;1;; network protocols menu displayed when the NETWORK option is choosen in PEEK.*
;1;;*

(DEFUN udp-peek (IGNORE)
  "Top level of the PEEK display of UDP"
  (LIST ()
	(LIST '(:pre-process-function extended-udp-meters)
	      (tv:scroll-parse-item
		:leader 1
		:mouse-self '(nil :eval (tv::peek-mouse-click 'self 0)
				  :documentation "Insert/remove display of global UDP meters.")		
		'(:string "UDP activity at ")
		'(:function time:print-current-time (nil))))
	(tv:scroll-parse-item "")
	(tv:scroll-maintain-list #'(lambda ()
				     *udp-receive-list*)
				 #'udp-item)))


(DEFUN extended-udp-meters (item)
  "Insert/remove extended UDP meters."
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
		     (LET ((udp-peek-window self))
		       (tv:scroll-parse-item
			 :mouse-self `(nil :eval
					   (LET ((terminal-io typwin))
					     (SEND ,udp-peek-window :force-kbd-input
						   '(INSPECT ,ip:*udp-handler*)))
					   :documentation "Inspect the UDP Handler object."
					   :bindings ((typwin ',(FUNCALL udp-peek-window :typeout-window))))
			 '(:string "Global UDP Meters")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*udp-handler* (:packets-received) nil ("  ~d. packet~:p received, "))
		       `(:function ,ip:*udp-handler* (:checksum-errors) nil ("~d. checksum error~:p, "))
		       `(:function ,ip:*udp-handler* (:nonexistant-ports) nil
				   ("~d. nonexistant port~:p")))
		     (tv:scroll-parse-item
		       `(:function ,ip:*udp-handler* (:server-packet-claims) nil
				   ("  ~d. server packet~:p claimed, "))
		       `(:function ,ip:*udp-handler* (:server-packet-timeouts) nil
				   ("~d. server packet timeout~:p")))
		     (tv:scroll-parse-item
		       `(:function ,#'(lambda () (LENGTH *udp-receive-list*)) nil nil
				   ("  ~d. port number~:p currently reserved"))))
	       ()))))
  (SETF (ARRAY-LEADER (FIRST (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ())) 

;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Reformatted text so any one line would not be longer than 83 characters - 11/24/87 CAT*
(DEFUN udp-item (port-cons &aux (port (CDR port-cons)))
  "Create the PEEK display for the specified connection"
  (cond (port
	 (LIST ()
	       (LET ((udp-peek-window self))
		 (tv:scroll-parse-item
		   :leader 4
		   :mouse-self `(nil :menu-choose ("Port Operations"
						   ("Reset" :eval (SETF (SEND ,port :reset-occured-p) t)
						    :documentation "Reset this port.")
						   ("Remove" :eval (SEND *udp-handler* :return-port ,port)
						    :documentation "Remove the port from active status.")
						   ("Inspect" :eval
						    (LET ((terminal-io typeout-window))
						      (SEND ,udp-peek-window :force-kbd-input
							    '(INSPECT ,port)))
						    :documentation "Inspect this port.")
						   ("Describe" :eval
						    (LET ((terminal-io typeout-window))
						      (SEND ,udp-peek-window :force-kbd-input
							    '(DESCRIBE ,port)))
						    :documentation "Describe this port."))
				     :documentation "Menu of things to do to this port."
				     :bindings ((port ',port)
						(typeout-window ',(FUNCALL udp-peek-window :typeout-window))))
		   `(:function udp-port-port-number (,port) nil ("Local Port ~D."))))
	       (tv:scroll-parse-item
		 `(:function ,#'(lambda (port)
				  (WHEN (PLUSP (udp-port-sender-address port))
				    (si:get-host-from-address (udp-port-sender-address port) :ip)))
			     (,port) nil ("Last packet received from ~:[Unknown Host~;~:*~a~] "))
		 `(:function ,port (:sender-port) nil ("(port ~d., "))
		 `(:function ,#'(lambda (port)
				  (LDB (BYTE 8 24) (COND ((udp-port-sender-address port))
							 (t 0))))
			     (,port) nil ("address ~d."))
		 `(:function ,#'(lambda (port)
				  (LDB (BYTE 8 16) (COND ((udp-port-sender-address port))
							 (t 0))))
			     (,port) nil ("~d."))
		 `(:function ,#'(lambda (port)
				  (LDB (BYTE 8 8) (COND ((udp-port-sender-address port))
							(t 0))))
			     (,port) nil ("~d."))
		 `(:function ,#'(lambda (port)
				  (LDB (BYTE 8 0) (COND ((udp-port-sender-address port))
							(t 0))))
			     (,port) nil ("~d) ")))
	     (WHEN (SEND port :last-buffered-time)
	       (tv:scroll-parse-item
		 `(:function ,#'(lambda (port)
				    (time:print-universal-time (SEND port :last-buffered-time) nil))
		    (,port) nil ("~@[Receive time: ~a~]"))))
	       (tv:scroll-parse-item
		 (FORMAT () "~33A~11A~10A~20A" " " "Packets" "Bytes" "Avg Bytes per Packet"))
	       	       (tv:scroll-parse-item
		 "Received:                        "
		 `(:function udp-port-packets-received (,port) 7 ("~6D."))
		 `(:function udp-port-bytes-received (,port) 10 ("~9D."))
		 `(:function udp-port-average-bytes-per-packet-received (,port) 20
			     ("~15D.")))
	      (tv:scroll-parse-item
		"Transmitted:                     "
		`(:function udp-port-packets-transmitted (,port) 7 ("~6D."))
		`(:function udp-port-bytes-transmitted (,port) 10 ("~9D."))
		`(:function udp-port-average-bytes-per-packet-transmitted (,port) 20
			    ("~15D.")))
	      (tv:scroll-parse-item
		"Dropped due to Checksum Errors:  "
		`(:function udp-port-checksum-errors (,port) 7
			    ("~6D.")))
	      (tv:scroll-parse-item
		"Dropped due to Receive Overruns: "	
		`(:function udp-port-receive-overruns (,port) 7
			    ("~6D.")))
	      (tv:scroll-parse-item "")))
      (t (LIST () (tv:scroll-parse-item "There are no UDP ports active."))))) 


(DEFUN udp-port-average-bytes-per-packet-transmitted (port)
  (IF (AND (NUMBERP (udp-port-bytes-transmitted port))
	   (NUMBERP (udp-port-packets-transmitted port))
	   (NOT (ZEROP (udp-port-packets-transmitted port))))
      (ROUND (udp-port-bytes-transmitted port) (udp-port-packets-transmitted port))
      0)) 
      

(DEFUN udp-port-average-bytes-per-packet-received (port)
  (IF (AND (NUMBERP (udp-port-bytes-received port)) (NUMBERP (udp-port-packets-received port))
	   (NOT (ZEROP (udp-port-packets-received port))))
      (ROUND (udp-port-bytes-received port) (udp-port-packets-received port))
      0)) 

