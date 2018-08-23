;; -*- Mode:Common-Lisp; Package:Chaos; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;
;; Copyright (c) 1981 Massachusetts Institute of Technology
;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved.
;;
;;
;;-----------------------------------------------------------------------------
;;                         CHAOSNet USER INTERFACE
;;-----------------------------------------------------------------------------
;; This file contains the high level user interface functions for Chaosnet.
;;-----------------------------------------------------------------------------
;;                          ADVERTISED FUNCTIONS

;;  CONNECT
;;  SIMPLE
;;  CLOSE
;;  OPEN-FOREIGN-CONNECTION
;;  LISTEN
;;  ACCEPT
;;  REJECT
;;  FORWARD
;;  FORWARD-ALL
;;  ANSWER
;;  ANSWER-STRING
;;  FAST-ANSWER-STRING
;;  OPEN-STREAM
;;  MAKE-STREAM
;;  WAIT

;;                               ADVERTISED METHODS
;;
;; :FOREIGN-HOST, :CLOSE, :FORCE-OUTPUT, :FINISH, :EOF, :CLEAR-EOF,

;;                          ADVERTISED VARIABLES
;;  No Variables or Constants defined.
;;------------------------------------------------------------------------------
;;  09-22-87 DAB - Fixed (basic-output-stream :before :close) to handle :IO differently.
;;  09-10-87 DAB    Added code to support (OPEN "...." :Direction :IO) in QFILE.
;;  3/24/86 MMG - Common Lisp conversion.
;;  9/12/85 RLA - added net-address-parser to be called from si:parse-host
;;  4/25/85 RAF - Fix so that simultaneous :CLOSE on a bidirectional stream will not hang.
;;  3/08/85 RAF - Documetation changes only.
;;  2/04/85 RAF - Fix OPEN-STREAM so that it ACCEPTs a request when it LISTENs for one.
;; 11/19/84 RAF - Add patches to use MY-ADDRESSES & MY-SUBNETS.
;;  9/21/84 RAF - Remove references to obs. functions.  Combine CONNECT & SIMPLE base
;;                  functions.  Combine common code in OPEN-BROADCAST-CONNECTION and
;;                  RETRANSMIT-BRD-PACKER.  General cleanup.
;;  7/13/84 RAF - Cloned file from CHSAUX.
;;  6/11/84 RAF - Add Patch Be-Beep 1.70.
;;  4/05/84 RAF - No code changes, only documentation.
;;-----------------------------------------------------------------------------





(defun establish-connection-inner ()
  (progn
    (assure-enabled)
    (setq host-name (or (si:get-host-from-address real-address :chaos)
			real-address))
    ;; This is not necessarily the best address. Go with what they sent in.
    ;;	  (when (typep host-name 'host:host)
    ;;        (setq real-address (send host-name :chaos-address)));Find best address.
    (setq conn (open-connection real-address contact-name window-size))
    (wait conn 'rfc-sent-state timeout
	  (format () "Net Connect: ~A"
		  (if (typep host-name 'host:host)
		      (send host-name :short-name)
		      host-name)))
    (case (state conn)
      (answered-state
       (if simple		   ;Normal case for SIMPLE
	   (return (prog1
		       (get-next-pkt conn)
		     (close-conn conn)))
	   (progn
	     (close-conn conn)
	     (ferror 'connection-error-1 "Received an ANS instead of an OPN." conn))))
      (open-state
       (if (not simple)
	   (return conn)	   ;Normal case for CONNECT
	   (progn
	     (close-conn conn "I expected an ANS, not an OPN.")
	     (ferror 'connection-error-1 "Received an OPN instead of an ANS." conn))))
      (rfc-sent-state (close-conn conn)
		      (ferror 'host-not-responding-during-connection "Host ~1*~A not responding." conn
			      host-name))
      (cls-received-state
       (let* ((pkt (get-next-pkt conn)) (string (string-append (pkt-string pkt))))	   ;COPY the string
	 (return-pkt pkt)
	 (close-conn conn)
	 (ferror 'connection-refused "Connection to ~1*~A refused: ~A." conn host-name
		 (if (equal string "")
		     "No reason given"
		     string))))
      (otherwise
       (unwind-protect (ferror
			'connection-error-1
			"Bad state in ~S: ~A~@[, ~A~]"
			conn
			(state conn)
			(when (read-pkts conn)
			  (pkt-string (read-pkts conn))))
	 (remove-conn conn))))))


;;; Functions called from the "User" process

;; This routine does the actual work of CONNECT and SIMPLE.
(defun establish-connection (address contact-name window-size timeout simple
			     &aux conn real-address host-name)
  ;; Establish a chaosnet connection, where address is a host name or
  ;; number, contact-name is a string containing the contact
  ;; name. Window-size is the number of packets that can be in transit
  ;; from the other side of this connection. Timeout is how long to
  ;; wait before giving up (in 60'ths of a second). If an error or
  ;; unexpected state occurs, causes a fatal errer. If simple is t,
  ;; returns the ans pkt and closes the conn.  If simple is nil,
  ;; returns the conn.
  (condition-case (condition)  ;added to handle CTRL-ABORT leaving connection opened.
      (loop
	 (catch-error-restart-explicit-if
	  t
	  (remote-network-error :retry-connection "Try the connection again.")
	 (if (null (setq real-address (address-parse address)))
	     (ferror 'unknown-address "~S is not a valid Chaosnet address." address)
	     (progn
	       (assure-enabled)
	       (setq host-name (or (si:get-host-from-address real-address :chaos)
				   real-address))
	       ;; This is not necessarily the best address. Go with what they sent in.
;	  (when (typep host-name 'host:host)
;	    (setq real-address (send host-name :chaos-address)));Find best address.
	       (setq conn (open-connection real-address contact-name window-size))
	       (wait conn 'rfc-sent-state timeout
		     (format () "Net Connect: ~A"
			     (if (typep host-name 'host:host)
				 (send host-name :short-name)
				 host-name)))
	       (case (state conn)
		 (answered-state
		  (if simple		   ;Normal case for SIMPLE
		      (return (prog1
				(get-next-pkt conn)
				(close-conn conn)))
		      (progn
			(close-conn conn)
			(ferror 'connection-error-1 "Received an ANS instead of an OPN." conn))))
		 (open-state
		  (if (not simple)
		      (return conn)	   ;Normal case for CONNECT
		      (progn
			(close-conn conn "I expected an ANS, not an OPN.")
			(ferror 'connection-error-1 "Received an OPN instead of an ANS." conn))))
		 (rfc-sent-state (close-conn conn)
				 (ferror 'host-not-responding-during-connection "Host ~1*~A not responding." conn
					 host-name))
		 (cls-received-state
		  (let* ((pkt (get-next-pkt conn)) (string (string-append (pkt-string pkt))))	   ;COPY the string
		    (return-pkt pkt)
		    (close-conn conn)
		    (ferror 'connection-refused "Connection to ~1*~A refused: ~A." conn host-name
			    (if (equal string "")
				"No reason given"
				string))))
		 (otherwise
		  (unwind-protect (ferror
				    'connection-error-1
				    "Bad state in ~S: ~A~@[, ~A~]"
				    conn
				    (state conn)
				    (when (read-pkts conn)
				      (pkt-string (read-pkts conn))))
		    (remove-conn conn)))))))
       ;; The second time, wait one minute.  (started as 10 secs.)
       (setq timeout (* 60 60)))
    (sys:abort (when conn (close-conn conn "CTRL aborting")) (signal-condition condition))))  ;01-29-88 DAB

;; Does a full "ICP": it sends an RFC, waits for a reply or timeout,
;; and returns a string to get an error, or else the CONN to indicate
;; that the foreign host sent an OPN and we are connected.  The first
;; argument gets parsed as an address.
(defsubst connect (address contact-name &optional
					  (window-size default-window-size)
					  ;07-11-88 increase timeout
					  ;to 15 seconds.
					  (timeout (* 15 60)))
  "Establish a chaosnet connection and return the connection object.
   ADDRESS is a host name or number. CONTACT-NAME is a string
   containing the contact name and optional additional data for the
   other host.  WINDOW-SIZE is the number of packets that can be in
   transit from the other side, on this connection. TIMEOUT is how
   long to wait before giving up (in 60'ths of a second). If the
   connection fails, an error is signaled."
  (establish-connection address contact-name window-size timeout ()))




;; This is used to perform a "simple connection".  An RFC is sent to the
;; specified address, expecting an ANS.  Returns a string if there was an error,
;; in which case the string is an ASCII explanation.  Otherwise returns the ANS.
;; When you are done perusing the ANS, RETURN-PKT the PKT.
;;

(defsubst simple (address contact-name &optional (timeout (* 10 60)));patched

  "Send a message to CONTACT-NAME at ADDRESS, expecting one ANS packet in return.
   No connection is established; if the other host tries to create a connection,
   it is considered an error.  If successful, the ANS packet object is returned.
   Otherwise, a string describing the reasons for failure is returned.
   TIMEOUT is how long to wait before giving up, in 60'ths of a second."

  (establish-connection address contact-name 5 timeout t))


(defun best-address-in (list addr)
  (or (car (member (subnet addr) list :test #'(lambda (x y)

(defmacro valid-address? (address)
  `(and (numberp ,address) (>= ,address 0) (<= ,address 65535)))

(defun open-connection-inner ()
  (let (pkt conn)
    (setq pkt (allocate-pkt))
    (setf (pkt-opcode pkt) rfc-op)
    (set-pkt-string pkt contact-name)
    (setf (pkt-link pkt) ())
    ;; Is this really needed for background?
    (without-interrupts
      (setf (window-available conn) 1) (setf (time-last-received conn) (time))
      (setf (state conn) 'rfc-sent-state))
    (transmit-normal-pkt conn pkt (pkt-num-sent conn))
    ;; Must not put on lists before calling TRANSMIT-NORMAL-PKT, which
    ;; fills in important information
    (without-interrupts
      (setf (send-pkts conn) pkt)
      (setf (send-pkts-last conn) pkt)
      (setf (send-pkts-length conn) 1)
      (setq retransmission-needed t)
      (setq pkt ()))))


;; This is called as the first step in opening a connection.  Note the
;; CONNECT function, which is a higher-level frob, which you may want
;; to use instead.  The first arg is the address of the foreign host.
;; Next is the contact name.  Optionally following are the one-way
;; flag and window size.
(defun open-connection (address contact-name
			&optional (window-size default-window-size))
  (let (pkt conn)
    (check-arg address (valid-address? address) "an address")
    (check-arg contact-name
	       (and (stringp contact-name) (<= (array-active-length contact-name) max-data-bytes-per-pkt))
	       "a string")
    (check-arg window-size numberp "a number")
    (setq conn (make-connection))
    (setf (local-window-size conn) (max 1 (min window-size maximum-window-size)))
    (setf (foreign-address conn) address)
    (setf (local-address conn) (best-address-in my-addresses address))
    (setf (getf (conn-plist conn) 'rfc-contact-name) contact-name)
    (unwind-protect (progn
		      (setq pkt (allocate-pkt))
		      (setf (pkt-opcode pkt) rfc-op)
		      (set-pkt-string pkt contact-name)
		      (setf (pkt-link pkt) ())
		      (without-interrupts;is this really needed for BACKGROUND?
			(setf (window-available conn) 1) (setf (time-last-received conn) (time))
			(setf (state conn) 'rfc-sent-state))
		      (transmit-normal-pkt conn pkt (pkt-num-sent conn))
		      ;; Must not put on lists before calling TRANSMIT-NORMAL-PKT, which fills in
		      ;; important information
		      (without-interrupts (setf (send-pkts conn) pkt)
					  (setf (send-pkts-last conn) pkt) (setf (send-pkts-length conn) 1)
					  (setq retransmission-needed t) (setq pkt ())))
      (when (not (null pkt))
	(free-pkt pkt)))
    conn))


;;------------------------------------------------------------------------------
;; Open up a connection for use with foreign protocols
;;

(defun open-foreign-connection (foreign-host foreign-index &optional (pkt-allocation 10) distinguished-port &aux conn)
  (check-arg foreign-host (valid-address? foreign-host) "an address")
  (setq conn (make-connection))
  (setf (local-window-size conn) (max 1 (min pkt-allocation maximum-window-size)))
  (setf (local-address conn) (best-address-in my-addresses foreign-host))
  (setf (foreign-address conn) foreign-host)
  (setf (foreign-index-num conn) foreign-index)
  (setf (state conn) 'foreign-state)
  (cond
    (distinguished-port
     (setf (aref index-conn (ldb maximum-index-log-2-minus-1 (local-index-num conn))) ())
     (setf (local-index-num conn) distinguished-port)
     (push (cons distinguished-port conn) distinguished-port-conn-table)))
  conn)



(defun open-broadcast-connection (subnet-bit-map subnet-bit-map-length contact-name &optional pkt-allocation);patched
  "Broadcast a service request for CONTACT-NAME over certain subnets.
   PKT-ALLOCATION is the buffering size for unread requests as they come over the net.
   SUBNET-BIT-MAP is an array of bytes (the type should be ART-STRING), where each bit is
   a 1 for the desired subnets.  SUBNET-BIT-MAP-LENGTH, can be no more 32., and must be a
   multiple of four.  The connection returned is in the CHAOS:BROADCAST-SENT-STATE."
  (check-arg subnet-bit-map-length
     (and (< subnet-bit-map-length 33) (zerop (mod subnet-bit-map-length 4)))
     "a valid length for the subnet bit map")
  (check-arg subnet-bit-map (stringp subnet-bit-map) "a subnet bit map as a string")
  (let ((conn (make-connection)))
    (setf (local-window-size conn) (max 1 (min pkt-allocation maximum-window-size)))
    (setf (local-address conn) my-address);seems like no better for a Broadcast.
    (setf (foreign-address conn) 0); seems ok
    (setf (getf (conn-plist conn) 'subnet-bit-map) subnet-bit-map)
    (setf (getf (conn-plist conn) 'subnet-bit-map-length) subnet-bit-map-length)
    (setf (getf (conn-plist conn) 'contact-name) contact-name)
    (transmit-brd-pkt subnet-bit-map subnet-bit-map-length contact-name)
    (setf (state conn) 'broadcast-sent-state)
    conn))



(defun retransmit-brd-packet (conn);new
  "Send out another request for service, if CONN was opened in broadcast mode."
  (transmit-brd-pkt (getf (conn-plist conn) 'subnet-bit-map)
		    (getf (conn-plist conn) 'subnet-bit-map-length)
		    (getf (conn-plist conn) 'contact-name)))


;; Takes anything anyone might use as a ChaosNet address, and tries to return
;; the corresponding host number.  If it fails, returns NIL.
;;

(defun address-parse (address &aux host)
  "Coerce the argument to a chaosnet address.
The argument can be a host name or host object, or an address."
  (condition-case ()
     (cond
       ((integerp address) address)
       ((and (typep address 'host:host) (send address :send-if-handles :chaos-address)))
       ((and (stringp address) (parse-number address 0 nil 8 t)))
       ((and (setq host (si:parse-host address)) (send host :chaos-address))))
     (unclaimed-message ())))

;;(defun address-parse (address &aux host)
;;  "Coerce the argument to a chaosnet address.
;;The argument can be a host name or host object, or an address."
;;  (condition-case ()
;;     (cond
;;       ((integerp address) address)
;;       ((and (typep address 'host:host) (send address :send-if-handles :chaos-address)))
;;       ((and (setq host (si:parse-host address t)) (send host :chaos-address)))
;;       ((and (stringp address) (parse-number address 0 () 8))))
;;     (unclaimed-message ())))

;;; RLA - new function (called by si:parse-host)

(defun net-address-parser (host)
  (and (stringp host)
       (string-equal host "CHAOS|" :end1 6 :end2 6)
       (parse-number host 6 () 8))) ;must be an octal number

(when (variable-boundp si:net-address-parsers)
  (unless (assoc :chaos si:net-address-parsers :test 'eq)
    (push '(:chaos chaos:net-address-parser) si:net-address-parsers)))


;;------------------------------------------------------------------------------
;; SERVER FUNCTIONS: Functions used by the server side of a connection only.
;;------------------------------------------------------------------------------



(defun listen (contact-name &optional (window-size default-window-size) (wait-for-rfc t) &aux conn)
  "Listen for an incoming RFC to CONTACT-NAME.  Returns the connection-object,
   ready to have CHAOS:ACCEPT, CHAOS:REJECT, CHAOS:ANSWER, or CHAOS:FORWARD done to it.
   A server function on SERVER-ALIST can call LISTEN to respond to the request which
   caused the server to be run.  If WAIT-FOR-RFC is NIL, doesn't wait for the RFC
   to arrive, just sets up a queue.  WINDOW-SIZE specifies how many packets can be in
   transit at once from the other side of the connection to this one, once the
   connection is established."
  (check-arg contact-name stringp "a string")
  (check-arg window-size numberp "a number")
  ;; Make a connection.  If table full, wait a little while and try again.
  (loop
   (condition-case () (setq conn (make-connection))
      (network-resources-exhausted (process-sleep 30)) (:no-error (return))))
  (setf (getf (conn-plist conn) 'listen-contact-name) contact-name)
  (setf (local-window-size conn) (max 1 (min window-size maximum-window-size)))
  (block listen
    (without-interrupts;First try to pick up a pending RFC
     (do ((pkt pending-rfc-pkts (pkt-link pkt))
	  (prev nil pkt))
	 ((null pkt))
       (when (string-equal (contact-name-from-rfc pkt) contact-name)
	 (cond
	   ((null prev) (setq pending-rfc-pkts (pkt-link pkt)))
	   (t (setf (pkt-link prev) (pkt-link pkt))))
	 (rfc-meets-lsn conn pkt)
	 (return-from listen conn)))
     (setf (state conn) 'listening-state);No RFC, let listen pend
     (push (cons contact-name conn) pending-listens))
    (when wait-for-rfc
      (process-wait "Net Listen" #'(lambda (conn)
				     (neq (state conn) 'listening-state))
		    conn)
      (unless (eq (state conn) 'rfc-received-state)
	(ferror 'bad-connection-state-1 "Listening connection ~S entered bad state ~S" conn
		(state conn))))
    (return-from listen conn)   ; 3/86 MMG
    ()))


;; If you have done a LISTEN and the state has changed to RFC-RECEIVED, you
;; call one of the following four functions.
;;
;; ACCEPT, REJECT, ANSWER, or ANSWER-STRING
;;------------------------------------------------------------------------------

;; Send an OPN, and leave conn in OPEN-STATE.  Note that when this returns the
;; other end has not yet acknowledged the OPN, and the window size is still 0.
;; Transmitting the first packet will wait.
;;

(defun accept (conn &aux pkt)
  "Accept a request for a connection, received on connection-object CONN.
   CONN is obtained by a previous call to LISTEN.  Note that the connection is not
   completely established until the other side replies to the packet we send."
  (unless (eq (state conn) 'rfc-received-state)
    (ferror 'bad-connection-state-1
	    "Attempt to accept ~S, which was in ~A, not RFC-RECEIVED-STATE" conn (state conn)))
  (setq pkt (read-pkts conn))
  (when (not (null pkt));In case the user has not read the RFC
    (setf (pkt-num-received conn) (pkt-num pkt))
    (setf (read-pkts conn) (pkt-link pkt))
    (when (null (read-pkts conn))
      (setf (read-pkts-last conn) ()))
    (free-pkt pkt))
  (setq pkt (allocate-pkt))
  (setf (pkt-opcode pkt) opn-op)
  (setf (pkt-nbytes pkt) 4)
  (setf (pkt-second-data-word pkt) (local-window-size conn))
  (setf (pkt-first-data-word pkt) (pkt-num-read conn))
  (without-interrupts (setf (pkt-link pkt) ()) (setf (window-available conn) 0)
     (setf (time-last-received conn) (time)) (setf (state conn) 'open-state));Set this -before- telling other end it's open!
  (transmit-normal-pkt conn pkt t)
  (without-interrupts
   ;; TRANSMIT-NORMAL-PKT fills in fields that must be filled before packet
   ;; can be put on transmit list
   (setf (send-pkts conn) pkt) (setf (send-pkts-last conn) pkt) (setf (send-pkts-length conn) 1)
   (setq retransmission-needed t))
  t)


;;------------------------------------------------------------------------------
;; Send a CLS and leave conn INACTIVE.
;;

(defun reject (conn reason)
  "Reject a request for a connection, received on connection-object CONN.
   CONN should have been returned by a previous call to LISTEN.  REASON is a
   string to be sent to the requestor and returned from his call to CONNECT."
  (unless (eq (state conn) 'rfc-received-state)
    (ferror 'bad-connection-state-1
	    "Attempt to reject ~S, which was in ~A, not RFC-RECEIVED-STATE" conn (state conn)))
  (close-conn conn reason)
  t)


;;------------------------------------------------------------------------------
;; Send an ANS, and leave conn INACTIVE.
;; The caller passes in a PKT with data and NBYTES set up.
;;

(defun answer (conn pkt)
  "Reply to a simple transaction received on connection-object CONN.
   PKT should be a packet with ANS as its opcode and the data and nbytes fields set up.
   This is the proper way to answer when the requestor has used the function CHAOS:SIMPLE.
   Note that there is no guarantee that the requestor will receive the answer;
   he will just repeat the request if he does not.  See also CHAOS:ANSWER-STRING."
  (when (eq (state conn) 'rfc-received-state)
    (setf (pkt-opcode pkt) ans-op)
    (transmit-normal-pkt conn pkt))
  (return-pkt pkt)
  (remove-conn conn)
  t)



(defun answer-string (conn string)
  "Reply with a STRING to a simple transaction received on connection-object CONN.
   STRING specifies the answer to send.  This is the proper way to answer when the
   requestor has used the function CHAOS:SIMPLE.  Note that there is no guarantee
   that the requestor will receive the answer; he will just repeat the request if he
   does not.  See also CHAOS:ANSWER, a lower level way of answering."
  (let ((pkt (get-pkt)))
    (setf (pkt-nbytes pkt) (min (length string) max-data-bytes-per-pkt))
    (copy-array-contents string (pkt-string pkt))
    (answer conn pkt)))


;;------------------------------------------------------------------------------
;; Minimal-consing simple-transaction answerer.
;; Returns T if succeeds, NIL if fails, although you probably don't care, since
;; a value of T does not assure that the ANS really reached the requestor.
;;

(defun fast-answer-string (contact-name string)
  "Reply to a simple transaction requested on CONTACT-NAME, with answer STRING.
   This is like (ANSWER-STRING (LISTEN contact-name) string) but conses less."
  (let ((prev nil)
	(success)
	rfc
	pkt
	pstr)
    (without-interrupts
      (setq rfc
	    (do ((pkt pending-rfc-pkts (pkt-link pkt)))
		((null pkt))
	      (and (string-equal (contact-name-from-rfc pkt) contact-name) (return pkt))
	      (setq prev pkt)))
      (if (null rfc)
	  (return-from fast-answer-string ())
	  (if (null prev)
	      (setq pending-rfc-pkts (pkt-link rfc))
	      (setf (pkt-link prev) (pkt-link rfc)))))
    (unwind-protect
	(progn
	  (setq pkt (allocate-int-pkt))
	  (setf (pkt-nbytes pkt) (min (length string) max-data-bytes-per-pkt))
	  (setq pstr			   ;Create indirect array to reference as a string

		(make-array max-data-bytes-per-pkt :element-type 'string-char :leader-list '(0)
			    :displaced-to pkt :displaced-index-offset 16))
	  (copy-array-contents string pstr)
	  (return-array (prog1
			  pstr
			  (setq pstr ())))
	  (setf (pkt-source-address pkt) my-address)
	  (setf (pkt-source-index-num pkt) 0)
	  (setf (pkt-dest-address pkt) (pkt-source-address rfc))
	  (setf (pkt-dest-index-num pkt) (pkt-source-index-num rfc))
	  (setf (pkt-opcode pkt) ans-op)
	  (setf (pkt-num pkt) 0)
	  (setf (pkt-ack-num pkt) 0)
	  (transmit-int-pkt pkt)
	  (setf success t)
	  (setf (pkt-status rfc) ())
	  (free-pkt rfc))
      (unless success
	(free-int-pkt pkt)))
    (return-from fast-answer-string t)))


;;------------------------------------------------------------------------------

(defun forward (conn pkt host)
  "Forward a request for a connection to some other host and/or contact name.
   CONN should be a connection object returned by LISTEN on which a request has been
   received.  PKT should have its data (and PKT-NBYTES) set to the new contact name
   to forward to.  HOST should specify the host to forward to."
  (setq host (address-parse host));convert HOST to Chaos address.
  (unless (eq (state conn) 'rfc-received-state)
    (ferror 'bad-connection-state-1
	    "Attempt to forward ~S, which was in ~A, not RFC-RECEIVED-STATE" conn (state conn)))
  (setf (pkt-opcode pkt) fwd-op)
  (transmit-normal-pkt conn pkt 0 host)
  (return-pkt pkt)
  (remove-conn conn)
  t)


(defun forward-all (contact-name host)
  "Tell all requests for chaosnet connections to CONTACT-NAME to try host HOST instead."
  (setq host (address-parse host))
  (push
   (list contact-name
	 `(prog (conn)
	    (setq conn (listen ,contact-name))
	    (forward conn (get-next-pkt conn) ,host)))
   server-alist)
  ())


;;------------------------------------------------------------------------------
;; CONTROL OPERATIONS USED BY BOTH USERS AND SERVERS.
;;------------------------------------------------------------------------------

;; If CONN has received a close, free it up.
;; If CONN is inactive, do nothing.
;; If CONN is open, send a CLS containing the reason, leaving CONN inactive.
;;

;;; *BJ* For *RWF*
(defun close-conn (conn &optional (reason "") &aux pkt)
  "Close a Chaosnet connection, given connection-object CONN.
   REASON is a string telling the other side why; but don't rely on its being received."
  (declare (special conns-already-inactive))
  (case (state conn)
    ((cls-received-state answered-state los-received-state host-down-state listening-state
			 rfc-sent-state)
     (remove-conn conn))
    (inactive-state (incf conns-already-inactive)
		    (setq conn-list (delete conn (the list conn-list) :test #'eq)))	;Just in case
    ((open-state rfc-received-state) (setq pkt (allocate-pkt)) (setf (pkt-opcode pkt) cls-op)
				     (set-pkt-string pkt reason) (transmit-normal-pkt conn pkt) (free-pkt pkt)
				     (remove-conn conn))
    (otherwise
     (ferror 'bad-connection-state-1
	     "Attempt to close ~S, which was in ~S, not an acceptable state" conn (state conn)))))

;(DEFUN CLOSE-CONN (CONN &OPTIONAL (REASON "") &AUX PKT)
;  "Close a Chaosnet connection, given connection-object CONN.
;   REASON is a string telling the other side why; but don't rely on its being received."
;  (CASE (STATE CONN)
;    ((CLS-RECEIVED-STATE ANSWERED-STATE LOS-RECEIVED-STATE HOST-DOWN-STATE LISTENING-STATE
;      RFC-SENT-STATE)
;     (REMOVE-CONN CONN))
;    (INACTIVE-STATE (SETQ CONN-LIST (DELETE CONN (THE LIST CONN-LIST) :TEST #'EQ)));Just in case
;    ((OPEN-STATE RFC-RECEIVED-STATE) (SETQ PKT (ALLOCATE-PKT)) (SETF (PKT-OPCODE PKT) CLS-OP)
;     (SET-PKT-STRING PKT REASON) (TRANSMIT-NORMAL-PKT CONN PKT) (FREE-PKT PKT)
;     (REMOVE-CONN CONN))
;    (OTHERWISE
;     (FERROR 'BAD-CONNECTION-STATE-1
;	     "Attempt to close ~S, which was in ~S, not an acceptable state" CONN (STATE CONN)))))


(deff close 'close-conn)
(deff chaos-close 'close-conn)
(make-obsolete close "use CHAOS:CLOSE-CONN")
(make-obsolete chaos-close "use CHAOS:CLOSE-CONN")

;; Wait until either:
;;  the state of CONN is not STATE  (return T), or
;;  over TIMEOUT 60ths of a second happen (return NIL).
;;

(defun wait (conn state timeout &optional (whostate "Chaosnet Wait") &aux start-time)
  "Wait for chaosnet connection CONN to be in a state other than STATE.
   Alternatively, waiting ends after TIMEOUT time (measured in 60'ths).
   Returns non-NIL iff the connection's state has changed.
   WHOSTATE is a string to tell the user what you are waiting for."
  (setq start-time (time))
  (loop (when (neq state (state conn))
	  (return t))
     (when (>= (time-difference (time) start-time) timeout)
       (return ()))
     (process-wait whostate
		   #'(lambda (conn state start-time timeout)
		       (or (neq (state conn) state)
			  (>= (time-difference (time) start-time) timeout)))
		   conn state start-time timeout)))

;;------------------------------------------------------------------------------
;;; Streams
;;------------------------------------------------------------------------------

;; This is included in all chaosnet streams, input or output
;;

(defflavor basic-stream ((connection nil)) ()
  (:included-flavors stream)
   (:initable-instance-variables connection)
   (:gettable-instance-variables connection))

(defmethod (basic-stream :network-type) () :chaos)

;To find out what chaos host a stream is open to.

(defmethod (basic-stream :foreign-host) ()
  (si:get-host-from-address (foreign-address connection) :chaos))

;;; *BJ* For *RWF*
(defmethod (basic-stream :close) (&optional abort-p)
  (when (and connection				;Allowed to keep doing this
             (typep connection 'conn))   ;01-29-88 DAB
    (close-conn connection (if abort-p
			       "Aborted"
			       ""))))

;; LS 1/23/87 - for generic-peek stuff
(defmethod (basic-stream :server-current-p) (&rest ignore)
  (when connection
    (send connection :server-current-p)))


;(DEFMETHOD (BASIC-STREAM :CLOSE) (&OPTIONAL ABORT-P)
;  (WHEN CONNECTION;Allowed to keep doing this
;    (CLOSE-CONN CONNECTION (IF ABORT-P
;			     "Aborted"
;			     ""))
;    (REMOVE-CONN (PROG1
;		   CONNECTION
;		   (SETQ CONNECTION ())))))


(defmethod (basic-stream :accept) ()
  (accept connection))

(defmethod (basic-stream :reject) (&optional reason)
  (reject connection (or reason "")))

;;------------------------------------------------------------------------------
;; This is included in all chaosnet input streams, character and binary
;;

(defflavor input-stream-mixin ((input-packet)) ()
   (:included-flavors si:basic-buffered-input-stream))


(defmethod (input-stream-mixin :discard-input-buffer) (ignore)
  (without-interrupts
    (when (and input-packet
	       (eq (chaos:pkt-status input-packet) 'chaos:released))
      (return-pkt input-packet))
    (setq input-packet ())))

;;------------------------------------------------------------------------------
;; This is included in all chaosnet output streams, character and binary
;;

(defflavor output-stream-mixin (output-packet) ()
   (:included-flavors si:basic-buffered-output-stream))

(defmethod (output-stream-mixin :discard-output-buffer) (ignore)
  (return-pkt output-packet)
  (setq output-packet ()))
;;------------------------------------------------------------------------------
;; This is included in all chaosnet IO streams, character and binary
;;


(defflavor IO-stream-mixin  ((output-packet nil)) ()
  (:included-flavors si:buffered-io-stream)
  )

(defmethod (io-stream-mixin :discard-input-buffer) (ignore)
  (without-interrupts
    (when (and output-packet
	       (eq (chaos:pkt-status output-packet) 'chaos:released))
      (return-pkt output-packet))
    (setq output-packet ())))



;;------------------------------------------------------------------------------
;; This is included in simple chaosnet input streams, but not file streams, where certain
;; opcodes have special meaning.
;;

(defflavor basic-input-stream ((input-packet nil)) (input-stream-mixin basic-stream))

(defmethod (basic-input-stream :get-next-input-pkt) (no-hang-p &aux op)
  (cond
    ((and input-packet (or (= (setq op (pkt-opcode input-packet)) eof-op) (= op cls-op))) nil)
    ((null (setq input-packet (get-next-pkt connection no-hang-p "Chaosnet Input" t))) nil)
    ((or (= (setq op (pkt-opcode input-packet)) eof-op) (= op cls-op)) nil)
    ((>= op dat-op) t)
    (t
     (ferror () "Unknown opcode ~O in packet ~S received from connection ~S" op input-packet
	     connection))))

(defmethod (basic-input-stream :clear-eof) ()
  (when (and input-packet (= (pkt-opcode input-packet) eof-op))
    (return-pkt input-packet)
    (setq input-packet ())))

;;------------------------------------------------------------------------------
;; This is included in simple chaosnet output streams, but not file streams, where a
;; connection is maintained for longer.
;;

(defflavor basic-output-stream () (output-stream-mixin basic-stream)
   (:included-flavors si:basic-buffered-output-stream))


(defmethod (basic-output-stream :eof) ()
  (send self :force-output)
  (send-pkt connection (get-pkt) eof-op)
  (finish-conn connection))


(defmethod (basic-output-stream :finish) ()
  (finish-conn connection))


(defflavor basic-io-stream ((output-packet nil))
	   (basic-output-stream io-stream-mixin basic-stream))

(defmethod (basic-io-stream :get-next-input-pkt) (no-hang-p &aux op)
  (cond
    ((and output-packet (or (= (setq op (pkt-opcode output-packet)) eof-op) (= op cls-op))) nil)
    ((null (setq output-packet (get-next-pkt connection no-hang-p "Chaosnet Input" t))) nil)
    ((or (= (setq op (pkt-opcode output-packet)) eof-op) (= op cls-op)) nil)
    ((>= op dat-op)  t)
    (t
     (ferror () "Unknown opcode ~O in packet ~S received from connection ~S" op output-packet
	     connection))))

(defmethod (basic-io-stream :clear-eof) ()
  (when (and output-packet (= (pkt-opcode output-packet) eof-op))
    (return-pkt output-packet)
    (setq output-packet ())))

(defmethod (basic-io-stream :new-output-buffer) ()
  (let ((next-buffer  (send self :next-input-buffer)))
    (unless next-buffer
      (setq output-packet (get-pkt))
      ))
  (values (pkt-string output-packet) 0 max-data-bytes-per-pkt))





(defun read-and-ignore-all-pkts (conn)
 ;; 4/25/85 RAF. Used to clear out all received pkts and ack them.
  (do ((pkt (get-next-pkt conn t) (get-next-pkt conn t)));Read and ack the pkt.
      ((null pkt))
    (return-pkt pkt)))


(defun interrupt-ack-all-pkts (interrupt conn &optional ignore)
 ;;This function is used to Read and Ack all incoming pkts while a Network stream
 ;; is closing.  This is so that it can ACK an incoming EOF.
  (select interrupt (:input (read-and-ignore-all-pkts conn))))



(defmethod (basic-output-stream :before :close) (&optional abort-p)
  "Send EOF saying we are closing down.  We also will not read anything else received."
  (when (and connection (not abort-p) (eq (state connection) 'open-state))
    (unwind-protect
	(if (eq (send self :direction) :io)  ;09-22-87 DAB
	    (progn
	      (send self :eof)
	      (setf (interrupt-function connection) 'interrupt-ack-all-pkts)
	      (read-and-ignore-all-pkts connection)	   ;read & ack any pkts already here
	      )

	    (setf (interrupt-function connection) 'interrupt-ack-all-pkts)
	    (read-and-ignore-all-pkts connection)  ;read & ack any pkts already here
	    (send self :eof)
	    )
      (setf (interrupt-function connection) ()))))


;;------------------------------------------------------------------------------

(defflavor character-input-stream-mixin (input-packet) (input-stream-mixin)
   (:included-flavors basic-stream si:basic-buffered-input-stream)
   ;;:GET-NEXT-INPUT-PKT returns T if INPUT-PACKET is a valid packet
   (:required-methods :get-next-input-pkt))


(defmethod (character-input-stream-mixin :element-type) ()
  'string-char)


(defmethod (character-input-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (when (send self :get-next-input-pkt no-hang-p)
    (values (pkt-string input-packet) 0 (pkt-nbytes input-packet))))


(defmethod (character-input-stream-mixin :tyi-no-hang) (&optional eof)
  (loop until (and si::stream-input-buffer (< si::stream-input-index si::stream-input-limit));Out of input, get some more
     unless (send self :setup-next-input-buffer t);Reached end of file
     return
     (and eof input-packet
	  (let ((packet-opcode (pkt-opcode input-packet)))
	    (or
	      (= packet-opcode eof-op)
	      (= packet-opcode los-op)
	      (= packet-opcode cls-op)))
	  (ferror 'end-of-file-1 "End of file on ~S." self))	   ;Here we have a character available
     finally
     (return
      (prog1
	(aref si::stream-input-buffer si::stream-input-index)
	(incf si::stream-input-index)))))

;;------------------------------------------------------------------------------

(defflavor binary-input-stream-mixin (input-packet) (input-stream-mixin)
   (:included-flavors basic-stream si:basic-buffered-input-stream)
   (:required-methods :get-next-input-pkt))


(defmethod (binary-input-stream-mixin :element-type) ()
  '(unsigned-byte 8))


(defmethod (binary-input-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (when (send self :get-next-input-pkt no-hang-p)
    (values input-packet first-data-word-in-pkt
	    (+ first-data-word-in-pkt (truncate (pkt-nbytes input-packet) 2)))))

;;------------------------------------------------------------------------------

(defflavor character-output-stream-mixin (output-packet) (output-stream-mixin)
   (:included-flavors basic-stream si:basic-buffered-output-stream))


(defmethod (character-output-stream-mixin :element-type) ()
  'string-char)


(defmethod (character-output-stream-mixin :new-output-buffer) ()
  (setq output-packet (get-pkt))
  (values (pkt-string output-packet) 0 max-data-bytes-per-pkt))

;; Some code actually calls the Function directly.  RAF 9/26/84

(defmethod (character-output-stream-mixin :send-output-buffer) send-character-pkt)


(defun send-character-pkt (ignore ignore length)
  (declare (:self-flavor character-output-stream-mixin))
  (setf (pkt-nbytes output-packet) length)
  (send-pkt connection output-packet)
  (setq output-packet ()))

;;------------------------------------------------------------------------------

(defflavor binary-output-stream-mixin (output-packet) (output-stream-mixin)
   (:included-flavors basic-stream si:basic-buffered-output-stream))


(defmethod (binary-output-stream-mixin :element-type) ()
  '(unsigned-byte 8))


(defmethod (binary-output-stream-mixin :new-output-buffer) ()
  (setq output-packet (get-pkt))
  (values output-packet first-data-word-in-pkt
	  (+ first-data-word-in-pkt (truncate max-data-bytes-per-pkt 2))))

;; Some code actually calls the Function directly.  RAF 9/26/84

(defmethod (binary-output-stream-mixin :send-output-buffer) send-binary-pkt)


(defun send-binary-pkt (ignore ignore length)
  (declare (:self-flavor binary-output-stream-mixin))
  (setf (pkt-nbytes output-packet) (* (- length first-data-word-in-pkt) 2))
  (send-pkt connection output-packet 192)
  (setq output-packet ()))



(defflavor character-io-stream-mixin (output-packet)
	   (basic-io-stream character-output-stream-mixin)
   (:included-flavors basic-stream si:buffered-io-stream)
   ;;:GET-NEXT-INPUT-PKT returns T if OUTPUT-PACKET is a valid packet
   (:required-methods :get-next-input-pkt))


(defmethod (character-io-stream-mixin :element-type) ()
  'string-char)


(defmethod (character-io-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (when (send self :get-next-input-pkt no-hang-p)
    (values (pkt-string output-packet) 0 (pkt-nbytes output-packet))
    ))


(defmethod (character-io-stream-mixin :tyi-no-hang) (&optional eof)
  (loop
    until (and si::stream-output-buffer (send self :check-buffer-eof))         ; 09-02-87 DAB IO Support
    unless (send self :setup-next-input-buffer t);Reached end of file
    return
    (and eof output-packet
	 (let ((packet-opcode (pkt-opcode output-packet)))
	   (or
	     (= packet-opcode eof-op)
	     (= packet-opcode los-op)
	     (= packet-opcode cls-op)))
	 (ferror 'end-of-file-1 "End of file on ~S." self))	   ;Here we have a character available
    finally
    (return
      (prog1
	(aref si::stream-output-buffer si::stream-output-index)
	(incf si::stream-output-index)))))



(defflavor binary-io-stream-mixin (output-packet) (io-stream-mixin binary-output-stream-mixin)
  (:included-flavors basic-stream si:buffered-io-stream)
   (:required-methods :get-next-input-pkt))

(defmethod (binary-io-stream-mixin :element-type) ()
  '(unsigned-byte 8))

(defmethod (binary-io-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (when (send self :get-next-input-pkt no-hang-p)
    (values output-packet first-data-word-in-pkt
	    (+ first-data-word-in-pkt (truncate (pkt-nbytes output-packet) 2)))))



;;------------------------------------------------------------------------------
;;; Now the instantiatable flavors

(defflavor input-character-stream ()
   (character-input-stream-mixin basic-input-stream si:buffered-input-character-stream))


(defflavor output-character-stream ()
   (character-output-stream-mixin basic-output-stream si:buffered-output-character-stream))

(defflavor io-character-stream ()
   (character-io-stream-mixin basic-io-stream
    si:buffered-io-character-stream output-character-stream ))

(defflavor character-stream ()
   (character-input-stream-mixin character-output-stream-mixin basic-input-stream
    basic-output-stream si:buffered-character-stream))

;;; This is to make the EVAL server work

(defmethod (character-stream :beep) (&optional ignore))


(compile-flavor-methods input-character-stream output-character-stream character-stream)

;;------------------------------------------------------------------------------

(defflavor input-binary-stream ()
   (binary-input-stream-mixin basic-input-stream si:buffered-input-stream))


(defflavor output-binary-stream ()
   (binary-output-stream-mixin basic-output-stream si:buffered-output-stream))

(defflavor io-binary-stream ()
   (binary-io-stream-mixin basic-io-stream si:buffered-io-stream))

(defflavor binary-stream ()
   (binary-input-stream-mixin binary-output-stream-mixin basic-input-stream basic-output-stream
    si:buffered-stream))


(compile-flavor-methods input-binary-stream output-binary-stream binary-stream
			io-character-stream  io-binary-stream)

;;------------------------------------------------------------------------------

(defflavor ascii-translating-input-character-stream ()
   (si:ascii-translating-input-stream-mixin character-input-stream-mixin basic-input-stream
    si:buffered-tyi-input-stream))


(defflavor ascii-translating-output-character-stream ()
   (si:ascii-translating-output-stream-mixin character-output-stream-mixin basic-output-stream
    si:buffered-tyo-output-stream))


(defflavor ascii-translating-character-stream ()
   (si:ascii-translating-input-stream-mixin si:ascii-translating-output-stream-mixin
    character-input-stream-mixin character-output-stream-mixin basic-input-stream
    basic-output-stream si:buffered-tyi-tyo-stream))


(compile-flavor-methods ascii-translating-input-character-stream
   ascii-translating-output-character-stream ascii-translating-character-stream)


;;------------------------------------------------------------------------------
;;                             CREATE CHAOSNET STREAMS
;;------------------------------------------------------------------------------


(defun open-stream (host contact-name &key &optional (window-size default-window-size) (timeout (* 10 60))
  (direction :bidirectional) (error t) (characters t) (ascii-translation nil) &aux conn)
  "Open a chaosnet connection and return a stream that does I/O to it.
   HOST is the host to connect to; CONTACT-NAME is the contact name at that host.
   The keyword arguments are:
   :WINDOW-SIZE - number of packets to allow in transit to this host over the connection.
   :TIMEOUT - how long to wait before assuming the host is down.
   :ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
    and translate to and from the Lisp machine character set as appropriate.
   :DIRECTION, :CHARACTERS, :ERROR - as in OPEN.  :DIRECTION defaults to ':BIDIRECTIONAL."
  (condition-case-if (not error) (error-object)
     (setq conn
	   (if host
	     (connect host contact-name window-size timeout)
	     (listen contact-name window-size)))
     (remote-network-error error-object)
     (:no-error (when (null host)
		  (accept conn))
      (make-stream conn :direction direction :characters characters :ascii-translation
		   ascii-translation))))



(defun make-stream (connection &key &optional (direction :bidirectional) (characters t) (ascii-translation nil))
  "Return a stream that does i/o to an already established chaos connection.
   :ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
    and translate to and from the Lisp machine character set as appropriate.
   :DIRECTION, :CHARACTERS - as in OPEN.  :DIRECTION defaults to ':BIDIRECTIONAL."
  (make-instance
   (case direction
     (:input
      (cond
	(ascii-translation 'ascii-translating-input-character-stream)
	(characters 'input-character-stream)
	(t 'input-binary-stream)))
     (:output
      (cond
	(ascii-translation 'ascii-translating-output-character-stream)
	(characters 'output-character-stream)
	(t 'output-binary-stream)))
     (:IO
      (cond
	(characters 'io-character-stream)
	(t 'io-binary-stream)))
     (:bidirectional
      (cond
	(ascii-translation 'ascii-translating-character-stream)
	(characters 'character-stream)
	(t 'binary-stream))))
   :connection connection))


(deff stream 'make-stream)




;;; The following function is used whenever one wants to make many connections
;;; to many hosts quickly.  It is used as part of Hostat, Finger (all LMs),
;;; and find-user-logged-in...


(defun make-fast-connection-list (hosts contact-name &optional (window-size 1) use-all-addresses)
  "Return a list of (HOST . CONNECTIONs) at CONTACT-NAME.  The caller is responsible for
   checking the state of the connection.  CONNECTIONS is a list of (ADDRESS . CONN).  CONN
   is () if the connection table was full at the time."
  (let ((table-full))
    (assure-enabled)
    (loop for host in hosts
	  when (send host :network-address-list :chaos)
	  collect (cons host
			(loop for address in (if use-all-addresses
						 (send host :network-address-list :chaos)
						 (list (send host :network-address :chaos)))
			      collect (cons address
					    (condition-case ()
						(if (not table-full)
						    (open-connection address contact-name window-size)
						    ())	   ;not likely for table to shrink
					      (network-resources-exhausted (setq table-full t) nil))))))))
