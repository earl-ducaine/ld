;;  -*- Mode:COMMON-LISP; Package: CHAOS; BASE:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

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
;; Copyright (c) 1980 Massachusetts Institute of Technology **
;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved
;;
;;
;;------------------------------------------------------------------------------
;;                    CHAOSNET ROUTING FOR LISP MACHINE
;;------------------------------------------------------------------------------
;;
;;  4/08/86 MMG - Moved menu stuff to Menus.Lisp.
;;  3/19/86 MMG - Common Lisp conversion.
;;  4/19/85 RAF - Increase routing table size from 64 to 256.
;;  3/13/85 RAF - Allow Chaosnet comm to self when no controller is in chasis.
;; 12/04/84 RAF - Modify for Controller objects, verify RUT code, to BASE 10.
;; 12/03/84 RAF - Start adding mods for Flavor based controllers.
;; 11/20/84 RAF - Fix XMIT-PKT-ROUTE to go thru the Routing table properly.
;; 11/19/84 RAF - Add Routing Patches to use MY-ADDRESSES & MY-SUBNETS.
;; 10/25/84 RAF - Add CHAOS-BAD-VERSION.
;;  9/18/84 RAF - Put MY-ADDRESS as gateway to MY-SUBNET.
;;  9/06/84 RAF - Fix call to XMIT-VIA-ETHERNET.
;;  8/01/84 RAF - Add NET-XXX-INIT-LISTs.
;;  7/31/84 RAF - Add MIT Patch 98.48, to Dump Remote Routing tables.
;;  5/31/84 RAF - Cloned from CHSNCP.
;;
;;------------------------------------------------------------------------------
;; NOTE - This version of CHAOSNET routing does NOT support broadcasting
;;        Chaosnet packets.
;;------------------------------------------------------------------------------
;;                          ADVERTISED FUNCTIONS
;;
;; RESET-ROUTING-TABLE - Reset Routing table to default state.
;; RECEIVE-PKT         - Receive a packet & either route it to another host or ME.
;; TRANSMIT-PKT-ROUTE  - Transmit a packet to appropriate subnet or to ME.
;; BACKGROUND-UPDATE-ROUTING-TABLE - Dynamically increases cost of nets with NIL type.
;; PRINT-ROUTING-TABLE - Prints my routing table.
;; SHOW-ROUTING-TABLE  - Display Routing Table of a remote host.
;; SHOW-ROUTING-PATH   - Print path to a host.  (not working yet.?)
;;
;;
;;                                PROCESSES
;;
;; RUT-TRANSMITTER-PROCESS - (not turned on) Process to send & update dynamic
;;     routing data.
;;------------------------------------------------------------------------------

;;; Routing table

;;; This array is the routing table: if we want to send a message to a given subnet,
;;; to where should I forward it? If the subnet # is greater than the length of the
;;; array, use the contents of array element zero. The contents of the array are
;;; the host number on one of our subnets who knows how to handle this packet. Note
;;; that for now we can only be on one subnet of each type. Don't use this table
;;; unless you are sure that the packet is not going to a host on THIS subnet!
;;; These tables are filled in by code in initialize-ncp-once.

(defparameter routing-table-size 256
  "The number of subnets in the routing table")

(defvar *routing-table*
  (make-array routing-table-size
	      :element-type t)
  "Contains controller object when type is 'direct, else contains
   address of gateway.")

(DEFVAR ROUTING-TABLE-COST
   (MAKE-ARRAY ROUTING-TABLE-SIZE :ELEMENT-TYPE '(UNSIGNED-BYTE 16) :AREA PERMANENT-STORAGE-AREA)
   "Relative cost of sending to this Subnet.")
(DEFVAR ROUTING-TABLE-TYPE
   (MAKE-ARRAY ROUTING-TABLE-SIZE :ELEMENT-TYPE T :AREA PERMANENT-STORAGE-AREA)
   "'DIRECT if a local subnet, 'FIXED-BRIDGE for fixed routing to a remote subnet,
   or nil for any other (assumed) remote subnet.")
(DEFVAR MAXIMUM-ROUTING-COST 1024 "The maximum value in the routing table that is real.")


(NET::DEFVAR-FOR-PEEK-A-BOO RUT-PKTS-RECEIVED 0 "Number of RUT pkts received.")
(NET::DEFVAR-FOR-PEEK-A-BOO CHAOS-PKTS-BAD-VERSION 0 "Number of pkts with bad version number.")
(NET::DEFVAR-FOR-PEEK-A-BOO PKTS-CANT-REACH-SUBNET 0
   "Number of pkts dropped due to not knowing how to route to their subnet.")

(DEFUN NULL-TRANSMITTER (OP &REST ARGS)
  (SELECT OP
    (:TRANSMIT (INCF PKTS-CANT-REACH-SUBNET)
	       (FREE-INT-PKT (THIRD ARGS)))
    (:BOARD-TYPE :NULL)))

;;; *BJ* For *RWF*


(DEFVAR *ALL-SUBNETS-ACCESSIBLE-P* T "if true, dump all unroutable packets unto controller F0")


(DEFUN RESET-ROUTING-TABLE ()			;update 10/18/84 RAF
  "Flush out old routing data & set it to the default state."
  ;; Assumes all hosts have an Ethernet connection.  Hosts on Chaos MAY be accessed via
  ;; Ethernet ==> ethernet hosts use Ethernet to reach hosts on Chaos subnet.
  (WHEN (BOUNDP 'NET::CONTROLLER-LIST)
    (DOTIMES (I ROUTING-TABLE-SIZE)		;Clear out the routing table
      (SETF (AREF ROUTING-TABLE I) 0)		;use gateway 0
      (SETF (AREF ROUTING-TABLE-COST I) MAXIMUM-ROUTING-COST)	;says we really don't know
      (SETF (AREF ROUTING-TABLE-TYPE I) ()))
    ;; Set the null transmitter for Subnet 0.
    ;; *NOTE* this is NOT used for Broadcast packets.
    (SETF (AREF ROUTING-TABLE 0) #'NULL-TRANSMITTER)
    (SETF (AREF ROUTING-TABLE-TYPE 0) 'DIRECT)

    ;; Setup routing from var MY-ADDRESSES instead of relying on subnet var in controller. *BJ*
    (DO* ((CC NET::CONTROLLER-LIST (rest cc))		;Set the addressed direct entries
	  (addresses my-addresses (rest addresses))
	  subnet)
	 ((or (null cc) (null addresses)))
      (setf subnet (subnet (first addresses)))
      (SETF (AREF ROUTING-TABLE SUBNET) (first cc))
      (SETF (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
      (SETF (AREF ROUTING-TABLE-COST SUBNET) 12))

;    (DOLIST (CC NET::CONTROLLER-LIST)		;Set the addressed direct entries
;      (LET ((SUBNET (SEND CC :SUBNET)))
;	(SETF (AREF ROUTING-TABLE SUBNET) CC)
;	(SETF (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
;	(SETF (AREF ROUTING-TABLE-COST SUBNET) 12)))

    (DOLIST (CABLE-GROUP (GET-SITE-OPTION :CHAOS-CABLE-ASSIGNMENTS t))	;Set the non-addressed direct entries
      (LET ((CC (GET-CONTROLLER-FOR-GROUP CABLE-GROUP)))
	(WHEN CC
	  (DOLIST (SUBNET CABLE-GROUP)
	    (COND
	      ((NUMBERP SUBNET)
	       (WHEN (NULL (AREF ROUTING-TABLE-TYPE SUBNET))
		 (SETF (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
		 (SETF (AREF ROUTING-TABLE SUBNET) CC)
		 (SETF (AREF ROUTING-TABLE-COST SUBNET) 12)))
	      ((CONSP SUBNET)
	       (DOTIMES (SUBNET-OFFSET (1+ (- (SECOND SUBNET) (FIRST SUBNET))))
		 (WHEN (NULL (AREF ROUTING-TABLE-TYPE (+ (FIRST SUBNET) SUBNET-OFFSET)))
		   (SETF (AREF ROUTING-TABLE-TYPE (+ (FIRST SUBNET) SUBNET-OFFSET)) 'DIRECT)
		   (SETF (AREF ROUTING-TABLE (+ (FIRST SUBNET) SUBNET-OFFSET)) CC)
		   (SETF (AREF ROUTING-TABLE-COST (+ (FIRST SUBNET) SUBNET-OFFSET)) 12))))
	      ((MEMBER SUBNET '(:ANY :ALL :EVERY T) :TEST #'EQL)
	       (SETF (AREF ROUTING-TABLE 0) CC)))))))
    (WHEN (AND *ALL-SUBNETS-ACCESSIBLE-P* (EQL (AREF ROUTING-TABLE 0) #'NULL-TRANSMITTER))

      (SETF (AREF ROUTING-TABLE 0) (FIRST NET::CONTROLLER-LIST)))))

;;(DEFUN RESET-ROUTING-TABLE ()			;update 10/18/84 RAF
;;  "Flush out old routing data & set it to the default state."
;;  ;; Assumes all hosts have an Ethernet connection.  Hosts on Chaos MAY be accessed via
;;  ;; Ethernet ==> ethernet hosts use Ethernet to reach hosts on Chaos subnet.
;;  (WHEN (BOUNDP 'NET::CONTROLLER-LIST)
;;    (DOTIMES (I ROUTING-TABLE-SIZE)		;Clear out the routing table
;;      (SETF (AREF ROUTING-TABLE I) 0)		;use gateway 0
;;      (SETF (AREF ROUTING-TABLE-COST I) MAXIMUM-ROUTING-COST)	;says we really don't know
;;      (SETF (AREF ROUTING-TABLE-TYPE I) ()))
;;    ;; Set the null transmitter for Subnet 0.
;;    ;; *NOTE* this is NOT used for Broadcast packets.
;;    (SETF (AREF ROUTING-TABLE 0) #'NULL-TRANSMITTER)
;;    (SETF (AREF ROUTING-TABLE-TYPE 0) 'DIRECT)
;;    (DOLIST (CC NET::CONTROLLER-LIST)		;Set the addressed direct entries
;;      (LET ((SUBNET (SEND CC :SUBNET)))
;;	(SETF (AREF ROUTING-TABLE SUBNET) CC)
;;	(SETF (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
;;	(SETF (AREF ROUTING-TABLE-COST SUBNET) 12)))

;;    (DOLIST (CABLE-GROUP (GET-SITE-OPTION :CHAOS-CABLE-ASSIGNMENTS))	;Set the non-addressed direct entries
;;      (LET ((CC (GET-CONTROLLER-FOR-GROUP CABLE-GROUP)))
;;	(WHEN CC
;;	  (DOLIST (SUBNET CABLE-GROUP)
;;	    (COND
;;	      ((NUMBERP SUBNET)
;;	       (WHEN (NULL (AREF ROUTING-TABLE-TYPE SUBNET))
;;		 (SETF (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
;;		 (SETF (AREF ROUTING-TABLE SUBNET) CC)
;;		 (SETF (AREF ROUTING-TABLE-COST SUBNET) 12)))
;;	      ((CONSP SUBNET)
;;	       (DOTIMES (SUBNET-OFFSET (1+ (- (SECOND SUBNET) (FIRST SUBNET))))
;;		 (WHEN (NULL (AREF ROUTING-TABLE-TYPE (+ (FIRST SUBNET) SUBNET-OFFSET)))
;;		   (SETF (AREF ROUTING-TABLE-TYPE (+ (FIRST SUBNET) SUBNET-OFFSET)) 'DIRECT)
;;		   (SETF (AREF ROUTING-TABLE (+ (FIRST SUBNET) SUBNET-OFFSET)) CC)
;;		   (SETF (AREF ROUTING-TABLE-COST (+ (FIRST SUBNET) SUBNET-OFFSET)) 12))))
;;	      ((MEMBER SUBNET '(:ANY :ALL :EVERY T) :TEST #'EQL)
;;	       (SETF (AREF ROUTING-TABLE 0) CC)))))))
;;    (WHEN (AND *ALL-SUBNETS-ACCESSIBLE-P* (EQL (AREF ROUTING-TABLE 0) #'NULL-TRANSMITTER))

;;      (SETF (AREF ROUTING-TABLE 0) (FIRST NET::CONTROLLER-LIST)))))


(DEFUN GET-CONTROLLER-FOR-GROUP (GROUP)		;rwf 08/04/86
  "Internal function that returns a controller that is known to handle at least one of the
   chaos subnets specified in the <group> argument."
  (LET (CONTROLLER)
    (DOLIST (SUBNET GROUP)
      (COND
	((NUMBERP SUBNET)
	 (WHEN (EQL 'DIRECT (AREF ROUTING-TABLE-TYPE SUBNET))
	   (RETURN (AREF ROUTING-TABLE SUBNET))))
	((CONSP SUBNET)
	 (SETQ CONTROLLER
	       (DOTIMES (SUBNET-OFFSET (1+ (- (SECOND SUBNET) (FIRST SUBNET))))
		 (WHEN (EQL 'DIRECT (AREF ROUTING-TABLE-TYPE (+ (FIRST SUBNET) SUBNET-OFFSET)))
		   (RETURN (AREF ROUTING-TABLE (+ (FIRST SUBNET) SUBNET-OFFSET))))))
	 (WHEN CONTROLLER
	   (RETURN CONTROLLER)))))))


;;; The following is a slightly dangerous, special purpose, function.  It is intended to
;;; handle two situations a: we do not know what nets we are really attached to
;;; (for example when an unconfigured band is booteed) and b: more than one
;;; logical net is running on a physical net (as when "transperant bridges" are
;;; in use linking multiple sites).   rwf 12/85

(DEFUN MAKE-ALL-SUBNETS-ACCESSIBLE ()
  "Set Up Routing such that if no route to a subnet is known, it is sent to whatever
   net is attached to the ethernet controller in the smallest numbered chassis slot."
  (WHEN (AND (BOUNDP 'NET::CONTROLLER-LIST) (CONSP NET::CONTROLLER-LIST))
    (SETF (AREF ROUTING-TABLE 0) (FIRST NET::CONTROLLER-LIST))
    (SETF (AREF ROUTING-TABLE-TYPE 0) 'DIRECT)
    (SETF (AREF ROUTING-TABLE-COST 0) 10)))

(DEFUN BACKGROUND-UPDATE-ROUTING-TABLE ()
  "Dynamically increases cost of subnets with TYPE of Nil. Called from BACKGROUND *only*."
  (DOTIMES (I (ARRAY-TOTAL-SIZE ROUTING-TABLE-COST))
    (WHEN (NULL (AREF ROUTING-TABLE-TYPE I))
      (SETF (AREF ROUTING-TABLE-COST I)
	    (MIN (+ (AREF ROUTING-TABLE-COST I) 1) MAXIMUM-ROUTING-COST)))))

;;(ADD-INITIALIZATION "Routing" '(RESET-ROUTING-TABLE) '(NORMAL) 'NET-SYSTEM-INIT-LIST)


;;; Called by anyone with an INT-PKT. It looks at the destination
;;; field in the INT-PKT and sends it somewhere. The host and subnet
;;; can be passed as optional arguments, so that this function can be
;;; used for debugging purposes.
(defun transmit-pkt-route (int-pkt &optional
				     (host (pkt-dest-address int-pkt))
				     (subnet (pkt-dest-subnet int-pkt)))
  (record-int-pkt-header int-pkt)
  ;; Simple routing
  (when (>= subnet (array-total-size routing-table))
    (setq subnet 0))
  ;; Now transmit to the host ,or gateway.
  (when (logtest 128 (pkt-opcode int-pkt))
    (incf data-pkts-out))
  (incf pkts-transmitted)
  (cond
    ;; Handle "transmission" to this machine by receiving this pkt.
    ;; 5/84 raf (eek shared ethernet?)
    ((member host my-addresses :test #'eql)
     (receive-int-pkt-for-me int-pkt (pkt-opcode int-pkt)))
    ((and (zerop host)
	  (not (zerop (pkt-fwd-count int-pkt))))
     ;; don't b-cast if forwarding. 5/84 raf.
     (free-int-pkt int-pkt))
    (t
     ;; Note - loop seems more efficient than do in this case. raf go
     ;; thru the routing-table until a route is found. If one is is
     ;; not found the loop will loop only (length routing-table) times
     ;; and then report an error. This keeps an infinite loop from
     ;; occuring. rvl. Just throw away the packet - rwf 07/31/86
     (loop for i from 0 to (length routing-table) by 1
	if (eq (aref routing-table-type subnet) 'direct)
	;; This send pulled up into the body of loop to prevent
	;; sending a packet to host 0.  *bj* for *rwf*
	do (progn
	     (when (zerop host)
	       (setf host (pkt-dest-address int-pkt)))
	     (send (aref routing-table subnet)
		   ;; Should be a controller object
		   :transmit :chaos host
		   ;; host should be a chaos address from pkt or
		   ;; routing table.  04-08-88 dab was 20. octal
		   ;; conversion error.
		   int-pkt (+ (pkt-nbytes int-pkt) 16))
	     (return))
	else do	(setq host (aref routing-table subnet)
		      subnet (subnet host))
	finally	(progn
		  ;; (setf host   (pkt-dest-address int-pkt))	;reset host   to original value.
		  ;; (setf subnet (pkt-dest-subnet  int-pkt))	;reset subnet to original value.
		  (setf subnet 0)
		  (incf pkts-cant-reach-subnet)
		  (free-int-pkt int-pkt)
		  ;; (ferror 'sys:network-error "routing table corrupted, contains cyclic routing path")
		  ))
	  ;;;  (when (and (< 0 (pkt-fwd-count int-pkt))
	  ;;;    (not (eql subnet 0)))		;rwf 08/10/86
	  ;;;     (cerror t t 'bad-forwarding))
     )))

(DEFF TRANSMIT-INT-PKT 'TRANSMIT-PKT-ROUTE)

;;------------------------------------------------------------------------------
;;                         MAIN RECEIVER FUNCTION
;;------------------------------------------------------------------------------
;;
;; This function is called to receive a Chaos packet.  It routes it to the correct handler.
;;
;;
;;; This function is the called on an INT-PKT which has just come in from the net.
;;; It is mostly a transfer vector to more specialized functions, but it also
;;; does error checking.
;;; Note: Functions reached from here must not, in any case, go blocked, particularily
;;;  now that the RECEIVER process is flushed.  A common case is to call functions
;;;  which ordinarily could go blocked waiting for a INT-PKT, but this is prevented from
;;;  happening by setting up RESERVED-INT-PKT to the INT-PKT just received.
;;

;;; *BJ* For *RWF*
(DEFVAR *RECORD-RUT-HEADERS* () "t to record rut broadcast packets as well as locals")


(NET::DEFVAR-FOR-PEEK-A-BOO PKTS-TOO-BIG 0 "Number of pkts too big to be valid Chaos pkts.")

(NET::DEFVAR-FOR-PEEK-A-BOO PKTS-UNFORWARDED 0
  "Number of pkts to be forwarded, except not a bridge.")

(NET::DEFVAR-FOR-PEEK-A-BOO NON-LOCAL-RUT-PKTS-RECEIVED 0
  "Number of rut pkts from undirectly reachable net.")


(DEFUN RECEIVE-PKT (INT-PKT &AUX (OP (PKT-OPCODE INT-PKT)))
  (WHEN (NOT (ZEROP (LDB (BYTE 8 0) (AREF INT-PKT 0))))
	(INCF CHAOS-PKTS-BAD-VERSION))

  (COND ((= OP RUT-OP)
	 (IF (NOT (EQL (AREF ROUTING-TABLE-TYPE (SUBNET (PKT-SOURCE-ADDRESS INT-PKT))) 'DIRECT))
	     (PROGN
	       (INCF NON-LOCAL-RUT-PKTS-RECEIVED)  ;rwf 08/12/86
	       (FREE-INT-PKT INT-PKT))	   ;ignore erroneously forwarded rut broadcasts

	     (PROGN
	       (INCF RUT-PKTS-RECEIVED)
	       (DO ((I FIRST-DATA-WORD-IN-PKT (+ I 2))
		    (N (/ (PKT-NBYTES INT-PKT) 4) (1- N))
		    (GATEWAY (PKT-SOURCE-ADDRESS INT-PKT))
		    (N-SUBNETS (ARRAY-TOTAL-SIZE ROUTING-TABLE))
		    (SUBNET)
		    (COST))
		   ((ZEROP N)
		    (FREE-INT-PKT INT-PKT))

		 (SETQ SUBNET (AREF INT-PKT I)
		       COST (AREF INT-PKT (1+ I)))

		 (WHEN (AND (< SUBNET N-SUBNETS)
			    (<= COST (AREF ROUTING-TABLE-COST SUBNET))
			    (NULL (AREF ROUTING-TABLE-TYPE SUBNET)))

		       (IF (NOT (EQL (SUBNET GATEWAY) SUBNET))
			   (SETF (AREF ROUTING-TABLE SUBNET) GATEWAY
				 (AREF ROUTING-TABLE-COST SUBNET) COST)
			   ;;; (ferror nil "Routing Table Screwup"))
			   )))))
	 (WHEN *RECORD-RUT-HEADERS*
	       (RECORD-INT-PKT-HEADER INT-PKT))	   ;rwf 08/11/86
	 )
	((ZEROP (PKT-DEST-ADDRESS INT-PKT))
	 (RECEIVE-INT-PKT-FOR-ME INT-PKT OP))	   ;RLA 9/15/86

	((NOT (MEMBER (PKT-DEST-ADDRESS INT-PKT) MY-ADDRESSES :TEST #'EQL))	   ;Pkt to be forwarded
	 (COND ((>= (PKT-FWD-COUNT INT-PKT) 17) (FREE-INT-PKT INT-PKT) (INCF PKTS-OVER-FORWARDED))
	       ((> (PKT-NBYTES INT-PKT) MAX-DATA-BYTES-PER-PKT)
		(FREE-INT-PKT INT-PKT)
		(INCF PKTS-TOO-BIG))

	       (T
		 (IF (NOT (LOCAL-HOST-IS-BRIDGE-P))
		     (PROGN
		       (INCF PKTS-UNFORWARDED)	   ;rwf 08/11/86
		       ;;;			     (ferror 'unforwarded)
		       (FREE-INT-PKT INT-PKT))
		     (PROGN
		       (INCF PKTS-FORWARDED)
		       (INCF (PKT-FWD-COUNT INT-PKT))
		       (TRANSMIT-PKT-ROUTE INT-PKT))))))

	(T (RECEIVE-INT-PKT-FOR-ME INT-PKT OP))))

(DEFF RECEIVE-INT-PKT 'RECEIVE-PKT)                ;Required for CHAOS-RECEIVER
(MAKE-OBSOLETE RECEIVE-INT-PKT "Use RECEIVE-PKT")

(DEFUN PRINT-ROUTING-TABLE (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Display onto STREAM a list of subnets, gateways, and chaonet costs for each subnet."
  (FORMAT STREAM "~&Subnet	Fastest Gateway~35TCost")
  (DOTIMES (I (ARRAY-TOTAL-SIZE ROUTING-TABLE))
    (IF (EQ 'DIRECT (AREF ROUTING-TABLE-TYPE I))
      (PROGN
	(FORMAT STREAM "~&~3O          Direct" I)
	(UNLESS (MEMBER I MY-SUBNETS :TEST #'EQL)
	  (FORMAT STREAM " - *Local Host has no address on this Subnet*")))
      (WHEN (NEQ (AREF ROUTING-TABLE I) 0)
	(LET ((GATEWAY (AREF ROUTING-TABLE I)))
	  (FORMAT STREAM "~&~3O	~O ~A~35T~O" I GATEWAY (HOST-DATA GATEWAY)
		  (AREF ROUTING-TABLE-COST I))
	  (WHEN (AREF ROUTING-TABLE-TYPE I)
	    (FORMAT STREAM " - ~A" (AREF ROUTING-TABLE-TYPE I))))))))

;;------------------------------------------------------------------------------
;;                       ROUTING PACKET TRANSMITTER
;;------------------------------------------------------------------------------

;; Looks like this ought to work.  12/84 RAF.
;;

(DEFVAR RUT-TRANSMITTER-PROCESS ())
(NET::DEFVAR-FOR-PEEK-A-BOO *NUMBER-OF-ROUTING-PACKETS* 0 "Number of routing packets")

;;; *BJ* For *RWF*
(DEFUN LOCAL-HOST-IS-BRIDGE-P ()
  (< 1 (LENGTH MY-SUBNETS)))

;; *BJ* for *DJ*
;; Only start RUT transmitter if this host is a bridge.
(DEFUN START-RUT-TRANSMITTER ()
  "Starts the RUT transmitter process for gateways."
  (WHEN (LOCAL-HOST-IS-BRIDGE-P)		   ;if we are a bridge, start a route transmitter
    (UNLESS (TYPEP RUT-TRANSMITTER-PROCESS 'PROCESS) ;unless we already have one
      (SETF RUT-TRANSMITTER-PROCESS
	    (MAKE-PROCESS "Chaos RUT transmitter" :WARM-BOOT-ACTION () :PRIORITY 30)))
    (SEND RUT-TRANSMITTER-PROCESS :PRESET 'RUT-TRANSMITTER-TOP-LEVEL)
    (PROCESS-RESET-AND-ENABLE RUT-TRANSMITTER-PROCESS)))

(DEFUN STOP-RUT-TRANSMITTER ()
  (WHEN (TYPEP RUT-TRANSMITTER-PROCESS 'PROCESS)
    (SEND RUT-TRANSMITTER-PROCESS :KILL t)
    (SETF RUT-TRANSMITTER-PROCESS NIL)))

;;(DEFUN START-RUT-TRANSMITTER ()			;OK for TI, 12/84 RAF.
;;  "Starts the RUT transmitter process for gateways."
;;  ;; Set priority 1 below the Receiver priority (same as Chaos-Background).
;;  (IF (NULL RUT-TRANSMITTER-PROCESS)
;;      (SETQ RUT-TRANSMITTER-PROCESS
;;	    (MAKE-PROCESS "Chaos RUT transmitter" :WARM-BOOT-ACTION () :PRIORITY 30)))
;;  (IF (NOT (LOCAL-HOST-IS-BRIDGE-P))
;;      (PROCESS-DISABLE RUT-TRANSMITTER-PROCESS)
;;      (PROGN
;;	(SEND RUT-TRANSMITTER-PROCESS :PRESET 'RUT-TRANSMITTER-TOP-LEVEL)
;;	(PROCESS-RESET-AND-ENABLE RUT-TRANSMITTER-PROCESS))))

;;(DEFUN STOP-RUT-TRANSMITTER ()
;;  (WHEN (NOT (NULL RUT-TRANSMITTER-PROCESS))
;;    (SEND RUT-TRANSMITTER-PROCESS :REVOKE-RUN-REASON)))

(add-initialization "Start RUT Transmitter" '(START-RUT-TRANSMITTER) nil 'enable-list)

(add-initialization "Stop RUT Transmitter" '(STop-RUT-TRANSMITTER) nil 'disable-list)

(DEFUN RUT-TRANSMITTER-TOP-LEVEL ()
 ;; Use MY-ADDRESSES & MY-SUBNETS, also
 ;;  add cost of local subnet to forwarding cost.  12/84 RAF
  (LOOP
   (DO ((SUBS MY-SUBNETS (CDR SUBS))
	(ADDRS MY-ADDRESSES (CDR ADDRS))
	(INT-PKT (ALLOCATE-INT-PKT) (ALLOCATE-INT-PKT)))
       ((NULL SUBS)
	(FREE-INT-PKT INT-PKT))
     (DO ((SUBNET 1 (1+ SUBNET))
	  (I FIRST-DATA-WORD-IN-PKT)
	  (N 0))
	 ((= SUBNET (ARRAY-TOTAL-SIZE ROUTING-TABLE))
	  (SETF (PKT-NBYTES INT-PKT) (* N 4)))
       (COND
	 ((AND (< (AREF ROUTING-TABLE-COST SUBNET) MAXIMUM-ROUTING-COST)
	     (NOT (= SUBNET (FIRST SUBS))));don't bother sending for this subnet.
	  (SETF (AREF INT-PKT I) SUBNET)
	  (SETF (AREF INT-PKT (1+ I)) (+ (AREF ROUTING-TABLE-COST SUBNET) 10))
	  (SETQ I (+ I 2)) (SETQ N (+ N 1)))))
     (SETF (PKT-OPCODE INT-PKT) RUT-OP)
     (SETF (PKT-SOURCE-ADDRESS INT-PKT) (FIRST ADDRS))
     (SETF (PKT-SOURCE-INDEX-NUM INT-PKT) 0)
     (SETF (PKT-DEST-ADDRESS INT-PKT) 0)
     (SETF (PKT-DEST-INDEX-NUM INT-PKT) 0)
     (TRANSMIT-PKT-ROUTE INT-PKT 0 (FIRST SUBS)); frees int-pkt
     (INCF *NUMBER-OF-ROUTING-PACKETS*))
   (PROCESS-SLEEP (* 15 60))))

;;------------------------------------------------------------------------------
;;                            SHOW ROUTING DATA
;;------------------------------------------------------------------------------

;; Routing table format: for N subnets, N*4 bytes of data, holding N*2 words
;; For subnet n, pkt[2n] has the method; if this is less than 400 (octal), it's
;; an interface number; otherwise, it's a host which will forward packets to that
;; subnet.  pkt[2n+1] has the host's idea of the cost.


(DEFUN FORMAT-ROUTING-TABLE-PKT (PKT &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX METHOD COST)
  (FORMAT STREAM "~%Subnet   Method                Cost~%")
  (DOTIMES (I (TRUNCATE (PKT-NBYTES PKT) 4))
    (WHEN (AND (NOT (ZEROP (SETQ METHOD (AREF PKT (+ FIRST-DATA-WORD-IN-PKT (* I 2))))))
	(< (SETQ COST (AREF PKT (+ FIRST-DATA-WORD-IN-PKT 1 (* I 2))))
	   (TRUNCATE MAXIMUM-ROUTING-COST 2)))
      (FORMAT STREAM "~3O      ~A~28T~6D~%" I
	      (IF (< METHOD 256)
		(FORMAT () "Interface ~D" METHOD)
		(HOST-DATA METHOD))
	      COST))))


(DEFUN SHOW-ROUTING-TABLE (HOST &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the routing table for some HOST."
  (CONDITION-CASE (PKT) (SIMPLE HOST "DUMP-ROUTING-TABLE")
     (NETWORK-ERROR (SEND STREAM :FRESH-LINE) (SEND STREAM :STRING-OUT "Network error: ")
      (SEND PKT :REPORT STREAM))
     (:NO-ERROR (UNWIND-PROTECT (FORMAT-ROUTING-TABLE-PKT
				 PKT
				 STREAM)
		  (RETURN-PKT PKT)))))


(DEFUN SHOW-ROUTING-PATH (&KEY TO &OPTIONAL (FROM SI:LOCAL-HOST) (TO-HOST TO) &AUX METHOD-AS-HOST TO-HOSTP)
  "Show how packets would most likely from host FROM to host TO.
   The required TO argument can be a ChaosNet hostaddress or a subnet number."
  (CHECK-ARG FROM ADDRESS-PARSE "a valid HOST identifier")
  (SETQ TO-HOSTP (EQ TO TO-HOST))
  (CHECK-ARG TO (OR (AND (NUMBERP TO) (< TO 255)) (ADDRESS-PARSE TO))
     "a valid HOST or SUBNET identifier")
  (WHEN TO-HOSTP
    (SETQ TO-HOST TO));to get any correction to TO.
  (CHECK-ARG TO-HOST ADDRESS-PARSE "a valid HOST identifier")
  (UNLESS (AND (NUMBERP TO) (< TO 255))
    (SETQ TO (LDB (BYTE 8 8) (ADDRESS-PARSE TO))))
  (CONDITION-CASE (PKT) (SIMPLE FROM "DUMP-ROUTING-TABLE")
     (NETWORK-ERROR (SEND *TERMINAL-IO* :FRESH-LINE)
      (SEND *TERMINAL-IO* :STRING-OUT "Network error: ") (SEND PKT :REPORT *TERMINAL-IO*))
     (:NO-ERROR
      (UNWIND-PROTECT (LET
		       ((METHOD (AREF PKT (+ FIRST-DATA-WORD-IN-PKT (* 2 TO))))
			(COST (AREF PKT (+ FIRST-DATA-WORD-IN-PKT 1 (* 2 TO)))))
		       (COND
			 ((OR (ZEROP METHOD) (>= TO (TRUNCATE (PKT-NBYTES PKT) 4)))
			  (FORMAT T "~&No routing table entry for subnet ~O in ~A." TO
				  (HOST-DATA FROM)))
			 ((< METHOD 256)
			  (FORMAT T
				  "~&Direct path from ~A to host ~A on subnet ~O at interface ~D."
				  (HOST-DATA FROM) TO-HOST TO METHOD))
			 (T
			  (FORMAT T "~&~A will bounce the packet off ~A at cost ~D."
				  (HOST-DATA FROM) (HOST-DATA METHOD) COST)
			  (SETQ METHOD-AS-HOST METHOD))))
	(RETURN-PKT PKT))
      (IF METHOD-AS-HOST
	(SHOW-ROUTING-PATH :FROM METHOD-AS-HOST :TO TO :TO-HOST TO-HOST)))))


(DEFUN DUMP-ROUTING-TABLE ();mod. for flavors 12/4/84 raf.
  (LET ((PKT (GET-PKT))
	(PKT-IDX FIRST-DATA-WORD-IN-PKT)
	(N-SUBNETS (MIN (ARRAY-TOTAL-SIZE ROUTING-TABLE) (TRUNCATE MAX-DATA-WORDS-PER-PKT 2))))
    (DOTIMES (SUBNET N-SUBNETS)
      (LET ((GATEWAY (AREF ROUTING-TABLE SUBNET)))
	(SETF (AREF PKT PKT-IDX)
	      (IF (EQ (AREF ROUTING-TABLE-TYPE SUBNET) 'DIRECT)
		  (Send Gateway :Slot)
		GATEWAY))
	(INCF PKT-IDX); deposit cost in next word
	(SETF (AREF PKT PKT-IDX) (AREF ROUTING-TABLE-COST SUBNET))
	(INCF PKT-IDX)))
    ;; set the number of bytes before actually sending....
    (SETF (PKT-NBYTES PKT) (* 4 N-SUBNETS))
    (ANSWER (LISTEN "DUMP-ROUTING-TABLE") PKT)))


(ADD-INITIALIZATION "DUMP-ROUTING-TABLE" '(DUMP-ROUTING-TABLE) () 'SERVER-ALIST)
