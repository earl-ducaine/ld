;;  -*- Mode:COMMON-LISP; Package: CHAOS; BASE:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;
;; Copyright (c) 1980 Massachusetts Institute of Technology
;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved.
;;
;; NOTE - All functions with the comment ";TI" were written at TI, and the MIT copyright
;;        does not apply to them.  Some of them bear the same name as the corresponding
;;        function written by MIT, but are different functions.  All other functions were
;;        written in whole or part at MIT, and some have been modified by TI.

;;------------------------------------------------------------------------------
;;                 ChaosNet definitions for the Lisp Machine
;;------------------------------------------------------------------------------
;;

;; Protocol of May, 1978

;; This file contains the CHAOS net definitions for implementing the sequenced packet
;; protocol of Chaos.

;;  5/06/86 MMG - Rewrote Named-Structure-Invocations for PKT's and CONN's.
;;  4/07/86 MMG - Moved here from CHSNCP.
;;  3/19/86 MMG - Common Lisp conversion.
;;  8/20/85 RAF - Change so CONNs, PKTs, and other arrays are cons'ed in CHAOS-AREA.
;;  3/08/85 RAF - Documentation changes.
;;  1/16/85 RAF - Modify for HAL build.
;; 10/01/84 RAF - Increase INT-PKT size to max for Ethernet.
;;  9/18/84 RAF - Reorganize parts of file.  General cleanup of code.  Remove extraneous
;;                 HOST-DOWN conns.  Free up Pending-RFC's at RESET.
;;  8/01/84 RAF on LAM5  - Add NET-XXX-INIT-LISTs.

;;-----------------------------------------------------------------------------
;;                     Definitions for high-level structures
;;-----------------------------------------------------------------------------
;;
;; With functions to print them textually and various macros for the specialized
;; formulas of the protocol (Everything here is used everywhere).

;;-----------------------------------------------------------------------------
;;                                      CONN
;;-----------------------------------------------------------------------------
;;
;;  CONN-STATE
;;  CONN-FOREIGN-STATE
;;  CONN-READ-PKTS
;;  CONN-WINDOW-AVAILABLE
;;  CONN-PLIST
;;  CONTACT-NAME
;;  PRINT-CONN
;;
;;-----------------------------------------------------------------------------
;;                                 PKT & INT-PKT
;;-----------------------------------------------------------------------------
;;
;;  PKT-OPCODE
;;  PKT-NBYTES
;;  PKT-STRING
;;  PRINT-PKT
;;  FIRST-DATA-WORD-IN-PKT

;;-----------------------------------------------------------------------------
;; PKT & INT-PKT functions referenced elsewhere:
;;   ALLOCATE-PKT, FREE-PKT, ALLOCATE-INT-PKT, FREE-INT-PKT, CONVERT-TO-PKT,
;;   CONVERT-TO-INT-PKT, RELEASE-PKT

;; Low-level CONN management functions referenced elsewhere:
;;   MAKE-CONNECTION

;; "High" level Transmission functions referenced elsewhere:
;;   TRANSMIT-LOS-INT-PKT, TRANSMIT-STS, TRANSMIT-NORMAL-PKT
;;   RECEIVE-INT-PKT-FOR-ME



;; Some standard abbreviations and mnemonic indicators:
;; Items in this list with no "-" at either end signify abbreviations
;; which may occur in any context, those with one or more "-"'s only occur
;; in a context in which there are "-"'s in exactly those places.
;;
;; PKT		Abbreviation for PACKET.  see the DEFSTRUCT
;; CONN		Abbreviation for CONNECTION.  see the DEFSTRUCT
;; SEND-       	Cause a packet to be "sent" with retry etc. if applicable.
;; TRANSMIT-	Cause a packet to be "transmitted" (i.e. placed on the net).
;;		 This is done by all the SEND- routines by calling an
;;		 appropriate TRANSMIT- routine to put the packet a list of
;;		 packets to be put on the net.
;; -STATE	A symbol representing a state that a connection might be in.
;; -OP		A SPECIAL whose value represents that of a packet op code.

;;; MACROS: for various random things

(in-package :chaos)

(defmacro pktnum-< (a b)
  "Compare two packet numbers, taking wrap-around into account."
  `(logtest 32768 (- ,a ,b)))

(defmacro pktnum-1+ (a)
  "Increment a packet number, with wrap-around."
  `(logand 65535 (1+ ,a)))

(defun pktnum-- (a b &aux tem)
  "Subtract one packet number from another, with wrap-around."
  (setq tem (- a b))
  (cond
    ((< tem 0) (+ tem 65536))
    (t tem)))

;;; Opcodes of packets

(defparameter rfc-op   1 "opcode value for rfc packets.") ;#x1
(defparameter opn-op   2 "opcode value for opn packets.") ;#x2
(defparameter cls-op   3 "opcode value for cls packets.") ;#x3
(defparameter fwd-op   4 "opcode value for fwd packets.") ;#x4
(defparameter ans-op   5 "opcode value for ans packets.") ;#x5
(defparameter sns-op   6 "opcode value for sns packets.") ;#x6
(defparameter sts-op   7 "opcode value for sts packets.") ;#x7
(defparameter rut-op   8 "opcode value for rut packets.") ;#x8
(defparameter los-op   9 "opcode value for los packets.") ;#x9
(defparameter lsn-op  10 "opcode value for lsn packets.") ;#xa
(defparameter mnt-op  11 "opcode value for mnt packets.") ;#xb
(defparameter eof-op  12 "opcode value for eof packets.") ;#xc
(defparameter unc-op  13 "opcode value for unc packets.") ;#xd
(defparameter brd-op  14 "opcode value for brd packets.") ;#xe
(defparameter who-op 16 "opcode value for who packets.")  ;#x10
(defparameter ur-op 17 "opcode value for ur packets.")    ;#x11
(defparameter dat-op 128 "default data opcode for 8-bit byte data.")  ;#x80

;;; This is for printing out packets nicely.

(defparameter opcode-list
  '(zero? rfc opn cls fwd ans sns sts rut los lsn mnt eof unc brd who ur)
  "list of system packet opcode names, indexed by opcode value.")

(defvar background '()
  "this process runs all chaosnet time response actions such as probes
   and retransmission, and some other things for the net.")

(defvar chaos-area '()
  "area for all user accessable structures, i.e. conns and pkts.")

(defvar enable nil "t if chaosnet is enabled (turned on for use).")

;;; Address & Subnet variables

(defvar *chaos-subnet* nil)
(defvar *ether-subnets* nil "A list of the Ethernet and Chaos subnets")
(defvar *my-address* :unbound "This machine's chaosnet address.")
(defvar *my-subnet* :unbound "This machine's chaosnet subnet number.")
(defvar *my-addresses* '() "All addresses for this machine.")
(defvar *my-subnets* '() "All subnets for this machine.")
(defvar *ether-subnets* '() "System wide ether subnets.")

(defun subnet (addr)
  (ldb (byte 8 8) addr))

(defun host-id (addr)
  (ldb (byte 8 0) addr))

(defun initialize-ether-chaos-subnets ()
  (setf *ether-subnets*
	(loop for addr in (chaos-addresses local-host)
	   collect (subnet addr)))
  (setf net:chaos-subnet (first net:ether-subnets)))

;;; Clock constants

(defparameter *retransmission-interval* 170) ;; < 3 seconds
(defparameter probe-interval (* 60 10)) ;; 10 seconds
(defparameter long-probe-interval (* 60 60 1)) ;; 1 minute
(defparameter host-down-interval (* 60 60 3))  ;; now 3, was 1.5 minutes

;;; Execution Lists

(defvar *enable-list* '()
  "Initialization list to enable subsystems")

(defvar *disable-list* '()
  "Iinitialization list to disable subsystems")

(defvar *net-before-cold-init-list* '()
  "Network initializations to run before disk save.")

(defvar *net-system-init-list* '()
  "Network initializations to run at system init time.")

(defvar *net-cold-init-list* '()
  "Network initializations to run at cold init time.")

(defvar *net-warm-init-list* '()
  "network initializations to run at warm init time.")

(defvar *background-execution-list* '()
  "List of forms to run every backbround cycle")

(defvar *background-requests* '()
  "List of requests to the background chaosnet process.")

(defvar *retransmission-needed* t
  "t if retransmission of packets may be needed.  enables background
    process to wake up on clock.  set this whenever you put something
    on send-pkts of a conn.")

(defvar *more-retransmission-needed*)

;; the following are used for negotiating the initial connection
;; between a host and a server.

(defvar *pending-rfc-pkts* '()
  "Incoming rfc packets not yet listened for, linked through the pkt-link.")

(defvar **pending-listens* '()
  "List of (contact-name . conn) for pending listens.")

(defvar *server-alist* ()
  "alist of (contact-name form-to-evaluate) for creating chaos servers.
   entries are put on with add-initialization.  the form is evaluated
   in the background task when an incoming rfc matches the contact
   name and there is no pending listen for it.")

;;; Meters & Debug Aids

(defvar-for-peek-a-boo pkts-made 0
  "Number of chaos pkts ever made")

(defvar-for-peek-a-boo pkts-received 0
  "Number of packets received via chaosnet.")

(defvar-for-peek-a-boo pkts-transmitted 0
  "Number of packets transmitted via chaosnet.")

(defvar-for-peek-a-boo pkts-over-forwarded 0
  "Number of pkts forwarded too often and discarded.")

(defvar-for-peek-a-boo pkts-forwarded 0
  "Number of pkts forwarded.")

(defvar-for-peek-a-boo los-pkt-count 0
  "Number of all los packets received.")

(defvar-for-peek-a-boo current-los-pkt-count 0
  "Number of current los packets.")

(defvar-for-peek-a-boo pkts-retransmitted 0
  "Number of packets retransmitted.")

(defvar-for-peek-a-boo pkts-duplicated 0
  "Number of duplicate packets received.")

(defvar-for-peek-a-boo data-pkts-in 0
  "Number of data packets received.")

(defvar-for-peek-a-boo data-pkts-out 0
  "Number of data packets transmitted.")

;;; Debugging aids which keep records into the past (a short way).

(defvar bad-pkt-list '()
  "List of strings describing packets received in error.")

(defvar los-pkts     '()
  "Chain of recent los pkts received from the network, linked by pkt-link.")

(defvar max-los-pkts-to-keep 0
  "Maximum number of LOS packets to keep on LOS-PKTS.
    There may actually be more but they will be used by allocator.")

(DEFVAR RECENT-HEADERS 0
  "Array of 200 most recent packet transactions' headers.
    Each row of the array contains the eight header words of the
    packet and the time at which the record was made.")

(DEFVAR RECENT-HEADERS-POINTER 0
  "Next index to use in storing in RECENT-HEADERS.")

;; Conn definitions

(defstruct (conn :named
		 (:conc-name nil)
		 (:predicate nil)
		 (:copier nil)
		 (:type vector))
  ;; Window size for receiving.
  local-window-size
  ;; Window size for transmitting.
  foreign-window-size
  ;; State of this connection.
  (state 'inactive-state)
  ;; Address <his> for the other end of this conn
  (foreign-address 0)
  ;; Index number <his> for the other end of this conn
  (foreign-index-num 0)
  ;; Index number <mine> for this end of the conn. Note, local-address
  ;; is no longer a constant because of multiple subnets per host.
  (local-index-num 0)
  ;; Packets which have been read from the net and are in order.
  (read-pkts ())
  ;; Last packet on above list.
  (read-pkts-last ())
  ;; Packets which have been received but are not in order.
  (received-pkts ())
  ;; The <his> highest packet number given to user.
  (pkt-num-read -1)
  ;; The <his> highest packet number in ordered list (read-pkts).
  (pkt-num-received -1)
  ;; The level of acknowledgement we have sent out to date.
  (pkt-num-acked -1)
  ;; Time of last input from net.
  (time-last-received)
  ;; List of packets which we must send.
  (send-pkts ())
  ;; Llast pkt <on above>
  (send-pkts-last ())
  ;; length of send-pkts chain
  (send-pkts-length 0)
  ;; Highest <our> packet number assigned.
  (pkt-num-sent 0)
  ;; The last packet number for which we received acknowledgement
  (send-pkt-acked 0)
  ;; Space in window not occupied by unacknowledged packets
  (window-available 0)
  ;;Ffunction to be called in when a new packet arrives at the head of
  ;; read-pkts
  (interrupt-function ())
  (conn-plist ())
  ;; Address that local host is using for this CONN.
  ;; Properties include RFC-CONTACT-NAME and LISTEN-CONTACT-NAME,
  ;; which record the contact names used in the two directions.
  (local-address 0))

(defun conn-state (conn)
  (state conn))

(defun conn-local-window-size (conn)
  (local-window-size conn))

(defun conn-foreign-window-size (conn)
  (foreign-window-size conn))

(defun conn-foreign-address (conn)
  (foreign-address conn))

(defun conn-foreign-index-num (conn)
  (foreign-index-num conn))

(defun conn-local-index-num (conn)
  (local-index-num conn))

(defun conn-read-pkts (conn)
  (read-pkts conn))

(defun conn-read-pkts-last (conn)
  (read-pkts-last conn))

(defun conn-received-pkts (conn)
  (received-pkts conn))

(defun conn-pkt-num-read (conn)
  (pkt-num-read conn))

(defun conn-pkt-num-received (conn)
  (pkt-num-received conn))

(defun conn-pkt-num-acked (conn)
  (pkt-num-acked conn))

(defun conn-time-last-received (conn)
  (time-last-received conn))

(defun conn-send-pkts (conn)
  (send-pkts conn))

(defun conn-send-pkts-last (conn)
  (send-pkts-last conn))

(defun conn-send-pkts-length (conn)
  (send-pkts-length conn))

(defun conn-pkt-num-sent (conn)
  (pkt-num-sent conn))

(defun conn-send-pkt-acked (conn)
  (send-pkt-acked conn))

(defun conn-window-available (conn)
  (window-available conn))

(defun conn-interrupt-function (conn)
  (interrupt-function conn))

(defun conn-local-address (conn)
  (local-address conn))    ;ti

(defparameter *default-window-size*  13
  "This is the default size of the window for a conn.")

(defparameter *maximum-window-size* 128
  "This is the maximum size of the window for a conn.")

;; States in which a connection may be:

;; inactive-state
;;   Indicates the conn is not currently associated with any chaos channel.

;; listening-state
;;   Given to a conn on which a lsn has been sent while it is awaiting
;;   the rfc.
;; rfc-received-state
;;   This state indicates that an rfc has been received on this conn
;;   but that no response has yet been given (such as done by accept
;;   and reject).
;; rfc-sent-state
;;   Indicates that there has been an rfc sent on this conn but that
;;   no response has yet been received from the foreign host.
;; open-state
;;   This state is the normal state for an open connection
;; answered-state
;;   Indicates the conn has received an ans from the other end of the
;;   channel; it is waiting to be read. no i/o is allowed except
;;   reading the ans packet already on the read-pkts chain.
;; cls-received-state
;;   Indicates the conn has received a cls from the other end of the
;;   channel; any data packets that were received in order before the
;;   cls are waiting to be read, followed by the cls packet. No i/o
;;   is allowed except reading the packets already on th read-pkts
;;   chain.
;; los-received-state
;;   Indicates the conn has received a los from the other end of the
;;   channel; the los packet will be the data packet waiting to be
;;   read; any read-pkts or send-pkts are discarded no i/o is allowed
;;   except reading the packets already on th read-pkts chain.
;; host-down-state
;;   State entered when it is determined that the foreign host is down
;;   (or something). This is done in probe-conn.  no i/o is allowed
;;   except reading the packets already on th read-pkts chain.
;; foreign-state
;;   Allows unc packets to go in and out, for implementing non-chaosnet protocols
;; broadcast-sent-state
;;   We have transmitted a broadcast packet. The following actions are possible:
;;	1. The user can send more brd packets.
;;	2. The user can read all packets except opns (until buffering
;;	   is exceeded)
;;      3. opns, when there are no queued input packets, will cause the
;;	   connection to enter the open state.  any other packets
;;	   received from other hosts after this open state is reached
;;	   will evoke a los packet.
;; This array holds the conn for the given index number.  It is big
;; enough that no uniquizing is needed, since it is used in circular
;; fashion.

(defparameter maximum-index 128
  "Length of index-conn; number of distinct connection-indices.")

(defvar index-conn (make-array maximum-index)
  "Array holding the connection for each index number.
    No two connections at any one time can have the same index number.
    Index numbers go in packets to identify which connection they are
    for, the TCP/IP analogy is ports.")

(defparameter maximum-index-log-2-minus-1 (1- (haulong maximum-index))
  "number of bits in a connection index.")

(defvar index-conn-free-pointer 1
  "Next connection index to consider using for a new connection.")

;; This array holds the uniquizer for the current (or last) connection
;; for a given index
(defvar *uniquizer-table*
  (make-array maximum-index :element-type '(unsigned-byte 16))
  "For each connection index, holds last uniquizer value.
   The uniquizer is incremented each time a new connection is made for
   the given index, and it is used together with the index in
   identifying the connection a packet is intended for.")

;;; conn utilities / connection list
(defvar *conn-list*      '() "list of existing conns.")
(defvar *free-conn-list* '() "list of free conn structures (save consing).")

(defvar *prototype-conn* :unbound
  "a conn object used for initializing other conns when they are made.
    the prototype is simply copied into the new conn.")

;; assq list of special port numbers and conns. This is because of eftp
(defvar *distinguished-port-conn-table* '())

(defun contact-name (conn)
  "Return the contact name with which connection conn was created, or
   nil if none.  can be nil if the connection is in a weird state."
  (or (getf (conn-plist conn) 'rfc-contact-name)
      (getf (conn-plist conn) 'listen-contact-name)))

;;; Packets -- the following structure is a packet, abbreviated
;;; pkt. The elements of the array are the actual bits of the packet,
;;; whereas the elements of the leader are internal information not
;;; transmitted.  Recompile it if they change!

(defparameter *max-words-per-pkt* 252
  "Largest packet possible in 16-bit words, including header.")

(defparameter *max-data-words-per-pkt* 244
  "Number of 16-bit data words in largest packet.")

(defparameter *max-data-bytes-per-pkt* 488
  "Number of 8-bit data bytes in largest packet.")

(defparameter *first-data-word-in-pkt* 8
  "Offset to first data word in packet (or, number of words in header).
    note that the contents of the packet, as an array, includes its
    header.")

(defvar *pkt-leader-size* :unbound
  "Number of elements in array leader of a pkt.")

(defstruct (pkt-leader
	     (:constructor nil)
	     (:conc-name nil)
	     (:predicate nil)
	     (:copier nil)
	     ;; Probably, we'll need to make this a property of the
	     ;; array, rather than the leader, which probably will
	     ;; also require us to extend the way the array is copied.
	     ;; (:type (:array-leader
	     ;; 	     (:initial-element 0)))
	     )        ; 4/86 mmg
  ;; not used
  pkt-active-length
  ;; Note pkt not pkt-leader; 2 defstructs for 1 object!
  (pkt-named-structure-symbol pkt)
  ;; time this pkt last transmitted
  pkt-time-transmitted
  ;; number of times this pkt has been transmitted
  pkt-times-transmitted
  ;; A string which is the bytes of the pkt
  pkt-string
  ;; Links pkts in the chain that describes them
  pkt-link
  ;; Links all packets ever made, for all three -links nil = last in
  ;; chain, t = not on chain at all. pkt-link is t only if the pkt
  ;; was freed but was on transmit list and so is kept temporarily.
  pkt-made-link
  ;; t if the packet is being retransmitted and so cannot be really
  ;; freed. If this is the case, it is bashed to be free so that the
  ;; retransmitter will know to free it up.
  pkt-being-retransmitted
  ;; status of the packet, used by the ncp to remember a small amount
  ;; of info about the packet:
  ;;   - nil:
  ;;     normal packet, in use by the ncp
  ;;   - 'released:
  ;;     packet has been given to the user
  pkt-status)


;; For a description of the fields in a pkt see the documentation on
;; the CHAOS Net
;; TODO -- Find out what the AIM is for tis and include it in the project.
(defstruct (pkt
	     (:constructor nil)
	     (:conc-name nil)
	     (:predicate nil)
	     (:copier nil)
	     (:type vector))
  (pkt-opcode-left-justified nil)
  (pkt-opcode 520)
  (pkt-nbytes 12)
  (pkt-fwd-count 772)
  (pkt-dest-address nil)
  (pkt-dest-host-num 8)
  (pkt-dest-subnet 520)
  pkt-dest-index-num
  (pkt-source-address nil)
  (pkt-source-host-num 8)
  (pkt-source-subnet 520)
  pkt-source-index-num
  pkt-num
  pkt-ack-num
  pkt-first-data-word
  pkt-second-data-word)

(defmacro pkt-nwords (pkt)
  `(+ first-data-word-in-pkt (lsh (1+ (pkt-nbytes ,pkt)) -1)))

(defmacro pkt-dest-conn (pkt)
  `(aref index-conn
	 (ldb maximum-index-log-2-minus-1
	      (pkt-dest-index-num ,pkt))))

(defmacro pkt-source-conn (pkt)
  `(aref index-conn
	 (ldb maximum-index-log-2-minus-1
	      (pkt-source-index-num ,pkt))))

;;; pkt utilities -- Packet lists and other pointers

(defvar free-pkts '()
  "Chain of free packets, linked through the pkt-link field.")

(defvar made-pkts '()
  "Chain of all packets constructed, linked thru the pkt-made-link field.")

(defmethod named-structure-invoke ((type (eql :pkt)) pkt op &optional x &rest args)  ; 5/86 mmg
  (let ((stream (or (car args) *standard-output*)))
    (case op
      (:which-operations '(:print-self :describe))
      (:describe (describe-defstruct x 'pkt)
		 (describe-defstruct x 'pkt-leader))
      (:print-self (printing-random-object (x stream)
					   (format stream "chaos packet. pkt-string ~s pkt-status ~s"
						   (pkt-string x) (pkt-status x)))))))

;;; The following macros are for accessing the elements of recent-headers

(defmacro rcnt-opcode (index)
  `(ldb 520 (aref recent-headers ,index 0)))

(defmacro rcnt-nbytes (index)
  `(ldb 12 (aref recent-headers ,index 1)))

(defmacro rcnt-fwd-count (index)
  `(ldb 772 (aref recent-headers ,index 1)))

(defmacro rcnt-dest-address (index)
  `(aref recent-headers ,index 2))

(defmacro rcnt-dest-index (index)
  `(aref recent-headers ,index 3))

(defmacro rcnt-source-address (index)
  `(aref recent-headers ,index 4))

(defmacro rcnt-source-index (index)
  `(aref recent-headers ,index 5))

(defmacro rcnt-pkt-num (index)
  `(aref recent-headers ,index 6))

(defmacro rcnt-ack-num (index)
  `(aref recent-headers ,index 7))

(defmacro rcnt-time-recorded (index)
  `(aref recent-headers ,index 8))

;;; Hacking chaos int-pkt's
;;;
;;;  7/20/84 RAF on LAM5 - Cloned from chaos-unibus-mit
;;;  7/19/84 RAF on Cadr6 - added enable/disable for chaos receiver process.

;;;; Interrupt (microcode) related specials

;; Freelist used by microcode
(defvar int-free-list-pointer)

;; Packets received at interrrupt level
(defvar *int-receive-list-pointer*)

;; Packets to be transmitted at interrupt level
(defvar *int-transmit-list-pointer*)
(defvar *chaos-buffer-area* :unbound
  "Area in which chaosnet INT-PKT's reside.")

;; If non-NIL, the INT-PKT to use.  This permits each
(defvar reserved-int-pkt nil)

;;; Receiver to reserve a packet and avoid the possibility of
;;; blocking. Definitions for interrupt hacking to access the database

(defmacro int-free-list ()
  '(%p-contents-offset int-free-list-pointer 0))

(defmacro int-receive-list ()
  '(%p-contents-offset int-receive-list-pointer 0))

(defmacro int-transmit-list ()
  '(%p-contents-offset int-transmit-list-pointer 0))

;;; The array leader offsets are defined in the system package

(defmacro int-pkt-word-count (int-pkt)
  `(array-leader ,int-pkt %chaos-leader-word-count))

(defmacro int-pkt-thread (int-pkt)
  `(array-leader ,int-pkt %chaos-leader-thread))

(defmacro int-pkt-csr-1 (int-pkt)
  `(array-leader ,int-pkt %chaos-leader-csr-1))

(defmacro int-pkt-csr-2 (int-pkt)
  `(array-leader ,int-pkt %chaos-leader-csr-2))

(defmacro int-pkt-bit-count (int-pkt)
  `(array-leader ,int-pkt %chaos-leader-bit-count))

;; The pkt defstruct applies to int-pkt's as well. Also, at the end of
;; an INT-PKT are the source address, destination address, and crc

(defmacro int-pkt-crc (int-pkt)
  `(aref ,int-pkt (1- (int-pkt-word-count ,int-pkt))))

(defmacro int-pkt-hardware-source (int-pkt)
  `(aref ,int-pkt (- (int-pkt-word-count ,int-pkt) 2)))

(defmacro int-pkt-hardware-dest (int-pkt)
  `(aref ,int-pkt (- (int-pkt-word-count ,int-pkt) 3)))

;;; These are routines to print out the preceding structures in a readable form

;;; *BJ* For *RWF*
(defun print-pkt (pkt &optional (short-display nil) (stream *standard-output*) (show-string nil))
  (terpri stream)
  (and short-display (format stream "   "))
  (format stream
	  "number:  ~16,4,'0r (~16,8,'0r)  opcode: ~16,4,'0r (~a).  number of bytes = ~16,4,'0r ."
	  (pkt-num pkt) (%pointer pkt) (pkt-opcode pkt)
	  (cond
	    ((< (pkt-opcode pkt) (length opcode-list)) (nth (pkt-opcode pkt) opcode-list))
	    ((>= (pkt-opcode pkt) dat-op) 'dat)
	    (t (format () "==> ~16,4,'0r <==" (pkt-opcode pkt))))
	  (pkt-nbytes pkt))
  (cond
    ((not short-display)
     (format stream "~%from ~16,4,'0r-~16,4,'0r to ~16,4,'0r-~16,4,'0r .~%"
	     (pkt-source-address pkt) (pkt-source-index-num pkt) (pkt-dest-address pkt)
	     (pkt-dest-index-num pkt))
     (when show-string
       (format stream "contents:~s~%   " (pkt-string pkt)))
     (let ((words (min 200 (max 10 (pkt-nwords pkt)))))
       (do ((i 0 (1+ i)))
	   ((>= i words))
	 (format stream "~16,4,'0r~:[,~;~%~]" (aref pkt i)
		 (or (= 0 (mod (1+ i) 10)) (= words (1+ i))))))
     (format stream "pkt number = ~16,4,'0r, ack number = ~16,4,'0r, forwarded ~d. times.~%"
	     (pkt-num pkt) (pkt-ack-num pkt) (pkt-fwd-count pkt))
     (format stream "retransmitted ~d. times, last at ~d,  link = ~16,8,'0r~%"
	     (pkt-times-transmitted pkt) (pkt-time-transmitted pkt) (%pointer (pkt-link pkt)))))
  ())

					;(DEFUN PRINT-PKT (PKT &OPTIONAL (SHORT-DISPLAY NIL))
					;  "Print out a packet, if SHORT-DISPLAY is T only 1 line is printed."
					;  (TERPRI)
					;  (AND SHORT-DISPLAY (FORMAT T "   "))
					;  (FORMAT T "Number: ~O (~O)  Opcode: ~O (~A).  Number of bytes = ~O ."
					;	  (PKT-NUM PKT)
					;	  (%POINTER PKT)
					;	  (PKT-OPCODE PKT)
					;	  (COND
					;	    ((< (PKT-OPCODE PKT) (LENGTH OPCODE-LIST))
					;	     (NTH (PKT-OPCODE PKT) OPCODE-LIST))
					;	    ((>= (PKT-OPCODE PKT) DAT-OP) 'DAT)
					;	    (T (FORMAT () "==> ~O <==" (PKT-OPCODE PKT))))
					;	  (PKT-NBYTES PKT))
					;  (COND
					;    ((NOT SHORT-DISPLAY)
					;     (FORMAT T "~%From ~O-~O to ~O-~O .~%"
					;	     (PKT-SOURCE-ADDRESS PKT)
					;	     (PKT-SOURCE-INDEX-NUM PKT)
					;	     (PKT-DEST-ADDRESS PKT)
					;	     (PKT-DEST-INDEX-NUM PKT))
					;     (FORMAT T "Contents:~S~%   "
					;	     (PKT-STRING PKT))
					;     (LET ((MIN-WORDS (MIN 8 (PKT-NWORDS PKT))))
					;       (DO ((I 0 (1+ I)))
					;	   ((>= I MIN-WORDS))
					;	 (FORMAT T "~6,48O~:[,~;~%~]"
					;		 (AREF PKT I) (= (1+ I) MIN-WORDS))))
					;     (FORMAT T "Pkt number = ~O, Ack number = ~O, Forwarded ~O times.~%"
					;	     (PKT-NUM PKT)
					;	     (PKT-ACK-NUM PKT)
					;	     (PKT-FWD-COUNT PKT))
					;     (FORMAT T "Retransmitted ~O times, last at ~S.~%Link = ~S~%"
					;	     (PKT-TIMES-TRANSMITTED PKT)
					;	     (PKT-TIME-TRANSMITTED PKT)
					;	     (PKT-LINK PKT))))
					;  ())


(defun print-conn (conn &optional (short-pkt-display t))
  (let ((last '()))
    (format t "~%chn: ~o (~o): contact: ~s state: ~s from: ~o-~o to ~o-~o .~%"
	    (local-index-num conn)
	    (%pointer conn)
	    (or (getf (conn-plist conn) 'rfc-contact-name)
		(getf (conn-plist conn) 'listen-contact-name))
	    (state conn)
	    (conn-local-address conn)
	    (local-index-num conn)
	    (foreign-address conn)
	    (foreign-index-num conn))
    (format
     t
     (concatenate 'string
		  " rcvd #~o, read #~o, acked #~o; sent #~o, "
		  "acked #~o.  windows: ~o, ~o (~o available).~%")
     (pkt-num-received conn)
     (pkt-num-read conn)
     (pkt-num-acked conn)
     (pkt-num-sent conn)
     (send-pkt-acked conn)
     (local-window-size conn)
     (foreign-window-size conn)
     (window-available conn))
    (cond
      ((send-pkts conn) (format t " send pkts:")
       (do ((pkt (send-pkts conn) (pkt-link pkt))
	    (last () pkt)
	    (len 0 (1+ len)))
	   ((null pkt)
	    (or (eq last (send-pkts-last conn))
		(format t "==> send-pkts-last is destroyed! <==~%"))
	    (or (= len (send-pkts-length conn))
		(format t "==> send-pkts-length is destroyed! <==~%")))
	 (cond
	   ((not (eq conn (pkt-source-conn pkt)))
	    (format
	     t
	     "~following pkt has bad pkt-source-conn pkt = ~s"
	     (pkt-source-conn pkt))))
	 (print-pkt pkt short-pkt-display))))
    (setq last ())
    (cond
      ((read-pkts conn) (format t " read pkts:")
       (do ((pkt (read-pkts conn) (pkt-link pkt))
	    (last () pkt)
	    (len 0 (1+ len)))
	   ((null pkt)
	    (or (eq last (read-pkts-last conn))
		(format t "==> read-pkts-last is destroyed! <==~%")))
	 (print-pkt pkt short-pkt-display))))
    (cond
      ((received-pkts conn) (format t " received pkts:")
       (do ((pkt (received-pkts conn) (pkt-link pkt)))
	   ((null pkt)
	    nil)
	 (setq last pkt)
	 (print-pkt pkt short-pkt-display))))))
