;;; -*- Mode:COMMON-LISP; Base:10; Fonts:(MEDFNT MEDFNB TR12BI); Package:IP; -*-

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

;1;  Internet Control Message Protocol (ICMP)  See DARPA RFC792*
;1;---------------------------------------------------------*
;1; Advertised Functions and processes*
;1;*
;1; ICMP-ERROR - User called function to send an ICMP message that*
;1;              is not able to be caught at the IP level.*
;1;*
;1; ICMP - Fuction called when the error occurs to send an ICMP packet.*
;1;*
;1; ICMP-background - Background process that handles all messages.*
;1;                   This also updates the IP-routing-table when needed,*
;1;                   and passes on messages to the appropriate protocol*
;1;                   handler when neccessary.*
;1;*
;1; ICMP-ECHO - Function send echo packets.*
;1;*
;1; RESET-ICMP-SERVICE - Resets the ICMP process and initializes handler.*
;1;*
;1; ICMP-DISABLE - Disables the ICMP-process and handler.*
;1;*
;1; ICMP-ENABLE - Starts up the ICMP-process makes handler instance if needed.*
;1;*
;1;========================================*

;1;; Global definitions for ICMP.*


(DEFCONSTANT icmp-protocol 1) 

(DEFPARAMETER *icmp-echo-timeout* 10
   "210 Second timeout.*"
   ) 
;1;; Message type constants.*

;1; Destination Unreachable*

(DEFCONSTANT destination-unreachable 3) 
;1;Codes for Destination Unreachable message.*

(DEFCONSTANT net-unreachable 0)  ;1;Used by Gateway*

(DEFCONSTANT host-unreachable 1)  ;1;Used by Gateway*

(DEFCONSTANT protocol-unreachable 2)  ;1;Used by Host*

(DEFCONSTANT port-unreachable 3)  ;1;Used by Host*

(DEFCONSTANT fragmentation-needed-df-set 4)  ;1;Used by Gateway*

(DEFCONSTANT source-route-failed 5)  ;1;Used by Gateway*

(DEFCONSTANT destination-unreachable-codes
   (LIST net-unreachable host-unreachable protocol-unreachable port-unreachable
	 fragmentation-needed-df-set source-route-failed)) 

;1; Time Exceeded*

(DEFCONSTANT time-exceeded 11) 
;1;Codes for Time Exceeded message.*

(DEFCONSTANT time-to-live-exceeded 0)  ;1;Used by Gateway*

(DEFCONSTANT fragment-reassembly-time-exceeded 1)  ;1;Used by Host*

;1; Redirect*

(DEFCONSTANT redirect 5) 
;1;Codes for Redirect message.*

(DEFCONSTANT redirect-network 0)  ;1;Used by Gateway*

(DEFCONSTANT redirect-host 1)  ;1;Used by Gateway*

(DEFCONSTANT redirect-tos-net 2)  ;1;Used by Gateway*

(DEFCONSTANT redirect-tos-host 3)  ;1;Used by Gateway*

(DEFCONSTANT redirect-codes
   (LIST redirect-network redirect-host redirect-tos-net redirect-tos-host))       

;1; Other messages that do not use codes.*

(DEFCONSTANT parameter-problem 12) 

(DEFCONSTANT source-quench 4) 

(DEFCONSTANT *icmp-echo* 8) 

(DEFCONSTANT *icmp-echo-reply* 0) 

(DEFCONSTANT *icmp-timestamp* 13) 

(DEFCONSTANT *icmp-timestamp-reply* 14) 

(DEFCONSTANT *icmp-info-req* 15) 

(DEFCONSTANT *icmp-info-reply* 16) 

;1;;*
;1;;=============================================*
;1;;TCP/IP Subnet implementation requires two ICMP types*
;1;;  1.  AM1  Address mask request message 17*
;1;;  2.  AM2  Address mask reply message 18*


(DEFCONSTANT address-mask-request 17) 

(DEFCONSTANT address-mask-reply 18) 

;1;;======================================*
;1;;*
;1;; ICMP header defstruct and macros.*
;1;;*

(DEFSTRUCT (icmp-header (:type (VECTOR (MOD 256))) (:conc-name "RAW-ICMP-") (:constructor nil)
  :size-symbol)
  (type)
  (code)
  (checksum-msb)
  (checksum-lsb)
  (option-1-msb)
  (option-1-lsb)
  (option-2-msb)
  (option-2-lsb)
  (address-mask-1)
  (address-mask-2)
  (address-mask-3)
  (address-mask-4)) 


(DEFF-MACRO icmp-type 'raw-icmp-type) 

(DEFF-MACRO icmp-code 'raw-icmp-code) 


(DEFMACRO icmp-option-1 (packet)
  `(+ (LSH (raw-icmp-option-1-msb ,packet) 8) (raw-icmp-option-1-lsb ,packet))) 


(DEFMACRO set-icmp-option-1 (packet value)
  `(SETF (raw-icmp-option-1-msb ,packet) (LDB 520 ,value)
	 (raw-icmp-option-1-lsb ,packet) (LDB 8 ,value))) 


(DEFSETF
 icmp-option-1 set-icmp-option-1) 


(DEFMACRO icmp-option-2 (packet)
  `(+ (LSH (raw-icmp-option-2-msb ,packet) 8) (raw-icmp-option-2-lsb ,packet))) 


(DEFMACRO set-icmp-option-2 (packet value)
  `(SETF (raw-icmp-option-2-msb ,packet) (LDB 520 ,value)
	 (raw-icmp-option-2-lsb ,packet) (LDB 8 ,value))) 


(DEFSETF icmp-option-2 set-icmp-option-2) 


(DEFMACRO icmp-checksum (packet)
  `(+ (LSH (raw-icmp-checksum-msb ,packet) 8) (raw-icmp-checksum-lsb ,packet))) 


(PROCLAIM '(inline set-icmp-checksum))
(DEFUN set-icmp-checksum (packet value)
  (SETF (raw-icmp-checksum-msb packet) (LDB 520 value)
	(raw-icmp-checksum-lsb packet) (LDB 8 value))) 


(DEFSETF icmp-checksum set-icmp-checksum) 


(DEFMACRO set-icmp-address-mask (packet value)
  `(SETF (raw-icmp-address-mask-1 ,packet) (LDB 1544 ,value)
	 (raw-icmp-address-mask-2 ,packet) (LDB 1032 ,value)
	 (raw-icmp-address-mask-3 ,packet) (LDB 520 ,value)
	 (raw-icmp-address-mask-4 ,packet) (LDB 8 ,value))) 


(DEFMACRO icmp-address-mask (packet)
  `(LET ((value 0))
     (SETF value (DPB (raw-icmp-address-mask-4 ,packet) (BYTE 8 0) value))
     (SETF value (DPB (raw-icmp-address-mask-3 ,packet) (BYTE 8 8) value))
     (SETF value (DPB (raw-icmp-address-mask-2 ,packet) (BYTE 8 16) value))
     (SETF value (DPB (raw-icmp-address-mask-1 ,packet) (BYTE 8 24) value)))) 

;1`(+ (LSH (RAW-ICMP-Address-Mask-1 ,packet) 24.)*
;1    (lsh (RAW-ICMP-Address-Mask-2 ,packet) 16.)*
;1    (lsh (RAW-ICMP-Address-Mask-3 ,packet) 8.)*
;1    (RAW-ICMP-Address-Mask-4 ,packet)))*


(DEFSETF icmp-address-mask set-icmp-address-mask) 

;1;;=========================================*
;1;;*
;1;; ICMP entry list*
;1;;*

(DEFSTRUCT (icmp-entry-list-item (:type list) (:conc-name "ICMP-ENTRY-") (:constructor nil) :size-symbol)
  data
  length
  source
  destination) 


(DEFVAR *icmp-entry-list* () "2The list of incoming ICMP packets to be processed.*"
   ) 

;1;; Maximum ICMP Packet size*

(DEFCONSTANT icmp-max-packet-16-bit (+ 4 4 (/ *max-header-bytes* 2))) 

(DEFCONSTANT icmp-normal-echo-size 255) 

;1;; Peek items*

(DEFVAR icmp-messages-sent 0) 

;1;; Debug receive list *

(DEFVAR *icmp-received-dbg* ()) 

;1;; Echo is on indicator*

(DEFVAR *icmp-echo-on* ()) 

(DEFVAR *icmp-echo-list* () "2The list to put echo replies in if echo is operating.*"
   ) 

;1;;========================================*
;1;;*
;1;; ICMP handler constant*
;1;;*

(DEFVAR *icmp-handler* () "2Instance of the ICMP-HANDLER that is in use.*") 
;1; *

;1;;=======================================*
;1;;*
;1;; ICMP Packet Resources*
;1;;*

(DEFRESOURCE icmp-packet (type code)
  :constructor (MAKE-ARRAY (- *max-ethernet-packet-bytes* *max-header-bytes*) :type :art-8b)
  :initializer
  (PROGN
    (SETF (icmp-type object) type)
    (SETF (icmp-code object) code)
    (SETF (icmp-checksum object) 0)
    (SETF (icmp-option-1 object) 0)
    (SETF (icmp-option-2 object) 0)
    (SETF (icmp-address-mask object) 0)
    (ARRAY-INITIALIZE object 0 4))
  :matcher t
  :free-list-size 3) 


(DEFMACRO allocate-icmp-packet (&optional (type 0) (code 0))
  `(ALLOCATE-RESOURCE 'icmp-packet ,type ,code)) 


(DEFMACRO free-icmp-packet (packet)
  `(DEALLOCATE-RESOURCE 'icmp-packet ,packet)) 

;1;;*
;1;; ICMP packet list entry*
;1;;*


(DEFRESOURCE icmp-packet-list-entry (pkt length source destination)
  :constructor (LIST pkt length source destination)
  :initializer (SETF (icmp-entry-data object) pkt
		     (icmp-entry-length object) length
		     (icmp-entry-source object) source
		     (icmp-entry-destination object) destination)
  :matcher t
  :free-list-size 3) 


(DEFMACRO allocate-icmp-entry (pkt length source dest)
  `(ALLOCATE-RESOURCE 'icmp-packet-list-entry ,pkt ,length ,source ,dest)) 


(PROCLAIM '(inline free-icmp-entry))
(DEFUN free-icmp-entry (entry)
  (DEALLOCATE-RESOURCE 'icmp-packet-list-entry entry)) 

;1;;========================================*
;1;;*
;1;; ICMP Handler *
;1;;*

(DEFFLAVOR icmp-handler
	   (listen-process
	    packets-sent
	    packets-received
	    packets-discarded
	    echo-req-rcvd
	    echo-reply-sent
	    echo-req-sent
	    echo-reply-rcvd
	    redirects-sent
	    redirects-rcvd
	    address-mask-request-sent
	    address-mask-request-rcvd
	    address-mask-reply-sent
	    address-mask-reply-rcvd
	    destination-unreachables-sent
	    du-net-sent
	    du-host-sent
	    du-protocol-sent
	    du-port-sent
	    du-fragmentation-needed-sent
	    du-source-route-failed-sent
	    destination-unreachables-rcvd
	    du-net-rcvd
	    du-host-rcvd
	    du-protocol-rcvd
	    du-port-rcvd
	    du-fragmentation-needed-rcvd
	    du-source-route-failed-rcvd
	    time-requests-rcvd
	    time-requests-replied
	    parameter-problems-sent
	    parameter-problems-rcvd
	    ttl-exceeded-sent
	    ttl-exceeded-rcvd
	    time-to-reassemble-exceeded-sent
	    time-to-reassemble-exceeded-rcvd
	    s-quench-sent
	    s-quench-rcvd)
	   ()
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables
  :outside-accessible-instance-variables)  


(DEFRESOURCE icmp-handlers ()
  :constructor (MAKE-INSTANCE 'icmp-handler)
  :initializer (PROGN
		 (SEND object :set-listen-process ())
		 (SEND object :set-packets-sent 0)
		 (SEND object :set-packets-received 0)
		 (SEND object :set-packets-discarded 0)
		 (SEND object :set-echo-req-rcvd 0)
		 (SEND object :set-echo-reply-sent 0)
		 (SEND object :set-echo-req-sent 0)
		 (SEND object :set-echo-reply-rcvd 0)
		 (SEND object :set-redirects-sent 0)
		 (SEND object :set-redirects-rcvd 0)
		 (SEND object :set-address-mask-request-sent 0)
		 (SEND object :set-address-mask-request-rcvd 0)
		 (SEND object :set-address-mask-reply-sent 0)
		 (SEND object :set-address-mask-reply-rcvd 0)
		 (SEND object :set-destination-unreachables-sent 0)
		 (SEND object :set-du-net-sent 0)
		 (SEND object :set-du-host-sent 0)
		 (SEND object :set-du-protocol-sent 0)
		 (SEND object :set-du-port-sent 0)
		 (SEND object :set-du-fragmentation-needed-sent 0)
		 (SEND object :set-du-source-route-failed-sent 0)
		 (SEND object :set-destination-unreachables-rcvd 0)
		 (SEND object :set-du-net-rcvd 0)
		 (SEND object :set-du-host-rcvd 0)
		 (SEND object :set-du-protocol-rcvd 0)
		 (SEND object :set-du-port-rcvd 0)
		 (SEND object :set-du-fragmentation-needed-rcvd 0)
		 (SEND object :set-du-source-route-failed-rcvd 0)
		 (SEND object :set-time-requests-rcvd 0)
		 (SEND object :set-time-requests-replied 0)
		 (SEND object :set-parameter-problems-sent 0)
		 (SEND object :set-parameter-problems-rcvd 0)
		 (SEND object :set-ttl-exceeded-sent 0)
		 (SEND object :set-ttl-exceeded-rcvd 0)
		 (SEND object :set-time-to-reassemble-exceeded-sent 0)
		 (SEND object :set-time-to-reassemble-exceeded-rcvd 0)
		 (SEND object :set-s-quench-sent 0)
		 (SEND object :set-s-quench-rcvd 0))
  :matcher t
  :free-list-size 1) 

;1;;========================================*
;1;;*
;1;; Allocation of ICMP packets for handler.*

(DEFMETHOD (icmp-handler :alloc-packet) (&optional (type 0) (code 0))
  (allocate-icmp-packet type code)) 


(DEFMETHOD (icmp-handler :free-packet) (packet)
  (free-icmp-packet packet)) 

;1;;========================================*
;1;; :RECEIVE-DATA, for the handler, is passed the IP packet,*
;1;; the source address, and the destination address by IP.  If*
;1;; the checksum is incorrect, it discards the packet.*
;1;; If correct then the packet is placed on a queue to be read *
;1;; by the ICMP Listen routine.*
;1;; NOTE: All handlers are assumed to accept the following args:*
;1;; Data, Length of the data, source address, and destination addr.*
;1;; The handler receiving the data is assumed to be able to handle *
;1;; whatever queues may be neccessary for packets coming in faster than*
;1;; can be handled.*
;1;;*

(DEFMETHOD (icmp-handler :receive-data) (ip-packet start length source destination)
  "2:RECEIVE-DATA method for ICMP.  This sets up the ICMP packet on a queue to be read by the ICMP LISTEN routine.*"
  (DECLARE (SPECIAL *receive-debug-mode*))
  (LET (icmp-packet
	packet-checksum
	checker
	entry)
    (INCF packets-received)
    (SETF icmp-packet (SEND self :alloc-packet))
    (SETF length (MIN length (ARRAY-LENGTH icmp-packet)))
    (copy-pkt ip-packet icmp-packet length start)
    (SETF packet-checksum (icmp-checksum icmp-packet))
    (SETF (icmp-checksum icmp-packet) 0)
    (SETF checker (ip-ones-complement-checksum icmp-packet length))
    (SETF (icmp-checksum icmp-packet) packet-checksum)
    (COND ((NOT (= packet-checksum checker))	   ;1Check sum not equal*
	   (INCF packets-discarded)	   ;1Discard the packet*
	   (WHEN *receive-debug-mode*
	     (FORMAT t "~&ICMP: :receive-data Checksum error Checksum = ~16r, Should be ~16r"
		     packet-checksum checker))
	   (WHEN (EQUAL *receive-debug-mode* :verbose)
	     (LET ((dbg (MAKE-ARRAY length :type :art-8b)))
	       (COPY-ARRAY-PORTION icmp-packet 0 length
				   dbg 0 length)
	       (PUSH dbg *icmp-received-dbg*)))
	   (SEND self :free-packet icmp-packet))
	  (t				   ;1Ok pass it on.*
	   (WHEN *receive-debug-mode*
	     (PROCESS-RUN-FUNCTION "notify" 'tv:notify
				   nil 
				   "ICMP :receive-data Dispatching to ICMP listen.")
	     (PROCESS-ALLOW-SCHEDULE))
	   (SETF entry (allocate-icmp-entry icmp-packet length source destination))
	   ;1; Place on list to be received.*
	   (PUSH entry *icmp-entry-list*))))) 
;1;;*
;1;;========================================*
;1;;*
;1;; ICMP function.*
;1;;*
;1;; Parameters are the IP packet that got the error,*
;1;;   the error message type, the associated code for this*
;1;;   message and optionally an-item the is to be put in the*
;1;;   packet for a special field in the ICMP message.*
;1;;*
;1;;    07/21/86 LER*
;1;; To implement TCP/IP subnetting, two new ICMP types have*
;1;; been added.  Address Mask Request and Address Mask Reply.*
;1;;*
;1;; This function will send ICMP messages for the following*
;1;; errors:*
;1;;    type. error*
;1;;      3 Destination Unreachable                  who sends         *
;1;;        codes: 0 - Net unreachable.                  gate*
;1;;               1 - host unreachable.                 gate*
;1;;               2 - Protocol unreachable.             host*
;1;;               3 - port unreachable.                 host*
;1;;               4 - fragmentation needed, df set.     gate*
;1;;               5 - source route failed.              gate*
;1;;     11 Time exceeded                            who sends*
;1;;        codes: 0 - time to live exceeded             gate*
;1;;               1 - fragment reassembly time exceeded. host.*
;1;;     12 Parameter problem*
;1;;        codes: 0 - pointer indicates octet in trouble.*
;1;;               all else not indication of trouble.*
;1;;               this error may be sent by either a gate or host.*
;1;;        an-item: the octet where the parameter is.*
;1;;      4 Source Quench*
;1;;        code:  0          can be sent by either a gate or host.*
;1;;      5 Redirect*
;1;;        code:  0 - Redirect for the network          gate.*
;1;;               1 - Redirect for the Host.            gate.*
;1;;               2 - Redirect for the TOS and network  gate.*
;1;;               3 - Redirect for the TOS and host.    gate.*
;1;;        an-item: The internet address of the host to redirect to.*
;1;;    *
;1;;    17  Address Mask Request            sent by isolated host*
;1;;        code: 0*
;1;;    18  Address Mask Reply              sent only by gateway*
;1;;        code: 0*
;1;;*

(DEFUN icmp (ip-pkt type code &optional (an-item nil))
  "2This function is called when an error occurs in IP and sends an ICMP packet to the 
    remote host.  This does not handle Echo, Timestamp, or Information-request.*"
  (DECLARE (SPECIAL *IP-HANDLER*))
  (LET ((icmp-pkt (allocate-icmp-packet type code))
	(data-length (+ 8 (* 4 (ip-header-length ip-pkt))))
	(route-entry nil) kludge)
    ;1;Check for errors and special items to setup.*
    (SELECT type
      (address-mask-reply
       (INCF (SEND *icmp-handler* :address-mask-reply-sent))
       ;1; Take mask from routing table and insert in*
       ;1; ICMP packet to be sent to isolated hosts*
       (WHEN *gateway-host-p*
	 (SETF route-entry (get-routing-entry (ip-src-addr ip-pkt)))
	 (SETF (icmp-address-mask icmp-pkt) (ip-routing-mask route-entry))))
      (address-mask-request
       (INCF (SEND *icmp-handler* :address-mask-request-sent)))
      ;1; When isolate host is booted, only a simple*
      ;1; ICMP mask request message is needed*
      (destination-unreachable
       (CASE code
	 (0 (SEND *icmp-handler* :set-du-net-sent (1+ (SEND *icmp-handler* :du-net-sent))))
	 (1 (SEND *icmp-handler* :set-du-host-sent (1+ (SEND *icmp-handler* :du-host-sent))))
	 (2
	  (SEND *icmp-handler* :set-du-protocol-sent
		(1+ (SEND *icmp-handler* :du-protocol-sent))))
	 (3 (SEND *icmp-handler* :set-du-port-sent (1+ (SEND *icmp-handler* :du-port-sent))))
	 (4
	  (SEND *icmp-handler* :set-du-fragmentation-needed-sent
		(1+ (SEND *icmp-handler* :du-fragmentation-needed-sent))))
	 (5
	  (SEND *icmp-handler* :set-du-source-route-failed-sent
		
		(1+ (SEND *icmp-handler* :du-source-route-failed-sent))))
	 (otherwise
	  (FERROR :icmp-destination-unreachable
		  "ICMP: Invalid code of ~A for a DESTINATION UNREACHABLE message."
		  code)))
       (SEND *icmp-handler* :set-destination-unreachables-sent
	     (1+ (SEND *icmp-handler* :destination-unreachables-sent))))
      (time-exceeded
       (CASE code
	 (0
	  (SEND *icmp-handler* :set-ttl-exceeded-sent
		(1+ (SEND *icmp-handler* :ttl-exceeded-sent))))
	 (1
	  (SEND *icmp-handler* :set-time-to-reassemble-exceeded-sent
		(1+ (SEND *icmp-handler* :time-to-reassemble-exceeded-sent))))
	 (otherwise
	  (FERROR :icmp-time-exceeded
		  "ICMP: Invalid code of ~A for a TIME EXCEEDED message."
		  code))))
      (redirect
       (UNLESS (MEMBER code '(0 1 2 3))
	 (FERROR :icmp-redirect
		 "ICMP: Invalid code of ~A for a REDIRECT message, must be zero."
		 code))
       (UNLESS (NUMBERP an-item)
	 (FERROR :icmp-redirect "ICMP: Invalid ip-address of ~A for a redirect message."
		 an-item))
       (SEND *icmp-handler* :set-redirects-sent (1+ (SEND *icmp-handler* :redirects-sent)))
       ;1; Setup the ip address in the packet.*
       (SETF (icmp-option-1 icmp-pkt) (LDB (BYTE 16 16) an-item))
       ;1;High order bits.*
       (SETF (icmp-option-2 icmp-pkt) (LDB (BYTE 16 0) an-item))
       ;1;Low order bits.*
       )
      (parameter-problem
       (UNLESS (ZEROP code)
	 (FERROR :icmp-parameter-problem
		 "ICMP: Invalid code of ~A for a parameter problem, must be zero."
		 ))
       (UNLESS (NUMBERP an-item)
	 ;1;An-item must be an index to the octet.*
	 (FERROR :icmp-parameter-problem
		 "ICMP: Invalid code of ~A for a PARAMETER PROBLEM message, number expected"
		 ))
       ;1; put the pointer in the packet.*
       (SEND *icmp-handler* :set-parameter-problems-sent
	     (1+ (SEND *icmp-handler* :parameter-problems-sent)))
       (SETF (icmp-option-1 icmp-pkt) an-item))
      (source-quench
       (UNLESS (ZEROP code)
	 (FERROR :icmp-redirect
		 "ICMP: Invalid code of ~A for a SOURCE QUENCH message, must be zero."
		 code))
       (SEND *icmp-handler* :set-s-quench-sent (1+ (SEND *icmp-handler* :s-quench-sent))))
      (:otherwise
       (FERROR :icmp-invalid-msg
	       "This call to send an ICMP message has an invalid type of ~A and code of ~A"
	       type code)))
    (BLOCK transmit-packet
      (SELECT type
	((address-mask-request address-mask-reply))
	(:otherwise
	 ;1; don't broadcast these types*
	 (WHEN (ip:local-broadcast-address-p (ip-src-addr ip-pkt))
	   (RETURN-FROM transmit-packet))
	 ;1;Copy in the header and 64bits from the ip-pkt.*
	 (SETF kludge (make-ip-data ip-pkt data-length 0))
	 (COPY-ARRAY-PORTION kludge 0 data-length icmp-pkt 8 (+ data-length 8))
	 (free-ip-data kludge)))
      ;1;Send the message.*
      (SETF (icmp-checksum icmp-pkt) (ip-ones-complement-checksum icmp-pkt (+ 8 data-length)))
      (SEND *ip-handler* :transmit icmp-pkt ;1; Data*
	    icmp-protocol ;1; Protocol*
	    (ip-src-addr ip-pkt) ;1; Destination*
	    (+ 8 data-length)) ;1; Length*
      (INCF icmp-messages-sent))
    (free-icmp-packet icmp-pkt)
    ())) 
;1;;======================================*
;1;;*
;1;; ICMP wait*
;1;;*

(DEFSUBST icmp-wait ()
  (DECLARE (SPECIAL *icmp-entry-list*))
  (NOT (NULL *icmp-entry-list*))) 

;1;;======================================*
;1;;*
;1;; ICMP Background function.*
;1;;*
;1;; This is the ICMP listen function.*
;1;; It is the fuction that receives incoming*
;1;; ICMP packets and responds accordingly.*
;1;;*
;1;; 1. Reports the appropriate message for the *
;1;;    following ICMP messages.*
;1;;    A. Destination Unreachable*
;1;;    B. Source Quench.*
;1;;    C. Time Exceeded.*
;1;;    D. Parameter Problem.*
;1;; 2. Updates the routing information for the*
;1;;    redirect message.*
;1;; 3. Responds by sending appropriate ICMP*
;1;;    messages to the appropriate incoming*
;1;;    ICMP messages.*
;1;;    A. Echo*
;1;;    B. Timestamp*
;1;;    C. Information request.*
;1;;*
;1;;  07/21/86 LER*
;1;;  To implement TCP/IP subnetting, two additional*
;1;;  ICMP type are required.  Address Mask Request*
;1;;  and Address Mask Reply*
;1;;*
;1;;*

(DEFVAR *icmp-transmit-list* ()) 

(DEFVAR *icmp-debug-mode* ()) 


(DEFMETHOD (icmp-handler :listen) ()
  "2Listen method for the ICMP-handler.*"
  (DECLARE (SPECIAL *icmp-entry-list* *IP-HANDLER*))
  (LET (entry
	data
	len
	src
	dest
	type
	code
	timer
	route-entry)
    (ERROR-RESTART-LOOP ((ERROR system:abort) "Wait for another ICMP packet."
			 )
      (LOOP (PROCESS-WAIT "ICMP Listen"
			  'icmp-wait)
	    (SETF entry (POP *icmp-entry-list*))
	    (WHEN *receive-debug-mode*
	      (PROCESS-RUN-FUNCTION "notify" 'tv:notify () "ICMP packet received."
				    )
	      (PROCESS-ALLOW-SCHEDULE))
	    (UNWIND-PROTECT (UNLESS
			      (NULL entry)
			      (SETF data (icmp-entry-data entry)
				    len (icmp-entry-length entry)
				    src (icmp-entry-source entry)
				    dest (icmp-entry-destination entry))
			      (SETF type (icmp-type data)
				    code (icmp-code data))
			      (SELECT type
				(address-mask-request
				 (INCF address-mask-request-rcvd)
				 ;1; get mask from routing table*
				 (COND
				   (*gateway-host-p*
				    (CONDITION-CASE ()
					(SETF route-entry (get-routing-entry src))
				      (incomplete-routing-table (SETF route-entry nil)))
				    (WHEN route-entry
				      (SETF (icmp-address-mask data) (ip-routing-mask route-entry))
				      (SETF (icmp-type data) address-mask-reply)
				      (SETF (icmp-code data) 0)
				      (SETF (icmp-checksum data) 0)
				      (SETF (icmp-checksum data) (ip-ones-complement-checksum data len))
				      (SEND *ip-handler* :transmit data icmp-protocol src len)))
					   ;1source address is not found in routing table - drop it*
				   (t)))
				(address-mask-reply
				 (INCF address-mask-reply-rcvd)
				 ;1; insure this turkey is not lying to us*
				 (CONDITION-CASE ()
				     (SETF route-entry (get-routing-entry src))
				   (incomplete-routing-table (SETF route-entry nil)))
				 (WHEN (AND route-entry (EQ :direct (ip-routing-address route-entry)))
				   (LET ((mask-changed-p nil))
				     ;1; update *gateway-addr-mask-list* with the correct mask*
				     (WITHOUT-INTERRUPTS
				       (DOLIST (gam *gateway-addr-mask-list*)
					 (WHEN (AND (EQL (get-network-number src)
							 (get-network-number (SECOND gam)))
						    (NOT (EQL (THIRD gam)
							      (icmp-address-mask data)))
						    (<= #xFF000000 (icmp-address-mask data)))
					   (SETF mask-changed-p t)
					   (SETF (THIRD gam) (icmp-address-mask data)))))
				     ;1; rebuild the routing table*
				     (WHEN mask-changed-p
				       (reset-ip-routing-table t *gateway-addr-mask-list*)))))
				;1; Redirect message.*
				(redirect (INCF redirects-rcvd)
					  (IF (MEMBER code redirect-codes)
					      (CONDITION-CASE ()
						  (process-redirect data)
						(sys:network-error))))
				(destination-unreachable
				 (INCF destination-unreachables-rcvd)
				 (SELECT code
				   (net-unreachable (INCF du-net-rcvd))
				   ;1;process-net-unreachable will be utilized once*
				   ;1;gateway-to-gatway protocol has been*
				   ;1;implemented*
					   ;1(PROCESS-NET-UNREACHABLE DATA))*
				   (host-unreachable (INCF du-host-rcvd))
				   (protocol-unreachable (INCF du-protocol-rcvd))
				   (port-unreachable (INCF du-port-rcvd))
				   (fragmentation-needed-df-set
				    (INCF du-fragmentation-needed-rcvd))
				   (source-route-failed
				    (INCF du-source-route-failed-rcvd))
				   (otherwise
				    ;1;Code is bad but ignore for time being.*
				    nil)))
				;1; Respond to an echo request*
				(*icmp-echo*
				 (COND
				   ((ZEROP code)
				    (INCF echo-req-rcvd)
				    (SETF (icmp-type data) *icmp-echo-reply*)
				    (SETF (icmp-checksum data) 0)
				    (SETF (icmp-checksum data) (ip-ones-complement-checksum data len))
				    (WHEN (NOT (local-broadcast-address-p src))
				      (SEND *ip-handler* :transmit data icmp-protocol src len)
				      (INCF echo-reply-sent)
				      (INCF packets-sent)))
				   (t (INCF packets-discarded))))
				;1; Put on echo list if echo procedure is on else drop packet.*
				(*icmp-echo-reply*
				 (INCF echo-reply-rcvd)
				 (IF *icmp-echo-on*
				     (LET* ((ED (allocate-icmp-packet 0 0))
					    (echo (allocate-icmp-entry ed len src dest)))
				       (COPY-ARRAY-PORTION data 0 len ed 0 len)
				       (PUSH echo *icmp-echo-list*))))
				;1; Timestamp request received.*
				(*icmp-timestamp* (INCF time-requests-rcvd)
						  (SETF timer (GET-UNIVERSAL-TIME))
						  ;1;Put the time in the Received slot.*
						  (SETF (AREF data 12) (LDB (BYTE 8 24) timer)
							(AREF data 13) (LDB (BYTE 8 16) timer)
							(AREF data 14) (LDB (BYTE 8 8) timer)
							(AREF data 15) (LDB (BYTE 8 0) timer))
						  (SETF (icmp-type data) *icmp-timestamp-reply*)
						  (SETF (icmp-checksum data) 0)
						  (SETF (icmp-checksum data)
							(ip-ones-complement-checksum data len))
						  (SETF timer (GET-UNIVERSAL-TIME))
						  ;1;Put the time in the transmit slot.*
						  (SETF (AREF data 16) (LDB (BYTE 8 24) timer)
							(AREF data 17) (LDB (BYTE 8 16) timer)
							(AREF data 18) (LDB (BYTE 8 8) timer)
							(AREF data 19) (LDB (BYTE 8 0) timer))
						  (SEND *ip-handler* :transmit data
							icmp-protocol src (* 2 len)))
				(parameter-problem (INCF parameter-problems-rcvd))
				(time-exceeded
				 (SELECT code (time-to-live-exceeded (INCF ttl-exceeded-rcvd))
					 (fragment-reassembly-time-exceeded
					  (INCF time-to-reassemble-exceeded-rcvd))
					 (otherwise (INCF packets-discarded))))
				(source-quench (INCF s-quench-rcvd))
				(otherwise
				 (UNLESS (NULL IP:*RECEIVE-DEBUG-MODE*)
				   (PROCESS-RUN-FUNCTION "notify" 'tv:notify ()
							 "ICMP packet received.~
                                                   Packet type of ~A is not implemented yet"
							 type)))))
	      (WHEN entry
		(free-icmp-packet data)
		(free-icmp-entry entry))))))) 

;1;;=====================================*
;1;;*
;1;; NET-UNREACHABLE code for Destination Unreachable message.*
;1;;*

(DEFUN process-net-unreachable (data)
  "2Updates the routing table on the receipt of a Destination Unreachable message with
    the network unreachable code.*"
  (ignore data)
  ;1;; to be rewritten once gateway to gateway protocol is implemented*
  (COMMENT (LET* ((destination
		    (+ (ASH (AREF data 24) 24)
		       ;1; Destination in old header*
		       (ASH (AREF data 25) 16) (ASH (AREF data 26) 8) (AREF data 27)))
		  (route-entry (get-routing-entry destination))
		  (network (ip-routing-network route-entry))
		  (my-addr-route-entry (get-routing-entry (FIRST ip:my-addresses))))
	     (WHEN (AND route-entry
			(= network (ip-routing-network my-addr-route-entry)))
	       (SETF ip-routing-table (DELETE route-entry ip-routing-table)))))) 

;1;;=====================================*
;1;;*
;1;; REDIRECT ICMP message.*
;1;;*
;1;; This handles the following codes.*
;1;; 0 - Redirect datagrams for the network.*
;1;; 1 - Redirect datagrams for the host.*
;1;; 2 - Redirect datagrams for the type of service and network.*
;1;; 3 - Redirect datagrams for the type of service and host.*
;1;;*

(DEFUN process-redirect (data)
  "2Processes ICMP Redirect messages.*"
  ;1; If the network is in the IP-ROUTING-TABLE,*
  ;1; the entry is changed with the new IP address of the machine*
  ;1; that is a valid gateway.  Otherwise, a new entry is*
  ;1; appended.*
  (LET* ((new-gateway-addr (+ (ASH (icmp-option-1 data) 16) (icmp-option-2 data)))
	 (gateway-route-entry (get-routing-entry new-gateway-addr))
	 (remote-address (DPB (AREF data 20) (BYTE 8 24)
			      (DPB (AREF data 21) (BYTE 8 16)
				   (DPB (AREF data 22) (BYTE 8 8)
					(AREF data 23)))))
	 (remote-route-entry (get-routing-entry remote-address)))
    (COND (remote-route-entry
	   (UNLESS (EQ (ip-routing-address remote-route-entry) :direct)
	     (SETF (ip-routing-address remote-route-entry) new-gateway-addr)))
	  ;1; insure good new gateway address*
          (gateway-route-entry
	   (PUSH (allocate-ip-route
		   remote-address
		   (ip-routing-controller gateway-route-entry)
		   new-gateway-addr
		   *max-time-to-live*
		   (IF (EQL (get-network-number remote-address)
			    (get-network-number new-gateway-addr))
		       (ip-routing-mask gateway-route-entry)
		       (get-default-mask remote-address)))
		 ip-routing-table))))) 
  
;1;;=====================================*
;1;;*
;1;; ICMP Reset, Disable, and Enable functions.*
;1;;*

(DEFUN reset-icmp-service (&optional enable-p)
  "2Resets the ICMP processesing.*"
  (WHEN (TYPEP *icmp-handler* 'icmp-handler)
    (SEND *icmp-handler* :disable)
    (CLEAR-RESOURCE 'icmp-handlers () ()))
  (COND (enable-p
	 (UNLESS (TYPEP *icmp-handler* 'icmp-handler)
	   (SETF *icmp-handler* (ALLOCATE-RESOURCE 'icmp-handlers)))
	 (SEND *icmp-handler* :enable)
	 (SEND *icmp-handler* :reset-stats))
	(t
	 (SETF *icmp-handler* #'(lambda (operation &rest ignore)
				  (CASE operation
				    (:receive-data nil)
				    (otherwise
				     (FERROR 'tcp-error "ICMP Service is not enabled.")))))))
  "ICMP Reset Complete") 


(DEFUN icmp-listen (icmp)
  "2Function to start ICMP listen method.*"
  (SEND icmp :listen)) 


(DEFMETHOD (icmp-handler :enable) ()
  "2ICMP-handler :enable method. Enables ICMP processing.*"
  (WHEN (NULL listen-process)
    (SETF listen-process
	  (MAKE-PROCESS "ICMP Listener"
			:warm-boot-action () :priority 15 :regular-pdl-size 5000
			:special-pdl-size 5000)))
  (SEND listen-process :preset 'LISTEN self)
  (PROCESS-RESET-AND-ENABLE listen-process)) 


(DEFMETHOD (icmp-handler :disable) ()
  "2ICMP-handler :disable method. Disables ICMP processing.*"
  (WHEN listen-process
    (SEND listen-process :kill t)
    (SETF listen-process nil))
  (SETF *icmp-entry-list* ())
  (CLEAR-RESOURCE 'icmp-packet)
  (CLEAR-RESOURCE 'icmp-packet-list-entry)) 


(DEFMETHOD (icmp-handler :reset-stats) ()
  "2ICMP-handler :reset-stats.  Resets the statistics counters for the handler.*"
  (SETF packets-sent 0
	packets-received 0
	packets-discarded 0
	echo-req-rcvd 0
	echo-reply-sent 0
	echo-req-sent 0
	echo-reply-rcvd 0
	redirects-sent 0
	redirects-rcvd 0
	destination-unreachables-sent 0
	du-net-sent 0
	du-host-sent 0
	du-protocol-sent 0
	du-port-sent 0
	du-fragmentation-needed-sent 0
	du-source-route-failed-sent 0
	destination-unreachables-rcvd 0
	du-net-rcvd 0
	du-host-rcvd 0
	du-protocol-rcvd 0
	du-port-rcvd 0
	du-fragmentation-needed-rcvd 0
	du-source-route-failed-rcvd 0
	time-requests-rcvd 0
	time-requests-replied 0
	parameter-problems-sent 0
	parameter-problems-rcvd 0
	ttl-exceeded-sent 0
	ttl-exceeded-rcvd 0
	time-to-reassemble-exceeded-sent 0
	time-to-reassemble-exceeded-rcvd 0
	s-quench-sent 0
	s-quench-rcvd 0)) 


;1;;=====================================*
;1;;*
;1;; Function to test the listen method.*
;1;;*

(DEFUN icmp-listen-tester ()
  (SEND *icmp-handler* :listen)) 

;1;;=====================================*
;1;;*
;1;; ICMP Echo *
;1;;*

(DEFUN ICMP-ECHO (Host &optional (repeats 1) (size ICMP-normal-echo-size) (stream *standard-output*))
  "2The ICMP Echo function.  Host may be a symbol, Host object, IP address. 
    Please note that the forms of an address outside of the number itself must be strings.
    Repeats is the number of times to echo. Size is the size of the echo packet.
    Stream is where a summary of the echo status will be output, none is output if it is nil.
    The function returns T if ANY of the echos were returned, otherwise NIL*"
  (DECLARE (SPECIAL *ICMP-ECHO-LIST* *IP-HANDLER*))
  (SETF *ICMP-ECHO-ON* t)
  (SETF *ICMP-ECHO-LIST* nil)
  (LET ((host-addr (FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
	(data (Allocate-ICMP-packet *ICMP-ECHO* 0))
	(len (+ 8 size))
	timeout
	entry
	entry-data
	entry-length
	(timeouts 0)
	(discards 0)
	)
    ;1; Setup echo data.*
    (UNWIND-PROTECT
	(PROGN 
	  (LOOP for h from 0 below size
		do (SETF (AREF data (+ h 8)) h))
	  (SETF (ICMP-Option-1 data) 0)	   ;1No Id needed.*
	  (DOTIMES (dummy repeats)
	    (SETF (ICMP-Option-2 data) (1+ dummy)) ;1Setup sequence number.*
	    (SETF (ICMP-checksum data) 0)  ;1 Clear out checksum.*
	    (SETF (ICMP-checksum data) (IP-ones-complement-checksum data len))
	    ;1;Send the echo*
	    (SEND *IP-HANDLER* :transmit data ICMP-protocol host-addr len)
	    (INCF (icmp-handler-echo-req-sent *ICMP-HANDLER*))
	    (INCF (icmp-handler-packets-sent *ICMP-HANDLER*))
	    ;1;Wait for response. time out in 5 seconds.*
	    (SETF timeout
		  (Process-WAIT-WITH-TIMEOUT "ICMP Echo"
					     (* *ICMP-ECHO-TIMEOUT* 60) 'ICMP-echo-wait))
	    ;1;Check for time-out (null means timeout.)*
	    (COND (timeout		   ;1Something there*
		   (UNWIND-PROTECT
		       (PROGN
			 (SETF entry (POP *ICMP-ECHO-LIST*))
			 (SETF entry-data   (ICMP-ENTRY-data   entry))
			 (SETF entry-length (ICMP-ENTRY-length entry))
			 (COND ((NOT (= len entry-length))
				(INCF discards)
				(unless (null stream)
				  (FORMAT stream "~& Echo ~A discarded. length = ~D, should be ~D"
					  (1+ dummy) entry-length len)
				  (print-data entry-data :size entry-length :stream stream))
				)
			       ((LOOP for h from 8 below len
				      when (NOT (EQUAL (AREF data h) (AREF entry-data h)))
				      do (RETURN t)
				      finally (RETURN nil))
				(INCF discards)
				(unless (null stream)
				  (FORMAT stream "~&Echo ~A discarded. Data not equal." (1+ dummy)))
				)
			       (t
				;1(unless (null stream)*
				;1 (FORMAT stream " Echo Received. Sequence #~D"*
				;	1(ICMP-Option-2 entry-data)))*
				)))
		     (Free-ICMP-packet entry-data)
		     (Free-ICMP-entry  entry))
		   )
		  (t			   ;1Timed out.*
		   (INCF discards)
		   (INCF timeouts)
		   (unless (null stream)
		     (FORMAT stream "~&Echo ~A Timed out." (1+ dummy)))
		   ))
	    ))
      (IF data (Free-ICMP-packet data)))   ;1 Free up the data resource used.*
    (SETF *ICMP-ECHO-ON* nil)
    (unless (null stream)
      (FORMAT stream "~%~%Echos sent = ~A, Successes = ~A, Discards = ~A, Timeouts = ~A"
	      repeats (- repeats discards) discards timeouts))
    (values (not (zerop (- repeats discards)))
	    ;1return non-nil if ANY echos were returned,  RWF 04/28/86*
	    repeats (- repeats discards) discards timeouts)
  )) 


(DEFUN icmp-echo-wait ()
  (DECLARE (SPECIAL *icmp-echo-list*))
  (NOT (NULL *icmp-echo-list*))) 


(COMPILE-FLAVOR-METHODS icmp-handler) 

;1;;==========================================*
;1;;*
;1;; ICMP-ERROR *
;1;;*
;1;; User callable function to send an ICMP*
;1;;  error message that was not caught by IP.*
;1;; Usual error messages are:*
;1;;    1. Destination Unreachable - Port unreachable.*
;1;;    2. Parameter problem. The parameter should be in*
;1;;       the first 8 bytes of the users data to be useful.*
;1;; Note that the parameters are the same as for the IP-transmit *
;1;;   method with the exception of the error and code*
;1;;   for ICMP use.*
;1;;*

(DEFUN icmp-error (type code data protocol destination &optional (an-item nil) (LENGTH (LENGTH data))
		   (source-address (closest-local-address destination)) (precedence 0) (delay 0) (throughput 0)
		   (reliability 0) (fragmentp t) (options nil))
  (DECLARE (SPECIAL *IP-HANDLER*))
  (LET* (data-part
	 (ip-pkt (ALLOCATE-RESOURCE 'ip-packet))
	 kludge)
    (UNWIND-PROTECT
	(PROGN
	  (parse-ip-options ip-pkt options)
	  ;1; Make the IP header to be put in the ICMP packet.*
	  (SETF (ip-version ip-pkt) ip-version)
	  (SETF (ip-precedence ip-pkt) precedence)
	  (SETF (ip-delay ip-pkt) delay)
	  (SETF (ip-thruput ip-pkt) throughput)
	  (SETF (ip-reliability ip-pkt) reliability)
	  (SETF (ip-tos-reserved ip-pkt) 0)
	  (SETF (ip-total-length ip-pkt) (+ (* 4 (ip-header-length ip-pkt)) length))
	  (SETF (ip-identification ip-pkt) (INCF *ip-id-counter*))
	  (SETF (ip-flags-reserved ip-pkt) 0)
	  (SETF (ip-dont-fragment-p ip-pkt) (NOT fragmentp))
	  (SETF (ip-more-fragments-p ip-pkt) nil)
	  (SETF (ip-fragment-offset ip-pkt) 0)
	  (SETF (ip-time-to-live ip-pkt) ip-maximum-routing-time)
	  (UNLESS (NUMBERP protocol)
	    (SETF protocol (CDR (ASSOC protocol *ip-protocol-mapping*))))
	  (SETF (ip-protocol ip-pkt) protocol) (SETF (ip-src-addr ip-pkt) destination)
	  (SETF (ip-dst-addr ip-pkt) source-address)
	  (SETQ data-part (make-ip-data ip-pkt))
	  ;1; Copy only the 8 bytes of user data to be used.*
	  (COPY-ARRAY-PORTION data 0 8 data-part 0 8)
	  ;1; Send this reconstructed packet to ICMP for transmit.*
	  (SETF kludge (make-ip-data ip-pkt 28 0)) ;1 (print-packet ip-pkt :size 36.) *
	  (icmp ip-pkt type code an-item))
      ;1; Cleanup resources.*
      (IF data-part
	  (free-ip-data data-part))
      (DEALLOCATE-RESOURCE 'ip-packet ip-pkt)))) 


;1;;=======================================*
;1;;*
;1;; ICMP-ADDRESS-MASK-TRANSMIT  07/21/86 LER*
;1;; *
;1;; Called by Enable-IP-Service.  To implement*
;1;; TCP/IP subnetting,  two new ICMP type are*
;1;; required.  Address Mask Reply and Address*
;1;; Mask Request.  When an isolated host is*
;1;; booted, he must send an address-mask-request*
;1;; to find out his subnetting mask.  When a*
;1;; gateway is booted, he acquires his mask for*
;1;; each of his subnets, and sends each subnet*
;1;; an address-mask-reply.*
;1;;*
;1;; This function builds an IP header and calls*
;1;; the function ICMP to send the proper ICMP packet*
;1;; with either a reply or request as its type.*
;1;;*

(DEFUN icmp-address-mask-transmit (type code protocol destination &optional (an-item nil) (LENGTH 0)
				   (source-address (closest-local-address destination))
				   (precedence 0) (delay 0) (throughput 0)
				   (reliability 0) (fragmentp t) (options nil))
  (DECLARE (SPECIAL *IP-HANDLER*))
  (LET* (data-part
	 (ip-pkt (ALLOCATE-RESOURCE 'ip-packet))
	 kludge)
    (UNWIND-PROTECT
	(PROGN
	  (parse-ip-options ip-pkt options)
	  ;1; Make the IP header to be put in the ICMP packet.*
	  (SETF (ip-version ip-pkt) ip-version)
	  (SETF (ip-precedence ip-pkt) precedence)
	  (SETF (ip-delay ip-pkt) delay)
	  (SETF (ip-thruput ip-pkt) throughput)
	  (SETF (ip-reliability ip-pkt) reliability)
	  (SETF (ip-tos-reserved ip-pkt) 0)
	  (SETF (ip-total-length ip-pkt) (+ (* 4 (ip-header-length ip-pkt)) length))
	  (SETF (ip-identification ip-pkt) (INCF *ip-id-counter*))
	  (SETF (ip-flags-reserved ip-pkt) 0)
	  (SETF (ip-dont-fragment-p ip-pkt) (NOT fragmentp))
	  (SETF (ip-more-fragments-p ip-pkt) nil)
	  (SETF (ip-fragment-offset ip-pkt) 0)
	  (SETF (ip-time-to-live ip-pkt) ip-maximum-routing-time)
	  (UNLESS (NUMBERP protocol)
	    (SETF protocol (CDR (ASSOC protocol *ip-protocol-mapping*))))
	  (SETF (ip-protocol ip-pkt) protocol) (SETF (ip-src-addr ip-pkt) destination)
	  (SETF (ip-dst-addr ip-pkt) source-address)
	  (SETQ data-part (make-ip-data ip-pkt))
	  ;1; Send this reconstructed packet to ICMP for transmit.*
	  (SETF kludge (make-ip-data ip-pkt 28 0)) ;1 (print-packet ip-pkt :size 36.)*
	  (CONDITION-CASE () (icmp ip-pkt type code an-item) (system:network-error)))
      ;1; Cleanup resources.*
      (IF data-part
	  (free-ip-data data-part))
      (DEALLOCATE-RESOURCE 'ip-packet ip-pkt))))          
