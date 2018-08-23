;;; -*- Mode: Common-Lisp; Base:10; Fonts:(MEDFNT MEDFNB TR12BI); Package:IP; -*-

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
;1;;*
;1;; Each transport-level handler should provide the following methods:*
;1;;*
;1;;  - send-data *
;1;;  - send-pkt*
;1;;*
;1;;  - receive-data*
;1;;  - receive pkt*
;1;;*
;1;;  - allocate packet*
;1;;  - deallocate packet*
;1;;*
;1;;  - print packet*
;1;;  - print packet data*
;1;;  - print packet header*
;1;;*
;1;;  - absolute maximum data size*
;1;;  - maximum efficient data size*
;1;;  - minimum data size*
;1;;  - total packets received*
;1;;  - total packets transmitted*
;1;;*

;1;;==========================================*
;1;;*
;1;; IP HANDLER CONSTANT*
;1;;*
;1;;  For use by upper level protocols.*
;1;;*

(DEFVAR *ip-handler* ()) 


;1;;========================================*
;1;;*
;1;;  IP HANDLER flavor*
;1;;*

(DEFFLAVOR ip-handler
	   (packets-sent
	    packets-received
	    packets-dropped
	    bytes-sent
	    bytes-received
	    time-sending
	    time-receiving
	    packets-allocated
	    packets-deallocated
	    packets-in-use
	    packets-fragmented
	    packet-fragments-sent
	    packets-reassembled
	    packet-fragments-received
	    packet-fragments-oversize
	    packet-fragments-timedout
	    packets-rerouted
	    packets-to-other-net
	    packets-from-other-net
	    packets-not-for-this-host
	    packets-with-bad-checksums
	    packets-with-unknown-gateway-for-destination
	    packets-with-expired-time-to-live
	    packets-received-with-security
	    packets-received-with-lsr
	    packets-received-with-ssr
	    packets-received-with-record-route
	    packets-received-with-stream-id
	    packets-received-with-timestamp
	    packets-sent-with-security
	    packets-sent-with-lsr
       	    packets-sent-with-ssr
	    packets-sent-with-record-route
	    packets-sent-with-stream-id
	    packets-sent-with-timestamp
	    ip-broadcast-pkts-received
	    timer-process)
	   ()
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables
  :outside-accessible-instance-variables) 


(DEFMETHOD (ip-handler :after :init) (ignore)
  (SEND self :initialize)) 


(DEFMETHOD (ip-handler :initialize) ()
  (SETF packets-sent 0
	packets-received 0
	packets-dropped 0
	bytes-sent 0
	bytes-received 0
	time-sending 0
	time-receiving 0
	packets-allocated 0
	packets-deallocated 0
	packets-in-use 0
	packets-fragmented 0
	packet-fragments-sent 0
	packet-fragments-received 0
	packet-fragments-oversize 0
	packet-fragments-timedout 0
	packets-reassembled 0
	packet-fragments-sent 0
	packets-rerouted 0
	packets-to-other-net 0
	packets-from-other-net 0
	packets-not-for-this-host 0
	packets-with-bad-checksums 0
	packets-with-unknown-gateway-for-destination 0
	packets-with-expired-time-to-live 0
	packets-received-with-security 0
	packets-received-with-lsr 0
	packets-received-with-ssr 0
	packets-received-with-record-route 0
	packets-received-with-stream-id 0
	packets-received-with-timestamp 0
	packets-sent-with-security 0
	packets-sent-with-lsr 0
	packets-sent-with-ssr 0
	packets-sent-with-record-route 0
	packets-sent-with-stream-id 0
	packets-sent-with-timestamp 0
	ip-broadcast-pkts-received 0
	timer-process ()))

;1;;==========================================*
;1;;*
;1;; IP PACKET RESOURCE *
;1;;*

(DEFVAR *ip-packet-area* (MAKE-AREA :name 'ip-packet-area :gc :static) "2Static area for IP packets.*")


(ADD-INITIALIZATION "Cleanup IP Packet Area" (si:clean-up-static-area *ip-packet-area*) :full-gc)


(DEFMACRO ip-packet-source-address (buffer)
  "2Array leader accessor for source address.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 0))


(DEFMACRO ip-packet-destination-address (buffer)
  "2Array leader accessor for destination address.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 1))


(DEFMACRO ip-packet-protocol (buffer)
  "2Array leader accessor for protocol.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 2))


(DEFMACRO ip-packet-identification (buffer)
  "2Array leader accessor for identification.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 3))


(DEFMACRO ip-packet-timestamp (buffer)
  "2Array leader accessor for timestamp.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 4))


(DEFMACRO ip-packet-hole-list-header (buffer)
  "2Array leader accessor for hole list header.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 5))


(DEFMACRO ip-packet-link (buffer)
  "2Array leader accessor for next buffer on reassembly list.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 6))


(DEFMACRO ip-packet-max-data-end (buffer)
  "2Array leader accessor for maximum fragment end on reassembly list.  Used on reassembly list only.*"
  `(ARRAY-LEADER ,buffer 7))


(DEFRESOURCE ip-packet ()
  "2The resource for allocating an IP packet.*"
  :constructor (PROGN
		 (MAKE-ARRAY *max-total-packet-16-bit*
			     :type :art-16b
			     :area *ip-packet-area*
			     :leader-list '(0 1 2 3 4 5 6 7)))
  :initializer (PROGN
		 (INCF (ip-handler-packets-in-use *ip-handler*))
		 (INCF (ip-handler-packets-allocated *ip-handler*))
		 (SETF (ip-packet-link object) nil)
		 (SETF (ip-packet-max-data-end object) 0))
  :deallocator (PROGN
		 (INCF (ip-handler-packets-deallocated *ip-handler*))
		 (DECF (ip-handler-packets-in-use *ip-handler*)))
  :matcher t
  :free-list-size 10)


(ethernet:add-net-packet-allocator *ip-ethernet-type*
				   #'(lambda () (ALLOCATE-RESOURCE 'ip-packet))
				   #'(lambda (packet) (DEALLOCATE-RESOURCE 'ip-packet packet)))


(DEFRESOURCE ip-option-data
	     (ip-pkt &optional (LENGTH (- 60 *min-header-bytes*)) (offset *min-header-bytes*))
  "2The resource for allocating the array leader for handling IP options.*"
  :constructor
  (MAKE-ARRAY length :type :art-8b :displaced-to ip-pkt
	      :displaced-index-offset offset :area *ip-area*)
  :initializer (si:change-indirect-array object 'art-8b length ip-pkt offset)
  :matcher t
  :free-list-size 2) 


(DEFUN make-ip-option-data (packet &optional (LENGTH (- 60 *min-header-bytes*)) (offset *min-header-bytes*))
  "2Given a packet return a 16 bit array displaced to the data part of the packet
   with as many data elements as the ip header of the packet designates.*"
  (ALLOCATE-RESOURCE 'ip-option-data packet length offset)) 


(DEFUN free-ip-option-data (packet)
  "2Deallocates the IP-OPTION-DATA resource.*"
  (DEALLOCATE-RESOURCE 'ip-option-data packet)) 


;1;;=============================================*
;1;;*
;1;; IP DATA ARRAY Resource.*
;1;;*
;1;; These are the resource definition and accessor functions for*
;1;; overlaying a packet so as to get the IP data.*
;1;;*
(DEFRESOURCE ip-data-array
	     (packet data-length &optional (offset (* 4 (ip-header-length packet))))
  "2The resource for allocating an array leader for overlaying a packet to get at the data.*"
  :constructor (MAKE-ARRAY data-length :type :art-8b :displaced-to packet :displaced-index-offset offset
			   :leader-length 3
			   :area *ip-area*)
  :initializer (progn
		 (si:change-indirect-array object 'art-8b data-length packet offset)
		 (SETF (ARRAY-LEADER object 1) packet)
		 (SETF (ARRAY-LEADER object 2) offset))
  :matcher t
  :free-list-size 2) 


(DEFUN make-ip-data (packet &optional (elements (ip-total-length packet))
			    (offset (* 4 (ip-header-length packet))))
  "2Allocates a leader for the overlayment of the packet to get at the data.*"
  (ALLOCATE-RESOURCE 'ip-data-array packet elements offset)) 


(DEFUN free-ip-data (data)
  "2Deallocates the array leader from an overlay.*"
  (DEALLOCATE-RESOURCE 'ip-data-array data))
 



;1;*
;1; ip layer service management*
;1;*

(DEFUN reset-ip-service (&optional (enable-p nil) (debug-mode nil) (local-only t))
  (terminate-ip-service)
  (COND ((NOT enable-p)
	 (SETF *ip-handler* #'(lambda (operation &rest ignore)
				(CASE operation
				  (:receive)
				  (otherwise
				   (FERROR 'ip-error "IP Service is not enabled."))))))
	(t (SETF *ip-debug-mode* debug-mode)
	   (SETF *receive-debug-mode* debug-mode)
	   (SETF *send-debug-mode* debug-mode)
	   (SETF *record-packet-mode* nil)
	   (enable-ip-service local-only)))
  "Reset IP Service Complete")


(DEFUN enable-ip-service (&optional (local-only t))
  "2Sets up the protocol handler list for use by IP.*"
  (DECLARE (SPECIAL *udp-protocol-ip-id* *udp-handler* *tcp-protocol* *tcp-handler* icmp-protocol))
  (LET (network-broadcast-addr route-entry)
    (SETF *ip-handler* (MAKE-INSTANCE (IF (TYPEP *ip-handler* 'ip-handler)
					  *ip-handler*
					  'ip-handler)))
    (SETF *ip-pkt-sent-list* ())
    (SETF *ip-pkt-rvcd-list* ())
    (SETF *ip-id-counter* (MOD (time:microsecond-time) 65536))
    (WHEN (VARIABLE-BOUNDP *udp-handler*)
      (PUSH (CONS :udp '*udp-handler*) *ip-protocol-handler-alist*)
      (PUSH (CONS *udp-protocol-ip-id* '*udp-handler*) *ip-protocol-handler-alist*))
    (WHEN (VARIABLE-BOUNDP *tcp-handler*)
      (PUSH (CONS :tcp '*tcp-handler*) *ip-protocol-handler-alist*)
      (PUSH (CONS *tcp-protocol* '*tcp-handler*) *ip-protocol-handler-alist*))
    (WHEN (VARIABLE-BOUNDP *icmp-handler*)
      (PUSH (CONS icmp-protocol '*icmp-handler*) *ip-protocol-handler-alist*)
      (PUSH (CONS :icmp '*icmp-handler*) *ip-protocol-handler-alist*))
    (reset-other-machines)
    (SETF ethernet::ip-ether-address-translations ())
    (SETF ethernet::*ip-arp-requests-received* 0)
    (SETF ethernet::*ip-arp-replies-sent* 0)
    (SETF ethernet::*ip-arp-requests-sent* 0)
    (SETF ethernet::*ip-arp-replies-received* 0)
    (ethernet::add-protocol *ip-ethernet-type*
			    #'(lambda (controller ip-pkt nbytes type src-addr dest-addr)
				(SEND *ip-handler* :receive-data
				      controller ip-pkt nbytes type src-addr dest-addr)))
    (reset-ip-routing-table local-only)
    (SETF (SEND *ip-handler* :timer-process)
	  (PROCESS-RUN-FUNCTION
	    '(:name "IP Packet Fragment Timer" :priority -15)
	    'ip-fragment-time-keeper))
    (enable)
    (DOLIST (addr my-addresses)
      (SETF route-entry (get-routing-entry addr))
      (SETF network-broadcast-addr
	    (LOGIOR (LOGAND addr (ip-routing-mask route-entry))
		    (LOGXOR #xFFFFFFFF (ip-routing-mask route-entry))))
      (DOTIMES (dummy 5)
	(IF *gateway-host-p*
	    (icmp-address-mask-transmit address-mask-reply 0 1 network-broadcast-addr)
	    (icmp-address-mask-transmit address-mask-request 0 1 network-broadcast-addr))
	(sleep .5 "Setup IP Routing")))))


(DEFUN terminate-ip-service (&aux timer-process)
  (DECLARE (SPECIAL *ip-reassembly-list*))
  (ethernet::remove-protocol *ip-ethernet-type*)
  (disable)
  (SETF *ip-reassembly-list* nil)
  (CLEAR-RESOURCE 'ip-data-array () ())
  (CLEAR-RESOURCE 'ip-option-data () ())
  (CLEAR-RESOURCE 'ip-packet () ())
  (SETF *ip-protocol-handler-alist* (QUOTE nil))
  (SETF ethernet::ip-ether-address-translations ())
  (SETF other-machines ())
  (WHEN (TYPEP *ip-handler* 'ip-handler)
    (WHEN (SETF timer-process (SEND *ip-handler* :timer-process))
      (SEND timer-process :kill t)
      (SETF (SEND *ip-handler* :timer-process) nil)))) 


(DEFUN enable ()
  "2Allow handling of ip packets.*"
  (SETQ *ip-receive-enabled* t)
  (SETQ *ip-transmit-enabled* t)) 


(DEFUN disable ()
  "2Disregard all ip packets.*"
  (SETQ *ip-receive-enabled* ())
  (SETQ *ip-transmit-enabled* ()))


;1;*
;1; copy-pkt is defined here so that it can be used inline by the functions below*
;1;*

(DEFUN copy-pkt (from-pkt to-pkt nbytes &optional (from-offset 0) (to-offset 0))
  "2Note that from-offset and to-offset are defined in the units of the from-pkt and to-pkt arrays, respectively.*"
  (IF (NOT *use-blt-p*)
      
      (PROGN
	(UNLESS (EQL (si:array-element-size from-pkt)
		     (si:array-element-size   to-pkt))
	  (FERROR nil "Array elements are different sizes and can't use blt."))
	(IF (EQL (si:array-element-size to-pkt) 8.)
	    (COPY-ARRAY-PORTION from-pkt from-offset (+ nbytes from-offset)
				to-pkt   to-offset (+ nbytes to-offset))
	    (COPY-ARRAY-PORTION from-pkt
				(CEILING from-offset 2)
				(+ (CEILING nbytes 2.) (CEILING from-offset 2))
				to-pkt
				(CEILING to-offset 2)
				(+ (CEILING nbytes 2.) (CEILING to-offset 2)))))
      
      (SETQ from-offset (* (si:array-element-size from-pkt) from-offset))
      (SETQ to-offset   (* (si:array-element-size   to-pkt)   to-offset))
      
      (DO () ((NOT (si:array-indirect-p from-pkt)))
	(AND (si:array-index-offset from-pkt)
	     (INCF from-offset (* (si:array-element-size from-pkt)
				  (si:array-index-offset from-pkt))))
	(SETQ from-pkt (si:array-indirect-to from-pkt)))
      
      (UNLESS (ZEROP (MOD from-offset 32))
	(FERROR nil "Bad FROM input to BLT, offset of ~d. bits is not on a word boundary." from-offset))
      
      (DO () ((NOT (si:array-indirect-p to-pkt)))
	(AND (si:array-index-offset to-pkt)
	     (INCF to-offset (* (si:array-element-size to-pkt)
				(si:array-index-offset to-pkt))))
	(SETQ to-pkt (si:array-indirect-to to-pkt)))
      
      (UNLESS (ZEROP (MOD to-offset 32))
	(FERROR nil "Bad TO input to BLT, offset of ~d. bits is not on a word boundary." to-offset))
      
      (LET ((si:inhibit-scavenging-flag t))
	(WITHOUT-INTERRUPTS
	  (sys:%blt (sys:%make-pointer-offset si:dtp-fix from-pkt (+ (CEILING from-offset 32.)
								     (si:array-data-offset from-pkt)))
		    (sys:%make-pointer-offset si:dtp-fix to-pkt   (+ (CEILING to-offset 32.)
								     (si:array-data-offset to-pkt)))
		    (CEILING nbytes 4)
		    1)))))


;1;*
;1; packet transmission*
;1;*

(DEFMETHOD (ip-handler :transmit) (data protocol destination-address
				   &optional (length (length data))
				   (source-address (closest-local-address destination-address))
				   (precedence 0)
				   (delay 0)
				   (throughput 0)
				   (reliability 0)
				   (fragmentp t)
				   (options nil))
  "2This does the initial setup of the IP packet and calls the IP-TRANSMIT function for sending.
The parameters are used as follows:
DATA - The data to be sent
PROTOCOL - the sending protocol. i.e. :UDP, :TCP, :ICMP
DESTINATION-ADDRESS - The Ip address of the destination.
Optional parameters are as follows:
LENGTH - The length of the data.
SOURCE-ADDRESS - IP address of the source.
PRECEDENCE - Numeric value from 0 to 7.
DELAY - 0 = Normal delay, 1 = Low delay.
THROUGHPUT - 0 = Normal Throughput, 1 = High Throughput.
RELIABILITY - 0 = Normal Reliability, 1 = High Reliability.
FRAGMENTP - t = May fragment this packet, nil = Don't fragment.
OPTIONS  - A list of options.  Each element in this list is a sublist beginning with a keyword and elements as follows:
:SECURITY security compartments handling transmission
  Security is the keyword :SECURITY followed by the integers representing
  the level of security, the compartments used, the handling restrictions
  and the transmission control.
:LOOSE-SRR <addr> [<addr> ...] 
  The Loose Source and Record Route is the keyword :LOOSE-SRR followed by one or more addresses to use as the route.
:STRICT-SRR <addr> [<addr> ...] 
  The Strict Source and Record Route is the keyword :STRICT-SRR followed by one or more addresses for the route to use.
:RECORD <number of addresses> 
  The Record route is the keyword :RECORD followed by the number of addresses to record.
:STREAM-ID id 
  The Stream Identifier is the keyword :STREAM-ID followed by the identifier for the stream.
:TIMESTAMP flag <number of stamps> [<addr> ...]  
  The Timestamp is the keyword :TIMESTAMP followed by a Flag field which
  can take the values of 0 - for Timestamps only, 1 - Get address as
  well as timestamps, and 3 - Specify the addresses to get timestamps
  from.  The number of stamps follows the flag field and is followed by
  the addresses if the flag field is a 3.*"
  ;1; Fragment down to *max-ethernet-packet-bytes*.  *
  ;1; Further fragmentation will be done if the routing logic determines it is necessary.*
  (DECLARE (inline copy-pkt))
  (LET ((default-cons-area *ip-area*)
	(start-time (time:fixnum-microsecond-time))
	ip-pkt header-length)
    (UNWIND-PROTECT
	(PROGN
	  (SETF ip-pkt (ALLOCATE-RESOURCE 'ip-packet))
	  (SETF header-length (parse-ip-options ip-pkt options))
	  (SETF (ip-version ip-pkt) ip-version)
	  (SETF (ip-precedence ip-pkt) precedence)
	  (SETF (ip-delay ip-pkt) delay)
	  (SETF (ip-thruput ip-pkt) throughput)
	  (SETF (ip-reliability ip-pkt) reliability)
	  (SETF (ip-tos-reserved ip-pkt) 0)	  
	  (SETF (ip-identification ip-pkt) (INCF *ip-id-counter*))
	  (SETF (ip-flags-reserved ip-pkt) 0)
	  (SETF (ip-dont-fragment-p ip-pkt) (NOT fragmentp))
	  (SETF (ip-time-to-live ip-pkt) ip-maximum-routing-time)
	  (UNLESS (NUMBERP protocol) (SETF protocol (CDR (ASSOC protocol *ip-protocol-mapping*))))
	  (SETF (ip-protocol ip-pkt) protocol)
	  (SETF (ip-src-addr ip-pkt) source-address)
	  (SETF (ip-dst-addr ip-pkt) destination-address)
	  (WHEN (> (+ length header-length) 65535)
	    (FERROR 'ip-error "IP maximum length exceeded"))
	  ;1; outbound fragmentation (optimized for ethernet)*
	  (DO* ((remaining-length length (- remaining-length copy-length))
		(from-offset 0 (+ from-offset copy-length))
		(frag-offset 0 (+ frag-offset max-frag-blocks))		
		(max-bytes (- *max-ethernet-packet-bytes* header-length))
		(max-frag-blocks (TRUNCATE max-bytes 8))
		(frag-copy-length (* max-frag-blocks 8))
		(frag-count 0 (1+ frag-count))
		copy-length)
	       ((ZEROP remaining-length))
	    ;1; complete the header*
	    (COND ((> remaining-length max-bytes)
		   (SETF copy-length frag-copy-length)
		   (SETF (ip-more-fragments-p ip-pkt) t))
		  (t (WHEN (PLUSP frag-count)
		       (INCF packet-fragments-sent (1+ frag-count))
		       (INCF packets-fragmented))
		     (SETF copy-length remaining-length)
		     (SETF (ip-more-fragments-p ip-pkt) nil)))
	    (WHEN (AND options (EQL frag-count 1))
	      (modify-header-options ip-pkt))
	    (SETF (ip-total-length ip-pkt) (+ header-length copy-length))
	    (SETF (ip-fragment-offset ip-pkt) frag-offset)
	    ;1; Set up Data portion of the packet.*
	    (copy-pkt data ip-pkt copy-length from-offset (TRUNCATE header-length 2))
	    (WHEN *ip-debug-mode*
              (FORMAT t "~%IP packet transmitted~%")
              (print-header ip-pkt))
	    ;1; route the packet*
	    (COND ((OR (EQL destination-address #xFFFFFFFF) (EQL destination-address #x00000000))
		   (DOLIST (route-entry ip-routing-table)
		     (WHEN (EQ (ip-routing-address route-entry) :direct)
		       (SETF (ip-dst-addr ip-pkt)
			     (LOGIOR (ip-routing-network route-entry)
				     (LOGXOR #xFFFFFFFF (ip-routing-mask route-entry))))
		       (CONDITION-CASE ()
			   (ip-transmit ip-pkt)
			 (incomplete-routing-table)))))
		  (t (CONDITION-CASE ()
			 (ip-transmit ip-pkt)
		       (incomplete-routing-table))))))
      ;1; unwind-protect cleanup*
      (WHEN ip-pkt (DEALLOCATE-RESOURCE 'ip-packet ip-pkt))
      (INCF time-sending
	    (ROUND (TIME-DIFFERENCE (time:fixnum-microsecond-time) start-time) 1000)))))


(DEFUN ip-transmit (ip-pkt &optional routing-entry)
  "2Common transmit function used in processing outbound packets from the local host, and
packets that have arrived from a (sub)network and are being retransmitted after routing.*"
  (DECLARE (:self-flavor ip-handler))
  (BLOCK ip-transmit
    (WHEN (NOT (AND *ip-transmit-enabled* (NOT (NULL my-addresses))))
      (RETURN-FROM ip-transmit))
    (LET* ((pkt-nbytes (ip-total-length ip-pkt))
	   (destination (ip-dst-addr ip-pkt))	   ;1This is for routing information.*
	   (routing-entry (OR routing-entry (get-routing-entry destination)))
	   (controller (ip-routing-controller routing-entry))
	   (out-address
	     (COND
	       ((EQ (ip-routing-address routing-entry) :direct) destination)
	       ((ip-routing-address routing-entry))))
	   (e-addr (unless (null out-address)
		     (ip-get-host-ethernet-address out-address controller))))
      ;1; when the routing table reflects max packet size per network, uncomment*
      (COMMENT
	(WHEN (> (ip-total-length ip-pkt) maximum-transmission-unit) 
	  (WHEN *send-debug-mode*
	    (FORMAT t "~&IP-TRANSMIT: Fragmenting the packet Length = ~A, MTU = ~A"
		    (ip-total-length ip-pkt) maximum-transmission-unit))
	  (UNLESS (ip-dont-fragment-p ip-pkt)
	    (fragment-ip-pkt ip-pkt maximum-transmission-unit routing-entry))
	  (RETURN-FROM ip-transmit)))
      (WHEN (EQ *send-debug-mode* :verbose)
	(FORMAT t "~&IP-TRANSMIT: Sending ~a Ethernet address = ~16,12r" ip-pkt e-addr))
      (SETF (ip-header-checksum ip-pkt) 0)
      (SETF (ip-header-checksum ip-pkt)
	    (ip-ones-complement-checksum ip-pkt (* 2 (ip-header-length ip-pkt))))
      (WHEN *send-debug-mode*
	(LET ((a (MAKE-ARRAY (ip-total-length ip-pkt) :type :art-16b :area *ip-area*)))
	  (COPY-ARRAY-PORTION ip-pkt 0 (FLOOR (ip-total-length ip-pkt) 2) a 0
			      (FLOOR (ip-total-length ip-pkt) 2))
	  (PUSH a *ip-pkt-sent-list*)
	  (FORMAT t "~&IP Packet transmitted:")
	  (print-header ip-pkt))
	(WHEN (EQ *send-debug-mode* :verbose)
	  (print-packet ip-pkt :size (ip-total-length ip-pkt))))
      ;1;only receive broadcasts for this subnet or network*
      (WHEN (local-broadcast-address-p destination)
	(WHEN (EQ *send-debug-mode* :verbose)
	  (FORMAT t "~&IP-TRANSMIT: Sent to self - ~A" ip-pkt))
	(WHEN *record-packet-mode* 
	  (record-packet ip-pkt :transmit))
	(INCF packets-sent)
	(INCF bytes-sent pkt-nbytes)
	(SEND self :receive-data controller ip-pkt pkt-nbytes nil nil nil nil))
      (COND ((MEMBER destination my-addresses)	   ;1Check all this machines addresses*
	     (WHEN (EQ *send-debug-mode* :verbose)
	       (FORMAT t "~&IP-TRANSMIT: Sent to self - ~A" ip-pkt))
	     (WHEN *record-packet-mode* 
	       (record-packet ip-pkt :transmit))
	     (INCF packets-sent)
	     (INCF bytes-sent pkt-nbytes)
	     (SEND self :receive-data controller ip-pkt pkt-nbytes nil nil nil nil))
	    (e-addr
	     (INCF packets-sent)
	     (INCF bytes-sent pkt-nbytes)
	     (WHEN *record-packet-mode*
	       (record-packet ip-pkt :transmit))
	     (WHEN (NOT (EQ (ip-routing-address routing-entry) :direct))
	       (INCF packets-to-other-net))
	     (WITHOUT-INTERRUPTS
	       (SEND controller :transmit-fast *ip-ethernet-type* e-addr ip-pkt pkt-nbytes))
	     (WHEN (EQ *send-debug-mode* :verbose)
	       (FORMAT t "~&IP-TRANSMIT: Sent ~a" ip-pkt)))
	    (t
	     (INCF packets-dropped)
	     (WHEN (EQ *send-debug-mode* :verbose)
	       (FORMAT t "~&IP-TRANSMIT: Dropped a packet due to no ethernet address. destination = ~16,8,'0r"
		       destination)))))))


(DEFUN fragment-ip-pkt (big-pkt maximum-transmission-unit routing-entry
			&aux ip-pkt header-length original-offset)
  "2Fragment or refragment and IP packet down to MAXIMUM-TRANSMISSION-UNIT size.*"
  (DECLARE (:self-flavor ip-handler))
  (SETF header-length (* 4 (ip-header-length big-pkt)))
  (SETF original-offset (* 8 (ip-fragment-offset big-pkt)))
  (UNWIND-PROTECT
      (PROGN
	(SETF ip-pkt (ALLOCATE-RESOURCE 'ip-packet))
	(copy-pkt big-pkt ip-pkt header-length)
	(DO* ((remaining-length (- (ip-total-length ip-pkt) header-length) (- remaining-length copy-length))
	      (from-offset (TRUNCATE header-length 2) (+ from-offset copy-length))
	      (frag-offset original-offset (+ frag-offset max-frag-blocks))		
	      (max-bytes (- maximum-transmission-unit header-length))
	      (max-frag-blocks (TRUNCATE max-bytes 8))
	      (frag-copy-length (* max-frag-blocks 8))
	      (frag-count 0 (1+ frag-count))
	      copy-length)
	     ((ZEROP remaining-length))
	  ;1; complete the header*
	  (COND ((> remaining-length max-bytes)
		 (SETF copy-length frag-copy-length)
		 (SETF (ip-more-fragments-p ip-pkt) t))
		(t (WHEN (PLUSP frag-count)
		     (INCF packet-fragments-sent (1+ frag-count))
		     (INCF packets-fragmented))
		   (SETF copy-length remaining-length)
		   (SETF (ip-more-fragments-p ip-pkt) nil)))
	  (WHEN (AND (ZEROP original-offset) (EQL frag-count 1))
	    (modify-header-options ip-pkt))
	  (SETF (ip-total-length ip-pkt) (+ header-length copy-length))
	  (SETF (ip-fragment-offset ip-pkt) frag-offset)
	  ;1; Set up Data portion of the packet.*
	  (copy-pkt big-pkt ip-pkt copy-length from-offset (TRUNCATE header-length 2))
	  (WHEN *ip-debug-mode*
	    (FORMAT t "~%IP packet transmitted~%")
	    (print-header ip-pkt))
	  ;1; route the fragment*
	  (ip-transmit ip-pkt routing-entry)))
    (WHEN ip-pkt (DEALLOCATE-RESOURCE 'ip-packet ip-pkt))))



;1;*
;1; packet reception*
;1;*

(DEFMETHOD (ip-handler :receive-data) (controller ip-pkt ignore
				       &optional pkt-type src-addr dest-addr (free-buffer t)
				       &aux (default-cons-area *ip-area*) (si:background-cons-area *ip-area*)
				       (words (TRUNCATE (1+ (ip-total-length ip-pkt)) 2))
				       original-checksum recalculated-checksum
				       inbound-rte outbound-rte dest-addr-mask
				       broadcast-hostaddr broadcast-received received-locally)
  "2Called by (:method ethernet:ethernet-controller :receive-top-level).*"
  (DECLARE (IGNORE pkt-type src-addr))
  (UNWIND-PROTECT
      (BLOCK receive-data
	(WHEN (OR (NOT *ip-receive-enabled*) (NULL my-addresses))
	  (RETURN-FROM receive-data))
	(WHEN *record-packet-mode*
	  (record-packet ip-pkt :receive))
	(WHEN *receive-debug-mode*
	  (LET ((a (MAKE-ARRAY words :type :art-16b :area *ip-area*)))
	    (COPY-ARRAY-PORTION ip-pkt 0 words a 0 words)
	    (PUSH a *ip-pkt-rvcd-list*)
	    (FORMAT t "~&IP Packet Received:")
	    (print-header ip-pkt)))
	(INCF packets-received)
	(SETF original-checksum (ip-header-checksum ip-pkt))
	(SETF (ip-header-checksum ip-pkt) 0)
	(SETF recalculated-checksum
	      (ip-ones-complement-checksum ip-pkt (* 2 (ip-header-length ip-pkt))))
	(SETF (ip-header-checksum ip-pkt) original-checksum)
	(CONDITION-CASE ()
	    (PROGN
	     ;1; checksum verification*
	     (WHEN (NOT (EQL original-checksum recalculated-checksum))
	       (INCF packets-with-bad-checksums)
	       (icmp ip-pkt parameter-problem 0 10)
	       (RETURN-FROM receive-data))
	     ;1; header verification*
	     (WHEN (OR (NOT (= (ip-version ip-pkt) ip-version))
		       (< (ip-header-length ip-pkt) 5) (> (ip-header-length ip-pkt) 15))
	       (INCF packets-dropped)
	       (icmp ip-pkt parameter-problem 0 0)
	       (RETURN-FROM receive-data))
	     (INCF bytes-received (- (ip-total-length ip-pkt) (* 4 (ip-header-length ip-pkt))))
	     ;1; routing logic*
	     (SETF dest-addr (ip-dst-addr ip-pkt))
	     (DOLIST (route-info ip-routing-table)
	       (WHEN (AND (EQ (ip-routing-controller route-info) controller)
			  (EQ (ip-routing-address route-info) :direct))
		 (SETF inbound-rte route-info)))
	     (SETF outbound-rte (IF (OR (EQL dest-addr #xFFFFFFFF) (EQL dest-addr #x00000000))
				    inbound-rte
				    (get-routing-entry dest-addr)))
	     ;1; receive packet locally*
	     (WHEN (SETF broadcast-received (local-broadcast-address-p dest-addr))
	       (INCF ip-broadcast-pkts-received))
	     (WHEN (SETF received-locally (OR broadcast-received (MEMBER dest-addr my-addresses)))
	       (ip-pass-on-packet ip-pkt))
	     ;1; route packet*
	     (WHEN *gateway-host-p*
	       (COND
		 ;1; when subnetted and destination host is broadcasthost*
		 (broadcast-received
		  (SETF dest-addr-mask (get-default-mask dest-addr))
		  (SETF broadcast-hostaddr (LOGXOR #xFFFFFFFF dest-addr-mask))
		  (WHEN (AND (NOT (EQL dest-addr-mask (ip-routing-mask inbound-rte)))
			     (OR (ZEROP (LOGAND dest-addr broadcast-hostaddr))
				 (EQL #xFFFFFFFF
				      (LOGIOR dest-addr-mask (LOGAND dest-addr broadcast-hostaddr)))))
		    ;1; recompute broadcast-hostaddr to exclude subnet bits*
		    (SETF broadcast-hostaddr (LOGXOR #xFFFFFFFF (ip-routing-mask inbound-rte)))
		    (DOLIST (route-info ip-routing-table)
		      (WHEN (AND (NOT (EQL route-info inbound-rte))
				 (EQL (LOGAND (ip-routing-network route-info) dest-addr-mask)
				      (LOGAND dest-addr dest-addr-mask)))
			(INCF packets-rerouted)
			(SETF (ip-dst-addr ip-pkt)
			      (LOGIOR broadcast-hostaddr (ip-routing-network route-info)))
			(ip-transmit ip-pkt route-info)))))
		 ;1; no routing needed here*
		 ((EQL inbound-rte outbound-rte)
		  (WHEN (NOT received-locally) (INCF packets-not-for-this-host)))
		 ;1; no routing possible here*
		 ((NULL outbound-rte)
		  (INCF packets-with-unknown-gateway-for-destination)
		  (WHEN (NOT (NULL (get-routing-entry (ip-src-addr ip-pkt))))
		    (icmp ip-pkt destination-unreachable 3 0)))
		 ;1; on to next hop*
		 (t (INCF packets-rerouted)
		    (ip-transmit ip-pkt outbound-rte)))))
	    (system:network-error (INCF packets-dropped))))
    ;1; CLean up resources.*
    (WHEN free-buffer (DEALLOCATE-RESOURCE 'ip-packet ip-pkt))))


(DEFUN ip-pass-on-packet (ip-pkt
			  &aux start-time deallocate-packet)
  "2Passes on packets to be reassembled or to the handler when complete.
All handlers take arguments: art-16b data array, start index, length, source address, destination address.*"
  (DECLARE (:self-flavor ip-handler))
  (LET (handler	complete-pkt header-length)
    (UNWIND-PROTECT
	(WHEN (SETF complete-pkt (COND ((AND (NOT (ip-more-fragments-p ip-pkt))
					     (ZEROP (ip-fragment-offset ip-pkt)))
					ip-pkt)
				       (t
					(SETF deallocate-packet t)
					(INCF packet-fragments-received)
					(COND ((ip-reassemble-packet ip-pkt))
					      (t (SETF deallocate-packet nil))))))	    
	  (COND ((SETF handler (EVAL (CDR (ASSOC (ip-protocol ip-pkt) *ip-protocol-handler-alist*))))
		 (SETF start-time (time:fixnum-microsecond-time))
		 (SEND handler :receive-data complete-pkt
		       (SETF header-length (* 2 (ip-header-length complete-pkt)))
		       (- (ip-total-length complete-pkt) (* 2 header-length))
		       (ip-src-addr complete-pkt)
		       (ip-dst-addr complete-pkt))
		 (INCF time-receiving
		       (ROUND (TIME-DIFFERENCE (time:fixnum-microsecond-time) start-time) 1000)))
		(t (INCF packets-dropped))))	  
      ;1; clean up*
      (WHEN deallocate-packet
	(DEALLOCATE-RESOURCE 'ip-packet complete-pkt)))))

 

;1; Packet reassembly logic.*
;1;*
;1; Warning: Holes in the reassembly buffer (and packet fragments)*
;1;          are sized in multiple of 8 octets.  The reassembly buffer*
;1;          is an art-16b array, and thus sized in 2 octet chunks.*
;1;          The hole list uses the indices of the base array, i.e.*
;1;          indexing art-16b sized elements.*
;1;*
;1; Note:  The reassembly list does not have a lock.  Rather, reassembly*
;1;        buffers are removed from the list when being operated on.  *
;1;        This could potentially cause problems when fragments arrive over*
;1;        different controllers simultaneously.  We consider this event*
;1;        as unlikely, unless multihoming is supported.*

(DEFMACRO set-hole-list-header (first-hole)
  "2Udate the hole list header in the reassembly buffer.*"
  `(SETF (ip-packet-hole-list-header reassembly-buffer) ,first-hole))


(DEFMACRO hole-list-header ()
  "2Return the value of the hole list header in the reassembly buffer.*"
  '(ip-packet-hole-list-header reassembly-buffer))


(DEFMACRO make-hole (hole-beg hole-end next-hole)
  "2Insert a hole descriptor in the reassembly buffer.*"
  `(PROGN
     (SETF (AREF reassembly-buffer ,hole-beg) ,hole-end)
     (SETF (AREF reassembly-buffer (+ ,hole-beg 1)) (COND (,next-hole) (t 0)))))


(DEFUN set-next-hole (hole-beg next-hole)
  "2Update a hole descriptor next-hole field of the reassembly buffer.*"
  `(SETF (AREF reassembly-buffer (+ ,hole-beg 1)) ,next-hole))


(DEFMACRO get-hole-information (hole-beg)
  "2Retrieve a hole descriptor from the reassembly buffer.  Return two values: hole end, next hole.*"
  `(LET ((next-hole (AREF reassembly-buffer (+ ,hole-beg 1))))
     (VALUES (AREF reassembly-buffer ,hole-beg)
	     (IF (ZEROP next-hole)
		 nil
		 next-hole))))


(DEFVAR *ip-reassembly-list* nil
  "2Active reassembly buffers.  This list is indexed by: source address, destination address, protocol, and identification.*")


(DEFUN ip-reassemble-packet (ip-pkt
			     &aux header-length total-length data-byte-length
			     reassembly-buffer frag-beg frag-end frag-offset from-offset
			     packet-source-address packet-destination-address
			     packet-protocol packet-identification)
  "2This function reassembles the packets and returns a fully reassembled packet to be processed.
If the packet is not fully assembled yet, nil is returned.  Refer to RFC815.*"
  (DECLARE (:self-flavor ip-handler))
  (DECLARE (inline copy-pkt))
  ;1; set up reassembly overhead*
  (SETF packet-source-address (ip-src-addr ip-pkt))
  (SETF packet-destination-address (ip-dst-addr ip-pkt))
  (SETF packet-protocol (ip-protocol ip-pkt))
  (SETF packet-identification (ip-identification ip-pkt))
  ;1; units are bytes for frag-offset*
  (SETF frag-offset (* 8 (ip-fragment-offset ip-pkt)))
  ;1; units are words for header-length*
  (SETF header-length (ip-header-length ip-pkt))
  ;1; units are bytes for total-length*
  (SETF total-length (ip-total-length ip-pkt))
  (SETF data-byte-length (- total-length (* 4 header-length)))
  ;1; units are 16 bit shortwords for from-offset*
  (SETF from-offset (* 2 header-length))
  ;1; find existing reassembly buffer, or allocate a new one*
  (COND ((SETF reassembly-buffer
	       (WITHOUT-INTERRUPTS
		 (DO ((p *ip-reassembly-list* (ip-packet-link p))
		      (q nil p))
		     ((NULL p))
		   ;1; delink the reassembly buffer (if found)*
		   (WHEN (AND (EQL packet-source-address (ip-packet-source-address p))
			      (EQL packet-destination-address (ip-packet-destination-address p))
			      (EQL packet-protocol (ip-packet-protocol p))
			      (EQL packet-identification (ip-packet-identification p)))
		     (IF q
			 (SETF (ip-packet-link q) (ip-packet-link p))
			 (SETF *ip-reassembly-list* (ip-packet-link p)))
		     (RETURN p)))))
	 ;1; copy the fragment zero header (if it did not arrive first)*
	 (WHEN (ZEROP frag-offset)
	   (COPY-ARRAY-PORTION ip-pkt 0 from-offset reassembly-buffer 0 from-offset)
	   (ARRAY-INITIALIZE reassembly-buffer 0 from-offset (TRUNCATE *max-header-bytes* 2))
	   (SETF (ip-header-length reassembly-buffer) *max-header-32-bit*)))
	(t
	 ;1; copy in the header of first fragment arriving (allow for the maximal sized header)*
	 (SETF reassembly-buffer (ALLOCATE-RESOURCE 'ip-packet))
	 (SETF (ip-packet-source-address reassembly-buffer) packet-source-address)
	 (SETF (ip-packet-destination-address reassembly-buffer) packet-destination-address)
	 (SETF (ip-packet-protocol reassembly-buffer) packet-protocol)
	 (SETF (ip-packet-identification reassembly-buffer) packet-identification)
	 (SETF (ip-packet-timestamp reassembly-buffer) (TIME))
	 (COPY-ARRAY-PORTION ip-pkt 0 from-offset reassembly-buffer 0 from-offset)
	 (ARRAY-INITIALIZE reassembly-buffer 0 from-offset (TRUNCATE *max-header-bytes* 2))
	 (SETF (ip-header-length reassembly-buffer) *max-header-32-bit*)
	 ;1; initialize the hole list*
	 (set-hole-list-header (TRUNCATE *max-header-bytes* 2))
	 (make-hole (TRUNCATE *max-header-bytes* 2) (ARRAY-LENGTH reassembly-buffer) nil)))
  ;1; convert units to 16 bit shortwords rather than bytes or longwords*
  (SETF frag-beg (TRUNCATE (+ *max-header-bytes* frag-offset) 2))
  (SETF frag-end (+ frag-beg (- (CEILING total-length 2) from-offset)))
  (BLOCK ip-reassemble-packet
    (WHEN (> frag-end (ARRAY-LENGTH reassembly-buffer))
      (WHEN *receive-debug-mode*
	(FORMAT t "~&OVERSIZE packet size = ~x, max length = ~x" frag-end (ARRAY-LENGTH reassembly-buffer)))
      (icmp reassembly-buffer parameter-problem 0 2)
      (DEALLOCATE-RESOURCE 'ip-packet reassembly-buffer)
      (INCF packet-fragments-oversize)
      ;1; packet not complete - return nil*
      (RETURN-FROM ip-reassemble-packet nil))
    ;1; perform the "eight step reassembly algorithm" described in RFC815*
    (DO ((hole-beg (hole-list-header) next-hole)
	 hole-end
	 (prev-hole nil hole-beg)
	 next-hole)
	;1; terminate when no more holes to consider (the hole list is ordered)*
	((OR (NULL hole-beg) (<= frag-end hole-beg)))
      ;1; check if fragment interacts with this hole*
      (MULTIPLE-VALUE-SETQ (hole-end next-hole) (get-hole-information hole-beg))
      (WHEN (< frag-beg hole-end)
	;1; delete the current hole from the hole list*
	(IF prev-hole
	    (set-next-hole prev-hole next-hole)
	    (set-hole-list-header next-hole))
	;1; create new holes, if necessary*
	(WHEN (> frag-beg hole-beg)
	  (IF prev-hole
	      (set-next-hole prev-hole hole-beg)
	      (set-hole-list-header hole-beg))
	  (make-hole hole-beg frag-beg next-hole)
	  (SETF prev-hole hole-beg))
	(WHEN (AND (< frag-end hole-end) (ip-more-fragments-p ip-pkt))	
	  (IF prev-hole
	      (set-next-hole prev-hole frag-end)
	      (set-hole-list-header frag-end))
	  (make-hole frag-end hole-end next-hole))))
    ;1; copy fragment into reassembly buffer*
    (copy-pkt ip-pkt reassembly-buffer (* 2 (- frag-end frag-beg)) from-offset frag-beg)
    (SETF (ip-packet-max-data-end reassembly-buffer) (MAX (+ *max-header-bytes* frag-offset data-byte-length)
							  (ip-packet-max-data-end reassembly-buffer)))
    ;1; if the hole list is not empty, return nil (reassembly incomplete)*
    (WHEN (hole-list-header)
      (WITHOUT-INTERRUPTS
	(SETF (ip-packet-link reassembly-buffer) *ip-reassembly-list*)
	(SETF *ip-reassembly-list* reassembly-buffer))
      (RETURN-FROM ip-reassemble-packet nil))
    ;1; update the total packet length, and return the ip-packet*
    (SETF (ip-total-length reassembly-buffer) (ip-packet-max-data-end reassembly-buffer))
    (WHEN *receive-debug-mode*
      (FORMAT t "~&Completed packet.")
      (print-packet reassembly-buffer :size (ip-total-length reassembly-buffer)))
    (INCF packets-reassembled)
    (RETURN-FROM ip-reassemble-packet reassembly-buffer)))
  

(DEFUN ip-fragment-time-keeper ()
  "2Checks each item on the *ip-reassembly-list* to see if its time has run out.
If it has, it deletes the item.  After checking the list it goes to sleep for a minute.*"
  (DO ((expired-list nil nil))
      (nil)
    (PROCESS-SLEEP 600)
    ;1; delink all expired reassembly buffers*
    (WITHOUT-INTERRUPTS
      (DO ((p *ip-reassembly-list* (ip-packet-link p))
	   (q nil p))
	  ((NULL p))
	(WHEN (NOT (TIME-LESSP (TIME)
			       (TIME-INCREMENT (ip-packet-timestamp p) (* 60 (ip-time-to-live p)))))
	  (IF q
	      (SETF (ip-packet-link q) (ip-packet-link p))
	      (SETF *ip-reassembly-list* (ip-packet-link p)))
	  (SETF (ip-packet-link p) expired-list)
	  (SETF expired-list p))))
    ;1; deallocate all expired reassembly buffers*
    (DO ((p expired-list (ip-packet-link p)))
	((NULL p))
      (WHEN *receive-debug-mode*
	(FORMAT t "~&FRAGMENT REASSEMBLY TIME EXPIRED: reassembly-buffer is ~A" p))
      (CONDITION-CASE ()
	  (icmp p time-exceeded fragment-reassembly-time-exceeded)
	(sys:network-error))
      (DEALLOCATE-RESOURCE 'ip-packet p)
      (INCF (ip-handler-packet-fragments-timedout *ip-handler*)))))


;1;*
;1; option parsing and setup functions*
;1;*
(DEFUN parse-ip-options (ip-pkt options)
  "2Parses the options as passed to the :TRANSMIT method for IP-HANDLER.
Returned value is the number of bytes in the header.*"
  (LET ((offset *min-header-bytes*)
	current-opt
	option-length)
    (ARRAY-INITIALIZE ip-pkt 0 0 (MIN (TRUNCATE *max-header-bytes* 2) (ARRAY-LENGTH ip-pkt)))
    (WHEN options
      (LOOP for h in options do
	    (PROGN
	      (CASE (FIRST h)
		(:security		   ;1Security Option*
		 (SETF current-opt (make-ip-option-data ip-pkt offset ip-security-option-size))
		 (UNLESS (NULL current-opt)
		   (setup-security-option current-opt h))
		 (INCF offset ip-security-option-size)
		 (INCF (ip-handler-packets-sent-with-security *ip-handler*)))
		((:loose-srr :strict-srr :record)  ;1Loose-srr, Strict-srr*
		 (SETF option-length (- 1 (* 4 (LENGTH h))))	   ;1or Record Route*
		 (SETF current-opt (make-ip-option-data ip-pkt offset option-length))
		 (UNLESS (NULL current-opt)
		   (setup-srr current-opt h))
		 (INCF offset option-length)
		 (INCF (ip-handler-packets-sent-with-lsr *ip-handler*)))
		(:stream		   ;1Stream Option*
		 (SETF current-opt (make-ip-option-data ip-pkt offset ip-stream-identifier-length))
		 (SETF (stream-type current-opt) ip-stream-identifier-option)
		 (SETF (stream-len current-opt) ip-stream-identifier-length)
		 (SETF (stream-id-high current-opt) (LDB (BYTE 8 8) (SECOND h)))
		 (SETF (stream-id-low current-opt) (LDB (BYTE 8 0) (SECOND h)))
		 (INCF offset ip-stream-identifier-length)
		 (INCF (ip-handler-packets-sent-with-stream-id *ip-handler*)))
		(:timestamp		   ;1Timestamp option*
		 (SETF option-length
		       (+ 4
			  (* (THIRD h)
			     (CASE (SECOND h)
			       ((1 3) 8)   ;1Time and address*
			       (0 4)	   ;1Time only*
			       (:otherwise ;1Wrong flag.*
				(FERROR 'ip-option-error "Timestamp option wrong flag ~A"
					(SECOND h)))))))
		 (SETF current-opt (make-ip-option-data ip-pkt offset option-length))
		 (SETF (time-type current-opt) ip-time-stamp-option)
		 (SETF (time-len current-opt) option-length) (SETF (time-pointer current-opt) 5)
		 (SETF (time-overflow current-opt) 0) (SETF (time-flag current-opt) (SECOND h))
		 (WHEN (EQUAL (SECOND h) 3)	   ;1Addresses specified.*
		   (LOOP for l from 4 below option-length by 8 for m from 3 below (LENGTH h) do
			 (SETF (AREF current-opt l) (LDB (BYTE 8 24) (NTH m h))) do
			 (SETF (AREF current-opt (+ 1 l)) (LDB (BYTE 8 16) (NTH m h))) do
			 (SETF (AREF current-opt (+ 2 l)) (LDB (BYTE 8 8) (NTH m h))) do
			 (SETF (AREF current-opt (+ 3 l)) (LDB (BYTE 8 0) (NTH m h)))))
		 (INCF offset option-length)
		 (INCF (ip-handler-packets-sent-with-timestamp *ip-handler*))))
	      (free-ip-option-data current-opt))))
    (SETF (ip-header-length ip-pkt) (TRUNCATE offset 4))
    offset)) 


(DEFUN setup-srr (where option)
  "2Sets up the LOOSE SOURCE RECORD ROUTE, STRICT SOURCE RECORD ROUTE, or RECORD ROUTE option.*"
  (LET ((number-addresses (1- (LENGTH option))))
    (UNLESS (NULL where)
	    (SETF (srr-type where)
		  (CASE (FIRST option)
			(:loose-srr ip-loose-source-and-record-route-option)
			(:strict-srr ip-strict-source-and-record-route-option)
			(:record ip-record-route-option)))
	    (UNLESS (NULL (srr-type where))
		    (SETF (srr-len where) (+ 3 (- (* number-addresses 4) 3)))
		    (SETF (srr-pointer where) 4))))) 
  

(DEFUN setup-security-option (where option)
  "2Sets up the security option as passed to transmit.*"
  (UNLESS (NULL where)
	  (SETF (security-type where) ip-security-option)
	  (SETF (security-len where) ip-security-option-size)
	  (SETF (security-security-high where) (LDB (BYTE 8 8) (SECOND option)))
	  (SETF (security-security-low where) (LDB (BYTE 8 0) (SECOND option)))
	  (SETF (security-compartments-high where) (LDB (BYTE 8 8) (THIRD option)))
	  (SETF (security-compartments-low where) (LDB (BYTE 8 0) (THIRD option)))
	  (SETF (security-handling-h where) (LDB (BYTE 8 8) (FOURTH option)))
	  (SETF (security-handling-l where) (LDB (BYTE 8 0) (FOURTH option)))
	  (SETF (security-tcc-high where) (LDB (BYTE 8 16) (FIFTH option)))
	  (SETF (security-tcc-mid where) (LDB (BYTE 8 8) (FIFTH option)))
	  (SETF (security-tcc-lsb where) (LDB (BYTE 8 0) (FIFTH option)))))


(DEFUN modify-header-options (ip-pkt &aux options header-length)
  "2NOP all options which are not copied into fragments other than fragment zero.*"
  (WHEN (> (SETF header-length (ip-header-length ip-pkt)) 4)
    (SETF options (make-ip-option-data ip-pkt *min-header-bytes*
				       (- (* 4 header-length) *min-header-bytes*)))
    (DO ((start 0 (+ start length))
	 copied-flag
	 number
	 length)
	((OR (ZEROP (AREF options start)) (>= start (ARRAY-LENGTH options))))
      (SETF copied-flag (LDB ip-option-type-copied-flag (AREF options start)))
      (SETF number (LDB ip-option-type-number (AREF options start)))
      (IF (> number ip-no-operation-option)
	  (SETF length (AREF options (1+ start)))
	  (SETF length 1))
      (WHEN (ZEROP copied-flag)
	(DOTIMES (i length) (SETF (AREF options (+ start i)) ip-no-operation-option))))
    (free-ip-option-data options))) 
    

;1;*
;1; utility functions*
;1;*

(DEFUN closest-local-address (destination-address)
  "2Address of this host with the shortest routing path to the given host*"
  (LET ((returned-addr (FIRST my-addresses)) first-hop-rte)
    (CONDITION-CASE ()
	(PROGN
	  (SETF first-hop-rte (get-routing-entry destination-address))
	  (WHEN (NOT (EQ (ip-routing-address first-hop-rte) :direct))
		(SETF first-hop-rte (get-routing-entry (ip-routing-address first-hop-rte))))
	  (DOLIST (a my-addresses)
		  (WHEN (EQUAL (get-routing-entry a) first-hop-rte)
			(SETF returned-addr a))))
      (incomplete-routing-table))
    returned-addr))


;1;;==============================================*
;1;;*
;1;; BYTES PER ELEMENT*
;1;;*
;1;; This accepts a "thing" that should be an :ART-8B or :ART-16B*
;1;; array.  It returns the number of bytes in each element.*
;1;; If the "thing" is not one of the above than nil is returned.*
;1;;*

(DEFUN bytes-per-element (thing)
  "2Number of bytes in thing, or if thing is an array, each element of thing.*"
  (IF (ARRAYP thing)
      (CASE (SECOND (ARRAY-ELEMENT-TYPE thing))
	    (256 1)
	    (65536 2)
	    (:otherwise nil))
      (CASE thing
	    (:art-8b 1)
	    (:art-16b 2)
	    (:otherwise nil)))) 

;1;;===========================================*
;1;;*
;1;; IP ONES COMPLEMENT CHECKSUM *
;1;;*
;1;; Calls the appropriate function for calculating the checksum.*
;1;;   IP-ONES-COMPLEMENT-CHECKSUM-8 for art-8b arrays or*
;1;;   IP-ONES-COMPLEMENT-CHECKSUM-16 for art-16v arrays.*
;1;; If the array is neither than it returns with an FERROR.*
;1;;*

(DEFUN ip-ones-complement-checksum (array &optional
				    (ARRAY-LENGTH (1- (LENGTH array)))
				    &rest parm-list)
  ;1; Calculate the sum of the data array elements.*
  ;1;*
  (LET ((sum 0)
	(suml 0)
	(sumr 0)
	(offset 0)
	(element-size (array-element-size array)))
;1;    (PUSH (LIST (si:array-element-size array) array-length (ARRAY-INDIRECT-P array)) *checksum-action*)*
    
    (WHEN (ARRAY-INDIRECT-P array) ;1; Most of the time array is NOT indirected*
      (SETF array-length (* array-length (ARRAY-ELEMENT-SIZE array)))
      (DO ((off (si::array-index-offset array) (si::array-index-offset array)))
	  ((NOT off))
	(INCF offset (* element-size off))
	(SETQ array (si::array-indirect-to array)
	      element-size (ARRAY-ELEMENT-SIZE array)))
      (CASE element-size
	(16
	 (UNLESS (ZEROP (MOD offset 16))
	   (FERROR () "Can't checksum word array if offset not on byte boundary"))
	 (SETF offset (FLOOR offset 16))
	 (UNLESS (ZEROP (PROG1
			  (MOD array-length 16)
			  (SETF array-length (FLOOR array-length 16))))
	   (SETF sum (LDB (BYTE 8 0) (AREF array (+ offset array-length))))))
	(8 (SETF array-length (CEILING array-length 8))
	   (SETF offset (CEILING offset 8)))))
    
    (CASE element-size
      (16
       (LOOP for index from offset below (+ offset array-length) do (INCF sum (AREF array index)))
       (SETQ sum (+ (ASH sum -8) (ASH (LDB (BYTE 8 0) sum) 8))))
      (8
       (LOOP for index from offset below (+ offset -1 array-length) by 2
	     do (INCF suml (AREF array index))
	     do (INCF sumr (AREF array (1+ index))))
       (SETQ sum (+ sumr (ASH suml 8)))
       (IF (ODDP array-length)
	   (INCF sum (ASH (AREF array (+ offset -1 array-length)) 8))))
      (t (FERROR () "Can't checksum if not byte or word elements")))
    ;1; Add to the array sum any other integers passed.*
    ;1;*
    (DOLIST (parm parm-list)
      (INCF sum parm))
    (LOOP until (< sum #x10000)
	  do (SETF sum (+ (LOGAND #xffff sum) (ASH sum -16))))
    ;1; Return the ones COMPLEMENT of the sixteen bit number.  If the checksum is 0 (#XFFFF*
    ;1; before the LOGXOR), return >FFFF. *
    ;1;*
    (IF (= #xffff sum)
	(VALUES #xffff)
      (VALUES (LOGXOR #xffff sum)))))

;1;;============================================*
;1;;*
;1;; IP ONES COMPLEMENT CHECKSUM 16*
;1;;*

(DEFUN ip-ones-complement-checksum-16 (sixteen-bit-array length &rest parm-list &aux (sum 0))
  "2Calculates the ones complement checksum for an art-16b array.*"
  ;1;Calculate the sum of the data array elements.*
  (LOOP for index from 0 below length do
	(INCF sum (net::swap-bytes (AREF sixteen-bit-array index)))
	;1;*	1do (SETF sum (+ (LOGAND #xffff sum) (ASH sum -16)))*
	)
  ;1;Add to the array sum any other integers passed.*
  (DOLIST (parm parm-list)
	  (INCF sum parm))
  (LOOP until (< sum 65536) do (SETF sum (+ (LOGAND 65535 sum) (ASH sum -16))))
  (LOGXOR 65535 sum)) 

;1;;============================================*
;1;;*
;1;; IP ONES COMPLEMENT CHECKSUM 8*
;1;;*

(DEFUN ip-ones-complement-checksum-8 (eight-bit-array length &rest parm-list &aux (sum 0))
  "2Calculates the ones complement checksum for an art-8b array.*"
  ;1; calculate the sum of the data array elements.*
  (DO ((index 0 (+ index 2)))
      ((>= index length))
    (INCF sum
	  (+ (ASH (AREF eight-bit-array index) 8)
	     (IF (>= (1+ index) length)
		 0
		 (AREF eight-bit-array (1+ index)))))
    (SETF sum (+ (LOGAND 65535 sum) (ASH sum -16))))
  ;1; add to the array sum any other integers passed.*
  (DOLIST (parm parm-list)
	  (INCF sum parm)
	  (LOOP until (< sum 65536) do (SETF sum (+ (LOGAND 65535 sum) (ASH sum -16)))))
  (LOGXOR 65535 sum)) 

;1;;=============================================*

(DEFSIGNAL ip-address-not-specified system:network-error () "No assigned IP address for Host") 
;1;;*
;1;; GET IP ADDRESSES*
;1;;*

(DEFUN get-ip-addresses (&optional (host "LM"))
  "2Get the IP address for this host.*"
  (UNLESS (TYPEP host 'net:host)
    (SETF host (IF (STRING-EQUAL host "lm")
		   si:local-host
		   (si:parse-host host))))
  (COND ((SEND host :network-address-list :ip))
	((FERROR 'ip-address-not-specified "No assigned IP address for Host ~A." host)))) 
  
;1;;=============================================*
;1;;*
;1;; GET IP ADDRESS*
;1;;*

(DEFUN get-ip-address (&optional (host "LM") (which 0))
  "2Get the Nth ip address from the addresses available.
    If host is a number the number is returned.*"
  (LET ((addrses (get-ip-addresses host)))
    (IF (>= (1- (LENGTH addrses)) which)
	(NTH which (get-ip-addresses host))
	())))


;1;;========================================*
;1;;*
;1;; IP GET HOST FROM ADDRESS Function*
;1;;*
;1;; Gets the host name from SI:HOST-ALIST otherwise*
;1;;   return nil.*
;1;;*

(DEFUN ip-get-host-from-address (address)
  "2Returns the host name from site information based on the IP address.*"
  (si:get-host-from-address address :ip)) 
 

;1;;===========================================*
;1;;*
;1;; PRINT HEADER of an IP packet.*
;1;;*

(DEFUN print-header (ip-pkt)
  "2Prints the header of an IP packet.*"
  (FORMAT t "~& Version:        ~16,4,'0r" (ip-version ip-pkt))
  (FORMAT t "   Header-Length:  ~16,4,'0r" (ip-header-length ip-pkt))
  (FORMAT t "   Precedence:     ~16,4,'0r" (ip-precedence ip-pkt))
  (FORMAT t "   Delay:          ~16,4,'0r" (ip-delay ip-pkt))
  (FORMAT t "~& Thruput:        ~16,4,'0r" (ip-thruput ip-pkt))
  (FORMAT t "   Reliability:    ~16,4,'0r" (ip-reliability ip-pkt))
  (FORMAT t "   Tos-Reserved:   ~16,4,'0r" (ip-tos-reserved ip-pkt))
  (FORMAT t "   Total-Length:   ~16,4,'0r" (ip-total-length ip-pkt))
  (FORMAT t "~& Identification: ~16,4,'0r" (ip-identification ip-pkt))
  (FORMAT t "   Flags-Reserved: ~16,4,'0r" (ip-flags-reserved ip-pkt))
  (FORMAT t "   Dont-Fragment:  ~16,4,'0r" (ip-dont-fragment ip-pkt))
  (FORMAT t "   More-Fragments: ~16,4,'0r" (ip-more-fragments ip-pkt))
  (FORMAT t "~& Fragment-Offset:~16,4,'0r" (ip-fragment-offset ip-pkt))
  (FORMAT t "   Time-To-Live:   ~16,4,'0r" (ip-time-to-live ip-pkt))
  (FORMAT t "   Protocol:       ~16,4,'0r" (ip-protocol ip-pkt))
  (FORMAT t "   Header-Checksum:~16,4,'0r" (ip-header-checksum ip-pkt))
  (FORMAT t "~& Src-Addr:       ~16,4,'0r" (ip-src-addr ip-pkt))
  (FORMAT t "   Dst-Addr:       ~16,4,'0r" (ip-dst-addr ip-pkt))) 


(COMPILE-FLAVOR-METHODS ip-handler)


(WHEN (VARIABLE-BOUNDP net::status-menu)		   ;1RLA 9/19/86*
  (net::add-to-menu net::status-menu "Print Address Translations"
		    '(:eval (ethernet::print-address-translations) :documentation
		    "Displays all known logical to Ethernet address translations on stream TERMINAL-IO"))   
  (net::add-to-menu net::status-menu "Print IP Address Translations"
		    '(:eval (ethernet::print-ip-ether-address-translations) :documentation
		    "Displays all known IP to Ethernet address translations on stream TERMINAL-IO"))
  (net::add-to-menu net::status-menu "Print CHAOS Address Translations"
		    '(:eval (ethernet::print-chaos-ether-address-translations) :documentation
		    "Displays all known CHAOS to Ethernet address translations on stream TERMINAL-IO"))) 
