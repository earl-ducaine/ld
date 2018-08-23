;; -*- Mode:Common-Lisp; Package:Ethernet; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                              RESTRICTED RIGHTS LEGEND
;;;
;;;      Use, duplication, or disclosure by the Government is subject to
;;;      restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;      Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                          TEXAS INSTRUMENTS INCORPORATED.
;;;                                   P.O. BOX 2909
;;;                               AUSTIN, TEXAS 78769
;;;                                      MS 2151

;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

;;; Datalink ethernet definitions
;;;
;;; This file contains all datalink definitions unique to Ethernet.
;;; An ETHERNET- CONTROLLER-MIXIN flavor is defined here for use in
;;; creating network controllers that operate on Ethernet hardware;
;;; this mixin is built on the DATALINK- CONTROLLER-MIXIN and
;;; therefore provides both basic Datalink and Ethernet functionality.
;;; In addition to this flavor, several Ethernet-specific functions
;;; are provided for establishing packet allocation/deallocation and
;;; registration of protocols supported by Ethernet:
;;;
;;;       ADD-PROTOCOL              --  Registers a protocol with Ethernet by 
;;;                                     establishing the protocol number and receive 
;;;                                     handler function for that protocol.
;;;       REMOVE-PROTOCOL           --  Removes a previously registered protocol 
;;;                                     from Ethernet.
;;;       ADD-NET-PACKET-ALLOCATOR  --  Registers the packet allocation and dealloc-
;;;                                     ation functions for a given protocol.
;;;
;;; As an example, to register protocol FOONET of type 133, the
;;; following is included at top-level within a FOONET file:
;;;
;;;       (ethernet:add-protocol 133 'foonet:receive-foonet-packet)
;;;       (ethernet:add-net-packet-allocator 133 'foonet:alloc-pkt 'foonet:dealloc-pkt)
;;; 
;;; This enables all Ethernet controllers to receive and transmit foonet packets.
;;;
;;; Building an Ethernet controller flavor on the
;;; ETHERNET-CONTROLLER-MIXIN provides all methods and instance
;;; variables of the DATALINK-CONTROLLER-MIXIN along with several
;;; methods and variables unique to Ethernet:
;;;
;;;       :RECEIVE-TOP-LEVEL  -- Calls the internal :RECEIVE method of the controller to
;;;                              get a packet from the hardware then passes it to the 
;;;                              receiver function registered by ADD-PROTOCOL.  This 
;;;                              occurs in the receive process of the DATALINK-
;;;                              CONTROLLER-MIXIN.
;;;       :MONITOR-LINK       -- Disables the receive process and intercepts packets 
;;;                              received by the hardware for printout to a stream.
;;;       :ETHERNET-ADDRESS   -- Returns the Ethernet Address of the controller.
;;;
;;; Both :RECEIVE-TOP-LEVEL and a transmit macro called
;;; TRANSFORMING-ARGUMENTS handle translations between protocol
;;; addresses and Ethernet addresses; this is accomplished by an
;;; internal protocol called Address Resolution Protocol or simply
;;; "ARP" (included in this file).  Note that this protocol has a type
;;; number, a receive handler, and packet allocation/deallocation
;;; function just like any other protocol (although it is only used
;;; internally).
;;;
;;; The ETHERNET-CONTROLLER-MIXIN provides instance variables for
;;; configuring multicast addresses and Ethernet hardware parameters.
;;; These instance variables are gettable and settable. Multicast
;;; addresses are stored as a list in MC-ADDR-LIST (which is initially
;;; nil) while the hardware parameters are stored in instance
;;; variables defaulted to the IEEE 802.3 spec.
;;;
;;; Maintenance History:
;;;
;;; 16 SEP 87  MMG -- Rewrite for Explorer II and Nupi/E compatibility.


;;; Ethernet definitions

(defconstant  broadcast-address      #xffffffffffff  "broadcast destination address")
(defconstant  header-size                        14  "number bytes in an ethernet header.")

(defconstant  min-frame-length                   64  "minimum byte size of ethernet frame")
(defconstant  min-data-length
	      (- min-frame-length header-size 4)     "minimum data byte size (4 bytes for crc)")
(defconstant  min-buffer-words
	      (truncate min-data-length 2)           "minimum buffer size")

(defconstant  max-frame-length                1518   "maximum byte size of ethernet frame")
(defconstant  max-data-length
	      (- max-frame-length header-size 4)     "maximum data byte size (4 bytes for crc)")
(defconstant  max-buffer-words
	      (truncate max-data-length 2)           "maximum buffer size")

(defconstant  max-mc-addr                       16   "maximum number of multicast addresses") 

(defparameter *ethernet-broadcast-address*      broadcast-address)   
(defparameter *ether-addr-length*                 6) 


;;; Ethernet functions

(defvar *ethernet-protocols* () "alist of ethernet protocol numbers & service functions.") 

(defun remove-protocol (number)
  "remove the receiver function for protocol number from the ethernet protocol list."
  (without-interrupts
    (setf *ethernet-protocols*
	  (delete number *ethernet-protocols* :test #'eq :key #'first))
    (dolist (cc net:controller-list)
      (send cc :send-if-handles :reset (send cc :enabled)))))

(defun add-protocol (number function)
  "add (or modify) a receiver function for the specified protocol number."
  (let ((element (assoc number *ethernet-protocols*)))
    (without-interrupts
      (if element
	  (setf (rest element) function)
	  (push (cons number function) *ethernet-protocols*))
      (dolist (cc net:controller-list)
	(send cc :send-if-handles :reset (send cc :enabled))))))

;;;  Packet allocation/deallocation:

(defvar *ethernet-net-packet-allocator-alist*  ()
  "an alist of protocol numbers and functions to call to allocate packets.")

(defsubst allocate-net-packet (protocol-number)
  "attempt to allocate a packet for the correct network protocol."
   (funcall
     (or (second (assoc protocol-number *ethernet-net-packet-allocator-alist*))
	 (ferror 'net::local-network-error
		 "attempt to allocate a packet; no allocator is defined for protocol type ~d"
		 protocol-number))))

(defsubst deallocate-net-packet (protocol-number packet)
  "attempt to deallocate a packet for the correct network protocol."
  (funcall
     (or (third (assoc protocol-number *ethernet-net-packet-allocator-alist*))
	 (ferror 'net::local-network-error
		 "attempt to deallocate a packet; no deallocator is defined for protocol type ~d"
		 protocol-number))	 
	 packet))

(defun add-net-packet-allocator (protocol-number allocator-fn deallocator-fn)
  "add a packet allocator and deallocator for protocol-number."
  (let ((element (assoc protocol-number *ethernet-net-packet-allocator-alist*)))
    (if element
	(setf (second element) allocator-fn
	      (third  element) deallocator-fn)
	(push (list protocol-number allocator-fn deallocator-fn)
	      *ethernet-net-packet-allocator-alist*))))

;;  Functions for reading/writing Ethernet addresses from/to packets:

(defun put-ether-address-to-array (addr array place)
  "stores addr into 16-bit array, with msb in lo-byte of element place."
  (setf (aref array place)
	(dpb (ldb (byte 8 32) addr) (byte 8 8)
	     (ldb (byte 8 40) addr)))
  (setf (aref array (+ place 1))
	(dpb (ldb (byte 8 16) addr) (byte 8 8)
	     (ldb (byte 8 24) addr)))
  (setf (aref array (+ place 2))
	(dpb (ldb (byte 8 0) addr) (byte 8 8)
	     (ldb (byte 8 8) addr)))) 

(defun get-ether-address-from-array (array place)
  "assembles ethernet address from data in array with msb in lo-byte of place."
  (let ((word1 (aref array place))
	(word2 (aref array (+ place 1)))
	(word3 (aref array (+ place 2))))
    (logior (ash (dpb (ldb (byte 8 0) word1) (byte 8 8) (ldb (byte 8 8) word1)) 32)
	    (ash (dpb (ldb (byte 8 0) word2) (byte 8 8) (ldb (byte 8 8) word2)) 16)
	    (dpb (ldb (byte 8 0) word3) (byte 8 8) (ldb (byte 8 8) word3))))) 

(defun print-ether-address-from-array-in-hex (array place)
  (format t "~16r " (ldb (byte 8 0) (aref array place)))
  (format t "~16r " (ldb (byte 8 8) (aref array place)))
  (format t "~16r " (ldb (byte 8 0) (aref array (+ place 1))))
  (format t "~16r " (ldb (byte 8 8) (aref array (+ place 1))))
  (format t "~16r " (ldb (byte 8 0) (aref array (+ place 2))))
  (format t "~16r " (ldb (byte 8 8) (aref array (+ place 2))))) 

;;; Ethernet address resolution protocol

(defconstant  address-resolution-type        1544  "address resolution protocol type code") 
(defconstant  chaos-type                    1032  "ethernet type code for chaosnet protocol")
(defconstant  *initial-cache-age*               63) 

(defparameter *ar-ethernet-hardware*        256)	   ; specifies ethernet hardware
(defparameter *ar-request*                     256)	   ; possible values for opcode
(defparameter *ar-reply*                     512) 

;; This is currently not used
(defparameter *ether-protocol-alist* 
   `((1544  "Addr Res")
     (1032  "Chaos")    
     (8     "IP")
     (2     "PUP")
     (6     "XNS")
     (264   "X.75")
     (520   "NBS IP")
     (776   "ECMA IP")
     (1288  "X.25")
     (1800  "XNS Compatibility")
     (7176  "Symbolix priv")
     (4224  "Excelan")
     (13696 "Rev Addr Res")
     (144   "Loopback"))) 


;;;  Arp packet allocation (we are borrowing Chaos int packets for now -- when
;;;  Chaosnet is removed, these will have to be replaced with functions that
;;;  allocate/deallocate generic Arp packets for use by all protocols):

(defparameter allocate-buffer 'chaos::allocate-int-pkt) 
(defparameter free-buffer     'chaos::free-int-pkt)     

(defvar address-translations-lists          () "a list of address translations lists.")
(defvar get-ethernet-address-handlers     () "an alist of (protocol-keyword handler)")
(defvar receive-addr-pkt-handlers          () "an alist of (arp-protocol-number handler)")
(defvar print-address-translations-handlers () "an alist of (protocol-keyword handler)")

(defvar *unknown-arp-requests-received*
	0  "no. arp request broadcasts received for unknown protocol") 
(defvar *unknown-arp-replies-received*
	0  "number of arp replies received for unknown protocol") 
(defvar *unknown-arp-packet-types-received*
	0  "if it is neither a request nor a reply, inc this")


;; Address resolution frame header:
(defstruct (res-frame
	     (:constructor             nil)
	     (:conc-name               ar-)
	     (:callable-constructors   nil)
	     (:alterant                alter-res-frame)
	     (:predicate               nil)
	     (:copier                  nil)
	     (:type                    :array))  
  hw-type				   ; hardware type
  protocol				   ; ether protocol being resolved
  ((lengths              518)
   (h-w-addr-length        8)		   ; = 6 for ethernet
   (protocol-addr-length 520))		   ; = 2 for chaosnet
  opcode)				   ; request or answer

;; Address Resolution Protocol receive handler:
(defun receive-addr-pkt (self array ignore
			 &optional (ether-pkt-type address-resolution-type)
			 &rest ignore)
  (unwind-protect
       (let ((handler
	      (second (assoc (ar-protocol array)
			     receive-addr-pkt-handlers :test #'eql))))
	 (if handler
	     (funcall handler self array ether-pkt-type)
	     (select (ar-opcode array)
		     (*ar-request* (incf *unknown-arp-requests-received*))
		     (*ar-reply*   (incf *unknown-arp-replies-received*))
		     (:otherwise   (incf *unknown-arp-packet-types-received*)))))
    (deallocate-net-packet ether-pkt-type array)))


;;; Register ARP as a protocol:

(add-net-packet-allocator address-resolution-type allocate-buffer free-buffer)
(add-protocol address-resolution-type 'receive-addr-pkt)


;;; Address translation functions:

(defun get-ethernet-address (address &optional (protocol :chaos))
  "top level function to call to resolve a network address for the ethernet."
  (let ((handler (second (assoc protocol get-ethernet-address-handlers))))
    (when handler
	  (funcall handler address)))) 

(defun reset-address-translations ()
  "clear out all address translations."
  (dolist (list address-translations-lists) (set (second list) nil)))

(defun print-address-translations (&optional (stream *terminal-io*))
  "prints all address translations on stream."
  (dolist (elem print-address-translations-handlers)
    (let ((handler (second elem)))
      (when handler
	(funcall handler stream)))))

;; Address/Controller mapping:

(defun map-address-to-gateway-controller (protocol address)
  "returns the controller object that will handle operations associated with the given
   address of the given protocol.  will return nil if no controller is handling packets
   for that protocol and address."
  ;; warning! assumed positional correspondence between lists.
  (loop with address-list
	= (send si:local-host :network-address-list protocol)
     for controller in net::controller-list
     for addr in address-list
     when (eql addr address) return controller))

(defun map-address-to-controller (protocol address)
  (cond ((and (boundp 'self)
	   (typep self 'ethernet-controller-mixin))
	 self)
	((null net::controller-list)
	 (ferror 'no-controller-for-address))
	(t
	 (map-address-to-gateway-controller protocol address))))

;;; Basic ethernet datalink controller 

(defflavor ethernet-controller-mixin
           (ethernet-address                       
	    ;;  framing parameters;
	    (enet-pream-len 8)   ; preamble length: can be 2, 4, 8, or 16 bytes
	    (enet-bc-dis nil)   ; disable reception of broadcast frames?
	    (enet-crc-16 nil)   ; use 16-bit crc polynomial instead of 32 bit?
	    (enet-ncrc-ins nil)   ; use no crc insertion after information field?
	    (enet-bt-stf nil)   ; use bitstuff rather than end of carrier framing?
	    (enet-pad nil)   ; pad any frames that are shorter than slot time?
	    (enet-addr-len
	      *ether-addr-length*)      ; number of address bytes
	    (enet-min-frm-len
	      min-frame-length)         ; minimum number of bytes in a frame 
	    ;;  link management parameters;
	    (enet-if-spacing      96.)  ; interframe spacing time in tclk units
	    (enet-slot-time      512.)  ; slot time in tclk units
	    (enet-retry-num       15.)  ; maximum number of retries after a collision
	    (enet-lin-prio         0.)  ; linear priority: number of slot times before xmit
	    (enet-acr              0.)  ; accelerated contention resolution priority number
	    (enet-bof-met         nil)  ; use a backoff method different from ieee 802.3?
	    ;;  serial interface parameters;
	    (enet-manch           nil)  ; use manchester instead of nrz encoding/decoding?
	    (enet-crs-src       ':ext)  ; carrier sense source - :external or :internal
	    (enet-crsf             0.)  ; width of carrier sense filter in tclk units
	    (enet-cdt-src       ':ext)  ; collision detect source - :external or :internal
	    (enet-cdtf             0.)  ; width of collision detect filter in tclk units
	    (enet-tono-crs        nil)  ; transmit on no carrier sense?
	    ;;  host interface parameters;
	    (enet-fifo-threshold   8)   ; fifo threshold
	    (enet-srdy           nil)   ; synchronous ready?
	    (enet-at-field-loc     0)   ; address/type field location
	    (enet-sav-bf         nil)   ; save bad frames in memory?
	    ;;  network management parameters;
	    (enet-int-lpbck      nil)   ; internal loopback
	    (enet-ext-lpbck      nil)   ; external loopback
	    (enet-prm            nil)   ; promiscuous mode (accept any frame?)
	    ;;  receive screening parameters;
	    (mc-addr-list    '())       ; list of multicast addresses
	    (valid-pkt-types '())       ; list of valid packet types
	    ;; statistics;
	    (collision-count                     0)
	    (ethernet-broadcast-packets-received 0))
          (net::datalink-controller-mixin)
   :gettable-instance-variables
   :inittable-instance-variables
   :settable-instance-variables
   (:required-methods :transmit :receive :initialize)
   (:documentation :essential-mixin "basic ethernet controller")) 

;;; Initializations

(defmethod (ethernet-controller-mixin :after :init) (&rest ignore)
  "adds controller meters to peek after instantiation"
  (send self :add-controller-meter
	:ethernet-broadcast-packets-received "number of ethernet broadcast pkts received")
  (send self :add-controller-meter
	:collision-count "collisions on ether"))
 
(defmethod (ethernet-controller-mixin  :init-pkt-types) (&rest ignore)
  "performs setup of valid pkt types list"
  (setf valid-pkt-types '())		   ; setup valid pkt types 
  (dolist (protocol *ethernet-protocols*)  ; from protocols list.
    (push (car protocol) valid-pkt-types))); init pkt types

(defmethod (ethernet-controller-mixin  :before :initialize) (&rest ignore)
  "performs setup of instance variables before controller initialization"
  (send self :init-pkt-types))

(defmethod (ethernet-controller-mixin  :after :reset-meters) (&rest ignore)
   "resets controller ethernet statistics"
  (send self :send-if-handles :update-stats)
  (setf ethernet-broadcast-packets-received 0
	collision-count 0))

(defmethod (ethernet-controller-mixin  :after :print-stats)
	   (&optional (stream *standard-output*))
  "print the statistics for this controller to stream."
  (format stream "~%~10d  ethernet broadcast packets received" ethernet-broadcast-packets-received)
  (format stream "~%~10d  collisions"  collision-count))


;; Datalink interface 

(defmethod (ethernet-controller-mixin  :receive-top-level) ()
  "calls receive method and passes frame to the receiver function."
  (let (receiver)
    (error-restart-loop
     ((error system:abort) "wait for next ethernet packet")
     (when (null net::enabled)
       (process-wait "controller disabled"  ; wait if controller is disabled.
		     #'(lambda (enc) (send enc :enabled)) self))	   
     (multiple-value-bind
	   (dest source type data n-bytes)
	 (send self :receive)		   ; get packet
       (when type                           ; nil if invalid packet type
	 (incf net::pkts-received)		   ; increment statistics
	 (when (eql dest broadcast-address)
	   (incf ethernet-broadcast-packets-received))
	 (when (setf receiver                 ; get receiver function
		     (cdr (assoc type *ethernet-protocols*)))
	   (funcall receiver                  ; pass to receiver function.
		    self data n-bytes type source dest)))))))

;; This macro is used by all Ethernet controller transmit methods:
(defmacro transforming-arguments (type addr data n-bytes deallocate-p
				  &body body)
  "Ensures that type, addr, & data are in ethernet format, then
     executes body. Type may be :chaos or an ethernet type code, and
     defines input addr & data format. Also does all standard ethernet
     processing, incf of counters, etc. When deallocate-p is t, the
     data buffer is deallocated before returning."
  ;; Check size against enet max.
  `(if (> ,n-bytes max-data-length)	   
       (ferror 'net::local-network-error  
	       "attempt to send a frame larger than legal ethernet maximum, n-bytes = ~d."
	       ,n-bytes)
       (cond ((equal ,type :chaos)
	      (setf ,type chaos-type))
	     ((equal ,type :ether)
	      (setf ,type  address-resolution-type)))
       (unwind-protect
	    (when net::enabled
	      (block ()
		(when (eql ,type chaos-type)
		  (if (null (setf ,addr (get-ethernet-address ,addr)))
		      ;; chaosnet should be responsible for this!
		      (return nil)))	   
		(setf ,n-bytes (max ,n-bytes min-data-length))
		,@body
		(incf net::pkts-transmitted)))
	 (when ,deallocate-p		   ; 12 jun 87  mmg
	   (deallocate-net-packet ,type ,data)))))

;;; Diagnostics

(Defmethod (ETHERNET-CONTROLLER-MIXIN  :MONITOR-LINK)
	   (&Optional (Stream *Terminal-Io*) (Print-Data-P T))	   
  "Print Ethernet frames as received on this link.  
   Disables normal reception on Ethernet while running."
  (When Print-Data-P
    (Format t "~%Type Control-~C to quit.~&" #\Abort))
  (Unwind-Protect
      (Progn                               ; Gracefully halt receive process:
	(Setf Net::Enabled Nil)	           ; Put controller in disabled state
	(Process-Reset-And-Enable
	  (Send Self :Receiver-Process))   ; Throw into wait state by resetting process and giving it
	(Process-Allow-Schedule)	   ; a chance to stop in a known place.
	(Loop
	  (Multiple-Value-Bind (Dest Src Type Data Nbytes)
	      (Send Self :Receive)	   ; Call bottom level receive function.
	    (Unwind-Protect
		(Progn			   ; Print out the packet
		  (Format Stream "~2%D=~16,12,48r S=~16,12,48r T=~16,4,48r Number bytes = ~d."
			  Dest Src Type Nbytes)
		  (When Print-Data-P
		    (Do ((Bytes 0 (+ 2 Bytes))
			 (Index 0 (1+ Index)))
			((>= Bytes Nbytes))
		      (When (Zerop (Ldb (Byte 3 0) Index))
			(Format Stream "~% ~16,3,48r:" Bytes))
		      (Format Stream " ~16,4,48r" (Aref Data Index)))))
	      (When Data		   ; Return the packet
		(Deallocate-Net-Packet Type Data))))))
    (Send Self :Enable)))

(defun monitor-ethernet ()
  (send (net::select-controller) :monitor-link))
