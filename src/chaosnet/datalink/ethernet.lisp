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

;;;      Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

;;--------------------------------------------------------------------------------------
;;                             DATALINK ETHERNET DEFINITIONS
;;--------------------------------------------------------------------------------------

;; This file contains all datalink definitions unique to Ethernet.  An ETHERNET-
;; CONTROLLER-MIXIN flavor is defined here for use in creating network controllers 
;; that operate on Ethernet hardware; this mixin is built on the DATALINK-
;; CONTROLLER-MIXIN and therefore provides both basic Datalink and Ethernet 
;; functionality.  In addition to this flavor, several Ethernet-specific
;; functions are provided for establishing packet allocation/deallocation and 
;; registration of protocols supported by Ethernet:
;;
;;       ADD-PROTOCOL              --  Registers a protocol with Ethernet by 
;;                                     establishing the protocol number and receive 
;;                                     handler function for that protocol.
;;       REMOVE-PROTOCOL           --  Removes a previously registered protocol 
;;                                     from Ethernet.
;;       ADD-NET-PACKET-ALLOCATOR  --  Registers the packet allocation and dealloc-
;;                                     ation functions for a given protocol.
;;
;; As an example, to register protocol FOONET of type 133, the following is included 
;; at top-level within a FOONET file:
;;
;;       (ETHERNET:ADD-PROTOCOL 133 'FOONET:RECEIVE-FOONET-PACKET)
;;       (ETHERNET:ADD-NET-PACKET-ALLOCATOR 133 'FOONET:ALLOC-PKT 'FOONET:DEALLOC-PKT)
;; 
;; This enables all Ethernet controllers to receive and transmit FOONET packets.
;;
;; Building an Ethernet controller flavor on the ETHERNET-CONTROLLER-MIXIN provides 
;; all methods and instance variables of the DATALINK-CONTROLLER-MIXIN along with 
;; several methods and variables unique to Ethernet:
;;
;;       :RECEIVE-TOP-LEVEL  -- Calls the internal :RECEIVE method of the controller to
;;                              get a packet from the hardware then passes it to the 
;;                              receiver function registered by ADD-PROTOCOL.  This 
;;                              occurs in the receive process of the DATALINK-
;;                              CONTROLLER-MIXIN.
;;       :MONITOR-LINK       -- Disables the receive process and intercepts packets 
;;                              received by the hardware for printout to a stream.
;;       :ETHERNET-ADDRESS   -- Returns the Ethernet Address of the controller.
;;
;; Both :RECEIVE-TOP-LEVEL and a transmit macro called TRANSFORMING-ARGUMENTS handle 
;; translations between protocol addresses and Ethernet addresses; this is
;; accomplished by an internal protocol called Address Resolution Protocol or simply 
;; "ARP" (included in this file).  Note that this protocol has a type number, a receive
;; handler, and packet allocation/deallocation function just like any other protocol 
;; (although it is only used internally).
;;
;; The ETHERNET-CONTROLLER-MIXIN provides instance variables for configuring multicast
;; addresses and Ethernet hardware parameters.  These instance variables are gettable 
;; and settable. Multicast addresses are stored as a list in MC-ADDR-LIST (which is 
;; initially nil) while the hardware parameters are stored in instance variables defaulted 
;; to the IEEE 802.3 spec.
;;
;;  Maintenance History:
;;
;;  16 SEP 87  MMG -- Rewrite for Explorer II and Nupi/E compatibility.

;;--------------------------------------------------------------------------------------
;;                                ETHERNET DEFINITIONS
;;--------------------------------------------------------------------------------------

(Defconstant  BROADCAST-ADDRESS      #xFFFFFFFFFFFF  "Broadcast destination address")
(Defconstant  HEADER-SIZE                        14  "Number bytes in an Ethernet header.")

(Defconstant  MIN-FRAME-LENGTH                   64  "Minimum byte size of Ethernet Frame")
(Defconstant  MIN-DATA-LENGTH
	      (- Min-Frame-Length Header-Size 4)     "Minimum data byte size (4 bytes for CRC)")
(Defconstant  MIN-BUFFER-WORDS
	      (Truncate Min-Data-Length 2)           "Minimum buffer size")

(Defconstant  MAX-FRAME-LENGTH                1518   "Maximum byte size of Ethernet Frame")
(Defconstant  MAX-DATA-LENGTH
	      (- Max-Frame-Length Header-Size 4)     "Maximum data byte size (4 bytes for CRC)")
(Defconstant  MAX-BUFFER-WORDS
	      (Truncate Max-Data-Length 2)           "Maximum buffer size")

(Defconstant  MAX-MC-ADDR                       16   "Maximum number of Multicast addresses") 

(Defparameter *ETHERNET-BROADCAST-ADDRESS*      BROADCAST-ADDRESS)   
(Defparameter *ETHER-ADDR-LENGTH*                 6) 


;;--------------------------------------------------------------------------------------
;;                                  ETHERNET FUNCTIONS
;;--------------------------------------------------------------------------------------

(Defvar *ETHERNET-PROTOCOLS* () "Alist of Ethernet protocol numbers & service functions.") 

(Defun REMOVE-PROTOCOL (Number)
  "Remove the receiver function for protocol number from the ethernet protocol list."
  (Without-Interrupts
    (Setf *Ethernet-Protocols*
	  (Delete Number *Ethernet-Protocols* :Test #'Eq :Key #'First))
    (dolist (cc net:controller-list)
      (SEND cc :send-if-handles :reset (SEND cc :enabled)))))

(Defun ADD-PROTOCOL (Number Function)
  "Add (or modify) a receiver FUNCTION for the specified protocol NUMBER."
  (Let ((Element (Assoc Number *Ethernet-Protocols*)))
    (Without-Interrupts
      (If Element
	  (Setf (Rest Element) Function)
	  (Push (Cons Number Function) *Ethernet-Protocols*))
      (dolist (cc net:controller-list)
	(SEND cc :send-if-handles :reset (SEND cc :enabled))))))

;;----------------------------------------------------------------------------------------
;;  Packet allocation/deallocation:

(Defvar *ETHERNET-NET-PACKET-ALLOCATOR-ALIST*  ()
  "An alist of protocol numbers and functions to call to allocate packets.")

(Defsubst ALLOCATE-NET-PACKET (Protocol-Number)
  "Attempt to allocate a packet for the correct network protocol."
   (Funcall
     (Or (Second (Assoc Protocol-Number *Ethernet-Net-Packet-Allocator-Alist*))
	 (Ferror 'Net::Local-Network-Error
		 "Attempt to allocate a packet; no allocator is defined for protocol type ~d"
		 Protocol-Number))))

(Defsubst DEALLOCATE-NET-PACKET (Protocol-Number Packet)
  "Attempt to deallocate a packet for the correct network protocol."
  (Funcall
     (Or (Third (Assoc Protocol-Number *Ethernet-Net-Packet-Allocator-Alist*))
	 (Ferror 'Net::Local-Network-Error
		 "Attempt to deallocate a packet; no deallocator is defined for protocol type ~d"
		 Protocol-Number))	 
	 Packet))

(Defun ADD-NET-PACKET-ALLOCATOR (Protocol-Number Allocator-Fn Deallocator-Fn)
  "Add a packet allocator and deallocator for protocol-number."
  (Let ((Element (Assoc Protocol-Number *Ethernet-Net-Packet-Allocator-Alist*)))
    (If Element
	(Setf (Second Element) Allocator-Fn
	      (Third  Element) Deallocator-Fn)
	(Push (List Protocol-Number Allocator-Fn Deallocator-Fn)
	      *Ethernet-Net-Packet-Allocator-Alist*)
	)				   ; if
    )					   ; let
  )					   ; add-net-packet-allocator

;;-----------------------------------------------------------------------------------------
;;  Functions for reading/writing Ethernet addresses from/to packets:

(Defun PUT-ETHER-ADDRESS-TO-ARRAY (Addr Array Place)
  "Stores ADDR into 16-bit ARRAY, with MSB in lo-byte of element PLACE."
  (Setf (Aref array place)
	(Dpb (Ldb (byte 8 32) addr) (byte 8 8)
	     (Ldb (byte 8 40) addr)))
  (Setf (Aref array (+ place 1))
	(Dpb (Ldb (byte 8 16) addr) (byte 8 8)
	     (Ldb (byte 8 24) addr)))
  (Setf (Aref array (+ place 2))
	(Dpb (Ldb (byte 8 0) addr) (byte 8 8)
	     (Ldb (byte 8 8) addr)))) 

(Defun GET-ETHER-ADDRESS-FROM-ARRAY (Array Place)
  "Assembles Ethernet address from data in ARRAY with MSB in lo-byte of PLACE."
  (Let ((Word1 (aref array place))
	(Word2 (aref array (+ place 1)))
	(Word3 (aref array (+ place 2))))
    (Logior (Ash (Dpb (ldb (byte 8 0) word1) (byte 8 8) (ldb (byte 8 8) word1)) 32)
	    (Ash (Dpb (ldb (byte 8 0) word2) (byte 8 8) (ldb (byte 8 8) word2)) 16)
	    (Dpb (Ldb (byte 8 0) word3) (byte 8 8) (ldb (byte 8 8) word3))))) 

(Defun PRINT-ETHER-ADDRESS-FROM-ARRAY-IN-HEX (Array Place)
  (format t "~16r " (ldb (byte 8 0) (aref array place)))
  (format t "~16r " (ldb (byte 8 8) (aref array place)))
  (format t "~16r " (ldb (byte 8 0) (aref array (+ place 1))))
  (format t "~16r " (ldb (byte 8 8) (aref array (+ place 1))))
  (format t "~16r " (ldb (byte 8 0) (aref array (+ place 2))))
  (format t "~16r " (ldb (byte 8 8) (aref array (+ place 2))))) 


;;------------------------------------------------------------------------------------------
;;                           ETHERNET ADDRESS RESOLUTION PROTOCOL
;;------------------------------------------------------------------------------------------

(Defconstant  ADDRESS-RESOLUTION-TYPE        1544  "Address Resolution protocol type code") 
(Defconstant  CHAOS-TYPE                    1032  "Ethernet type code for Chaosnet protocol")
(Defconstant  *INITIAL-CACHE-AGE*               63) 

(Defparameter *AR-ETHERNET-HARDWARE*        256)	   ; specifies Ethernet hardware
(Defparameter *AR-REQUEST*                     256)	   ; possible values for OPCODE
(Defparameter *AR-REPLY*                     512) 

(Defparameter *ETHER-PROTOCOL-ALIST* ; this is currently not used
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

;;  Arp packet allocation (we are borrowing Chaos int packets for now -- when
;;  Chaosnet is removed, these will have to be replaced with functions that
;;  allocate/deallocate generic Arp packets for use by all protocols):

(Defparameter ALLOCATE-BUFFER 'CHAOS::ALLOCATE-INT-PKT) 
(Defparameter FREE-BUFFER     'CHAOS::FREE-INT-PKT)     

(Defvar ADDRESS-TRANSLATIONS-LISTS          () "A list of address translations lists.")
(Defvar GET-ETHERNET-ADDRESS-HANDLERS     () "An alist of (protocol-keyword handler)")
(Defvar RECEIVE-ADDR-PKT-HANDLERS          () "An alist of (arp-protocol-number handler)")
(Defvar PRINT-ADDRESS-TRANSLATIONS-HANDLERS () "An alist of (protocol-keyword handler)")

(Defvar *UNKNOWN-ARP-REQUESTS-RECEIVED*
	0  "No. ARP Request Broadcasts Received for Unknown Protocol") 
(Defvar *UNKNOWN-ARP-REPLIES-RECEIVED*
	0  "Number of ARP Replies Received for Unknown Protocol") 
(Defvar *UNKNOWN-ARP-PACKET-TYPES-RECEIVED*
	0  "If it is neither a request nor a reply, inc this")

;;----------------------------------------------------------------------------------------
;; Address resolution frame header:

(Defstruct (RES-FRAME
	     (:Constructor             Nil)
	     (:Conc-name               Ar-)
	     (:Callable-Constructors   Nil)
	     (:Alterant                Alter-Res-Frame)
	     (:Predicate               Nil)
	     (:Copier                  Nil)
	     (:Type                    :Array))  
  Hw-Type				   ; Hardware type
  Protocol				   ; Ether protocol being resolved
  ((Lengths              518)
   (H-W-Addr-Length        8)		   ; = 6 for Ethernet
   (Protocol-Addr-Length 520))		   ; = 2 for Chaosnet
  Opcode)				   ; Request or Answer

;;-----------------------------------------------------------------------------------------
;; Address Resolution Protocol receive handler:

(Defun RECEIVE-ADDR-PKT (Self Array Ignore
		        &Optional (Ether-Pkt-Type Address-Resolution-Type)
			&Rest Ignore)
  (Unwind-Protect
      (Let ((Handler
	      (Second (Assoc (Ar-Protocol Array)
			     Receive-Addr-Pkt-Handlers :Test #'EQL))))
	(If Handler
	    (Funcall Handler Self Array Ether-Pkt-Type)
	    (Select (Ar-Opcode Array)
	      (*Ar-Request* (Incf *Unknown-Arp-Requests-Received*))
	      (*Ar-Reply*   (Incf *Unknown-Arp-Replies-Received*))
	      (:Otherwise   (Incf *Unknown-Arp-Packet-Types-Received*))
	      )				   ; select
	    )				   ; if
	)				   ; let
    (Deallocate-Net-Packet Ether-Pkt-Type Array)
    )					   ; unwind-protect
  )					   ; receive-addr-packet

;; Register ARP as a protocol:

(Add-Net-Packet-Allocator ADDRESS-RESOLUTION-TYPE Allocate-Buffer Free-Buffer)
(Add-Protocol Address-Resolution-Type 'Receive-Addr-Pkt)



;;-----------------------------------------------------------------------------------------
;; Address translation functions:

(Defun GET-ETHERNET-ADDRESS (Address &Optional (Protocol :Chaos))
  "Top level function to call to resolve a network address for the ethernet."
  (Let ((Handler (Second (Assoc Protocol Get-Ethernet-Address-Handlers))))
    (When Handler
	  (Funcall Handler Address)))) 

(Defun RESET-ADDRESS-TRANSLATIONS ()
  "Clear out all address translations."
  (Dolist (List Address-Translations-Lists) (Set (Second List) Nil)))

(Defun PRINT-ADDRESS-TRANSLATIONS (&optional (Stream *Terminal-Io*))
  "Prints all address translations on stream."
  (Dolist (Elem Print-Address-Translations-Handlers)
    (Let ((Handler (Second Elem)))
      (When Handler
	(Funcall Handler Stream)))))


;;-----------------------------------------------------------------------------------------
;; Address/Controller mapping:

(Defun MAP-ADDRESS-TO-GATEWAY-CONTROLLER (Protocol Address)
  "Returns the controller object that will handle operations associated with the given
   address of the given protocol.  Will return nil if no controller is handling packets
   for that protocol and address."

  ;; WARNING! Assumed positional correspondence between lists.
  (Loop With Address-list
	= (Send Si:Local-Host :Network-Address-List Protocol)
     For Controller In Net::Controller-List
     For Addr In Address-List
     When (Eql Addr Address) Return Controller))
 

(Defun MAP-ADDRESS-TO-CONTROLLER (Protocol Address)
  (Cond ((And (Boundp 'Self)
	   (Typep Self 'Ethernet-Controller-Mixin))
	 Self)
	((Null Net::Controller-List)
	 (Ferror 'No-Controller-For-Address))
	(T
	 (Map-Address-To-Gateway-Controller Protocol Address))))


;;------------------------------------------------------------------------------------------
;;                             BASIC ETHERNET DATALINK CONTROLLER 
;;------------------------------------------------------------------------------------------

(Defflavor ETHERNET-CONTROLLER-MIXIN
           (Ethernet-Address                       
	   
	    ;;  Framing Parameters;
	    
	    (Enet-Pream-Len        8)   ; Preamble Length: can be 2, 4, 8, or 16 bytes
	    (Enet-Bc-Dis         NIL)   ; Disable reception of broadcast frames?
	    (Enet-Crc-16         NIL)   ; Use 16-bit CRC polynomial instead of 32 bit?
	    (Enet-Ncrc-Ins       NIL)   ; Use no CRC insertion after information field?
	    (Enet-Bt-Stf         NIL)   ; Use bitstuff rather than End of Carrier framing?
	    (Enet-Pad            NIL)   ; Pad any frames that are shorter than slot time?
	    (Enet-Addr-Len
	      *Ether-Addr-Length*)      ; Number of address bytes
	    (Enet-Min-Frm-Len
	      MIN-FRAME-LENGTH)         ; Minimum number of bytes in a frame 
	    
	    ;;  Link Management Parameters;
	    
	    (Enet-If-Spacing      96.)  ; Interframe spacing time in TCLK units
	    (Enet-Slot-Time      512.)  ; Slot time in TCLK units
	    (Enet-Retry-Num       15.)  ; Maximum number of retries after a collision
	    (Enet-Lin-Prio         0.)  ; Linear Priority: number of slot times before xmit
	    (Enet-Acr              0.)  ; Accelerated contention resolution priority number
	    (Enet-Bof-Met         NIL)  ; Use a backoff method different from IEEE 802.3?
	    
	    ;;  Serial Interface Parameters;
	    
	    (Enet-Manch           NIL)  ; Use Manchester instead of NRZ encoding/decoding?
	    (Enet-Crs-Src       ':EXT)  ; Carrier sense source - :EXTernal or :INTernal
	    (Enet-Crsf             0.)  ; Width of Carrier Sense filter in TCLK units
	    (Enet-Cdt-Src       ':EXT)  ; Collision detect source - :EXTernal or :INTernal
	    (Enet-Cdtf             0.)  ; Width of Collision detect filter in TCLK units
	    (Enet-Tono-Crs        NIL)  ; Transmit on no carrier sense?
	    
	    ;;  Host Interface Parameters;

	    (Enet-Fifo-Threshold   8)   ; Fifo Threshold
	    (Enet-Srdy           NIL)   ; Synchronous Ready?
	    (Enet-At-Field-Loc     0)   ; Address/Type field location
	    (Enet-Sav-Bf         NIL)   ; Save bad frames in memory?
	    
	    ;;  Network Management Parameters;
	    
	    (Enet-Int-Lpbck      NIL)   ; Internal loopback
	    (Enet-Ext-Lpbck      NIL)   ; External loopback
	    (Enet-Prm            NIL)   ; Promiscuous mode (accept any frame?)

	    ;;  Receive Screening Parameters;
	    
	    (Mc-Addr-List    '())       ; List of Multicast Addresses
	    (Valid-Pkt-Types '())       ; List of valid packet types

	    ;; Statistics;
	    
	    (Collision-Count                     0)
	    (Ethernet-Broadcast-Packets-Received 0))
          (Net::Datalink-Controller-Mixin)
   :Gettable-Instance-Variables
   :Inittable-Instance-Variables
   :Settable-Instance-Variables
   (:Required-methods :Transmit :Receive :Initialize)
   (:Documentation :Essential-Mixin "Basic Ethernet Controller")) 


;;------------------------------------------------------------------------------------------
;;                                    INITIALIZATIONS
;;------------------------------------------------------------------------------------------

(Defmethod (ETHERNET-CONTROLLER-MIXIN :AFTER :INIT) (&Rest Ignore)
  "Adds controller meters to peek after instantiation"
  (Send Self :Add-Controller-Meter
	:Ethernet-Broadcast-Packets-Received "Number of Ethernet broadcast pkts received")
  (Send Self :Add-Controller-Meter
	:Collision-Count "Collisions on ether")
  )					   ; AFTER INIT

 
(Defmethod (ETHERNET-CONTROLLER-MIXIN  :INIT-PKT-TYPES) (&Rest Ignore)
  "Performs setup of Valid Pkt Types list"
  (Setf Valid-Pkt-Types '())		   ; Setup valid pkt types 
  (Dolist (Protocol *Ethernet-Protocols*)  ; from protocols list.
    (Push (Car Protocol) Valid-Pkt-Types))
  )					   ; Init Pkt Types

(Defmethod (ETHERNET-CONTROLLER-MIXIN  :BEFORE :INITIALIZE) (&Rest Ignore)
  "Performs setup of instance variables before controller initialization"
  (Send Self :Init-Pkt-Types)
  )					   ; BEFORE INITIALIZE


(Defmethod (ETHERNET-CONTROLLER-MIXIN  :AFTER :RESET-METERS) (&Rest Ignore)
   "Resets controller Ethernet statistics"
  (Send Self :Send-If-Handles :Update-Stats)
  (Setf Ethernet-Broadcast-Packets-Received 0
	Collision-Count                     0)
  )					   ; AFTER RESET-METERS

(Defmethod (ETHERNET-CONTROLLER-MIXIN  :AFTER :PRINT-STATS)
	   (&Optional (Stream *Standard-Output*))
  "Print the statistics for this controller to STREAM."
  (Format stream "~%~10D  Ethernet Broadcast Packets Received" Ethernet-Broadcast-Packets-Received)
  (Format stream "~%~10D  Collisions"  Collision-Count) 
  )					   ; AFTER PRINT-STATS


;;------------------------------------------------------------------------------------------
;;                                    DATALINK INTERFACE 
;;------------------------------------------------------------------------------------------

(Defmethod (ETHERNET-CONTROLLER-MIXIN  :RECEIVE-TOP-LEVEL) (&Aux Receiver)
  "Calls receive method and passes frame to the receiver function."
  (Error-Restart-Loop
    ((Error System:Abort) "Wait for next Ethernet Packet")
    (When (Null Net::Enabled)
      (Process-Wait "Controller Disabled"  ; Wait if controller is disabled.
		    #'(Lambda (Enc) (Send Enc :Enabled)) Self))	   
    (Multiple-Value-Bind
      (Dest Source Type Data N-Bytes)
	(Send Self :Receive)		   ; Get packet
      (When type                           ; NIL if invalid packet type
	(Incf Net::Pkts-Received)		   ; Increment statistics
	(When (Eql Dest BROADCAST-ADDRESS)
	  (Incf Ethernet-Broadcast-Packets-Received))
	(When (Setf Receiver                 ; Get receiver function
		  (Cdr (Assoc Type *Ethernet-Protocols*)))
	  (Funcall Receiver                  ; Pass to receiver function.
		 Self Data N-Bytes Type Source Dest)))
      )					   ; multiple-value-bind
    )					   ; error-restart-loop
  )					   ; receive-top-level


;; This macro is used by all Ethernet controller Transmit methods:

(Defmacro TRANSFORMING-ARGUMENTS (Type Addr Data N-bytes Deallocate-P
				  &Body Body)
  "Ensures that TYPE, ADDR, & DATA are in Ethernet format, then executes BODY.
   TYPE may be :CHAOS or an Ethernet type code, and defines input ADDR & DATA format.
   Also does all Standard Ethernet processing, incf of counters, etc.
   When DEALLOCATE-P is T, the DATA buffer is deallocated before returning."
  
  `(If (> ,N-Bytes MAX-DATA-LENGTH)	   ; Check size against Enet max.
       (Ferror 'Net::Local-Network-Error  
	       "Attempt To Send a Frame Larger Than Legal Ethernet Maximum, N-bytes = ~D."
	       ,N-Bytes)
       (Cond ((Equal ,Type :CHAOS)
	      (Setf ,Type CHAOS-TYPE))
	     ((Equal ,Type :ETHER)
	      (Setf ,Type  ADDRESS-RESOLUTION-TYPE)))
       (Unwind-protect
	   (When Net::Enabled
	     (Block ()
	       (When (Eql ,Type CHAOS-TYPE)
		 (If (Null (Setf ,Addr (Get-Ethernet-Address ,Addr)))
		     (Return Nil)))	   ; Chaosnet should be responsible for this!
	       (Setf ,N-bytes (Max ,N-bytes MIN-DATA-LENGTH))
	       ,@Body
	       (Incf Net::Pkts-Transmitted)
	       )			   ; Block
	     )				   ; when enabled
	 (When ,Deallocate-P		   ; 12 JUN 87  MMG
	   (Deallocate-Net-Packet ,Type ,Data))	   
	 )				   ; unwind-protect
       )				   ; if
  )					   ; transforming-arguments

 
;;------------------------------------------------------------------------------------------
;;                                       DIAGNOSTICS
;;------------------------------------------------------------------------------------------

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
		(Deallocate-Net-Packet Type Data)
		)			   ; when data
	      )				   ; unwind-protect packet
	    )				   ; mult value bind
	  )				   ; loop
	)				   ; progn	
    (Send Self :Enable)
    )					   ; unwind-protect process
  )					   ; monitor-ethernet


(Defun MONITOR-ETHERNET ()
  (Send (Net::Select-Controller) :Monitor-Link))