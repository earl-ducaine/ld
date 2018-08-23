;;       -*- Mode: COMMON-LISP; Package: ETHERNET; Base:10; Fonts:(cptfont); -*-


;;;                                  RESTRICTED RIGHTS LEGEND
;;;
;;;      Use, duplication, or disclosure by the Government is subject to restrictions as
;;;      set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer
;;;      Software clause at 52.227-7013.
;;;
;;;                               TEXAS INSTRUMENTS INCORPORATED  
;;;                                       P.O. BOX 2909  
;;;                                   AUSTIN, TEXAS 78769  
;;; 
;;;      Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.

;;
;;  Maintenance History:
;;
;;  22 JAN 88  MMG -- Original Version incorporated into MX Rel 1.0.
;;  26 JAN 88  ab  -- Add DEFINE-UNLESS load-time conditional.
;;  08 FEB 88  MMG -- Changed :ENABLE-RECEPTION to force resetting of acb servicer and
;;                    requester complete bits during reset.
;;  12 FEB 88  MMG -- Changed Buffers-Per-Protocol to a parameter that can be changed
;;                    at run-time for fine-tuning of controller buffering.
;;  23 FEB 88  MMG -- Rewrote WAIT-RECV-ACBS to use a more efficient buffer-search algorithm.
;;  25 FEB 88  MMG -- Check Acb Error flag before calling :SIGNAL-ERROR in :TRANSMIT and
;;                    :RECEIVE; this eliminates an expensive function call.
;;  18 MAR 88  MMG -- Fix memory overflow on Mac side by preventing :INITIALIZE from 
;;                    reissuing receive buffers that are still pending.
;;  25 MAR 88  MMG -- Added out-of-memory (on Mac side) signal for error handler.
;;  01 JUN 88  CRW -- Changed :RECEIVE method to check for valid packet type before
;;                    copying packet into protocol specific resource packet.
;;  07 JUL 88 CW/AB - Added support for setting Multicast addresses and setting ethernet
;;                    address to be used at next microExplorer launch.
;;  26 JUL 88  JJP -- Add MAC-WARM-BOOT-ENC to clear out active packets on board
;;		      to prevent its writing to invalid acb packets. (All acbs 
;;		      re-allocated after warm boot.
;;  23 AUG 88  JJP -- Change :transmit to use up to *transmit-acb-limit* acbs instead of just
;;		      one. Up to 3x perf improvement
;;  13 SEP 88 CW/AB - Changed IA-SETUP to do real CLOSE/OPEN sequence to set up a new
;;                    Ethernet address (for DECNET support).
;;  27 sep 88 cw/ab - Additional cleanup in :CLOSE method; disable/enable receiver in :IA-SETUP.
;;  09 nov 88 cw/ab - Don't nil out mc-addr-list on :close.
;;  28 MAR 89  BJ     Modify the :set-eaddr method of a MAC_ENC to use the toolbox interface to write out an eadr resource on the EtherTalk 
;;                    driver and return nil to show that the ethernet address was not changed. The changed address will not take effect 
;;;                   until after the next Macintosh boot. Also added a return value of t to the same method on nupi-enc.
;;; 29 MAR 89 LER   - To reduced ethernet consing, a hash table algorithm is used to divide the ethernet address into three fixnums.


;;-------------------------------------------------------------------------------------------
;;                        DRIVER FOR THE MAC II ETHERNET CONTROLLER BOARD
;;-------------------------------------------------------------------------------------------
    
;;     The Mac Ethernet Controller uses the Macintosh II Ethernet board to transmit 
;;     and receive frames.  Since the Lisp processor board is not a bus master on the
;;     Macintosh NuBus, the Micronet Driver on the Macintosh side must act as
;;     intermediary for all Ethernet accesses; the Micronet has a specific channel set
;;     aside for Ethernet I/O -- the Macintosh side of this channel is serviced by
;;     the LAP (Link Access Protocol) Handler which is part of the Micronet Driver.
;;     The LAP Handler is responsible for interfacing the MAC-ENC on the Lisp side to
;;     the Device Manager on the MAC side for access to the Mac Ethernet device driver.
;;    
;;     The LAP Handler has three commands (OPEN, CLOSE, CONTROL) that correspond 
;;     to the PBOpen, PBClose, and PBControl commands of the Mac Device Manager (see
;;     Inside Macintosh, II-180); OPEN opens the Ethernet driver, CLOSE closes it, and
;;     CONTROL sends control commands to it.  The Lap Handler also provides a SLOTS 
;;     command that searches the MAC NuBus for Ethernet cards.  These commands are 
;;     entered into the OPCODE field of the acb; for the CONTROL command, the Ethernet 
;;     Driver command code is also entered (SUBOPCODE field). 
;;
;;     Within the Parameter block of the ACB, the LAP Handler command block is 
;;     structured as follows:
;;
;;          |<------  16  ------>|
;;   
;;          +--------------------+
;;      0   |     Error Code     |           LH-ERROR
;;          +--------------------+
;;      1   |  Reference Number  |           LH-REFNUM
;;          +--------------------+
;;      2   |      Protocol      |           LH-PROTOCOL
;;          +--------------------+
;;      3   |     Byte Count     |           LH-BYTES
;;          +--------------------+
;;      4   |        Data        |           LH-DATA
;;          |                    |
;;          :                    :
;;          +--------------------+
;; 
;;
;;     Errors signaled by the MAC OS are returned in the ERROR CODE field.  When the Ethernet
;;     Device Driver is opened, the MAC OS returns a reference number to be used for all
;;     subsequent accesses to that driver; this number is returned in the REFERENCE NUMBER
;;     field by the OPEN command and must be included in this field for all CONTROL commands.
;;     Some Ethernet Driver commands (like EAttachPH and ERead), require a protocol type to be
;;     supplied in the PROTOCOL field.  Finally, the DATA buffer of the command (with length
;;     stored in BYTE COUNT) is used for transmitting/receiving frames, getting driver statistics,
;;     and reading the board's Ethernet id.
;;
;;     To open the MAC Ethernet driver, the MAC NuBus must first be searched for Ethernet cards.
;;     the Lap Handler provides a slot-search command that returns a list of slots (with 
;;     corresponding spID's) containing EtherTalk cards.  This information is returned in the 
;;     data portion of the parameter block:
;;
;;          |<------  16  ------>|
;;
;;          +--------------------+
;;      0   |       Slot         |           ESLOT
;;          +--------------------+
;;      1   |       spID         |           ESPID
;;          +--------------------+
;;          |       Slot         |           ESLOT + 2
;;          +--------------------+
;;          |       spID         |           ESPID + 2
;;          +--------------------+
;;          |     Slot = 256     |           ESLOT + 2n
;;          +--------------------+
;;
;;
;;     Slot field = 256 marks the end of the list of Slot/spID pairs (normally just one pair).
;;     The Open command must include this Slot and spID in the first two halfwords of the data
;;     segment (LH-DATA) of the parameter block (in that order).
;;
;;     For Receives and Transmits, the data segment of the parameter block is structured as a 
;;     "Frame" (packet) in the following way:
;;
;;          |<------  16  ------>|
;;   
;;          +--------------------+
;;      0   |    Destination     |           EDESTADDR
;;          |      Address       |
;;          |                    |
;;          +--------------------+
;;      3   |      Source        |           ESRCADDR
;;          |      Address       |
;;          |                    |
;;          +--------------------+
;;      6   |      Protocol      |           EPROTTYPE
;;          +--------------------+
;;      7   |        Data        |           EDATA
;;          |                    |
;;          :                    :
;;          +--------------------+
;;


;;-------------------------------------------------------------------------------------
;;                                      CONSTANTS
;;-------------------------------------------------------------------------------------

;;  LAP Handler Opcodes (Note that these constants are also defined in Enet.h):

(Defconstant      ECONTROL              47)    ; Control Command to Driver 
(Defconstant      ECLOSE                46)    ; Close Ethernet driver 
(Defconstant      EOPEN                 45)    ; Open Ethernet driver 
(Defconstant      ESLOTS                44)    ; Get Ethernet Slots 


;;  Ethernet Driver SubOpcodes:


(Defconstant      ESETGENERAL          253)    ; Set General mode
(Defconstant      EGETINFO             252)    ; Get driver info
(Defconstant      ERDCANCEL            251)    ; Cancel Read Request
(Defconstant      EREAD                250)    ; Read in a packet
(Defconstant      EWRITE               249)    ; Write out a packet to the net
(Defconstant      EDETACHPH            248)    ; Detach Protocol Handler
(Defconstant      EATTACHPH            247)    ; Attach Protocol Handler
(Defconstant      EADDMULTI            246)    ; Add a Multicast Address to the mc-addr-list
(Defconstant      EDELMULTI            245)    ; Delete a Multicast Address
(Defconstant      EMODEADDR            200)	       ;ab/cw 9/13/88
(Defconstant      EGETEADDR            201)
(Defconstant      ESETEADDR            202)	       ;ab/cw 9/13/88

;;  Micronet Command Channel:

(Defconstant      LAPCHANNEL              2)   ; Channel number for LAP Handler


;;  Lap Handler Command block (halfword offsets):

(Defconstant      LH-ERROR                0)   ; Offset to Error code returned by driver
(Defconstant      LH-REFNUM               1)   ; Offset to Driver reference number field
(Defconstant      LH-PROTOCOL             2)   ; Offset to protocol type for command
(Defconstant      LH-BYTES                3)   ; Offset to Number of bytes in Buffer
(Defconstant      LH-DATA                 4)   ; Offset to Data Buffer

(Defconstant      LH-RESID                0)   ; Offset to Resource Id field
(Defconstant      LH-EADDR                1)   ; Offset to Ethernet Address field


;;  Open halfword offsets (Lap Handler data):

(Defconstant      LH-SLOT                 0)   ; Offset to slot (open)
(Defconstant      LH-SPID                 1)   ; Offset to spid (open)
(Defconstant      LH-MCADDR               0)   ; Offset to 1st 16-b of mc-addr

;;  Frame halfword offsets (Lap Handler data):

(Defconstant      EDESTADDR               0)   ; Frame offset to destination address
(Defconstant      ESRCADDR                3)   ; Frame offset to source address
(Defconstant      EPROTTYPE               6)   ; Frame offset to type field
(Defconstant      EDATA                   7)   ; Frame offset to data

;;  Status halfword offsets (Lap Handler data):

(Defconstant      STAT-ENET-ID            0)   ; offset to board ethernet id
(Defconstant      STAT-OVERRUN            3)   ; offset to overrun count
(Defconstant      STAT-XMIT-TIMEOUT       5)   ; offset to transmit timeouts
(Defconstant      STAT-BAD-PKT            7)   ; offset to bad packet count


;;  Acb-Sizes:
  
(Defconstant      STATUS-SIZE          1024)   ; used for status, open, etc.
(Defconstant      FRAME-SIZE           2048)   ; used for receive/transmit

(Defparameter     BUFFERS-PER-PROTOCOL   16)   ; number of acb's per protocol type


;;  Error Codes:

(Defconstant      NOERR                   0)   ; No Error


;;  End Markers:

(Defconstant      END-SLOTS             256)   ; End of slot list




;;-------------------------------------------------------------------------------------
;;                          LAP HANDLER COMMAND BLOCK ACCESSORS
;;-------------------------------------------------------------------------------------

(Defmacro LAP-ERROR (acb)
  "Accessor for Error code field in ACB parm"        `(Add:Read-Parm ,Acb LH-ERROR 16))
(Defmacro SET-LAP-ERROR (acb value)
  "Updates the  Error code field in ACB parm"        `(Add:Set-Parm ,Acb LH-ERROR ,value 16))
(DefSetf LAP-ERROR SET-LAP-ERROR)


(Defmacro LAP-REFNUM (acb)
  "Accessor for driver reference number field in ACB" `(Add:Read-Parm ,Acb LH-REFNUM 16))
(Defmacro SET-LAP-REFNUM (acb value)
  "Updates the driver reference number field in ACB"  `(Add:Set-Parm ,Acb LH-REFNUM ,value 16))
(DefSetf LAP-REFNUM SET-LAP-REFNUM)


(Defmacro LAP-PROTOCOL (acb)
  "Accessor for LAP protocol field in ACB parm"      `(Net:Swap-Bytes
					                (Add:Read-Parm ,Acb LH-PROTOCOL 16)))
(Defmacro SET-LAP-PROTOCOL (acb value)
  "Updates the LAP protocol field in ACB parm"       `(Add:Set-Parm ,Acb LH-PROTOCOL

							(Net:Swap-Bytes ,value) 16))
(DefSetf LAP-PROTOCOL SET-LAP-PROTOCOL)


(Defmacro LAP-BYTECOUNT (acb)
  "Accessor for byte count field in ACB parm"        `(Add:Read-Parm ,Acb LH-BYTES 16))
(Defmacro SET-LAP-BYTECOUNT (acb value)
  "Updates the byte count field in ACB parm"         `(Add:Set-Parm ,Acb LH-BYTES ,value 16))
(DefSetf LAP-BYTECOUNT SET-LAP-BYTECOUNT)



;;-------------------------------------------------------------------------------------
;;                                    OPEN ACCESSORS
;;-------------------------------------------------------------------------------------

(Defmacro LAP-SLOT (acb)
  "Accessor for slot field in ACB parm"    `(Add:Read-Parm ,Acb (+ LH-DATA LH-SLOT) 16))
(Defmacro SET-LAP-SLOT (acb value)
  "Updates the slot field in ACB parm"     `(Add:Set-Parm ,Acb (+ LH-DATA LH-SLOT) ,value 16))
(DefSetf LAP-SLOT SET-LAP-SLOT)


(Defmacro LAP-SPID (acb)
  "Accessor for spid field in ACB parm"    `(Add:Read-Parm ,Acb (+ LH-DATA LH-SPID) 16))
(Defmacro SET-LAP-SPID (acb value)
  "Updates the spid field in ACB parm"     `(Add:Set-Parm ,Acb (+ LH-DATA LH-SPID) ,value 16))
(DefSetf LAP-SPID SET-LAP-SPID)

;;-------------------------------------------------------------------------------------
;;                                    RESOURCE ACCESSORS
;;-------------------------------------------------------------------------------------
(Defmacro LAP-RESID (acb)
  "Accessor for resource id field in ACB parm"      `(Add:Read-Parm ,Acb (+ LH-DATA LH-RESID) 16))

(Defmacro SET-LAP-RESID (acb value)
  "Updates the resource id field in ACB parm"       `(Add:Set-Parm  ,Acb (+ LH-DATA LH-RESID) ,value 16))

(DefSetf LAP-RESID SET-LAP-RESID)


(Defmacro LAP-EADDR(acb)
  "Accessor for the Ethernet Address field in a Get or Modify Eaddr command"    
  `(dpb      (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-EADDR 0) 16)) (Byte 16 32)
	(dpb (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-EADDR 1) 16)) (Byte 16 16)
	     (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-EADDR 2) 16)))))

(Defmacro SET-LAP-EADDR (acb value)
  "Updates the resource name field in ACB parm"   
  (Once-Only (Value)
    `(Progn
       (Add:Set-Parm ,Acb (+ LH-DATA  LH-EADDR  0)
		     (Net:Swap-Bytes (Ldb (Byte 16 32) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA  LH-EADDR  1)
		     (Net:Swap-Bytes (Ldb (Byte 16 16) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA  LH-EADDR  2)
		     (Net:Swap-Bytes (Ldb (Byte 16  0) ,Value)) 16))))

(DefSetf LAP-EADDR SET-LAP-EADDR)

;;-------------------------------------------------------------------------------------
;;                                  STATUS ACCESSORS
;;-------------------------------------------------------------------------------------

(Defmacro STATUS-ENET-ID (Acb)
  "Accessor for Ethernet ID in status of ACB."
  `(dpb      (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA STAT-ENET-ID 0) 16)) (Byte 16 32)
	(dpb (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA STAT-ENET-ID 1) 16)) (Byte 16 16)
	     (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA STAT-ENET-ID 2) 16)))))

(Defmacro STATUS-XMIT-TIMEOUTS (acb)
  "Accessor for transmit timeouts field in ACB parm"
  `(Add:Read-Parm ,Acb (+ LH-DATA STAT-XMIT-TIMEOUT) 32))

(Defmacro STATUS-OVERRUN-CNT (acb)
  "Accessor for receive overrun counts field in ACB parm"
  `(Add:Read-Parm ,Acb (+ LH-DATA STAT-OVERRUN) 32))

(Defmacro STATUS-BAD-PKTS (acb)
  "Accessor for receive packets with incorrect address field in ACB parm"
  `(Add:Read-Parm ,Acb (+ LH-DATA STAT-BAD-PKT) 32))


;;-------------------------------------------------------------------------------------
;;                                   FRAME ACCESSORS
;;-------------------------------------------------------------------------------------
;; To avoid ethernet bignum consing, a hash table is used to divide the ethernet
;; address into three fixnums.

(defvar *enet-addr-hash* (make-hash-table :test 'equal))

(defmacro build-enet-addr (high med low)
  (once-only (high med low)
    `(with-stack-list (key ,high ,med ,low)
       (or (gethash key *enet-addr-hash*)
	   (setf (gethash (list ,high ,med ,low) *enet-addr-hash*) (dpb ,high (byte 16 32) (dpb ,med (byte 16 16) ,low)))))))

(Defmacro ACB-ENET-DEST-ADDR (Acb)
  "Accessor for Ethernet Destination Address in frame of ACB."
  `(build-enet-addr
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA EDESTADDR 0) 16))
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA EDESTADDR 1) 16))
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA EDESTADDR 2) 16))))

(Defmacro SET-ACB-DEST-ADDR (Acb Value)
  "Updates the Ethernet Destination Address in frame of ACB"
  (Once-Only (Value)
    `(Progn
       (Add:Set-Parm ,Acb (+ LH-DATA EDESTADDR 0)
		     (Net:Swap-Bytes (Ldb (Byte 16 32) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA EDESTADDR 1)
		     (Net:Swap-Bytes (Ldb (Byte 16 16) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA EDESTADDR 2)
		     (Net:Swap-Bytes (Ldb (Byte 16  0) ,Value)) 16))))
(DefSetf ACB-ENET-DEST-ADDR SET-ACB-DEST-ADDR)


(Defmacro ACB-ENET-SRC-ADDR (Acb)
  "Accessor for Ethernet Source Address in frame of ACB."
  `(build-enet-addr
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA ESRCADDR 0) 16))
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA ESRCADDR 1) 16))
     (Net:Swap-Bytes (Add:Read-Parm ,Acb (+ LH-DATA ESRCADDR 2) 16))))

(Defmacro SET-ACB-SRC-ADDR (Acb Value)
  "Updates the Ethernet Source Address in frame of ACB"
  (Once-Only (Value)
    `(Progn
       (Add:Set-Parm ,Acb (+ LH-DATA ESRCADDR 0)
		     (Net:Swap-Bytes (Ldb (Byte 16 32) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA ESRCADDR 1)
		     (Net:Swap-Bytes (Ldb (Byte 16 16) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA ESRCADDR 2)
		     (Net:Swap-Bytes (Ldb (Byte 16  0) ,Value)) 16))))
(DefSetf ACB-ENET-SRC-ADDR SET-ACB-SRC-ADDR)

;;-------------------------------------------------------------------------------------
;;                             MULTICAST ADDRESS ACCESSORS
;;-------------------------------------------------------------------------------------
(Defmacro ACB-ENET-MCADDR (Acb)
  "Accessor for Ethernet Multicast Address in frame of ACB."
  `(dpb      (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-MCADDR 0) 16)) (Byte 16 32)
	(dpb (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-MCADDR 1) 16)) (Byte 16 16)
	     (Net:Swap-Bytes
	       (Add:Read-Parm ,Acb (+ LH-DATA LH-MCADDR 2) 16)))))

(Defmacro SET-ACB-MCADDR (Acb Value)
  "Updates the Ethernet Multicast Address in frame of ACB"
  (Once-Only (Value)
    `(Progn
       (Add:Set-Parm ,Acb (+ LH-DATA LH-MCADDR 0)
		     (Net:Swap-Bytes (Ldb (Byte 16 32) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA LH-MCADDR 1)
		     (Net:Swap-Bytes (Ldb (Byte 16 16) ,Value)) 16)
       (Add:Set-Parm ,Acb (+ LH-DATA LH-MCADDR 2)
		     (Net:Swap-Bytes (Ldb (Byte 16  0) ,Value)) 16))))
(DefSetf ACB-ENET-MCADDR SET-ACB-MCADDR)


(Defmacro ACB-ENET-PROT (Acb)
  "Accessor for Protocol type within frame of ACB"
  `(Add:Read-Parm ,Acb (+ LH-DATA EPROTTYPE) 16))
(Defmacro SET-ACB-ENET-PROT (Acb Value)
  "Updates Protocol type field within frame of ACB"
  `(Add:Set-Parm ,Acb (+ LH-DATA EPROTTYPE) ,Value 16))
(DefSetf ACB-ENET-PROT SET-ACB-ENET-PROT)


(Defsubst COPY-ACB-TO-PKT (Acb Pkt Count)
  "Copies COUNT elements from the data in frame of ACB to 16-bit array PKT."
  (Without-Interrupts
    (Add:Copy-Parms Acb Pkt (+ LH-DATA EDATA) 0 Count :TO-ARRAY 16))
  )					   ; acb-to-pkt

(Defsubst COPY-PKT-TO-ACB (Pkt Acb Count)
  "Copies COUNT elements from the 16-bit array PKT to the frame of ACB."
  (Without-Interrupts
    (Add:Copy-Parms Acb Pkt (+ LH-DATA EDATA) 0 Count :TO-ACB 16))
  )					   ; pkt-to-acb


;;-------------------------------------------------------------------------------------
;;                                RECEIVE ACB LIST HANDLER
;;-------------------------------------------------------------------------------------


(Defun WAIT-RECV-ACBS (Rcv-List Channel)
  "Wait until COMMAND-COMPLETE becomes true of an ACB in RCV-LIST return it."
  (declare (ignore channel))
  (Without-Interrupts
    (Block Find-Acb
      (Dolist (Pair Rcv-List)
	(dolist (acb (cadr pair))
	  (When (Add:Command-Complete Acb)	       ; a process-wait.
	    (setf (add:command-complete acb) nil)
	    (Return-From Find-Acb Acb)		       ; then return it.
	    )					       ; when
	  )					       ; dolist
	)					       ; dolist
      
      (return-from find-acb 
	(si:process-wait-with-timeout "Network Wait" nil
				   #'(lambda (recv-list)
				       (block search
					 (Dolist (Pair Recv-List)              ; list of protocol lists
					   (dolist (acb (cadr pair))
					     (When (Add:Command-Complete Acb)  ; a process-wait.
					       (setf (add:command-complete acb) nil)
					       (Return-from search Acb)	       ; then return it.
					       )       ; when
					     ))	       ; dolist
					 ))
				   rcv-list))	       ; dolist
      
      )						       ; block
    )						       ; w/o interrupts
  )						; wait-recv-list



;;-------------------------------------------------------------------------------------
;;                             MACINTOSH ETHERNET CONTROLLER
;;-------------------------------------------------------------------------------------

(Defflavor MAC-ENC
	   
	   ((Open              Nil)		; Driver open flag
	    (Refnum              0)		; Driver reference number
	    (Spid                0)		; Structure ID (EtherTalk card ROM)
	    (Bad-Pkts            0)		; Number of packets with incorrect address
	    (Overrun-Count       0)		; Number of buffer overwrites on receive
	    (Attached-Protocols ())		; list of protocols that have been attached
	    (Xmit-Pending      Nil)		; flag if xmit acb is pending
	    
	    ;;  Channel
	    
	    (Channel            ())		; Channel for Micronet commands
	    
	    ;;  Receive and Transmit ACB's
	    
	    (Receive-Acb-List  ())		; Allocated at init time.
	    (Receive-Acb-Lock  ())		; You must have this to access receive-acb list
	    (Transmit-Acb      ())		; Allocated at init time.
	    (Transmit-Acb-Lock ()))		; You must have this to access transmit-acb
	   (Ethernet-Controller-Mixin)
  
  :Gettable-Instance-Variables
  :Inittable-Instance-Variables
  (:Default-Init-Plist :Board-Type :Mac)
  (:Documentation "Macintosh Ethernet Controller")
  )

					   ; MAC-ENC

(si:DEFINE-UNLESS :ENET

;;-------------------------------------------------------------------------------------
;;                                    ERROR HANDLING
;;-------------------------------------------------------------------------------------

(Defmethod (MAC-ENC :SIGNAL-ERROR) (acb)
  "Translates and signals OS errors returned in the error field of ACB"
  
  (Let ((Err (Lap-Error Acb)))
    (Unless (Eql Err NOERR)
      (Error 
	(Case Err
	  (#xFFEF "Mac OS returned a Control error")	        ; -17 
	  (#xFFEE "Mac OS returned a Status error")	        ; -18
	  (#xFFED "Mac OS returned a Read error")	        ; -19
	  (#xFFEC "Mac OS returned a Write error")	        ; -20
	  (#xFFEB "EtherTalk is not installed or is not activated") ; -21
	  (#xFFEA "Mac OS returned a Unit Empty error")         ; -22
	  (#xFFE9 "Mac OS returned an Open permission error")	; -23
	  (#xFFE8 "Mac OS returned a Close error")	        ; -24
	  (#xFFE7 "MAC OS tried to remove an open driver")      ; -25
	  (#xFFE6 "MAC OS couldn't find driver in resources")   ; -26
	  (#xFFE5 "Print manager abort")	                ; -27
	  (#xFFE4 "MAC OS: driver isn't open")                  ; -28
	  (#xFFDD "Mac OS could not open the Ethernet driver")	; -35
	  (#xFFD5 "Mac OS could not open the Ethernet driver")	; -43	   
	  (#xFFCD "Bad Driver Reference Number")	        ; -51	   
	  (#xFFA5 "Bad socket number")	                        ; -91	   
	  (#xFFA4 "Data Length is too big")	                ; -92	   
	  (#xFFA3 "No network bridge for non-local send")       ; -93	   
	  (#xFFA2 "Error attaching/detaching protocol")         ; -94	   
	  (#xFFA1 "Excessive collisions on write")	        ; -95
	  (#xFF9F "Driver open error -- port is in use")	; -97	   
	  (#xFF9E "Parameter ram not configured")	        ; -98	   
	  (#xFF9D "Error in ROZ")		                ; -99
	  (#xFF94 "Mac OS could not allocate more memory")      ; -108
	  
	  (Otherwise "Undefined error code returned by Mac OS: ~a"))
	Err)
      )						; unless
    )						; let     
  )						; signal-error



;;-------------------------------------------------------------------------------------
;;                                    INITIALIZATIONS
;;-------------------------------------------------------------------------------------

(Defmethod (MAC-ENC :INITIALIZE) (&rest ignore)
  "Performs the setup of the Ethernet controller and resets it."
  
  (Unless Open
    (Setf Channel
	  (Add:Find-Channel LAPCHANNEL))	; get channel
    (Send Self :Open)                           ; open driver
    (Send Self :General-Mode))                  ; and put it into general mode
  (Setf Ethernet-Address
	(Send Self :Read-Rom-Id))		; Get Ethernet id from board
  (Send Self :Screen-Setup)			; Attach Protocols, setup receive acbs
  (setf Transmit-Acb nil)			; allocated from :transmit 8/88 JP
;;;  (Unless Transmit-Acb
;;;    (Setf Transmit-Acb 
;;;	  (Add:Get-acb-fast FRAME-SIZE)))		; Set up transmit ACB
;;;  (Setf (Add:Servicer-Complete Transmit-Acb) t)
  (Send Self :Enable-Reception)			; Begin receiving frames

  (Send Self :MC-Setup)
  )

(Defmethod (MAC-ENC :MC-SETUP) (&Optional New-List)      
  "Sets up the controller with valid multicast addresses. If NEW-LIST is not
   specified, the instance variable MC-ADDR-LIST is used, else MC-ADDR-LIST is
   updated to NEW-LIST. An empty multicast address list disables reception of
   any frame with a multicast address."

  (When (Setf Mc-Addr-List (Or New-list Mc-Addr-List))
    (If (> (List-Length Mc-Addr-List) MAX-MC-ADDR)
	(Error nil "Multicast address list is too large; ~a addresses, ~a allowed."
	       (List-Length Mc-Addr-List) MAX-MC-ADDR))

    (Dolist (Enet-Addr Mc-Addr-List)
      (If (Not (Logbitp (* 8 (- Enet-Addr-Len 1)) Enet-Addr))
	  (Error nil "Illegal multicast address; first octet lsb is zero: ~16R" enet-addr)) 
      (SEND self :add-mc-addr enet-addr)))
  )

(Defmethod (MAC-ENC :MC-CLEAR) ()
  (Dolist (Enet-Addr Mc-Addr-List)
    (SEND self :delete-mc-addr enet-addr))
  )


(Defmethod (MAC-ENC :READ-ROM-ID) (&Aux Ethernet-ID)
  "Returns the Ethernet ID from the Ethernet Driver"
  
  (Setf Ethernet-Id 0)
  (let (Acb)
    (unwind-protect
	(progn
	  (setf acb (add:get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb ECONTROL EGETINFO)	       ; Setup Command block
	  (Setf (Lap-Bytecount Acb) STATUS-SIZE)
	  (Setf (Lap-Refnum    Acb) Refnum)       
	  (Add:transmit-packet-And-Wait Acb Channel)
	  (Send Self :Signal-Error Acb)
	  (Setf Ethernet-Id (Status-Enet-Id Acb)))     ; Read address from acb
      (add:return-acb acb t)))
  (When (Or (Zerop Ethernet-Id)
	    (Eql Ethernet-Id BROADCAST-ADDRESS))
    (Error nil "The EtherTalk driver returned an invalid Ethernet Id: ~16r" Ethernet-Id))
  Ethernet-Id)

(Defmethod (MAC-ENC :GENERAL-MODE) ()
  "Puts the Ethernet driver on the MAC side into General Mode"
  (let (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb ECONTROL ESETGENERAL)
	  (Setf (Lap-Refnum Acb) Refnum)						
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb))
      (add:return-acb acb t))))


(Defmethod (MAC-ENC :ENABLE-RECEPTION) ()
  "Enable the Mac Ethernet Controller to receive frames."
  
  (Dolist (Pair Receive-Acb-List)
    (Dolist (Acb (Cadr Pair))
      (When (Add:Command-Complete Acb)		       ; If acb is free to reissue
	(Add:Init-Acb Acb ECONTROL EREAD)	       ; Setup Command block
	(Setf (Lap-Bytecount         Acb) FRAME-SIZE)
	(Setf (Lap-Refnum            Acb) Refnum)
	(Setf (Lap-Protocol          Acb) (Car Pair))
	(Add:Transmit-packet Acb Channel)))))

;;ab/cw 9/13/88.  This now will not be used.
(Defmethod (MAC-ENC :MODIFY-EADR-RESOURCE) (New-Eadr &optional for-slot)
  "In the current resource chain, get the handle for this resource and then modify the address value."
  (declare (values))
  (WHEN (NULL for-slot)
    (SETQ for-slot net:slot))
  (let  (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb ECONTROL EMODEADDR)	       ; Setup Command block
	  (Setf (Lap-Refnum Acb) Refnum)       
	  (Setf (Lap-ResId  Acb) for-slot)
	  (Setf (Lap-Eaddr  Acb) New-Eadr)	                                   
	  (Add:Transmit-packet-And-wait Acb Channel)	       ; Execute command
	  (Send Self :Signal-Error Acb))
      (add:return-acb-fast acb t))))

(Defmethod (MAC-ENC :READ-EADR) (&optional for-slot)
  "In the current resource chain, get the handle for this resource and then modify the address value."
  (declare (values ethernet-address))
  (WHEN (NULL for-slot) (SETQ for-slot net:slot))
  (let  (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb ECONTROL EGETEADDR)
	  (Setf (Lap-Refnum Acb) Refnum)       
	  (Setf (Lap-Resid  Acb) for-slot)
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb)
	  (Lap-Eaddr Acb))
      (add:return-acb-fast acb t))))

(DefMethod (MAC-ENC :SET-EADDR) (address)
  (COND ((= (SEND self :read-eadr) address)
	 ;; Our address is already ADDRESS.
	 (SETQ ethernet-address address))
	(t
	 ;; Use the toolbox interface to put an EADR resource on the ethernet driver. *BJ*
	 (let* ((tb:*application-channel* 8)
		(inhibit-scheduling-flag t)
		eaddr-resource
	       (cur-enet-addr 0)
	       ether-res-file)
	   (unwind-protect
	       (progn

		 (tb:with-system-directory tb:*system-dir-wd* (setf ether-res-file (tb:!OpenResFile "EtherTalk")))
		 
		 (tb:suppress-some-oserrs (tb:!resNotFound) (setf eaddr-resource (tb:Get1Resource "eadr" net:slot)))
		 
		 (cond ((or (not (typep eaddr-resource 'tb:mac-handle))
				 (zerop (send eaddr-resource :handle)))
			(setf eaddr-resource (tb:!NewHandle 6))
			(when (zerop (send eaddr-resource :handle))
			  (ferror 'cant-set-enet-addr "Cannot set the ethernet address because of a resource manager error."))
			(tb:!addResource eaddr-resource "eadr" net:slot "ethernet address"))
		       (t		  
			(dotimes (x 3)
			  (setf cur-enet-addr (dpb (tb:fetchWordHandle eaddr-resource (* 2 (- 2 x))) (byte 16 (* x 16)) cur-enet-addr)))))
		 
		 ;; If the ethernet address has changed write it out.
		 (unless (= cur-enet-addr address)
		   (dotimes (x 3)
		     (tb:StowWordHandle eaddr-resource (* 2 (- 2 x)) (ldb (byte 16 (* x 16)) address)))
		   (tb:!changedResource eaddr-resource)
		   (tb:!WriteResource eaddr-resource)
		   (tv:notify nil "~5&***                Ethernet address changed for Decnet                    ***~&~
                                *** This change will not take effect until after the next Macintosh boot. ***~5&"))
		 )
	     (when  ether-res-file (tb:!CloseResFile ether-res-file)))))))

(DefMethod (MAC-ENC :IA-SETUP) (address)
  "Set up our Ethernet address to be ADDRESS."
  (cond ((= ethernet-address address))
	(t
	   (cerror "Set address (#x~x) for next Macintosh boot." "The Ethernet address cannot be setup on a microExplorer." address)
	   (send self :set-eaddr address)
	 nil)))

(DefMethod (MAC-ENC :ADD-MC-ADDR) (Eaddr)
  "Load multicast addresses into the Ethernet card"
  (declare (values))
  (let  (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE)) 
	  (Add:Init-Acb Acb ECONTROL EADDMULTI)
	  (Setf (Lap-Refnum   Acb) Refnum)
	  (setf (acb-enet-mcaddr acb) eaddr)
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb))
      (Add:Return-Acb Acb t))))


(Defmethod (MAC-ENC :DELETE-MC-ADDR) (Eaddr)
  "Deletes a multicast address from the Ethernet driver on the Mac Side"
  (declare (values))
  (let (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE)) 
	  (Add:Init-Acb Acb ECONTROL EDELMULTI)
	  (Setf (Lap-Refnum   Acb) Refnum)       
	  (setf (acb-enet-mcaddr acb) eaddr)
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb))
      (Add:Return-Acb Acb t))))

(Defmethod (MAC-ENC :MC-SETUP)(&rest ignore)
  "Loads each of the multicast addresses from the mc-addr-list."
  (let ((mc-addrs (send self :mc-addr-list)))
    (dolist (mc-addr mc-addrs)
      (send self :add-mc-addr mc-addr))
    )
  )


;;-------------------------------------------------------------------------------------
;;                             PROTOCOL ATTACHING/DETACHING
;;-------------------------------------------------------------------------------------


(Defmethod (MAC-ENC :ATTACH-PROTOCOL) (Protocol)
  "Attaches PROTOCOL to the Ethernet driver on the Mac Side"
  (declare (values))
  (Unless (Member Protocol Attached-Protocols)
    (let (Acb)
      (unwind-protect
	  (progn
	    (setf acb (Add:Get-acb-fast STATUS-SIZE))
	    (Add:Init-Acb Acb ECONTROL EATTACHPH)
	    (Setf (Lap-Protocol Acb) Protocol)
	    (Setf (Lap-Refnum   Acb) Refnum)       
	    (Add:Transmit-packet-And-wait Acb Channel)
	    (Send Self :Signal-Error Acb))
	    (Add:Return-Acb Acb t)))
    (Push Protocol Attached-Protocols)))

(Defmethod (MAC-ENC :DETACH-PROTOCOL) (Protocol)
  "Detaches PROTOCOL from the Ethernet driver on the Mac Side"
						       ; Get Acb and Channel
  (When (Member Protocol Attached-Protocols)
    (let (Acb)
      (unwind-protect
	  (progn
	    (setf acb (Add:Get-acb-fast STATUS-SIZE)) 
	    (Add:Init-Acb Acb ECONTROL EDETACHPH)      ; Setup Command block
	    (Setf (Lap-Protocol Acb) Protocol)
	    (Setf (Lap-Refnum   Acb) Refnum)       
	    (Add:Transmit-packet-And-wait Acb Channel)
	    (Send Self :Signal-Error Acb))
	(Add:Return-Acb Acb t)))
    (Setf Attached-Protocols
	  (Remove Protocol
		  Attached-Protocols))))


(Defmethod (MAC-ENC :SCREEN-SETUP) (&rest ignore &Aux Acb-List Prot-List New-Acb)
  "Sets up the controller with the valid protocol types.  A list of receive
   acbs is created for each protocol type.  Each received packet is screened 
   against these types by the hardware."
  
  (When Valid-Pkt-Types
    (Dolist (Prot Valid-Pkt-Types)
      (If (Not (Setf Prot-List (Assoc Prot Receive-Acb-List)))
	  (Progn				; Attach protocol to driver if new.
	    (Send Self :Attach-Protocol Prot)  
	    (Setf Acb-List ())			; then build receive list
	    (Dotimes (I Buffers-Per-Protocol)
	      (Push (Setf New-Acb (Add:Get-acb-fast FRAME-SIZE)) Acb-List)
	      (Setf (Add:Servicer-Complete New-Acb) t)
	      (Setf (Add:Input-Complete    New-Acb) t))

	    (Push (List Prot Acb-List) Receive-Acb-List)) 	
						; Else, Add extra buffers to receive list
	  (Setf Acb-List (Cadr Prot-List))
	  (Dotimes (I (Max 0 (- BUFFERS-PER-PROTOCOL (Length Acb-List))))
	    (Push (Add:Get-acb-fast FRAME-SIZE) Acb-List))
	  (Setf (Cadr Prot-List) Acb-List)	; put in new list
	  )					; if
      )						; dolist
    )						; when  
  )						; SCREEN-SETUP



;;-------------------------------------------------------------------------------------
;;                                   DRIVER OPEN/CLOSE
;;-------------------------------------------------------------------------------------

(Defmethod (MAC-ENC :OPEN) ()
  "Opens the Ethernet driver on the MAC side"
  (declare (values))
  (let (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb EOPEN EOPEN)
	  (Setf (Lap-Slot Acb) Net::Slot)
	  (Setf (Lap-Spid Acb) Spid)
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb)
	  (Setf Refnum (Lap-Refnum Acb)))
      (Add:Return-Acb Acb t)))
  (Setf Open T))

;; Turn this into an error just in case someone tries to do it.
(Defmethod (MAC-ENC :CLOSE) ()
  "Closes the Ethernet driver on the MAC side and puts the MAC-ENC Ethernet 
controller in a closed state."
  (cerror "Resume without closing the ethernet driver" "Trying to close the Ethernet driver on a microExplorer.")
  )


;;-------------------------------------------------------------------------------------
;;                                 DATALINK INTERFACE
;;-------------------------------------------------------------------------------------
(defvar *transmit-acb-limit* 16.)

(Defmethod (MAC-ENC :TRANSMIT) (Type Dest Data-Array N-Bytes
				&Optional (Deallocate T) &aux (add:*no-interrupt* t))
  "Transmit the 16-bit DATA-ARRAY to Ethernet address DEST.
   N-BYTES    = Number of bytes to transmit.
   TYPE       = Ethernet Frame type, :CHAOS or numeric Ethernet type code.
   DEALLOCATE = T for arrays that must be freed after transmission."
  
  (Transforming-Arguments Type Dest Data-Array N-Bytes Deallocate
    (when (> (length transmit-acb) *transmit-acb-limit*)	  ;; keep us from running out of acbs
      (Add:Wait-Command-Complete (nth (min 3 *transmit-acb-limit*) Transmit-Acb) Channel)	;; wait for some
      								;;		to get finished (but not all (3))
      (without-interrupts
	(dolist (acb transmit-acb)		       ; release fininshed acb's check for errors
	  (when  (add:servicer-complete acb)
	    (Unless (Eql (Lap-Error Acb) NOERR)
	      (Send Self :Signal-Error Acb))	       ; flag any errors
	    (setq transmit-acb (delete acb transmit-acb))
	    (add:return-acb-fast acb t)))))
    (Let ((N-Words (Net::Convert-To-Words N-bytes))
	  (acb (Add:Get-acb-fast FRAME-SIZE)))

	(Setf (Acb-Enet-Dest-Addr  Acb) Dest)	        ; Load Destination Address
	(Setf (Acb-Enet-Prot       Acb) Type)	        ; Load protocol type 
	(Setf (Lap-Bytecount       Acb) (+ N-Bytes HEADER-SIZE))
	(Setf (Lap-Refnum          Acb) Refnum)	; Load reference number
	(Copy-Pkt-To-Acb Data-Array Acb N-Words)	; Copy data for next packet
	(Add:Init-Acb Acb ECONTROL EWRITE)	        ; Setup Command Block
	(Add:Transmit-packet Acb Channel)	; Issue command
	(without-interrupts (pushnew acb transmit-acb)))
    (without-interrupts
      (dolist (acb transmit-acb)		       ; release fininshed acb's check for errors
	(when  (add:servicer-complete acb)
	  (Unless (Eql (Lap-Error Acb) NOERR)
	    (Send Self :Signal-Error Acb))	       ; flag any errors
	  (setf transmit-acb (delete acb transmit-acb))
	  (add:return-acb-fast acb t))))
    )						; with transformed args
  )
						; Transmit
(Defmethod (MAC-ENC :TRANSMIT-FAST) (Type Dest Data-Array N-Bytes)
  "Call :TRANSMIT with Deallocate set to nil."
  (Send Self :Transmit Type Dest Data-Array N-Bytes Nil)
  ) ; transmit-fast


(Defmethod (MAC-ENC :RECEIVE) (&rest ignore)
  "Returns the DESTINATION, SOURCE, TYPE and DATA for the next valid frame.
   Data is returned as a 16-bit array."
  
  (Let (Array M-bytes N-words Destination Source Type Rcv-Acb)    
    (Block Receive
      (With-Lock (Receive-Acb-Lock)		; Lock the acbs from other processes
	(Setf Rcv-Acb				; Wait for an acb to finish
	      (Wait-Recv-Acbs Receive-Acb-List Channel))
	(Unless (Eql (Lap-Error Rcv-Acb) NOERR)
	  (Send Self :Signal-Error Rcv-Acb))	; flag any errors	    
						; Get info from header
	(Setf M-Bytes     (- (Lap-Bytecount Rcv-Acb) HEADER-SIZE))
	(Setf N-Words     (Net::Convert-To-Words M-bytes))
	(Setf Type        (Acb-Enet-Prot         Rcv-Acb))
	(Setf Destination (Acb-Enet-Dest-Addr    Rcv-Acb))
	(Setf Source      (Acb-Enet-Src-Addr     Rcv-Acb))

	(if	                                          ; If known type field,
	  (Member Type Valid-Pkt-Types)
	  (progn
	    (Setf Array (Allocate-Net-Packet Type))	; Get a packet array
	    (Copy-Acb-To-Pkt Rcv-Acb Array N-Words)	; Copy data buffer
	    (Add:Init-Acb Rcv-Acb ECONTROL EREAD)	; Setup Command block
	    (Setf (Lap-Bytecount Rcv-Acb) MAX-FRAME-LENGTH)
	    (Setf (Lap-Refnum    Rcv-Acb) Refnum)
	    (Setf (Lap-Protocol  Rcv-Acb) Type)
	    (setf (add:servicer-complete rcv-acb) nil)
	    (Add:Transmit-packet Rcv-Acb Channel))	        ; Reissue Acb
	  (progn 
	    (Add:Init-Acb Rcv-Acb ECONTROL EREAD)	; Setup Command block
	    (Setf (Lap-Bytecount Rcv-Acb) MAX-FRAME-LENGTH)
	    (Setf (Lap-Refnum    Rcv-Acb) Refnum)
	    (Setf (Lap-Protocol  Rcv-Acb) Type)
	    (setf (add:servicer-complete rcv-acb) nil)
	    (Add:Transmit-packet Rcv-Acb Channel)	        ; Reissue Acb	  
	    (setf type nil)))
	  )					        ; with lock
      (Return-From Receive Destination Source Type Array M-bytes)
      )						        ; receive-block
    )						        ; Let   
  )


;;-------------------------------------------------------------------------------------
;;                                      DISPLAYS
;;-------------------------------------------------------------------------------------


(Defmethod (MAC-ENC :PRINT-STATUS) (&Optional (Stream *Terminal-Io*)
				    &Rest ignore)
  (Format t "~:|~3% Status of Macintosh Ethernet controller in slot ~16r:" Net::Slot)
  (Send Self :Print-Ethernet-Id  Stream)
  (Send Self :Print-Error-Counts Stream))	; PRINT-STATUS


(Defmethod (MAC-ENC :PRINT-ETHERNET-ID) (&Optional (Stream *Standard-Output*)
					 &Rest Ignore)
  "Prints the Ethernet ID loaded into this controller"
  
  (Format Stream "~%~%  Ethernet Address of this controller: #x~16r"
	  Ethernet-Address))			; PRINT-ETHERNET-ID


(Defmethod (MAC-ENC :PRINT-ERROR-COUNTS) (&Optional (Stream *Terminal-Io*)
					  &Rest Ignore)
  "Prints the error statistics of this controller"
  
  (Send Self :Update-Stats)
  (Format Stream "~2%    Number of timeouts during transmit = ~d." Net::Transmit-Time-Outs)
  (Format Stream "~%    Number of pkts received with incorrect address = ~d." Bad-Pkts)
  (Format Stream "~%    Number of buffer overwrites on receive = ~d." Overrun-Count)
  )						; PRINT-ERROR-COUNTS


(Defmethod (MAC-ENC :UPDATE-STATS) ()
  "Updates instance variables from error statistics recorded by the EtherTalk board."
  (declare (values))
  (let (Acb)
    (unwind-protect
	(progn
	  (setf acb (Add:Get-acb-fast STATUS-SIZE))
	  (Add:Init-Acb Acb ECONTROL EGETINFO)
	  (Setf (Lap-Bytecount Acb) STATUS-SIZE)
	  (Setf (Lap-Refnum    Acb) Refnum)       
	  (Add:Transmit-packet-And-wait Acb Channel)
	  (Send Self :Signal-Error Acb)
	  (Without-Interrupts
	    (Setf Net::Transmit-Time-Outs (Status-Xmit-Timeouts Acb))
	    (Setf Net::Pkts-Lost
		  (+ (Setf Bad-Pkts       (Status-Bad-Pkts      Acb))
		     (Setf Overrun-Count  (Status-Overrun-Cnt   Acb))))))
      (Add:Return-Acb Acb t))))
    

(DEFUN si:MAC-WARM-BOOT-ENC ()
  "Disables all protocols on net:controller-list and clears acb lists to 
   prevent controller board from writing to invalid acb packets after warm boot"

  (dolist (el net:controller-list)
    (send el :eval-inside-yourself
	  '(progn
	     (setf net:enable nil)
	     (dolist (protocol ethernet:attached-protocols)
	       (send self :detach-protocol protocol))
	     (setq ethernet:receive-acb-list nil
		   ethernet:transmit-acb nil
		   ethernet:transmit-acb-lock nil)))))


;;-------------------------------------------------------------------------------------
;;                                      DIAGNOSTICS
;;-------------------------------------------------------------------------------------

(Defmethod (MAC-ENC :REFLECTOMETER-TEST) (&Optional (Stream *Standard-Output*)
					  &Rest Ignore)
  (Format Stream "~%~%  Reflectometer Tests are not supported by this controller"))

(Defmethod (MAC-ENC :SELFTEST) (&Optional (Stream *Standard-Output*)
				&Rest Ignore)
  (Format Stream "~%~%  Self Tests are not supported by this controller"))

(Defmethod (MAC-ENC :MEMORY-DUMP) (&Optional (Stream *Standard-Output*)
				   &Rest Ignore)
  (Format Stream "~%~%  Memory Dump is not supported by this controller"))


(Compile-Flavor-Methods MAC-ENC)

;;End of DEFINE-UNLESS
)