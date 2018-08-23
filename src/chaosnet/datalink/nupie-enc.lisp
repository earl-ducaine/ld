;; -*- Mode: COMMON-LISP; Package: ETHERNET; Base:10; Fonts:(MEDFNT HL12B HL12I) -*-


;;;                             RESTRICTED RIGHTS LEGEND
;;;
;;; Use, duplication, or disclosure by the Government is subject to restrictions as
;;; set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer
;;; Software clause at 52.227-7013.
;;;
;;;                          TEXAS INSTRUMENTS INCORPORATED  
;;;                                  P.O. BOX 2909  
;;;                              AUSTIN, TEXAS 78769  
;;; 
;;; Copyright (C) 1987- 1989 Texas Instruments Incorporated. All rights reserved.

1;;*--------------------------------------------------------------------------------
1;;           *           1DRIVER FOR THE NUPI/E ETHERNET CONTROLLER BOARD
2;;
;;  Maintenance History:
;;
;;  16 SEP 87  MMG -- Original Version incorporated into Rel 3.2**.
2;;  26 JAN 88  ab    -- Add DEFINE-WHEN load-time conditionals.


1;;**-------------------------------------------------------------------------------------
1;;           *    1                 *  1   *     1CONSTANTS
;;*-------------------------------------------------------------------------------------

1;;   Ethernet specific commands (defined by system-internals).

2;;      %nupie-command-enet-reset        Ethernet Software Reset
;;      %nupie-command-enet-setup        Ethernet Setup
;;      %nupie-command-enet-status       Ethernet Status
;;      %nupie-command-enet-receive      Ethernet Receive Packet
;;      %nupie-command-enet-transmit     Ethernet Transmit Packet
;;      %nupie-command-enet-tdr          Ethernet Time-Domain-Reflectometer test
;;      %nupie-command-enet-selftest     Ethernet Logic Self Test
;;      %nupie-command-enet-read         Ethernet pass through read
;;      %nupie-command-enet-write        Ethernet pass through write*

;;   Ethernet hardware constants:*

(Defconstant  1ETHERNET-UNIT*           #xFF     2"Unit Select value for all Ethernet Commands"*)
(Defconstant  1MAX-MF-TYPES*            16      2"Maximum number of Match Field Types"*) 
(Defconstant  1NUPIE-ADDR-REG*      #xE00004     2"NUPI/E addr register for the command block"*)
(Defconstant  1NUPIE-ID-ADDR*   1 *    #xFF0000     2"Address of first byte of Ethernet ID in config rom"*)

1;;   Ethernet Setup/Status Descriptor Codes:*

(Defconstant  1NUPIE-ALL-PARAMETERS *         #x00  2"Setup/Status of all parameters"*)
(Defconstant  1NUPIE-OP-FLAGS *               #x01  2"Operational Flags"*)
(Defconstant  1NUPIE-RCV-PKT-EVENT-DESC*      #x02  2"Receive Packet Event Descriptor"*)
(Defconstant  1NUPIE-MATCH-FIELDS*            #x03  2"Type fields for screening packets"*)
(Defconstant  1NUPIE-ADAPTOR-CONTROL*         #x04  2"Set control lines on Ethernet adaptor board"*)
(Defconstant  1NUPIE-INDIVIDUAL-ADDR*           #x05  2"Individual address setup/status"*)
(Defconstant  1NUPIE-MULTICAST-ADDR*          #x06  2"Multicast address setup"*)
(Defconstant  1NUPIE-82586-CONFIG *           #x07  2"82586 Ethernet controller configuration"*)
(Defconstant  1NUPIE-82586-STATUS *           #x10  2"82586 Ethernet controller status"*)
(Defconstant  1NUPIE-ERROR-COUNTS *           #x11  2"Ethernet error counts status"*)
(Defconstant  1NUPIE-STATUS*                  #x12  2"Nupi/E status"*)
(Defconstant  1NUPIE-82586-DATA*              #x13  2"82586 internal register contents"*)

(Defconstant  1DISK-BLOCK-BYTE-SIZE*    (* Si:DISK-BLOCK-WORD-SIZE 4))

1;;  Offsets into descriptor blocks:*

(Defconstant DESC-BLOCK-LENGTH       0   2"Offset to Block Length"*)
(Defconstant DESCRIPTOR-CODE         3   2"Offset to Descriptor Code"*)
(Defconstant ENTRY-SIZE              4   2"Offset to Data Entry Size"*)
(Defconstant DESCRIPTOR-DATA         8   2"Offset to the first byte of data"*)

1;; * 1Fixed Descriptor Block sizes:*

(Defconstant CFG-DESC-SIZE          #x14)
(Defconstant IA-DESC-SIZE           #x0E)
(Defconstant OP-DESC-SIZE           #x0C)
(Defconstant CEC-DESC-SIZE          #x08)
(Defconstant ADAPTOR-DESC-SIZE      #x09) 1; not presently used

;; * 1Used for updating the Setup descriptors:*

(DefMacro 1DESCRIPTOR-BYTE*  (Descriptor Field)
  2"References the descriptor byte of DESCRIPTOR located at byte FIELD."*
  `(Aref ,Descriptor ,Field)
  )					   1; Descriptor-byte*
  

1;;*-------------------------------------------------------------------------------------
1;;           *    1             *     1 * 1REQUEST BLOCK DEFINITIONS
;;*-------------------------------------------------------------------------------------

2;;;  A Request Block (called an RQB) contains several parts; its physical implementation is as an array
;;;  with a leader.  An RQB contains both the command block and the data buffers: to perform an I/O
;;;  request, a user allocates an RQB, fills in the command portions (if the request is for output, the user
;;;  also fills in the data portions), and submits the request by passing the RQB as an argument to a macro
;;;  that writes the physical address of the RQB command block to the Nupi/E.
1;;;
;;;** 1 An RQB has the following form:
;;;   Array Leader:
;;;*       +----------------------------------------------+
1;;;*     0 |         %IO-RQ-Leader-N-Half-Words           | 1; software overhead
;;;*       +----------------------------------------------+
1;;;*     1 |            %IO-RQ-Leader-N-Pages             |
1;;;*       +----------------------------------------------+
1;;;*     2 |            %IO-RQ-Leader-Buffer              | 1; used for 16-bit array accesses
;;;*       +----------------------------------------------+
1;;;*     3 |         %IO-RQ-Leader-8-Bit-Buffer           | 1; used for 8-bit array accesses
;;;*       +----------------------------------------------+
1;;;*     4 |         %IO-RQ-Leader-N-Pages-Wired          |
1;;;*       +----------------------------------------------+
1;;;
;;; * 1  RQB Array:
;;;*       +----------------------------------------------+
1;;;*     0 |                 %IO-RQ-Link                  | 1; These 2 fields
;;;*       +----------------------------------------------+ 1; are used by microcode
;;;*     1 |             %IO-RQ-Information               | 1; device queues in the NUPI
;;;*       +----------------------------------------------+
1;;;*     2 |               %IO-RQ-Command                 | 1; Nupi command block proper 
;;;*       +----------------------------------------------+ 1; starts here
;;;*     3 |                %IO-RQ-Status                 |
1;;;*       +----------------------------------------------+
1;;;*     4 |                %IO-RQ-Buffer                 |
1;;;*       +----------------------------------------------+
1;;;*     5 |            %IO-RQ-Transfer-Length            |
1;;;*       +----------------------------------------------+
1;;;*     6 |             %IO-RQ-Device-Address            |
1;;;*       +----------------------------------------------+
1;;;*     7 |             %IO-RQ-Event-Address             |
1;;;*       +----------------------------------------------+
1;;;*     8 |                    Spare                     |
1;;;*       +----------------------------------------------+
1;;;*     9 |                    Spare                     |
1;;;*       +----------------------------------------------+
1;;;*    10 |         %IO-RQ-Done-Address (Nupi/E)         |
1;;;*       +----------------------------------------------+
1;;;*    11 |          %IO-RQ-Done-Value (Nupi/E)          |
1;;;*       +----------------------------------------------+
1;;;*    12 |        %IO-RQ-Transfer-Count (Nupi/E)        |
1;;;*       +----------------------------------------------+
1;;;*    13 |        %IO-RQ-Status-Address (Nupi/E)        |
1;;;*       +----------------------------------------------+
1;;;*    14 |        %IO-RQ-Enet-Dest-Addr (Nupi/E)        |
1;;;*       +----------------------------------------------+
1;;;*    15 |        %IO-RQ-Enet-Src-Dest (Nupi/E)         |
1;;;*       +----------------------------------------------+
1;;;*    16 |        %IO-RQ-Enet-Src-Addr (Nupi/E)         |
1;;;*       +----------------------------------------------+
1;;;*    17 |          %IO-RQ-Enet-Type (Nupi/E)           |
1;;;*       +----------------------------------------------+
1;;;*    18 |                    Spare                     |
1;;;*       +----------------------------------------------+
1;;; *   19 |            %IO-RQ-Parameter-List             |
1;;;*       |                                              |
1;;;*   255 |                                              |
1;;;*       +----------------------------------------------+
1;;;
;;;   Remaining pages in array:
;;;       Data buffer space.

2;;  This is a compile-time macro used to generate the field accessor macros in this 
;;  file, it is not used at run time.  The macros generated by this macro can be used
;;  with (SETF (...) ...).**

(Defmacro 1DEF-RQBFIELD* (name word byte-desc)
2  "Create a MACRO called NAME to access a byte field, described by BYTE-DESC,
   in the field accessed by (aset rqb ,word). The MACRO takes 1 arg -- the RQB."*
  
      `(Defmacro ,NAME (Rqb)
	 `(Ldb ,',Byte-Desc (Aref ,Rqb ,',Word))))


1;;*-------------------------------------------------------------------------------------
1;;           *    1             *  1 *  1RQB COMMAND BLOCK FIELD ACCESSORS
;;*-------------------------------------------------------------------------------------

1;;   Hardware Select Fields:*
 
(Def-RqbField UNIT-SELECT         Si:%Io-Rq-Command        Si:%%Io-Rq-Command-Unit-Number) 
 
1;;   Command Fields:*

(Def-RqbField COMMAND             Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-Command)
(Def-RqbField POST-DONE-VALUE     Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-Done-Bit)
(Def-RqbField NO-BLOCK-TRANSFER   Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-No-Block-Bit)
(Def-RqbField POST-XFER-COMPLETE  Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-Xfer-Complete)
(Def-RqbField SCATTER             Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-Scatter-Bit)
(Def-RqbField POST-EVENT          Si:%Io-Rq-Command-High   Si:%%Io-Rq-Command-Event-Bit)

1;;   Status Fields:*

(Def-RqbField RETRY-COUNT         Si:%Io-Rq-Status         Si:%%Io-Rq-Status-Retry-Count)
(Def-RqbField DEVICE-ERROR        Si:%Io-Rq-Status         Si:%%Io-Rq-Status-Device-Error)
(Def-RqbField NUPI-ERROR          Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Nupi-Error)
(Def-RqbField ECC-APPLIED         Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Ecc)
(Def-RqbField XFER-COMPLETE       Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Xfer-Complete)
(Def-RqbField AUX-STATUS          Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Aux-Status)
(Def-RqbField RETRY-STATUS        Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Retry)
(Def-RqbField ERROR-STATUS        Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Error)
(Def-RqbField COMMAND-COMPLETE    Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Cmd-Complete)
(Def-RqbField BUSY                Si:%Io-Rq-Status-High    Si:%%Io-Rq-Status-Busy)

(Defmacro 1STATUS-WORD* (rqb)
  2"Accessor for Status Word of RQB."*
  `(dpb (aref ,rqb  Si:%Io-Rq-Status-High) (byte 16 16)
	(aref ,rqb  Si:%Io-Rq-Status)))

(Defmacro SET-STATUS-WORD (rqb value)
  2"Updates the Status Word of RQB."*
  (Once-Only (Value)
    `(progn
       (setf (aref ,rqb Si:%Io-Rq-Status-High) (ldb (byte 16 16) ,value))
       (setf (aref ,rqb Si:%Io-Rq-Status)      (ldb (byte 16  0) ,value)))))

(DefSetf STATUS-WORD SET-STATUS-WORD)

(Defmacro 1BYTE-XFER-COUNT* (rqb)
  2"Accessor for Byte Transfer Count of RQB."*
  `(dpb (aref ,rqb  Si:%Io-Rq-Transfer-Length-High) (byte 16 16)
	(aref ,rqb  Si:%Io-Rq-Transfer-Length)))

(Defmacro SET-BYTE-XFER-COUNT (rqb value)
  2"Updates the Byte Transfer Count of RQB."*
  (Once-Only (Value)
    `(progn
       (setf (aref ,rqb Si:%Io-Rq-Transfer-Length-High) (ldb (byte 16 16) ,value))
       (setf (aref ,rqb Si:%Io-Rq-Transfer-Length)      (ldb (byte 16  0) ,value)))))

(DefSetf BYTE-XFER-COUNT SET-BYTE-XFER-COUNT)

(Defmacro 1DEV-BLK-ADDR* (rqb)
  2"Accessor for Device Block Address of RQB."*
  `(dpb (aref ,rqb  Si:%Io-Rq-Device-Address-High) (byte 16 16)
	(aref ,rqb  Si:%Io-Rq-Device-Address)))

(Defmacro SET-DEV-BLK-ADDR (rqb value)
  2"Updates the Device Block Address of RQB."*
  (Once-Only (Value)
    `(progn
       (setf (aref ,rqb Si:%Io-Rq-Device-Address-High) (ldb (byte 16 16) ,value))
       (setf (aref ,rqb Si:%Io-Rq-Device-Address)      (ldb (byte 16  0) ,value)))))

(DefSetf DEV-BLK-ADDR SET-DEV-BLK-ADDR)


2;;  Note: The "least significant byte" as described in the spec is referred
;;         to as the "high order octet" in the documentation supplied with
;;          ethernet addresses -- that's why it looks like we're trying to
;;          load the ethernet address backwards:*

(Defmacro 1RQB-ENET-DEST-ADDR* (rqb)
  2"Accessor for Ethernet Destination Address of RQB."*
  `(dpb      (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-0)) (byte 16 32)
	(dpb (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-1)) (byte 16 16)
	     (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-2)))))

(Defmacro SET-ENET-DEST-ADDR (rqb value)
  2"Updates the Ethernet Destination Address of RQB."*
  (Once-Only (Value)
    `(progn
       (setf (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-0) (Net::Swap-Bytes (ldb (byte 16 32) ,value)))
       (setf (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-1) (Net::Swap-Bytes (ldb (byte 16 16) ,value)))
       (setf (aref ,rqb Si:%Io-Rq-Enet-Dest-Addr-2) (Net::Swap-Bytes (ldb (byte 16  0) ,value))))))

(DefSetf RQB-ENET-DEST-ADDR SET-ENET-DEST-ADDR)

(Defmacro 1RQB-ENET-SRC-ADDR* (rqb)
  2"Accessor for Ethernet Destination Address of RQB."*
  `(dpb      (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-0)) (byte 16 32)
	(dpb (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-1)) (byte 16 16)
	     (Net::Swap-Bytes (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-2)))))

(Defmacro SET-ENET-SRC-ADDR (rqb value)
  2"Updates the Ethernet Destination Address of RQB."*
  (Once-Only (Value)
    `(progn
       (setf (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-0) (Net::Swap-Bytes (ldb (byte 16 32) ,value)))
       (setf (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-1) (Net::Swap-Bytes (ldb (byte 16 16) ,value)))
       (setf (aref ,rqb Si:%Io-Rq-Enet-Src-Addr-2) (Net::Swap-Bytes (ldb (byte 16  0) ,value))))))

(DefSetf RQB-ENET-SRC-ADDR SET-ENET-SRC-ADDR)

(Def-RqbField 1ENET-TYPE*   Si:%Io-Rq-Enet-Type  (byte 16 0))



1;;*-------------------------------------------------------------------------------------
1;;           *    1                    *    1RQB <-> PKT COPYING
;;*-------------------------------------------------------------------------------------

1;; Used for quickly copying data between packets and Rqb's:*

(Defsubst 1COPY-RQB-TO-PKT* (Rqb Pkt Count)
  2"Copies COUNT elements from the buffer array of RQB to 16-bit array
   PKT without type checking."*
  
2;;  This needs to add a data type to each word, it could be faster than
;;  copy-array-portion:*
  
2;  (Let* ((Buffer (Si:Rqb-Buffer Rqb))
;*	2 (Array  (Si:Array-Indirect-To Buffer)))
;    (Without-Interrupts
;      (%Blt
;*	2(%Make-Pointer-Offset DTP-FIX Array
;*			2      (+ (Ash (Si:Array-Index-Offset Buffer) -1)
;*				2 (Si:Array-Data-Offset Array)))
;*	2(%Make-Pointer-Offset DTP-FIX Pkt (Si:Array-Data-Offset Pkt))      *	
2;*	2(Ceiling (Max 0 Count) 2) 1)
;      )*				2   ; w/o interrupts
;    )*					2   ; let*
  
2;; * 2Use copy-array-portion for now:*
  
  (Without-Interrupts    
    (Copy-Array-Portion (Si:Rqb-Buffer Rqb) 0 Count Pkt 0 Count))
  
  )					   1; rqb-to-pkt*

(Defsubst 1COPY-PKT-TO-RQB* (Pkt Rqb Count)
  2"Copies COUNT elements from 16-bit array PKT to the buffer array of RQB
   without type checking."*

  (Let* ((Buffer (Si:Rqb-Buffer Rqb))
	 (Array  (Si:Array-Indirect-To Buffer)))
    (Without-Interrupts
      (%Blt
	(%Make-Pointer-Offset DTP-FIX Pkt (Si:Array-Data-Offset Pkt))      	
	(%Make-Pointer-Offset DTP-FIX Array
			      (+ (Ash (Si:Array-Index-Offset Buffer) -1)
				 (Si:Array-Data-Offset Array)))
	(Ceiling (Max 0 Count) 2) 1)
      )					   1; w/o interrupts*
    )					   1; let*
  )					   1; pkt-to-rqb*



1;;*-------------------------------------------------------------------------------------
1;;           *    1                         COMMAND HANDLING
;;*-------------------------------------------------------------------------------------

2;; * 2Note that we are not forced to use the %IO miscop since the Nupi/E
;; * 2supports device queueing (the Nupi doesn't).*

(Defmacro 1WAIT-FOR-COMPLETION* (Rqb &Optional (Wait-String 2"Nupi Wait"*))
  2"Waits for completion of the RQB and checks for errors."*   
  `(Process-Wait
     ,Wait-String
     #'(Lambda (Rqb)
	 (Not (Zerop (Command-Complete Rqb)))) ,Rqb))
 
(Defmacro 1ISSUE-REQUEST* (rqb initiate wait-string &body Set-Cmd-Blk)
  2"Issues an Rqb to the Nupi and waits (if no initiate) for successful
   completion of the request. "*
  
  `(Progn
     ,@Set-Cmd-Blk
     (%NuBus-Write Net::Slot NUPIE-ADDR-REG
		   (Si:Get-Physical-Address ,Rqb Si:%IO-RQ-COMMAND-WORD))
     (Unless ,Initiate			   1; If no initiate, wait*.  
       (Wait-For-Completion ,Rqb ,Wait-String))
     )					   1; progn*
  )					   1; issue request*

(Defmacro 1WITH-RQB-WIRED* (Rqb Nbytes Leave-Wired &body Request-Forms)
  2"Clears the Rqb status word and wires the Rqb into physical memory.
   NBYTES will be wired up to the required number of blocks needed to hold
   nbytes worth of data.  Unless LEAVE-WIRED is t, the Rqb will be automatically
   unwired."*
  
  `(Unwind-Protect
       (Progn
	 (Setf (Status-Word ,Rqb) 0)
	 (Si:Wire-Nupi-Rqb ,Rqb (Ceiling ,Nbytes DISK-BLOCK-BYTE-SIZE) t)
	 ,@Request-Forms)
     (Unless ,Leave-Wired (Si:Unwire-Disk-Rqb ,Rqb))
     )					   1; unwind-protect*
  )					   1; with-rqb-wired*



1;;*-------------------------------------------------------------------------------------
1;;           *    1             *  1  *  1STATUS DESCRIPTOR FIELD ACCESSORS
;;*-------------------------------------------------------------------------------------

1;; *  1Used for the 82586 Configuration Descriptor:*

(Def-RqbField SAV-BF       (+ DESCRIPTOR-DATA  2) (byte 1 7))  ; 1Save Bad Frames in Memory.*

(Def-RqbField ADDR-LEN     (+ DESCRIPTOR-DATA  3) (byte 3 0))  ; 1No. of address bytes.*
(Def-RqbField PREAM-LEN    (+ DESCRIPTOR-DATA  3) (byte 2 4))  ; 1Preamble-Length.*
(Def-RqbField INT-LPBCK    (+ DESCRIPTOR-DATA  3) (byte 1 6))  ; 1Internal-Loopback.*
(Def-RqbField EXT-LPBCK    (+ DESCRIPTOR-DATA  3) (byte 1 7))  ; 1External-Loopback.*

(Def-RqbField LIN-PRIO     (+ DESCRIPTOR-DATA  4) (byte 3 0))  ; 1Linear Priority.*
(Def-RqbField ACR          (+ DESCRIPTOR-DATA  4) (byte 3 4))  ; 1Accelerated Contention Resolution*
(Def-RqbField BOF-MET      (+ DESCRIPTOR-DATA  4) (byte 1 7))  ; 1Exponential Backoff Method.*

(Def-RqbField IF-SPACING   (+ DESCRIPTOR-DATA  5) (byte 8 0))  ; 1Interframe spacing.*
(Def-RqbField SLOT-TIME-LO (+ DESCRIPTOR-DATA  6) (byte 8 0))  ; 1Slot time number*
(Def-RqbField SLOT-TIME-HI (+ DESCRIPTOR-DATA  7) (byte 3 0))  ; 1Slot time number*
(Def-RqbField RETRY-NUM    (+ DESCRIPTOR-DATA  7) (byte 3 4))  ; 1Max no. retries on collisions.*

(Def-RqbField PRM          (+ DESCRIPTOR-DATA  8) (byte 1 0))  ; 1Promiscuous Mode.* 
(Def-RqbField BC-DIS       (+ DESCRIPTOR-DATA  8) (byte 1 1))  ; 1Broadcast Disable.*
(Def-RqbField MANCH-NRZ    (+ DESCRIPTOR-DATA  8) (byte 1 2))  ; 1Manchester or NRZ encoding*
(Def-RqbField TONO-CRS     (+ DESCRIPTOR-DATA  8) (byte 1 3))  ; 1Transmit on no carrier sense.*
(Def-RqbField NCRC-INS     (+ DESCRIPTOR-DATA  8) (byte 1 4))  ; 1No CRC insertion.*
(Def-RqbField CRC-16       (+ DESCRIPTOR-DATA  8) (byte 1 5))  ; 1CRC Type.*
(Def-RqbField BT-STF       (+ DESCRIPTOR-DATA  8) (byte 1 6))  ; 1Bitstuffing.*
(Def-RqbField PAD          (+ DESCRIPTOR-DATA  8) (byte 1 7))  ; 1Padding.*

(Def-RqbField CRSF         (+ DESCRIPTOR-DATA  9) (byte 3 0))  ; 1Carrier Sense Filter.*
(Def-RqbField CRS-SRC      (+ DESCRIPTOR-DATA  9) (byte 1 3))  ; 1Carrier Sense Source.*
(Def-RqbField CDTF         (+ DESCRIPTOR-DATA  9) (byte 3 4))  ; 1Collision-detect filter.*
(Def-RqbField CDT-SRC      (+ DESCRIPTOR-DATA  9) (byte 1 7))  ; 1Collision-detect Source.*

(Def-RqbField MIN-FRM-LEN  (+ DESCRIPTOR-DATA 10) (byte 8 0))  ; 1Minimum frame byte number.

;;  * 1Used for the 82586 Status Descriptor:*

(Def-RqbField RECV-UNIT-ST (+ DESCRIPTOR-DATA  0) (byte 3 4))  1;  Receive unit status.*
(Def-RqbField CMD-UNIT-ST  (+ DESCRIPTOR-DATA  1) (byte 3 0))  1;  Command unit status.*
(Def-RqbField RNR          (+ DESCRIPTOR-DATA  1) (byte 1 4))  1;  The Receive Unit left the ready state.*
(Def-RqbField CNA          (+ DESCRIPTOR-DATA  1) (byte 1 5))  1;  The Command Unit left the active state.*
(Def-RqbField FR           (+ DESCRIPTOR-DATA  1) (byte 1 6))  1;  The Receive Unit finished receiving a frame.*
(Def-RqbField CX           (+ DESCRIPTOR-DATA  1) (byte 1 7))  1;  The Command Unit finished an action command.

;;   Used for the Error Counts Descriptor:*

(Defmacro 1CRC-CNT* (status)
  2"Accessor for the CRC Error Count of Status."*
  `(dpb (ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 1))) (byte 8 8)
	(ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 0)))))

(Defmacro 1ALIGN-CNT* (status)
  2"Accessor for the Alignment Error Count of Status."*
  `(dpb (ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 3))) (byte 8 8)
	(ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 2)))))

(Defmacro 1RI-CNT* (status)
  2"Accessor for the Resource Inavailability Error Count of Status."*
  `(dpb (ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 5))) (byte 8 8)
	(ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 4)))))

(Defmacro 1OVERRUN-CNT* (status)
  2"Accessor for the Overrun Error Count of Status."*
  `(dpb (ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 7))) (byte 8 8)
	(ldb (byte 8 0) (aref ,status (+ DESCRIPTOR-DATA 6)))))

1;;  Used for the Nupi/E and Adaptor Status Descriptor:*

(Def-RqbField BUFF-SZ      (+ DESCRIPTOR-DATA  0) (byte 8 0))  1; Internal Nupi/E buffer size.*
(Def-RqbField LBRTN        (+ DESCRIPTOR-DATA  1) (byte 1 0))  1; Loop back of adapter return field.*
(Def-RqbField VDTR         (+ DESCRIPTOR-DATA  1) (byte 1 1))  1; Status of VDTR control bit.*
(Def-RqbField ROB1         (+ DESCRIPTOR-DATA  1) (byte 1 2))  1; Received Adaptor Status bit 1.*
(Def-RqbField ROB2         (+ DESCRIPTOR-DATA  1) (byte 1 3))  1; Received Adaptor Status bit 2.*
(Def-RqbField ROB3         (+ DESCRIPTOR-DATA  1) (byte 1 4))  1; Received Adaptor Status bit 3.*
(Def-RqbField ROB4         (+ DESCRIPTOR-DATA  1) (byte 1 5))  1; Received Adaptor Status bit 4.*
(Def-RqbField ROB5         (+ DESCRIPTOR-DATA  1) (byte 1 6))  1; Received Adaptor Status bit 5.*
(Def-RqbField ROB6         (+ DESCRIPTOR-DATA  1) (byte 1 7))  1; Received Adaptor Status bit 6.

;;  Used to access Reflectometer Test results:*

(Def-RqbField TIME-LSB     0  (byte 8 0))1  ; LSB of time used to specify distance on the link*
(Def-RqbField TIME-MSB     1  (byte 3 0))1  ; MSB of time used to specify distance on the link*
(Def-RqbField ET-SRT       1  (byte 1 4))1  ; Short on the link identified.*
(Def-RqbField ET-OPN       1  (byte 1 5))1  ; Open on the link identified.*
(Def-RqbField XCVR-PRB     1  (byte 1 6))1  ; Transceiver cable problem identified.*
(Def-RqbField LNK-OK       1  (byte 1 7))1  ; No problem detected on the link.*


1;;*-------------------------------------------------------------------------------------
1;;           *    1        *   1 *       1NUPI/E ETHERNET CONTROLLER
;;*-------------------------------------------------------------------------------------

(Defflavor 1NUPIE-ENC*
	   
	   1;; * 1Operational Flags;*
	   
	   ((Discard-Rcv-Pkts  NIL)	   1; Discard Receive Packets?*
	    
	    1;;  Receive and Transmit RQB's*
	    
	    2;; For transmits and receives, we maintain a monogamous relationship*
	    2;; with two RQB's because of the ridiculous overhead involved in*
	    2;; allocating/deallocating an RQB for every packet:*
	    
	    (Receive-Rqb       ())         1; Allocated at init time*.
	    (Receive-Rqb-Lock  ())	   1; You must have this to access receive-rqb*
	    (Transmit-Rqb      ())         1; Allocated at init time*.
	    (Transmit-Rqb-Lock ()))	   1; You must have this to access transmit-rqb*
	   (Ethernet-Controller-Mixin)
  :Gettable-Instance-Variables
  :Inittable-Instance-Variables
  (:Default-Init-Plist :Board-Type :Nupi/E)
  (:Documentation 2"Nupi/E Ethernet Controller"*)
  )					   1; NUPIE-ENC*


(si:DEFINE-WHEN :ENET

1;;*-------------------------------------------------------------------------------------
1;;           *    1        *              1ERROR HANDLING
;;*-------------------------------------------------------------------------------------

(Defmethod 1(NUPIE-ENC :SIGNAL-ERROR)* (rqb)
  2"Translates and signals Nupi/E errors returned in the status field of RQB"*
  
  (let ((retry-count   (Retry-Count   rqb))       
	(device-error  (Device-Error  rqb))     
	(nupi-error    (Nupi-Error    rqb))       
	(aux           (not (zerop (Aux-Status    rqb))))
	(retry         (not (zerop (Retry-Status  rqb))))
	(err           (not (zerop (Error-Status  rqb)))))
    
    (cond ((not (zerop device-error))
	   (cond
	     
	     1;; Recoverable Ethernet errors:*
	     ((eq device-error #x63)	   1; Net data fifo underrun or overrun.*
	      (Incf Net::Pkts-Lost))
	     ((eq device-error #x85)	   1; Rcv Pkt; insufficient controller memory.*
	      (Incf Net::Pkts-Lost)) 
	     ((eq device-error #xC2)	   1; CRC error on received frame.*
	      (Incf Net::Fcs-Errors))
	     ((eq device-error #xCB)	   1; Xmit Pkt; no carrier detect.*
	      (Incf Net::Transmit-Time-Outs))
	     ((eq device-error #xCC)	   1; Rcv Pkt; alignment error.*
	      (Incf Net::Fcs-Errors))
	     ((eq device-error #xCD)	   1; Xmit Pkt; loss of clear-to-send.*
	      (Incf Net::Transmit-Time-Outs))
	     ((eq device-error #xCE)	   1; Xmit Pkt; defer to link traffic.*
	      (Incf Net::Transmit-Time-Outs))
	     ((eq device-error #xCF)	   1; Rcv Pkt; fewer bits received than minimum.*
	      (Incf Net::Pkts-Lost))
	     ((eq device-error #xD2)	   1; Max no. of packet collisions exceeded.*
	      (Incf Net::Transmit-Time-Outs))
	     
	     1;; Unrecoverable Ethernet errors:*
	     (t (error
		  (case device-error
		    (#x3D       2"Net Nupi request completed with busy still set"*)
		    (#x3E       2"82586 initialization failed"*)
		    (#x3F       2"Nupi HW/SW integrity check failed"*)
		    (#x81       2"Net Nupi request was aborted"*)
		    (#x82       2"An invalid device command was issued to the Nupi/E"*)
		    (#x83       2"An invalid device parameter was issued to the Nupi/E"*)	       
		    (#x8E       2"Unknown Net RQB command completion codes"*)
		    (#xA1       2"Illegal 82586 interrupt"*)
		    (#xD0       2"Transmit Packet: SQE TEST detected"*)
		    (#xD1       2"Received Packet: no EOF flag detected"*)
		    (otherwise  2"Undefined device error code signaled by Nupie/E: ~16r"*))
		  device-error))))
	  
	  ((not (zerop nupi-error))
	   
	   1;; Nupi errors (unrecoverable):*
	   (error
	     (case nupi-error
	       (#x61       2"Nupi/E - Nubus timeout"*)
	       (#x62       2"Nupi/E - Nubus bus error"*)
	       (#x66       2"Illegal bus error trap occurred"*)
	       (#x80       2"Nupi/E device command queue abort"*)
	       (#x81       2"Nupi/E command was aborted"*)
	       (#x82       2"An invalid command was issued to the Nupi/E"*)
	       (#x83       2"An invalid parameter was issued to the Nupi/E"*)
	       (#xA1       2"Nupi/E - Illegal interrupt"*)
	       (#xA5       2"Nupi/E microprocessor software error trap occurred"*)
	       (#xA5       2"Nupi/E microprocessor hardware error trap occurred"*)
	       (#xA7       2"Nupi/E queue overflow occurred"*)
	       (#xA8       2"Nupi/E microprocessor address error trap occurred"*)
	       (#xA9       2"Nupi/E microprocessor illegal instruction error trap occurred"*)
	       (#xAA       2"Nubus DMA locked up; may need HW reset"*)
	       (otherwise  2"Undefined error code signaled by Nupie/E: ~16r"*))
	     nupi-error))
	  
	  (retry
	   (error 2"Nupi/E command failed after ~a retries"* retry-count))
	  (aux
	   (error 2"A special event error was signaled by the Nupi/E"*))
	  (err
	   (error 2"The Nupi/E signaled an unspecified error"*))
	  (t nil)
	  )				   1; cond*
    )					   1; let   *  
  )					   1; signal-error*


1;;*-------------------------------------------------------------------------------------
1;;           *    1        *   1 *           1INITIALIZATIONS
;;*-------------------------------------------------------------------------------------


(Defmethod 1(NUPIE-ENC :AFTER :INIT)* (&rest ignore)
  2"Reads and initializes controller parameters after instantiation."*
  (Let ((Rqb-Size
	  (Ceiling MAX-FRAME-LENGTH DISK-BLOCK-BYTE-SIZE))) 
    (Setf Receive-Rqb  (Si:Get-Disk-Rqb Rqb-Size))  1; Get receive RQB*
    (Setf Transmit-Rqb (Si:Get-Disk-Rqb Rqb-Size))) 1; Get transmit RQB*
  (Setf Ethernet-Address
	(Send Self :Read-Rom-Id))	           1; Get ROM Ethernet address*
  (Send Self :Reset)                               1; Reset controller*
  )					           1; AFTER INIT*

  
(Defmethod 1(NUPIE-ENC :INITIALIZE)* (&rest ignore)
  2"Performs the setup of the Ethernet controller and resets it."*
  (Send Self :Hw-Reset)		           1; Reset the hardware*
  (Send Self :Opflag-Setup)                1; Load Operational Flags*
  (Send Self :Configure)                   1; Load 82586 parameters*
  (Send Self :Ia-Setup)                    1; Load Ethernet address*
  (Send Self :Screen-Setup)                1; Load Screened Protocol Types*
  (Send Self :Mc-Setup)                    1; Load Multicast Addresses*
  )					   1; INITIALIZE*


(Defmethod 1(NUPIE-ENC :HW-RESET)* (&rest ignore)
2  "Completely resets the Ethernet controller hardware."*
  
  (Let ((Rqb (Si:Get-Disk-Rqb)))
    (Unwind-Protect
	(With-Rqb-Wired Rqb DISK-BLOCK-BYTE-SIZE Nil
	  (Issue-Request Rqb Nil 2"Enc Reset"*
	    (Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-RESET)
	    (Setf (Unit-Select        Rqb) ETHERNET-UNIT)
	    )				   1; issue request*
	  (Unless
	    (Zerop (Error-Status Rqb))	   1; check for errors*
	    (Send Self :Signal-Error Rqb))	  
	  )				   1; with rqb wired*      
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   1; unwind-protect*
    )					   1; Let*
  )					   1; HW-RESET*


(Defmethod 1(NUPIE-ENC :OPFLAG-SETUP)* (&Rest ignore)
  2"Sets up the Nupi/E Ethernet operational flags."*
  
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Cfg (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(Progn				   1; Load parameters into descriptor*
	  (Array-Initialize Cfg 0)
	  (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)  OP-DESC-SIZE)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-OP-FLAGS)	  
	  (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         1)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-DATA)
		(Dpb (Net::Numerical-Value Discard-Rcv-Pkts) (byte 1 2)
		     (Dpb (Net::Numerical-Value (Not (Null Valid-Pkt-Types))) (byte 1 0)
			  0)))
	  (Let ((Nbytes (Net::Round-Up-To-Quad  OP-DESC-SIZE)))
	    (With-Rqb-Wired Rqb Nbytes Nil 1; Issue the RQB:*
	      (Issue-Request Rqb Nil 2"Enc Flags"*
		(Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
		(Setf (Unit-Select        Rqb) ETHERNET-UNIT)
		(Setf (Byte-Xfer-Count    Rqb) Nbytes)
		)			   1; issue request*	      
	      1;; Check for errors:*
	      (Unless (Zerop (Error-Status Rqb))
		(Send Self :Signal-Error Rqb))
	      )				   1; with rqb wired*
	    )				   1; let*
	  )				   1; progn*
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   1; unwind-protect*
    )					   1; let*
  )					   1; OPFLAG-SETUP*

(Defmethod 1(NUPIE-ENC :READ-ROM-ID)* (&Aux Ethernet-ID)
  2"Returns the Ethernet ID from config rom"*
  (Setf Ethernet-Id 0)

2  ;; When you issue a status, you get the last enet-id that we loaded
  ;; into the board, which *may not* be what is actually in config rom
  ;; (especially with Decnet loaded):
  
;    (Setf Status (Send Self :Board-Status NUPIE-INDIVIDUAL-ADDR))
;    (Setf Enet-Addr-Len (Ldb (Byte 8 0) (Aref Status 4)))
;    (Dotimes (I Enet-Addr-Len) 
;      (Setf Ethernet-Id
;*	2    (Dpb (Aref Status (+ DESCRIPTOR-DATA i))
;*		2 (Byte 8 (* 8 (- Enet-Addr-Len I 1))) Ethernet-Id)))    
  
  ;; So read it directly from config rom and have the hardware guys 
  ;; drawn and quartered when they attempt to change it's location:*

  (Dotimes (I Enet-Addr-Len)		   1; Read Ethernet ID from config rom*
    (Setf Ethernet-Id
	  (Dpb (Si:%Nubus-Read-8B Net::Slot (+ NUPIE-ID-ADDR (Ash I 2)))
	       (Byte 8 0) (Ash Ethernet-Id 8))))
  (When (Or (Zerop Ethernet-Id)	           1; Check for validity*
	    (Eql Ethernet-Id BROADCAST-ADDRESS))
    (Error nil 2"Nupi/E Board Config ROM has bad Ethernet address: ~16r"* Ethernet-Id))
  Ethernet-Id)


(Defmethod (NUPIE-ENC :IA-SETUP) (&Optional Ethernet-Id)
  "Sets up the controller with the Ethernet Address ID. "
  
  (When Ethernet-Id
    (Setf Ethernet-Address Ethernet-Id))   ; Set instance variable to valid ID
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Cfg (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(Progn				   ; Load parameters into descriptor
	  (Array-Initialize Cfg 0)
	  (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)  IA-DESC-SIZE)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-INDIVIDUAL-ADDR)	  
	  (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         (Net::Numerical-Value Enet-Addr-Len))
	  
;          Note: The "least significant byte" as described in the spec is referred
;	       to as the "high order octet" in the documentation supplied with
;	       ethernet addresses -- that's why it looks like we're trying to
;                 load the ethernet address backwards:

	  (Dotimes (I (Net::Numerical-Value Enet-Addr-Len))
	    (Setf (Descriptor-Byte Cfg (+ I DESCRIPTOR-DATA))
		  (Ldb (Byte 8 (* (- Enet-Addr-Len I 1) 8)) Ethernet-Address)))
	  (Let ((Nbytes (Net::Round-Up-To-Quad  IA-DESC-SIZE)))
	    (With-Rqb-Wired Rqb Nbytes Nil ; Issue the RQB:
	      (Issue-Request Rqb Nil "Enc IA Setup"
		(Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
		(Setf (Unit-Select        Rqb) ETHERNET-UNIT)
		(Setf (Byte-Xfer-Count    Rqb) Nbytes)
		)			   ; issue request	      
	      ;; Check for errors:
	      (Unless (Zerop (Error-Status Rqb))
		(Send Self :Signal-Error Rqb))
	      )				   ; with rqb wired
	    )				   ; let
	  )				   ; progn
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   ; unwind-protect
    )					   ; Let
  ;; Return t to show that the Ethernet address was successfully reset. *BJ*
  t 
  )					   1; IA-SETUP*

(Defmethod 1(NUPIE-ENC :CONFIGURE)* (&rest ignore)
  2"Sets up the controller board with Ethernet configuration parameters."*
  
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Cfg (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(Progn				   1; Load parameters into descriptor*
	  (Array-Initialize Cfg 0)
	  (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)  CFG-DESC-SIZE)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-82586-CONFIG)
	  (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         1)	  

	  1;; Note that all Host-Interface parameters are defined by*
	  1;; the Nupi/E firmware rather than Lisp.*

	  (Setf (Sav-Bf          Cfg) (Net::Numerical-Value Enet-Sav-Bf))
	  (Setf (Addr-Len        Cfg) (Net::Numerical-Value Enet-Addr-Len))
	  (Setf (Pream-Len       Cfg) (- (Integer-Length
					   (Net::Numerical-Value Enet-Pream-Len)) 2.))
	  (Setf (Int-Lpbck       Cfg) (Net::Numerical-Value Enet-Int-Lpbck))
	  (Setf (Ext-Lpbck       Cfg) (Net::Numerical-Value Enet-Ext-Lpbck))
	  (Setf (Lin-Prio        Cfg) (Net::Numerical-Value Enet-Lin-Prio))
	  (Setf (Acr             Cfg) (Net::Numerical-Value Enet-Acr))
	  (Setf (Bof-Met         Cfg) (Net::Numerical-Value Enet-Bof-Met))
	  (Setf (If-Spacing      Cfg) (Net::Numerical-Value Enet-If-Spacing))
	  (Setf (Slot-Time-Lo    Cfg) (ldb (byte  8 0) (Net::Numerical-Value Enet-Slot-Time)))
	  (Setf (Slot-Time-Hi    Cfg) (ldb (byte  8 8) (Net::Numerical-Value Enet-Slot-Time)))
	  (Setf (Retry-Num       Cfg) (Net::Numerical-Value Enet-Retry-Num))
	  (Setf (Prm             Cfg) (Net::Numerical-Value Enet-Prm))
	  (Setf (Bc-Dis          Cfg) (Net::Numerical-Value Enet-Bc-Dis))
	  (Setf (Manch-Nrz       Cfg) (Net::Numerical-Value Enet-Manch))
	  (Setf (Tono-Crs        Cfg) (Net::Numerical-Value Enet-Tono-Crs))
	  (Setf (Ncrc-Ins        Cfg) (Net::Numerical-Value Enet-Ncrc-Ins))
	  (Setf (Crc-16          Cfg) (Net::Numerical-Value Enet-Crc-16))
	  (Setf (Bt-Stf          Cfg) (Net::Numerical-Value Enet-Bt-Stf))
	  (Setf (Pad             Cfg) (Net::Numerical-Value Enet-Pad))
	  (Setf (Crsf            Cfg) (Net::Numerical-Value Enet-Crsf))
	  (Setf (Crs-Src         Cfg) (Net::Numerical-Value Enet-Crs-Src))
	  (Setf (Cdtf            Cfg) (Net::Numerical-Value Enet-Cdtf))
	  (Setf (Cdt-Src         Cfg) (Net::Numerical-Value Enet-Cdt-Src))
	  (Setf (Min-Frm-Len     Cfg) (Net::Numerical-Value Enet-Min-Frm-Len))
	  
	  (Let ((N-bytes (Net::Round-Up-To-Quad  CFG-DESC-SIZE)))
	    (With-Rqb-Wired Rqb N-bytes Nil
	      (Issue-Request Rqb Nil 2"Enc Config"*
		(Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
		(Setf (Unit-Select        Rqb) ETHERNET-UNIT)
		(Setf (Byte-Xfer-Count    Rqb) N-bytes)
		)			   1; issue request*	      
	      1;; Check for errors:*
	      (Unless (Zerop (Error-Status Rqb))
		(Send Self :Signal-Error Rqb))
	      )				   1; with rqb wired*
	    )				   1; let*
	  )				   1; progn*
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   1; unwind-protect*
    )					   1; Let*
  )					   1; CONFIGURE*


(Defmethod 1(NUPIE-ENC :SCREEN-SETUP)* (&rest ignore)
  2"Sets up the controller with the valid protocol types.  Each received
   packet is screened against these types by the hardware."*
  
  (When Valid-Pkt-Types
    (Let* ((Rqb (Si:Get-Disk-Rqb))
	   (Cfg (Si:Rqb-8-Bit-Buffer Rqb))
	   (Mf-Count (List-Length Valid-Pkt-Types))
	   (Data-Size (* Mf-Count 2))
	   (Block-Size (+ Data-Size DESCRIPTOR-DATA)))
      
      (Unwind-Protect
	  (Progn			   1; Load parameters into descriptor*
	    (Array-Initialize Cfg 0)	    
	    (If (> Mf-Count MAX-MF-TYPES)
		(Error nil 2"Too many protocol types to be screened; ~a protocols, ~a allowed."*
		       Mf-Count MAX-MF-TYPES))	    
	    (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)  Block-Size)
	    (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-MATCH-FIELDS)	      
	    (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         2)	    
	    (Dotimes (N Mf-Count)
	      (Let ((Match-Field (Nth N Valid-Pkt-Types))
		    (Mf-Pointer (+ DESCRIPTOR-DATA (* N 2))))
		
		1;; Load each match field into Descriptor Block, LSB first:*		
		(Setf (Descriptor-Byte Cfg Mf-Pointer)
		      (ldb (byte 8 0) Match-Field))
		(Setf (Descriptor-Byte Cfg (+ Mf-Pointer 1))
		      (ldb (byte 8 8) Match-Field))
		)			   1; let*
	      )				   1; Dotimes *	    
	    (Let ((N-bytes (Net::Round-Up-To-Quad  Block-Size)))
	      (With-Rqb-Wired Rqb N-bytes Nil
		(Issue-Request Rqb Nil 2"Enc Screening"*
		  (Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
		  (Setf (Unit-Select        Rqb) ETHERNET-UNIT)
		  (Setf (Byte-Xfer-Count    Rqb) N-bytes)
		  )			   1; issue request*	      
		1;; Check for errors:*
		(Unless (Zerop (Error-Status Rqb))
		  (Send Self :Signal-Error Rqb))
		)			   1; with rqb wired*
	      )				   1; let*
	    )				   1; progn*
	(When Rqb (Si:Return-Disk-Rqb Rqb))
	)				   1; unwind-protect*
      )					   1; Let*
    )					   1; when valid-pkt-types*
  )					   1; SCREEN-SETUP*


(Defmethod 1(NUPIE-ENC :MC-SETUP)* (&Optional New-List)
2  "Sets up the controller with valid multicast addresses. If NEW-LIST is not
   specified, the instance variable MC-ADDR-LIST is used, else MC-ADDR-LIST is
   updated to NEW-LIST. An empty MC-ADDR-LIST  disables reception of
   any frame with a multicast address."*
  
  (When (Setf Mc-Addr-List (Or New-list Mc-Addr-List))
    (Let* ((Rqb (Si:Get-Disk-Rqb))
	   (Cfg (Si:Rqb-8-Bit-Buffer Rqb))
	   (Addr-Count (List-Length MC-Addr-List))
	   (Addr-Data-Size (* Addr-Count (Net::Numerical-Value Enet-Addr-Len)))
	   (Block-Size (+ Addr-Data-Size DESCRIPTOR-DATA)))
      (Unwind-Protect
	  (Progn			   1; Load parameters into descriptor*
	    (Array-Initialize Cfg 0)	      
	    (If (> Addr-Count MAX-MC-ADDR)
		(Error nil 2"Multicast address list is too large; ~a addresses, ~a allowed."*
		       Addr-Count MAX-MC-ADDR))	    
	    (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)       Block-Size)
	    (Setf (Descriptor-Byte Cfg (+ DESC-BLOCK-LENGTH 1)) (Ldb (byte 8  8) Block-Size))
	    (Setf (Descriptor-Byte Cfg (+ DESC-BLOCK-LENGTH 2)) (Ldb (byte 8 16) Block-Size))
	    (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-MULTICAST-ADDR)	      
	    (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         (Net::Numerical-Value Enet-Addr-Len))	    
	    (Dotimes (N Addr-Count)
	      (Let ((Enet-Addr (Nth N Mc-Addr-List))
		    (Addr-Pointer (+ DESCRIPTOR-DATA (* N (Net::Numerical-Value Enet-Addr-Len)))))
		
		1;; Load each address into Descriptor Block, LSB first:*		
		(If (Not (Logbitp (* 8 (- (Net::Numerical-Value Enet-Addr-Len) 1)) Enet-Addr))
		    (Error nil 2"Illegal multicast address; first octet lsb is zero: ~16R"* Enet-Addr))
		(Dotimes (J (Net::Numerical-Value Enet-Addr-Len))
		  (Setf (Descriptor-Byte Cfg (+ J Addr-Pointer))
			(ldb (byte 8 (* (- Enet-Addr-Len J 1) 8)) Enet-Addr))
		  )			   1; Dotimes addr-len*
		)			   1; let*
	      )				   1; Dotimes addr-count*
	    (Let ((N-bytes (Net::Round-Up-To-Quad  Block-Size)))
	      (With-Rqb-Wired Rqb N-bytes Nil
		(Issue-Request Rqb Nil 2"Enc Multicast"*
		  (Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
		  (Setf (Unit-Select        Rqb) ETHERNET-UNIT)
		  (Setf (Byte-Xfer-Count    Rqb) N-bytes)
		  )			   1; issue request*	      
		1;; Check for errors:*
		(Unless (Zerop (Error-Status Rqb))
		  (Send Self :Signal-Error Rqb))
		)			   1; with rqb wired*
	      )				   1; let*
	    )				   1; progn*
	(When Rqb (Si:Return-Disk-Rqb Rqb))
	)				   1; unwind-protect*
      )					   1; let*
    )					   1; when mc-addr-list*  
  )					   1; MC-SETUP*



1;;*-------------------------------------------------------------------------------------
1;;           *    1    * 1   *           1DATALINK INTERFACE
;;*-------------------------------------------------------------------------------------


(Defmethod 1(NUPIE-ENC :TRANSMIT)* (Type Dest Data-Array N-Bytes
				      &Optional (Deallocate T))
2  "Transmit the 16-bit DATA-ARRAY to Ethernet address DEST.
   N-BYTES    = Number of bytes to transmit.
   TYPE       = Ethernet Frame type, :CHAOS or numeric Ethernet type code.
   DEALLOCATE = T for arrays that must be freed after transmission."*
  
  (Transforming-Arguments Type Dest Data-Array N-Bytes Deallocate
    (Let ((N-Words (Net::Convert-To-Words N-bytes))           
	  (Rqb-Bytes (Net::Round-Up-To-Quad N-bytes)))      
      (With-Lock (Transmit-Rqb-Lock)                      1; Lock* 1the Rqb from other processes*
	(When (Not (Zerop (Busy Transmit-Rqb)))
	  (Wait-For-Completion Transmit-Rqb))             1; Wait for last transmit to finish*
	(Unless (Zerop (Error-Status Transmit-Rqb))       1; Check for errors*
	  (Send Self :Signal-Error Transmit-Rqb))
	(Incf Collision-Count (Retry-Count Transmit-Rqb)) 1; Update statistics*
	(Si:Unwire-Disk-Rqb Transmit-Rqb)                 1; Unwire Rqb for next packet*
	(Copy-Pkt-To-Rqb Data-Array Transmit-Rqb N-Words) 1; Copy data for next packet*
	(With-Rqb-Wired Transmit-Rqb Rqb-Bytes T          1; Wire Rqb with new packet*
	  (Issue-Request                                  1; Initiate next transmit*
	    Transmit-Rqb T 2"Enc Transmit"*
	    (Setf (Command            Transmit-Rqb) SI:%NUPIE-COMMAND-ENET-TRANSMIT)
	    (Setf (Unit-Select        Transmit-Rqb) ETHERNET-UNIT)
	    (Setf (Byte-Xfer-Count    Transmit-Rqb) Rqb-Bytes)
	    (Setf (Rqb-Enet-Dest-Addr Transmit-Rqb) Dest)
	    (Setf (Rqb-Enet-Src-Addr  Transmit-Rqb) Ethernet-Address)
	    (Setf (Enet-Type          Transmit-Rqb) Type)
	    )				   1; issue request*	  
	  )				   1; with rqb wired*
	)				   1; with-lock*
      )					   1; Let*   
    )					   1; with transformed args*
  )					   1; Transmit*


(Defmethod 1(NUPIE-ENC :RECEIVE)* (&rest ignore)
2  "Returns the DESTINATION, SOURCE, TYPE and DATA for the next valid frame.
   Data is returned as a 16-bit array."*
  
  (Let (Array M-bytes N-words Destination Source Type)    
    (Block Receive
      (With-Lock (Receive-Rqb-Lock)	                   1; Lock the rqb from other processes*
	(Block Retry-Loop
	  (Loop
	    (With-Rqb-Wired
	      Receive-Rqb (Net::Round-Up-To-Quad MAX-FRAME-LENGTH) Nil
	      (Issue-Request Receive-Rqb Nil 2"Network Wait"*
		(Setf (Command            Receive-Rqb) SI:%NUPIE-COMMAND-ENET-RECEIVE)	  
		(Setf (Unit-Select        Receive-Rqb) ETHERNET-UNIT))
	      (If (Not (Zerop		                   1; Check for errors*
			 (Error-Status Receive-Rqb)))
		  (Send Self :Signal-Error Receive-Rqb))   1; Move back one and roll again. *
	      (Progn			                   1; Else, read data from the returned Rqb:*
		(Setf M-Bytes     (Byte-Xfer-Count    Receive-Rqb))
		(Setf N-words     (Net::Convert-To-Words   M-bytes))
		(Setf Type        (Enet-Type          Receive-Rqb))
		(Setf Destination (Rqb-Enet-Dest-Addr Receive-Rqb))
		(Setf Source      (Rqb-Enet-Src-Addr  Receive-Rqb))		    
		(If (> M-Bytes MAX-FRAME-LENGTH)
		    (Incf Net::Pkts-Too-Big-To-Receive)
		    (When (Member Type Valid-Pkt-Types)	   1; Is this a protocol we support?*
		      (Return-From Retry-Loop)))           1; Then pass Go, collect $200.*
		)			   1; progn*		  
	      )				   1; with-rqb-wired*	    
	    )				   1; loop*
	  )				   1; retry-loop*	
	(Setf Array (Allocate-Net-Packet Type))            1; Get a packet array*
	(Copy-Rqb-To-Pkt Receive-Rqb Array N-Words)        1; Copy data buffer*
	)				   1; with lock*      
      (Return-From Receive Destination Source Type Array M-bytes)
      )					   1; receive-block*
    )					   1; Let*   
  )					   1; Receive*

 

1;;*-------------------------------------------------------------------------------------
1;;           *    1        *      1  *       1DISPLAYS
;;*-------------------------------------------------------------------------------------


(Defmethod 1(NUPIE-ENC :BOARD-STATUS)* (Descriptor)
2  "Returns the status that the Nupi/E and the 82586 have accumulated on
   the Ethernet operations since the last reset or issuance of this command"*
  
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Status (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(With-Rqb-Wired Rqb DISK-BLOCK-BYTE-SIZE Nil
	  (Issue-Request Rqb Nil 2"Enc Status"*
	    (Setf (Command            Rqb) SI:%NUPIE-COMMAND-ENET-STATUS)
	    (Setf (Unit-Select        Rqb) ETHERNET-UNIT)
	    (Setf (Dev-Blk-Addr       Rqb) Descriptor)
	    (Setf (Byte-Xfer-Count    Rqb) DISK-BLOCK-BYTE-SIZE)
	    )				   1; issue request*
	  
	  1;; Check for errors:*	   
	  (Unless (Zerop (Error-Status Rqb))
	    (Send Self :Signal-Error Rqb))	  
	  )				   1; with rqb wired*
      1;; unwind-protect forms:*
      (When Rqb (Si:Return-Disk-Rqb Rqb)))
    Status)				   1; Let*   
  )					   1; Status*

(Defmethod 1(NUPIE-ENC :PRINT-ETHERNET-ID)* (&Optional (Stream *Standard-Output*)
				       &Rest Ignore)
  2"Prints the Ethernet ID loaded into this controller"*
  
  (Let ((Enet-Address 0)
	Status Len)
    (Setf Status (Send Self :Board-Status NUPIE-INDIVIDUAL-ADDR))
    (Setf len (aref status 4))		   1; size of address*
    (Dotimes (i len) 
      (Setf Enet-Address
	    (dpb (aref status (+ DESCRIPTOR-DATA i))
		 (byte 8 (* 8 (- len i 1))) Enet-Address)))
    (Format Stream 2"~%~% * 2Ethernet Address loaded into this controller: #x~16r"*
	    Enet-Address)))


(Defmethod 1(NUPIE-ENC :PRINT-586-CONFIG)E **(&Optional (Stream *Standard-Output*)
				      &Rest Ignore &Aux Status)
  2"Prints the configuration of the 82586 Lan Processor in this controller"*
  
    (Setf status (Send Self :Board-Status NUPIE-82586-CONFIG))   
    (Format Stream 2"~2%  82586 LAN Processor configuration:"*)
    
    (Format Stream "~%    Save Bad Frames in Memory is ~a"         (Net::On-Off (Sav-Bf status)))      
    (Format Stream "~%    Number of address bytes = ~a"            (Addr-Len status))
    (Format Stream "~%    Preamble Length = ~a bytes."             (expt 2 (+ 1 (Pream-Len status))))
    (Format Stream "~%    Internal Loopback is ~a"                 (Net::On-Off (Int-Lpbck status)))
    (Format Stream "~%    External Loopback is ~a"                 (Net::On-Off (Ext-Lpbck status)))      
    (Format Stream "~%    Linear Priority = ~a"                    (Lin-Prio status))
    (Format Stream "~%    Accelerated Contention Resolution = ~a"  (Acr status))
    (Format Stream "~%    Exponential Backoff Method is ~a"        (Net::On-Off (Bof-Met status)))
    (Format Stream "~%    Interframe spacing = ~a"                 (If-Spacing status))      
    (Format Stream "~%    Slot time number = ~a"   (dpb (Slot-Time-Hi status)
						  (byte 3 8) (Slot-Time-Lo status)))      
    (Format Stream "~%    Max no. retries on collisions = ~a"      (Retry-Num status))      
    (Format Stream "~%    Promiscuous Mode is ~a"                  (Net::On-Off (Prm status))) 
    (Format Stream "~%    Broadcast Disable is ~a"                 (Net::On-Off (Bc-Dis status)))
    (Format Stream "~%    Encoding = ~a" (if (zerop (Manch-Nrz status)) "NRZ" "Manchester"))
    (Format Stream "~%    Transmit on no carrier sense is ~a"      (Net::On-Off (Tono-Crs status)))
    (Format Stream "~%    CRC insertion is ~a" (if (zerop (Ncrc-Ins status)) "ON" "OFF"))
    (Format Stream "~%    CRC Type = ~a" (if (zerop (Crc-16 status)) "32 bit" "16 bit"))
    (Format Stream "~%    Bitstuffing is ~a"                       (Net::On-Off (Bt-Stf status)))
    (Format Stream "~%    Padding is ~a"                           (Net::On-Off (Pad status)))      
    (Format Stream "~%    Carrier Sense Filter = ~a"               (Crsf status))
    (Format Stream "~%    Carrier Sense Source = ~a" (if (zerop (Crs-Src status))
						   "External" "Internal"))
    (Format Stream "~%    Collision detect filter = ~a"            (Cdtf status))
    (Format Stream "~%    Collision detect Source = ~a" (if (zerop (Cdt-Src status))
						      "External" "Internal"))      
    (Format Stream "~%    Minimum frame length = ~a bytes."        (Min-Frm-Len status)))
  
  
(Defmethod 1(NUPIE-ENC :PRINT-586-STATUS) *(&Optional (Stream *Standard-Output*)
				       &Rest Ignore &Aux Status)
  2"Prints the status of the 82586 Lan Processor in this controller"*
  
    (Setf Status (Send Self :Board-Status NUPIE-82586-STATUS))    
    (Format Stream 2"~2%  Status of the 82586 LAN processor:"*)
    (Format Stream 2"~%  * 2 Receive Unit Status = ~a."*
	    (case (Recv-Unit-St status)
	      (0           "IDLE")
	      (1           "SUSPENDED")
	      (2           "NO RESOURCES")
	      (4           "READY")
	      (otherwise   "Undefined state")))
    (Format Stream 2"~%    Command Unit Status = ~a"*
	    (case (Cmd-Unit-St status)
	      (0           "IDLE")
	      (1           "SUSPENDED")
	      (2           "ACTIVE")
	      (otherwise   "Undefined state")))
    (when (not (zerop (Rnr status)))
      (Format Stream 2"~%    The Receive Unit Left the READY state."*))
    (when (not (zerop (Cna status)))
      (Format Stream 2"~%    The Command Unit Left the ACTIVE state."*))
    (when (not (zerop (Fr status)))
      (Format Stream 2"~%    The Receive Unit finished receiving a frame."*))
    (when (not (zerop (Cx status)))
      (Format Stream 2"~%    The Command Unit executed an action command w/ its int. bit set."*)))

	   
(Defmethod 1(NUPIE-ENC :PRINT-586-DUMP)* (&Optional (Stream *Terminal-Io*)
				     &Rest ignore)
  2"Prints a synopsis of the internal status of the 82586 controller chip"*
  
  (Send Self :Print-586-Config Stream)
  (Send Self :Print-586-Status Stream)
  (Send Self :Print-Error-Counts Stream))
    

(Defmethod 1(NUPIE-ENC :PRINT-STATUS)* (&Optional (Stream *Terminal-Io*)
				   &Rest ignore)
  (Format t 2"~:|~3% Status of Nupi/E controller in slot ~16r:"* Net::Slot)
  (Send Self :Print-Ethernet-Id Stream)
  (Send Self :Print-586-Dump Stream))


(Defmethod 1(NUPIE-ENC :PRINT-ERROR-COUNTS) *(&Optional (Stream *Terminal-Io*) Reset-P
					 &Aux Status)
  2"Prints the error* 2counts of the 82586 Lan Processor in this controller"*

    (Setf Status (Send Self :Board-Status NUPIE-ERROR-COUNTS))   
    (Format Stream 2"~2%    CRC Error Count = ~d."* (Crc-Cnt status))
    (Format Stream 2"~%    Alignment Error Count = ~d."* (Align-Cnt status))
    (Format Stream 2"~%    Resource Inavailability Error Count = ~d."* (Ri-Cnt status))
    (Format Stream 2"~%    Overrun Error Count = ~d."* (Overrun-Cnt status))
    (When Reset-P (Send Self :Update-Stats)))


(Defmethod 1(NUPIE-ENC  :UPDATE-STATS)* (&Aux Status)
2  "Updates instance variables from error statistics recorded by the 82586."*
  
  (Setf Status (Send Self :Board-Status NUPIE-ERROR-COUNTS))   
  (Without-Interrupts
    (Incf Net::Fcs-Errors			   1; Increment FCS by CRC and Alignment errors.*
	  (+ (Crc-Cnt Status) (Align-Cnt Status)))
    (Incf Net::Pkts-Lost			   1; Increment Packets-Lost by Resource and Overrun errors.*
	  (+ (Ri-Cnt Status) (Overrun-Cnt Status))))
    
  1;; Clear the board's count fields:*
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Cfg (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(Progn				   1; Load parameters into descriptor*
	  (Array-Initialize Cfg 0)
	  (Setf (Descriptor-Byte Cfg DESC-BLOCK-LENGTH)  CEC-DESC-SIZE)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-CODE)    NUPIE-ERROR-COUNTS)	  
	  (Setf (Descriptor-Byte Cfg ENTRY-SIZE)         0)
	  (Setf (Descriptor-Byte Cfg DESCRIPTOR-DATA)    0)
	  
	  (With-Rqb-Wired Rqb DISK-BLOCK-BYTE-SIZE Nil
	    (Issue-Request Rqb Nil 2"Clear Error Counts"*
	      (Setf (Command          Rqb) SI:%NUPIE-COMMAND-ENET-SETUP)
	      (Setf (Unit-Select      Rqb) ETHERNET-UNIT)
	      (Setf (Byte-Xfer-Count  Rqb) 8)
	      )				   1; issue request*
	    1;; Check for errors:*	   
	    (Unless (Zerop (Error-Status Rqb))
	      (Send Self :Signal-Error Rqb))	  
	    )				   1; with rqb wired*     
	  )				   1; progn*
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   1; unwind-protect*
    )					   1; let*
  )					   1; UPDATE-STATS*


1;;*-------------------------------------------------------------------------------------
1;;           *    1        *                1DIAGNOSTICS
;;*-------------------------------------------------------------------------------------


(Defmethod 1(NUPIE-ENC :MEMORY-DUMP)* (&Rest ignore &aux buffer)
  2"Dumps the internal registers of the 82586 to the screen in raw form."*
  
  (format t "~:|")			   ;1clear the screen.*
  (format t 2"~2%  Internal register dump of 82586 LAN Processor in slot #x~x:"* Net::Slot)
  (format t "~%")
  (Setf Buffer (Send self :Board-Status NUPIE-82586-DATA))
  (dotimes (i 34)
    (format t "~%   Byte ~16,2,48R: ~16,2,48R"        i  (char-int (aref buffer (+   8 i))))
    (format t "     Byte ~16,2,48R: ~16,2,48R" (+  34 i) (char-int (aref buffer (+  42 i))))
    (format t "     Byte ~16,2,48R: ~16,2,48R" (+  68 i) (char-int (aref buffer (+  76 i))))
    (format t "     Byte ~16,2,48R: ~16,2,48R" (+ 102 i) (char-int (aref buffer (+ 110 i))))
    (format t "     Byte ~16,2,48R: ~16,2,48R" (+ 136 i) (char-int (aref buffer (+ 144 i)))))	    
  )					   1; memory-dump

2;;**--------------------------------------------------------------------------------
2;;   SELF TESTS:
;;*
 
(Defmethod 1(NUPIE-ENC :SELFTEST)* (&rest ignore)
  2"Executes the Nupi/E selftest on the 82586 and its associated logic.
   After completion of the command, the Nupi/E reinitializes the 82586
   with the powerup default parameters."*

  (Format t 2"~2% Executing Nupi/E Ethernet selftest (slot #x~x):"* Net::Slot)
  (Let ((Rqb (Si:Get-Disk-Rqb)))
    (Unwind-Protect
	(With-Rqb-Wired Rqb DISK-BLOCK-BYTE-SIZE Nil
	  (Issue-Request Rqb Nil 2"Enc Selftest"*
	    (Setf (Command     Rqb) SI:%NUPIE-COMMAND-ENET-SELFTEST)
	    (Setf (Unit-Select Rqb) ETHERNET-UNIT)
	    )				   1; issue request*	  
	  1;; Check for errors:*	   
	  (Unless (Zerop (Error-Status Rqb))
	    (Send Self :Signal-Error Rqb))	  
	  )				   1; with rqb wired*      
      (When Rqb (Si:Return-Disk-Rqb Rqb))
      )					   1; unwind-protect*
    1;; If we get this far without signalling an error, selftest passed.*
    (Format t 2"~%*  2Nupi/E Ethernet selftest completed successfully."*)
    )					   1; Let*   
  )					   1; Selftest

2;;**--------------------------------------------------------------------------------
2;;   REFLECTOMETER TEST:
;;*

(Defmethod 1(NUPIE-ENC :REFLECTOMETER-TEST)* (&rest ignore)
  2"Performs a Time Domain Reflectometer test on the serial link to help
   identify shorts (or opens) and location."*
  
  (Format t 2"~2% Executing Time Domain Reflectometer Test (Slot #x~x):"* Net::Slot)
  (Let* ((Rqb (Si:Get-Disk-Rqb))
	 (Result (Si:Rqb-8-Bit-Buffer Rqb)))
    (Unwind-Protect
	(With-Rqb-Wired Rqb DISK-BLOCK-BYTE-SIZE Nil
	  (Issue-Request Rqb Nil 2"Enc TDR Test"*
	    (Setf (Command          Rqb) SI:%NUPIE-COMMAND-ENET-TDR)
	    (Setf (Unit-Select      Rqb) ETHERNET-UNIT)
	    (Setf (Byte-Xfer-Count  Rqb) 2)
	    )				   1; issue request*
	  1;; Check for errors:*	   
	  (Unless (Zerop (Error-Status Rqb))
	    (Send Self :Signal-Error Rqb))	  
	  )				   1; with rqb wired*     
      
      1;; Translate and display results:*
      
      (If (Zerop (Lnk-Ok result))
	  (Let ((show-distance nil))
	    (cond
	      ((not (zerop (Et-Srt result)))
	       (setf show-distance t)
	       (format t 2"~%   A short has been detected on the link --"*))
	      ((not (zerop (Et-Opn result)))
	       (setf show-distance t)
	       (format t 2"~%   An open has been detected on the link --"*))
	      ((not (zerop (Xcvr-Prb result)))
	       (format t 2"~%   A Transceiver Cable problem has been detected!"*))
	      (t (format t 2"~%   An unspecified link problem has been detected!"*)))
	    (when show-distance
	      (format t 2"~%    it is located ~a transmit clocks away."*
		      (dpb (byte 3 0) (Time-Msb result)
			   (Time-Lsb result)))))
	  (format t 2"~%   No problem was detected on the link."*)
	  )				   1; if*	  
      1;; unwind-protect forms:*
      (When Rqb (Si:Return-Disk-Rqb Rqb)))
    )					   1; Let*   
  )					   1; Reflectometer-Test*


    
1;;*--------------------------------------------------------------------------------
1;;                *     1  *       1RELEASE 3 COMPATIBILITY
;;*--------------------------------------------------------------------------------

(Defmethod 1(NUPIE-ENC :TRANSMIT-FAST)* (Type Dest Data-Array N-Bytes)
  "Call :TRANSMIT with Deallocate set to nil."
  (Send Self :Transmit Type Dest Data-Array N-Bytes Nil)
  ) 1; transmit-fast*


(Net::Define-Controller-Board "NPE" 'Ethernet::NUPIE-ENC)
(Compile-Flavor-Methods NUPIE-ENC)

;; End of DEFINE-WHEN
)
