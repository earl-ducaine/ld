;; -*- Mode:Common-Lisp; Package:ETHERNET; Base:10.; Fonts:(MEDFNT HL12B HL12I); -*-

;;;
;;;                           RESTRICTED RIGHTS LEGEND
;;;
;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

1;;*--------------------------------------------------------------------------------
1;;           *          1DRIVER FOR THE NUBUS ETHERNET CONTROLLER BOARD
2;;
;;  Maintenance History:
;;
;;  16 SEP 87  MMG -- Rewrite for Explorer II compatibility.
;;  26 JAN 88  ab  ** 2-- Add DEFINE-WHEN load-time conditionals.


1;;**--------------------------------------------------------------------------------
1;;*                                  1CONSTANTS
;;*--------------------------------------------------------------------------------

2;; Standard constants for this controller*

(Defconstant  1*NULL-STATUS**  1    * #x0000)	   1; A status word with everything set to nil*
(Defconstant  1*NORMAL-FINISH*  *   #xA000)	   1; Normal, correct completion.*
(Defconstant  1*NULL-COMMAND**    #x0000)	   1; A Null command.*
(Defconstant  1*NULL-CMD-EOL**     #x8000)	   1; A Null command with End-Of-List on.*
(Defconstant  1*NULL-PTR**         #xFFFF)	   1; An 82586 null pointer.

2;; Fixed locations within the NuBus Ether Controller memory**

(Defconstant 1*CHANNEL-ATTENTION-ADDR**     #x8000     2"Hit this address to assert CA"*)  
(Defconstant 1*EVENT-ADDRESS-REGISTER* *     #xA000     2"Contains address for event register"*) 
(Defconstant 1*CONFIG-REGISTER-ADDR**       #xC000     2"Address of config register"*) 
(Defconstant 1*FLAG-REGISTER-ADDR* *        #xC002     2"Address of flag register"*) 
(Defconstant 1*SCP-ADDR* *                 #xFFFFF6   2"Address of Sys Config Ptr."*) 
(Defconstant 1*NUETHER-ID-ADDR* *           #xFFFEE0   2"Address of first byte of Ethernet ID in config rom"*)

1;;*--------------------------------------------------------------------------------
1;;*                                  1PARAMETERS
;;*--------------------------------------------------------------------------------

(Defparameter 1COMMAND-SIZE*
	      (Net::Round-Up-To-Quad
		(Max 18 (+ 8 (* *ETHER-ADDR-LENGTH*
				MAX-MC-ADDR))))           2"Bytes per command block"*)
(Defparameter 1SCP-SIZE*  10                                 2"Bytes in System Configuration Pointer"*) 
(Defparameter 1ISCP-SIZE  *(Net::Round-Up-To-Quad  8)         2"Bytes in Intermediate System Control Pointer"*) 
(Defparameter 1SCB-SIZE * (Net::Round-Up-To-Quad 20)         2"Bytes in System Control Block"*)  
(Defparameter 1RECEIVE-BLOCK-SIZE* (Net::Round-Up-To-Quad 22) 2"Bytes in a Receive Frame Descriptor."*) 
(Defparameter 1TBD-SIZE  *(Net::Round-Up-To-Quad   8)        2"Bytes in a Transmit Buffer Descriptor"*) 
(Defparameter 1RBD-SIZE*  (Net::Round-Up-To-Quad  10)        2"Bytes in a Receive Buffer Descriptor"*) 
(Defparameter 1DUMP-AREA-SIZE* (Net::Round-Up-To-Quad 170)   2"Bytes required for 586 Dump buffer"*) 
(Defparameter 1ISCP-ADDR * 0                                  2"Address of Intermediate System Control Pointer."*) 
(Defparameter 1SCB-ADDR*  Iscp-Size                          2"Offset to the System COntrol BLock."*) 

(Defparameter 1*MAX-NUMBER-BYTES** 1024 2"Max number bytes in a buffer"*)                               
(Defparameter 1*INTERRUPT-DRIVEN-P* *() 2"T if interrupt driven, NIL if polled."*) 
                              
(Defparameter 1*COMMAND-TIMEOUT* *     30	   1; Timeout of 500 milliseconds* 1(Intel min = 369)*
  2"Timeout for 82586 commmand execution wait"*)
(Defparameter 1*RECEIVE-TIMEOUT** (* 60. 75.)  1; Timeout of 75 seconds*.
  2"Timeout for packet reception wait"*)

(Defsignal 1ENC-TIMEOUT-ERROR* Net::Local-Network-Error ()
  2"The condition signaled when an Ethernet controller command times out."*)
(Defsignal 1ENC-BUFFER-ERROR* Net::Local-Network-Error ()
  2"The condition signaled when the Ethernet controller data structures
  * 2 need to be reinitialized."*)

1;;*--------------------------------------------------------------------------------
1;;*                                    1MACROS
;;*--------------------------------------------------------------------------------

(Defsubst 1NUBUS-READ* (Slot Addr)
  2"Return the 16bit value from NuBus at ADDR in SLOT-address."*
  (Si:%Nubus-Read-16b Slot Addr)) 

(Defsubst 1NUBUS-WRITE* (Slot Addr Val)
  2"Writes a 16 bit value to NuBus.  DO NOT DEPEND on the returned value!"*
  (Si:%Nubus-Write-16b Slot Addr Val)) 

(Defsetf Nubus-Read Nubus-Write)

2;;*--------------------------------------------------------------------------------
2;; The Macros generated by the next two Macros can be used with (SETF (...) ...).
;; The generated MACRO will default the offset field to the System Control Block (SCB).*

(Defmacro 1DEF-NUFIELD *(Name Field-Offset Comment)
2 "Create a MACRO called NAME to access the field at displacement FIELD-OFFSET.
  The MACRO take 2 args; the slot and base addr of the block."*
  `(Defmacro ,Name (Slot &Optional Block-Offset)
     ,Comment
     (If Block-Offset
       `(Nubus-Read ,Slot (+ ,',Field-Offset ,Block-Offset))
       `(Nubus-Read ,Slot (+ ,',Field-Offset Scb-Addr))))) 

(Defmacro 1DEF-NUBYTEF *(name word byte-desc comment)
2 "Create a MACRO called NAME to access a byte field, described by BYTE-DESC,
  in the field accessed by (WORD ,slot ,base).
  The MACRO take 2 args; the slot and the base addr of the block."*
  `(Defmacro ,Name (Slot &Optional Block-Base)
     ,Comment
     (If Block-Base
       `(Ldb ,',Byte-Desc (,',Word ,Slot ,Block-Base))
       `(Ldb ,',Byte-Desc (,',Word ,Slot Scb-Addr))))) 


2;;*--------------------------------------------------------------------------------
2;;  * 2READ & WRITE ETHERNET ADDRESS FIELDS.
;;
;;  Note: The "least significant byte" as described in the spec is referred
;;         to as the "high order octet" in the documentation supplied with
;;          ethernet addresses -- that's why it looks like we're trying to
;;          load the ethernet address backwards:*

(Defsubst 1ENC-ETHER-ADDRESS* (Slot Addr)
  2"Read the Ethernet address at ADDR in SLOT."* 
  (Dpb (Net::Swap-Bytes (NuBus-Read Slot Addr))
       (Byte 16 32)
       (Dpb (Net::Swap-Bytes (NuBus-Read Slot (+ 2 Addr)))
	    (Byte 16 16)
	    (Net::Swap-Bytes (NuBus-Read Slot (+ 4 Addr))))))

(Defsubst 1SET-ENC-ETHER-ADDRESS* (Slot Addr Address)
  2"Place ethernet ADDRESS, at ADDR in SLOT."*
  (Nubus-Write Slot       Addr (Net::Swap-Bytes (Ldb (Byte 16 32) Address)))
  (Nubus-Write Slot (+ Addr 2) (Net::Swap-Bytes (Ldb (byte 16 16) Address)))
  (Nubus-Write Slot (+ Addr 4) (Net::Swap-Bytes (Ldb (byte 16  0) Address))))

(Defsetf Enc-Ether-Address Set-Enc-Ether-Address) 


2;;*--------------------------------------------------------------------------------
2;; * 2 READ & WRITE 24 BIT ADDRESS FIELDS*.

(Defsubst 1ADDRESS-FIELD *(Slot Addr)
  2"Read a 24 bit Address at ADDR in SLOT, quad aligned."*
  (Dpb (Si:%Nubus-Read-8b Slot (+ 2 Addr))
       (Byte 8 16) (Si:%Nubus-Read-16b Slot Addr)))

(Defsubst 1WRITE-ADDRESS-FIELD *(Slot Addr Address)
  2"Place a 24 bit address, ADDRESS, at ADDR in SLOT, quad aligned."*
  (Si:%Nubus-Write-16b Slot Addr Address) 1;; Only takes the bottom 16 bits*
  (Si:%Nubus-Write-8b  Slot (+ 2 Addr) (Ldb (Byte 8 16) Address)))

(Defsetf Address-Field Write-Address-Field) 


2;;*--------------------------------------------------------------------------------
2;;   READ & WRITE CONTROL BITS:*

(Defconstant 1CONFIG-MASK* #x0106 2"Mask used to update config register"*)

(Defmacro 1DEF-REGISTER-BIT* (Name Byte-Desc Register-Addr Comment)     
2  "Creates a MACRO called NAME to access the bit described by BYTE-DESC
   at REGISTER-ADDR in Slot.  This bit is SETFable  to a 1 or 0."*
  
  `(Progn
     (Defmacro ,Name (Slot)
       ,Comment
       `(Ldb ,',Byte-Desc (NuBus-Read ,Slot ,',Register-Addr)))
     
     (Defmacro ,(Read-From-String (String-Append "SET-" Name)) (Slot Value)
       `(Let ((Register (Logand CONFIG-MASK (NuBus-Read ,Slot ,',Register-Addr))))
	  (Setf Register (Dpb ,Value ,',Byte-Desc Register))
	  (NuBus-Write ,Slot ,',Register-Addr Register)))
     
     (Defsetf ,Name ,(Read-From-String (String-Append "SET-" Name)))
     )					   1; progn*
  )					   1; Def-Register-Bit

2;;  Configuration Register Bits:**

(Def-Register-Bit 1HW-RESET*
		  (Byte 1 0) *CONFIG-REGISTER-ADDR* "1Set to 1 to reset board.*")
(Def-Register-Bit 1MASTER-ENABLE*
		  (Byte 1 1) *CONFIG-REGISTER-ADDR* "1NuBus Master Enabled iff = 1*")
(Def-Register-Bit 1FAULT-LED*
		  (Byte 1 2) *CONFIG-REGISTER-ADDR* "11 => LED on (ON at power up)*")
(Def-Register-Bit 1LOOPBACK*
		  (Byte 1 8) *CONFIG-REGISTER-ADDR* "11 => Loop back test*")


(Defmacro 1DEF-REGISTER-FLAG* (Name Byte-Desc Addr Comment)     
2  "Creates a MACRO called NAME to read the flag bit described by BYTE-DESC
   at register ADDR in Slot.  Each Flag is read-only."*
  
  `(Defmacro ,Name (Slot)
     ,Comment
     `(Ldb ,',Byte-Desc (NuBus-Read ,Slot ,',Addr))))


2;;  Flag Register Bits:*

(Def-Register-Flag  1BUS-ERROR*
		    (Byte 1 0) *FLAG-REGISTER-ADDR*   2"NuBus error occurred while posting event"*)
(Def-Register-Flag  1HOLD-LED*
		    (Byte 1 1) *FLAG-REGISTER-ADDR*   2"Memory Handshake active"*)
(Def-Register-Flag  1RTS-LED*
		    (Byte 1 2) *FLAG-REGISTER-ADDR*   2"Request to send active"*)
(Def-Register-Flag  1CRS-LED*
		    (Byte 1 3) *FLAG-REGISTER-ADDR*   2"Carrier sensed."*)
(Def-Register-Flag  1CDT-LED*
		    (Byte 1 4) *FLAG-REGISTER-ADDR*   2"Collision detected LED"*)
(Def-Register-Flag  1MEM-SIZE*  
		    (Byte 1 5) *FLAG-REGISTER-ADDR*   2"1 => 32KB, 0 => 8 KB buffer size"*)


(Defmacro 1DEF-STATUS-FLAG* (Name Byte-Desc Comment)     
2  "Creates a MACRO called NAME to access the status bit in the SCB status
   word as described BYTE-DESC. T or Nil is returned for 1 or 0, respectively.
   Each flag is read-only."*
  
  `(Defmacro ,Name (Slot)
     ,Comment
     `(Let ((Status (Si:%Nubus-Read-8b-Careful ,Slot (+ 1 Scb-Addr))))
	(When (Numberp Status)
	  (Not (Zerop (Ldb (Logand #x1FF ,',Byte-Desc) Status)))))))
  
(Def-Status-Flag 1CX-P*  (Byte 1 15) 2"The Command Unit finished with an interrupt"*)
(Def-Status-Flag 1FR-P*  (Byte 1 14) 2"The Receive Unit finished receiving a frame"*)
(Def-Status-Flag 1CNA-P* (Byte 1 13) 2"The Command Unit left the active state"*)
(Def-Status-Flag 1RNR-P* (Byte 1 12) 2"The Receive Unit left the ready state"*)



2;;*--------------------------------------------------------------------------------
2;;   SYSTEM CONTROL BLOCK (SCB) DEFINITIONS:
;;
;; * 2Only first 8 words are standard part of the SCB.  Rest are added for this application.*

(Def-Nufield SCB-BUSY-WORD   0 2"Low Byte is 1 if busy, 0 otherwise."*) 
(Def-Nufield SCB-OFFSET      2 2"Offset of SCB relative to SCB-BASE."*)

(Def-Nufield SCB-STATUS      0 2"SCB Status word (read only)."*) 
(Def-Nufield SCB-COMMAND     2 2"Read SCB Command word."*) 
(Def-Nufield SCB-CBL-OFFSET  4 2"Ptr to Command Block List."*) 
(Def-Nufield SCB-RFA-OFFSET  6 2"Ptr to Receive Frame Area."*) 
(Def-Nufield SCB-CRCERRS     8 2"CRC Error counter."*) 
(Def-Nufield SCB-ALNERRS    10 2"Misaligned frame counter."*) 
(Def-Nufield SCB-RSCERRS    12 2"Counter of frames dropped because no buffers available."*) 
(Def-Nufield SCB-OVRNERRS   14 2"Counter of frames lost to lack of bus availability."*) 
(Def-Nufield SCB-FREE-CBL   16 2"Ptr to list of free Command Blocks."*) 
(Def-Nufield SCB-FREE-TBD   18 2"Ptr to list of free Transmit Buffer descriptors."*) 

(Def-Nubytef SCB-INT-FLAGS            scb-status  (byte 4 13) E"Interrupted command executed?"*) 
(Def-Nubytef SCB-CX-FLAG              scb-status  (byte 1 15) 2"Interrupted command executed?"*) 
(Def-Nubytef SCB-FR-FLAG              scb-status  (byte 1 14) 2"Frame received interrupt"*) 
(Def-Nubytef SCB-CNR-FLAG             scb-status  (byte 1 13) 2"Command unit not ready interrupt"*) 
(Def-Nubytef SCB-RNR-FLAG             scb-status  (byte 1 12) 2"Receive unit not ready interrupt"*) 
(Def-Nubytef SCB-COMMAND-UNIT-STATUS  scb-status  (byte 3  8) 2"Status of command unit"*)  
(Def-Nubytef SCB-RECEIVE-UNIT-STATUS  scb-status  (byte 3  4) 2"status of command unit"*)
(Def-Nubytef SCB-ACK-INT              scb-command (byte 4 13) 2"Ack Interrupts command"*) 
(Def-Nubytef SCB-ACK-CX               scb-command (byte 1 15) 2"Ack CX interrupt"*) 
(Def-Nubytef SCB-ACK-FR               scb-command (byte 1 14) 2"Ack FR Interrupt"*) 
(Def-Nubytef SCB-ACK-CNR              scb-command (byte 1 13) 2"Ack CNR Interrupt"*) 
(Def-Nubytef SCB-ACK-RNR              scb-command (byte 1 12) 2"Ack RNR Interrupt"*) 
(Def-Nubytef SCB-CONTROL-COMMAND      scb-command (byte 3  8) 2"Control unit command"*) 
(Def-Nubytef SCB-RESET                scb-command (byte 1  7) 2"Reset the controller chip"*) 
(Def-Nubytef SCB-RECEIVE-COMMAND      scb-command (byte 3  4) 2"Receive unit command"*) 

(Defsubst 1SCB-BASE *(Slot Addr)
  2"Base address (relative to slot) for all blocks in buffer area, except buffers."*
  (Address-Field Slot (+ Addr 4)))

(Defmacro 1SCB-CURRENT-RECEIVE-FRAME *(slot)
  2"Pointer to the current receive-frame for ENC."*
  `(Command-Link ,Slot Last-Free-Rfd)) 

2;; Command and Receive Unit States*

(Defconstant 1IDLE-STATE *         0 "Idle State") 
(Defconstant 1SUSPENDED-STATE *    1 "Suspended State") 
(Defconstant 1COMMAND-READY *    2  "Command Ready State") 
(Defconstant 1NO-RESOURCES *      2  "No Resources State") 
(Defconstant 1RECEIVE-READY *      4 "Receive Ready State")

2;; Transmit & Receive Unit Commands*

(Defconstant 1SCB-NOP-CMD *       0 "NOP Command") 
(Defconstant 1SCB-START-CMD *     1 "Start Command") 
(Defconstant 1SCB-RESUME-CMD *    2 "Resume Command") 
(Defconstant 1SCB-SUSPEND-CMD *   3  "Suspend Command") 
(Defconstant 1SCB-ABORT-CMD *     4 "Abort Command") 

2;;*--------------------------------------------------------------------------------
2;;  SYSTEM CONTROL BLOCK INITIALIZATIONS:*

(Defsubst 1RESET-SCB-STATS *(Slot)      
  2"Set the Statistics fields to Zero."*
  (Setf (Scb-Crcerrs  Slot)  0)     1; Zero CRC errors.*
  (Setf (Scb-Alnerrs  Slot)  0)     1; Zero alignment errors.*
  (Setf (Scb-Rscerrs  Slot)  0)     1; Zero frames lost to "no resources"*
  (Setf (Scb-Ovrnerrs Slot)  0))    1; Zero frames lost to bus not available*

(Defsubst 1INIT-SCB *(Slot Addr Cmd-Ptr Rec-Ptr)
  2"Make & initialize the Place SCB at ADDR,* 2initialize CBL to CMD-PTR, and RFA to REC-PTR."*
  (Setf (Scb-Status     Slot Scb-Addr) *NULL-STATUS*)
  (Setf (Scb-Command    Slot Scb-Addr) *NULL-COMMAND*)	   1; NOP the command word.*
  (Setf (Scb-Cbl-Offset Slot Scb-Addr) *NULL-PTR*)
  (Setf (Scb-Rfa-Offset Slot Scb-Addr) Rec-Ptr)
  (Setf (Scb-Free-Cbl   Slot Scb-Addr) Cmd-Ptr)	           1; List of free Command Blocks*
  (Setf (Scb-Free-Tbd   Slot Scb-Addr) *NULL-PTR*)
  (Reset-Scb-stats Slot)		                   1; Clear the statistics fields.*
  Addr) 



2;;*--------------------------------------------------------------------------------
2;;   COMMAND BLOCK (CB) DEFINITIONS:
;;*

(Def-Nufield COMMAND-STATUS       0        2"The Status word of a command block"*) 
(Def-Nufield COMMAND-WORD         2        2"The Command word of a command block."*) 
(Def-Nufield COMMAND-LINK         4        2"Pointer to next command block."*) 
(Def-Nufield COMMAND-BUFFER-PTR   6        2"Pointer to 1st buffer of frame."*) 
(Def-Nufield COMMAND-FRAME-TYPE  14        2"Type of frame to transmit."*) 

(Defsubst 1COMMAND-DESTINATION-ADDR *(Slot Addr)
  2"Ethernet Destination address of this frame."*
  (Enc-Ether-Address Slot (+ Addr 8))) 

(Defsubst 1BLOCK-PARAMETER *(Slot Offset Parm-Number)
  2"PARM-NUMBER (starts at 0) parameter to the command block."*
  (Nubus-Read Slot (+ 6 Parm-Number Parm-Number Offset))) 

2;; Additional fields of COMMAND-STATUS*

(Def-Nubytef COMMAND-COMPLETE-FLAG    command-status (byte 1 15) 2"Command is complete"*) 
(Def-Nubytef COMMAND-BUSY-FLAG        command-status (byte 1 14) 2"Controller executing this command."*) 
(Def-Nubytef COMMAND-EXECUTION-STATUS command-status (byte 2 14) 2"State of frame block"*) 
(Def-Nubytef COMMAND-STATUS-FIELD     command-status (byte 14 0) 2"Command specific status."*) 
(Def-Nubytef COMMAND-ERROR-FLAG       command-status (byte 1 13) 2"No error occurred during execution."*)
   
2;; Transmit Errors*

(Def-Nubytef COMMAND-ABORT-ERROR      command-status (byte 1 12) 2"Command aborted."*) 
(Def-Nubytef COMMAND-DIAGNOSE-FAIL    command-status (byte 1 11) 2"586 failed DIAGNOSE test"*) 
(Def-Nubytef COMMAND-NO-CARRIER       command-status (byte 1 10) 2"Carrier sense lost, xmit failed."*) 
(Def-Nubytef COMMAND-NO-CLEAR-SEND    command-status (byte 1  9) 2"Xmit failed, lost Clear to Send."*) 
(Def-Nubytef COMMAND-XMIT-DEFERRED    command-status (byte 1  7) 2"Xmit deferred, link activity."*) 
(Def-Nubytef COMMAND-RETRY-ERROR      command-status (byte 1  5) 2"Number of retries exhausted."*) 
(Def-Nubytef COMMAND-RETRY-COUNT      command-status (byte 4  0) 2"Number of collisions (retries)."*) 
(Def-Nubytef COMMAND-END-OF-LIST      command-word   (byte 1 15) 2"This is last block on list."*)
(Def-Nubytef COMMAND-SUSPEND-FLAG     command-word   (byte 1 14) 2"Suspend after this block."*) 
(Def-Nubytef COMMAND-INTERRUPT-FLAG   command-word   (byte 1 13) 2"Interrupt after this block."*) 
(Def-Nubytef COMMAND-BLOCK-CMD        command-word   (byte 3  0) 2"Command byte."*)

2;; VALUES for COMMAND-EXECUTION-STATUS*

(Defconstant COMMAND-READY-TO-EXECUTE   0) 
(Defconstant COMMAND-EXECUTING          1) 
(Defconstant COMMAND-COMPLETE           2) 

2;; COMMAND Numbers for Command-Block-Cmd field.*

(Defconstant NOP-COMMAND       0 2"NOP command"*) 
(Defconstant ADDRESS-SETUP     1 2"Load Chip with my address"*) 
(Defconstant CONFIGURE         2 2"Send Configuration parameters to chip"*) 
(Defconstant MULTICAST-SETUP   3 2"Load Chip with Multicast addresses to accept."*) 
(Defconstant TRANSMIT          4 2"Transmit a frame"*) 
(Defconstant TDR-COMMAND       5 2"Time Domain Reflectometer test"*) 
(Defconstant DUMP-STATUS       6 2"Dump Status registers of 586 chip"*) 
(Defconstant DIAGNOSE-586      7 2"Perform diagnostics on the 586 chip"*)




2;; Receive Frame Definitions:*

(Defsubst 1RECEIVE-SOURCE-ADDR *(Slot Addr)
  2"Ethernet source address of this frame."*
  (Enc-Ether-Address Slot (+ Addr 14))) 

(Def-Nufield RECEIVE-FRAME-TYPE        20 2"Type of frame received."*)

(Def-Nubytef COMMAND-CRC-ERROR    command-status (byte 1 11) 2"CRC error in aligned frame."*) 
(Def-Nubytef COMMAND-ALIGN-ERROR  command-status (byte 1 10) 2"CRC error in misaligned frame."*) 
(Def-Nubytef COMMAND-BUFFER-ERROR command-status (byte 1  9) 2"Ran out of buffer space."*) 
(Def-Nubytef COMMAND-DMA-ERROR    command-status (byte 1  8) 2"DMA Over(under)run recv (xmit)"*) 
(Def-Nubytef COMMAND-TOO-SHORT    command-status (byte 1  7) 2"Frame too short."*) 
(Def-Nubytef COMMAND-NO-EOF-ERROR command-status (byte 1  6) 2"No EOF Flag, for bitstuffing only."*) 


2;; Configure frame definitions:*

(Def-Nufield CFG-PARAMS-A      6 2"Configure Command Parameter bytes 6-7"*) 
(Def-Nufield CFG-PARAMS-B      8 2"Configure Command Parameter bytes 8-9"*) 
(Def-Nufield CFG-PARAMS-C     10 2"Configure Command Parameter bytes 10-11"*) 
(Def-Nufield CFG-PARAMS-D     12 2"Configure Command Parameter bytes 12-13"*) 
(Def-Nufield CFG-PARAMS-E     14 2"Configure Command Parameter bytes 14-15"*) 
(Def-Nufield CFG-PARAMS-F     16 2"Configure Command Parameter byte 16"*) 

(Def-Nubytef CFG-BYTE-CNT     cfg-params-a (byte  4  0)  2"No. of bytes to be configured."*)
(Def-Nubytef CFG-FIFO-LIM     cfg-params-a (byte  4  8)  2"Value of FIFO threshold."*)
(Def-Nubytef CFG-SYNC         cfg-params-b (byte  1  6)  2"Internal/External Syncronization."*)
(Def-Nubytef CFG-SAV-BF       cfg-params-b (byte  1  7)  2"Save Bad Frames in Memory."*)
(Def-Nubytef CFG-ADDR-LEN     cfg-params-b (byte  3  8)  2"No. of address bytes."*)
(Def-Nubytef CFG-AT-LOC       cfg-params-b (byte  1 11)  2"Address/Type field location."*)
(Def-Nubytef CFG-PREAM-LEN    cfg-params-b (byte  2 12)  2"Preamble-Length."*)
(Def-Nubytef CFG-INT-LPBCK    cfg-params-b (byte  1 14)  2"Internal-Loopback."*)
(Def-Nubytef CFG-EXT-LPBCK    cfg-params-b (byte  1 15)  2"External-Loopback."*)
(Def-Nubytef CFG-LIN-PRIO     cfg-params-c (byte  3  0)  2"Linear Priority."*)
(Def-Nubytef CFG-ACR          cfg-params-c (byte  3  4)  2"Accelerated Contention Resolution"*)
(Def-Nubytef CFG-BOF-MET      cfg-params-c (byte  1  7)  2"Exponential Backoff Method."*)
(Def-Nubytef CFG-IF-SPACING   cfg-params-c (byte  8  8)  2"Interframe spacing."*)
(Def-Nubytef CFG-SLOT-TIME    cfg-params-d (byte 11  0)  2"Slot time number"*)
(Def-Nubytef CFG-RETRY-NUM    cfg-params-d (byte  4 12)  2"Max no. retries on collisions."*)
(Def-Nubytef CFG-PRM          cfg-params-e (byte  1  0)  2"Promiscuous Mode."*) 
(Def-Nubytef CFG-BC-DIS       cfg-params-e (byte  1  1)  2"Broadcast Disable."*)
(Def-Nubytef CFG-MANCH-NRZ    cfg-params-e (byte  1  2)  2"Manchester or NRZ encoding"*)
(Def-Nubytef CFG-TONO-CRS     cfg-params-e (byte  1  3)  2"Transmit on no carrier sense."*)
(Def-Nubytef CFG-NCRC-INS     cfg-params-e (byte  1  4)  2"No CRC insertion."*)
(Def-Nubytef CFG-CRC-16       cfg-params-e (byte  1  5)  2"CRC Type."*)
(Def-Nubytef CFG-BT-STF       cfg-params-e (byte  1  6)  2"Bitstuffing."*)
(Def-Nubytef CFG-PAD          cfg-params-e (byte  1  7)  2"Padding."*)
(Def-Nubytef CFG-CRSF         cfg-params-e (byte  3  8)  2"Carrier Sense Filter."*)
(Def-Nubytef CFG-CRS-SRC      cfg-params-e (byte  1 11)  2"Carrier Sense Source."*)
(Def-Nubytef CFG-CDTF         cfg-params-e (byte  3 12)  2"Collision-detect filter."*)
(Def-Nubytef CFG-CDT-SRC      cfg-params-e (byte  1 15)  2"Collision-detect Source."*)
(Def-Nubytef CFG-MIN-FRM-LEN  cfg-params-f (byte  8  0)  2"Minimum frame byte number."*)


2;; Multicast Address Setup Definitions*

(Defconstant  MC-LIST         8 2"First word of Multicast address list in command block"*)
(Def-Nufield  MC-CNT-WORD     6 2"Number of MC addresses multiplied by # bytes per address"*)
(Def-Nubytef  MC-CNT          Mc-Cnt-Word (Byte 14 0)  2"Number of bytes in MC-List"*)



2;;*--------------------------------------------------------------------------------
2;;  TRANSMIT/RECEIVE BUFFER DEFINITIONS:
;;*

(Def-Nufield BUFFER-COUNT     0 2"Number of data bytes in buffer; & flags"*) 
(Def-Nufield BUFFER-LINK      2 2"Link to next buffer in frame"*) 

(Def-Nufield BUFFER-SIZE-WORD 8 2"Size of buffer in bytes."*)  1;* 1Receive buffers only.*
(Defsubst BUFFER-ADDRESS (Slot Addr)
  2"Pointer to Buffer."*
  (Address-Field Slot (+ Addr 4))) 

(Def-Nubytef BUFFER-BYTE-COUNT   buffer-count     (byte 14 0) 2"Number of data bytes in buffer."*) 
(Def-Nubytef BUFFER-FULL         buffer-count     (byte 1 14) 2"Buffer has been filled."*) 
(Def-Nubytef BUFFER-END-OF-FRAME buffer-count     (byte 1 15) 2"This is the last buffer in the frame"*) 
(Def-Nubytef BUFFER-END-OF-FBL   buffer-size-word (byte 1 15) 2"End of list marker"*) 
(Def-Nubytef BUFFER-SIZE         buffer-size-word (byte 14 0) 2"Size of Buffer in bytes"*)


2;; Dump accessors:*

(Def-Nufield DUMP-PARAMS-A      0 2"Dump Command Parameter bytes 0-1"*) 
(Def-Nufield DUMP-PARAMS-B      2 2"Dump Command Parameter bytes 2-3"*) 
(Def-Nufield DUMP-PARAMS-C      4 2"Dump Command Parameter bytes 4-5"*) 
(Def-Nufield DUMP-PARAMS-D      6 2"Dump Command Parameter bytes 6-7"*) 
(Def-Nufield DUMP-PARAMS-E      8 2"Dump Command Parameter bytes 8-9"*) 
(Def-Nufield DUMP-PARAMS-F     10 2"Dump Command Parameter byte 10"*) 

(Def-Nubytef DUMP-FIFO-LIM     DUMP-PARAMS-A  (byte  4  8)  2"Value of FIFO threshold."*)
(Def-Nubytef DUMP-SYNC         DUMP-PARAMS-B  (byte  1  6)  2"Internal/External Syncronization."*)
(Def-Nubytef DUMP-SAV-BF       DUMP-PARAMS-B  (byte  1  7)  2"Save Bad Frames in Memory."*)
(Def-Nubytef DUMP-ADDR-LEN     DUMP-PARAMS-B  (byte  3  8)  2"No. of address bytes."*)
(Def-Nubytef DUMP-AT-LOC       DUMP-PARAMS-B  (byte  1 11)  2"Address/Type field location."*)
(Def-Nubytef DUMP-PREAM-LEN    DUMP-PARAMS-B  (byte  2 12)  2"Preamble-Length."*)
(Def-Nubytef DUMP-INT-LPBCK    DUMP-PARAMS-B  (byte  1 14)  2"Internal-Loopback."*)
(Def-Nubytef DUMP-EXT-LPBCK    DUMP-PARAMS-B  (byte  1 15)  2"External-Loopback."*)
(Def-Nubytef DUMP-LIN-PRIO     DUMP-PARAMS-C  (byte  3  0)  2"Linear Priority."*)
(Def-Nubytef DUMP-ACR          DUMP-PARAMS-C  (byte  3  4)  2"Accelerated Contention Resolution"*)
(Def-Nubytef DUMP-BOF-MET      DUMP-PARAMS-C  (byte  1  7)  2"Exponential Backoff Method."*)
(Def-Nubytef DUMP-IF-SPACING   DUMP-PARAMS-C  (byte  8  8)  2"Interframe spacing."*)
(Def-Nubytef DUMP-SLOT-TIME    DUMP-PARAMS-D  (byte 11  0)  2"Slot time number"*)
(Def-Nubytef DUMP-RETRY-NUM    DUMP-PARAMS-D  (byte  4 12)  2"Max no. retries on collisions."*)
(Def-Nubytef DUMP-PRM          DUMP-PARAMS-E  (byte  1  0)  2"Promiscuous Mode."*) 
(Def-Nubytef DUMP-BC-DIS       DUMP-PARAMS-E  (byte  1  1)  2"Broadcast Disable."*)
(Def-Nubytef DUMP-MANCH-NRZ    DUMP-PARAMS-E  (byte  1  2)  2"Manchester or NRZ encoding"*)
(Def-Nubytef DUMP-TONO-CRS     DUMP-PARAMS-E  (byte  1  3)  2"Transmit on no carrier sense."*)
(Def-Nubytef DUMP-NCRC-INS     DUMP-PARAMS-E  (byte  1  4)  2"No CRC insertion."*)
(Def-Nubytef DUMP-CRC-16       DUMP-PARAMS-E  (byte  1  5)  2"CRC Type."*)
(Def-Nubytef DUMP-BT-STF       DUMP-PARAMS-E  (byte  1  6)  2"Bitstuffing."*)
(Def-Nubytef DUMP-PAD          DUMP-PARAMS-E  (byte  1  7)  2"Padding."*)
(Def-Nubytef DUMP-CRSF         DUMP-PARAMS-E  (byte  3  8)  2"Carrier Sense Filter."*)
(Def-Nubytef DUMP-CRS-SRC      DUMP-PARAMS-E  (byte  1 11)  2"Carrier Sense Source."*)
(Def-Nubytef DUMP-CDTF         DUMP-PARAMS-E  (byte  3 12)  2"Collision-detect filter."*)
(Def-Nubytef DUMP-CDT-SRC      DUMP-PARAMS-E  (byte  1 15)  2"Collision-detect Source."*)
(Def-Nubytef DUMP-MIN-FRM-LEN  DUMP-PARAMS-F  (byte  8  0)  2"Minimum frame byte number."*)



2;;*--------------------------------------------------------------------------------
2;; * 2SYSTEM CONTROL BLOCK COMMAND HANDLING:*

(Defsubst 1CHANNEL-ATTENTION* (Slot)
  2"Assert CHANNEL ATTENTION to the 82586."*
  (Nubus-Write Slot *CHANNEL-ATTENTION-ADDR* 1)) 

(Defsubst 1COMMAND-COMPLETE-P* (Slot)	   ; 12 JUN 87  MMG
2  "Provides a deadman timer for 82586 command completion; returns
   nil if the 82586 does not respond within a reasonable time."*
  (Process-Wait-With-Timeout
     "ENC Cmd Wait" *Command-Timeout*
     #'(Lambda (Enc-Slot)
	 (Or
	   (Zerop (Scb-Command Enc-Slot))  1; SCB command word zero*
	   (Cx-P Enc-Slot))) Slot))	   1; or command interrupt*

(Defmacro 1ISSUE-CONTROL-COMMAND* (Slot Wait-P &Body Body)        
2  "Eval BODY to set the command, then issue channel attention.
   When WAIT-P is T, wait for interrupt or zero command field;
   if the command doesn't complete, signal a timeout error."*
					   ; 17 JUN 87  MMG
  `(Without-Interrupts
     (Progn
       ,@Body				   1; Setup command*
       (Channel-Attention ,Slot))	   1; Issue channel-attention*
     (When ,Wait-P
       (Unless (Command-Complete-P ,Slot)  1; If command didn't complete*,
	 (Ferror 'Enc-Timeout-Error	   1; signal timeout error.*
		 " Ethernet control command timed out"))
       )				   1; when wait-p*
     )					   1; w/o interrupts*
  )					   1; issue-control-command

2;;**--------------------------------------------------------------------------------
2;; * 2SYSTEM CONTROL BLOCK INTERRUPT HANDLING:*

(Defsubst 1COMMAND-INTERRUPT-P* (Slot)
  2"Returns T when a command interrupt is posted to signal command completion."*
  (Process-Wait-With-Timeout           
    "ENC Command Wait" *Command-Timeout*
    #'(Lambda (Enc-Slot)		   1; Wait for interrupt as*
	(And (Or (Cx-P Enc-Slot)	   1; Command-Complete flag*
		 (Cna-P Enc-Slot))	   1; or CU not active*
	     (Zerop
	       (Scb-Command Enc-Slot))))   1; with SCB command word zero *
    Slot)
  )					   1; command-interrupt-p*

(Defsubst 1ACK-COMMAND-INTERRUPT* (Slot)
  2"Acks the Command-Complete or Command-Unit-Not-Active interrupts."*
  (Issue-Control-Command Slot t
    (Setf (Scb-Ack-Cx   Slot)		   1; Ack if Command-Complete Interrupt*
	  (Scb-Cx-Flag  Slot))
    (Setf (Scb-Ack-Cnr  Slot)		   1; or Command-Unit not active interrupt*
	  (Scb-Cnr-Flag Slot)))
  )					   1; ack command interrupt *

(Defsubst 1RECEIVE-INTERRUPT-P* (Slot)
  2"Returns T if a receive interrupt is posted to signal receive completion."*
  (Process-Wait-With-Timeout           
    "Network Wait" *Receive-Timeout*
    #'(Lambda (Enc-Slot)		   1; Wait for RCV interrupt as*
	(Or (Fr-P Enc-Slot)		   1; Frame-Received flag*
	    (Rnr-P Enc-Slot)))		   1; or No-Resources*
    Slot)
  )					   1; receive-interrupt-p*

(Defsubst 1ACK-RECEIVE-INTERRUPT* (Slot)
  2"Acks the Frame-Received or No-Resources receive interrupts."*
  (Issue-Control-Command Slot t                  
    (Setf (Scb-Ack-Fr   Slot)		   1; Ack if Frame-Received Interrupt*
	  (Scb-Fr-Flag  Slot))
    (Setf (Scb-Ack-Rnr  Slot)		   1; Ack if No-Resources Interrupt*
	  (Scb-Rnr-Flag Slot)))
  )					   1; ack receive interrupt*


2;;*--------------------------------------------------------------------------------
2;; * 2 INTERMEDIATE SYSTEM CONTROL POINTER (ISCP):*

(Defsubst 1ISCP-ADDRESS* (Slot)
  2"The address of the ISCP in the System Config. Pointer."*
  (Address-Field Slot (+ (Send Self :Scp-Addr) 6))) 

(Defun 1INIT-ISCP* (Slot Scb-Pointer)  
  2"Initialize the ISCP to location Iscp-Addr. SCB-POINTER = offset to SCB, the SCB-Base is assumed zero."*
  (Setf (Scb-Busy-Word Slot Iscp-Addr) 1)                  1; Set board busy*
  (Setf (Scb-Offset    Slot Iscp-Addr) Scb-Pointer)	   1;* 1Set addr of SCB*
  (Setf (Scb-Base      Slot Iscp-Addr) 0))                 1; Base is 0.*

(Defun 1ISCP-BUSYP* (Slot)
  2"Returns T if the ISCP BUSY flag is set for controller ENC."*
  (Not (Zerop (Ldb (Byte 8 0) (Scb-Busy-Word Slot Iscp-Addr))))) 




(Defmacro 1ISSUE-COMMAND-BLOCK* (Slot Cmd-Block Ack-P &Body Body)
2  "Issue the command block symbol CMD-BLOCK to the controller in SLOT.
   BODY contains the forms to fill in the free command-block. 
   Interrupt is turned on and End-Of-List flag is set (since only one 
   CB is used).  When Ack-P is T, wait until the command interrupt
   is returned, ack it, and free the command block.  The request-lock
   must be owned before issuing a command block."*
					   ; 29 JUN 87  MMG
  `(With-Lock (Request-Lock)
     (Let ((,Cmd-Block (Send Self :Allocate-Command-Block)))
       ,@Body
       (Setf (Command-End-Of-List	   1; Set End-Of-List flag*
	       ,Slot ,Cmd-Block) (Net::Numerical-Value T))
       (Setf (Command-Link		   1; Set link field to null*
	       ,Slot ,Cmd-Block) *NULL-PTR*)
       (Setf (Command-Interrupt-Flag	   1; Turn interrupt on*
	       ,Slot ,Cmd-Block) (Net::Numerical-Value :ON))            
       (Issue-Control-Command ,Slot Nil	   1; Issue the Command*:
	 (Setf (Scb-Cbl-Offset      ,Slot)
	       ,Cmd-Block)		   1; Plug in command block and*
	 (Setf (Scb-Control-Command ,Slot)
	       SCB-START-CMD))		   1; Issue start to Command Unit*       
       (When ,Ack-P			   1; If ack command*,
	 (When (Command-Interrupt-P ,Slot) 1; when interrupt is returned,*
	   (Without-Interrupts		   1; Free the command block*
	     (Send Self :Free-Command-Block ,Cmd-Block)		   
	     (Ack-Command-Interrupt ,Slot) 1; And ack the command interrupt*
	     )				   1; w/o interrupts*
	   )				   1; when command-interrupt*
	 )				   1; when ack-p*       
       )				   1; let*
     )					   1; with lock*
  )					   1; issue-command-block*

  
(Defsubst 1XFER-WORDS-TO-NUBUS* (Slot Addr Array Start-Idx Stop-Idx)
  2"Copies ARRAY (between START-IDX & STOP-IDX-1) to the NuBus at SLOT, beginning at ADDR.
   ARRAY must be art16b,* 2 ADDR must be on quad boundary, START & STOP must be even."*
  (Si:%Blt-To-Physical
    (+ (Si:Array-Data-Offset Array) (Net::Convert-To-Words Start-Idx) (%Pointer Array))
    (Dpb Slot (Byte 8 24) Addr) (Net::Convert-To-Words (- Stop-Idx Start-Idx)) 1)) 

(Defsubst 1XFER-WORDS-FROM-NUBUS *(Slot Addr Array Start-Idx Stop-Idx)
  2"Copies into ARRAY (between START-IDX & STOP-IDX-1) from the NuBus at SLOT, beginning at ADDR.
   ARRAY must be art16b, ADDR must be on quad boundary. Will *always* copy an EVEN number
 * 2of bytes,* 2beginning at START !"*
  (Si:%Blt-From-Physical
    (Dpb Slot (Byte 8 24) Addr)
    (+ (Si:Array-Data-Offset Array) (Net::Convert-To-Words Start-Idx) (%Pointer Array))
    (Net::Convert-To-Words (- Stop-Idx Start-Idx)) 1)) 



1;;*--------------------------------------------------------------------------------
1;;                                 *   1 CONDITION HANDLING
;;*--------------------------------------------------------------------------------

(Defmacro 1WITH-CONTROLLER-DISABLED* (&Body Body)               
  2"Executes BODY with the controller disabled"*
  `(Progn
     (Setf Net::Enabled Nil)		   1; Go into disabled state*
     (Process-Allow-Schedule)		   1; Let any other ENC processes sync up*
     (Setf (Loopback Net::Slot)
	   (Net::Numerical-Value :ON))	   1; Assert loopback line to isolate 82586*
     ,@Body
     (Setf (Loopback Net::Slot)
	   (Net::Numerical-Value :OFF))    1; Turn off loopback line.*
     (Setf Net::Enabled T)))		   1; Go into enabled state*


(Defmacro 1HANDLING-CONDITIONS* (&Body body)
2  "Wraps a condition-handler around BODY to deal with errors by reinitializing
   the controller and re-executing BODY."*
  `(Block Retry-Loop
     (Loop
       (Condition-Case ()
	   (Progn ,@Body)
	 (Eh:User-NuBus-Error              1; NuBus error*
	  (With-Controller-Disabled
	    (Incf NuBus-Errors)
	    (Send Self :Initialize)))
	 (Enc-Timeout-Error                1; Controller timeout error*
	  (With-Controller-Disabled
	    (Send Self :Initialize)))
	 (Enc-Buffer-Error                 1; Controller memory buffer error*
	  (With-Controller-Disabled
	    (Send Self :Initialize)))
	 (:NO-ERROR
	  (Return-From Retry-Loop))
	 )				   1; condition-case*
       )				   1; loop*
     )					   1; block*
  )					   1; handling-conditions*


(Defmacro 1PRINT-FLAG-DOC* (state flag slot base-addr)
  2"Sends output to STREAM, and uses ENC (controller."*
  `(if (= ,state (,flag ,slot ,base-addr))
     (progn
       (terpri stream)
       (princ (documentation (function ,flag)) stream)))) 



1;;*--------------------------------------------------------------------------------
1;;                                NuBUS ETHERNET CONTROLLER FLAVOR
;;*--------------------------------------------------------------------------------

(Defflavor 1NUBUS-ENC*
           ((Request-lock nil)             1; Mutual exclusion to use the COMMAND UNIT.*
            (Memory-Size (* 32 1024.))     1; Size of memory buffer*
            (Transmit-Buffers 0)           1; Number of transmit buffers*
            (Transmit-Buffer-Size 576.)    1; Size of the transmit buffers - Fix to allow max Ethernet size.*
            (Transmit-Buffers-Per-Frame 2) 1; Number of buffers per command block.*
            (Receive-Buffer-Size 512.)     1; Size of the receive buffers.*
            (Receive-Buffers-Per-Frame 1)  1; Number of buffers / Recv. Frame descriptor.*
	    (Start-of-Buffers 0)           1; Start of receive-frame area.*
            (Nubus-Errors 0)               1; Total number of NuBus errors encountered.*
	    (Receive-Frame-Errors 0)       1; Number of frames actually read with errors.*
            (Last-Rbd -1)                  1; Offset to the last Free RBD.*
            (Last-Free-Rfd -1)             1; Offset to the last free RFD.*
	    (Rbds 0)			   1; Number of receive buffers.*
	    (Rfds 0))			   1; Number of receive frame descriptors*
           (Ethernet-Controller-Mixin)
  :Gettable-Instance-Variables
  :Inittable-Instance-Variables
  :Settable-Instance-Variables
  (:Default-Init-Plist :Board-Type :NuBus)
  (:Documentation 2"NuBus Ethernet Controller"*))


(si:DEFINE-WHEN :ENET

(Defmethod 1(NUBUS-ENC :TRANSMIT-FRAME-SIZE)* ()
  2"Number bytes  Command Block"*
  (+ Command-Size (* Transmit-Buffers-Per-Frame (+ Transmit-Buffer-Size Tbd-Size)))) 

(Defmethod 1(NUBUS-ENC :RECEIVE-FRAME-SIZE)* ()
  2"Number of bytes  Recv. Frame Desc."*
  (+ Receive-Block-Size (* Receive-Buffers-Per-Frame (+ Receive-Buffer-Size Rbd-Size)))) 

(Defmethod 1(NUBUS-ENC :SCP-ADDR)* ()
  2"Location of System Config ptr"*
  (- Memory-Size Scp-Size))



1;;*--------------------------------------------------------------------------------
1;;                               *      182586 DATA STRUCTURES
;;*--------------------------------------------------------------------------------

2;;*--------------------------------------------------------------------------------
2;; * 2SYSTEM CONFIGURATION POINTER (SCP)* 2DEFINITIONS:
;;
;; * 2Note: the SCP always starts at #xFFFFF6 in the chip addr space.*

(Defmethod 1(NUBUS-ENC :SET-SCP-SYSBUS-WIDTH)* (Bit-Width)
  2"ENC is the <Slot> address; BIT-WIDTH is bit width of data bus (8 or 16)."*
  (Nubus-Write Net::Slot
	       (Send Self :Scp-Addr) (If (Eql Bit-Width 8) 1 0)))

(Defmethod 1(NUBUS-ENC :READ-SCP-SYSBUS-WIDTH) *()
  (Let ((Encoded-Width
	  (Ldb (Byte 8 0) (Nubus-Read Net::Slot (Send Self :Scp-Addr)))))
    (Case Encoded-Width
      (0 16)
      (1 8)
      (T (Send Self :Set-Scp-Sysbus-Width
	   (Cerror t 'Error-Type
		   3"Invalid width code for NuBus Ether controller, code = ~2o~* 3Enter correct value to proceed"*
		   Encoded-Width)))))) 


(Defmethod 1(NUBUS-ENC :CREATE-SCB)* (Addr Nxmit-Frames) 
  2"Initialize the SCB, & Buffers in NuBus Memory for a TI NuBus Ethernet Controller.
   ADDR = address of System Control Block in the slot.
   NXMIT-FRAMES = number of transmit frames to make.
   Rest of memory will be used for receive buffers."*
  1;; The buffer for the 586 dump immediately follows the SCB*
  (Let* ((Next-Addr (+ Addr Scb-Size Dump-Area-Size))	   1;* 1Start address of buffers.*
         (Buffer-size			                   1;* 1Number bytes avail for buffers.*
           (- Memory-Size
              Next-Addr
              Scp-Size))
         (Xmit-Area-Bytes		                   1;* 1Number bytes in transmit area*
           (* Nxmit-Frames (Send Self :Transmit-Frame-Size)))
         (Recv-Area-Bytes		                   1;* 1Number bytes in receive area*
           (- Buffer-Size Xmit-Area-Bytes))
         (Nrecv-Frames (Floor Recv-Area-Bytes	           1;* 1Num of receive frames to use.*
                              (Send Self :Receive-Frame-Size))))
    (Setf Rfds Nrecv-Frames)
    (Init-Scb Net::Slot Addr *NULL-PTR* *NULL-PTR*)
    (Setf Next-Addr                                        1; Pushes the CB's on Free-List.*
          (Send Self :Create-Cbl Next-Addr Nxmit-Frames))	   
    (Setf Next-Addr
          (Send Self :Create-Rfa Next-Addr Nrecv-Frames))
    (Setf Start-Of-Buffers Next-Addr)
    (Setf Next-Addr
          (Send Self :Create-Xmit-Buffers  
		Next-Addr
		(Setf Transmit-Buffers
		      (* Nxmit-Frames Transmit-Buffers-Per-Frame))
		Transmit-Buffer-Size))	                   1;* 1Holds in ENC*
    (Setf Next-Addr
          (Send Self :Create-Receive-Buffers
		Next-Addr
		(Setf Rbds
		      (* Nrecv-Frames Receive-Buffers-Per-Frame))
		Receive-Buffer-Size))
    Addr))


2;;*--------------------------------------------------------------------------------
2;;  COMMAND BLOCK INITIALIZATION:
;;*

(Defmethod 1(NUBUS-ENC :CREATE-CBL)* (Start-Addr Count)  
  2"Create COUNT Command-Blocks, starting at START-ADDR.
   Returns the next available address in the buffer emeory area."*
  (Do ((Size Command-Size)
       (I 0 (+ 1 I))			   1; loop counter*
       (Addr Start-Addr (+ Addr Size)))	   1; address of command block*
      ((Eql I Count) Addr)		   1; return next available address.*
    (Setf (Command-Status     Net::Slot  Addr) *NULL-STATUS*)
    (Setf (Command-Word       Net::Slot  Addr) *NULL-COMMAND*)
    (Setf (Command-Link       Net::Slot  Addr) (Scb-Free-Cbl Net::Slot))
    (Setf (Command-Buffer-Ptr Net::Slot  Addr) *NULL-PTR*) 
    (Setf (Scb-Free-Cbl       Net::Slot) Addr)))



2;;*--------------------------------------------------------------------------------
2;;  COMMAND BLOCK MANAGEMENT:
;;*

(Defmethod 1(NUBUS-ENC :FREE-COMMAND-BLOCK)* (Cb)
  2"Initialize Command Block CB & push it on the Free-CBL list."*
					   ; 29 JUN 87  MMG
  (Without-Interrupts
    (Setf (Command-Status     Net::Slot  Cb)
	  *NULL-STATUS*)		   1; Clear status field of CB*
    (Setf (Command-Word       Net::Slot  Cb)
	  *NULL-COMMAND*)		   1; Clear command field of CB*
    (Setf (Command-Buffer-Ptr Net::Slot  Cb)
	  *NULL-PTR*)			   1; Clear the TBD pointer field of CB*
    (Setf (Command-Link       Net::Slot  Cb)
	  (Scb-Free-Cbl Net::Slot))	   1; Push this CB onto the front of*
    (Setf (Scb-Free-Cbl Net::Slot) Cb)	   1; the free CB list.*
    (Setf (Scb-Cbl-Offset     Net::Slot)   1; Clear SCB command block list*
	  *NULL-PTR*)			   1; since we have only one CB*
    )					   1; w/o interrupts*
  )					   1; free-command-block*

(Defmethod 1(NUBUS-ENC :ALLOCATE-COMMAND-BLOCK)* ()
  2"Allocate a free command block & return the command block pointer.
   if no command block is available, wait until the last command
   completes before freeing its command block."*
					   ; 29 JUN 87  MMG
  (Let (Block)	                           
    (When                                 
      (Eql (Scb-Free-Cbl Net::Slot) *NULL-PTR*) 1; Check free list*	   		   
      (Unless (Command-Interrupt-P Net::Slot)   1; if no CB available,* 1wait until last command completes.*
	(Ferror 'Enc-Timeout-Error	        1; signal error if timeout*
		" Ethernet controller command timed out"))
      (Let ((Cb (Scb-Cbl-Offset Net::Slot)))	1; Get pointer to CB in use*
	(When (Not (Eql Cb *NULL-PTR*))
	  (Send Self :Free-Xmit-Buffers	        1; Free transmit buffers if needed*
		(Command-Buffer-Ptr Net::Slot Cb))
	  (Send Self
		:Free-Command-Block Cb)         1; Free the command block*
	  (Ack-Command-Interrupt Net::Slot))	1; Then Ack command interrupt*
	)				   1; let*
      )					   1; when*
    (Without-Interrupts			   1; Pop this CB off the free list*
      (Setf Block (Scb-Free-Cbl Net::Slot)) 
      (Setf (Scb-Free-Cbl Net::Slot) (Command-Link Net::Slot Block)))
    Block)				   1; Return free command block*
  )					   1; allocate-command-block*


2;;*--------------------------------------------------------------------------------
2;;   TRANSMIT BUFFER INITIALIZATION:
;;*

(Defmethod 1(NUBUS-ENC :CREATE-XMIT-BUFFERS)* (Start-Addr Count Size) 
  2"Create COUNT buffers & descriptors of SIZE bytes, starting at START-ADDR.
   Returns the address of the first buffer in the list. Allocates each buffer immediately*
  2after its descriptor."*
  (Setf Size (Net::Round-Up-To-Quad Size))
  (Setf Transmit-Buffer-Size Size)
  (Do ((I 0 (+ 1 I))			           1; loop counter*
       (Addr Start-Addr (+ Addr Size Tbd-Size))	   1; address of buffer desc.*
       (Last-Addr *NULL-PTR* Addr))	           1; address of last buffer*
      ((Eql I Count)
       (Setf (Scb-Free-Tbd Net::Slot) Last-Addr)   1; Anchor the list.*
       Addr)				           1; Return avail address.*
    (Setf (Buffer-Link    Net::Slot Addr) Last-Addr)
    (Setf (Buffer-Address Net::Slot Addr) (+ Addr Tbd-Size))
    (Setf (Buffer-Count   Net::Slot Addr) Size)))  1; also clears End-of-Frame.*
     

2;;*--------------------------------------------------------------------------------
2;;   TRANSMIT BUFFER MANAGEMENT:
;;*

(Defmethod 1(NUBUS-ENC :FREE-XMIT-BUFFERS)* (Start-Tbd) 
  2"Frees all TBD's and buffers in the list rooted at START-TBD"*
					   ; 8 JUL 87  MMG
  (Without-Interrupts
    (Unless (Eql Start-Tbd *NULL-PTR*)
      (Do ((Buff Start-Tbd (Buffer-Link Net::Slot Buff))
	   (Last-Buff Nil Buff)
	   (Free-Count 0 (+ 1 Free-Count))
	   (Eof 0))
	  (())
	(When
	  (Or (Eql Buff *NULL-PTR*)	   1; If this is a null buffer*
	      (Eql Eof 1))		   1; or EOF was set in the last buffer,*
	  (Setf (Buffer-Link Net::Slot Last-Buff)
		(Scb-Free-Tbd Net::Slot))  1; Push this chain of freed buffers onto the*
	  (Setf (Scb-Free-Tbd Net::Slot)   1; front of the free TBD list*
		Start-Tbd)
	  (Return))			   1; and return.*
	(When
	  (> Free-Count Transmit-Buffers)  1; If more buffers than we have,*
	  (Ferror 'Enc-Buffer-Error	   1; signal an error.*
		  2"ENC Transmit data structures are damaged!"*))
	(Setf Eof			   1; Read EOF flag*
	      (Buffer-End-of-Frame Net::Slot Buff))
	(Setf (Buffer-Count Net::Slot Buff)	   1; Initialize the TBD data count*
	      Transmit-Buffer-Size)	   1; and EOF flag to clear this buffer*       
	)				   1; do*
      )					   1; when*
    )					   1; w/o interrupts*
  )					   1; free-xmit-buffers*

(Defmethod 1(NUBUS-ENC :COPY-TO-XMIT-BUFFERS) *(Array Data-Length)  
  2"Returns the address of the first allocated buffer after copying ARRAY into it.
   Nil is returned if there is not enough space. DATA-LENGTH is in bytes."*
					   ; 14 JUN 87  MMG
  (Without-Interrupts
    (Let ((Tbd (Scb-Free-Tbd Net::Slot))   1; First free Tbd*
	  (Buff-Size			   1; Size of each buffer in words*
	    (Net::Convert-To-Words Transmit-Buffer-Size))
	  (Words-To-Copy		   1; Total number of words to copy*
	    (Net::Convert-To-Words Data-Length))
	  Last-Tbd)
      (Unless (Eql Tbd *NULL-PTR*)	   1; Unless space is unavailable*,
	(Do* ((Start-Idx 0  End)	   1; Copy array segments to linked transmit buffers.*
	      (End (Min Words-To-Copy Buff-Size)
		   (Min Words-To-Copy (+ End Buff-Size)))
	      (Prev-Bd Tbd Current-Bd)
	      (Current-Bd Tbd (Buffer-Link Net::Slot Current-Bd)))
	     ((>= Start-Idx Words-To-Copy) 1; Quit when all words have been copied*
	      (Setf Last-Tbd Prev-Bd))	   1; and return the last Tbd*
	  (If (Eql Current-Bd *NULL-PTR*)
	      (Return)			   1; if out of space, quit (no tbd).*
	      (Xfer-Words-To-NuBus	   1; Else, copy array portion to this buffer*
		Net::Slot (Buffer-Address Net::Slot Current-Bd) Array Start-Idx End)
	      (Setf (Buffer-Count Net::Slot Current-Bd)
		    (* 2 (- End Start-Idx)))   1; and set number of bytes in buffer*
	      )				   1; if*
	  )				   1; do*	 
	(When Last-Tbd
	  (Setf (Buffer-End-Of-Frame
		  Net::Slot Last-Tbd) 1)	   1; Mark EOL*
	  (Setf (Scb-Free-Tbd Net::Slot)	   1; Put next free Tbd on free list.*
		(Buffer-Link Net::Slot Last-Tbd))
	  Tbd)
	)				   1; unless buffer not available*
      )					   1; let*
    )					   1; without-interrupts*
  )					   1; copy-to-xmit-buffer*



2;;*--------------------------------------------------------------------------------
2;;   RECEIVE BUFFER INITIALIZATION:
;;*

(Defmethod 1(NUBUS-ENC :CREATE-RECEIVE-BUFFERS)* (Start-Addr Count Size)
  2"Create COUNT buffers & descriptors of SIZE bytes, starting at START-ADDR."*
  (Declare (Values Next-Avail-Addr))
  1;; allocates each buffer immediately after its descriptor.*
  (Setf Size (Net::Round-Up-To-Quad Size))
  (Setf Last-Rbd Start-Addr)		           1; Init this value*
  (Do ((I 0 (+ 1 I))			           1; loop counter*
       (Addr Start-Addr (+ Addr Size Rbd-Size))	   1; address of buffer desc.*
       (Last-Addr *NULL-PTR* Addr))	           1; address of last buffer*
      ((Eql I Count)
       (Setf (Buffer-Link Net::Slot Start-Addr)
	     Last-Addr)			           1; close the list*
       (Setf (Buffer-End-of-Fbl Net::Slot Start-Addr) 1)	   1; make this one the tail*
       (Setf Last-Rbd Start-Addr)	           1; anchor the list*
       Addr)				           1; Return last buffer made.*
    (Setf (Buffer-Link       Net::Slot Addr) Last-Addr)
    (Setf (Buffer-Address    Net::Slot Addr) (+ Addr Rbd-Size))
    (Setf (Buffer-Size       Net::Slot Addr) Size) 1; also clears the End of FBL flag.*
    (Setf (Buffer-Count      Net::Slot Addr) 0)    1; also clears EOF & F flags.*
    (Setf (Buffer-End-Of-Fbl Net::Slot Addr) 0)))


(Defmethod 1(NUBUS-ENC :CREATE-RFA)* (Start-Addr Count &Aux Size)
  2"Create COUNT Receive Frame Descriptors, starting at START-ADDR.
   Returns the next available address in the buffer memory area."*
  (Setf Size Receive-Block-Size)
  (Setf Last-Free-Rfd Start-Addr)	   1; <HACK> Init this value.*
  (Do ((I 0 (+ 1 I))			   1; loop counter*
       (Addr Start-Addr (+ Addr Size))	   1; address of command block*
       (Last-Addr *NULL-PTR* Addr))	   1; address of last block*
      ((Eql I Count)
       (Setf (Command-Link        Net::Slot Start-Addr) Last-Addr)      1; Make '1st' RFD be the tail of the list.*
       (Setf (Command-Status      Net::Slot Start-Addr) *NULL-STATUS*)  1; Enable reuse of this block.*
       (Setf (Command-Word        Net::Slot Start-Addr) *NULL-CMD-EOL*) 1; Make this new end of list.*
       (Setf (Command-Buffer-ptr  Net::Slot Start-Addr) *NULL-PTR*)	1; Let go of any buffers*
       (Setf (Command-End-of-List Net::Slot Last-Free-Rfd) 0)           1; Reset the list pointers*
       (Setf Last-Free-Rfd Start-Addr)
       Addr)
    (Setf (Command-Status      Net::Slot Addr) *NULL-STATUS*)   1; Enable reuse of this block.*
    (Setf (Command-Word        Net::Slot Addr) *NULL-CMD-EOL*)  1; Make this new end of list.*
    (Setf (Command-Buffer-ptr  Net::Slot Addr) *NULL-PTR*)	1; Let go of any buffers*
    (Setf (Command-End-of-List Net::Slot Last-Free-Rfd) 0)	1; Reset the list pointers*
    (Setf Last-Free-Rfd Addr)
    (Setf (Command-Link Net::Slot Addr) Last-Addr))) 


2;;*--------------------------------------------------------------------------------
2;;   RECEIVE BUFFER MANAGEMENT:
;;*

(Defmethod 1(NUBUS-ENC :FREE-RCV-BUFFERS)* (Start-Rbd)
  2"Release all buffers starting at START-RBD for reuse by the controller."*
					   ; 20 JUL 87  MMG
  (Without-Interrupts
    (Unless (Eql Start-Rbd *NULL-PTR*)       
      (Do ((Buff Start-Rbd (Buffer-Link Net::Slot Buff))
	   (Last-Buff Nil Buff)
	   (Free-Count 0 (+ 1 Free-Count))
	   (Eof 0))
	  (())
	(When (Eql Eof 1)		   1; If EOF was set in the last buffer,*	   
	  (Setf (Buffer-End-of-Fbl Net::Slot Last-Buff) 1) 1; Set new end of FBL*
	  (Setf (Buffer-End-of-Fbl Net::Slot Last-Rbd)  0)  
	  (Setf Last-Rbd Last-Buff)	   
	  (Return))			   1; and return.*
	(When
	  (> Free-Count Rbds)		   1; If more buffers than we have,*
	  (Ferror 'Enc-Buffer-Error	   1; signal an error.*
		  2"ENC Receive data structures are damaged!"*))
	(Setf Eof			   1; Read EOF flag*
	      (Buffer-End-of-Frame Net::Slot Buff))
	(Setf (Buffer-Count Net::Slot Buff) 0)             1; Then clear the byte count*
	(Setf (Buffer-End-Of-Fbl Net::Slot Buff) 0))	   1; and flag fields*
      )					   1; unless null rbd*
    )					   1; w/o interrupts*
  )					   1; free receive buffers*

(Defmethod 1(NUBUS-ENC :FREE-RFD-AND-RCV-BUFFERS)* (Rfd Buff) 
  2"Release the RFD and any buffers it points to."*
  
  (Without-Interrupts
    (Send Self :Free-Rcv-Buffers Buff)	                           1; Free Rcv buffers*
    (Setf (Command-Status     Net::Slot Rfd) *NULL-STATUS*)	   1; Enable reuse of this block.*
    (Setf (Command-Word       Net::Slot Rfd) *NULL-CMD-EOL*)	   1; Make this the new end of list.*
    (Setf (Command-Buffer-Ptr Net::Slot Rfd) *NULL-PTR*)	   1; Let go of any buffers*
    (Setf (Command-End-Of-List Net::Slot Last-Free-Rfd) 0)	   1; Reset the list pointers*
    (Setf Last-Free-Rfd Rfd)
    )					   1; w/o interrupts*
  )					   1; free rfd and buffers*

(Defmethod 1(NUBUS-ENC :COPY-FROM-RCV-BUFFERS)* (Descriptor Array)
  2"Copies the contents of buffer described at DESCRIPTOR to ARRAY (an art-16b).
   Returns NIL if ran out of room, else the number of bytes copied."*
  (Declare (Values Num-Bytes-In-Array))
  (Do ((Array-Length (Array-Total-Size Array))
       (I 0 Next)			   1; Index into array*
       (Bd Descriptor
	   (If (Eql 1 (Buffer-End-Of-Frame Net::Slot Bd))
	       *NULL-PTR*		   1; End of buffer list*
	       (Buffer-link Net::Slot Bd))) 1; Next buffer*
       Next)				   1; Next value of I*
      ((Eql Bd *NULL-PTR*) (* 2 I))	   1; Return bytes copied.*
    (Setf Next (+ I (Net::Convert-To-Words
		      (Buffer-Byte-Count Net::Slot Bd))))
    (When (> (Net::Round-Up-To-Even Next) Array-Length)
      (Return ()))			   1; Error Return*
    (Xfer-Words-From-Nubus		   1; Copy across NuBus*
      Net::Slot (Buffer-Address Net::Slot Bd) Array I Next)))


(Defmethod 1(NUBUS-ENC :FIND-1ST-FREE-RBD)* (Start-Rbd)  
  2"Returns ptr to 1st Free RBD, Nil if none.  Starts at START-RBD."*
  (Do ((Rbd (Buffer-Link Net::Slot Start-Rbd)
	    (Buffer-Link Net::Slot Rbd))
       (Unfree-Count 0 (+ 1 Unfree-Count)))
      (())
    (When (Zerop (Buffer-Count Net::Slot Rbd))
      (Return Rbd))			   1; Found an unused buffer*
    (When (Eql Rbd Start-Rbd)
      (Return Nil))                        1; Nothing available*
    (When (> Unfree-Count Rbds)		   1; if too many rbds,*
      (Ferror 'Enc-Buffer-Error		   1; SIgnal an error.*
	      2"ENC Receive data structures are damaged!"*))
    )					   1; do*
  )					   1; find 1st free rbd*

(Defmethod 1(NUBUS-ENC :FIND-1ST-FREE-RFD) *(Start-Rfd)
  2"Returns ptr to 1st Free RFD.  Starts at START-RFD."*
  
  1;;  This may only be called AFTER at least one RFD has been released.*
  (Do ((Rfd (Command-Link Net::Slot Start-Rfd)
	    (Command-Link Net::Slot Rfd))
       (Unfree-Count 0 (+ 1 Unfree-Count)))
      (())
    (When (Eql (Command-Status Net::Slot Rfd) *NULL-STATUS*)
      (Return Rfd))			   1; Found an unused RFD*
    (When (Eql Rfd Start-Rfd)
      (Return Nil))                        1; Nothing available*
    (When (> Unfree-Count Rfds)		   1; if too many rfds,*
      (Ferror 'Enc-Buffer-Error		   1; SIgnal an error.*
	      2"ENC Receive data structures are damaged!"*))
    )					   1; do*
  )					   1; find 1st free rbd*


1;;*--------------------------------------------------------------------------------
1;;                   *     1 *           1INITIALIZATIONS
;;*--------------------------------------------------------------------------------

(Defmethod 1(NUBUS-ENC :AFTER :INIT)* (&Rest Ignore)
2  "Sets up controller meters and sets up the memory size and Ethernet address
 * 2as read from config ROM."*
  (Send Self :Add-Controller-Meter
	:NuBus-Errors 2"Number of NuBus errors encountered."*)
  (Send Self :Add-Controller-Meter
	:Receive-Frame-Errors 2"Number of frames actually read with errors."*)
  (Setf Memory-Size			   1; Read memory size*
	(If (Zerop (Mem-Size Net::Slot))
	    (* 8  1024)			   1;  8K memory if 0*
	    (* 32 1024)))		   1; 32K memory if 1*
  (Setf Ethernet-Address
	(Send Self :Read-Rom-Id))          1; Setup Ethernet-Address*
  (Send Self :Reset)                       1; Reset controller*
  )					   1; AFTER INIT*
 
(Defmethod 1(NUBUS-ENC :AFTER :RESET-METERS)* (&Rest Ignore)
  (Setf Receive-Frame-Errors 0)
  (Setf Nubus-Errors  0))

 
(Defmethod 1(NUBUS-ENC :READ-ROM-ID)* (&Aux Ethernet-ID)
  2"Returns the Ethernet ID from config rom"*
  (Setf Ethernet-Id 0)
  (Dotimes (I Enet-Addr-Len)		   1; Read Ethernet ID from config rom*
    (Setf Ethernet-Id
	  (Dpb (Si:%Nubus-Read-8B Net::Slot (+ *NUETHER-ID-ADDR* (Ash I 2)))
	       (Byte 8 0) (Ash Ethernet-Id 8))))
  (When (Or (Zerop Ethernet-Id)	           1; Check for validity*
	    (Eql Ethernet-Id BROADCAST-ADDRESS))
    (Error nil 2"Ethernet Board Config ROM has bad Ethernet address: ~16r"* Ethernet-Id))
  Ethernet-Id)


(Defmethod 1(NUBUS-ENC :INITIALIZE)* ()
  2"Performs the setup of the Ethernet controller and resets it."*
  
  (Send Self :Hw-Reset)                    1; Reset the hardware*
  (Send Self :Configure)		   1; Load 82586 parameters*
  (Send Self :IA-Setup)			   1; Load Ethernet address*
  (Send Self :MC-Setup)	                   1; Load Multicast addresses*
  (Send Self :Start-Receive-Unit)	   1; Start the Receive Unit*
  )					   1; INITIALIZE*


(Defmethod 1(NUBUS-ENC  :INITIALIZE-CONTROL-STRUCTURES) *(Nxmit-Frames)     
  2"Initialize all of the NuBus board Ethernet data structures."*
  (Init-Iscp Net::Slot			   1;* 1Make ISCP at location 0000*
	     (Send Self			   1;* 1Make SCB following the ISCP*
		   :Create-Scb Iscp-Size Nxmit-Frames))
  1;; Initialize the SCP*
  (Setf (Iscp-Address Net::Slot) 0)	   1;* 1Intermediate. Sys Control Ptr is at loc zero.*
  (Send Self :Set-Scp-Sysbus-Width 16))	   1;* 1Chip's bus is word mode.*


(Defmethod 1(NUBUS-ENC  :HW-RESET) *()     
2  "Completely resets the Ethernet controller hardware."*
					   ; 18 JUL 87  MMG
  (With-Controller-Disabled		   1; Disable controller*
    (With-Lock (Request-Lock)
      (Send Self                           1; Setup memory structures w/ 2 xmit frames.*
	    :Initialize-Control-Structures 2)
      (When (< (* Transmit-Buffers Transmit-Buffer-Size) MAX-FRAME-LENGTH)
	(Ferror 'Net::Local-Network-Error  1; Check buffer space*.
		2"Transmit buffer space is smaller than Ethernet maximum: ~d.<~d."*
		(* Transmit-Buffers Transmit-Buffer-Size) MAX-FRAME-LENGTH))
      
      (Setf (Scb-Command   Net::Slot  Scb-Addr)
	    *Null-Command*)		   1; Clear command word*
      (Setf (Hw-Reset Net::Slot)
	    (Net::Numerical-Value :ON))    1; Issue a hardware reset.*
      (If (Command-Complete-P Net::Slot)   1; Wait for zero command word*
	  (Progn
	    (Process-Sleep 1)		   1; then wait at least 8 clock cycles*
	    (Channel-Attention Net::Slot))	   1; before issuing Channel Attention*.
	  (Ferror 'Net::Local-Network-Error	   1; Signal error if reset failed*
		  2" Ethernet Board failed to RESET"*))
      (Setf (Fault-Led Net::Slot)
	    (Net::Numerical-Value :OFF))   1; Turn off NuBus Fault LED*
      (Setf (Master-Enable Net::Slot)	   1; Set NuBus Master Enable bit (Also set up *
	    (Net::Numerical-Value
	      *Interrupt-Driven-P*))	   1; the Event Address if Interrupt-driven).*      
      )					   1; with lock*    
    )					   1; with controller disabled*
  )					   1; HW-RESET*
 
(Defmethod 1(NUBUS-ENC  :IA-SETUP)* (&Optional Ethernet-Id)
2  "Sets up the controller with the Ethernet Address ID. If ETHERNET-ID
   is not specified, it is read from config rom."*
					   ; 17 AUG 87 MMG
  (When Ethernet-Id
    (Setf Ethernet-Address Ethernet-Id))
  (Issue-Command-Block Net::Slot Cb t
    (Setf (Command-Word Net::Slot Cb) ADDRESS-SETUP)
    (Setf (Enc-Ether-Address Net::Slot (+ CB 6)) Ethernet-Address)
    )					   1; issue-command-block*
  )					   1; IA-setup*

(Defmethod 1(NUBUS-ENC  :CONFIGURE)* () 
  2"Sets up the controller board with Ethernet configuration parameters."*
					   ; 03 AUG 87  MMG
  (Issue-Command-Block Net::Slot Cb t	   1; Set up Command Block*
    (Setf (Command-Word      Net::Slot Cb) CONFIGURE)
    (Setf (Cfg-byte-cnt      Net::Slot Cb) 12)
    (Setf (Cfg-fifo-lim      Net::Slot Cb) Enet-Fifo-Threshold)
    (Setf (Cfg-sync          Net::Slot Cb) (Net::Numerical-Value Enet-Srdy))
    (Setf (Cfg-sav-bf        Net::Slot Cb) (Net::Numerical-Value Enet-Sav-Bf))
    (Setf (Cfg-addr-len      Net::Slot Cb) Enet-Addr-Len)
    (Setf (Cfg-at-loc        Net::Slot Cb) (Net::Numerical-Value Enet-At-Field-Loc))
    (Setf (Cfg-pream-len     Net::Slot Cb) (- (Integer-Length Enet-Pream-Len) 2.))
    (Setf (Cfg-int-lpbck     Net::Slot Cb) (Net::Numerical-Value Enet-Int-Lpbck))
    (Setf (Cfg-ext-lpbck     Net::Slot Cb) (Net::Numerical-Value Enet-Ext-Lpbck))
    (Setf (Cfg-lin-prio      Net::Slot Cb) Enet-Lin-Prio)
    (Setf (Cfg-acr           Net::Slot Cb) Enet-Acr)
    (Setf (Cfg-bof-met       Net::Slot Cb) (Net::Numerical-Value Enet-Bof-Met))
    (Setf (Cfg-if-spacing    Net::Slot Cb) Enet-If-Spacing)
    (Setf (Cfg-Slot-time     Net::Slot Cb) Enet-Slot-Time)
    (Setf (Cfg-retry-num     Net::Slot Cb) Enet-Retry-Num)
    (Setf (Cfg-prm           Net::Slot Cb) (Net::Numerical-Value Enet-Prm))
    (Setf (Cfg-bc-dis        Net::Slot Cb) (Net::Numerical-Value Enet-Bc-Dis))
    (Setf (Cfg-manch-nrz     Net::Slot Cb) (Net::Numerical-Value Enet-Manch))
    (Setf (Cfg-tono-crs      Net::Slot Cb) (Net::Numerical-Value Enet-Tono-Crs))
    (Setf (Cfg-ncrc-ins      Net::Slot Cb) (Net::Numerical-Value Enet-Ncrc-Ins))
    (Setf (Cfg-crc-16        Net::Slot Cb) (Net::Numerical-Value Enet-Crc-16))
    (Setf (Cfg-bt-stf        Net::Slot Cb) (Net::Numerical-Value Enet-Bt-Stf))
    (Setf (Cfg-pad           Net::Slot Cb) (Net::Numerical-Value Enet-Pad))
    (Setf (Cfg-crsf          Net::Slot Cb) Enet-Crsf)
    (Setf (Cfg-crs-src       Net::Slot Cb) (Net::Numerical-Value Enet-Crs-Src))
    (Setf (Cfg-cdtf          Net::Slot Cb) Enet-Cdtf)
    (Setf (Cfg-cdt-src       Net::Slot Cb) (Net::Numerical-Value Enet-Cdt-src))
    (Setf (Cfg-min-frm-len   Net::Slot Cb) Enet-Min-Frm-Len)
    )					   1; issue-command-block*
  )					   1; CONFIGURE*


(Defmethod 1(NUBUS-ENC :MC-SETUP)* (&Optional New-List &Aux Mc-List-Offset)      
2  "Sets up the controller with valid multicast addresses. If NEW-LIST is not
   specified, the instance variable MC-ADDR-LIST is used, else MC-ADDR-LIST is
   updated to NEW-LIST. An empty multicast address list disables reception of
   any frame with a multicast address."*
					   ; 13 JUL 87 MMG
  (When (Setf Mc-Addr-List (Or New-list Mc-Addr-List))
    (If (> (List-Length Mc-Addr-List) MAX-MC-ADDR)
	(Error nil 2"Multicast address list is too large; ~a addresses, ~a allowed."*
	       (List-Length Mc-Addr-List) MAX-MC-ADDR))
    (Issue-Command-Block Net::Slot Cb t
      (Setf (Command-Word Net::Slot Cb) MULTICAST-SETUP)
      (Setf Mc-List-Offset (+ Mc-List cb))
      
      1;; Set up Command Block*:      
      (Setf (Mc-Cnt Net::Slot Cb)		   1; Address byte count.*
	    (* (List-Length Mc-Addr-List) Enet-Addr-Len))	
      (Dolist (Enet-Addr Mc-Addr-List)	   1; Load addresses.*
	(If (Not (Logbitp (* 8 (- Enet-Addr-Len 1)) Enet-Addr))
	    (Error nil 2"Illegal multicast address; first octet lsb is zero: ~16R"* enet-addr)) 
	(Set-Enc-Ether-Address Net::Slot Mc-List-Offset Enet-Addr)
	(Setf Mc-List-Offset (+ Mc-List-Offset Enet-Addr-Len))
	)				   1; dolist*
      )					   1; issue-command-block*
    )					   1; when Mc-Addr-List*
  )					   1; MC-SETUP*


(Defmethod 1(NUBUS-ENC  :START-RECEIVE-UNIT)* () 
2  "Enable the Ethernet Controller to receive frames.
   Return nil if the receive unit could not be started."*
					   ; 16 JUN 87  MMG
  (With-Lock (Request-Lock)
    (Unless (Eql (Scb-Receive-Unit-Status Net::Slot)
		RECEIVE-READY)	           1; Don't start if already running.*
      (Without-Interrupts		   1; Interrupts off if event driven!*
	1;; Find the first free Rfd and Rbd:*
	(Let ((Rfd (Send Self :Find-1st-Free-Rfd Last-Free-Rfd))
	      (Rbd (Send Self :Find-1st-Free-Rbd Last-Rbd)))
	  (When (And Rfd Rbd)		   1; Setup pointers to the Rfd and Rbd:*
	    (Setf (Command-Buffer-Ptr Net::Slot  Rfd) Rbd)	   
	    (When (Command-Complete-P Net::Slot)  1; Make sure last command completed.*
	      (Setf (Scb-Rfa-Offset   Net::Slot) Rfd)
	      (Issue-Control-Command Net::Slot t  1; Issue start & channel attention*
		(Setf (Scb-Receive-Command Net::Slot) SCB-START-CMD)
		)			   1; issue-control-command*
	      T)			   1; when last command completes*
	    )				   1; when rfd and rbd*
	  )				   1; let*
	)				   1; w/o interrupts*
      )					   1; unless already started*
    )					   1; with lock*
  )					   1; START-RECEIVE-UNIT*



1;;*--------------------------------------------------------------------------------
1;;                *     1  *          1DATALINK INTERFACE 
;;*--------------------------------------------------------------------------------
  
(Defmethod 1(NUBUS-ENC :TRANSMIT)* (Type Dest Data-Array N-Bytes
			        &Optional (Deallocate T) &Aux Blk)
  2"Transmit the 16-bit DATA-ARRAY to Ethernet address DEST.
   N-BYTES    = Number of bytes to transmit.
   TYPE       = Ethernet Frame type, :CHAOS or numeric Ethernet type code.
   DEALLOCATE = T for arrays that must be freed after transmission."*
                                                                      ; 15 JUL 87  MMG
  (Transforming-Arguments Type Dest Data-Array N-Bytes Deallocate
    (Handling-Conditions                                           
      (Issue-Command-Block Net::Slot Cb Nil
	(Do ((Buff (Send Self :Copy-To-Xmit-Buffers Data-Array N-Bytes)
		   (Send Self :Copy-To-Xmit-Buffers Data-Array N-Bytes)))
	    (Buff			                              1; Exit loop when copy is successful*:
	      (Setf (Command-Word             Net::Slot Cb) TRANSMIT) 1; Setup Command field*
	      (Setf (Command-Buffer-Ptr       Net::Slot Cb) Buff)     1; Setup Command block pointer*
	      (Setf (Command-Destination-Addr Net::Slot Cb) Dest)     1; Setup Destination address*
	      (Setf (Command-Frame-Type       Net::Slot Cb) Type))    1; Setup Type field*
	  
	  (If (Eql (Setf Blk (Scb-Cbl-Offset Net::Slot)) *NULL-PTR*)  1; Nothing to deallocate?*
	      (Ferror 'Enc-Buffer-Error	                              1; Then signal condition*
		      "ENC Command buffers are damaged!")	           
	      (If (Command-Interrupt-P Net::Slot)	              1; Else, wait for last XMIT to complete*
		  (Progn
		    (Incf Collision-Count
			  (Command-Retry-Count Net::Slot Blk))        1; Update collision count*
		    (Send Self :Free-Xmit-Buffers	              1; Free transmit buffers*
			  (Command-Buffer-Ptr Net::Slot Blk))
		    (Send Self :Free-Command-Block Blk)               1; Free the command block*
		    (Ack-Command-Interrupt Net::Slot))                1; Then Ack command interrupt*
		  (Ferror 'Enc-Timeout-Error
			  "ENC Transmit Timeout")))	              1; Else signal timeout error*
	  )				   1; do*
	)				   1; issue* 1command* 1block*
      )					   1; handling conditions*
    )					   1; transforming args*
  )					   1; transmit*


(Defmethod 1(NUBUS-ENC :RECEIVE)* ()
  2"Returns the DESTINATION, SOURCE, TYPE and DATA for the next valid frame;
   DATA is returned as a 16-bit array."*
					                   ; 18 JUN 87  MMG
  (Block Receive
    (Handling-Conditions		                   1; Reinit and Restart on errors.*      
      (Let (Type Array N-Bytes)
	(Loop
	  (Let ((Rfd
		  (Scb-Current-Receive-Frame Net::Slot))   1; Get current receive frame area*
		Buff Receiver)	    
	    (If (Zerop
		  (Command-Complete-Flag Net::Slot Rfd))   1; No received frame?*
		(If (And (Receive-Interrupt-P Net::Slot)   1; Then Wait for receive interrupt*
			 (Command-Complete-P Net::Slot))   1; Finish pending control commands*
		    (Ack-Receive-Interrupt Net::Slot)      1; Ack the receive interrupt*
		    (Ferror 'Enc-Timeout-Error
			    "ENC receive timeout"))	   1; Signal error if wait timed out. *
		
		(Unwind-Protect		                   1; Frame Received, *
		    (Progn		                   1; So do receive-processing*
		      (Setf Buff (Command-Buffer-Ptr Net::Slot Rfd))
		      (If (Not (Eql (Command-Status  Net::Slot Rfd)	   1 *
				    *NORMAL-FINISH*))	   1; If it didn't complete normally,* 
			  (Incf Receive-Frame-Errors)	   1; bump error stat* 1and forget it*
			  (Progn	                   1; else, continue (valid frame).*
			    (Setf Type (Receive-Frame-Type Net::Slot Rfd))
			    (When	                   1; If known type field,*
			      (Member Type Valid-Pkt-Types)
			      (Setf Array                  1; copy to a packet*,
				    (Allocate-Net-Packet Type))
			      (If (Setf N-Bytes
					(Send Self :Copy-From-Rcv-Buffers Buff Array))
				  (Return-From Receive	   1; and return*
				    (Command-Destination-Addr Net::Slot Rfd)
				    (Receive-Source-Addr      Net::Slot Rfd)
				    Type Array N-Bytes Receiver)				
				  (Progn                   1; unless pkt too big.*
				    (Incf Net::Pkts-Too-Big-To-Receive) 
				    (Deallocate-Net-Packet Type Array)))
			      )		   1; if known type*
			    )		   1; process valid frame*
			  )		   1; if didn't complete properly*
		      )			   1; receive-processing*					   
		  (Send Self :Free-Rfd-And-Rcv-Buffers Rfd Buff) 1; Always Free Rfd/Buffers*    
		  (Send Self :Start-Receive-Unit)                1; and restart Rcv unit*
		  )			   1; unwind-protect*	      
		)			   1; frame complete*
	    )				   1; let 2*
	  )				   1; loop*
	)				   1; let 1*
      )					   1; handling conditions*  
    )					   1; receive block*
  )					   1; receive*

 
1;;*-------------------------------------------------------------------------------------
1;;           *    1        *      1 *          1DISPLAYS
;;*-------------------------------------------------------------------------------------

(Defmethod 1(NUBUS-ENC :PRINT-ISCP) *(&Optional All? (Stream *Terminal-Io*))
  2"Prints the current state of ISCP (everything when ALL? is T) on STREAM."*
  (Format Stream 3"~%*  3Initialization ~A"*
	  (If (Iscp-Busyp Net::Slot) 3"In Progress"* 3"Complete"*))
  (When All?
    (Format Stream 3"~%*  3SCB base address = ~16,6,48,r~%*  3SCB offset address = ~16,4,48r"*
	    (Scb-Base Net::Slot Iscp-Addr) (Scb-Offset Net::Slot Iscp-Addr)))) 

(Defmethod 1(NUBUS-ENC :PRINT-SCB-STATUS) *(&Optional (Stream *Terminal-Io*))
  2"Print the Status of the SCB for ENC on STREAM."*
  (Format stream 3"~&*   3Interrupts Pending: "*)
  (If (= 0 (scb-int-flags Net::Slot scb-addr))
    (princ 3"None"* stream)
    (progn
      (if (= 1 (scb-cx-flag Net::Slot scb-addr))
	(princ 3"CX  "* stream))
      (if (= 1 (scb-fr-flag Net::Slot scb-addr))
	(princ 3"FR  "* stream))
      (if (= 1 (scb-cnr-flag Net::Slot scb-addr))
	(princ 3"CNR  "* stream))
      (if (= 1 (scb-rnr-flag Net::Slot scb-addr))
	(princ 3"RNR"* stream))))
  (format stream 3"~&*   3Control Command = ~A~%*   3Receive Command = ~A"*
	  (select (scb-control-command Net::Slot scb-addr) ((SCB-NOP-CMD) 3"NOP"*) ((SCB-START-CMD) 3"START"*)
	     ((SCB-RESUME-CMD) 3"RESUME"*) ((SCB-SUSPEND-CMD) 3"SUSPEND"*) ((SCB-ABORT-CMD) 3"ABORT"*)
	     (otherwise 3"Invalid Command"*))
	  (select (scb-receive-command Net::Slot scb-addr) ((SCB-NOP-CMD) 3"NOP"*) ((SCB-START-CMD) 3"START"*)
	     ((SCB-RESUME-CMD) 3"RESUME"*) ((SCB-SUSPEND-CMD) 3"SUSPEND"*) ((SCB-ABORT-CMD) 3"ABORT"*)
	     (otherwise 3"* 3Invalid Command"*)))
  (format stream 3"~&*   3Command Unit = ~A~%*   3Read Unit = ~A"*
	  (select (scb-command-unit-status Net::Slot scb-addr) ((IDLE-STATE) 3"Idle"*)
	     ((SUSPENDED-STATE) 3"Suspended"*) ((COMMAND-READY) 3"Active"*)
	     (otherwise 3"Invalid State"*))
	  (select (scb-receive-unit-status Net::Slot scb-addr) ((IDLE-STATE) 3"Idle"*)
	     ((SUSPENDED-STATE) 3"Suspended"*) ((NO-RESOURCES) 3"No Resources"*)
	     ((RECEIVE-READY) 3"Ready"*) (otherwise 3"Invalid State"*)))) 


(Defmethod 1(NUBUS-ENC :PRINT-SCB-STATE) *(&optional (stream *terminal-io*))
  2"Print the entire state of the SCB for ENC."*
  (Send Self :Print-Scb-Status Stream)
  (Send Self :Print-Cbl-Status Stream t)          1;* 1Print status of 1st item on CBL.*
  (Send Self :Print-Rfa-Status Stream t)
  (format stream 3"~&* 3Free Block: ~16,4,48r~@
                    * 3Free Buff : ~16,4,48r~%"*
	  (scb-free-cbl Net::Slot) (scb-free-tbd Net::Slot)))


(Defmethod 1(NUBUS-ENC :PRINT-GENERIC-FRAME-STATUS) *(frame-address &optional (stream *terminal-io*))
  2"Prints the status info common to all frame blocks.
   FRAME-ADDRESS is the address of the frame."*
  (if (= 0 (command-execution-status Net::Slot frame-address))
    (progn
      (terpri stream)
      (princ 3"*  3Block is available"* stream)))
  (print-flag-doc 1 command-complete-flag Net::Slot frame-address)
  (print-flag-doc 1 command-busy-flag     Net::Slot frame-address)
  (print-flag-doc 1 command-error-flag    Net::Slot frame-address)) 

(Defmethod 1(NUBUS-ENC :PRINT-COMMAND-NAME) *(cmd-block &optional (stream *terminal-io*))
  (terpri stream)
  (Princ
   (Documentation
     (Select (command-block-cmd Net::Slot cmd-block)
       (NOP-COMMAND 'nop-command)
       (ADDRESS-SETUP 'address-setup)
       (CONFIGURE 'configure)
       (MULTICAST-SETUP 'multicast-setup)
       (TRANSMIT 'transmit)
       (TDR-COMMAND 'tdr-command)
       (DUMP-STATUS 'dump-status)
       (DIAGNOSE-586 'diagnose-586)) 'variable)
   Stream))
  

(Defmethod 1(NUBUS-ENC :PRINT-COMMAND-BLOCK-STATUS) *(cmd-block &optional (stream *terminal-io*))
  2"Prints the status info for a command block."*
  (format stream 3"~%*  3COMMAND-BLOCK STATUS: Address = ~16,4,48r"* cmd-block)
  (Send Self :Print-Command-Name Cmd-Block Stream)
  (Send Self :Print-Generic-Frame-Status cmd-block stream)
  (print-flag-doc 1 command-diagnose-fail Net::Slot cmd-block)
  (print-flag-doc 1 command-abort-error   Net::Slot cmd-block)
  (print-flag-doc 1 command-no-carrier    Net::Slot cmd-block)
  (print-flag-doc 1 command-no-clear-send Net::Slot cmd-block)
  (print-flag-doc 1 command-dma-error     Net::Slot cmd-block)
  (print-flag-doc 1 command-xmit-deferred Net::Slot cmd-block)
  (print-flag-doc 1 command-retry-error   Net::Slot cmd-block)
  (if (> 0 (command-retry-count           Net::Slot cmd-block))
    (format stream 3"~%*  3~D Frame collisions"*
	    (command-retry-count Net::Slot cmd-block)))) 

(Defmethod 1(NUBUS-ENC :PRINT-RECEIVE-FRAME-STATUS) *(frame &optional (stream *terminal-io*))
  2"Prints the status info for a receive frame block."*
  (format stream 3"~%*  3RECEIVE-FRAME STATUS: Address = ~16,4,48r"* frame)
  (unless (= (command-buffer-ptr Net::Slot frame) *NULL-PTR*)
    (format stream 3"~&*  3Buffer pointer = ~16,4,48r"*
	    (command-buffer-ptr Net::Slot frame)))
  (Send Self :Print-Generic-Frame-Status frame stream)
  (print-flag-doc 1 command-crc-error    Net::Slot frame)
  (print-flag-doc 1 command-align-error  Net::Slot frame)
  (print-flag-doc 1 command-buffer-error Net::Slot frame)
  (print-flag-doc 1 command-dma-error    Net::Slot frame)
  (print-flag-doc 1 command-too-short    Net::Slot frame)
  (print-flag-doc 1 command-no-eof-error Net::Slot frame)) 

(Defmethod 1(NUBUS-ENC :PRINT-RECEIVE-BUFFER-STATUS) *(Buff &Optional (Stream *Terminal-Io*))
  2"Prints the state of the receive Buffer at BUFF."*
  (format stream
	  3"~%*  3Receive Buffer Descriptor: ~16,4,48r, Buffer: ~16,6,48r, Size = ~16,4,48r bytes"*
	  buff (buffer-address Net::Slot buff) (buffer-size Net::Slot buff))
  (format stream 3"~%*  3Actual byte count = ~16,4,48r"*
	  (buffer-byte-count Net::Slot buff))
  (print-flag-doc 1 buffer-full Net::Slot buff)
  (print-flag-doc 1 buffer-end-of-frame Net::Slot buff)) 

(Defmethod 1(NUBUS-ENC :PRINT-CBL-STATUS) *(&optional (stream *terminal-io*) first-only-p)
  2"Print the status of the Command-Block List on ENC."*
  (format stream 3"~&*  3Command Block List (CBL) = ~16,4,48r"* (scb-cbl-offset Net::Slot scb-addr))
  (unless (Eql *NULL-PTR* (scb-cbl-offset Net::Slot scb-addr))
    (do ((cb (scb-cbl-offset Net::Slot scb-addr) (command-link Net::Slot cb)))
	((or first-only-p (= 1 (command-end-of-list Net::Slot cb)))
	 (Send Self :Print-Command-Block-Status cb stream))
      (Send Self :Print-Command-Block-Status cb stream)))) 

(Defmethod 1(NUBUS-ENC :PRINT-RFA-STATUS) *(&optional (stream *terminal-io*) first-only-p)
  2"Print the status of the Received Frames on ENC."*
  (format stream 3"~&*  3Receive Frame Area (RFA) = ~16,4,48r"* (scb-current-receive-frame Net::Slot))
  (do ((rfd (command-link Net::Slot last-free-rfd) (command-link Net::Slot rfd))
       (prev-rfd 0 rfd))
      ((or first-only-p (= 1 (command-end-of-list Net::Slot rfd)))
       (Send Self :Print-Receive-Frame-Status rfd stream))
    (Send Self :Print-Receive-Frame-Status rfd stream)
    (format stream 3"   (~16R)"* (- prev-rfd rfd)))) 

(Defmethod 1(NUBUS-ENC :PRINT-ALL-RECV-BUFFERS) *(&optional (stream *terminal-io*) first-only-p)
  2"Print the status of the Receive Buffers on ENC."*
  (do ((rbd (buffer-link Net::Slot last-rbd) (buffer-link Net::Slot rbd)))
      ((or first-only-p (= 1 (buffer-end-of-fbl Net::Slot rbd)))
       (Send Self :Print-Receive-Buffer-Status rbd stream))
    (Send Self :Print-Receive-Buffer-Status rbd stream)))

(Defmethod 1(NUBUS-ENC :PRINT-CONTROL-SPACE-STATUS) *(&optional (stream *terminal-io*))
  2"Print the last written and the currently read state of ENC on STREAM."*
  (format stream 3"2~2%**  3CONTROL SPACE BITS"*)
  (format stream 3"~%    * 3 Master Enabled:    ~A"* (Net::On-Off (master-enable Net::Slot)))
  (format stream 3"~%    * 3      Fault LED:    ~A"*  (Net::On-Off (fault-led Net::Slot)))
  (format stream 3"~%    * 3      Loop Back:    ~A"*  (Net::On-Off (loopback Net::Slot))))


(Defmethod 1(NUBUS-ENC :PRINT-ETHERNET-ID)* (&Optional (Stream *Terminal-Io*)
				        &Rest Ignore &Aux Id)
  2"Prints the Ethernet ID of this controller"*
  (Let ((Addr (+ Scb-Addr Scb-Size)))	   1;* 1Dump area follows the SCB*
    (With-Controller-Disabled
      (Issue-Command-Block Net::Slot Cb t
	(Setf (Command-Buffer-Ptr Net::Slot Cb) Addr)
	(Setf (Command-Word Net::Slot Cb) DUMP-STATUS))
      (Setf Id (Enc-Ether-Address Net::Slot (+ Addr 12)))
      (Format Stream 2"~%~% * 2Ethernet Address loaded into this controller: #x~16r"* ID)
      )					   1; with controller disabled*
    )					   1; let*
  )					   1; PRINT ENET ID*

(Defmethod 1(NUBUS-ENC :PRINT-586-CONFIG)* (&Optional (Stream *Terminal-Io*)
				       &Aux Cmd-Blk)
  2"Dump the internal status of the 586 controller and print some of the data."*
  (Let ((Addr (+ Scb-Addr Scb-Size)))	   1;* 1Dump area follows the SCB*
    (With-Controller-Disabled
      (Issue-Command-Block Net::Slot Cb t
	(Setf Cmd-Blk Cb)
	(Setf (Command-Buffer-Ptr Net::Slot Cb) Addr)
	(Setf (Command-Word Net::Slot Cb) DUMP-STATUS))
      (Format Stream 2"~%~%  82586 LAN Processor configuration:"*)
      
      (Format Stream "~%    Save Bad Frames in Memory is ~a"  (Net::On-Off (Dump-Sav-Bf Net::Slot Addr)))
      (Format Stream "~%    Number of address bytes = ~a"     (Dump-Addr-Len Net::Slot Addr))
      (Format Stream "~%    Preamble Length = ~a bytes."      (expt 2 (+ 1 (Dump-Pream-Len Net::Slot Addr))))
      (Format Stream "~%    Internal Loopback is ~a"          (Net::On-Off (Dump-Int-Lpbck Net::Slot Addr)))
      (Format Stream "~%    External Loopback is ~a"          (Net::On-Off (Dump-Ext-Lpbck Net::Slot Addr)))
      (Format Stream "~%    Linear Priority = ~a"             (Dump-Lin-Prio Net::Slot Addr))
      (Format Stream "~%    Accelerated Contention Resolution = ~a"  (Dump-Acr Net::Slot Addr))
      (Format Stream "~%    Exponential Backoff Method is ~a" (Net::On-Off (Dump-Bof-Met Net::Slot Addr)))
      (Format Stream "~%    Interframe spacing = ~a"          (Dump-If-Spacing Net::Slot Addr))      
      (Format Stream "~%    Slot time number = ~a"            (Dump-Slot-Time Net::Slot Addr))      
      (Format Stream "~%    Max no. retries on collisions = ~a"      (Dump-Retry-Num Net::Slot Addr))      
      (Format Stream "~%    Promiscuous Mode is ~a"           (Net::On-Off (Dump-Prm Net::Slot Addr))) 
      (Format Stream "~%    Broadcast Disable is ~a"          (Net::On-Off (Dump-Bc-Dis Net::Slot Addr)))
      (Format Stream "~%    Encoding = ~a" (if (zerop (Dump-Manch-Nrz Net::Slot Addr)) "NRZ" "Manchester"))
      (Format Stream "~%    Transmit on no carrier sense is ~a" (Net::On-Off (Dump-Tono-Crs Net::Slot Addr)))
      (Format Stream "~%    CRC insertion is ~a" (if (zerop (Dump-Ncrc-Ins Net::Slot Addr)) "ON" "OFF"))
      (Format Stream "~%    CRC Type = ~a" (if (zerop (Dump-Crc-16 Net::Slot Addr)) "32 bit" "16 bit"))
      (Format Stream "~%    Bitstuffing is ~a"                (Net::On-Off (Dump-Bt-Stf Net::Slot Addr)))
      (Format Stream "~%    Padding is ~a"                    (Net::On-Off (Dump-Pad Net::Slot Addr)))
      (Format Stream "~%    Carrier Sense Filter = ~a"        (Dump-Crsf Net::Slot Addr))
      (Format Stream "~%    Carrier Sense Source = ~a" (if (zerop (Dump-Crs-Src Net::Slot Addr))
							   "External" "Internal"))
      (Format Stream "~%    Collision detect filter = ~a"     (Dump-Cdtf Net::Slot Addr))
      (Format Stream "~%    Collision detect Source = ~a" (if (zerop (Dump-Cdt-Src Net::Slot Addr))
							      "External" "Internal"))      
      (Format Stream "~%    Minimum frame length = ~a bytes." (Dump-Min-Frm-Len Net::Slot Addr))      
      )					   1; with controller disabled*
    )					   1; let*
  )					   1; PRINT-586-CONFIG*


(Defmethod 1(NUBUS-ENC :PRINT-586-STATUS) *(&Optional (Stream *Terminal-Io*))
  2"Prints the status of the 82586 Lan Processor in this controller"*
  
  (Format Stream 2"~%~%  Status of the 82586 LAN processor:"*)
  (Send Self :Print-Scb-Status Stream)
  )					   1; PRINT-586-STATUS*

(Defmethod 1(NUBUS-ENC :PRINT-586-DUMP)* (&Optional (Stream *Terminal-Io*) &rest ignore)
  2"Prints a synopsis of the internal status of the 82586 controller chip"*
  
  (Send Self :Print-586-Config Stream)
  (Send Self :Print-586-Status Stream)
  (Send self :Print-Error-Counts Stream)
  )					   1; PRINT-586-DUMP*

(Defmethod 1(NUBUS-ENC :PRINT-STATUS) *(&Optional (Stream *Terminal-Io*))
  2"Prints the status of the 82586 Lan Processor in this controller"*
  (Format Stream 2"~:|~3% Status of NuBus Ethernet controller in slot ~x:"* Net::Slot)
  (Send Self :Print-Ethernet-Id Stream)
  (Send Self :Print-Control-Space-Status Stream)
  (Send Self :Print-586-Dump Stream)
  )					   1; PRINT-STATUS*

(Defmethod 1(NUBUS-ENC :PRINT-ERROR-COUNTS) *(&Optional (Stream *Terminal-Io*) Reset-P)	  
  2"Print the statistics for the SCB at ENC."*
  (Format stream 2"~2%  CRC Errors:            ~d"* (scb-crcerrs Net::Slot)) 
  (Format stream 2"~%  Misaligned Frames:     ~d"* (scb-alnerrs  Net::Slot)) 
  (Format stream 2"~%  No Resource Errors:    ~d"* (scb-rscerrs Net::Slot)) 
  (Format stream 2"~%  Overrun Errors:        ~d"* (scb-ovrnerrs Net::Slot)) 
  (When Reset-P (Send Self :Update-Stats))
  )					   1; PRINT-ERROR-COUNTS*

(Defmethod 1(NUBUS-ENC  :UPDATE-STATS)* ()
2  "Updates instance variables from error statistics recorded by the 82586."*
  
  (Without-Interrupts
    (Incf Net::Fcs-Errors		   1; Increment FCS by CRC and Alignment errors.*
	  (+ (Scb-Crcerrs Net::Slot) (Scb-Alnerrs  Net::Slot)))
    (Incf Net::Pkts-Lost		   1; Increment Packets-Lost by Resource and Overrun errors.*
	  (+ (Scb-Rscerrs Net::Slot) (Scb-Ovrnerrs Net::Slot)))
    (Reset-Scb-Stats Net::Slot)		   1; Reset chip statistics registers.*
    )					   1; w/o interrupts*
  )					   1; UPDATE-STATS*

 
1;;*--------------------------------------------------------------------------------
1;;           *    1        *      1 *         1DIAGNOSTICS
;;*--------------------------------------------------------------------------------

(Defmethod 1(NUBUS-ENC :MEMORY-DUMP)* (&Optional (Base-Addr 0))
  2"Dumps the contents of the NuBus Controller memory to the screen."*
  (Do ((Base-Addr Base-Addr (+ Base-Addr 2048)))
      ((Or (Eql Base-Addr Memory-Size)
	   (Not (Y-or-N-P 3"Next page? "*))))
    (Format t 3"~:|"*)			   1;* 1Clear the Screen.*
    (Do ((Line 0 (+ Line 32)))
	((Eql Line 2048))
      (Do* ((I 30 (- I 16))
	    (N))
	   ((< I 0))
	(Setf N (+ Base-Addr Line I))
	(Format t
		3"~16,4,48R ~16,4,48R  ~16,4,48R ~16,4,48R   ~16,4,48R ~16,4,48R  ~16,4,48R ~16,4,48R : "*
		(Nubus-Read Net::Slot (- N  0))
		(Nubus-Read Net::Slot (- N  2))
		(Nubus-Read Net::Slot (- N  4))
		(Nubus-Read Net::Slot (- N  6))
		(Nubus-Read Net::Slot (- N  8))
		(Nubus-Read Net::Slot (- N 10))
		(Nubus-Read Net::Slot (- N 12))
		(Nubus-Read Net::Slot (- N 14))))
      (Format t 3"~16,3,48R~%"* (+ Base-Addr Line))
      )					   1; do 2*
    )					   1; do 1*
  )					   1; MEMORY-DUMP

2;;**--------------------------------------------------------------------------------
2;;   SELF TESTS:
;;*

(Defmethod 1(NUBUS-ENC :MEMORY-TEST)* (&Optional (Value-Base 0) (Print? T)
				    &Aux (Ok-P T))
  2"Do a read-back check of every word of buffer memory.  Returns T if OK.
   Should *ONLY* be run with the network disabled (preferably the cable
   unplugged as well)."*

  (Format t 2"~2% Executing Memory Test of NuBus Controller Board in slot ~x . . ."* Net::Slot)
  (Let ((Nwords (Truncate Memory-Size 2)))
    (With-Controller-Disabled
      (Dotimes (I Nwords)
	(Nubus-Write Net::Slot (* 2 I) (+ I Value-Base)))
      (Dotimes (I Nwords)
	(Unless (Eql (+ I Value-Base) (Nubus-Read Net::Slot (* 2 I)))
	  (When Print?
	    (Format t 3"~%*  3~16,4,48r: = ~16,4,48r, should be ~16,4,48r"* (* 2 i)
		    (Nubus-Read Net::Slot (* 2 I)) (+ I Value-Base)))
	  (Setf Ok-P ())))
      (When (And Ok-P Print?)
	(Format t 2"~%  Mem3ory Test OK**.3"*))
      )					   1; with controller disabled*
    )					   1; let*
  )					   1; MEMORY-TEST*

(Defmethod 1(NUBUS-ENC :DIAGNOSE-CHIP)* (&Aux Cmd-Blk)
  2"Runs the DIAGNOSE command for the 586 and prints the result."*
  (Format t 2"~2% Executing 82586 LAN processor diagnostics . . ."*)
  (With-Controller-Disabled
    (Issue-Command-Block Net::Slot Cb t
      (Setf Cmd-Blk Cb)
      (Setf (Command-Word Net::Slot Cb) DIAGNOSE-586))
    (If (Not (Zerop (Command-Diagnose-Fail Net::Slot Cmd-Blk)))	
	(Format t 2"~%  82586 passed diagnostics."*)
	(Format t 2"~%  82586 failed diagnostics!"*))
    )					   1; with controller disabled*
  )					   1; DIAGNOSE-CHIP*

(Defmethod 1(NUBUS-ENC :SELFTEST)* ()
  2"Executes all diagnostics defined for selftest"*
  (Send Self :Send-if-handles :MEMORY-TEST)
  (Send Self :Send-if-handles :DIAGNOSE-CHIP)
  (Send Self :Send-if-handles :LOOP-BACK-TEST)
  ) 1; SELF TEST*
  

2;;*--------------------------------------------------------------------------------
2;;   REFLECTOMETER TEST:
;;*

(Defmethod 1(NUBUS-ENC :REFLECTOMETER-TEST)* (&Aux Cmd-Blk)
  2"Runs the TIME-DOMAIN-REFLECTOMETER command for the 586 and prints the result."*
  (Format t 2"~2% Executing Reflectometer Test for NuBus Controller Board in slot ~x . . ."* Net::Slot)
  (Without-Interrupts
    (Issue-Command-Block Net::Slot Cb t
      (Setf Cmd-Blk Cb)
      (Setf (Command-Word Net::Slot Cb) TDR-COMMAND))
    (Let ((Status (Command-Buffer-Ptr Net::Slot Cmd-Blk)))
      (If (Ldb-Test (Byte 1 15) Status)
	  (Format t 3"*~%  3No Link problems identified"*)
	  (Progn
	    (When (Ldb-Test (Byte 1 14) Status)
	      (Format t 3"*~%  3Bad cable to Transceiver (unless Xceiver doesn't return Carrier Sense during Xmit)"*))
	    (When (Ldb-Test (Byte 1 13) Status)
	      (Format t 3"*~%  3There is a short in the Net"* t))
	    (When (Ldb-Test (Byte 1 12) Status)
	      (Format t 3"*~%  3The Net is not terminated properly, i.e an Open"*))
	    (When (Ldb-Test (Byte 2 12) Status)
	      (Let ((Distance (Ldb (Byte 11 0) Status)))
		(Format t 3"~%*  3Time (distance) to problem = ~d transmit clock cycles.~@
                Distance = Time * V /(2*F); V=wave speed on link (M/s), F=clock freq (Hz)"*
			Distance)
		(Format t 3"~%*  3{Estimated distance is ~D - ~D meters}~&"* (* 5 Distance)
			(* 10 Distance))
		)			   1; let 2*
	      )				   1; when*
	    )				   1; progn*
	  )				   1; if*
      )					   1; let 1*
    )					   1; without-interrupts*
  )					   1; REFLECTOMETER-TEST*


 
1;;*-------------------------------------------------------------------------------------
1;;                *     1  *           1RELEASE 3 COMPATIBILITY
;;*-------------------------------------------------------------------------------------

(Defmethod 1(NUBUS-ENC :TRANSMIT-FAST)* (Type Dest Data-Array N-Bytes)
2  "Call :TRANSMIT with Deallocate set to nil."*
  (Send Self :Transmit Type Dest Data-Array N-Bytes Nil)
  ) 1; transmit-fast*



(Net::Define-Controller-Board "NEC" 'Ethernet::NUBUS-ENC)
(Compile-Flavor-Methods NUBUS-ENC)

;;End DEFINE-WHEN
)
