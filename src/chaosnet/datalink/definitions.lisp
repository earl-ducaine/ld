;; -*- Mode:Common-Lisp; Package:Net; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                                RESTRICTED RIGHTS LEGEND
;;;
;;;         Use, duplication, or disclosure by the Government is subject to
;;;         restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;         Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                            TEXAS INSTRUMENTS INCORPORATED.
;;;                                    P.O. BOX 2909
;;;                                 AUSTIN, TEXAS 78769
;;;                                       MS 2151

;;;         Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.


;;-------------------------------------------------------------------------------------------
;;                               NETWORK DATALINK DEFINITIONS
;;-------------------------------------------------------------------------------------------
 
;;  This file defines network definitions for the datalink layer of the network.  The datalink
;;  layer encompasses Ethernet, Starlan, Token-Ring, etc., and is manifested at the Lisp level
;;  mainly in the form of Controller Objects.  For each hardware item that we support at the
;;  physical layer, we instantiate a Network Controller Object to act as a driver -- this object
;;  accepts messages such as :TRANSMIT and :RECEIVE from the Network/Routing layers and
;;  translates them into device-specific commands required by the hardware.
;;
;;  Controller Objects are defined as flavors; each flavor includes mixins that define
;;  basic network controller variables and methods shared by similar controllers.  The
;;  fundamental mixin shared by all controllers is the DATALINK-CONTROLLER-MIXIN, which
;;  is defined in this file.  The DATALINK-CONTROLLER-MIXIN defines the following essential
;;  methods:
;;
;;       :DISABLE  --  Disables the receive process of the controller.
;;       :ENABLE   --  Enables the receive process of the controller.
;;       :RESET    --  Returns the controller hardware and software to an
;;                       initialized state.
;;
;;  The DATALINK-CONTROLLER-MIXIN creates a receive process to act on received packets
;;  in a different process from that of transmit and other operations; this allows
;;  a kind of parallel processing on the part of the controller for handling receives and
;;  transmits "simultaneously".
;;
;;  The DATALINK-CONTROLLER-MIXIN also defines several hardware-specific variables such as
;;  BOARD-TYPE and SLOT which are to be setup during instantiation.  The DATALINK-CONTROLLER-MIXIN
;;  also requires that the methods :INITIALIZE and :RECEIVE-TOP-LEVEL be provided by any
;;  flavor that it is mixed into.  :INITIALIZE is called by :RESET to initialize the hardware while
;;  :RECEIVE-TOP-LEVEL is called by the receive process to handle packets received on the link.
;;
;;  A set of functions for managing controller objects are included in this file:
;;
;;       DEFINE-CONTROLLER-BOARD  --  Associates a board's type to its controller flavor.
;;       SEND-ALL-CONTROLLERS     --  Sends a message to all controllers.
;;       CLEAR-CONTROLLER-LIST    --  Clears all controller objects from the controller list.
;;       RESET-CONTROLLER-LIST    --  Sends a :RESET message to all controllers.
;;       SELECT-CONTROLLER        --  Interactively selects a controller object when more than
;;                                             one is on the controller list.
;;
;;  Several metering and peek aids are included for checking network performance at the
;;  controller level.
;;
;;  Maintenance History:
;;
;;  16 SEP 87  MMG -- Rewrite for Explorer II, Nupi/E, and microExplorer compatibility.
;;  27 JAN 88  MMG -- Move Create-Controller-List to Initializations.Lisp for MX


;;------------------------------------------------------------------------------
;; Controller Definitions:

(Defparameter CONTROLLER-BOARD-TYPE-ALIST ()
  "Alist of network controller board types dotted with controller flavor names.")

(Defvar CONTROLLER-LIST ()  "List of All Network Datalink Controller objects") 

(Defun DEFINE-CONTROLLER-BOARD (Board-Type Controller-Flavor &Aux Item)
  "Adds (BOARD-TYPE . CONTROLLER-FLAVOR) to the Controller-Board-Type-Alist.
   BOARD-TYPE is a string such as 'NEC' or 'NPE' while CONTROLLER-FLAVOR is
   a symbol like 'Ethernet:NuBus-Enc or 'Ethernet:Nupie-Enc.  This is used only for
   Explorer boards"
                                           ; 23 JUL 87  MMG
   (If (Null
	 (Setf Item (Assoc Board-Type Controller-Board-Type-Alist :Test #'EQUALP)))
       (Push (Cons Board-Type Controller-Flavor) Controller-Board-Type-Alist)
       (Rplacd Item Controller-Flavor)))


;;------------------------------------------------------------------------------
;; Metering aids:

(Defvar PEEK-A-BOO-LIST      ()  "List of Meters to be displayed by PEEK") 
(Defvar CONTROLLER-METERS    ()  "List of Controller Meters to be displayed by PEEK")

;; This belongs in Network-Support:
(Defmacro DEFVAR-FOR-PEEK-A-BOO (Symbol Value &Optional (Doc "Some Network Variable"))
  `(Progn 'Compile
          (Defvar ,Symbol ,Value ,Doc)
          (Pushnew '((Symbol-Value',Symbol),Doc) Net:Peek-A-Boo-List :Test #'Equal)))


(Defun RESET-METERS ()
  "Resets all network and controller meters"
  (Dolist (Meter Peek-A-Boo-List)
    (Unless (Consp Meter) (Set Meter 0)))
  (Dolist (Cont Controller-List)
     (Send Cont :Send-If-Handles :Reset-Meters)))


;;------------------------------------------------------------------------------
;; Utility macros & functions:

(Defmacro SWAP-BYTES (Word)
  "Returns WORD with the LSB in the MSB position and the MSB in the LSB position."
  (Once-Only (Word)
    `(Dpb (Ldb (Byte 8 0) ,Word) (Byte 8 8) (Ldb (Byte 8 8) ,Word))))

(Defsubst ROUND-UP-TO-QUAD (Addr)
  "Round ADDR up to a multiple of 4."
  (Dpb 0 (Byte 2 0) (+ Addr 3))) 

(Defsubst ROUND-UP-TO-EVEN (addr)
  "Round ADDR up to an even number."
  (Dpb 0 (Byte 1 0) (+ Addr 1))) 

(Defsubst CONVERT-TO-WORDS (Nbytes)
  (Ash (+ 1 Nbytes) -1))

(Defmacro BLT-ARRAY-COPY (Source Dest Words)
  "Use %BLT to copy WORDS words from SOURCE to DEST arrays."
  `(Without-Interrupts
     (%Blt (%Make-Pointer-Offset DTP-FIX ,Source
				 (Si:Array-Data-Offset ,Source))
	   (%Make-Pointer-Offset DTP-FIX ,Dest
				 (Si:Array-Data-Offset ,Dest)) ,Words 1)))

(Defun ARRAY-COPY (Array Size)
  "Returns a copy of the the same type as ARRAY, containing the first SIZE elements of ARRAY."
  (let ((new-array (make-array size :type (array-type array))))
    (copy-array-portion array 0 size new-array 0 size)
    new-array))

(Defun NUMERICAL-VALUE (Parameter)                        
  "Translates NIL/T, No/Yes, Off/On, or Ext/Int to 0 or 1; used to force 
   user-defined values to to numerical values."
   (Cond ((Numberp Parameter) Parameter)
	 ((String-Equal Parameter "EXT") 0.)
	 ((String-Equal Parameter "INT") 1.)
	 ((String-Equal Parameter  "NO") 0.)
	 ((String-Equal Parameter "YES") 1.)
	 ((String-Equal Parameter "OFF") 0.)
	 ((String-Equal Parameter  "ON") 1.)
	 (Parameter 1.)
	 (t 0.)))
	  
(Defun ON-OFF (Value)
  "Converts 0 or 1 to ON or OFF"
  (if (Zerop Value) "OFF" "ON"))

;;-------------------------------------------------------------------------------------------
;;                               NETWORK CONTROLLER MANAGEMENT
;;-------------------------------------------------------------------------------------------

(Defmacro SEND-ALL-CONTROLLERS (Message &Rest Args)
  "Send MESSAGE & ARGS to every controller on CONTROLLER-LIST"
  `(Dolist (Cont Controller-List)
     (When Cont
       (Send Cont :Send-If-Handles ,Message ,@Args))))


;;  Note: since this is the Age of Temporal GC, old controller objects are
;;  no longer saved for reuse.

(Defun CLEAR-CONTROLLER-LIST (&Aux Process W-Process)
  "Clears the controllers from CONTROLLER-LIST." 

  (Dolist (Cont Controller-List)
    (When (Setf Process (Send Cont :Receiver-Process))
      (Send Process :Kill t) 		   ; Kill receiver process for each object.
      (Without-Interrupts
	(Setf Sys:All-Processes
	      (Delete Process Sys:All-Processes)))
                                           ; double-check, look through window system
      (Dolist (Window TV:*Full-Screen-Hacking-Windows*)      
	(When (And (Typep Window 'TV::Background-Lisp-Interactor)
		   (Setf W-Process (Send Window :Send-If-Handles :Process))
		   (Equal W-Process Process))
	  (Send Window :Send-If-Handles :KILL)))))

  (Setf Controller-Meters ())		   ; Remove controller meters from PEEK.
  (Setf Controller-List ())		   ; Clear controller objects from list.
  )					   ; clear controller list

(Defun RESET-CONTROLLER-LIST (&Optional (Enable-P T))
  "Resets hardware and software for all existing network controllers"
  (Send-All-Controllers :Reset Enable-P))


(Defun SELECT-CONTROLLER (&Optional (Clist Controller-List))
  "Pops up a menu to select one of the existing controllers.  Returns NIL if none."
  (If (< (Length Clist) 2)
      (First Clist)
      (W:Menu-Choose
	(Mapcar
	  #'(Lambda (x)
	      (List (Format () "~A-Controller [~16R]"
			    (Send x :Board-Type)
			    (Send x :Slot)) X)) Clist)
	:Label "Select Controller")))

(Defvar HALT () "T if Network Controllers are to be halted under *ALL* conditions.") 

(Defun HALT (Stop-P)
  "Halt all controller activity when STOP-P is T, Re-ENABLE when NIL."
  (Setf Halt Stop-P)
  (If Stop-P
      (Send-All-Controllers :Disable)   
      (Send-All-Controllers :Enable)))


;;-------------------------------------------------------------------------------------------
;;                            BASIC DATALINK CONTROLLER DEFINITIONS
;;-------------------------------------------------------------------------------------------

(Defflavor DATALINK-CONTROLLER-MIXIN 
	   (Board-Type                   ; Board Type of controller
	    (Slot 0)                     ; NuBus slot of this controller.
	    (Subnet 0)                   ; Subnet this controller is for.
	    (Receiver-Process ())        ; Process to receive on this link.
	    (Enabled ())                 ; T if controller is enabled.
	    (Pkts-Transmitted 0)         ; Total pkts xmitted on this link.
	    (Pkts-Received 0)            ; Total pkts received on this link.
	    (Pkts-Lost 0)                ; No. pkts that hardware couldn't get.
	    (Pkts-Too-Big-To-Receive 0)  ; No. pkts that were too big to receive.
	    (Jam-Count 0)                ; No. of controller commands that failed
	    (Transmit-Time-Outs 0)       ; No. of transmits that failed
	    (Fcs-Errors 0))              ; No. of pkts received with CRC errors.
	   ()
  :Abstract-Flavor
  :Gettable-Instance-Variables
  :Inittable-Instance-Variables
  (:Settable-Instance-Variables Subnet Enabled)
  (:Required-Methods :Initialize
		     :Receive-Top-Level)
  (:Documentation
    :Essential-Mixin "Basic Datalink Controller")) 

(Defun RECEIVE-PROCESS (Controller)
  "Top level function for CONTROLLER's receive process"
  (Send Controller :Receive-Top-Level))


(Defmethod (DATALINK-CONTROLLER-MIXIN  :DISABLE) ()
  "Stops the receiver process and disables this controller"

  ;; Always stop the receiver in a known state so that it doesn't
  ;; run undesirable unwind-protects when restarted.
  (Setf Enabled Nil)			   ; Go into disabled state
  (Process-Allow-Schedule)		   ; Let receiver process sync up
  (When Receiver-Process		   ; Then stop him while in disable-wait.
    (Process-Disable Receiver-Process)
    )					   ; when
  )					   ; :Disable

(Defmethod (DATALINK-CONTROLLER-MIXIN  :ENABLE) ()
  "Resets and restarts the receiver process and enables this controller."
  
  (Setf Enabled T)			   ; Go into enabled state
  (When Receiver-Process		   ; then turn on the receiver process
    (Process-Reset-And-Enable Receiver-Process)
    )					   ; when 
  T)					   ; :Enable
  
(Defmethod (DATALINK-CONTROLLER-MIXIN  :RESET) (&Optional (Enable-P T))
  "Resets & Enables this controller."
  
  (Send Self :Disable)			   ; Go into disabled state  
  (Send Self :Initialize)		   ; initialize the controller
  (Send Self :Reset-Meters)                ; reset meters
  (Unless Receiver-Process
    (Setf Receiver-Process		   ; Instantiate process object if null
	  (Make-Process                    
	    (Format nil "~a Receiver, #x~16r" Board-Type Slot)
	    :Warm-Boot-Action Nil
	    :Priority 25
	    :Regular-Pdl-Size 10000
	    :Special-Pdl-Size 10000)
	  )				   ; setf
    )					   ; unless 
  (Send Receiver-Process :Preset           ; Preset receive process
	                 'Net::Receive-Process Self)
  (When Enable-P
    (Send Self :Enable))		   ; Go into enabled state
  )					   ; :Reset

;;------------------------------------------------------------------------------
;;  DISPLAYS:

(Defmethod (DATALINK-CONTROLLER-MIXIN  :PRINT-SELF) (&rest args)
  (Si:Printing-Random-Object (Self (Car Args))
    (Format (Car Args)
	    "~a-Controller [~16R]" Board-Type Slot)))

(Defmethod (DATALINK-CONTROLLER-MIXIN  :PRINT-STATS)
	   (&Optional (Stream *Standard-Output*))
  "Print the statistics for this controller to STREAM."
  (Send Self :Send-If-Handles :Update-Stats)
  (Format stream "~3% Statistics for network controller in slot #x~16R:" Slot)
  (Format stream "~%~10D  Packets Received"  Pkts-Received) 
  (Format stream "~%~10D  Packets Transmitted"  Pkts-Transmitted)
  (Format stream "~%~10D  Packets Missed by Hardware"  Pkts-Lost) 
  (Format stream "~%~10D  Packets Too Big to Receive"  Pkts-Too-Big-To-Receive)
  (Format stream "~%~10D  Packets Received with CRC Errors"   Fcs-Errors))


(Defmethod (DATALINK-CONTROLLER-MIXIN  :ADD-CONTROLLER-METER) (Message Desc)
  "Adds controller meter accessible by MSG (which is sent to the controller
   object) to the Controller-Meters.  DESC is a description of the meter to
   be printed by Peek."
  (Let ((Item `((Send ,Self ,Message)
		,(Format () "~A ~16R ~A" Board-Type Slot Desc))))
    (Unless (Member Item Controller-Meters :Test #'EQUALP)
      (Push-End Item Controller-Meters))))


(Defmethod (DATALINK-CONTROLLER-MIXIN  :RESET-METERS) ()
  (Setf Pkts-Received 0
	Pkts-Transmitted 0
	Pkts-Too-Big-To-Receive 0
	Jam-Count 0
	Transmit-Time-Outs 0
	Fcs-Errors 0
	Pkts-Lost 0)) 


(Defmethod (DATALINK-CONTROLLER-MIXIN  :AFTER :INIT) (&Rest Ignore)
  "Adds controller meters to peek after instantiation"
  (Send Self :Add-Controller-Meter
	:Pkts-Received "Total Number of packets received")
  (Send Self :Add-Controller-Meter
	:Pkts-Transmitted "Total Number of packets transmitted")
  (Send Self :Add-Controller-Meter
	:Pkts-Too-Big-To-Receive "Total Number of packets too big to receive")
  (Send Self :Add-Controller-Meter
	:Fcs-Errors "Total Number of packets received with CRC errors")
  (Send Self :Add-Controller-Meter
	:Pkts-Lost "Total Number of packets lost by hardware")
  )					   ; AFTER INIT
