;; -*- Mode:Common-Lisp; Package:Net; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                             RESTRICTED RIGHTS LEGEND
;;;
;;;        Use, duplication, or disclosure by the Government is subject to
;;;        restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;        Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                          TEXAS INSTRUMENTS INCORPORATED.
;;;                                  P.O. BOX 2909
;;;                              AUSTIN, TEXAS 78769
;;;                                     MS 2151

;;;    Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.


;;
;;  Maintenance History:
;;
;;  
;;  22 SEP 87  MMG -- Rewrite for Rel 3.2.
;;  27 JAN 88  MMG -- Added Create-Mac-Controllers for microExplorer (MX).
;;  04 FEB 88  MMG -- Move Controller Enable to Warm initialization list.
;;  02/27/89   JLM -- [may] remove ethernet from %slots-i-own if being snatched by re-booting processor*
;;		      see also : reboot-decide-ethernet-ownership*.
;;  03/07/89   JLM -- Changed (MP:~) to (funcall mp-~) and added var for funcall

;;-------------------------------------------------------------------------------------
;;                               CONTROLLER INSTANTIATION
;;-------------------------------------------------------------------------------------


(si:DEFINE-UNLESS :ENET                         ; only for MX systems

(defvar *network-disabled* nil "This variable is only set if the user proceeds out of an error when creating Mac controllers.")

(add-initialization "Clear error variable" '(setf *network-disabled* nil) :before-cold)

(Defun CREATE-MAC-CONTROLLERS (&rest ignore &aux index)
  "Scans the Macintosh NuBus for Ethernet cards and creates a controller for each"

  (condition-resume '((error) :resume resume-create-mac-controllers-p ("Disable the network") resume-create-mac-controllers)
    (catch 'create-mac-controllers-restart
      (Unless (or Net:Controller-List *network-disabled*)		       ; don't recreate controllers
	(let (Acb)
	  (unwind-protect
	      (progn
		(setf acb (Add:Get-Acb Ethernet::STATUS-SIZE))
		(Add:Init-Acb Acb Ethernet::ESLOTS Ethernet::ESLOTS)
		(Add:transmit-packet-And-Wait Acb Ethernet::LAPCHANNEL)
		
		(Setf Index 0)
		(Do ((Slot (Add:Read-Parm Acb (+ Ethernet::LH-DATA Index)   16)
			   (Add:Read-Parm Acb (+ Ethernet::LH-DATA Index)   16))
		     (Spid (Add:Read-Parm Acb (+ Ethernet::LH-DATA Index 1) 16)
			   (Add:Read-Parm Acb (+ Ethernet::LH-DATA Index 1) 16)))
		    ((Eql Slot Ethernet::END-SLOTS))
		  (Push-End (Make-Instance 'Ethernet::Mac-Enc
					   :Subnet 0
					   :Slot   Slot
					   :Spid   Spid)  Net::Controller-List)
		  (Setf Index (+ 2 Index))))
	    (Add:Return-Acb Acb t)))))))

(defun resume-create-mac-controllers-p (cond)
  (declare (ignore cond))
  t)

(defun resume-create-mac-controllers (cond)
  (declare (ignore cond))
  (setf net:controller-list nil)
  (setf *network-disabled* t)
  (throw 'create-mac-controllers-restart t))

;;End of DEFINE-UNLESS
)


(PROCLAIM '(ftype (function (&rest t) t) Net::Create-Mac-Controllers))

(defvar mp-cool-boot-decide-ethernet-ownership)		; jlm 3/08/89

(Defun CREATE-CONTROLLER-LIST (&Aux Flavor Cfr Cont)
  "Creates a new controller object for each valid network controller board on the NuBus."
  
  (If (Si:MX-P)				   ; If MX chassis,    ;; 27 JAN 88  MMG
      (Create-Mac-Controllers)		   ; create Macintosh controllers
      (Progn				   ; else, create Explorer controllers
	(Clear-Controller-List)            ; kill off old controllers
	(if (si:mp-system-p)
	    (funcall mp-cool-boot-decide-ethernet-ownership));  jlm 3/08/89
	(Do ((Slot #xF0 (+ 1 Slot)))	   ; For each slot on the NuBus,
	    ((> Slot #xFF))
	  (When
	    (And (Si:Slot-Owned-P  Slot)   ; Do we own this slot?
		 (Si:Valid-Crom-P  Slot)   ; Does it have a valid config ROM? (board there?)
		 (Setf Cfr
		       (Si:Cfg-Register Slot))	   ; Can we get to the Config Register?
		 (Not (Logbitp #x02 Cfr))  ; If so, is the Fault Bit off?
		 (Setf Flavor		   ; Controller flavor defined for this board?
		       (Cdr                         
			 (Assoc (Si:Board-Type Slot) Controller-Board-Type-Alist
				:Test #'EQUALP))))
	    (Setf Cont
		  (Make-Instance Flavor
				 :Slot Slot
				 :Subnet 0))	   ; Instantiate controller
	    (Push-End	Cont Controller-List)	   ; and push onto list.
	    )				   ; when
	  )				   ; do
	)				   ; progn
      )					   ; if
  )					   ; create controller list


;;-------------------------------------------------------------------------------------
;;                             ETHERNET RESET & INITIALIZATION
;;-------------------------------------------------------------------------------------
 

(Defun RESET-ETHERNET (&optional (Enable-P T) (Create-Controllers-P NIL))
  "Resets the Ethernet Layer; if ENABLE-P is T, all controllers are enabled.
   When Create-Controllers-P is T, new controller objects are instantiated and reset.
   Note that this must be followed by a NET:RESET if new objects are created."
  (When (Or (Null Controller-List)
	    Create-Controllers-P)
    (When (Si:Find-System-Named "Chaosnet" T T)
      (Funcall 'Chaos:Create-Chaosnet-Buffers 50.)) ; We need to get Chaosnet out of here!
    (Create-Controller-List))
  (Reset-Controller-List Enable-P)
  (Reset-Meters))


;;-------------------------------------------------------------------------------------
;;                                 INITIALIZATIONS LISTS
;;-------------------------------------------------------------------------------------

;; Disable SI:INITIALIZE-NUBUS-SLOTS function (this function needs to
;; be removed from the LISP-REINITIALIZE file):

(Setf Si:*NuBus-Board-Alist* ())


(Add-Initialization "Reset controllers"
		    '(net:reset-ethernet net:*net-reset-enable-p*)
		    nil 'net:*reset-initialization-list*)

(Add-Initialization "Reset controllers"
		    '(net:reset-ethernet net:*net-reset-enable-p*)
		    nil 'net:*network-reset-initialization-list*)

;; This initialization list is called before disk-save:

(Add-Initialization "Remove Network Controllers"
		    '(net:clear-controller-list)
		    :Head-Of-List 'net:*network-before-cold-initialization-list*)

;; This initialization list is called during boot:

(Add-Initialization "Create Network Controllers"
		    '(net:reset-ethernet nil t)  ; Create controllers, but leave in stop state
		    :Head-of-List 'net:*network-system-initialization-list*)

(Add-Initialization "Enable Network Controllers"
		    '(net:reset-ethernet t)      ; Start controllers from warm list
		    :Head-of-List 'net:*network-warm-initialization-list*)
