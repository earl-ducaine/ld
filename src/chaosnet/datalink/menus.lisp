;;      -*- Mode:Common-Lisp; Package:Net; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                             RESTRICTED RIGHTS LEGEND
;;;
;;;     Use, duplication, or disclosure by the Government is subject to
;;;     restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;     Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                         TEXAS INSTRUMENTS INCORPORATED.
;;;                                  P.O. BOX 2909
;;;                               AUSTIN, TEXAS 78769
;;;                                     MS 2151

;;;     Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.



;;-------------------------------------------------------------------------------- 
;;                         NETWORK CONTROLLER MENU INTERFACE
;;-------------------------------------------------------------------------------- 

;;-------------------------------------------------------------------------------- 
;;  Control Menu Items:


(Add-To-Menu Control-Menu "Clear Address Translations "
   `(:Funcall Ethernet:Reset-Address-Translations
	      :Documentation "Clear all entries from the address translation tables")) 

(Add-To-Menu Control-Menu "Reset All Network Meters "
   `(:Funcall Reset-Meters :Documentation "Set all network meters to zero")) 

(Add-To-Menu Control-Menu "Reset One Network Controller "
   `(:Eval
      (Let ((Cont (Select-Controller))) (When Cont (Send Cont :Reset)))
     :Documentation
     "Reset one network controller (select from a menu if more than one controller)")) 

(Add-To-Menu Control-Menu "Reset All Network Controllers "
   `(:Funcall Reset-Controller-List
	      :Documentation ,(Documentation #'Reset-Controller-List))) 

(Add-To-Menu Control-Menu "Create Network Controllers"
   `(:Funcall Create-Controller-List
	      :Documentation ,(Documentation #'Create-Controller-List))) 


;;--------------------------------------------------------------------------------
;;  Status Menu Items:


(Add-To-Menu Status-Menu "Print Address Translations "
   `(:Funcall Ethernet:Print-Address-Translations
	      :Documentation
	      ,(Documentation 'Ethernet:Print-Address-Translations))) 
 
(Add-To-Menu Status-Menu "Reset Controller Statistics "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Reset-Meters)))
	   :Documentation "Resets controller meters"))

(Add-To-Menu Status-Menu "Print Controller Statistics "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Print-Stats)))
	   :Documentation "Prints controller meters"))

(Add-to-Menu Status-Menu "Print Controller Status "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Print-Status)))
	   :Documentation "Prints status of controller"))


;;--------------------------------------------------------------------------------
;;  Diagnostic Menu Items:


(Add-To-Menu Diagnostic-Menu "Dump Controller Memory "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Memory-Dump)))
      :Documentation "Dumps the contents of the controller memory to the screen.")) 

(Add-To-Menu Diagnostic-Menu "Execute Controller Selftests "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Selftest)))
      :Documentation "Runs all controller selftests"))

(Add-To-Menu Diagnostic-Menu "Execute Reflectometer Test "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Reflectometer-Test)))
      :Documentation "Runs a Time Domain Reflectometer test on this controller's link.")) 

(Add-To-Menu Diagnostic-Menu "Monitor Controller Link "
   `(:Eval
      (Let ((Cont (Select-Controller)))
	(When Cont (Send Cont :Send-If-Handles :Monitor-Link)))
      :Documentation
      "Prints frames as received on this controller's link (disables normal reception).")) 

