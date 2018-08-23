;;   -*- Mode:COMMON-LISP; Package:NET; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*-



;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.

;;;
;;    08 nov 86 BJ  - Moved here from menus to separate network components.
;;    04 APR 86 MMG - Moved here from various network files.
;;
;;------------------------------------------------------------------------------
;;                         MENU CONTROL DEFINITIONS
;;------------------------------------------------------------------------------
 

(add-to-menu control-menu "Reset"
   '(:eval (chaos:reset t) :documentation "Reset & Enable the Network")) 

(add-to-menu control-menu "Enable"
   '(:funcall chaos:enable :documentation "Enable the Network")) 

(add-to-menu control-menu "Disable"
   '(:funcall chaos:disable :documentation "Disable the Network")) 

(add-to-menu control-menu "Reset Routing Table"
   `(:funcall chaos:reset-routing-table
	      :documentation ,(documentation #'chaos:reset-routing-table))) 

(add-to-menu status-menu "Print Routing Table"
   '(:funcall chaos:print-routing-table :documentation "Show my routing table")) 

(add-to-menu diagnostic-menu "Show Routing Table"
   `(:eval
     (chaos:show-routing-table
      (fquery '(:type :readline :choices (:any)) "Show routing table of Host: "))
     :documentation ,(documentation #'chaos:show-routing-table))) 

(add-to-menu diagnostic-menu "Show Routing Path"
   '(:eval
     (chaos:show-routing-path :from
			(fquery `(:type :readline :choices ((,si:local-host "LM") :any))
				"Path from Host: ")
			:to (fquery '(:type :readline :choices (:any)) " to Host: "))
     :documentation "Show the most likely path between two hosts.")) 

(add-to-menu status-menu "Print Chaosnet State"
   `(:funcall chaos:display-chaos-state :documentation ,(documentation #'chaos:display-chaos-state))) 

(add-to-menu status-menu "Print Recent Headers"
   '(:eval (chaos:print-recent-headers 50) :documentation "Print headers from last 50. Chaos packets.")) 

(add-to-menu status-menu "Print Chaosnet STS Why"
   '(:funcall chaos:print-sts-why :documentation
     "Print the reasons for sending the last 64. STS packets."))
