;;;               -*- Mode:Common-Lisp; Package:ETHERNET; Base:8 -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;			TEXAS INSTRUMENTS INCORPORATED.
;;;				 P.O. BOX 2909
;;;			      AUSTIN, TEXAS 78769
;;;				    MS 2151
;;;
;;; Copyright (C) 1984, 1988 Texas Instruments Incorporated. All rights reserved.

(NET::ADD-TO-MENU NET::STATUS-MENU "Print Address Translations"
   `(:EVAL
     (PROGN
       (PRINT-CHAOS-ETHER-ADDRESS-TRANSLATIONS)
       (PRINT-IP-ETHER-ADDRESS-TRANSLATIONS))
     :DOCUMENTATION ,(DOCUMENTATION #'PRINT-ADDRESS-TRANSLATIONS))) 


(NET::ADD-TO-MENU NET::STATUS-MENU "Print Chaos <-> Ether Address Translations"
   `(:FUNCALL PRINT-CHAOS-ETHER-ADDRESS-TRANSLATIONS :DOCUMENTATION
     ,(DOCUMENTATION #'PRINT-ADDRESS-TRANSLATIONS))) 


(NET::ADD-TO-MENU NET::STATUS-MENU "Print IP <-> Ether Address Translations"
   `(:FUNCALL PRINT-IP-ETHER-ADDRESS-TRANSLATIONS :DOCUMENTATION
     ,(DOCUMENTATION #'PRINT-ADDRESS-TRANSLATIONS))) 

