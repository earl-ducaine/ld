;; -*- Mode:Common-Lisp; Package:IP; Base:10 -*-

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
;;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.

;;; 9/30/86 - rla: this does not need to be in the chaos package, check that menu function is there

(WHEN (FBOUNDP 'net:add-to-peek-menu)
  (NET::ADD-TO-PEEK-MENU "IP" IP::IP-PEEK "Display the IP statistics") 

  (NET::ADD-TO-PEEK-MENU "UDP" IP::UDP-PEEK "Display the UDP statistics") 

  (NET::ADD-TO-PEEK-MENU "TCP" IP::TCP-PEEK "Display the TCP statistics") 

  (NET::ADD-TO-PEEK-MENU "ICMP" IP::ICMP-PEEK "DIsplay the ICMP statistics") )


