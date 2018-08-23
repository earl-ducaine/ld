;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT MEDFNB TR12BI); Base:10 -*-

;1;;*			1      RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;*			1TEXAS INSTRUMENTS INCORPORATED.*
;1;;*				1 P.O. BOX 2909*
;1;;*			1      AUSTIN, TEXAS 78769*
;1;;*				1    MS 2151*
;1;;*
;1;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.*

;1;; This file should contain all hosts that have IP addresses that do *
;1;;    not respond to CHAOS style ARP packets (which is everybody but*
;1;;    those with lisp servers).*

;1;; Format is as follows:*
;1;;   This is setting a variable IP:OTHER-MACHINES.  It is a list.*
;1;;   Each entry to this list is as follows:*
;1;;     IP-ADD - BIGNUM - The IP address of the host.*
;1;;     NAME   - String - The list of name's for the host.*
;1;;     ETHER  - BIGNUM - The Ethernet address of the host.*

;1;; Reset the variable.*

;1;; 9/19/86 - RLA - took out hard-wired list for TI-Austin and added a function*
;1;;                 for adding a machine to the list.*

(DEFVAR *PRESET-OTHER-MACHINES* NIL
  "2List of machines and their addresses which cannot be reached via ARP*")

(DEFUN ADD-NON-ARP-MACHINE (NAME-S IP-ADDRESS ETHERNET-ADDRESS)
  "2Add a machine known by NAME-S (string or list of strings) having IP-ADDRESS and ETHERNET-ADDRESS
    to the list of machines which do not respond to CHAOS-style ARP packets.*"
  (LET ((NEW-ENTRY (LIST IP-ADDRESS (IF (STRINGP NAME-S) (LIST NAME-S) NAME-S) ETHERNET-ADDRESS)))
     (PUSH NEW-ENTRY *PRESET-OTHER-MACHINES*)
     (PUSH NEW-ENTRY OTHER-MACHINES)))

(DEFUN RESET-OTHER-MACHINES (&OPTIONAL HARD-CLEAR)
  (SETF OTHER-MACHINES
	(UNLESS HARD-CLEAR *PRESET-OTHER-MACHINES*)))

