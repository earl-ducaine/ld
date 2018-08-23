;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT HL12B HL12bi); Base:10 -*-

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

(DEFUN load-access-table (&aux addr badp ip-access-list)
  "2Load the ip-access-table from the site option :ip-access-list.*"
  (DECLARE (SPECIAL ip-access-table))
  (SETF ip-access-table
	(WHEN (SETF ip-access-list (GET-SITE-OPTION :ip-access-list))
	  (MAKE-HASH-TABLE :compare-function #'EQL :size 123)))
  (DOLIST (item ip-access-list)
    (SETF item (EVAL item))
    (COND ((NUMBERP item)
	   (SETF item (net:ip-address-parser item)))
	  ((SYMBOLP item)
	   (SETF item (get-ip-addresses (si:parse-host item))))
	  ((STRINGP  item)
	   (MULTIPLE-VALUE-SETQ (addr badp) (net:ip-address-parser item nil))
	   (SETF item (IF badp
			  (get-ip-addresses (si:parse-host item))
			  addr)))
	  (t (FERROR nil "~a is not a symbol, string, or number" item)))
    (UNLESS (LISTP item) (SETF item (LIST item)))
    (DOLIST (addr item) (SETF (GETHASH addr ip-access-table) t))))


(DEFUN access-permitted (address)
  "2Return T if the passed address can access this machine.
Access can be declared on an individual network, an individual host, or by both.*"
  (DECLARE (SPECIAL ip-access-table))
  (OR (NOT ip-access-table) ;1; anyone can access this machine*
      (GETHASH address ip-access-table) ;1; this host can *
      (GETHASH (get-network-number address) ip-access-table))) 	  ;1; hosts in this network*


(ADD-INITIALIZATION "LOAD ACCESS TABLE" '(ip:load-access-table) :now 'net:*network-warm-initialization-list*) 
                 
