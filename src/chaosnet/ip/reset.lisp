;;; -*- Mode:Common-Lisp; Package:IP; Base:10 -*-

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

(DEFSUBST SAFE-CALL (FNAME &REST ARGS) (WHEN (FBOUNDP FNAME) (APPLY FNAME ARGS)))

(DEFUN RESET (&OPTIONAL (enable-p nil) (debug-p nil) (local-only t))
  "Resets the IP/ICMP, UDP, TFTP, TCP, and FTP services.
    The optional debug argument should be nil for no debug, t for some debug, :verbose for all debug code.
    If local-only arg is T, nameserver is not contacted for gateway information; IP utilizes data that has
    been cached."
  (COND ((NOT enable-p)
	 (SAFE-CALL 'RESET-TFTP-SERVICE)
	 (SAFE-CALL 'RESET-UDP-SERVICE)
	 (SAFE-CALL 'FS::RESET-FTP-SERVICE)
	 (SAFE-CALL 'RESET-TCP-SERVICE)
	 (SAFE-CALL 'RESET-IP-SERVICE)
	 (SAFE-CALL 'RESET-ICMP-SERVICE))
	(net:controller-list
	 (setup-my-address)
	 (WHEN (AND (BOUNDP 'ip:my-addresses)
		    ip:my-addresses)
	   (disable)
	   (SAFE-CALL 'RESET-ICMP-SERVICE enable-p)
	   (SAFE-CALL 'RESET-IP-SERVICE enable-p debug-p local-only)
	   (SAFE-CALL 'RESET-UDP-SERVICE enable-p)
	   (SAFE-CALL 'RESET-TFTP-SERVICE enable-p debug-p)
	   (SAFE-CALL 'RESET-TCP-SERVICE enable-p)
	   (SAFE-CALL 'FS::RESET-FTP-SERVICE enable-p))))
  "IP reset complete")

(ADD-INITIALIZATION "Halt IP" '(IP:RESET nil) nil 'HOST:*NETWORK-BEFORE-COLD-INITIALIZATION-LIST*)  

(ADD-INITIALIZATION "Reset IP" '(IP:RESET net:*net-reset-enable-p* nil) NIL 'NET:*RESET-INITIALIZATION-LIST*)

(ADD-INITIALIZATION "Enable IP" '(ip:reset t nil nil) nil 'net:*network-warm-initialization-list*)
