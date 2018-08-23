;;;               -*- Mode:Common-Lisp; Package:IP; Base:10 -*-

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
 

(DEFVAR MY-ADDRESSES () "All IP addresses for this machine.") 

(DEFSUBST HOST-ID (ADDRESS)
  (COND
    ((NOT (INTEGERP ADDRESS)) NIL)
    ((= (LDB (BYTE 1 31) ADDRESS) 0) (LDB (BYTE 24 0) ADDRESS))
    ((= (LDB (BYTE 2 30) ADDRESS) 2) (LDB (BYTE 16 0) ADDRESS))
    ((= (LDB (BYTE 3 29) ADDRESS) 6) (LDB (BYTE 8 0) ADDRESS))
    (T NIL))) 

(DEFUN SETUP-MY-ADDRESS ()
  (let ((addr (send si:local-host :network-address-list :ip)))
    (if (not (equal (length net:controller-list) (length addr)))
	(setq ip:my-addresses (remove-if #'numberp addr :start (length net:controller-list)))
	(setq ip:my-addresses addr))))
 
	  
