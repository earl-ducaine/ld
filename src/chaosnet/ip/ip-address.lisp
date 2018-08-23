;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(CPTFONT HL12B TR12BI) -*-

;1;;                    RESTRICTED RIGHTS LEGEND*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986, 1988 Texas Instruments Incorporated.  All rights reserved.*


(DEFUN parse-ip-host-spec (host-spec &optional dont-create)
  "2Takes a host-spec (host-object, name, IP address, or nil) and returns the corresponding 
   host-object (or nil);  An unnamed-host will be created for an unknown address unless dont-create is true.
   An error will be signalled for a non-existant host name or a syntactically-invalid IP address.*"
  (COND
    ((NULL host-spec) nil)
    ((TYPEP host-spec 'net:host) host-spec)
    ((SYMBOLP host-spec) (si:parse-host host-spec))
    ;1; legal host name (even if it looks like a number) - continue if not*
    ((AND (STRINGP host-spec) (si:parse-host host-spec :no-error)))
    ((OR (NUMBERP host-spec)
	 ;1; or convertible to a number *
	 (AND (STRINGP host-spec)
	      (MULTIPLE-VALUE-BIND (host invalid-p) (net:ip-address-parser host-spec nil)
	         (UNLESS invalid-p (SETQ host-spec host)))))
     (COND
       ((si:get-host-from-address host-spec :ip))
       (dont-create nil)
       (t (create-ip-host () host-spec))))
    (t (FERROR 'ip-error "Unrecognized host spec ~A" host-spec)))) 


(DEFUN create-ip-host (name address-spec &aux address (host nil))
  "2Create a TEMPORARY host whose name can be used for TCP/IP access, but NOT in pathnames.  
   If name is nil, an unnamed host will be returned.*"
  (COND
    ((SETQ address (net:ip-address-parser address-spec nil))
     (COND
       (name
	(WHEN (SETQ host (si:parse-host name t))
	  (UNLESS (MEMBER address (SEND host :send-if-handles :ip-addresses) :test #'EQL)
	    (FERROR 'ip-error "Existing host ~A  has same name but different address" host))))
       ((COND
	  ((SETQ host (si:get-host-from-address address :ip)))
	  (t (SETQ name (STRING-APPEND "UNKNOWN-IP-" (FORMAT () "~D" address)))))))
     (COND
       (host)
       (t
	(si:define-host name :host-names (LIST name) :system-type :unknown :machine-type
			:unknown :ip (LIST address))
	(SETQ host (si:parse-host name))	;1this is ok even though si:parse-host may call this routine*
	(SEND host :set-server-type ())
	host)))
    (t (FERROR 'ip-error "Illegal IP address ~A" address-spec)))) 


(DEFUN ip-address-display (address &aux b1 b2 b3 b4 (d1 0) (d2 0))
  "2Returns five values.  The values are (respectively) the string representations of: dotted decimal format, 
network:address format, comma octal format, decimal equivalent, hexadecimal equivalent, and octal equivalent.*"
  (SETQ b1 (LDB (BYTE 8 24) address))
  (SETQ b2 (LDB (BYTE 8 16) address))
  (SETQ b3 (LDB (BYTE 8 8) address))
  (SETQ b4 (LDB (BYTE 8 0) address))
  (COND
   ;1; Leading bits are 11, 3 byte : 1 byte*
   ((> b1 191) (SETQ d1 (DPB b1 (BYTE 8 16) (DPB b2 (BYTE 8 8) b3))) (SETQ d2 b4))
   ;1; Leading bits are 10, 2 byte: 2 byte*
   ((> b1 127) (SETQ d1 (DPB b1 (BYTE 8 8) b2)) (SETQ d2 (DPB b3 (BYTE 8 8) b4)))
   ;1; Leading bit is 0, 1 byte : 3 byte*
   (t (SETQ d1 b1) (SETQ d2 (DPB b2 (BYTE 8 16) (DPB b3 (BYTE 8 8) b4)))))
  (VALUES
    (FORMAT () "~D.~D.~D.~D" b1 b2 b3 b4)
    (FORMAT () "~D:~D" d1 d2)
    (FORMAT () "~O,~O,~O,~O" b1 b2 b3 b4)
    (FORMAT () "#d~D" address)
    (FORMAT () "#x~X" address)
    (FORMAT () "#o~O" address)))


