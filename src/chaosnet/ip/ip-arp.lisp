;;   -*- Mode:COMMON-LISP; Package:ETHERNET; Base:10; Fonts:(CPTFONT HL12B CPTFONT) -*-

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
;1;; Copyright (C) 1988 Texas Instruments Incorporated. All rights reserved.*
 
;1;; RLA - allow for separate compilation*
(PROCLAIM '(SPECIAL ADDRESS-TRANSLATIONS-LISTS))
(PROCLAIM '(SPECIAL GET-ETHERNET-ADDRESS-HANDLERS))
(PROCLAIM '(SPECIAL RECEIVE-ADDR-PKT-HANDLERS))
(PROCLAIM '(SPECIAL ALLOCATE-BUFFER))
(PROCLAIM '(SPECIAL PRINT-ADDRESS-TRANSLATIONS-HANDLERS))

(DEFPARAMETER *AR-IP-PROTOCOL*             8)
(DEFPARAMETER *IP-ADDR-LENGTH*             4)
(DEFVAR *IP-ARP-REQUESTS-RECEIVED*          0 "2Number of IP ARP Request Broadcasts Received*") 
(DEFVAR *IP-ARP-REPLIES-SENT*               0 "2Number of IP ARP Replies Sent*") 
(DEFVAR *IP-ARP-REQUESTS-SENT*              0 "2Number of IP ARP Request Broadcasts Sent*") 
(DEFVAR *IP-ARP-REPLIES-RECEIVED*           0 "2Number of IP ARP Replies Received*")

(DEFSTRUCT (IP-ETHER-RES-FRAME
	     (:INCLUDE                   RES-FRAME)
	     (:CONSTRUCTOR               NIL)
	     (:CONC-NAME                 AR-IP-)
	     (:CALLABLE-CONSTRUCTORS     NIL)
	     (:ALTERANT                  ALTER-IP-ETHER-RES-FRAME)
	     (:PREDICATE                 NIL)
	     (:COPIER                    NIL)
	     (:TYPE                      :ARRAY))  
  SENDER-THIRD                                        ;1Sender's ethernet address*
  SENDER-SECOND                                       ;1 note reversed order!*
  SENDER-FIRST
  SENDER-IP-SECOND                                    ;1Sender's IP address*
  SENDER-IP-FIRST
  RECVER-THIRD                                        ;1Recver's ethernet address*
  RECVER-SECOND
  RECVER-FIRST
  RECVER-IP-SECOND
  RECVER-IP-FIRST)

(DEFMACRO AR-IP-SENDER-ETHER (ARRAY)
  `(GET-ETHER-ADDRESS-FROM-ARRAY ,ARRAY 4)) 

(DEFMACRO AR-IP-RECVER-ETHER (ARRAY)
  `(GET-ETHER-ADDRESS-FROM-ARRAY ,ARRAY 9)) 

(DEFMACRO AR-IP-SENDER-IP (ARRAY)
  `(GET-IP-ADDRESS-FROM-ARRAY ,ARRAY 7)) 

(DEFMACRO AR-IP-RECVER-IP (ARRAY)
  `(GET-IP-ADDRESS-FROM-ARRAY ,ARRAY 12))


;1;--------------------------------------------------------------------------------------------------*
;1;                                   RESOLVED ADDRESS CACHE*
;1;--------------------------------------------------------------------------------------------------*

(DEFSTRUCT (IP-ETHER-CACHE-ELEMENT
	     (:CONC-NAME AR-IP-ETHER-CACHE-)
	     (:CALLABLE-CONSTRUCTORS NIL)
	     (:ALTERANT ALTER-IP-ETHER-CACHE-ELEMENT)
	     (:PREDICATE NIL)
	     (:COPIER NIL)
	     (:TYPE :LIST))
  IP-ADDR
  ETHER-ADDR
  (AGE *INITIAL-CACHE-AGE*))

(DEFVAR IP-ETHER-ADDRESS-TRANSLATIONS    ())       ;1a list of IP-ETHER-CACHE-ELEMENTs*

(DEFUN GET-IP-ETHERNET-ADDRESS (IP-ADDRESS)
  (COND
    ((IP:LOCAL-BROADCAST-ADDRESS-P IP-ADDRESS)
     *ETHERNET-BROADCAST-ADDRESS*)
    ((MEMBER IP-ADDRESS
	     (SEND SI:LOCAL-HOST :NETWORK-ADDRESS-LIST :IP) :TEST #'EQL)
     (SEND (MAP-ADDRESS-TO-CONTROLLER :IP IP-ADDRESS) :ETHERNET-ADDRESS))
    (T
     (LET ((ADDRESS-INFO (ASSOC IP-ADDRESS IP-ETHER-ADDRESS-TRANSLATIONS :TEST #'EQUAL)))
       (IF (NOT (NULL ADDRESS-INFO))
	   (AR-IP-ETHER-CACHE-ETHER-ADDR ADDRESS-INFO)
	   (SEND-IP-ADDR-PKT IP-ADDRESS
			     (IP:CLOSEST-LOCAL-ADDRESS IP-ADDRESS))
	   ())					;1IF*
       ))))
(pushnew '(:ip IP-ETHER-ADDRESS-TRANSLATIONS) address-translations-lists :test #'equal)

(pushnew '(:ip GET-IP-ETHERNET-ADDRESS) get-ethernet-address-handlers :test #'equal)

(pushnew (list *AR-IP-PROTOCOL* 'RECEIVE-IP-ADDR-PKT) receive-addr-pkt-handlers :test #'equal)

(DEFUN RECEIVE-IP-ADDR-PKT (SELF ARRAY ETHER-PKT-TYPE)
  (declare (special *proxy-arp-switch* ip-routing-table *gateway-host-p*))
  (SELECT (AR-OPCODE ARRAY)
    (*AR-REQUEST* (INCF *IP-ARP-REQUESTS-RECEIVED*))
    (*AR-REPLY* (INCF *IP-ARP-REPLIES-RECEIVED*))
    (:OTHERWISE (INCF *UNKNOWN-ARP-PACKET-TYPES-RECEIVED*)))
  (WITHOUT-INTERRUPTS
    (LET ((ADDRESS-INFO
	    (ASSOC (AR-IP-SENDER-IP ARRAY) IP-ETHER-ADDRESS-TRANSLATIONS :TEST #'EQUAL)))
      (IF (NULL ADDRESS-INFO)
	  (PUSH
	    (MAKE-IP-ETHER-CACHE-ELEMENT IP-ADDR (AR-IP-SENDER-IP ARRAY) ETHER-ADDR
					 (AR-IP-SENDER-ETHER ARRAY))
	    IP-ETHER-ADDRESS-TRANSLATIONS)
	  (UPDATE-IP-ETHER-ADDR-TRANSLATION ADDRESS-INFO (AR-IP-SENDER-ETHER ARRAY)))))
  (WHEN (AND (= (AR-OPCODE ARRAY) *AR-REQUEST*)
	     (OR (MEMBER (AR-IP-RECVER-IP ARRAY) (SEND SI:LOCAL-HOST :NETWORK-ADDRESS-LIST :IP) :TEST #'EQL)
		 (AND ip:*proxy-arp-switch*
		      ip:*gateway-host-p*   ;11-7-88 DAB (ip:gateway-host-p) is obselete*
		      (MEMBER (ar-ip-recver-ip array) ip:ip-routing-table
			      :test #'(lambda (addr rte)
					(AND (NOT (EQ :direct (ip:ip-routing-address rte)))
					     (EQL (LOGAND addr (ip:ip-routing-mask rte))
						  (LOGAND (ip:ip-routing-network rte) (ip:ip-routing-mask rte)))))))))
    (SEND-IP-ADDR-PKT (AR-IP-SENDER-IP ARRAY) (AR-IP-RECVER-IP ARRAY) (AR-IP-SENDER-ETHER ARRAY)
		      ETHER-PKT-TYPE)))

(DEFUN UPDATE-IP-ETHER-ADDR-TRANSLATION (OLD-INFO ETHER-ADDR)
  "2Update the OLD-INFO from IP-ETHER-ADDRESS-TRANSLATIONS from the data in ARRAY.*"
  (UNLESS (= ETHER-ADDR (AR-IP-ETHER-CACHE-ETHER-ADDR OLD-INFO))
    (SETF (AR-IP-ETHER-CACHE-ETHER-ADDR OLD-INFO) ETHER-ADDR))
  (SETF (AR-IP-ETHER-CACHE-AGE OLD-INFO) *INITIAL-CACHE-AGE*))

(DEFUN SEND-IP-ADDR-PKT (DEST-IP-ADDRESS SOURCE-IP-ADDRESS &OPTIONAL HIS-ETHER (ETHER-PKT-TYPE :ETHER))
  (declare (special *NO-XMIT-NO-INT-PKT*)) 
  (LET ((PKT (FUNCALL ALLOCATE-BUFFER))
	(CONTROLLER
	 (OR (MAP-ADDRESS-TO-CONTROLLER :IP SOURCE-IP-ADDRESS) (FIRST NET::CONTROLLER-LIST))))
    (COND
      ((NOT (NULL PKT))
       (MAKE-IP-ADDR-PKT PKT DEST-IP-ADDRESS SOURCE-IP-ADDRESS
			 (IF HIS-ETHER
			   *AR-REPLY*
			   *AR-REQUEST*)
			 (OR HIS-ETHER 0))
       (SEND CONTROLLER :TRANSMIT ETHER-PKT-TYPE (OR HIS-ETHER *ETHERNET-BROADCAST-ADDRESS*) PKT
	  24)
       (IF HIS-ETHER
	 (INCF *IP-ARP-REPLIES-SENT*)
	 (INCF *IP-ARP-REQUESTS-SENT*)))
      (T (INCF *NO-XMIT-NO-INT-PKT*))))) 

(DEFUN MAKE-IP-ADDR-PKT
       (ARRAY DEST-IP-ADDRESS SOURCE-IP-ADDRESS
	&OPTIONAL (OPCODE *AR-REQUEST*) (HIS-ETHER 0))         ;1array is art-16b*
  (SETF (AR-HW-TYPE ARRAY) *AR-ETHERNET-HARDWARE*)
  (SETF (AR-PROTOCOL ARRAY) *AR-IP-PROTOCOL*)
  
  ;1; NOTE: For IP the HW ADDR length comes before the protocol addr length.*
  ;1;         The lisp machines are using the wrong byte sex!!! No, it's OK. KPK 10/9/85*
  
  (SETF (AR-H-W-ADDR-LENGTH ARRAY) *ETHER-ADDR-LENGTH*)        ;1 = 6 for ethernet*
  (SETF (AR-PROTOCOL-ADDR-LENGTH ARRAY) *IP-ADDR-LENGTH*)      ;1 = 4 for IP*
  (SETF (AR-OPCODE ARRAY) OPCODE)                              ;1Request or Answer*
  (PUT-ETHER-ADDRESS-TO-ARRAY
   (SEND (OR (MAP-ADDRESS-TO-CONTROLLER :IP SOURCE-IP-ADDRESS)
	     (FIRST NET::CONTROLLER-LIST))
      :ETHERNET-ADDRESS)
   ARRAY 4)
  (PUT-IP-ADDRESS-TO-ARRAY SOURCE-IP-ADDRESS ARRAY 7)          ;1WARNING! Track offset for changes*
                                                               ;1in AR-IP-ETHER-RES-FRAME*
  (PUT-ETHER-ADDRESS-TO-ARRAY HIS-ETHER ARRAY 9)               ;1WARNING! Track offset for changes*
                                                               ;1in AR-IP-ETHER-RES-FRAME*
  (PUT-IP-ADDRESS-TO-ARRAY DEST-IP-ADDRESS ARRAY 12))          ;1WARNING! Track offset for changes*
                                                               ;1in AR-IP-ETHER-RES-FRAME*

(DEFUN PUT-IP-ADDRESS-TO-ARRAY (ADDR ARRAY PLACE)
  (SETF (AREF ARRAY PLACE) (DPB (LDB (BYTE 8 16) ADDR) (BYTE 8 8) (LDB (BYTE 8 24) ADDR)))
  (SETF (AREF ARRAY (1+ PLACE)) (DPB (LDB (BYTE 8 0) ADDR) (BYTE 8 8) (LDB (BYTE 8 8) ADDR))))

(DEFUN GET-IP-ADDRESS-FROM-ARRAY (ARRAY PLACE)
  (LET ((WORD1 (AREF ARRAY PLACE))
	(WORD2 (AREF ARRAY (1+ PLACE))))
    (LOGIOR (ASH (DPB (LDB (BYTE 8 0) WORD1) (BYTE 8 8) (LDB (BYTE 8 8) WORD1)) 16)
	    (DPB (LDB (BYTE 8 0) WORD2) (BYTE 8 8) (LDB (BYTE 8 8) WORD2)))))

(DEFUN PRINT-IP-ADDRESS-FROM-ARRAY-IN-HEX (ARRAY PLACE)
  (FORMAT T "~16r " (LDB (BYTE 8 0) (AREF ARRAY PLACE)))
  (FORMAT T "~16r " (LDB (BYTE 8 8) (AREF ARRAY PLACE)))
  (FORMAT T "~16r " (LDB (BYTE 8 0) (AREF ARRAY (+ PLACE 1))))
  (FORMAT T "~16r " (LDB (BYTE 8 8) (AREF ARRAY (+ PLACE 1)))))

(pushnew '(:IP PRINT-IP-ETHER-ADDRESS-TRANSLATIONS) print-address-translations-handlers :test #'equal)

(DEFUN DOTTED-FORMAT-IP-ADDRESS (ADDRESS)
  "2This prints an address (ip hopefully) in dotted decimal format. 
   If the address is not a number it is passed on through as this
   is used in format statements.*"
  (IF (NUMBERP ADDRESS)
    (FORMAT () "~3,'0D.~3,'0D.~3,'0D.~3,'0D" (LDB (BYTE 8 24) ADDRESS) (LDB (BYTE 8 16) ADDRESS)
	    (LDB (BYTE 8 8) ADDRESS) (LDB (BYTE 8 0) ADDRESS))
    ADDRESS))

(DEFUN PRINT-IP-ETHER-ADDRESS-TRANSLATIONS (&OPTIONAL (STREAM *TERMINAL-IO*))
  "2Prints the IP -> Ethernet translation table to STREAM.*"
  (FORMAT STREAM "~2%   IP -> Ethernet translations~%")
  (FORMAT STREAM "~&Host                  IP            Ethernet    \"time\"")
  (DOLIST (ENC NET::CONTROLLER-LIST)
    (FORMAT STREAM "~% ~15A~a" (SEND SI:LOCAL-HOST :NAME)
	    (DOTTED-FORMAT-IP-ADDRESS
	      (OR
		(NTH (POSITION ENC NET::CONTROLLER-LIST :TEST #'EQUAL)
		     (SEND SI:LOCAL-HOST :NETWORK-ADDRESS-LIST :IP))
		"???????????????")))
    (FORMAT STREAM "   ~16,12,48r    me" (SEND ENC :ETHERNET-ADDRESS)))
  (LOOP FOR (IP-ADDR ETHER-ADDR AGE) IN IP-ETHER-ADDRESS-TRANSLATIONS DO
     (FORMAT STREAM "~% ~15A~a"
	     (LET ((HOST (SI:GET-HOST-FROM-ADDRESS IP-ADDR :IP)))
	       (IF (NULL HOST)
		 "<Unknown-IP>"
		 (SEND HOST :NAME)))
	     (DOTTED-FORMAT-IP-ADDRESS (OR IP-ADDR 0)))
     DO (FORMAT STREAM "   ~16,12,48R   ~3D" (OR ETHER-ADDR 0) (OR AGE 0)))
  (FORMAT STREAM "~%"))
