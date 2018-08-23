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

(DEFVAR PACKET-RECORD-P T
   "2controls whether packets are copied*"
   ) 


(DEFPARAMETER *MAX-RECENT-PACKETS* 120
   "2maximum pkt copies retained*"
   ) 


(DEFVAR RECENT-PACKETS (MAKE-ARRAY *MAX-RECENT-PACKETS* :AREA *IP-AREA*)
   "2Set of copied ip packets recently sentreceived*"
   ) 


(DEFVAR NEXT-RECENT-PACKET 0
   "2next packet slot to be filledrefilled*"
   ) 


(DEFSTRUCT (RCNT-PKT (:CONC-NAME RCNT-PKT-) (:CALLABLE-CONSTRUCTORS NIL) (:ALTERANT ALTER-RCNT-PKT)
  (:PREDICATE NIL) (:COPIER NIL) (:TYPE :ARRAY))
  (TIME ());1time the packet was record*
  (MODE NIL);1indicates whether packet was transmitted or received*
  (COPY NIL)) 				   ;1duplicate of contents of entire packet*


(defun RECORD-PACKET (pkt mode
		      &aux slot
		      (words (min *max-total-packet-16-bit* (ceiling (IP-total-length pkt)))))
  "2Called to copy packet into history buffer along with information about time and
   mode (whether it was a :transmit or :receive packet).*" 
  (when packet-record-p 
    (unless (aref recent-packets next-recent-packet)
      (setf (aref recent-packets next-recent-packet) (make-rcnt-pkt)))
    (setf slot (aref recent-packets next-recent-packet))
    (unless (rcnt-pkt-copy slot)
      (setf (rcnt-pkt-copy slot)
	    (MAKE-ARRAY *max-total-packet-16-bit* :type :art-16b :area *ip-area*)))
    
    ;1; Check for valid packet size to prevent garbage*
    ;1; packet length from hanging program.*
    
    (if (not *use-blt-p*)
	(progn     
	  (copy-array-portion pkt 0 words (rcnt-pkt-copy slot) 0 words)
	  (copy-pkt pkt (rcnt-pkt-copy slot) (IP-total-length pkt)))
	(LET ((si:inhibit-scavenging-flag t))
	  (WITHOUT-INTERRUPTS
	    (SYS:%BLT (SYS:%MAKE-POINTER-OFFSET SI:DTP-FIX PKT (SI:ARRAY-DATA-OFFSET PKT))
		      (SYS:%MAKE-POINTER-OFFSET SI:DTP-FIX (rcnt-pkt-copy slot)
						(SI:ARRAY-DATA-OFFSET (rcnt-pkt-copy slot)))
		      (CEILING words 2)
		      1))))
    
    (setf (rcnt-pkt-time slot) (time))
    (setf (rcnt-pkt-mode slot) mode)
    (setf next-recent-packet (mod (1+ next-recent-packet) *max-recent-packets*))))


(DEFUN DISPLAY-RECENT-PACKET-HEADERS (&KEY (STREAM *STANDARD-OUTPUT*) (LIMIT 16)
					   ;1max number of packets to display*
				      (IP-HEADER T)	   ;1display the ip header part?*
				      (TRANSPORT-HEADER T) ;1display the udp, tcp, icmp part?*
				      (DISPLAY-DATA 0)	   ;1how much data?*
				      (PROTOCOLS '(:TCP :UDP :ICMP))	   ;1show which protocol pkts?*
				      (PORT NIL)   ;1show if src OR dst matches, udp or tcp port*
  (SRC-PORT NIL)                      ;1show if src port matches*
  (DST-PORT NIL)                      ;1show if dst port matches*
  (ADDRESS NIL)                       ;1show if src or dst matches, ip address    *
  (SRC-ADDR NIL)                      ;1show if src address matches*
  (DST-ADDR NIL)                      ;1show if dst address matches*
  (RECEIVE T)                         ;1show received packets*
  (TRANSMIT T))                       ;1show transmitted packets*
 
  "2Outputs a formatted summary of most information in packet copies in the history buffer.
   The packets to be displayed can be chosen via a variety of criteria.  See the source
   for details.  To initiate trace, set *record-packet-mode* to T.*"
  (CATCH 'DISPLAY-DONE
    (LET* (I
	   (DISPLAYED 0)
	   (DISPLAY-DATA-LENGTH 0)
	   (RECENT-PACKET-INDEX NEXT-RECENT-PACKET))
      (DOTIMES (X *MAX-RECENT-PACKETS* NIL)
	(WHEN (> DISPLAYED LIMIT)
	  (THROW 'DISPLAY-DONE
		 ()))
	(SETQ I (MOD (- RECENT-PACKET-INDEX X 1) *MAX-RECENT-PACKETS*))
	(UNLESS (OR (NULL (AREF RECENT-PACKETS I)) (NULL (RCNT-PKT-COPY (AREF RECENT-PACKETS I))))
	  (LET* ((SLOT (AREF RECENT-PACKETS I))
		 (PKT (RCNT-PKT-COPY SLOT))
		 (UDP-PKT (MAKE-IP-DATA PKT))
		 (TCP-PKT UDP-PKT)
		 (PKT-PROTOCOL
		  (CAR (RASSOC (IP-PROTOCOL PKT) *IP-PROTOCOL-MAPPING* :TEST #'EQUAL))))
	    (WHEN (AND (MEMBER PKT-PROTOCOL PROTOCOLS :TEST #'EQUAL)
		(OR
		 (AND RECEIVE;1include sends and receives*
		    (MEMBER (RCNT-PKT-MODE (AREF RECENT-PACKETS I)) '(:RCV :RECEIVE) :TEST
			    #'EQUAL))
		 (AND TRANSMIT
		    (MEMBER (RCNT-PKT-MODE (AREF RECENT-PACKETS I)) '(:XMT :XMIT :TRANSMIT)
			    :TEST #'EQUAL)))
		(OR (AND (NULL ADDRESS);1include all addresses*
		       (NULL SRC-ADDR) (NULL DST-ADDR))
		   (AND (NUMBERP ADDRESS)
		      (OR (EQL ADDRESS (IP-SRC-ADDR PKT)) (EQL ADDRESS (IP-DST-ADDR PKT))))
		   (AND (NUMBERP SRC-ADDR) (EQL SRC-ADDR (IP-SRC-ADDR PKT)))
		   (AND (NUMBERP DST-ADDR) (EQL DST-ADDR (IP-DST-ADDR PKT))))
		(OR (AND (NULL PORT);1include all ports*
		       (NULL SRC-PORT) (NULL DST-PORT))
		   (SELECT PKT-PROTOCOL
		      (:UDP
		       (OR
			(AND (NUMBERP PORT)
			   (OR (EQL PORT (UDP-HEADER-SOURCE-PORT UDP-PKT))
			      (EQL PORT (UDP-HEADER-DESTINATION-PORT UDP-PKT))))
			(AND (NUMBERP SRC-PORT) (EQL SRC-PORT (UDP-HEADER-SOURCE-PORT UDP-PKT)))
			(AND (NUMBERP DST-PORT)
			   (EQL DST-PORT (UDP-HEADER-DESTINATION-PORT UDP-PKT)))))
		      (:TCP
		       (OR
			(AND (NUMBERP PORT)
			   (OR (EQL PORT (TCP-HEADER-SOURCE-PORT TCP-PKT))
			      (EQL PORT (TCP-HEADER-DESTINATION-PORT TCP-PKT))))
			(AND (NUMBERP SRC-PORT) (EQL SRC-PORT (TCP-HEADER-SOURCE-PORT TCP-PKT)))
			(AND (NUMBERP DST-PORT)
			   (EQL DST-PORT (TCP-HEADER-DESTINATION-PORT TCP-PKT)))))
		      (:OTHERWISE T))))
	      (DISPLAY-RECORDED-PACKET-HEADER I STREAM IP-HEADER TRANSPORT-HEADER)
	      (SETF DISPLAY-DATA-LENGTH (MIN DISPLAY-DATA (* 2 (IP-TOTAL-LENGTH PKT)))))
	      (WHEN (PLUSP DISPLAY-DATA-LENGTH)
		(PRINT-PACKET TCP-PKT :STREAM STREAM :SIZE DISPLAY-DATA-LENGTH)) 
	      (INCF DISPLAYED))))))) 


(DEFUN DISPLAY-RECORDED-PACKET-HEADER (&OPTIONAL (INDEX *MAX-RECENT-PACKETS*) (STREAM *STANDARD-OUTPUT*)
				       (IP-HEADER T)
  (TRANSPORT-HEADER T))
  "2Displays to stream a formatted summary of most information in the designated
   packet copy in the history buffer.*"
  (LET ((SLOT (AREF RECENT-PACKETS INDEX)))
    (WHEN SLOT
      (LET ((PKT (RCNT-PKT-COPY SLOT)))
	(FORMAT STREAM "~&~16,4,'0r" (MOD (RCNT-PKT-TIME SLOT) 65536))
	(SELECT (RCNT-PKT-MODE SLOT) ((:RCV :RECEIVE) (SEND STREAM :STRING-OUT " R"))
	   ((:XMT :XMIT :TRANSMIT) (SEND STREAM :STRING-OUT " X")))
	(WHEN IP-HEADER
	  (DISPLAY-IP-HEADER PKT STREAM))
	(WHEN (AND IP-HEADER TRANSPORT-HEADER)
	  (SEND STREAM :FRESH-LINE)
	  (SEND STREAM :STRING-OUT "      "))
	(WHEN TRANSPORT-HEADER
	  (SELECT (CAR (RASSOC (IP-PROTOCOL PKT) *IP-PROTOCOL-MAPPING* :TEST #'EQUAL))
	     (:UDP (DISPLAY-UDP-HEADER (MAKE-IP-DATA PKT) STREAM))
	     (:TCP (DISPLAY-TCP-HEADER (MAKE-IP-DATA PKT) STREAM))
	     (:ICMP (DISPLAY-ICMP-HEADER (MAKE-IP-DATA PKT) STREAM)))))))) 


(DEFUN DISPLAY-IP-HEADER-ADDRESS (ADDRESS STREAM)
  (IF (NUMBERP ADDRESS)
    (FORMAT STREAM "~3,'0D.~3,'0D.~3,'0D.~3,'0D" (LDB (BYTE 8 24) ADDRESS)
	    (LDB (BYTE 8 16) ADDRESS) (LDB (BYTE 8 8) ADDRESS) (LDB (BYTE 8 0) ADDRESS))
    (FORMAT STREAM "~a" ADDRESS))) 


(DEFUN DISPLAY-IP-HEADER (PKT &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "2Displays to stream a formatted summary of the ip header information in a packet.*"
  (SEND STREAM :STRING-OUT " pr:")
  (SELECT (CAR (RASSOC (IP-PROTOCOL PKT) *IP-PROTOCOL-MAPPING* :TEST #'EQUAL))
     (:UDP (SEND STREAM :STRING-OUT "UDP")) (:TCP (SEND STREAM :STRING-OUT "TCP"))
     (:ICMP (SEND STREAM :STRING-OUT "ICMP"))
     (OTHERWISE (FORMAT STREAM "~16,2,'0r" (IP-PROTOCOL PKT))))
  (FORMAT STREAM " sa:")
  (DISPLAY-IP-HEADER-ADDRESS (IP-SRC-ADDR PKT) STREAM)
  (FORMAT STREAM " da:")
  (DISPLAY-IP-HEADER-ADDRESS (IP-DST-ADDR PKT) STREAM)
  (FORMAT STREAM " tl:~16,4,'0r" (IP-TOTAL-LENGTH PKT))
  (FORMAT STREAM " hl:~16,1,'0r" (IP-HEADER-LENGTH PKT))
  (SEND STREAM :STRING-OUT " ")
  (IF (IP-DONT-FRAGMENT-P PKT)
    (SEND STREAM :STRING-OUT "D")
    (SEND STREAM :STRING-OUT "."))
  (IF (IP-MORE-FRAGMENTS-P PKT)
    (SEND STREAM :STRING-OUT "M")
    (SEND STREAM :STRING-OUT "."))
  (FORMAT STREAM " fo:~16,4,'0r" (IP-FRAGMENT-OFFSET PKT))
  (FORMAT STREAM " tt:~16,2,'0r" (IP-TIME-TO-LIVE PKT))
  (FORMAT STREAM " vs:~16,1,'0r" (IP-VERSION PKT))
  (FORMAT STREAM " hc:~16,4,'0r" (IP-HEADER-CHECKSUM PKT))) 



(DEFUN DISPLAY-TCP-HEADER (PKT &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "2Displays to stream a formatted summary of the tcp header information in a packet.*"
  (FORMAT STREAM " sp:~16,4,'0r" (TCP-HEADER-SOURCE-PORT PKT))
  (FORMAT STREAM " dp:~16,4,'0r" (TCP-HEADER-DESTINATION-PORT PKT))
  (FORMAT STREAM " se:~16,8,'0r" (TCP-HEADER-SEQUENCE-# PKT))
  (FORMAT STREAM " ak:~16,8,'0r" (TCP-HEADER-ACK-# PKT))
  (FORMAT STREAM " wn:~16,4,'0r" (TCP-HEADER-WINDOW PKT))
  (SEND STREAM :STRING-OUT " ")
  (IF (ZEROP (TCP-HEADER-URGENT-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "U"))
  (IF (ZEROP (TCP-HEADER-ACK-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "A"))
  (IF (ZEROP (TCP-HEADER-PUSH-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "P"))
  (IF (ZEROP (TCP-HEADER-RESET-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "R"))
  (IF (ZEROP (TCP-HEADER-SYN-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "S"))
  (IF (ZEROP (TCP-HEADER-FIN-FLAG PKT))
    (SEND STREAM :STRING-OUT ".")
    (SEND STREAM :STRING-OUT "F"))
  (FORMAT STREAM " up:~16,4,'0r" (TCP-HEADER-URGENT-POINTER PKT))
  (FORMAT STREAM " do:~16,4,'0r" (TCP-HEADER-DATA-OFFSET PKT))
  (FORMAT STREAM " ck:~16,4,'0r" (TCP-HEADER-CHECKSUM PKT))) 


(DEFUN DISPLAY-ICMP-HEADER (PKT &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "2Displays to stream a formatted summary of the Icmp header information in a packet.*"
  (FORMAT STREAM " tp:~16,2,'0r" (ICMP-TYPE PKT))
  (FORMAT STREAM " cd:~16,2,'0r" (ICMP-CODE PKT))
  (FORMAT STREAM " ck:~16,4,'0r" (ICMP-CHECKSUM PKT))
  (FORMAT STREAM " o1:~16,4,'0r" (ICMP-OPTION-1 PKT))
  (FORMAT STREAM " o2:~16,4,'0r" (ICMP-OPTION-2 PKT))) 


(DEFUN DISPLAY-UDP-HEADER (PKT &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (FORMAT STREAM " sp:~16,4,'0r" (UDP-HEADER-SOURCE-PORT PKT))
  (FORMAT STREAM " dp:~16,4,'0r" (UDP-HEADER-DESTINATION-PORT PKT))
  (FORMAT STREAM " ln:~16,4,'0r" (UDP-HEADER-LENGTH PKT))
  (FORMAT STREAM " ck:~16,4,'0r" (UDP-HEADER-CHECKSUM PKT))) 


(DEFUN DISPLAY-PKT-DATA (PKT &KEY &OPTIONAL (STREAM *TERMINAL-IO*) (SIZE 0) (OFFSET 0))
  "2This dumps an art-8b array to the stream stream.
    If no size is given than the size of the array is used.*"
  (INCF SIZE OFFSET)
  (LET ((LEN (IF (ZEROP SIZE)
	       (LENGTH PKT)
	       SIZE))
	CHAR1)
    (DO ((I OFFSET))
	((>= I LEN))
      (SEND STREAM :FRESH-LINE)
      (SEND STREAM :STRING-OUT "       ")
      (LOOP FOR H FROM 0 TO 15 UNLESS (>= (+ I H) LEN) DO
	 (LET ((*STANDARD-OUTPUT* STREAM))
	   (FORMAT:ONUM (AREF PKT (+ I H)) 16 2 :PAD-CHAR 48))
	 DO (SEND STREAM :STRING-OUT " "))
      (SEND STREAM :STRING-OUT "    "
	 )
      (LOOP FOR H FROM 0 TO 15 UNLESS (>= (+ I H) LEN) DO (SETF CHAR1 (AREF PKT (+ I H))) AND DO
	 (SEND STREAM :TYO (CHARACTER (IF (OR (< CHAR1 32) (>= CHAR1 128))
					0
					CHAR1))) FINALLY
	 (INCF I H)))
    (SEND STREAM :FRESH-LINE))) 

;1;Enable TCP/IP and associated protocols once the make-system is complete
*(ip:reset t)