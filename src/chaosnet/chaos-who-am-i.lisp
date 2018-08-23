;;; -*- Mode:Common-Lisp; Package:Chaos; Fonts:(Cptfont Cptfontb Hl12bi Cptfont Medfnb); Base:10 -*-

;1;;                           RESTRICTED RIGHTS LEGEND*

;1;;Use, duplication, or disclosure by the Government is subject to*
;1;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in*
;1;;Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;                                 MS 2151*
;1;;*
;1;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;;********************************************************************************
;;;    This file contains changes to the chaosnet code to implement connection of
;;;    a newly booted system to a name server and thus the world.  It does this by
;;;    using BRD broadcast packets. The WHO packet is sent when a system is first booted
;;;    and contains the name of the system from the pack id.  Any nameserver who
;;;    recognizes the name replies with a UR packet that contains the name of the
;;;    requester, the name of the namespace (domain name), the name of the server, 
;;;    the chaos address of the name server and the requestors chaos address.
;;;    Both packets are broadcast because the chaosnet addresses are 
;;;    not known until after the transaction has taken place.  
;;;********************************************************************************


;;; Some notes about who-am-I
;;; 1) The protocol is nice and light and does not require
;;;    knowledge of subnets and such which we don't know
;;;    anything about at this point.  
;;; 2) The syncronization between the local caller of who-am-I 
;;;    and the chaos net background process that posts the reply
;;;    is kind of nasty.  The code here uses without-interrupts to 
;;;    avoid conflicting with the chaos background, but nothing is 
;;;    done to lock out other who-am-I requestors.  Not a problem if
;;;    this is only done at boot.
;;; 3) The notion of just posting the replies on a polled list is not 
;;;    real elegant, but it is simple.  Maybe a way of solving several
;;;    problems is to define a structure that contains a process lock,
;;;    a list for responses and the name that we are asking about.  (Note
;;;    that while who-am-i accepts a name argument, the routine that
;;;    fields replies will not accept any that are not for this host.) 

;;;  *) Might eventually want to make this a protocol at the Ethernet level.*

;;; 04-22-88 DAB Fixed BRD-MESSAGE to set pkt-ack-num to 0 before transmitting.


(DEFPARAMETER 4*WHO-AM-I-RETRIES** 10.)
(DEFPARAMETER 4*INITIAL-WHO-AM-I-DELAY** 2.)
(DEFPARAMETER 4*RETRY-WHO-AM-I-DELAY** 4.)

(DEFVAR 4*UR-RESPONSES** NIL
  2"A list for the network to place responses to the who-am-i query."*)

(PROCLAIM '(SPECIAL NAME:*WHO-AM-I-IMPLEMENTATIONS*))
(PROCLAIM '(SPECIAL NAME:*ENABLE-WHO-AM-I-SERVICE-FUNCTIONS*))
(PROCLAIM '(SPECIAL NAME:*DISABLE-WHO-AM-I-SERVICE-FUNCTIONS*))
(PROCLAIM '(SPECIAL NAME:*WHO-AM-I-ANSWER-FUNCTION*))
(PROCLAIM '(SPECIAL NAME:*SYMBOLICS-WHO-AM-I-FUNCTION*))

1;;; Interface to protocol-independent WHO-AM-I called during booting
;;; Someday we may want an ordered list here.*
(DEFUN PUSH-END-NEW (THING LIST)
  (UNLESS (MEMBER THING LIST :TEST 'EQ)
     (PUSH-END THING LIST))
  LIST)
	
(WHEN (BOUNDP 'NAME:*WHO-AM-I-IMPLEMENTATIONS*)
  (PUSH-END-NEW 'CHAOS:WHO-AM-I NAME:*WHO-AM-I-IMPLEMENTATIONS*))
(WHEN (BOUNDP 'NAME:*ENABLE-WHO-AM-I-SERVICE-FUNCTIONS*)
  (PUSH-END-NEW 'CHAOS:TURN-ON-WHO-AM-I-SERVER NAME:*ENABLE-WHO-AM-I-SERVICE-FUNCTIONS*))
(WHEN (BOUNDP 'NAME:*DISABLE-WHO-AM-I-SERVICE-FUNCTIONS*)
  (PUSH-END-NEW 'CHAOS:TURN-OFF-WHO-AM-I-SERVER NAME:*DISABLE-WHO-AM-I-SERVICE-FUNCTIONS*))


(DEFUN 4WHO-AM-I* (&OPTIONAL (NAME (SI:GET-PACK-HOST-NAME)) (RETRY *WHO-AM-I-RETRIES*))
  2"This function shouts on the net to find out who we are.  
   Returns all values returned in the response packet"*

 (UNWIND-PROTECT
  (LET (RESPONSE-LIST
	(DELAY *INITIAL-WHO-AM-I-DELAY*)
	(OLD-ADDRESSES (net:get-host-attribute net:local-host :addresses))
	RESTORE-ADDRESSES)
    
    (SETQ *UR-RESPONSES* NIL)

    1;; Temporarily register a special contact-name *
    (UR-SWITCH NAME :ON)

    (UNLESS (SEND SI:LOCAL-HOST :NETWORK-ADDRESS-LIST :CHAOS)
       (SETF RESTORE-ADDRESSES T)
       (NET:SET-HOST-ATTRIBUTE SI:LOCAL-HOST :ADDRESSES '((:CHAOS #X0100)))
       (CHAOS:SETUP-MY-ADDRESS)
       (CHAOS:RESET T))

    1;; This is not a reliable protocol: may need to try multiple times*
    (DOTIMES (I RETRY)				
      (BRD-MESSAGE 3"TI-WHO-AM-I"* MY-SUBNET NAME)		
      (PROCESS-SLEEP (* DELAY 60.) 3"Who am I"*)	1;wait for an answer*
      (WITHOUT-INTERRUPTS
	(WHEN  (SETQ RESPONSE-LIST (CAR *UR-RESPONSES*))
           (RETURN)))
      (SETQ DELAY *RETRY-WHO-AM-I-DELAY*))

    (WHEN RESTORE-ADDRESSES
       (NET:SET-HOST-ATTRIBUTE SI:LOCAL-HOST :ADDRESSES OLD-ADDRESSES)
       (CHAOS:SETUP-MY-ADDRESS)
       (CHAOS:RESET T))

    (APPLY 'VALUES (CDR RESPONSE-LIST)))

    1;; Deregister contact-name*
    (UR-SWITCH NAME NIL)))


(DEFPARAMETER 4*CATCH-WHO-AM-I-SERVER-ERRORS* *T)


(DEFUN WHO-AM-I-SERVER ()
  (CONDITION-CASE-IF *CATCH-WHO-AM-I-SERVER-ERRORS* (COND)
      (LET* ((PKT (BRD-LISTEN "TI-WHO-AM-I"))
	     (NAME 
	       (WHEN PKT (PROG1
			   (let* ((string (pkt-string pkt))
				  (start (POSITION #\" STRING :TEST 'CHAR-EQUAL)))
			     
			     (if start 
				 (subseq string (+ 1 start) (POSITION #\" STRING :start (1+ start) :TEST 'CHAR-EQUAL))))
			   (FREE-PKT PKT))))
	     
	     (DOMAIN-NAME (WHEN NAME (FUNCALL NAME:*WHO-AM-I-ANSWER-FUNCTION* NAME)))
	     (HIS-HOST-OBJECT (WHEN DOMAIN-NAME (SI:PARSE-HOST (NAME:QUALIFIED-NAME NAME DOMAIN-NAME) :NO-ERROR)))
	     (HIS-ADDRESSES (WHEN HIS-HOST-OBJECT (SEND HIS-HOST-OBJECT :SEND-IF-HANDLES :NETWORK-ADDRESS-LIST :CHAOS)))
	     (HIS-BEST (WHEN HIS-ADDRESSES (BEST-RELATIVE-ADDRESS HIS-ADDRESSES MY-ADDRESSES)))
	     (MY-BEST (WHEN HIS-ADDRESSES (BEST-RELATIVE-ADDRESS MY-ADDRESSES HIS-ADDRESSES))))
	
	;; Difference here: packet source is my-address instead of my-best
	;; Also, should routing use my-address or my-best?
	(WHEN (AND HIS-BEST MY-BEST)
	  (let ((*print-base* 10.)
		(*print-radix* t))
	    (BRD-MESSAGE (STRING-APPEND "TI-UR-" NAME) (SUBNET HIS-BEST)
			 DOMAIN-NAME
			 SI:LOCAL-HOST-NAME
			 MY-BEST
			 HIS-BEST)))
	T)
    
    (ERROR NIL)))

(DEFUN 4TURN-ON-WHO-AM-I-SERVER* () (WHO-AM-I-SWITCH T))
(DEFUN 4TURN-OFF-WHO-AM-I-SERVER* () (WHO-AM-I-SWITCH NIL))

(DEFUN 4WHO-AM-I-SWITCH* (ON?)
  (IF ON?
     (ADD-INITIALIZATION 3"TI-WHO-AM-I"*
		       (LIST 'PROCESS-RUN-FUNCTION 3"TI-WHO-AM-I"* ''WHO-AM-I-SERVER)
		       NIL 'CHAOS:SERVER-ALIST)
     (DELETE-INITIALIZATION 3"TI-WHO-AM-I"* NIL 'CHAOS:SERVER-ALIST)))


(DEFUN 4UR-SERVER* (FULL-NAME)
  (LET ((PKT (BRD-LISTEN FULL-NAME)))
    (WHEN PKT
      (WITHOUT-INTERRUPTS
        (PUSH-END (PARSE-STRING (PKT-STRING PKT)) *UR-RESPONSES*)) 1;order in speed of response*
      (FREE-PKT PKT))))


(DEFUN 4UR-SWITCH* (MY-NAME ON?)
  (LET ((FULL-NAME (STRING-APPEND 3"TI-UR-"* MY-NAME)))
    (IF ON?
	(ADD-INITIALIZATION FULL-NAME
			    `(PROCESS-RUN-FUNCTION ,FULL-NAME 'UR-SERVER ,FULL-NAME)
			    NIL 'CHAOS:SERVER-ALIST)
	(DELETE-INITIALIZATION FULL-NAME NIL 'CHAOS:SERVER-ALIST))))

(DEFUN PARSE-STRING (STRING)
  (let ((*read-base* 10.)
	(length (length string)))
    (LOOP
      WITH CURSOR = 0
      with end = 0
      WITH VALUE = T
      WITH RESPONSE-LIST = NIL
      WHILE (AND cursor (< CURSOR length)) DO
      
      (setf end (position #\space string :start (1+ cursor)))
      (cond ((eql (char string cursor) #\")
	     (setf value (subseq string (1+ cursor) (1- end))))
	    ((digit-char-p (char string cursor))
	     (setf value (parse-number string cursor end 10. nil))) 
	    (t
	     (setf value (subseq string cursor end))))
      (setf cursor (when end (1+ end)))
      
      (WHEN VALUE (PUSH VALUE RESPONSE-LIST))
      FINALLY (RETURN (NREVERSE RESPONSE-LIST)))))


(DEFUN 4BRD-LISTEN* (CONTACT-NAME)
  1;; Pull pkt off pending rfc-list for contact-name*
  1;; but don't create a connection object *
  (WITHOUT-INTERRUPTS                     
    (DO ((PKT PENDING-RFC-PKTS (PKT-LINK PKT))
	 (PREV NIL PKT))
	((NULL PKT))
      (WHEN (STRING-EQUAL (CONTACT-NAME-FROM-RFC PKT) CONTACT-NAME)
	(COND ((NULL PREV) (SETQ PENDING-RFC-PKTS (PKT-LINK PKT)))
	      (T (SETF (PKT-LINK PREV) (PKT-LINK PKT))))
	(RETURN PKT)))))


(DEFUN 4BRD-MESSAGE* (CONTACT-NAME SUBNET &REST ARGS)
 (LET ((PKT (ALLOCATE-PKT)))
	
    (SETF (PKT-OPCODE PKT) BRD-OP)
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) 0)
    (SETF (PKT-DEST-ADDRESS PKT) 0)		1;broadcast*
    (SETF (PKT-DEST-INDEX-NUM PKT) 0)
    (SET-PKT-STRING PKT (FORMAT NIL 3"~A ~{~*S3 ~}"* CONTACT-NAME ARGS))
    (SETF (PKT-FWD-COUNT PKT) 0)
    (SETF (PKT-ACK-NUM PKT) 0)                  ;04-22-88 DAB This mmust be initialized to 0.
    (TRANSMIT-PKT-ROUTE (CONVERT-TO-INT-PKT PKT) 0 subnet)		
    (FREE-PKT PKT)))


(DEFUN 4BEST-RELATIVE-ADDRESS* (FROM TO)
  "2Returns the best address in the list FROM relative to the
   addresses in TO.  Best means a common subnet.  If no* 2best, returns the
   first address in FROM.*"
  (LET ((BEST (DOLIST (ADDR TO)
		(LET ((GOOD (CAR (MEMBER (SUBNET ADDR) FROM :TEST #'(LAMBDA (X Y) (= X (SUBNET Y)))))))
		  (WHEN GOOD
		    (RETURN GOOD))))))
    (IF BEST
	BEST
	(CAR FROM))))


1;;; This is here to keep CHAOS references out of the namespace boot code*
(DEFUN 4SYMBOLICS-WHO-AM-I-REQUEST* (HOST)
  2"Issue a symbolics who-am-i-request to HOST.  Return DOMAIN-NAME or NIL"*
    (LET* ((CONN (CHAOS:ESTABLISH-CONNECTION HOST 3"WHO-AM-I"* 15. 1200. T))
	   (ME-STRING (WHEN CONN (CHAOS:PKT-STRING CONN)))
	   (SPLIT (WHEN ME-STRING (POSITION #\| ME-STRING))))
       (WHEN ME-STRING (SUBSEQ ME-STRING 0 SPLIT))))

(SETF NAME:*SYMBOLICS-WHO-AM-I-FUNCTION* 'SYMBOLICS-WHO-AM-I-REQUEST)

(PUSHNEW ':CHAOS *FEATURES*)  ;required for rel4.
