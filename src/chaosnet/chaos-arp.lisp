;;; -*- Mode:Common-Lisp; Package:ETHERNET; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

(in-package :ethernet)

(defparameter *ar-chaos-protocol* #x408)
(defparameter *chaos-addr-length* 2)
(defparameter *initial-cache-age* 63)
(defvar *address-translations-lists* '()
  "A list of address translations lists.")
(defvar *get-ethernet-address-handlers* ()
  "An alist of (protocol-keyword handler)")

(defstruct (res-frame
	     (:constructor nil)
	     (:conc-name ar-)
	     (:predicate nil)
	     (:copier nil)
	     (:type vector))

  hw-type
  ;; ether protocol being resolved, ethernet: 6, chaosnet: 2
  protocol
  (lengths 518)
  (h-w-addr-length 8)
  (protocol-addr-length 520)
  ;; request or answer
  opcode)

(defstruct (chaos-ether-res-frame
	     (:include res-frame)
	     (:constructor nil)
	     (:conc-name ar-chaos-)
	     (:predicate nil)
	     (:copier nil)
	     (:type vector))
  ;; sender's ethernet address in reverse order
  sender-third
  sender-second
  sender-first
  Sender's chaosnet address
  sender-chaos
  ;; recver's ethernet address in reverse order
  recver-third
  recver-second
  recver-first
  recver-chaos)

(defmacro ar-chaos-sender-ether (array)
  `(get-ether-address-from-array ,array 4))

(defmacro ar-chaos-recver-ether (array)
  `(get-ether-address-from-array ,array 8))

(defvar-for-peek-a-boo *chaos-arp-requests-received* 0
   "number of chaos arp request broadcasts received")

(defvar-for-peek-a-boo *chaos-arp-replies-sent* 0 "number of chaos arp replies sent")

(defvar-for-peek-a-boo *chaos-arp-requests-sent* 0
   "number of chaos arp request broadcasts sent")

(defvar-for-peek-a-boo *chaos-arp-replies-received* 0
   "number of chaos arp replies received")

;;; Resolve chaos addresses

(defstruct (chaos-ether-cache-element
	     (:conc-name ar-chaos-ether-cache-)
	     (:predicate nil)
	     (:copier nil)
	     (:type list))
  chaos-addr
  ether-addr
  (age *initial-cache-age*))

;; a list of chaos-ether-cache-elements
(defvar chaos-ether-address-translations '())

(pushnew '(:chaos chaos-ether-address-translations)
	 *address-translations-lists*)

(pushnew '(:chaos GET-CHAOS-ETHERNET-ADDRESS)
	 get-ethernet-address-handlers :test #'equal)

(defun get-chaos-ethernet-address (chaos-address)
  "Top level function called to resolve a chaos address for the ethernet."
  (cond ((zerop chaos-address)
	 *ethernet-broadcast-address*)
	((member chaos-address chaos::*my-addresses* :test #'eql)
	 (send (map-address-to-controller :chaos chaos-address) :ethernet-address))
	(t
	 (let ((address-info (assoc chaos-address chaos-ether-address-translations :test #'eql)))
	   (cond (address-info
		  (ar-chaos-ether-cache-ether-addr address-info))
		 (t
		 (send-chaos-addr-pkt chaos-address chaos:*my-address*)
		 nil))
	   ))))

;;; Handle Received Arp requests.

(pushnew (list *AR-CHAOS-PROTOCOL* 'RECEIVE-CHAOS-ADDR-PKT) receive-addr-pkt-handlers :test #'equal)

(defun receive-chaos-addr-pkt (self array ether-pkt-type)
  (select (ar-opcode array)
    (*ar-request* (incf *chaos-arp-requests-received*))
    (*ar-reply* (incf *chaos-arp-replies-received*))
    (:otherwise (incf *unknown-arp-packet-types-received*)))
  (without-interrupts
   (let ((address-info (assoc (ar-chaos-sender-chaos array) chaos-ether-address-translations :test #'eql)))
     (if (null address-info)
       (push (make-chaos-ether-cache-element chaos-addr (ar-chaos-sender-chaos array) ether-addr
					     (ar-chaos-sender-ether array))
	chaos-ether-address-translations)
       (update-chaos-ether-addr-translation address-info (ar-chaos-sender-ether array)))))
  (when (and (= (ar-opcode array) *ar-request*)
	     (member (ar-chaos-recver-chaos array) chaos::my-addresses :test #'eql))
    (send-chaos-addr-pkt (ar-chaos-sender-chaos array)
			 (ar-chaos-recver-chaos array)
			 (ar-chaos-sender-ether array) ether-pkt-type)))

(DEFUN UPDATE-CHAOS-ETHER-ADDR-TRANSLATION (OLD-INFO ETHER-ADDR)
  "Update the OLD-INFO from CHAOS-ETHER-ADDRESS-TRANSLATIONS from the data in ARRAY."
  (UNLESS (= ETHER-ADDR (AR-CHAOS-ETHER-CACHE-ETHER-ADDR OLD-INFO))
    (SETF (AR-CHAOS-ETHER-CACHE-ETHER-ADDR OLD-INFO) ETHER-ADDR))
  (SETF (AR-CHAOS-ETHER-CACHE-AGE OLD-INFO) *INITIAL-CACHE-AGE*))

(DEFUN SEND-CHAOS-ADDR-PKT (DEST-CHAOS-ADDRESS SOURCE-CHAOS-ADDRESS
						  &OPTIONAL HIS-ETHER (ETHER-PKT-TYPE :ETHER))
  (declare (special  *NO-XMIT-NO-INT-PKT*))
  (LET ((PKT (FUNCALL ALLOCATE-BUFFER)))
    (COND (PKT (MAKE-CHAOS-ADDR-PKT PKT DEST-CHAOS-ADDRESS SOURCE-CHAOS-ADDRESS
				    (IF HIS-ETHER
					*AR-REPLY*
					*AR-REQUEST*)
				    (OR HIS-ETHER 0))
	       (SEND (MAP-ADDRESS-TO-CONTROLLER :CHAOS SOURCE-CHAOS-ADDRESS) :TRANSMIT ETHER-PKT-TYPE
		     (OR HIS-ETHER *ETHERNET-BROADCAST-ADDRESS*) PKT 24)
	       (IF HIS-ETHER
		   (INCF *CHAOS-ARP-REPLIES-SENT*)
		   (INCF *CHAOS-ARP-REQUESTS-SENT*)))
	  (T
	    (INCF *NO-XMIT-NO-INT-PKT*)))))

(defun make-chaos-addr-pkt
       (array dest-chaos-address source-chaos-address
	&optional (opcode *ar-request*) (his-ether 0))         ;array is art-16b
  (setf (ar-hw-type array) *ar-ethernet-hardware*)
  (setf (ar-protocol array) *ar-chaos-protocol*)
  (setf (ar-h-w-addr-length array) *ether-addr-length*)        ; = 6 for ethernet
  (setf (ar-protocol-addr-length array) *chaos-addr-length*)   ; = 2 for chaosnet
  (setf (ar-opcode array) opcode)                              ;request or answer
  (put-ether-address-to-array
   (send (map-address-to-controller :chaos source-chaos-address)
	 :ethernet-address) array 4)                           ;warning! track offset for changes
                                                               ;in ar-chaos-ether-res-frame
  (setf (ar-chaos-sender-chaos array) source-chaos-address)
  (put-ether-address-to-array his-ether array 8)               ;warning! track offset for changes
                                                               ;in ar-chaos-ether-res-frame
  (setf (ar-chaos-recver-chaos array) dest-chaos-address))

(defun print-chaos-addr-pkt (array)
  (format t "~&ar_hardware (should be #o400 or #o1000) ~o" (aref array 0))
  (format t "~&ar_protocol (should be #o2010) ~o" (aref array 1))
  (format t "~&ar_hlength & ar_plength (should be #o1006) ~o" (aref array 2))
  (format t "~&ar_opcode (should be #o400-request or #o1000-reply) ~o" (aref array 3))
  (format t "~&ar_esender ")
  (print-ether-address-from-array-in-hex array 4)
  (format t "~&ar_csender ~o" (aref array 7))
  (format t "~&ar_etarget ")
  (print-ether-address-from-array-in-hex array 8)
  (format t "~&ar_ctarget ~o" (aref array 11)))

(add-initialization "Reset address translations"
		    '(setq ethernet:chaos-ether-address-translations ())
		    nil 'chaos:disable-list)

(add-initialization "Reset address translations"
		    '(setq ethernet:chaos-ether-address-translations ())
		    nil 'chaos:enable-list)

;;; Peek
(pushnew '(:chaos print-chaos-ether-address-translations)
	 print-address-translations-handlers :test #'equal)

(DEFUN PRINT-CHAOS-ETHER-ADDRESS-TRANSLATIONS (&OPTIONAL (STREAM *TERMINAL-IO*))
  "Prints the Chaos -> Ethernet translation table to STREAM."
  (declare (special chaos:routing-table ))
  (FORMAT STREAM "~2%   Chaos -> Ethernet translations~%")
  (FORMAT STREAM "~&Host                 Chaos     Ethernet    \"time\"")
  (DOLIST (ENC NET::CONTROLLER-LIST)
    (WHEN (MEMBER (SEND ENC :BOARD-TYPE) '(:NUBUS :NUPI/E :ETHER) :TEST #'EQ);
      (FORMAT STREAM "~% ~15A    #o~4,'0O   ~16,12,48R   me" (SEND SI:LOCAL-HOST :NAME)
	      (OR
		(let ((subnet (position enc chaos:routing-table :from-end t :test #'eq)))
		  (and subnet
		       (NTH (POSITION SUBNET CHAOS::MY-SUBNETS :TEST #'EQUAL)
			    CHAOS::MY-ADDRESSES)))
		"????????")
	      (SEND ENC :ETHERNET-ADDRESS))))
  (LOOP FOR (CHAOS-ADDR ETHER-ADDR AGE) IN CHAOS-ETHER-ADDRESS-TRANSLATIONS DO
     (FORMAT STREAM "~% ~15A    #o~4,'0O   ~16,12,48R   ~3D"
	     (LET ((HOST (SI:GET-HOST-FROM-ADDRESS CHAOS-ADDR :CHAOS t)))
	       (IF (NULL HOST)
		 "<Unknown-Chaos>"
		 (SEND HOST :NAME)))
	     CHAOS-ADDR ETHER-ADDR AGE))
  (FORMAT STREAM "~%"))
