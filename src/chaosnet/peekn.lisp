;;; -*- Mode:COMMON-LISP; Package: CHAOS; BASE: 10; Fonts: Medfnt, Medfnb, Courier, Medfnb; -*-


1;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;
;; Copyright (c) 1981 Massachusetts Institute of Technology 
;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved.
;;
;;-----------------------------------------------------------------------------
;;                                    Chaosnet PEEK Functions
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;*
;;;  12-14-88 DAB   Fixed some heading alienments in chaos-peek.
;;;  **************** Rel 5.0 OS 12-12-88 DAB
1;;  10/15/86 LS - Made some changes to go along with Generic Peek access.
;;  9/30/86 RLA - Separated net vs. chaos parts 
;;  3/25/86 MMG - Conversion to Common Lisp.
;;  9/19/85 RWF - Made the Network item extensible.  A dynamic pop-up choose
;;                window can be used to choose between different protocols.
;;  5/01/85 MJF - Changed  (:METHOD TV:BASIC-PEEK :PEEK-CHAOS-HOST-MENU 
;;                handling of HOSTAT-ONE AND HOSTAT-ALL.
;;  4/40/85 MJF - Changed how DESCRIBE is called in PEEK-CHAOS-CONN.
;;                Also changed the LOCAL-DECLARE Special of PEEK-CHAOS-HOST to 
;;                to a DECLARE SPECIAL inside the LET.
;;  3/12/85 RAF - Add C-PEEK-A-BOO-LIST for controller instance variables.
;;  1/07/85 RAF - Update for Flavor based Network controllers.
;;  9/27/84 RAF - Rename CHAOS -> NETWORK as seen by User.  Reformat Meter Display
;;                and add PEEK-AVERAGES-LIST as a special type of meter.
;;-----------------------------------------------------------------------------*

(defvar 4subnet-name-cache* nil 2"saves current name of subnet bridge"*)

(defun 4peek-hostat* (&rest ignore)
  (hostat)) 


(net::add-to-peek-menu 3"CHAOS"* chaos-peek 2"Display the CHAOS statisics"*)


1;;; *BJ* For *RWF**
(defun 4chaos-peek* (ignore)
  2"Displays state of all chaos net connections, meters, and routing table"*
  (setf subnet-name-cache (quote nil))
  (list ()
	(tv:scroll-parse-item 3"Chaosnet connections at "*
			      `(:function time:print-current-time (nil)))
	(tv:scroll-parse-item 3""*)
	(tv:scroll-maintain-list #'(lambda ()
				     conn-list)
				 #'peek-chaos-conn)
	(tv:scroll-parse-item 3""*) (tv:scroll-parse-item net::controller-header-string)
	(tv:scroll-maintain-list #'(lambda ()
				     net::controller-list)
				 #'net::peek-controller-data)
	(tv:scroll-parse-item 3""*)
	(tv:scroll-parse-item 3"== Values of Other Interesting Meters ==============="*)
	;(tv:scroll-maintain-list #'(lambda ()
	;			     net::c-peek-a-boo-list)
	;			 #'net::peek-a-boo-parse)
	(tv:scroll-maintain-list #'(lambda ()
				     net::peek-a-boo-list)
				 #'net::peek-a-boo-parse)
	(tv:scroll-parse-item 3""*) (tv:scroll-parse-item 3"      *   3  Gateway*    G3ateway"*)
	(tv:scroll-parse-item 3"Subnet*   3  Address  *   3Name *     3Cost"*)
	(tv:scroll-parse-item 3"------ *   3 ------- *  3 -------*   3 ----"*)
	(tv:scroll-maintain-list #'(lambda ()
				     1)
				 #'(lambda (subnet)
				     (tv:scroll-parse-item
				       `(:string ,(if subnet
						      (format () 3" ~16,4,'0r"* subnet)
						      3""*)
						 10)
				       `(:function peek-chaos-gateway-address (,subnet) 11)	1;rwf 08/12/86*
				       `(:function peek-chaos-subnet-name (,subnet) 11)
				       `(:function peek-chaos-routing-cost (,subnet) 4)))
				 ()
				 #'(lambda (subnet)
				     (let ((new-subnet
					     (position-if-not
					       #'(lambda (s)
						   (and (numberp s) (zerop s)))
					       routing-table :start subnet)))
				       (values new-subnet (and new-subnet (+ new-subnet 1))
					       (not new-subnet)))))))

(defun 4peek-chaos-gateway-address* (subnet)	1;rwf 08/12/86*
  (if (null subnet)
      3""*
      (if (numberp (aref routing-table subnet))
	  (format () 3" ~16,4,'0r"* (aref routing-table subnet))
	  3""*)))







(defun 4peek-chaos-packet-item* (pkt &optional (indent 0))
  2"Returns an item that describes a chaosnet packet.  Mouseable subfields are:
   The host:  Left: Causes info about the host to displayed inferior to the packet.*
	2      Middle: Causes a static hostat to be displayed inferior to the packet.
  *	2      Right (menu): Typeout Hostat, Telnet, Qsend

Sample output:
Pkt [to ! from] <name> (number){, transmitted <n> times (at <time>)}{, being retransmitted}{, released}{, fowarded <n> times}
    <op> (<number>), <n> bytes, number <n>, acking <n>, source idx <n>, dest idx <n>
    Words from <n>: <wordn> ... <wordn+m>
    String: <string>

Packet: to AI (2026), transmitted 27 times (at 1231232), being retransmitted
 CLS (11), 432 bytes, number 3422, acking 3221, source idx 177777, dest idx 177777
 Words from 0: 123123 12371 1227 272727 272626
 String: 'Now is the time for all good men'

Packet: from MC (1440), released, forwarded 17 times
 DAT (201), 100 bytes, number 432, acking 102, source idx 123451, dest idx 123441
 Words from 0: 123123 64532
 String: 'BAD !'

"*
  (let ((to-us (and (zerop (pkt-times-transmitted pkt)) (= (pkt-dest-address pkt) my-address)))
	(other-host))
    (setq other-host (if to-us
		       (pkt-source-address pkt)
		       (pkt-dest-address pkt)))
    (list ()
	  (list '(:pre-process-function peek-chaos-packet-insert-hostat)
		(tv:scroll-parse-item :leader 4
				      `(:mouse-item
					(nil :eval
					 (net:peek-host-menu
					   ',(si:get-host-from-address other-host :chaos) ;; LS 1/15/87
					   'tv:item 0 ,indent)
					 :documentation
					 3"Menu of useful things to do to this host."*)
					:string
					,(format () 3"~VXPacket ~:[to~;from~] ~@[~A ~](~O)"*
						 indent to-us
						 (si:get-host-from-address other-host :chaos)
						 other-host))
				      (and (not to-us)
					 `(:function ,(function pkt-times-transmitted) (,pkt)
					   nil (3", transmitted ~D times"*)))
				      (and (not to-us)
					 `(:function ,(function pkt-time-transmitted) (,pkt) nil
					   (3" (at ~O)"*)))
				      (and (not to-us)
					 `(:function ,(function pkt-being-retransmitted) (,pkt)
					   nil (3"~:[, being retransmitted~;~]"*)))
				      `(:function ,(function pkt-status) (,pkt) nil
					(3"~:[~;, Status: ~0G~A~]"*))
				      (and to-us
					 (format () 3", fowarded ~D times"* (pkt-fwd-count pkt)))))
	  1;; Second line*
	  (let ((op (pkt-opcode pkt)))
	    (tv:scroll-parse-item
	     (format () 3"~VX~A (~O), ~O bytes, number ~O, acking ~O, source idx ~O, dest idx ~O"*
		     indent (if (>= op dat-op)
			      3"Data"*
			      (nth op opcode-list))
		     op (pkt-nbytes pkt) (pkt-num pkt) (pkt-ack-num pkt)
		     (pkt-source-index-num pkt) (pkt-dest-index-num pkt))))
	  (tv:scroll-parse-item (format () 3"~VX"* indent) (peek-chaos-pkt-words pkt 0 6))
	  (tv:scroll-parse-item (format () 3"~VXString: "* indent) (peek-chaos-pkt-string pkt))))) 




(defun 4peek-chaos-pkt-words* (pkt start number &aux string)
  2"Returns a string consisting of words from the packet."*
  (setq string (format () 3"Words from ~O: "* start))
  (do ((i start (1+ i))
       (len (array-total-size pkt)))
      ((or (>= i (+ start number)) (>= i len))
       string)
    (setq string
	  (string-append string (format () 3"~6O"* (aref pkt (+ first-data-word-in-pkt i))) 3" "*)))) 


1;;; Boy, is this piece of *#!$ ad hoc!!*

(defun 4peek-chaos-pkt-string* (pkt &optional count)
  2"Returns a 'safe' string as far as the scrolling stuff is concerned"*
  (do ((string (make-array 64 :element-type 'string-char :leader-list '(0)))
       (pkt-string (pkt-string pkt))
       (char)
       (i 0 (1+ i))
       (len (length (pkt-string pkt))))
      ((or (>= i len) (and count (>= i count)))
       string)
    (setq char (aref pkt-string i))
    (if (and (< char 128) (/= char #\))
      (vector-push-extend char string)
      (progn
	(vector-push-extend #\ string)
	(if (/= char #\)
	  (vector-push-extend (logior 64 (logand char 63)) string)
	  (vector-push-extend #\ string)))))) 

1;; *BJ* For *RWF**

(defun 4peek-chaos-conn* (conn)
  2"Format is:

Connection to <contact> atfrom Host <name> (<number>),
<state>, local idx <n>, foreign idx <n>
Windows: local <n>, foreign <n> (<n> available)
Received: pkt <n> (time <n>), read pkt <n>, ack pkt <n>, <n> queued
Sent: pkt <n>, ack for pkt <n>, <n> queued
"*
  (list ()
	(list '(:pre-process-function peek-chaos-conn-insert-hostat)
	      (tv:scroll-parse-item :leader 3
				    `(:mouse-item
				       (nil :menu-choose
					    (3"Connection Operations"*
					     (3"Close"* :eval (close-conn conn) :documentation
					      3"Close this connection"*)
					     (3"Probe"* :eval (probe-conn conn t) :documentation
					      3"Probe this connection"*)
					     (3"Status"* :eval (transmit-sts conn 'peek) :documentation
					      3"Send Status on this connection"*)
					     (3"Retransmit"* :eval (retransmission conn t)
					      :documentation
					      3"Retransmit any packets still pending on this connection"*)
					     (3"Send LOS"* :eval (los-conn conn) :documentation
					      3"Send an LOS packet to nuke the other sides connection"*)
					     (3"Remove"* :eval (remove-conn conn) :documentation
					      3"Arbitrarily remove this conn as an active connection"*)
					     (3"Describe"* :eval
					      (let ((*terminal-io* typwin))
						(send ,self :force-kbd-input '(describe ,conn)))
					      :documentation 3"Describe this connection."*)
					     (3"Inspect"* :eval
					      (let ((*terminal-io* typwin))
						(send ,self :force-kbd-input '(inspect ,conn)))
					      :documentation 3"Inspect this connection."*))
					    :documentation 3"Menu of things to do to this connection."*
					    :bindings
					    ((conn ',conn) (typwin ',(funcall self :typeout-window))))
				       :function contact-name (,conn) nil
				       (3"~@[Connection to ~A~]"*))
				    `(:function
				       ,(function
					  (lambda (conn)
					    (cond
					      ((getf (conn-plist conn) 'rfc-contact-name)
					       3" at host"*)
					      ((getf (conn-plist conn) 'listen-contact-name)
					       3" from host"*)
					      (t 3"Host"*))))
				       (,conn) nil (3"~A "*))
				    1;; The following code returns a list which specifies the entry*
				    1;; that prints the name of the connection's host.*
				    1;; The hair is because we create a closure to put*
				    ;;      1in the entry specification.*
				    (let ((peek-chaos-host (cons -1 ())))
				      (declare (special peek-chaos-host))
				      `(:mouse-item
					 (nil :eval
					      (net:peek-host-menu
						`,(si:get-host-from-address               ;; LS 1/15/87
						    (car ',(locf (car peek-chaos-host)))
						    :chaos)
						'tv:item 0)
					      :documentation
					      3"Menu of useful things to do to this host."*)
					 :function
					 ,(closure '(peek-chaos-host)
						   #'(lambda (conn)
						       (and
							 (/= (car peek-chaos-host)
							     (prog2
							       (rplaca peek-chaos-host
								       (foreign-address conn))
							       (car peek-chaos-host)))
							 (rplacd peek-chaos-host
								 (format () 3"~@[~A ~](~O), "*
									 (si:get-host-from-address
									   (car peek-chaos-host)
									   :chaos)
									 (car peek-chaos-host))))
						       (cdr peek-chaos-host)))
					 (,conn) nil))))
	(tv:scroll-parse-item `(:function state (,conn) nil)
			      `(:function local-index-num (,conn) nil (3", local idx ~O, "*))
			      `(:function foreign-index-num (,conn) nil (3"foreign idx ~O"*)))
	(tv:scroll-parse-item
	  `(:function ,(function local-window-size) (,conn) nil (3"Windows: local ~D, "*))
	  `(:function ,(function foreign-window-size) (,conn) nil (3"foreign ~D, "*))
	  `(:function ,(function window-available) (,conn) nil (3"(~D available)"*)))
	(list `(:pre-process-function peek-chaos-conn-received-pkts :connection ,conn)
	      (tv:scroll-parse-item :leader 1 :mouse-self
				    '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					  3"Insert/remove display of packets on receive list."*)
				    `(:function ,(function pkt-num-received) (,conn) nil
						(3"Received: pkt ~O"*))
				    `(:function ,(function time-last-received) (,conn) nil
						(3" (time ~O), "*))
				    `(:function ,(function pkt-num-read) (,conn) nil
						(3"read pkt ~O, "*))
				    `(:function ,(function pkt-num-acked) (,conn) nil
						(3"ack pkt ~O, "*))
				    `(:function
				       ,(function
					  (lambda (conn)
					    (- (pkt-num-received conn) (pkt-num-read conn))))
				       (,conn) nil (3"~D queued"*))))
	(list `(:pre-process-function peek-chaos-conn-send-pkts :connection ,conn)
	      (tv:scroll-parse-item :leader 1 :mouse-self
				    '(nil :eval (tv::peek-mouse-click 'self 0) :documentation
					  3"Insert/remove display of packets on transmit list."*)
				    `(:function ,(function pkt-num-sent) (,conn) nil
						(3"Sent: pkt ~O, "*))
				    `(:function ,(function send-pkt-acked) (,conn) nil
						(3"ack for pkt ~O, "*))
				    `(:function ,(function send-pkts-length) (,conn) nil
						(3"~D queued"*))))
	(tv:scroll-parse-item 3""*)))

(compiler:make-obsolete safe-divide "Obsolete?")
(deff safe-divide 'net:safe-divide)
;;(defun 4safe-divide* (num denom)1;obsolete ?? 1/85 RAF*
;; 1;; Computes the ratio of NUM to DENOM, returning ZERO if DENOM is zero.*
;;  (if (= 0 denom)
;;    0
;;    (round num denom))) 


1;;; *BJ* For *RWF**
(defun 4peek-chaos-subnet-name* (subnet)		1;update for new Routing-Table, RAF.*
  (if (null subnet)
      3""*
      (let ((string
	      (format () 3"~:[Direct~;~A~]"* (not (eql (aref routing-table-type subnet) 'direct))
		      1;;this could really be clever and try to save away the info somewhere*
		      (let ((bridge (aref routing-table subnet)))
			(cond
			  ((and bridge (numberp bridge) (not (zerop bridge)))
			   (or (cdr (assoc bridge subnet-name-cache :test #'equal))
			       (let ((bridge-name (host-data bridge)))
				 (push (cons bridge bridge-name) subnet-name-cache)
				 bridge-name)))
			  (t 3"No Connection"*))))))
	(subseq string 0 (min 22 (length string))))))

;(DEFUN PEEK-CHAOS-SUBNET-NAME (SUBNET);update for new Routing-Table, RAF.
;  (LET ((STRING
;	 (FORMAT () "~:[Direct~;~A~]" (NOT (MEMBER SUBNET MY-SUBNETS :TEST #'EQUAL))
;				      ;;this could really be clever and try to save away the info somewhere
;		 (LET ((BRIDGE (AREF ROUTING-TABLE SUBNET)))
;		   (COND
;		     ((AND BRIDGE (NUMBERP BRIDGE) (NOT (ZEROP BRIDGE))) (HOST-DATA BRIDGE))
;		     (T "No Connection"))))))
;    (SUBSEQ STRING 0 (MIN 18 (LENGTH STRING))))) 


1;;; *BJ* For *RWF**
(defun 4peek-chaos-routing-cost* (subnet)		1;update for new Routing-Table, RAF.*
  (if (null subnet)
      3""*
      (format () 3"~:[~4D.~]"*
	      (equal (AREF ROUTING-TABLE-TYPE SUBNET) 'direct)
	      (aref routing-table-cost subnet))))

1;(DEFUN PEEK-CHAOS-ROUTING-COST (SUBNET);update for new Routing-Table, RAF.
;  (FORMAT () "~:[~4D.~]" (AND (MEMBER SUBNET MY-SUBNETS :TEST #'EQUAL) (NUMBERP SUBNET))
;*	1  (AREF ROUTING-TABLE-COST SUBNET))) 




;; NEW SERVER CONNECTION MENU -- LS -- 10/16/86 -- GENERIC PEEK NETWORK*

(defun 4chaos-peek-server-connection-menu* (conn item)
  (let ((*terminal-io* (send tv:selected-window :typeout-window)))
    (let ((choice
	    (W:menu-choose
	      '((3"Close"* :value :close :documentation 3"Close connection forcibly."*)
		(3"Insert Detail"* :value :detail :documentation
		 3"Insert detailed info about network connection."*)
		(3"Remove Detail"* :value :undetail :documentation
		 3"Remove detailed info from Peek display."*)
		(3"Describe"* :value describe :documentation 3"Describe the connection."*)
		(3"Inspect"* :value inspect :documentation 3"Inspect the connection."*))
	      :label (format () 3"~S"* conn))))
      (case choice
	    (:close (send conn :close 3"Manual Close from PEEK"*))
	    ((describe inspect) (send tv:selected-window :force-kbd-input `(,choice ,conn)))
	    (:detail (store-array-leader conn item (+ 4 tv:scroll-item-leader-offset))
		     (store-array-leader t item (+ 5 tv:scroll-item-leader-offset)))
	    (:undetail (store-array-leader () item (+ 4 tv:scroll-item-leader-offset))
		       (store-array-leader () item (+ 5 tv:scroll-item-leader-offset)))))))


;1; USED AS A PRE-PROCESS FUNCTION BY PEEK-CHAOS-CONN*
(defun 4peek-chaos-conn-insert-hostat* (item &aux host)
  2"A pre-process function to insertremove a host-status from the display."*
  (cond
    ((array-leader (first (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset)
     1;; Want a host-status, make sure it's there and for the right host*
     (if (and
       (eq
	(setq host
	      (array-leader (first (tv::scroll-item-component-items item))
			    (1+ tv:scroll-item-leader-offset)))
	(array-leader (first (tv::scroll-item-component-items item))
		      (+ tv:scroll-item-leader-offset 2)))
       (cddr item))
       ()
       (progn
	 (rplacd (cdr item) (net:peek-insert-host-status host 1))
	 (setf (array-leader (first (tv::scroll-item-component-items item))
			     (+ tv:scroll-item-leader-offset 2))
	       host))))
    (t (rplacd (cdr item) ())
     (setf (array-leader (first (tv::scroll-item-component-items item))
			 (+ tv:scroll-item-leader-offset 2)) ())))) 


;1; USED AS A PRE-PROCESS FUNCTION BY* 4PEEK-CHAOS-PACKET-ITEM*
(defun 4peek-chaos-packet-insert-1hostat** (item &aux host si)
  2"A pre-process function to insertremove a host-status from the display."*
  (cond
    ((array-leader (setq si (first (tv::scroll-item-component-items item)))
		   tv:scroll-item-leader-offset)
     1;; Want a host-status, make sure it's there and for the right host*
     (if (and
       (eq (setq host (array-leader si (1+ tv:scroll-item-leader-offset)))
	   (array-leader si (+ tv:scroll-item-leader-offset 3)))
       (cddr item))
       ()
       (progn
	 (rplacd (cdr item)
		 (net:peek-insert-host-status
		   host
		   (1+ (array-leader si (+ tv:scroll-item-leader-offset 2)))))
	 (setf (array-leader si (+ tv:scroll-item-leader-offset 3)) host))))
    (t (rplacd (cdr item) ())
       (setf (array-leader si (+ tv:scroll-item-leader-offset 3)) ())))) 



(defun 4peek-chaos-conn-received-pkts* (item &optional (indent 0) &aux conn)
  2"Showunshow the received pkts of the connection"*
  (or (setq conn (getf (tv::scroll-item-plist item) :connection))
     (ferror () 3"~S has no associated connection, can't display packets."* item))
  (cond
    ((not (array-leader (first (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset))
     1;; Want to leave state alone*
     )
    ((cdr (tv::scroll-item-component-items item))
     1;; Remove display*
     (rplacd (tv::scroll-item-component-items item) ()))
    (t
     1;; Add display*
     (rplacd (tv::scroll-item-component-items item)
	     (cons
	       (tv:scroll-maintain-list `(lambda ()
					   (read-pkts ',conn))
					`(lambda (x)
					   (peek-chaos-packet-item x ,(+ indent 2)))
					()
					#'(lambda (state)
					    (block stepper-block (return-from stepper-block    ;; hacked-on - LS
								   (values state (pkt-link state)
									   (null (pkt-link state))))
						   ())))

					
	       ()))))
  (setf (array-leader (first (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ())) 



(defun 4peek-chaos-conn-send-pkts* (item &optional (indent 0) &aux conn)
  2"Show*\/2unshow the send pkts of the connection"*
  (or (setq conn (getf (tv::scroll-item-plist item) :connection))
     (ferror () 3"~S has no associated connection, can't display packets."* item))
  (cond
    ((not (array-leader (first (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset))
     1;; Want to leave state alone*
     )
    ((cdr (tv::scroll-item-component-items item))
     1;; Remove display*
     (rplacd (tv::scroll-item-component-items item) ()))
    (t
     1;; Add display*
     (rplacd (tv::scroll-item-component-items item)
	     (cons
	      (tv:scroll-maintain-list `(lambda ()
					  (send-pkts ',conn))
				       `(lambda (x)
					  (peek-chaos-packet-item x ,(+ indent 2)))
				       ()
				       #'(lambda (state)
					   (block stepper-block              ;; hacked-on - ls
					     (return-from stepper-block
					       (values state (pkt-link state)
						       (null (pkt-link state))))
					     ())))
	      ()))))
  (setf (array-leader (first (tv::scroll-item-component-items item)) tv:scroll-item-leader-offset) ()))



;
;1;;-----------------------------------------------------------------------------*
;1;;                              PEEK HOSTAT support functions*
;1;;-----------------------------------------------------------------------------*
;; moved (with changes) to SYS:NETWORK-SUPPORT;PEEKN.LISP - LS 2/25/87

;(defvar 4*peek-hostat-list*)* 
;(defvar 4*peek-hostat-string*)* 
;(defvar 4*peek-hostat-indent*)* 


;(defun 4peek-chaos-hostat* (host *peek-hostat-indent* &optional pkt &aux (*peek-hostat-list* nil)
;  (*peek-hostat-string* nil))
;  (cond
;    ((or pkt (setq pkt (get-host-status-packet host))) (peek-hostat-stream :tyo #\Newline)
;     (hostat-heading 'peek-hostat-stream)
;     (hostat-format-ans (pkt-source-address pkt) pkt 'peek-hostat-stream)
;     1;; Parse the strings into scroll items, removing any blank lines*
;     (setq *peek-hostat-list* (nreverse *peek-hostat-list*))
;     (do ((l *peek-hostat-list* (cdr l)))
;	 ((null l)
;	  (list* () *peek-hostat-list*))
;       (if (position #\Space (the string (string (car l))) :test-not #'char-equal)
;	 (rplaca l (tv:scroll-parse-item (car l)))
;	 (setq *peek-hostat-list* (delete (car l) (the list *peek-hostat-list*) :test #'eq)))))
;    (t (cons (tv:scroll-parse-item 3"Host data unavailable"*) ())))) 



;(defun 4peek-hostat-stream* (op &optional arg1 &rest rest)
;  (case op
;    (:which-operations '(:tyo :read-cursorpos :set-cursorpos))
;    (:tyo
;     (cond
;       ((= arg1 #\Newline)
;	(and *peek-hostat-string* (push *peek-hostat-string* *peek-hostat-list*))
;	(setq *peek-hostat-string* (make-array 50 :element-type 'string-char :leader-list '(0)))
;	(peek-hostat-stream :set-cursorpos *peek-hostat-indent*))
;       (t (vector-push-extend arg1 *peek-hostat-string*))))
;    (:read-cursorpos (length *peek-hostat-string*))
;    (:set-cursorpos
;     (let ((spaces (- arg1 (length *peek-hostat-string*))))
;       (and (> spaces 0) (dotimes (i spaces)
;			   (peek-hostat-stream :tyo #\Space)))))
;    (t (stream-default-handler 'peek-hostat-stream op arg1 rest)))) 
