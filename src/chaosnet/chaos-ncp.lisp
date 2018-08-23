;;  -*- Mode:COMMON-LISP; Package: CHAOS; BASE:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (c) 1980 Massachusetts Institute of Technology
;;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved.
;;
;; NOTE - All functions with the comment ";TI" were written at TI, and the MIT copyright
;;        does not apply to them.  Some of them bear the same name as the corresponding
;;        function written by MIT, but are different functions.  All other functions were
;;        written in whole or part at MIT, and some have been modified by TI.

;;------------------------------------------------------------------------------
;;                 ChaosNet protocols for the Lisp Machine
;;------------------------------------------------------------------------------
;;

;; Protocol of May, 1978

;; This file contains the CHAOS net software which implements the sequenced packet
;; protocol of Chaos.  It does not include most of the user interface, the routing,
;; or device interfaces.

;;  10/15/86 LS - Changed CONN Named-structure-invoke to include some methods to accomodate
;;                        generic Peek network access.
;;  4/07/86 MMG - Moved definitions to CHAOS_DEFS.  Moved menu stuff to MENUS.LISP
;;  3/19/86 MMG - Common Lisp conversion.
;;  1/17/86 RWF - Modify REMOVE-CONN so that RESET does not put conns on FREE-CONN-LIST.
;;  1/15/86 MMG - Fix RETRANSMISSION's sending of bogus window sizes in RFC pkts (don't ack RFC's).
;;  8/22/85 RAF - Fix SPR 265. PRINT-STS-WHY now formats so you can tell where the last item is.
;;  4/30/85 RAF - Fix race condition between foreground & Chaos:BACKGROUND to RESET-ROUTING-TABLE. Net-1.6
;;  3/13/85 RAF - Remove CREATE-CONTROLLER-LIST from Chaos Init.  It is already done.
;;  3/08/85 RAF - Documentation changes.
;;  2/21/85 RAF - Remove old debugging aids.  They were moved to Chaos-Debug.
;;  1/16/85 RAF - Modify for HAL build.
;; 12/11/84 RAF - Integrate working Flavor Controller with system.  HALT to Netdefs.
;; 11/27/84 RAF - Changes for Flavor based controllers, HALT, and moving PEEK to NET:.
;; 11/20/84 RAF - Fix MAKE-CONNECTION to discard any "old", short CONN's.
;; 11/19/84 RAF - Copy in Routing changes to use MY-ADDRESSES & MY-SUBNETS.
;; 10/25/84 RAF - Fix ALLOCATE-INT-PKT to clear Version field to prevent discarding pkts
;;                because the Version no. is not zero.
;; 10/05/84 RAF - Add functions to NET: menus
;; 10/01/84 RAF - Increase INT-PKT size to max for Ethernet.
;;  9/21/84 RAF - Move TRANSMIT-BRD-PKT from Chaos-User.
;;  9/18/84 RAF - Reorganize parts of file.  General cleanup of code.  Remove extraneous
;;                 HOST-DOWN conns.  Free up Pending-RFC's at RESET.
;;  8/01/84 RAF on LAM5  - Add NET-XXX-INIT-LISTs.
;;  7/27/84 RAF on CADR3 - Restore INT-PKTs to this file, since all modules use them
;;  7/19/84 RAF on CADR6 - Add Enable/Disable Lists (Functions to call).
;;  7/13/84 RAF on LAM5  - Add debug stuff from CHSAUX.
;;  7/10/84 RAF on BAMBI - Fix up initializations. (I hope!!)
;;  5/31/84 RAF on LAM5  - Removed the Chaos board driver & routing.



;;-----------------------------------------------------------------------------
;;                     Definitions for high-level structures
;;-----------------------------------------------------------------------------
;;
;; with functions to print them textually and various macros for the specialized
;; formulas of the protocol (Everything here is used everywhere).

;;-----------------------------------------------------------------------------
;;                                      CONN
;;
;;  REMOVE-CONN
;;  MAY-TRANSMIT
;;  FINISH-CONN
;;  GET-NEXT-PKT
;;  DATA-AVAILABLE
;;  INTERRUPT-FUNCTION

;;-----------------------------------------------------------------------------
;;                                 PKT & INT-PKT
;;
;;  FIRST-DATA-WORD-IN-PKT
;;  SET-PKT-STRING
;;  GET-PKT
;;  RETURN-PKT
;;  SEND-PKT
;;  SEND-STRING
;;  SEND-UNC-PKT
;;  PKT-LINK
;;  PRINT-ALL-PKTS

;;-----------------------------------------------------------------------------
;;                            Control And Maintainence
;;
;;  RESET
;;  ASSURE-ENABLED
;;  ENABLE
;;  DISABLE


;; Some standard abbreviations and mnemonic indicators:
;; Items in this list with no "-" at either end signify abbreviations
;;	which may occur in any context, those with one or more "-"'s only occur
;;	in a context in which there are "-"'s in exactly those places.
;; PKT		Abbreviation for PACKET.  see the DEFSTRUCT
;; CONN		Abbreviation for CONNECTION.  see the DEFSTRUCT
;; SEND-       	Cause a packet to be "sent" with retry etc. if applicable.
;; TRANSMIT-	Cause a packet to be "transmitted" (i.e. placed on the net).
;;		 This is done by all the SEND- routines by calling an
;;		 appropriate TRANSMIT- routine to put the packet a list of
;;		 packets to be put on the net.
;; -STATE	A symbol representing a state that a connection might be in.
;; -OP		A SPECIAL whose value represents that of a packet op code.



(defparameter *buffer-size-in-words* 750 "size of buffers in 16b words")    ;ti
;; 750 words (1500. bytes) holds the maximum Ethernet data size.  128. was for
;; "Max 32-bit words in pkt including hardware words" for Chaos hardware.

(defvar sts-why-array (make-array 64 :leader-list '(64 0) :area chaos-area))

;;; Random servers are not likely to work during the cold load, leave them turned off until
;;; the first disk save.

(defvar chaos-servers-enabled () "t enables rfcs to create server processes.")

;;; Adds a new background task to the queue:  these tasks are ORDERED on a fifo bases

(defmacro background-task (task)
  `(without-interrupts (push ,task *background-requests*)))




;;; int-pkt management routines

;; Right now, every one uses INT-PKTs, including routines in CHSNCP.

(defvar *int-pkt-list* () "list of all int-pkts")

(defvar debug-int-pkts ())


(defvar int-pkts-allocated 0 "number of pkts ever created.")

(pushnew
  '((symbol-value 'int-pkts-allocated) "number of chaos int pkts currently allocated")
  net::peek-a-boo-list :test #'equal)


;;; *BJ* For *RWF*
(defun free-int-pkt (int-pkt)
  (or (not debug-int-pkts) (not (is-free-int-pkt int-pkt))
      (ferror 'net::local-network-error-1
	      "int-pkt is already free at beginning of free-int-pkt ~a" int-pkt))
  (or (not debug-int-pkts) (not (free-list-cyclic))
      (ferror 'net::local-network-error-1 "free list cyclic at beginning of free-int-pkt ~a"
	      int-pkt))
  (or (= (%area-number int-pkt) chaos-buffer-area)
      (ferror 'net::local-network-error-1 "attempt to free non-interrupt packet ~a" int-pkt))
  (prog (old-free-list)
     loop
	(setq old-free-list (int-free-list))
	(setf (int-pkt-thread int-pkt) old-free-list)
	(or (%store-conditional int-free-list-pointer old-free-list int-pkt) (go loop))
	;;(%chaos-wakeup)
	)
  (decf int-pkts-allocated)
  (or (not debug-int-pkts) (not (free-list-cyclic))
      (ferror 'net::local-network-error-1 "free list cyclic at end of free-int-pkt ~a" int-pkt)))

(defmacro convert-to-pkt (int-pkt &optional (free-pkt-flag t))
  "allocates a new packet, copies the int-pkt to it, and then
   deallocates the int-pkt"
  `(let ((pkt (allocate-pkt))
	 (int-pkt-internal ,int-pkt)
	 nw)
     (setq nw (pkt-nwords int-pkt-internal))
     (without-interrupts
      (%blt
       (%make-pointer-offset dtp-fix int-pkt-internal (si:array-data-offset int-pkt-internal))
       (%make-pointer-offset dtp-fix pkt (si:array-data-offset pkt))
       (ceiling nw 2) 1))
     (store-array-leader (pkt-nbytes int-pkt-internal)
			 (pkt-string pkt) 0)
     (and ,free-pkt-flag (free-int-pkt int-pkt-internal))
     pkt))


;; *bj* for *rwf*
(defun allocate-int-pkt (&optional (wait-if-necessary t) &aux int-pkt free-list (was-reserved reserved-int-pkt))
  (or (not debug-int-pkts) (not (free-list-cyclic))
      (ferror 'net::local-network-error-1 "free list cyclic at beginning of allocate-int-pkt ~a"
	      int-pkt))
  (cond
    ((null reserved-int-pkt)
     (do ()
	 (nil)
       (setq free-list (int-free-list))
       (cond
	 ((null free-list)
	  (if wait-if-necessary
	      (process-wait "chaos buffer" #'(lambda ()
					       (int-free-list)))
	      (return ())))
	 ((%store-conditional int-free-list-pointer free-list (int-pkt-thread free-list))
	  (return (setq int-pkt free-list))))))
    ;; no without-interrupts needed here because reserved-int-pkt is non-null
    ;; only in a receiver, and each receiver binds a unique pkt to it.
    (t (setq int-pkt reserved-int-pkt
	     reserved-int-pkt ())))
  (when int-pkt
    (setf (int-pkt-thread int-pkt) ())
    (setf (pkt-opcode-left-justified int-pkt) 0)
    (setf (pkt-fwd-count int-pkt) 0)		;rwf 08/12/86
    (unless was-reserved
      (incf int-pkts-allocated)))
  (or (not debug-int-pkts) (not (is-free-int-pkt int-pkt))
      (ferror 'net::local-network-error-1
	      "allocated pkt is still free at end of allocate-int-pkt ~a" int-pkt))
  (or (not debug-int-pkts) (not (free-list-cyclic))
      (ferror 'net::local-network-error-1 "free list cyclic at end of allocate-int-pkt ~a"
	      int-pkt))
  int-pkt)

(defun count-free-pkts (&aux (limit 23420))
  (ignore-errors
    (do ((n 0 (1+ n))
	 (pkt free-pkts (pkt-link pkt)))
	((or (null pkt) (> n limit))
	 n))))


(pushnew '((funcall 'count-free-pkts) "number of non-int chaos pkts actually free")
		     net::peek-a-boo-list :test #'equal)


(defun count-free-int-pkts (&aux (free 0))
  (dolist (pkt *int-pkt-list*)
    (when (is-free-int-pkt pkt)
      (incf free)))
  free)


(pushnew '((funcall 'count-free-int-pkts) "number of chaos int pkts actually free")
		     net::peek-a-boo-list :test #'equal)


(defun count-inuse-int-pkts (&aux (inuse 0))
  (dolist (pkt *int-pkt-list*)
    (unless (or (int-pkt-thread pkt) (is-free-int-pkt pkt))
      (incf inuse)))
  inuse)


(pushnew
  '((funcall 'count-inuse-int-pkts) "number of chaos int pkts actually in use")
  net::peek-a-boo-list :test #'equal)


(defun count-int-pkts ()
  (values (count-free-int-pkts) (count-inuse-int-pkts)))

(defun print-int-pkt-thread (&optional (thread (int-free-list)) (limit 100))
  (without-interrupts
    (do ((n 0 (1+ n))
	 (pkt thread (int-pkt-thread pkt)))
	((or (null pkt) (> n limit))
	 n)
      (print (list pkt (int-pkt-thread pkt))))))

(defun is-free-int-pkt (packet &optional (thread (int-free-list)) (limit 144))
  (without-interrupts				;   (or (int-pkt-thread packet)
    (do ((n 0 (1+ n))
	 (pkt thread (int-pkt-thread pkt)))
	((or (null pkt) (> n limit) (eq pkt packet)) (eq pkt packet)))))

(defun free-list-cyclic (&optional (thread (int-free-list)) (limit 144))
  (without-interrupts
    (do ((n 0 (1+ n))
	 (pkt thread (int-pkt-thread pkt)))
	((or (null pkt) (> n limit))
	 (> n limit)))				;or
    ))

(defun convert-to-int-pkt (pkt &aux int-pkt nw)
  (setq int-pkt (allocate-int-pkt))
  (setq nw (pkt-nwords pkt))
  (without-interrupts
   (%blt (%make-pointer-offset dtp-fix pkt (si:array-data-offset pkt))
	 (%make-pointer-offset dtp-fix int-pkt
			       (si:array-data-offset int-pkt))
	 (ceiling nw 2) 1))
  (setf (int-pkt-word-count int-pkt) (+ 3 nw));this is probably superfluous
  int-pkt)



;; for now, doesn't worry about changing number of buffers.  if called more than
;; once, will discard all old buffers.  you better not try to increase the number
;; of buffers, though.

;; *bj* for *rwf*
(defun create-chaosnet-buffers (n-buffers)
  (cond
    ((not (boundp 'chaos-buffer-area))
     (make-area :name 'chaos-buffer-area :size
		(*  (ceiling (* n-buffers
				(+ 3		;leader header, leader length, array header
				   (ceiling *buffer-size-in-words* 2)
				   (length chaos-buffer-leader-qs)))
			     page-size)
		    page-size)
		:gc :temporary :force-temporary t))	   ;ab 1/29/88
    (t (reset-temporary-area chaos-buffer-area)))
  (setq reserved-int-pkt ())
  (setq *int-pkt-list* ())
  (do ((prev nil buf)
       (buf)
       (count n-buffers (1- count)))
      ((zerop count)
       (si:wire-area chaos-buffer-area t)
       (setf (system-communication-area %sys-com-chaos-free-list) buf)
       (setf (system-communication-area %sys-com-chaos-transmit-list) ())
       (setf (system-communication-area %sys-com-chaos-receive-list) ()))
    (setq buf
	  (make-array *buffer-size-in-words* :element-type '(unsigned-byte 16) :area
		      chaos-buffer-area :leader-length (length chaos-buffer-leader-qs)))
    (push buf *int-pkt-list*)
    (store-array-leader 0 buf %chaos-leader-word-count)
    (store-array-leader prev buf %chaos-leader-thread))
  (setf int-pkts-allocated 0))

;;------------------------------------------------------------------------------
;;                      initialize microcode pointers
;;------------------------------------------------------------------------------


(setq int-free-list-pointer
      (locf (aref #'system-communication-area %sys-com-chaos-free-list))
      int-receive-list-pointer
      (locf (aref #'system-communication-area %sys-com-chaos-receive-list))
      int-transmit-list-pointer
      (locf (aref #'system-communication-area %sys-com-chaos-transmit-list)))





;;------------------------------------------------------------------------------
;;                             pkt management.
;;------------------------------------------------------------------------------

;; creates a new pkt.  only allocates the storage, doesn't initialize anything.
;; this should only be called by allocate and with interrupts inhibited
;; make sure it doesnt happen in a temporary area.
;;

(defun make-pkt (&aux pkt (default-cons-area background-cons-area))
  (setq pkt
	(make-array max-words-per-pkt
		    :element-type '(unsigned-byte 16)
		    :initial-element 0                    ; 4/86 mmg
		    :area chaos-area
		    :leader-length pkt-leader-size
		    :named-structure-symbol 'pkt))
  (setf
    (pkt-string pkt);create indirect array to reference as a string
	       (make-array max-data-bytes-per-pkt
		    :element-type 'string-char
		    :area chaos-area
		    :leader-list '(0)
		    :displaced-to pkt
		    :displaced-index-offset 16))
  (setf (pkt-made-link pkt) made-pkts)
  (setq made-pkts pkt)
  pkt)


(defun allocate-pkt (&aux pkt)
  "allocate, initialize and return a packet, reusing one if possible."
  (without-interrupts
   (setq pkt
	 (cond
	   (free-pkts (prog1
			free-pkts
			(setq free-pkts (pkt-link free-pkts))))
	   ((and (> current-los-pkt-count max-los-pkts-to-keep)
		 los-pkts)
	    (prog1
	      los-pkts
	      (setq los-pkts (pkt-link los-pkts))
	      (setq current-los-pkt-count (1- current-los-pkt-count))))
	   (t (setq pkts-made (1+ pkts-made))
	      (make-pkt))))
   (setf (pkt-time-transmitted pkt) 0)
   (setf (pkt-times-transmitted pkt) 0)
   (store-array-leader 0 (pkt-string pkt) 0)
   (setf (pkt-link pkt) t)
   (setf (pkt-opcode-left-justified pkt) 0)
   (setf (pkt-nbytes pkt) 0)
   (setf (pkt-fwd-count pkt) 0) pkt))


(defun free-pkt (pkt)
  "release the packet pkt so that allocate-pkt can reuse it.
   it is ok to call this while pkt is still awaiting transmission
   at interrupt level; it will not really be reused until it has been sent.
   note: this is for internal use by the chaosnet ncp only.
   user programs should use return-pkt to free packets obtained with
   get-pkt or get-next-pkt."
  (without-interrupts
   (cond
     ((null (pkt-being-retransmitted pkt))
      (setf (pkt-link pkt) free-pkts)
      (setq free-pkts pkt))
     (t
      (setf (pkt-being-retransmitted pkt) 'free)))))


(defun set-pkt-string (pkt string &rest other-strings &aux len)
  "store data into packet pkt from string and other-strings concatenated.
   the pkt-nbytes field is updated."
  (copy-array-portion (setq string (the string (string string))) 0 (array-active-length string)
		      (pkt-string pkt) 0 (array-active-length string))
  (setq len (array-active-length string))
  (cond
    (other-strings
     (do ((strings other-strings (cdr strings))
	  (pkt-string (pkt-string pkt)))
	 ((or (null strings) (>= len max-data-bytes-per-pkt)))
       (do ((idx 0 (1+ idx))
	    (str (string (car strings)))
	    (str-len (length (string (car strings)))))
	   ((or (>= idx str-len) (>= len max-data-bytes-per-pkt)))
	 (setf (aref pkt-string len) (aref str idx))
	 (setq len (1+ len))))))
  (setq len (min max-data-bytes-per-pkt len))
  (setf (pkt-nbytes pkt) len)
  (store-array-leader len (pkt-string pkt) 0))



;;------------------------------------------------------------------------------
;;                            conn management.
;;------------------------------------------------------------------------------

;;; create a connection.  returns the connection.

(defun make-connection (&optional conn &aux cons)
  (without-interrupts

   (do ()           ; all this hair is to ensure that any
       ((and conn   ; surviving short "old" conns are discarded.
	   (= (array-total-size conn) (array-total-size prototype-conn))))
     (cond
       ((setq cons free-conn-list)              ;recycle one
	(setq free-conn-list (cdr cons)
	      conn (car cons))
	(copy-array-contents prototype-conn conn))
       ((setq conn (make-conn))))))

  (cond
    ((neq (state conn) 'inactive-state)
     (ferror () "attempt to reuse ~s, which is in the ~a, not inactive-state" conn (state conn)))
    ((member conn free-conn-list :test #'eq)
     (ferror () "attempt to reuse ~s, which was on free-conn-list twice (now only once)." conn))
    ((member conn conn-list :test #'eq)
     (ferror () "attempt to reuse ~s, which is already in use." conn))
    (t
     (do ((fp
	    (rem (1+ index-conn-free-pointer) maximum-index)
	    (rem (1+ fp) maximum-index))
	  (counter maximum-index (1- counter)))
	 ((%store-conditional (locf (ar-1 index-conn (cond
						       ((= fp 0) (setq fp 1))
						       (t fp)))) ()
			      conn)
	  (setq index-conn-free-pointer fp)
	  (setf (local-index-num conn)
		(dpb
		  (setf (aref uniquizer-table fp) (1+ (aref uniquizer-table fp)))
		  (dpb maximum-index-log-2-minus-1 (byte 6 6) (- 16 maximum-index-log-2-minus-1))
		  fp))
	  (without-interrupts (setq conn-list (rplacd (or cons (cons conn ())) conn-list)))
	  conn)
       (when (minusp counter)
	 (ferror 'network-resources-exhausted "connection table full"))))))


;; *bj* for *rwf*
(defvar removed-conn-list () "list of free but unreuseable conn structures (save consing).")



(defun remove-conn (conn &optional (free-conn t))
  "remove connection-object conn from the connection tables and free its packets."
  (without-interrupts (free-all-read-pkts conn) (free-all-received-pkts conn)
		      (free-all-send-pkts conn) (setf (state conn) 'inactive-state)
		      (setf (aref index-conn (ldb maximum-index-log-2-minus-1 (local-index-num conn))) ())
		      (setq distinguished-port-conn-table
			    (delete (rassoc conn distinguished-port-conn-table :test #'eq)
				    (the list distinguished-port-conn-table) :test #'eq))
		      (let ((cons (member conn conn-list :test #'eq)))
			(setq conn-list (delete conn (the list conn-list) :test #'eq))
			(if free-conn
			    (unless (member conn free-conn-list :test #'eq)	;05/86 rwf newly freed conns go to end of free list
						;to make it more likely that dual conn allocation
						;will be caught as use of a conn in an inactive state
			      (setq free-conn-list (nconc free-conn-list (rplacd (or cons (cons conn ())) ()))))
			    (unless (member conn removed-conn-list :test #'eq)	;05/86 rwf newly removedd conns go to end of removed list
						;to make it more likely that dual conn allocation
						;will be caught as use of a conn in an inactive state
			      (setq removed-conn-list
				    (nconc removed-conn-list (rplacd (or cons (cons conn ())) ()))))))
		      (dolist (x pending-listens)
			(and (eq (cdr x) conn)
			     (setq pending-listens (delete x (the list pending-listens) :test #'eq))))
		      ()))

;must be called with interrupts off.

(defun free-all-read-pkts (conn)
  (do ((pkt (read-pkts conn) (pkt-link pkt))
       (prev nil pkt))
      (nil)
    (and prev (free-pkt prev))
    (or pkt (return ())))
  (setf (read-pkts conn) ())
  (setf (read-pkts-last conn) ()))

;must be called with interrupts off.

(defun free-all-received-pkts (conn)
  (do ((pkt (received-pkts conn) (pkt-link pkt))
       (prev nil pkt))
      (nil)
    (when prev
      (setf (pkt-status prev) ())   ;12-08-87 dab
      (free-pkt prev))
    (or pkt (return ())))
  (setf (received-pkts conn) ()))

;must be called with interrupts off.

(defun free-all-send-pkts (conn)
  (do ((pkt (send-pkts conn) (pkt-link pkt))
       (prev nil pkt))
      (nil)
    (when prev
      (setf (pkt-status prev) ())  ;12-08-87 dab
      (free-pkt prev));this offseting so it doesnt rely on pkt-link of
    (or pkt (return ()))); a pkt it has freed.
  (setf (send-pkts conn) ())
  (setf (send-pkts-last conn) ())
  (setf (send-pkts-length conn) 0))


; causes the conn's interrupt-function to be run in the background process with the
; specified reason and arguments
; reasons are:
;	:input			input has arrived
;	:output			the window, which was full, now has room in it
;	:change-of-state	the state of the connection has just changed

(defun interrupt-conn (reason conn &rest args
		       &aux (ifun (interrupt-function conn)))
  (when ifun
       (background-task
	 `(interrupt-conn-internal ',ifun ',reason ',conn ',(append args ())))))

;if while the request was on the queue, the connection was flushed, get rid
;of the interrupt.  because of connection reusing, this is somewhat heuristic.

(defun interrupt-conn-internal (ifun reason conn args)
  (or (eq (state conn) 'inactive-state)
      (neq (interrupt-function conn) ifun)
      (apply ifun reason conn args)))


(defun (:property conn named-structure-invoke) (op &optional x &rest args)
  (let ((stream (or (car args) *standard-output*)))
    (case op
      ;; generic datagram methods
      (:get-next-pkt (let ((input-packet (apply 'get-next-pkt x args)))
		       (when input-packet (values input-packet (pkt-string input-packet)))))
      (:packet-present-p (or (eq (state x) 'answered-state) (read-pkts x)))
      (:return-input-pkt (apply 'return-pkt args))
      (:get-empty-pkt (let ((output-packet (get-pkt)))
			(values output-packet (pkt-string output-packet))))
      (:send-pkt (let ((output-packet (first args))
		       (nbytes (second args)))
		   (setf (pkt-nbytes output-packet) nbytes)
		   (send-pkt x output-packet)))
      (:return-output-pkt (apply 'return-pkt args))
      (:answer (let ((output-packet (first args))
		     (nbytes (second args)))
		 (setf (pkt-nbytes output-packet) nbytes)
		 (answer x output-packet)))
      (:reject (apply 'reject x (cond (args) (""))) (remove-conn x))
      ((:close-connection :return-connection) (apply 'close-conn x args) (remove-conn x))
      ;; added for generic peek access
      (:close (apply 'close-conn x args))
      (:name (host-short-name (foreign-address x)))
      (:host-object (si:get-host-from-address (foreign-address x) :chaos nil))
      (:peek-host-menu (apply 'net:peek-host-menu args))
      (:peek-server-connection-menu
       (apply #'process-run-function "peek server menu" 'net:short-server-object-menu x args))
      (:server-current-p (or (eq (state x) 'open-state) (eq (state x) 'rfc-received-state)
			     (eq (state x) 'answered-state)))
      ;;
      (:which-operations '(:get-next-pkt :packet-present-p :return-input-pkt :get-empty-pkt
					 :send-pkt :return-output-pkt :answer :reject
					 :close-connection :return-connection
					 :close :name :host-object :peek-host-menu
					 :peek-server-connection-menu :server-current-p
					 :which-operations :describe :send-if-handles :operation-handled-p
					 :print-self))
      (:describe (print-conn x) (describe-defstruct x 'conn))
      (:send-if-handles (when (send x :operation-handled-p (first args)) (lexpr-send x args)))
      (:operation-handled-p (member (first args) (send x :which-operations)))
      (:print-self
       (printing-random-object
	 (x stream)
	 (princ "chaos connection" stream)
	 (let ((fhost (si:get-host-from-address (foreign-address x) :chaos))
	       contact)
	   (cond
	     ((setq contact (getf (conn-plist x) 'rfc-contact-name))
	      (format stream " to ~a ~a" fhost contact))
	     ((setq contact (getf (conn-plist x) 'server-contact-name))
	      (format stream " from ~a to ~a server" fhost contact))))))
      (otherwise (ferror () "i have never heard of ~s" op)))))

;;; if given a number, this always returns something that address-parse would make into that
;;; number.

(defun host-short-name (host &aux host1)
  "return a brief name for the specified host."
  (cond ((not (numberp host))
	 (send host :short-name))
	((setq host1 (si:get-host-from-address host :chaos))
	 (send host1 :short-name))
	(t
	 (send (si:make-unnamed-host :default `(:chaos ,(list host))) :short-name)
	 )))


;;; High level transmission routines -- cause a packet to be queued
;;; for transmission.

;; Put the pkt on the transmit list, and create a phony transmitter
;; interrupt if needed so that the interrupt level will start sending.
;; If the second arg is t, put an ack aboard this pkt.  This is a very
;; low level function, called mainly by the following functions.
;; (Also called by the retransmitter and forwarder)
(defun transmit-pkt (pkt &optional ack-p)
  (when (> (pkt-nbytes pkt) max-data-bytes-per-pkt)
    (cerror (concatenate 'string "attempt to transmit an invalid packet (~s).~@"
			 "the length ~o is greater than the maximum packet size (~o)."
			 pkt (pkt-nbytes pkt) max-data-bytes-per-pkt)))
  (when ack-p
    (lock-transmit-queue
      (let ((conn (pkt-source-conn pkt)))
	(when (null conn)
	  (ferror () "~s has null connection." pkt))
	(let ((ackn (pkt-num-read conn)))
	  (setf (pkt-ack-num pkt) ackn)
	  (setf (pkt-num-acked conn) ackn)))))
  (setf (pkt-time-transmitted pkt) (time))
  (incf (pkt-times-transmitted pkt))
  (transmit-pkt-route (convert-to-int-pkt pkt)))




;; Send a normal pkt (i.e., not los nor data). Caller must allocate
;; the pkt, and fill in the opcode, nbytes, and data parts. The pkt
;; and ack-pkt numbers to place in the packet are optional arguments,
;; they default to 0. If t is provided for either, the usual thing
;; happens.
(defun transmit-normal-pkt (conn pkt &optional (pktn 0) (ack-pktn 0) &aux ack-p)
  (when (eq pktn t)
    (setq pktn (pktnum-1+ (pkt-num-sent conn)))
    (setf (pkt-num-sent conn) pktn))
  (setf (pkt-num pkt) pktn)
  (cond
    ((eq ack-pktn t) (setq ack-p t))
    (t (setf (pkt-ack-num pkt) ack-pktn)))
  (setf (pkt-source-address pkt) (conn-local-address conn))
  (setf (pkt-source-index-num pkt) (local-index-num conn))
  (setf (pkt-dest-address pkt) (foreign-address conn))
  (setf (pkt-dest-index-num pkt) (ldb (byte 16 0) (foreign-index-num conn)))
  (transmit-pkt pkt ack-p))


(defun transmit-int-pkt-for-conn (conn pkt)
  (setf (pkt-source-address pkt) (conn-local-address conn))
  (setf (pkt-source-index-num pkt) (local-index-num conn))
  (setf (pkt-dest-address pkt) (foreign-address conn))
  (setf (pkt-dest-index-num pkt) (ldb (byte 16 0) (foreign-index-num conn)))
  (without-interrupts
   (let ((ackn (pkt-num-read conn)))
     (setf (pkt-ack-num pkt) ackn)
     (setf (pkt-num-acked conn) ackn)))
  (transmit-pkt-route pkt))

;;; given a losing pkt or an rfc we want to reject, shuffle the
;;; pkt and return it.  caller must specify opcode, either los or cls.
;;; if the op is cls, include a string which is the reason the rfc was
;;; rejected.  note that the very same pkt is used, so when this is called
;;; the pkt had better not be on any lists or anything.

;;; *bj* for *rwf*
(defun transmit-los-int-pkt (int-pkt op &optional reason &aux dh di len)
  (setf (pkt-opcode int-pkt) op)
  (when reason
    (setf (pkt-nbytes int-pkt) (setq len (array-active-length reason)))
    (do ((sidx 0 (+ sidx 2))
	 (widx first-data-word-in-pkt (1+ widx)))
	((>= sidx len))
      (setf (aref int-pkt widx)
	    (if (= (1+ sidx) len)
		(aref reason sidx)
		(dpb (aref reason (1+ sidx)) (byte 8. 8.) (aref reason sidx))))))
  (setq dh (pkt-dest-address int-pkt)
	di (pkt-dest-index-num int-pkt))
  (setf (pkt-dest-address int-pkt) (pkt-source-address int-pkt))
  (setf (pkt-dest-index-num int-pkt) (pkt-source-index-num int-pkt))
  (setf (pkt-source-address int-pkt) dh)
  (setf (pkt-source-index-num int-pkt) di)
  (setf (pkt-fwd-count int-pkt) 0)		;rwf 08/12/86
  (transmit-pkt-route int-pkt))

(defun transmit-brd-pkt (subnet-bit-map subnet-bit-map-length contact-name);ti
  (let ((pkt (allocate-pkt)))
    (unwind-protect (progn
		     (setf (pkt-ack-num pkt) subnet-bit-map-length)
		     (setf (pkt-opcode pkt) brd-op)
		     (setf (pkt-dest-address pkt) 0)
		     (setf (pkt-dest-index-num pkt) 0)
		     (setf (pkt-source-address pkt) my-address)
		     (set-pkt-string pkt (subseq (string subnet-bit-map) 0 (1- subnet-bit-map-length))
				     contact-name)
		     (transmit-pkt pkt ()))
      (free-pkt pkt))))


(defun print-sts-why ();add count and formatting at start & end to fix spr 265.
  (let ((n (array-leader sts-why-array 1)))
    (format t "~&oldest sts reason printed first.~%")
    (do* ((i (rem (1+ n) 64) (rem (1+ i) 64))
	  (count i (1+ count)))
	 (nil);return *after* last iteration.
      (format t "~&~2d ~a" count (aref sts-why-array i))
      (when (= i n)
	(return ())))
    (terpri)))

;;; internal routine to send a status packet to a connection.

(defun transmit-sts (conn why &aux pkt)
  (setf (aref sts-why-array (array-leader sts-why-array 1)) why)
  (store-array-leader (rem (1+ (array-leader sts-why-array 1)) 64) sts-why-array 1)
  (setq pkt (allocate-int-pkt))
  (setf (pkt-opcode pkt) sts-op)
  (setf (pkt-nbytes pkt) 4)
  (setf (pkt-first-data-word pkt) (pkt-num-received conn))
  (setf (pkt-second-data-word pkt) (local-window-size conn))
  (transmit-int-pkt-for-conn conn pkt))



;;------------------------------------------------------------------------------
;; output-main program level
;;------------------------------------------------------------------------------

;;; release a packet to a routine outside the ncp.
;;; this routine should be called whenever returning a packet as a value
;;;	to a caller which is outside the ncp

(defun release-pkt (pkt)
  (cond
    ((null (pkt-status pkt))
     (setf (pkt-status pkt) 'released)
     (setf (pkt-link pkt) ()))
    (t (ferror () "attempt to release ~s, which is already released." pkt))))

;; to send a pkt, first call get-pkt to give you a pkt, fill it full of
;; cruft and set its nbytes, and then call send-pkt on it.
;;

(defun get-pkt (&aux pkt)
  "allocate and return a \"released\" packet.
a released packet is one which can be in use outside the chaosnet ncp itself.
this is the proper way for a progam which uses the chaosnet to get a packet.
when you are finished with it, call return-pkt to allow it to be reused."
  (setq pkt (allocate-pkt))
  (release-pkt pkt)
  pkt)

;; conn must be in open-state, and the opcode must be a dat opcode.

(defun send-pkt (conn pkt &optional (opcode dat-op))
  "send the data packet pkt to connection conn.  opcode specifies the type of packet;
   the default is dat (opcode 200).  pkt is returned to the chaosnet ncp
   and should not be used further by the caller.
   the value is the packet number assigned to the packet."
  (case (state conn)
    (open-state
     (or (logtest dat-op opcode) (= eof-op opcode)
	(ferror () "~o is not a legal opcode." opcode))
     (process-wait "chaosnet output"
		   #'(lambda (x)
		       (or (may-transmit x) (neq (state x) 'open-state)))
		   conn)
     (cond
       ((eq (state conn) 'open-state)
	(unless (eq (pkt-status pkt) 'released)
	  (ferror () "attempt to transmit ~s, which is not released." pkt))
	(setf (pkt-status pkt) ()) (setf (pkt-opcode pkt) opcode) (decf (window-available conn))
	(transmit-normal-pkt conn pkt t t);and send it for the first time.
	(without-interrupts;must do the transmit before putting it
	 (let ((last (send-pkts-last conn)));in send-pkts because transmit-normal-pkt
	   (cond
	     (last (setf (pkt-link last) pkt));fills in lots of fields.
	     (t (setf (send-pkts conn) pkt)))
	   (setf (send-pkts-last conn) pkt)
	   (setf (pkt-link pkt) ())
	   (incf (send-pkts-length conn))
	   (setq retransmission-needed t))
	 (pkt-num pkt)))
       (t (report-bad-connection-state conn "transmit on"))))
    (t (report-bad-connection-state conn "transmit on"))))


(defun send-string (conn &rest strings &aux pkt)
  "send data made by concatenating strings down connection conn."
  (setq pkt (get-pkt))
  (apply #'set-pkt-string pkt strings)
  (send-pkt conn pkt))

;; user level routine to transmit an uncontrolled packet.

(defun send-unc-pkt (conn pkt &optional (pktn-field (pkt-num pkt)) (ack-field (pkt-ack-num pkt)))
  "send a user-handled uncontrolled packet pkt down connection conn.
   the opcode field of pkt should be set up by the caller."
  (setf (pkt-opcode pkt) unc-op)
  (transmit-normal-pkt conn pkt pktn-field ack-field))


(defun may-transmit (conn)
  "t if more packets can be sent down conn without filling up the transmit window.
   packets awaiting transmission count against the limits."
  (> (window-available conn) 0))


(defun data-available (conn)
  "t if input data is available on connection conn."
  (not (null (read-pkts conn))))


(defun finish-conn (conn &optional (whostate "net finish"))
  "wait until all packets awaiting transmission on conn have been sent and acknowledged.
   also returns if the connection is closed.
   returns t if the connection is still open."
  (process-wait whostate #'conn-finished-p conn)
  (eq (state conn) 'open-state))


(defun conn-finished-p (conn)
  "t unless connection is open but not all our transmissions have been acknowledged."
  (or (>= (window-available conn) (foreign-window-size conn))
      (neq (state conn) 'open-state)))



;;------------------------------------------------------------------------------
;; input-main program level
;;------------------------------------------------------------------------------

;; *bj* for *rwf*
(defparameter ack-last-p () "send ack on every data pkt read which is last in q")

(defparameter ack-every-pkt-p () "send ack on every data or eof read")

(defparameter prompt-before-hang-p () "send ack before every pkt read which will hang")

(defun  get-next-pkt (conn &optional (no-hang-p nil) (whostate "chaosnet input") (check-conn-state (not no-hang-p))
		     &aux pkt)			;2/22/85 raf hal-1-71.
  "return the next input packet from connection conn.
   the packet may contain data, or it may be a cls, ans or unc.  if the next input
   packet is not available, either wait or return nil according to no-hang-p.
   whostate is a string to put in the who-line while we wait.
   check-conn-state non-nil says get an error now if connection is in an invalid state.
   default is t unless no-hang-p.  when you are finished with the data in the packet,
   use return-pkt to allow chaosnet to reuse the packet."
  ;; loop until we get a packet, decide not to hang, or error out
  (loop
    ;; check for connection in an erroneous state
    (when check-conn-state
      (unless (member (state conn) '(open-state rfc-received-state answered-state foreign-state) :test
		      #'eq)
	(if (eq (state conn) 'cls-received-state)
	    (unless (read-pkts conn)
	      (ferror 'connection-no-more-data
		      "attempt to receive from ~s,~@
                            a connection which has been closed by foreign host."
		      conn))
	    (report-bad-connection-state conn "receive from"))))
    ;; now see if there are any packets we can have
    (without-interrupts (setq pkt (read-pkts conn))
			(when (not (null pkt))	;got packet, take off of read list
			  (when (/= unc-op (pkt-opcode pkt))
			    (setf (pkt-num-read conn) (pkt-num pkt)))
			  (setf (read-pkts conn) (pkt-link pkt))
			  (when (null (read-pkts conn))
			    (setf (read-pkts-last conn) ()))))
    (when (eq (state conn) 'open-state)
      (if (null pkt)				;got no packet
	  (when (not no-hang-p)			;if we are going to wait, prompt other end for more if we haven't already
	    (if prompt-before-hang-p
		(transmit-sts conn 'no-pkt-ready)
		(when (not (= (pkt-num-read conn) (pkt-num-acked conn)))
		  (transmit-sts conn 'no-pkt-ready-and-unacked))))
	  (cond
	    ((and ack-last-p (= (pkt-num-read conn) (pkt-num-received conn))
		  (not (= (pkt-num-read conn) (pkt-num-acked conn))))
	     (transmit-sts conn 'ack-last))
	    ((>= (* 3 (- (pkt-num-read conn) (pkt-num-acked conn))) (local-window-size conn))
	     (transmit-sts conn 'ack-one-third))
	    (ack-every-pkt-p (transmit-sts conn 'ack-every)))))
    (when (not (null pkt))			;got packet, release from ncp
      (release-pkt pkt))
    (when (or pkt no-hang-p)
      (return pkt))				;if satisfied, return
    ;; not satisfied, wait for something interesting to happen
    (process-wait whostate
		  #'(lambda (x)
		      (or (read-pkts x)
			  (not (member (state x) '(open-state foreign-state) :test #'eq))))
		  conn)))


(defprop report-bad-connection-state t :error-reporter)

(defun report-bad-connection-state (conn operation-string)
  (case (state conn)
    (host-down-state
     (ferror 'host-stopped-responding
	     "attempt to ~*~a ~s,~%a connection whose foreign host died." conn
	     operation-string conn))
    (los-received-state
      (let ((reason (if (read-pkts-last conn) (pkt-string (read-pkts-last conn)) "??")))
	(ferror 'connection-lost "attempt to ~2*~a ~s,~%which got a los: ~a" conn reason
		operation-string conn reason)))
    (cls-received-state
     (ferror 'connection-closed
	     "attempt to ~2*~a ~s,~%a connection which has been closed by foreign host." conn
	     operation-string operation-string conn))
    (otherwise
     (ferror 'bad-connection-state-1
	     "attempt to ~*~a ~s,~%which is in ~s, not a valid state" conn  operation-string conn
	     (state conn)))))

(defun return-pkt (pkt)
  "tell chaosnet you are finished with packet pkt.
   pkt should be a \"released\" packet, obtained with get-pkt or get-next-pkt."
  (case (pkt-status pkt)
    (released (setf (pkt-status pkt) ()) (free-pkt pkt))
    (:otherwise (ferror () "attempt to return unreleased packet (~s) to the ncp." pkt))))



;;------------------------------------------------------------------------------
;;; receiver functions:  these run at receiver interrupt level.
;;------------------------------------------------------------------------------

;;; this function is the called on an int-pkt which has just come in from the
;;; net for this host.  it is mostly a transfer vector to more specialized
;;; functions, but it also does error checking.
;;;
;;; *bj* for *rwf*
(defvar reserved-int-pkts-dropped 0 "number of pkts dropped due to expired reservation.")

(pushnew
  '((symbol-value 'reserved-int-pkts-dropped) "number of pkts dropped due to expired reservation.")
  net::peek-a-boo-list :test #'equal)


;;; this function is the called on an int-pkt which has just come in from the
;;; net for this host.  it is mostly a transfer vector to more specialized
;;; functions, but it also does error checking.
;;;

(defun receive-int-pkt-for-me (int-pkt op)	;4/22/85 raf
  (let (conn ackn (reserved-int-pkt nil))	;bind my copy of this special variable.
    (record-int-pkt-header int-pkt)
    (incf pkts-received)
    (when (logtest 200 op)
      (incf data-pkts-in))
    (cond
      ((= op rfc-op) (receive-rfc int-pkt))
      ((= op brd-op) (receive-rfc int-pkt))	   ;*rla* 11/3/86
      ((= op los-op) (receive-los int-pkt))
      ((= op cls-op) (receive-cls int-pkt))
      ((= op mnt-op) (free-int-pkt int-pkt))
;x      ((= op who-op) (receive-who int-pkt))      ;*rla* 11/3/86
;x      ((= op ur-op) (receive-ur int-pkt))        ;*rla* 11/3/86
      ((and
	 (or (null (setq conn (pkt-dest-conn int-pkt)))
	     (/= (pkt-dest-index-num int-pkt) (local-index-num conn))
	     (/= (pkt-source-address int-pkt) (foreign-address conn)))
	 (not
	   (setq conn
		 (cdr
		   (assoc (pkt-dest-index-num int-pkt) distinguished-port-conn-table :test #'eq)))))
       (transmit-los-int-pkt int-pkt los-op
			     (if conn
				 "you are not connected to the specified remote conn"
				 "no such remote connection exists")))
      ((prog2
	 (setf (time-last-received conn) (time))
	 (= op opn-op))
       (receive-opn conn int-pkt))
      ((= op fwd-op) (receive-fwd conn int-pkt))
      ((= op ans-op) (receive-ans conn int-pkt))
      ((= op unc-op) (receive-unc conn int-pkt))
      ((not (or (= op sns-op) (= op sts-op) (= op eof-op) (>= op dat-op)))
       (transmit-los-int-pkt int-pkt los-op "the received packet had an illegal opcode"))
      ;;; below here can be sns, sts, eof, or dat, all packets having ack fields.
      ((not (remote-conn-connected-p conn int-pkt op)))	;if not connected, already handled.
      (t
       ;;; below here, this int-pkt contains a normal acknowledgement field.
       (setq ackn (pkt-ack-num int-pkt))	;acknowledgement field
       (receipt conn ackn)			;clear receipted packets from send list
       (when (pktnum-< (send-pkt-acked conn) ackn)
	 (setf (send-pkt-acked conn) ackn))
       (update-window-available conn)
       (cond
	 ((or (>= op dat-op) (= op eof-op)) (receive-eof-or-dat conn int-pkt))
	 ((= op sns-op) (receive-sns conn int-pkt))
	 ((= op sts-op) (receive-sts conn int-pkt)))))
    (when reserved-int-pkt
      (incf reserved-int-pkts-dropped)))	;let
  )

(defun remote-conn-connected-p (conn int-pkt op);9/18/84
  "returns t if int-pkt if from the remote conn attached to conn.
   sends an appropriate los pkt if there is an error.
   if op is sns & things are not connected, just frees int-pkt."
  (cond
    ((not (= (pkt-source-index-num int-pkt) (foreign-index-num conn)))
     (cond
       ((= op sns-op) (free-int-pkt int-pkt));ignore sns if not open
       (t (transmit-los-int-pkt int-pkt los-op "remote conn is not connected to you")))
     nil);not connected
    ((not (eq (state conn) 'open-state))
     (cond
       ((= op sns-op) (free-int-pkt int-pkt));ignore sns if not open
       (t (transmit-los-int-pkt int-pkt los-op "remote connection was not open")))
     nil);conn not open
    (t t)))


(defun record-int-pkt-header (int-pkt)
  (dotimes (i 8)
    (setf (aref recent-headers recent-headers-pointer i) (aref int-pkt i)))
  (setf (aref recent-headers recent-headers-pointer 8) (time))
  (setq recent-headers-pointer (rem (1+ recent-headers-pointer) 128)))

;; *bj* for *rwf*
(defun print-recent-headers (&optional (nbr 128.))
  "print the chaos headers for the last nbr packets"
  (do ((i (rem (+ 127. recent-headers-pointer) 128.) (if (zerop i)
							     127.
							     (1- i)))
       (count nbr (1- count)))
      ((zerop count))
    (if (or (= sns-op (rcnt-opcode i)) (= sts-op (rcnt-opcode i)) (= los-op (rcnt-opcode i))
	    (= rfc-op (rcnt-opcode i)) (= ans-op (rcnt-opcode i)) (= rut-op (rcnt-opcode i)))
	(format t "~%     ")
	(format t "~%~16,4,'0r " (rcnt-pkt-num i)))
    (if (or (= los-op (rcnt-opcode i)) (= rfc-op (rcnt-opcode i)) (= ans-op (rcnt-opcode i))
	    (= rut-op (rcnt-opcode i)))
	(format t "          ")
	(format t "ack:~16,4,'0r, " (rcnt-ack-num i)))
    (format t "~a"
	    (cond
	      ((< (rcnt-opcode i) (length opcode-list)) (nth (rcnt-opcode i) opcode-list))
	      ((= (rcnt-opcode i) (+ 1 dat-op)) 'smk)	;synchronous mark
	      ((= (rcnt-opcode i) (+ 2 dat-op)) 'amk)	;asynchronous mark
	      ((>= (rcnt-opcode i) dat-op) 'dat)
	      (t (format () "==> ~o <==" (rcnt-opcode i)))))
    (format t "(~16,2,'0r) " (rcnt-opcode i))
    (format t "~3d. bytes " (rcnt-nbytes i))
    (format t "from ")
    (let ((host (si:get-host-from-address (rcnt-source-address i) :chaos)))
      (if host
	  (format t "~@6a" (send host :name))
	  (format t "  ~16,4,'0r" (rcnt-source-address i))))
    (format t "-~16,4,'0r" (rcnt-source-index i))
    (format t " to ")
    (let ((host (si:get-host-from-address (rcnt-dest-address i) :chaos)))
      (if host
	  (format t "~@6a" (send host :name))
	  (format t "  ~16,4,'0r" (rcnt-dest-address i))))
    (format t "-~16,4,'0r" (rcnt-dest-index i))
    (format t " time:~5d." (rcnt-time-recorded i))
    (unless (zerop (rcnt-fwd-count i))
      (format t " ~d. frwds" (rcnt-fwd-count i)))))

(defun print-int-pkt-headers ();ti
 ;; this version written before i knew about print-recent-headers - raf.
  (format t "~%     op  size dest didx src  sidx num  ack# time")
  (do ((i recent-headers-pointer (rem (1+ i) 128))
       (n 0 (1+ n)))
      ((and (= i recent-headers-pointer) (>= n 128))
       (format t "~%     op  size dest didx src  sidx num  ack# time~%"))
    (format t "~&~3d " n)
    (dotimes (j 9)
      (format t "~16,4,48r " (aref recent-headers i j)))))


(defun print-int-pkts ()
  (dolist (pkt *int-pkt-list*)
    (print (list pkt (int-pkt-thread pkt)))
    (setq pkt (convert-to-pkt pkt ()))
    (print-pkt pkt)
    (free-pkt pkt)))

(defun print-all-pkts (chain &optional (short-display t))
  (do ((pkt chain (pkt-link pkt)))
      ((null pkt))
    (print-pkt pkt short-display)))

;discard packets from send-list which have been receipted by other end

(defun receipt (conn ack-lev)
  (without-interrupts
   (let ((sends (send-pkts conn));(save array references...)
	 (next nil);prevent weird screw.
	 (length (send-pkts-length conn)))
     (do ((pkt sends next));for each pkt not yet acked which this acks,
	 ((or (null pkt) (pktnum-< ack-lev (pkt-num pkt))))
	  ;;(setq next (pkt-link pkt))
       (setq next (setq sends (pkt-link pkt)));two variables only for "clairity"
       (free-pkt pkt)
       (decf length))
     (setf (send-pkts conn) sends)
     (setf (send-pkts-length conn) length)
     (when (null sends)
       (setf (send-pkts-last conn) ())))))

;;; a new ack has come in, so adjust the amount left in the window.  if the window was
;;; full, and has now become "un-full", cause an output interrupt

;;; *bj* for *rwf*

(defun update-window-available (conn &aux (available (window-available conn)))
  (setf (window-available conn)
	(min maximum-window-size		;in case rcvd garbled sent/acked
	     (max available			;in case rcvd out of order
		  (- (foreign-window-size conn)
		     (pktnum-- (pkt-num-sent conn) (send-pkt-acked conn))))))
  (and (zerop available) (not (zerop (window-available conn))) (interrupt-conn :output conn)))

(defun reset-window-available (conn &aux (available (window-available conn)))
  (setf (window-available conn)
	(max 0
	     (min maximum-window-size
		  (- (foreign-window-size conn)
		     (pktnum-- (pkt-num-sent conn) (send-pkt-acked conn))))))
  (when (and (zerop available) (not (zerop (window-available conn))))
    (interrupt-conn :output conn)))

;called when a broadcast packet is received from the net.  for now, ignore it.

(defun receive-broadcast-int-pkt (int-pkt)
  (free-int-pkt int-pkt))



;the following functions are called to process the receipt of a particular kind of packet.

;;; this uses the pkt-num to correctly order the pkts.
;;;	if the pkt-num is less than or equal to the highest we received
;;;	     ignore it and send a receipt
;;;	if one higher then this one is added to the end of the successfully recieved pkts.
;;;	     then the out of sequence list is appended to the insequence list and the
;;;		point of break in sequence is found whereupon the list is broken
;;;		and all the appropriate pointers are set up.
;;;	if more than one larger try locating it's position in the out of order list
;;; when this is called, conn is known to be in open-state.


(defun receive-eof-or-dat (conn int-pkt &aux pkt pkt-num pktl-num prev)
  (setq pkt-num (pkt-num int-pkt))
  (cond
    ((not (pktnum-< (pkt-num-received conn) pkt-num))
     (setq reserved-int-pkt int-pkt)
     (incf pkts-duplicated)
     (transmit-sts conn '<-num-rcvd));this is a duplicate, receipt and ignore
    ((= pkt-num (pktnum-1+ (pkt-num-received conn)))
     ;;; this is the one we were waiting for add it to read-pkts
     (setq pkt (convert-to-pkt int-pkt))
     (when (null (read-pkts conn))
       (interrupt-conn :input conn))
     (without-interrupts
       (setf (pkt-link pkt) (received-pkts conn));link the two lists together
       (setf (received-pkts conn) ())
       (cond
	 ((null (read-pkts-last conn))
	  (setf (read-pkts conn) pkt))
	 (t (setf (pkt-link (read-pkts-last conn)) pkt)))
       (do ((pktl-num (pkt-num pkt) (pktnum-1+ pktl-num)))
	   ((or (null pkt) (/= pktl-num (pkt-num pkt)))
	    (setf (pkt-num-received conn) (pktnum-- pktl-num 1))
	    (setf (received-pkts conn) pkt)
	    (when (not (null prev))
	      (setf (pkt-link prev) ()))
	    (setf (read-pkts-last conn) prev))
	 (setq prev pkt)
	 (setq pkt (pkt-link pkt)))))
    (t
     (without-interrupts
       (do ((pktl (received-pkts conn) (pkt-link pktl))
	    (prev nil pktl))
	   ((null pktl)
	    (setq pkt (convert-to-pkt int-pkt))
	    (cond
	      ((null prev)
	       (setf (received-pkts conn) pkt))
	      (t (setf (pkt-link prev) pkt)))
	    (setf (pkt-link pkt) ()))
	 (setq pktl-num (pkt-num pktl))
	 (cond
	   ((= pkt-num pktl-num);same as existing one, forget about it.
	    (setq reserved-int-pkt int-pkt)
	    (transmit-sts conn 'already-on-rcvd-pkts);send a receipt
	    (return ()))
	   ((pktnum-< pkt-num pktl-num);this is the place!
	    (setq pkt (convert-to-pkt int-pkt))
	    (cond
	      ((null prev)
	       (setf (pkt-link pkt) (received-pkts conn))
	       (setf (received-pkts conn) pkt))
	      (t (setf (pkt-link pkt) (pkt-link prev))
		 (setf (pkt-link prev) pkt)))
	    (return ()))))))))
;;; *bj* for *rwf*

(defun receive-sts (conn int-pkt)
  (receipt conn (pkt-first-data-word int-pkt))
  (setf (foreign-window-size conn) (pkt-second-data-word int-pkt))
  (reset-window-available conn)
  (free-int-pkt int-pkt))

;;;   when this is called, conn is known to be in open-state.

(defun receive-sns (conn int-pkt)
  (setq reserved-int-pkt int-pkt)
  (transmit-sts conn 'sns))


(defun receive-unc (conn int-pkt &aux pkt);9/18/84
  (when (or (eq (state conn) 'foreign-state);foreign-protocol state--no checks
      (remote-conn-connected-p conn int-pkt unc-op));will free a bad pkt.
    (if (>= (loop for x = (read-pkts conn) then (pkt-link x) while x count t)
	 (local-window-size conn))
     ;; there are more packets on the list than the window size.  discard this packet.
     ;; we don't want to allocate infinite packets if someone throws lots of unc packets at us.
      (free-int-pkt int-pkt)
      ;; convert to regular packet, do interrupt-conn, and thread it on.
      (progn
	(setq pkt (convert-to-pkt int-pkt))
	(when (null (read-pkts conn))
	  (interrupt-conn :input conn))
	(without-interrupts
	  (setf (pkt-link pkt) (read-pkts conn))
	  (setf (read-pkts conn) pkt)
	  (when (null (read-pkts-last conn))
	    (setf (read-pkts-last conn) pkt)))))))


(defun contact-name-from-rfc (pkt &aux contact-string tem)
  (setq contact-string (pkt-string pkt))
  (cond
    ((setq tem (position #\space (the string (string contact-string)) :test #'char-equal))
     (nsubstring contact-string 0 tem))
    (t contact-string)))

;;; if rfc matches a pending lsn, call rfc-meets-lsn, else if there is a server,
;;; add to pending list and start up a server.
;;; (so far all we have done is verified pkt-dest-address.)
;;; note that because of rfc-ans stuff, the contact "name" is not the
;;; whole string, so we must do a simple parse.

(defun receive-rfc (int-pkt &aux pkt lsn server contact-name conn)
  (cond
    ((or
      (do ((tst-pkt pending-rfc-pkts (pkt-link tst-pkt)))
	  ((null tst-pkt)
	   nil)
	(and (= (pkt-source-address int-pkt) (pkt-source-address tst-pkt))
	     (= (pkt-source-index-num int-pkt) (pkt-source-index-num tst-pkt)) (return t)))
      (do ((i 1 (1+ i)))
	  ((>= i maximum-index)
	   nil)
	(and (setq conn (aref index-conn i))
	     (= (foreign-address conn) (pkt-source-address int-pkt))
	     (= (foreign-index-num conn) (pkt-source-index-num int-pkt)) (return t))))
     ;;; duplicate rfc, just through the packet away
     (free-int-pkt int-pkt))
    (t (setq pkt (convert-to-pkt int-pkt)) (setq contact-name (contact-name-from-rfc pkt))
       (cond
	 ((setq lsn (assoc contact-name pending-listens :test 'equalp))
	  (setq pending-listens (delete lsn (the list pending-listens) :test #'eq))
	  (rfc-meets-lsn (cdr lsn) pkt))
	 ((and (or (equalp contact-name "status") chaos-servers-enabled)
	       (setq server (assoc contact-name server-alist :test 'equalp)))
	  (without-interrupts;seems like a good idea, altho probably not necessary
	    (setf (pkt-link pkt) pending-rfc-pkts)
	    (setq pending-rfc-pkts pkt))
	  ;; this assumes that the name is in the car of an init list entry
	  ;; was just eval
	  (background-task (si::init-form server)))
	 (t (free-pkt pkt))))))



;;; this is called when we have a lsn matching an rfc.  it can be called when we do
;;; a lsn (m.p. level) or when an rfc gets here (p.i. level).
;;; here listen has filled in some of the fields of the conn, we must
;;; fill in the rest.

(defun rfc-meets-lsn (conn pkt)
  (setf (foreign-address conn) (pkt-source-address pkt))
  (setf (foreign-index-num conn) (pkt-source-index-num pkt))
  (setf (foreign-window-size conn) (pkt-ack-num pkt))
  (setf (local-address conn) (pkt-dest-address pkt))
  (setf (pkt-num-read conn) (pkt-num pkt))
  (setf (pkt-num-received conn) (pkt-num pkt))
  (setf (pkt-num-acked conn) (pkt-num pkt))
  (setf (state conn) 'rfc-received-state)
  (setf (read-pkts conn) pkt)
  (setf (read-pkts-last conn) pkt)
  (setf (pkt-link pkt) ())
  (interrupt-conn :change-of-state conn 'rfc-received-state)
  (interrupt-conn :input conn))

;;; so far both host and both index numbers have been verified.

;;; *bj* for *rwf*
(defun receive-opn (conn int-pkt)
  (case (state conn)
    (rfc-sent-state
     (setf (foreign-index-num conn) (pkt-source-index-num int-pkt))	;			     (when (and (numberp (foreign-window-size conn)) (> (foreign-window-size conn) 2048.))
						;			       (cerror nil "bogus window size from receive-opn"))
     (setf (foreign-window-size conn) (pkt-second-data-word int-pkt))
     (setf (pkt-num-read conn) (pkt-num int-pkt))
     (setf (pkt-num-received conn) (pkt-num int-pkt))
     (setf (pkt-num-acked conn) (pkt-num int-pkt)) (setf (time-last-received conn) (time))
     (receipt conn (pkt-ack-num int-pkt)) (reset-window-available conn)
     (setf (state conn) 'open-state) (setq reserved-int-pkt int-pkt) (transmit-sts conn 'opn)
     (interrupt-conn :change-of-state conn 'open-state))
    (open-state
     (cond
       ((and (= (foreign-address conn) (pkt-source-address int-pkt))
	     (= (foreign-index-num conn) (pkt-source-index-num int-pkt))
	     (= (conn-local-address conn) (pkt-dest-address int-pkt))
	     (= (local-index-num conn) (pkt-dest-index-num int-pkt)))
	(setq reserved-int-pkt int-pkt) (transmit-sts conn 'opn))
       (t (transmit-los-int-pkt int-pkt los-op "you didn't open this connection"))))
    (otherwise (transmit-los-int-pkt int-pkt los-op "bad state for opn"))))




;; we have received a cls.  if the connection which he is closing is still open,
;; put it in closed state, free up all pending send-pkts, and put the cls packet
;; on the read-pkts list so that mp level can see it.
;;      if the connection does not exist, this is not an error, because two
;; clss might have passed each other.  so just free the pkt.

(defun receive-cls (int-pkt &aux pkt (int-flag nil))
  (let ((conn (pkt-dest-conn int-pkt)))
    (cond
      ((null conn) (free-int-pkt int-pkt))
      ((member (state conn) '(open-state rfc-sent-state) :test #'eq)
       (setq pkt (convert-to-pkt int-pkt))
       (without-interrupts
	 (free-all-send-pkts conn)
	 (free-all-received-pkts conn)
	 (setf (state conn) 'cls-received-state)
	 (cond
	   ((null (read-pkts-last conn))
	    (setf (read-pkts conn) pkt)
	    (setq int-flag t))
	   (t (setf (pkt-link (read-pkts-last conn)) pkt)))
	 (setf (read-pkts-last conn) pkt)
	 (setf (pkt-link pkt) ()))
       (interrupt-conn :change-of-state conn 'cls-received-state)
       (and int-flag
	    (interrupt-conn :input conn)))
      (t (transmit-los-int-pkt int-pkt los-op "you sent a cls to the wrong kind of connection.")))))


(defun receive-los (int-pkt &aux (pkt (convert-to-pkt int-pkt)) my-index conn (int-flag nil))
  (setq my-index (ldb maximum-index-log-2-minus-1 (pkt-dest-index-num pkt)))
  (cond
    ((and (< my-index maximum-index)
	  (setq conn (aref index-conn my-index))
	  (eq (state conn) 'open-state)
	  (= (pkt-source-address int-pkt) (foreign-address conn))
	  (= (pkt-source-index-num int-pkt) (foreign-index-num conn)))
     (without-interrupts
       (free-all-send-pkts conn)
       (free-all-received-pkts conn)
       (setf (state conn) 'los-received-state)
       (cond
	 ((null (read-pkts-last conn))
	  (setf (read-pkts conn) pkt)
	  (setq int-flag t))
	 (t (setf (pkt-link (read-pkts-last conn)) pkt)))
       (setf (read-pkts-last conn) pkt)
       (setf (pkt-link pkt) ()))
     (interrupt-conn :change-of-state conn 'los-received-state)
     (and int-flag
	  (interrupt-conn :input conn)))
    (t
     (without-interrupts
       (setf (pkt-link pkt) los-pkts)
       (setq los-pkts pkt)
       (setq current-los-pkt-count (1+ current-los-pkt-count))))))


(defun receive-fwd (conn int-pkt &aux pkt)
  (cond
    ((neq (state conn) 'rfc-sent-state)
     (transmit-los-int-pkt int-pkt los-op "an fwd was sent to a non-rfc-sent index."))
    (t (setq pkt (convert-to-pkt int-pkt))
       (setf (foreign-address conn) (pkt-ack-num pkt))
       (setf (pkt-opcode pkt) rfc-op)
       (transmit-normal-pkt conn pkt (pkt-num-sent conn) (local-window-size conn)))))


(defun receive-ans (conn int-pkt &aux pkt)
  (cond
    ((neq (state conn) 'rfc-sent-state)
     (free-int-pkt int-pkt))
    (t (setq pkt (convert-to-pkt int-pkt))
       (setf (state conn) 'answered-state)
       (setf (read-pkts conn) pkt)
       (setf (pkt-link pkt) ())
       (interrupt-conn :change-of-state conn 'answered-state)
       (interrupt-conn :input conn))))



;; Timed responses and background tasks
(defun background (&aux last-wakeup-time (last-probe-time (time)) tasks time);9/18/84
  (loop (setq time (time)) (setq last-wakeup-time time)
     (do ()
	 ((or (>= (time-difference time last-wakeup-time) retransmission-interval)
	     (>= (time-difference time last-probe-time) probe-interval)))
       (process-wait "background task"
		     #'(lambda (last-wakeup-time last-probe-time &aux (time (time)))
			 (or (>= (time-difference time last-probe-time) probe-interval)
			    (and retransmission-needed
			       (>= (time-difference time last-wakeup-time)
				   retransmission-interval))
			    background-requests))
		     last-wakeup-time last-probe-time)
       (without-interrupts
	 (setq tasks (nreverse background-requests))
	 (setq background-requests ()))
       (mapc 'global:eval tasks)
       (setq time (time)))
     (when retransmission-needed
       (setq retransmission-needed ()
	     more-retransmission-needed ())
       (mapc 'retransmission conn-list);retransmit on all connections
       (without-interrupts
	 (setq retransmission-needed (or retransmission-needed more-retransmission-needed))))
     (when (>= (time-difference time last-probe-time) probe-interval)
       (setq last-probe-time time);do probes and timeouts
       (background-update-routing-table)
       (mapc 'probe-conn conn-list))
     (initializations 'background-execution-list)))


;;; retransmit all unack'ed packets not recently sent


;; *bj* for *rwf*
(defvar retransmission-check-interval 36 "reactivate the retransmission process at this rate")	;rwf 08/12/86


(defun retransmission (conn &optional force-immediate &aux time (inhibit-scheduling-flag t))
  (cond
    ((member (state conn) '(open-state rfc-sent-state) :test #'eq)	;only if it is open or awaiting a response from rfc
     (setq time (time))
     (block conn-done
       (do ()
	   (nil)
	 (let ((inhibit-scheduling-flag t))
	   (do ((pkt (send-pkts conn) (pkt-link pkt)))
	       ((null pkt)
		(return-from conn-done ()))
	     (cond
	       ((not (eq conn (pkt-source-conn pkt)))
		(ferror 'net::local-network-error-1
			"~s in send-pkts list for incorrect conn:
conn ~s, (pkt-source-conn pkt) ~s."
			pkt conn (pkt-source-conn pkt))))
	     (setq more-retransmission-needed t)
	     (cond
	       ((or
		  (>= (time-difference (time) (pkt-time-transmitted pkt)) retransmission-interval)
		  force-immediate)
		(setf (pkt-being-retransmitted pkt) t) (setq inhibit-scheduling-flag ())
		(transmit-pkt pkt (neq (pkt-opcode pkt) rfc-op))	; 1/15/86 mmg.
		(setq pkts-retransmitted (1+ pkts-retransmitted))
		(setq inhibit-scheduling-flag t)
		(cond
		  ((eq (pkt-being-retransmitted pkt) 'free)
		   (setf (pkt-being-retransmitted pkt) ()) (free-pkt pkt))
		  (t (setf (pkt-being-retransmitted pkt) ())))
		(return ())))))
	 (process-sleep retransmission-check-interval))	;must always start from beginning of chain if
						; turned on scheduling, since chain could be invalid
       ))))

;;; send a sns on this conn if necessary.  decide whether remote host is down.
;;; this gets called every probe-interval.

(defvar *inactivity-time-inconsistent-count* 0
  "the number of times a probed conns inactivity time was too large when probing.")

(defvar *debug-inactivity-time* nil "t to signal inactivity time error.")

(defun probe-conn (conn &optional (force-immediate nil) &aux delta-time)	;9/18/84
  (when (member (state conn) '(open-state rfc-sent-state) :test #'eq)
	;;only if it is open or awaiting a response from rfc
	(setq delta-time (time-difference (time) (time-last-received conn)))
	(cond
	  ((> delta-time host-down-interval)
	   (when (> delta-time (* 10 host-down-interval))
	     (incf *inactivity-time-inconsistent-count*)
	     (when *debug-inactivity-time*
		 (ferror 'net::local-network-error-1
			 "bug: inactivity time is inconsistent with host down interval.")))
	   (without-interrupts (free-all-send-pkts conn)
			       (free-all-received-pkts conn)
			       ;;; 05/86 do not do the following,it can result in dual allocation of conns -rwf
			       ;;;
			       ;;; remove any old conn's for this host which are in the host-down-state
			       ;;;             (do* ((remote-addr (foreign-address conn))
			       ;;;                   (cons conn-list next-cons)
			       ;;;                   (next-cons (cdr cons) (cdr cons))
			       ;;;                   (conn2 (car cons) (car cons)))
			       ;;;                  ((null cons))
			       ;;;               (when (and (= (foreign-address conn2) remote-addr)
			       ;;;                          (eq (state conn2) 'host-down-state))
			       ;;;                 (remove-conn conn2)))
			       (setf (state conn) 'host-down-state))
	   (interrupt-conn :change-of-state conn 'host-down-state))
	  ((and (eq (state conn) 'open-state)	;send sns only on open connections
		(or (< (window-available conn) (foreign-window-size conn))
		    (> delta-time long-probe-interval) force-immediate))
	   (let ((pkt (allocate-int-pkt)))
	     (setf (pkt-opcode pkt) sns-op)
	     (setf (pkt-nbytes pkt) 0)
	     (transmit-int-pkt-for-conn conn pkt))))))


(defun los-conn (conn &optional (reason "los generated in peek") &aux len (int-pkt (allocate-int-pkt)))
  (setf (pkt-opcode int-pkt) los-op)
  (if reason
      (progn
	(setf (pkt-nbytes int-pkt) (setq len (array-active-length reason)))
	(do ((sidx 0 (+ sidx 2))
	     (widx first-data-word-in-pkt (1+ widx)))
	    ((>= sidx len))

	  (setf (aref int-pkt widx)
		(if (= (1+ sidx) len)
		    (aref reason sidx)
		    (dpb (aref reason (1+ sidx)) (byte 8. 8.) (aref reason sidx))))))
      (setf (pkt-nbytes int-pkt) 0))
  (transmit-int-pkt-for-conn conn int-pkt)
  (remove-conn conn ()))



;; enable-ing and reset functions


(defun assure-enabled ()
  "make sure the chaosnet ncp is operating.
user programs may call this before attempting to use the chaosnet."
  (when net::halt
    (ferror 'net::local-network-error
	    "network is halted.   (net:halt nil) to restart it before proceeding."))
  (or enable (enable)))

(defun enable ()
  (unless net::halt
    (send background :revoke-run-reason)
    (send background :preset 'background)
    (send background :run-reason)
    (initializations 'enable-list t)
    (setq enable t)
    (setq chaos-servers-enabled t)
    (ethernet::add-net-packet-allocator
      ethernet::chaos-type 'chaos::allocate-int-pkt 'chaos::free-int-pkt)
    (ethernet:add-protocol ethernet::chaos-type 'chaos:check-and-receive-chaos-int-pkt)))


(deff stop 'disable)

(defun disable ();ti
  (ethernet:remove-protocol ethernet::chaos-type)
  (setq chaos-servers-enabled nil)
  (setq enable ())
  (send background :revoke-run-reason)
  (initializations 'disable-list t))

(defvar conns-already-inactive 0 "connections that were deactivated when already inactive.")

(pushnew
  '((symbol-value 'conns-already-inactive) "connections that were deactivated when already inactive.")
  net::peek-a-boo-list :test #'equal)

;;; *bj* for *rwf*
(defun reset (&optional enable-p hard)		;9/18/84
  "turn off and reinitialize the chaosnet software.  this may unwedge it
if it is not working properly.  causes all currently open connections to
lose.  leaves chaosnet disabled. (chaos:enable) turns the chaosnet ncp
on.  many user-level functions that use the net do that for you. calling
this function with enable-p of t has the same effect."
  (disable)
  (without-interrupts

    (setq background-requests ())		;get rid of requests for connections flushing
    (setq retransmission-needed t)

    (dolist (conn conn-list)
	    (remove-conn conn ()))		;11/84 raf.

    (dolist (conn distinguished-port-conn-table)
	    (remove-conn conn ()))		;11/84 raf.

    (when hard					;05/86
	  (setf free-conn-list (nconc free-conn-list removed-conn-list)))

    (setf removed-conn-list ())

    (dotimes (i maximum-index)			;probably redundant.
	     (setf (aref index-conn i) ()))

    (setq distinguished-port-conn-table ())	;probably redundant.

    (setq conn-list ())				;probably redundant.

    (unless (and (integerp index-conn-free-pointer)
		 (> maximum-index index-conn-free-pointer -1))
	    (setq index-conn-free-pointer 1))
    ;; the initialization is needed because if the lisp machine has an open connection,
    ;; it gets reloaded, and the connection is established on the same index before the
    ;; other end has closed his connection, then the rfc will look like a duplicate.
    ;; though this may sound like a rare event, it is exactly what happens with the
    ;; file job connection!!
    (do ((index 0 (1+ index)))
	((>= index maximum-index))
      (setf (aref uniquizer-table index) (+ (time) index)))

    (setq pending-listens ())

    (do ((pkt pending-rfc-pkts next-pkt)	;free up any pkts in pending rfc's
	 (next-pkt))
	((null pkt)
	 (setq pending-rfc-pkts ()))
      (setq next-pkt (pkt-link pkt))
      (free-pkt pkt))

    ;; fifty (50) is a pretty arbitrary number, but it used to be maximum-index which
    ;; grew like mad causing an absurd number of pages to get wired.  this is undoubtedly
    ;; enough for average use.
    (create-chaosnet-buffers 50)
    (do ((pkt made-pkts (pkt-made-link pkt)))	;reclaim normal chaos buffers  rwf 08/05/86
	((null pkt))
      (setf (pkt-status pkt) ())
      (setf (pkt-link pkt) (pkt-made-link pkt)))

    (setf free-pkts made-pkts))

  (net::reset-meters)				; clear the meters (1.61, raf)
  (reset-routing-table)
  (setq conns-already-inactive 0
	reserved-int-pkts-dropped 0)
  (if (not enable-p)
      "reset and disabled"
      (progn
	(enable)
	"reset and enabled")))

(add-initialization "reset chaosnet" '(reset net:*net-reset-enable-p*) nil
		    'net:*reset-initialization-list*)




(defun setup-my-address ()		   ;ti

  ;; setup address variables.
  (setf my-addresses (sort (send si:local-host :network-address-list :chaos) #'<))

  (when my-addresses
    (setf my-subnets (loop for addr in my-addresses collect (subnet addr)))
    (setf my-address (first my-addresses))
    (setf my-subnet (subnet my-address))
    (chaos:reset-routing-table)))

;;(defun setup-my-address ()		   ;ti

;;  ;; setup address variables.
;;  (setf my-addresses (send si:local-host :network-address-list :chaos))

;;  (when my-addresses
;;    (setf my-subnets (loop for addr in my-addresses collect (subnet addr)))
;;    (setf my-address (first my-addresses))
;;    (setf my-subnet (subnet my-address))

;;    ;; inform controllers.
;;    (loop for (controller) on net:controller-list
;;	  for (subnet) on my-subnets
;;	  do (setf (send controller :subnet) subnet))
;;    (chaos:reset-routing-table)))


;;  formerly in ethernet file, moved here for network partitioning.    4/86 mmg

(defun check-and-receive-chaos-int-pkt (ignore int-pkt nbytes &rest ignore)
  (setf (int-pkt-word-count int-pkt)
	(+ 3 (pkt-nwords int-pkt)))                      ; 3 words for hw hdr.
  (cond ((> (pkt-nwords int-pkt) max-words-per-pkt)
	 (free-int-pkt int-pkt))
	((> (+ 16 (pkt-nbytes int-pkt)) nbytes)	   ; length in hdr longer than data sent?
	 (free-int-pkt int-pkt))
	(t (setf (int-pkt-csr-1     int-pkt)  0)   ; say no crc-1 error
	   (setf (int-pkt-csr-2     int-pkt)  0)   ; say no crc-2 error
	   (setf (int-pkt-thread    int-pkt) ())
	   (setf (int-pkt-bit-count int-pkt)
		 (* (- (int-pkt-word-count int-pkt) 3) 16))	   ; fill in bit count
	   (setf (int-pkt-hardware-dest int-pkt) my-address)
	   (without-interrupts
	     (let ((reserved-int-pkt nil))
	       (receive-pkt int-pkt)
	       (when reserved-int-pkt
		 (ferror () "int pkt about to be lost in ethernet stuff!")))))))


(defun display-chaos-state (&optional (stream *standard-output*))
  "prints the current state of chaos to stream"
  (let ((*print-base* 8))
    (format stream "~2%state of ~a~2%" (send (si:get-host-from-address my-address :chaos) :name))
    (format stream "~&chaosnet is ~:[running~;halted~]." net::halt)
    (format stream "~&chaosnet is ~:[dis~;en~]abled." enable)
    (format stream "~&address = ~o [~:*~16r], ~a" my-address my-addresses)
    (format stream "~&subnet(s) = ~o , ~a~2%" my-subnet my-subnets)
    (format stream "~&:ether-subnets = ~a, :chaos-subnet = ~a" net::ether-subnets
	    net::chaos-subnet)
    (dolist (cont net::controller-list)
      (format stream "~&~a controller (subnet ~16r) in slot ~16r is ~:[dis~;en~]abled."
	      (send cont :board-type)
	      (send cont :subnet)
	      (send cont :slot)
	      (send cont :enabled)))))

;; this now done by enable.
;(ethernet:add-protocol ethernet::chaos-type 'chaos:check-and-receive-chaos-int-pkt)

;(add-initialization "allow chaos servers" '(setq chaos-servers-enabled t) '(:before-cold))
;(add-initialization "chaos-ncp" '(initialize-ncp-once) '(once))
;(add-initialization "reset chaos" '(reset) '(before-cold))
;(add-initialization "initialize ncp system" '(initialize-ncp-system) '(system normal)) ; normal overrides first default
;(add-initialization "initialize ncp cold" '(initialize-ncp-cold) '(cold))
;(add-initialization "enable chaos" '(enable) '(warm))
