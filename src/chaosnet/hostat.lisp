;;; -*- Mode:Common-Lisp; Package::CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*- 



1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.
;;;*
;;; 05-11-88 DAB Print unknown hosts in hostat.
;;; 12-12-87 DAB Fixed create-hostat-connection-list delete-duplicates test from EQl to EQUAL.


(defun 4host-up-p* (host &optional (timeout 180) &aux pkt)
  2"Return T if the host is up, otherwise ().  Always () for non-chaosnet machines."*
  (let ((host (net:parse-host host)))
  (cond
    ((send host :network-address-list :chaos)
     (condition-case ()
	 (setq pkt (simple host 3"STATUS"* timeout))
       (sys:network-error nil)
	(:no-error (return-pkt pkt) t)))
    (t nil)))) 


(deff 4host-up* 'host-up-p) 


1;;------------------------------------------------------------------------------
;;                                UP-HOSTS Function
;;------------------------------------------------------------------------------*


(defun 4up-hosts* (list-of-hosts &optional number-of-hosts (timeout 240) &aux connections time-began winners)
  2"Returns a list of hosts the hosts in LIST-OF-HOSTS that are deemed to be up.
   IF NUMBER-OF-HOSTS not NIL, then return as soon as we determine that at least
   NUMBER-OF-HOSTS are up.  In that case, we still return a list of hosts.
   TIMEOUT is how long in sixtieths of a second to give each host a chance to respond
   before deeming that that host is down.  All testing is done in parallel.
   Non-chaosnet machines are presently not checked at all; they are assumed to be down."*
  1;;the last line should be fixed in the future*
  (setq list-of-hosts (mapcar #'si:parse-host list-of-hosts))
  1;;parse them! *
  (catch 'done
    (unwind-protect (progn
		      (setq connections (make-fast-connection-list list-of-hosts 3"STATUS"* 1))
		      (process-allow-schedule)
		      1;;wait around a bit*
		      (setq time-began (time))
		      (do ()
			  ((null connections))
			(dolist (host-and-connections connections)
			  (let ((host (car host-and-connections)))
			    (dolist (connection (cdr host-and-connections))
			      (let ((address (car connection))
				    (conn (cdr connection)))
				(if (not conn)
				  (setf (cdr connection)1; MAKE-FAST-CONNECTION-LIST lost*

					(condition-case ()1; before, so try again.*
					   (open-connection address 3"STATUS"* 1)
					   (network-resources-exhausted nil)))
				  (let ((state (state conn)))
				    (cond
				      ((eq state 'answered-state)
				       (dolist (c (cdr host-and-connections))1; Close all connections*
					 (if (cdr c)
					   (close-conn (cdr c))))1; knock it off*
				       (setq connections
					     (delete host-and-connections (the list connections)
						     :test #'eq))
				       (push host winners))
				      ((member state
					       '(open-state cls-received-state
						 los-received-state)
					       :test #'eq)
				       (push host winners) (close-conn conn)
				       (setf (cdr host-and-connections)1; Remove a single connection*

					     (delete connection
						     (the list (cdr host-and-connections)) :test
						     #'eq)))
				      (t
				       1;;otherwise (this is primarily rfc-sent-state)*
				       (when (>= (time-difference (time) (time-last-received conn))
					    timeout)
					 (close-conn conn)
					 1;;loser*
					 (setf (cdr host-and-connections)1;Remove a single connection*

					       (delete connection
						       (the list (cdr host-and-connections))
						       :test #'eq))))))))
			      (cond
				((and number-of-hosts1;not NIL*
				    (>= (length winners) number-of-hosts))
				 (throw 'done
					winners)))
			      (if (null (cdr host-and-connections))
				(setq connections
				      (delete host-and-connections (the list connections) :test
					      #'eq)))))))
		      (throw 'done
			     winners))1; Remove host objects with no connections attached to them*
		     1;; Unwind-protect cleanup -- Flush any connections that remain*
      (dolist (h-and-c connections)
	(dolist (connection (cdr h-and-c))
	  (if (cdr connection)
	    (remove-conn (cdr connection)))))))
  winners)


(defun 4find-any-up-host-of-type* (type &optional dont-use-hosts even-if-down (timeout 360) &aux hosts-to-try)
  2"Returns a host which is of that is of system type TYPE.
Will not return a host if it is in the list DONT-USE-HOSTS.
TIMEOUT specifies how long to wait before giving up, in 60ths of a sec.  Default is 6 seconds.
If DONT-USE-HOSTS is T, then use every host with that system type, instead of just one.
If EVEN-IF-DOWN is T, return all such hosts, regardless if they are up or not.
If you are using the EVEN-IF-DOWN argument, you probably want LIST-ALL-NET-MACHINES."*
  (setq hosts-to-try (list-all-net-machines type))
  (if (neq dont-use-hosts t)
    (dolist (host dont-use-hosts)
      (setq hosts-to-try (delete (si:parse-host host) (the list hosts-to-try) :test #'eq))))
  (if even-if-down
    hosts-to-try
    (up-hosts hosts-to-try (if (eq dont-use-hosts t)
			     ()
			     1;;find all*
			     1)
	      timeout)))

1;;------------------------------------------------------------------------------
;;                  STATUS SERVER (also known as HOSTAT)
;;------------------------------------------------------------------------------

;; Format is as follows:
;; Bytes  Description
;;  0-31: Name, left justified, null (0) filled.
;; 32-33: Subnet ID + #x100 (to define format differences from prior version.)
;; 34-35: Number of 16-bit words of data about this Subnet Host.
;; 36-39: PKTS-RECEIVED
;; 40-43: PKTS-TRANSMITTED
;; 44-47: JAM-COUNT (%Count-Chaos-Transmit-Abort for Chaos hardware)
;; 48-51: PKTS-LOST
;; 52-55: FCS-ERRORS
;; 56-67: Cadr-specific-data.
;;
;; The fields in bytes 32-67 may be repeated for a different Subnet ID.*



;(defmethod 4(net::basic-controller :status*) (pkt)
;  2"Fill in the STATUS data for this controller."*
;  (let* ((subnet (multiple-value-bind (ignore subnet) (find self chaos:routing-table :from-end t)
;		  (or subnet 0)))
;	(count (pkt-add-32 pkt (+ subnet 256) ())))
;    (pkt-add-32 pkt net::pkts-received count)
;    (pkt-add-32 pkt net::pkts-transmitted count)
;    (pkt-add-32 pkt net::jam-count count)
;    (pkt-add-32 pkt net::pkts-lost count)
;    (pkt-add-32 pkt net::fcs-errors count)
;    (send self :send-if-handles :other-status pkt count))) 

; (compile-flavor-methods net::basic-controller)

(defun 4pkt-add-32* (pkt count size)1;1/85 RAF*
 1;; Returns the index of the high-order word of data.*
 1;; When size is non-nil, bumps the count at SIZE by 2 16bit words.*
  (let ((idx (+ first-data-word-in-pkt (truncate (pkt-nbytes pkt) 2))))
    (setf (aref pkt idx) (ldb (byte 16 0) count))
    (setf (aref pkt (1+ idx)) (ldb (byte 16 16) count))
    (incf (pkt-nbytes pkt) 4)
    (when (not (null size))
      (incf (aref pkt size) 2))
    (1+ idx))) 



(DEFUN 1SEND-STATUS* ()
  (declare (special 2CHAOS:ROUTING-TABLE *)) ;06-13-88
  (LET* ((CONN (LISTEN 3"STATUS"*))
	 (PKT (GET-PKT))
	 (STRING (PKT-STRING PKT)))
    (SET-PKT-STRING PKT (HOST-DATA MY-ADDRESS))
    (DO ((I (ARRAY-ACTIVE-LENGTH STRING) (1+ I)))
	((>= I 32))
      (VECTOR-PUSH 0 STRING))
    (SETF (PKT-NBYTES PKT) 32)
    (DOLIST (CONTROLLER NET::CONTROLLER-LIST)
      2(LET* ((SUBNET*
	      2(MULTIPLE-VALUE-BIND (IGNORE SUBNET)*
		  2(FIND CONTROLLER CHAOS:ROUTING-TABLE :FROM-END T)*
	        2(OR SUBNET 0)))*
	     2(COUNT (PKT-ADD-32 PKT (+ SUBNET 256) ())))*
        2(PKT-ADD-32 PKT (SEND CONTROLLER :PKTS-RECEIVED) COUNT)*
	2(PKT-ADD-32 PKT (SEND CONTROLLER :PKTS-TRANSMITTED) COUNT)*
	2(PKT-ADD-32 PKT (SEND CONTROLLER :JAM-COUNT) COUNT)*
	2(PKT-ADD-32 PKT (SEND CONTROLLER :PKTS-LOST) COUNT)*
	2(PKT-ADD-32 PKT (SEND CONTROLLER :FCS-ERRORS) COUNT)*
	2(SEND CONTROLLER :SEND-IF-HANDLES :OTHER-STATUS PKT COUNT)*))2     *
    (ANSWER CONN PKT))) 



(add-initialization 3"STATUS"* '(send-status) () 'server-alist)


1;;------------------------------------------------------------------------------
;;                                HOSTAT, User side
;;------------------------------------------------------------------------------*


(defparameter 4hostat-timeout* (* 15 60))             1;timeout in 15 seconds. RAF. 

;;what a kludge *

(defvar 4canonical-hostat-all-list* ()
   2"Internal list in the form (host address nil) which is used to speed up hostat."*) 



(defun 4hostat* (&rest hosts)
  2"Prints out information on **STANDARD-OUTPUT*2 on the status of all of the hosts specified by HOSTS."*
  (poll-hosts *standard-output* hosts 3"STATUS"* #'hostat-heading #'hostat-format-ans))

 
(defun poll-hosts (stream hosts contact-name header-function format-function &optional
	 list-of-skipped-connection-states (window-size 1) (timeout hostat-timeout) &aux
	 connections pkt time-began (open-connections 0) punt-this-host unknown-host)  ;05-05-88 DAB
  "Print the status of chaosnet hosts in HOSTS, or all known chaosnet hosts.
  STREAM is where all of the information is printed.  
  HOSTS is a list of hosts to report about.  If NIL, then all chaonset hosts are used.
  HEADER-FUNCTION is a function called to print out the intial header, and
  FORMAT-FUNCTION is called with an address and packet on the successful connections.
  The WINDOW-SIZE defaults to 1 and the TIMEOUT defaults to 10 seconds (600.)
  LIST-OF-SKIPPED-CONNECTION-STATES is either NIL or a list of states as :answered-state
  for which nothing is printed if the connection is in that state.   Default is NIL.
  The states can be one of :rfc-sent-state :answered-state :cls-received-state
  :open-state :los-received-state or :otherwise (a state not listed above)."
  ;;Print header, at the beginning!!
  (unwind-protect
      (progn
	;;connection should be a list in the form (host address connection)
	(setf (values connections unknown-host) (create-hostat-connection-list hosts)) ;05-05-88 DAB
	(funcall header-function)
	(setq time-began (time))
	(do ()
	    ((null connections))
	  ;;loop until there are no more
	  ;; Handle any replies that have come in.
	  ;; Note host-name truncated to 27. characters to make more room for statistics
	  ;;Only have up to 20 outstanding connections at a time
	  (loop for element in connections
		while (< open-connections 20)
		when (null (third element))
		doing (let ((new-conn
			      (condition-case ()
				  (open-connection (second element) contact-name window-size)
				(network-resources-exhausted nil))))
			(when new-conn
			      (incf open-connections))
			(setf (third element) new-conn)))
	  (process-allow-schedule)
	  ;;take some chaosnet interrupts
	  (loop for (host address conn) in connections when (not (null conn)) doing
		(decf open-connections) (setq punt-this-host t)	;assume that we will then want to close all conns
		(case (and conn (state conn))
		      ;;conn may be NIL
		      (rfc-sent-state (incf open-connections)
				      ;;assume conn may stay open
				      (setq punt-this-host ())
				      ;; ibid
				      (when (>= (time-difference (time) (time-last-received conn)) timeout)
					    (unless (member :rfc-sent-state list-of-skipped-connection-states :test
							    #'eq)
						    (format stream "~O~7T~@[~A   ~]Host not responding~%" address
							    host))
					    ;;;		      (DECF OPEN-CONNECTIONS) ;;??
					    (setq punt-this-host t)))
		      (answered-state		;This is what we want
			(incf open-connections)
			;;we subtract one for each we close
			(setq pkt (get-next-pkt conn))
			(unless (member :answered-state list-of-skipped-connection-states :test #'eq)
				(funcall format-function address pkt))
			(return-pkt pkt)
			;; Delete not only this connection, but every one to this same host, in
			;; case it has multiple addresses.  One copy of the answer is enough, but
			;; if it fails we would like to see all paths.
						; Close all connections to this host
			)
		      (cls-received-state (setq pkt (get-next-pkt conn))
					  (unless (member :cls-received-state list-of-skipped-connection-states :test
							  #'eq)
						  (format stream "~O~7T~@[~A   ~]returned a CLS:~A~%" address host
							  (pkt-string pkt)))
					  (return-pkt pkt))
		      (open-state
			(unless (member :open-state list-of-skipped-connection-states :test #'eq)
				(format stream "~O~7T~@[~A   ~]returned an OPN~%" address host))
			(close-conn conn "I expected an ANS, not an OPN.") (setq conn ()))
		      (los-received-state (setq pkt (read-pkts-last conn))
					  (unless (member :los-received-state list-of-skipped-connection-states :test
							  #'eq)
						  (format stream "~O~7T~@[~A   ~]returned a LOS:~A~%" address host
							  (pkt-string pkt))))
		      (otherwise
			(cond
			  (conn
			    ;;make sure conn exists
			    (unless (member :otherwise list-of-skipped-connection-states :test #'eq)
				    (format stream
					    "~O~7T~@[~A   ~]connection entered bad state: ~A~%"
					    address host (state conn))))
			  (t (incf open-connections)))))
		(cond
		  (punt-this-host (and conn (close-conn conn))
				  ;;or remove-conn
				  (decf open-connections)
				  (setq connections
					(delete (assoc host connections :test #'equal)
						(the list connections) :test #'equal)))))))
    (when unknown-host (format stream "~%Unknown Hosts: ~a~%" unknown-host)) ;05-11-88 DAB
    ;; Remove host objects with no connections attached to them
    ;; Unwind-protect cleanup -- Flush any connections that remain
    (loop for (host address conn) in connections when conn
	  ;;still remains
	  do (close-conn conn))))

 



1;;------------------------------------------------------------------------------
;;                             HOSTAT service routines
;;------------------------------------------------------------------------------*



(defun 4hostat-heading* (&optional (stream *standard-output*))
  (format stream
	  3"~&Chaosnet host status report.  Type Control-Abort to quit.~@
                  ~%~7A~25A"*
	  3"Site"* 3"Name/Status"*)
  (do ((heads '(3"Subnet"* 3"#-in"* 3"#-out"* 3"abort"* 3"lost"* 3"crc"* 3"ram"* 3"bitc"* 3"other"*) (cdr heads))
       (widths '(6 9 9 8 8 8 4 5 6) (cdr widths)))
      ((null heads)
       (send stream :tyo #\Newline))
    (format stream 3"~V@A"* (car widths) (car heads)))) 



(defun 4hostat-format-ans* (host pkt &optional (stream *standard-output*))
  (format stream 3"~7@<~O ~>~27A"1;Print host number and name as returned**
	  host
	  (nsubstring (pkt-string pkt) 0
		      (min (pkt-nbytes pkt) 27
			   (or
			    (position 0 (the string (string (pkt-string pkt))) :start 0 :end 32
				      :test #'char-equal)
			    1;; This line is temporary! ********
			    (position 128 (the string (string (pkt-string pkt))) :start 0 :end
				      32 :test #'char-equal)
			    32))))
  (hostat-format-ans-1 pkt 34 '(4 9 9 8 8 8 4 5 6) stream)) 



(defun 4hostat-format-ans-1* (pkt start-column column-widths stream)
  (do ((i 24 (+ i 2 ct))1;Now display subnet meters*
       (first-line t nil)
       (id)
       (ct)
       (maxi (+ first-data-word-in-pkt (truncate (pkt-nbytes pkt) 2))))
      ((>= i maxi)
       (when first-line
	 (send stream :tyo #\Newline)))
    (setq id (aref pkt i)
	  ct (aref pkt (1+ i)))1;Block header*
    (when (null first-line)
      (format stream 3"~VA"* start-column 3""*))
    (cond
      ((< id 256)1;Subnet info (old 16-bit format)*
       (format stream 3"~VO"* (car column-widths) id)
       (do ((j (+ i 2) (1+ j))1;Now print those meters that are present*
	    (l (cdr column-widths) (cdr l))
	    (n (min ct 8) (1- n)))
	   ((zerop n))
	 (format stream 3"~VD"* (car l) (aref pkt j))))
      ((< id 512)1;Subnet info*
       (format stream 3"~VO"* (car column-widths) (- id 256))
       (do ((j (+ i 2) (+ j 2))1;Now print those meters that are present*
	    (l (cdr column-widths) (cdr l))
	    (n (min (truncate ct 2) 8) (1- n)))
	   ((zerop n))
	 (format stream 3"~VD"* (car l) (dpb (aref pkt (1+ j)) (byte 16 16) (aref pkt j)))))
      (t1;I don't know about this*
       (format stream 3"~O unknown info block ID"* id)))
    (send stream :tyo #\Newline))) 



(defun 4initialize-canonical-hostat-all-list* (&aux hosts)
  2"Initializes the variable CANONICAL-HOSTAT-ALL-LIST to have the right value."*
  (setq canonical-hostat-all-list ()
	hosts ())
  1;;reverse does a copylist which we need before sortcar*
  (dolist (host (reverse (all-chaos-hosts)))
   1;;speed up as this is likely to be in reverse alphabetical order*
    (push (cons (send host :name) host) hosts))
  1;;alphabatize so it looks pretty.*
  (setq hosts (sortcar hosts #'alphalessp))
  (dolist (cons hosts)
    (push (list (cdr cons) (address-parse (cdr cons)) ()) canonical-hostat-all-list))
  (setq canonical-hostat-all-list (nreverse canonical-hostat-all-list))) 

(defun create-hostat-connection-list (&optional hosts)
  "Return a list in the form (host address nil) for each host in HOSTS, or for all chaos hosts. 
   Return a second list of unknown hosts."
  (unless hosts (setf hosts host:*host-list*))
  (let ((unknown-host ())) ;05-11-88 DAB
    (values
      (delete-duplicates 
	(loop for host in hosts
	      with parsed-host
	      do (setf parsed-host (si:parse-host host t t))
	      when (not parsed-host) do (push host unknown-host) ;05-05-88 DAB
	      when (and parsed-host
			(address-parse parsed-host))
	      collect (list parsed-host (address-parse parsed-host) ()))
	:test #'equal :key #'(lambda (host) (send (first host) :network-address-list :chaos)))
      unknown-host)))


(defun 4hostat-full-handler* (&rest ignore)
  (throw 'connection-table-full
	 ()))


1;HOST-DATA: returns information about a specified host.  Currently,
; returns name of machine as primary value and host number as second value


;;; *BJ* For *RWF*
;; Remove reference to si:machine-location-alist. *BJ**
(defun 4host-data* (&optional (host my-address) &aux host-address host-name tem)
  2"Return the long name and chaos address of a host."*
  (declare (values host-name host-address))
  
  (unless (setq host-address (address-parse host))
	  (ferror () 3"~S is an illegal host specification"* host))
  
  
  (cond ((member host-address chaos:my-addresses :test #'eql)
	 (setf host-name (send si:local-host :fully-qualified-name)))
	
	((setq tem (get-host-status-packet host-address))
	 (let ((string (pkt-string tem)))
	   (setq host-name (subseq (string string) 0
				   (min (pkt-nbytes tem) 32
					(or (position 0 (the string (string string)) :test #'char-equal) 32))))
	   (return-pkt tem)))
	(t
	  (setq host-name 3"Unknown"*)))
  (values host-name host-address))


(defun 4get-host-status-packet* (host &aux connection pkt adr)
  2"Returns a STATUS packet from the specified host or NIL if couldn't get the packet"*
  (assure-enabled)
  (setq adr (or (address-parse host) (ferror () 3"Not a known Chaos address: ~S"* host)))
  (setq connection (open-connection adr 3"STATUS"* 1))
  (do ()
      ((null connection))
    (process-sleep 10)1;Take a few chaos net interrupts*
    (case (state connection)
      (rfc-sent-state
       (cond
	 ((>= (time-difference (time) (time-last-received connection)) 300)1;5-second timeout*
	  (remove-conn connection) (return ()))))
      (answered-state1;This is what we want*
       (setq pkt (get-next-pkt connection)) (close-conn connection) (return pkt))
      (cls-received-state (close-conn connection) (return ()))
      (open-state (close-conn connection 3"I expected an ANS, not an OPN."*) (return ()))
      (los-received-state (close-conn connection) (return ()))
      (otherwise (close-conn connection) (return ())))))

