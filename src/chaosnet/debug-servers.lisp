;;; -*- Mode:Common-Lisp; Package:CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*-

1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151


;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; ----------------------------------------------------------------------
;;; Babel server
;;; ----------------------------------------------------------------------*


(defvar 4*babel-string**
   2" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"*
   2"This is the string which we send out over and over during the BABEL server."*) 


(defun 4babel-server* (&aux conn stream)
  2"Gives the same characters to a connection over and over.
Useful for debugging chaosnet."*
  (setq conn (listen 3"BABEL"*))
  (accept conn)
  (setq stream (make-stream conn))
  (condition-case () (loop (format stream *babel-string*))
     (remote-network-error (close-conn conn)))) 


(add-initialization 3"BABEL"* '(process-run-function 2"Babel Server"* 'babel-server) ()
		    'server-alist)

1;;; ----------------------------------------------------------------------
;;; Discard server
;;; ----------------------------------------------------------------------*


(defvar 4discard-server-on* :notify
  2"T means always allow DISCARD server requests, :NOTIFY means allow them but notify the user,
NIL means never to allow them, and :NOT-LOGGED-IN means allow them when no one is logged in."*) 


(defvar 4discard-server-connections* ()) 


(defun 4discard-server-function* (&aux pkt conn)
  (setq conn (listen 3"DISCARD"*))
  (cond
    ((and (null discard-server-on) (not (member user-id '(nil 3""*) :test #'equal)))
     (reject conn (format () 3"This machine is in use by ~A, try again later."* user-id))
     (return-from discard-server-function ()))
    ((eq discard-server-on :notify)
     (process-run-function 3"Notify"* 'tv:notify () 3"DISCARD server being used by ~A"*
			   (host-short-name (foreign-address conn)))
     (process-allow-schedule) (accept conn))
    (t (accept conn)))
  (push conn discard-server-connections)
  (send tv:who-line-file-state-sheet :add-server conn 3"DISCARD"*)
  (catch-error (loop (setq pkt (get-next-pkt conn)) (return-pkt pkt)) ())
  (setq discard-server-connections
	(delete conn (the list discard-server-connections) :test #'equal))
  (return-from discard-server-function (close-conn conn))
  ()) 


(add-initialization 3"Discard"* '(process-run-function 2"Discard Server"* 'discard-server-function)
		    () 'server-alist) 


1;;; ----------------------------------------------------------------------
;;; Echo Server
;;; ----------------------------------------------------------------------*


(defvar 4echo-server-on* :notify
  2"T means always allow ECHO server requests, :NOTIFY means allow them but notify the user,
NIL means never to allow them, and :NOT-LOGGED-IN means allow them when no one is logged in."*) 


(defvar 4echo-server-connections* ()) 


(defun 4echo-server-function* (&aux pkt conn)
  (setq conn (listen 3"ECHO"*))
  (cond
    ((and (null echo-server-on) (not (member user-id '(nil 3""*) :test #'equal)))
     (reject conn (format () 3"This machine is in use by ~A, try again later."* user-id))
     (return-from echo-server-function  ()))
    ((eq echo-server-on :notify)
     (process-run-function 3"Notify"* 'tv:notify () 3"ECHO server being used by ~A"*
			   (host-short-name (foreign-address conn)))
     (process-allow-schedule) (accept conn))
    (t (accept conn)))
  (push conn echo-server-connections)
  (send tv:who-line-file-state-sheet :add-server conn 3"ECHO"*)
  (catch-error (loop (setq pkt (get-next-pkt conn)) (send-pkt conn pkt)) ())
  (setq echo-server-connections (delete conn (the list echo-server-connections) :test #'equal))
  (return-from echo-server-function (close-conn conn))
  ()) 


(add-initialization 3"Echo"* '(process-run-function 2"Echo Server"* 'echo-server-function) ()
		    'server-alist) 


1;;; ----------------------------------------------------------------------*



(defun 4remote-echo* (&optional (host si:local-host) &aux chaos-stream form)
  (unwind-protect (progn
		    (setq chaos-stream (open-stream host 3"ECHO"* :error () :ascii-translation t))
		    (cond
		      ((errorp chaos-stream)
		       (format t 3"~&Error opening ECHO Server stream: ~A"* chaos-stream))
		      (t
		       1;; STREAM EXISTS, NOW SEND FORM TO ECHO*
		       (format t
			       3"~%Echo Serving ~A.
Type in forms to be echoed by remote host.  To quit press ABORT or type QUIT.~%"*
			       host)
		       (loop			1;*		1 (SETQ FORM (READ))*
			 (setq form (send *standard-input* :line-in)) (terpri)
			 (prin1 form chaos-stream) (send chaos-stream :tyo #\Newline)
			 (send chaos-stream :force-output)
			 (when (and (symbolp form) (string-equal (symbol-name form) 3"Quit"*))
			   (return ()))		1;*		1 (DO ((CHAR (TYI CHAOS-STREAM) (TYI CHAOS-STREAM)))*
						1;*		1     ((EQ CHAR #/END))*
						1;*		1   (SEND *STANDARD-OUTPUT* :TYO CHAR))*
			 (send *standard-output* :string-out (send chaos-stream :line-in))))))
    (unless (and chaos-stream (not (errorp chaos-stream)))
      (cli:close chaos-stream)))) 



(defparameter 4*echo-string**
	      (string-append *babel-string* *babel-string* *babel-string* *babel-string* *babel-string*
			     (subseq (string *babel-string*) 0 (mod max-data-bytes-per-pkt (length *babel-string*))) )) 



(defun 4echo* (&optional (host si:local-host) (limit 1747) (check-equal t) &aux conn pkt (sends 0) (matches 0) (mismatches 0))
  (unwind-protect (progn
		    (setq conn (connect host 3"ECHO"*))
		    (loop while (and (< sends limit) (eql (state conn) 'open-state)) do
			  (progn
			    (process-wait 3"Echo Turnaround"*
					  #'(lambda (conn sends limit)
					      (or (data-available conn)
						  (and (< sends limit) (may-transmit conn))
						  (not
						    (member (state conn) '(open-state foreign-state)
							    :test #'eq))))
					  conn sends limit)
			    (loop while (and (< sends limit) (may-transmit conn)) do
				  (progn
				    (setq pkt (get-pkt))
				    (setf (pkt-nbytes pkt) (length *echo-string*))
				    (without-interrupts
				      (%blt
					(%make-pointer-offset dtp-fix *echo-string*
							      (si:array-data-offset *echo-string*))
					(%make-pointer-offset dtp-fix pkt
							      (+ (floor first-data-word-in-pkt 2)
								 (si:array-data-offset pkt)))
					(ceiling (length *echo-string*) 4) 1))
				    (send-pkt conn pkt)
				    (incf sends)))
			    1;;*		1   (loop while (CHAOS:DATA-AVAILABLE CONN) do*
			    (when (data-available conn)
			      (progn
				(setq pkt (get-next-pkt conn))
				(when check-equal
				  (if (string-equal (pkt-string pkt) *echo-string*)
				      (progn
					(incf matches))
				      (progn
					(incf mismatches))))
				(return-pkt pkt))))))
    (unless (or (null conn) (errorp conn))
      (close-conn conn)))
  (values matches mismatches)) 


(defun 4echo-test* (&optional &special (limit 303237) (host si:local-host) (repeats 144))
  (let* ((total-time 0)
	 (average-time 0)
	 (average-rate 0)
	 (current-time 0)
	 (good-compares 0)
	 (bad-compares 0)
	 (start-time 0)
	 (end-time 0)
	 (total-size 0)
	 (current-rate 0)
	 (max-rate 0)
	 (min-time 3641077)
	 (good 0)
	 (bad 0)
	 (total-good-pkts 0)
	 (total-bad-pkts 0)
	 (total-pkts 0)
	 (bytes 0))
    (format *standard-output* 3"~&ECHO Test on ~a at "* si:local-host)
    (time:print-current-time)
    (send *standard-output* :fresh-line)
    (unwind-protect (catch
		      :abort
		      (loop for iteration from 0 below limit do
			    (condition-case (err)
				(progn
				 (setq start-time (get-universal-time))
				 (condition-case (inerr)
				     (multiple-value-setq (good bad)
							  (echo host repeats))
				   (abort (format *standard-output* 3"~&******* ABORTED *******"*)
					  (throw :abort
						 ())))
				 (setq end-time (get-universal-time))
				 (incf total-good-pkts good)
				 (incf total-bad-pkts bad)
				 (incf total-pkts good)
				 (incf total-pkts bad)
				 (setq current-time (- end-time start-time))
				 (setq bytes (* (+ good bad) max-data-bytes-per-pkt))
				 (setq current-rate (floor (* 10 bytes) current-time))
1;;*				1 (BEEP)*
				 )
			      (error
			       (progn
				 (format *standard-output*
					 3"~&******* Error on iteration ~d.:  ~a~&        at "*
					 iteration err)
				 (time:print-current-time)
				 (send *standard-output* :fresh-line)
				 (incf bad-compares)))
			      (:no-error
			       (progn
				 (format *standard-output*
					 3"~&******* Completion of iteration ~d. at "* iteration)
				 (time:print-current-time)
				 (format *standard-output* 3" in  ~d. seconds "* current-time)
				 (format *standard-output* 3" for ~d. bits/second ~%~%"* current-rate)
				 (incf good-compares)
				 (when (and current-time)
				   (setf min-time (min min-time current-time))
				   (setf max-rate (max max-rate current-rate))
				   (incf total-time current-time)
				   (incf total-size bytes)
				   (when (> good-compares 0)
				     (setq average-time (floor total-time good-compares)))
				   (when (> total-time 0)
				     (setq average-rate (floor (* 10 total-size) total-time)))))))))
      (format *standard-output* 3"~&Average compare time was ~d. seconds"* average-time)
      (format *standard-output* 3"~&Average compare rate was ~d. bits/second"* average-rate)
      (format *standard-output* 3"~&Maximum echo rate was    ~d. packets/second"*
	      (floor repeats min-time))
      (format *standard-output* 3"~&Maximum echo rate was    ~d. bits/second"* max-rate)
      (format *standard-output* 3"~&Batch successes were     ~d."* good-compares)
      (format *standard-output* 3"~&Batch failures were      ~d."* bad-compares)
      (format *standard-output* 3"~&Packet successes were    ~d."* total-good-pkts)
      (format *standard-output* 3"~&Packet failures were     ~d.~%"* total-bad-pkts)))) 

1;;; ----------------------------------------------------------------------*


(defun 4user-chargen* (&optional (host si:local-host) &optional (limit 1747) (target 2"DISCARD"*) &aux conn pkt)
  (unwind-protect (progn
		    (setq conn (connect host target))
		    (loop for j from 0 to limit while (eql (state conn) 'open-state) do
			  (progn
			    (setq pkt (get-pkt))
			    (setf (pkt-nbytes pkt) (length *echo-string*))
			    (without-interrupts
			      (%blt
				(%make-pointer-offset dtp-fix *echo-string*
						      (si:array-data-offset *echo-string*))
				(%make-pointer-offset dtp-fix pkt
						      (+ (floor first-data-word-in-pkt 2)
							 (si:array-data-offset pkt)))
				(ceiling (length *echo-string*) 4) 1))	1;*		1   (SET-PKT-STRING PKT *echo-STRING*)*
			    (send-pkt conn pkt))))
    (when (and conn (not (errorp conn)))
      (close-conn conn)))) 

