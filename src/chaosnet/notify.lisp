;;  -*- Mode:COMMON-LISP; Package:Chaos; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*-

1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;
;; Copyright (c) 1981 Massachusetts Institute of Technology 
;; Copyright (c) 1984-1989 Texas Instruments Incorporated.  All Rights Reserved.
;;
;; 10 APR 86 MMG - Moved here from Chaos_Servers (dependent on Converse).
;; 24 MAR 86 MMG - Conversion to Common Lisp.*
;;; 12-04-87 DAB - Fixed notify-server to not hand up when *more* occurs.
1;;
;;-----------------------------------------------------------------------------
;;                              ADVERTISED FUNCTIONS
;;
;;  SHOUT
;;  NOTIFY
;;  NOTIFY-ALL-LISPMS
;;
;;-----------------------------------------------------------------------------*

(defvar 4last-notification* ()
   2"This variable contains the last string which was a notification for another chaos host.
If you want an older notification, try looking in TV:NOTIFICATION-HISTORY"*) 

 1;;fix to use with-help-stream *

(defun 4shout* (&aux msg-text connect-to)	   1;SHOUT to all Lisp Machines.  *
  2"Send a message to all Lisp machines.  The message is read from the terminal."*
  (fs:force-user-to-login)
  (format t 3"~%Message: (terminate with ~:@C)~%"* #\End)
  (setq connect-to 3"SEND Everybody"*
	msg-text
	(zwei:qsend-get-message
	  *standard-input*
	  (format ()
		  3"You are typing a message which will be sent to everyone who is using a Lisp Machine.*~
                   3If you want to quit, hit the ~:@C key.  To end your message, hit the ~:@C key."*
		  #\Abort #\End)))
  (dolist (host (net:all-lispms))
    (when (send host :network-address-list :chaos)
      (with-open-stream (stream (open-stream host connect-to :error () :direction :output))
	(cond
	  ((not (errorp stream)) (format stream 3"~A@~A ~\\DATIME\\~%"* user-id si:local-host)
				 (send stream :string-out msg-text) (send stream :close))))))) 

(defun 4notify-get-message* (&optional (stream *query-io*))
 1;; Add default value for STREAM.  11/23/83 RAF@TI-CSL*
  (format stream 3"~&Message: (terminate with End)~%"*)
  (zwei:qsend-get-message stream
			  (format ()
				  3"You are typing in a message which will be send to someone on another chaosnet machine.
If you want to quit, hit the ~:@C key, to end your message hit the ~:@C key."*
				  #\Abort #\End))) 


(defun 4notify-server* ()
  (let* ((conn (listen 3"NOTIFY"*))
	 (pkt (read-pkts conn))
	 (pkt-string (subseq (string (pkt-string pkt)) 7) )
	 (host (foreign-address conn))
	 (inhibit-scheduling-flag t))
    (cond
      ((or (null last-notification) (not (string-equal last-notification pkt-string)))
       (setq last-notification pkt-string) (setq inhibit-scheduling-flag ())
       (setq host (or (si:get-host-from-address host :chaos) (format () 3"chaos host ~O"* host)))
       (let ((w:more-processing-global-enable nil))  ;temporarily turn off *more* so chaosnet will not 
    	 (tv:notify () 3"From ~A: ~A"* host pkt-string))) ;lock up. 12-04-87 DAB
      (t (return-array (prog1
			 pkt-string
			 (setq pkt-string ())))))
    (answer-string conn 3"Done"*))) 


(add-initialization 3"NOTIFY"* '(notify-server) () 'server-alist) 


(defun 4notify* (host &optional (message (notify-get-message)))
  2"Send a brief message MESSAGE to HOST.  It is printed as a notification.
If MESSAGE is omitted, it is read from the terminal."*
  (let ((pkt (simple host (string-append 3"NOTIFY "* message))))
    (if (stringp pkt)
      pkt
      (prog1
	(string-append (pkt-string pkt))
	(return-pkt pkt))))) 


(defun 4notify-all-lms* (&optional (message (notify-get-message)) &aux conns)
  2"Send a brief message MESSAGE to all Lisp machines.  It is printed as a notification.
If MESSAGE is omitted, it is read from the terminal."*
  (assure-enabled)
  (unwind-protect (progn
		   (loop with contact = (string-append 3"NOTIFY "* message)
			 for host in (net:all-lispms)
			 as address = (address-parse host)
			 when address
			 do (push (open-connection address contact) conns))
		   (process-wait-with-timeout 3"Notify"* (* 5 60)
					      #'(lambda (conns)
						  (loop for conn in conns always
						     (neq (state conn) 'rfc-sent-state)))
					      conns)
		   (loop for conn in conns as state = (state conn) do
		      (format t 3"~%~A: ~A"*
			      (si:get-host-from-address (foreign-address conn) :chaos)
			      (case state
				(answered-state (pkt-string (read-pkts conn)))
				(rfc-sent-state 3"Host not responding"*)
				(otherwise (format () 3"Connection in ~A?"* state))))))
    (mapc #'remove-conn conns))) 

