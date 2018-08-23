;;; -*- Mode:Common-Lisp; Package::CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;-*-



1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.*

;1; ----------------------------------------------------------------------
;; Generic EVAL Server*

(defflavor generic-eval-server
	   (conn
	     server-stream)
	   (net::generic-peek-server-mixin)
  :settable-instance-variables
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (generic-eval-server :host-object) ()
  (ignore-errors (send server-stream :foreign-host)))

(defmethod (generic-eval-server :close) ()
  (send server-stream :close))

(defmethod (generic-eval-server :server-current-p) ()
  (ignore-errors (send conn :server-current-p)))


(defvar 4eval-server-on* :notify
   2"T means always allow EVAL server requests, :NOTIFY means allow them but notify the user,
    NIL means never to allow them, and 
    :NOT-LOGGED-IN means allow them when no one is logged in."*) 


;;----------------------------------------------------------------------
(defvar 4eval-server-connections* ()) 

(defvar 4*eval-server-process-priority** -30 2"Priority at which the eval server runs at."*) 

1;Call this if you want to enable the eval server on your machine*
(defun 4eval-server-on* (&optional (mode t))
  2"Allow remote connections to this machine's EVAL server.
   MODE can be T, :NOTIFY, NIL and :NOT-LOGGED-IN.
   T means always allow EVAL server requests, :NOTIFY means allow them but notify the user,
   NIL means never to allow them, and :NOT-LOGGED-IN means allow them when no one is logged in."*
  (setq eval-server-on mode))

(defun 4eval-server-function* ()
  (let ((conn (listen 3"EVAL"*)))
    (cond ((and (null eval-server-on)
		(not (member user-id '(nil 3""*) :test #'equal)))
	   (reject conn (format nil 3"This machine is in use by ~A, try again later."* user-id))
	   (return-from eval-server-function nil))               
	  
	  ((eq eval-server-on :notify)
	   (process-run-function 3"Notify"* 'tv:notify () 3"EVAL server being used by ~A"*
				 (host-short-name (foreign-address conn)))
	   (process-allow-schedule)
	   (accept conn))
	  (t (accept conn)))
    (push conn eval-server-connections)

    (return-from eval-server-function
      (catch-error
	(with-open-stream (stream (make-stream conn :ascii-translation t))
			      (let ((generic-server (make-instance 'generic-eval-server
								   :conn conn
								   :server-stream stream)))
				(send tv:who-line-file-state-sheet
				      :add-server generic-server "EVAL"
				      si:current-process))
			  (do* ((*terminal-io* stream)	1; Don't blow away machine on lossage*
				(input (read stream nil 'quit) (read stream nil 'quit)))
			       ((and (symbolp input) (string-equal (symbol-name input) 3"Quit"*))
				nil)
			    (catch-error
			      (dolist (value (multiple-value-list (eval input)))
				      (format stream 3"~s~%"* value))
			      t)
			    (write-char #\End stream)
			    (send stream :force-output)))
	()))
    ()))


(defun 4remote-eval* (host &aux form)
  (with-open-stream (chaos-stream (open-stream host 3"EVAL"* :ascii-translation t))
		    (format t 3"~%Eval Service ~A.~
                               ~%Type in forms to be evaluated on remote host.  To quit press ABORT.~%"*
			    host)
		    (loop (setq form (read *standard-input* t))
			  (format *standard-output*  3"~&"*)
			  (format chaos-stream 3"~s~%"* form)
			  (send chaos-stream :force-output)
			  (when (and (symbolp form)
				     (string-equal (symbol-name form) 3"Quit"*))
				(return ()))
			  
			  (do ((char (read-char chaos-stream) (read-char chaos-stream)))
			      ((eql char #\End))
			    (write-char char *standard-output*)))))


(add-initialization 3"EVAL"* '(process-run-function (list :name 2"EVAL Server"* :priority *eval-server-process-priority*)
						  'eval-server-function) ()
		    'server-alist)
