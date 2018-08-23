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


;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.*

;;; Edit History
;;;
;;;   Date    Patcher  Patch #  Description
;;; ----------------------------------------------------------
;;; 05-03-89    DAB     Fixed remote-disk-handler to make sure the unit return from decode-unit-argument
;;;                     is valid, otherwise return an error. 
;;; 11-10-88    DAB     Added code in MAC-disk to handle find-disk-partition for a microexplorer.
;;; 10-21-88    DAB     Added code to handle "mac-disk" command.
;;; 07-11-88    DAB     Change calls to tv:notify to fs:notify-nicely. This will not lock up the system when
;;;                      the user is not around to respond. [7533]
;;; 04-07-88    DAB     Make sure they don't overwrite the booted band.
;;; 12-23-86    ab       --     Fix for 2K page size.  
;;;

(add-initialization 3"REMOTE-DISK"*
		    '(process-run-function 2"REMOTE-DISK Server"* 'remote-disk-server) ()
		    'server-alist) 

1;;; *BJ* For *RWF**
(Defun remote-disk-server (&aux conn stream line cmd cmdx unit block n-blocks rqb block-pkt-1
			   block-pkt-2 block-pkt-3 disposep)
  (setq conn (listen "REMOTE-DISK" 31))
  (unwind-protect
      (progn
	(accept conn)
	(send tv:who-line-file-state-sheet :add-server conn "REMOTE-DISK")
	(setq stream (make-stream conn))
	(condition-case (condition)
	    (loop
	     (process-wait "Net Input"
			   #'(lambda (conn)
			       (or (read-pkts conn)
				   (neq (state conn) 'open-state)))
			   conn)
	     
	     (and (neq (state conn) 'open-state)
		  (return-from remote-disk-server ()))
	     
	     (setq line (read-line stream)	;Get a command line		    
		   cmdx (position #\Space (the string (string line)) :test #'char-equal)
		   cmd  (subseq (string line) 0 cmdx))
	     
	     (cond ((or (string-equal cmd "READ")
			(string-equal cmd "WRITE"))
		    (let ((*read-base* 10)
			  (pos cmdx)
			  temp-unit)  ; DAB 05-03-89
		      (setq unit (subseq (string line) (1+ pos)
					 (search (the string (string #\Space))
						 (the string (string line)) :start2 (1+ pos) :test #'char-equal)))
		      (setf temp-unit unit)
		      (setf (values unit disposep)
			    (si::decode-unit-argument (if (string-equal "NIL" unit)
							  (setq unit si::*default-disk-unit*)
							  unit)
						      "Chaos disk server"))
		      ;; DAB 05-03-89 Decode-unit argument should return us a logical unit or a closure.
                      ;; If nil or a symbol is returned, the unit must be invalid. Return an error.
		      (unless (or (integerp unit) (closurep unit))
			(ferror nil "Unknown disk unit ~a" temp-unit))
			
		      (setq pos (1+ (search (the string (string #\Space))
					    (the string (string line)) :start2 (1+ pos) :test #'char-equal)))
		      
		      (setf (values block pos)
			    (read-from-string line () 'si:no-eof-option :start pos))
		      
		      (setf (values n-blocks pos)
			    (read-from-string line () 'si:no-eof-option :start pos))
		      
		      (setq rqb ()))
		    
		    (unwind-protect
			(progn
			  (setq rqb (get-disk-rqb n-blocks)
				block-pkt-1 (get-disk-string rqb 0 484 t)
				block-pkt-2 (get-disk-string rqb 121 484 t)
				block-pkt-3 (get-disk-string rqb 242 56 t))
			  
			  (cond ((string-equal cmd "READ")
				 (disk-read rqb unit block)
				 ;; Give to net
				 (do ((block block (1+ block))
				      (n-blocks n-blocks (1- n-blocks)))
				     ((zerop n-blocks))
				   
				   ;; Transmit three packets from block in buffer
				   (si::transmit-partition-packet conn block-pkt-1)
				   (si::transmit-partition-packet conn block-pkt-2)
				   (si::transmit-partition-packet conn block-pkt-3)
				   
				   ;; Advance magic strings to next block
				   (%p-store-contents-offset (+ (%p-contents-offset block-pkt-1 3)
								si:disk-block-byte-size)
							     block-pkt-1 3)
				   
				   (%p-store-contents-offset (+ (%p-contents-offset block-pkt-2 3)
								si:disk-block-byte-size)
							     block-pkt-2 3)
				   (%p-store-contents-offset
				     (+ (%p-contents-offset block-pkt-3 3)
					si:disk-block-byte-size)
				     block-pkt-3 3)))
				(t
				 (when (EQ UNIT *DEFAULT-DISK-UNIT*)
				   (multiple-value-bind
				     (start length)
				       (si:find-disk-partition-for-read  si:*LOADED-BAND*)
				     (cond  ((null start) ())
					    ((> block start (+ start length)) ())
					    ((< block (+ block n-blocks) start) ())
					    (t 
					     (FERROR nil
						     "~& Do not attempt to write into the current band. You will lose")
					     ))))
				 
				 ;; Get from net
				 (do ((block block (1+ block))
				      (n-blocks n-blocks (1- n-blocks)))
				     ((zerop n-blocks))
				   ;; Get 3 packets and form a block in the buffer
				   ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
				   (si::receive-partition-packet conn block-pkt-1)
				   (si::receive-partition-packet conn block-pkt-2)
				   (si::receive-partition-packet conn block-pkt-3)
				   ;; Advance magic strings to next block
				   (%p-store-contents-offset
				     (+ (%p-contents-offset block-pkt-1 3)
					si:disk-block-byte-size)
				     block-pkt-1 3)
				   (%p-store-contents-offset
				     (+ (%p-contents-offset block-pkt-2 3)
					si:disk-block-byte-size)
				     block-pkt-2 3)
				   (%p-store-contents-offset
				     (+ (%p-contents-offset block-pkt-3 3)
					si:disk-block-byte-size)
				     block-pkt-3 3))
				 (disk-write rqb unit block))))
		      (and block-pkt-3
			   (return-array (prog1
					   block-pkt-3
					   (setf block-pkt-3 ()))))
		      (and block-pkt-2
			   (return-array (prog1
					   block-pkt-2
					   (setf block-pkt-2 ()))))
		      (and block-pkt-1
			   (return-array (prog1
					   block-pkt-1
					   (setf block-pkt-1 ()))))
		      (return-disk-rqb rqb))
		    (if disposep
			(si::dispose-of-unit unit)))
		   ((string-equal cmd "MAC-DISK") ;10-21-88 DAB
			(let ((*read-base* 10)
			      (pos cmdx)
			      result PKT sendlen FUNC (ARG-LIST ()) RETURN-Values)
			  (condition-case (condition)
			      (progn
			       (setq FUNC (subseq (string line) (1+ pos)
						  (search (the string (string #\Space))
							  (the string (string line)) :start2 (1+ pos) :test #'char-equal)))
			       (setq pos (1+ (search (the string (string #\Space))
						     (the string (string line)) :start2 (1+ pos) :test #'char-equal)))
			       (setf arg-list (read-from-string  (string line)  () 'si:no-eof-option :start pos))
			       (setf arg-list (subst sys:*default-disk-unit* '*default-disk-unit*  arg-list))
			       (when (eql (car arg-list) :return-value)  ;11-10-88 DAB
				 (setf  RETURN-Values T)
				 (setf arg-list (cdr arg-list)))
			       (setf result (if RETURN-Values
						(format nil "~a"
							(multiple-value-list
							  (apply (read-from-string func) arg-list)))
						(with-output-to-string (*standard-output*)
						  (setf arg-list (subst  *standard-output* 'stream arg-list))
						  (apply (read-from-string func) arg-list))))
			       ) ;progn
			    (error (setf result (format nil "~%ERROR: ~A" (send condition :report-string)))))
			    (doTIMES (x (CeilING (ARRAY-TOTAL-SIZE result) 484.)
				      (send-pkt conn (get-pkt) chaos:eof-op))
			    (setq pkt (GET-PKT))
			    (COPY-ARRAY-CONTENTS
			      (subseq result (* 484. x)
				      (+ (* 484. x)
					 (setq sendlen 
					    (min 484.
						 (- (ARRAY-TOTAL-SIZE result) (* 484. x))))))
			      (CHAOS:PKT-STRING PKT))
			    (setf (CHAOS:PKT-NBYTES PKT) sendlen)
			    (SEND-PKT CONN PKT)
			    ))
		      )
		   ((string-equal cmd "SAY")
		    (process-run-function "Notify" 'fs:notify-nicely	;07-11-88 DAB
					  ()
					  "REMOTE-DISK-SERVER:~A"
					  (subseq (string line) cmdx) ))
		   
		   ))
	  (error (close-conn conn (send condition :report-string)) (setf conn ())	;05/86 RWF
		 )))
    (send tv:who-line-file-state-sheet :delete-server conn)
    (and conn (remove-conn conn))
    )
  )



;(DEFUN REMOTE-DISK-SERVER (&AUX CONN STREAM LINE CMD CMDX UNIT BLOCK N-BLOCKS RQB BLOCK-PKT-1 BLOCK-PKT-2 BLOCK-PKT-3
;  DISPOSEP)
;  (SETQ CONN (LISTEN "REMOTE-DISK" 25))
;  (UNWIND-PROTECT (PROGN
;		   (ACCEPT CONN)
;		   (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN "REMOTE-DISK")
;		   (SETQ STREAM (MAKE-STREAM CONN))
;		   (CONDITION-CASE (CONDITION)
;		      (DO ()
;			  (NIL)
;			(PROCESS-WAIT "Net Input"
;				      #'(LAMBDA (CONN)
;					  (OR (READ-PKTS CONN) (NEQ (STATE CONN) 'OPEN-STATE)))
;				      CONN)
;			(AND (NEQ (STATE CONN) 'OPEN-STATE) (RETURN ()))
;			(SETQ LINE (READLINE STREAM);Get a command line

;			      CMDX
;			      (POSITION #\SPACE (THE STRING (STRING LINE)) :TEST #'CHAR-EQUAL)
;			      CMD (SUBSEQ (STRING LINE) 0 CMDX) )
;			(COND
;			  ((OR (STRING-EQUAL CMD "READ") (STRING-EQUAL CMD "WRITE"))
;			   (LET ((*READ-BASE* 10)
;				 (POS CMDX))
;			     (SETQ UNIT
;				   (SUBSTRING LINE (1+ POS)
;					      (SEARCH (THE STRING (STRING #\SPACE))
;						      (THE STRING (STRING LINE)) :START2
;						      (1+ POS) :TEST #'CHAR-EQUAL)))
;			     (SETF (VALUES UNIT DISPOSEP)
;				   (SI::DECODE-UNIT-ARGUMENT
;				    (IF (STRING-EQUAL "NIL" UNIT)
;				      (SETQ UNIT SI::*DEFAULT-DISK-UNIT*)
;				      UNIT)
;				    "Chaos disk server"))
;			     (SETQ POS
;				   (1+
;				    (SEARCH (THE STRING (STRING #\SPACE))
;					    (THE STRING (STRING LINE)) :START2 (1+ POS) :TEST
;					    #'CHAR-EQUAL)))
;			     (SETF (VALUES BLOCK POS)
;				   (READ-FROM-STRING LINE () 'SI:NO-EOF-OPTION :START POS))
;			     (SETF (VALUES N-BLOCKS POS)
;				   (READ-FROM-STRING LINE () 'SI:NO-EOF-OPTION :START POS))
;			     (SETQ RQB ()))
;			   (UNWIND-PROTECT (PROGN
;					    (SETQ RQB (GET-DISK-RQB N-BLOCKS)
;						  BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484 T)
;						  BLOCK-PKT-2 (GET-DISK-STRING RQB 121 484 T)
;						  BLOCK-PKT-3 (GET-DISK-STRING RQB 242 56 T))
;					    (COND
;					      ((STRING-EQUAL CMD "READ")
;					       (DISK-READ RQB UNIT BLOCK)
;					       ;; Give to net
;					       (DO ((BLOCK BLOCK
;						      (1+ BLOCK))
;						    (N-BLOCKS N-BLOCKS (1- N-BLOCKS)))
;						   ((ZEROP N-BLOCKS))
;						    ;; Transmit three packets from block in buffer
;						 (SI::TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-1)
;						 (SI::TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-2)
;						 (SI::TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-3)
;						 ;; Advance magic strings to next block
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-1 3)
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-2 3)
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-3 3)))
;					      (T
;					       ;; Get from net
;					       (DO ((BLOCK BLOCK
;						      (1+ BLOCK))
;						    (N-BLOCKS N-BLOCKS (1- N-BLOCKS)))
;						   ((ZEROP N-BLOCKS))
;						    ;; Get 3 packets and form a block in the buffer
;						    ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
;						 (SI::RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-1)
;						 (SI::RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-2)
;						 (SI::RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-3)
;						 ;; Advance magic strings to next block
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-1 3)
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-2 3)
;						 (%P-STORE-CONTENTS-OFFSET
;						  (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
;						     (* 4 PAGE-SIZE))
;						  BLOCK-PKT-3 3))
;					       (DISK-WRITE RQB UNIT BLOCK))))
;			     (AND BLOCK-PKT-3
;				(RETURN-ARRAY (PROG1
;						BLOCK-PKT-3
;						(SETF BLOCK-PKT-3 ()))))
;			     (AND BLOCK-PKT-2
;				(RETURN-ARRAY (PROG1
;						BLOCK-PKT-2
;						(SETF BLOCK-PKT-2 ()))))
;			     (AND BLOCK-PKT-1
;				(RETURN-ARRAY (PROG1
;						BLOCK-PKT-1
;						(SETF BLOCK-PKT-1 ()))))
;			     (RETURN-DISK-RQB RQB))
;			   (IF DISPOSEP
;			     (SI::DISPOSE-OF-UNIT UNIT)))
;			  ((STRING-EQUAL CMD "SAY")
;			   (PROCESS-RUN-FUNCTION "Notify" 'TV:NOTIFY () "REMOTE-DISK-SERVER:~A"
;						 (SUBSTRING LINE CMDX)))))
;		      (ERROR (CLOSE-CONN CONN (SEND CONDITION :REPORT-STRING)))))
;    (AND CONN (REMOVE-CONN CONN))))



