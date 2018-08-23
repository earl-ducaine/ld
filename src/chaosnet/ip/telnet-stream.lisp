;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(CPTFONT HL12B CPTFONT); Base:10.-*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986,1988 Texas Instruments Incorporated.  All rights reserved.*

;1;;-------------------------------------------------------------------------------------*
;1;;                       T E L N E T   S T R E A M *
;1;;*
;1;; ADVERTISED FUNCTIONS:*
;1;;   open-telnet-stream*
;1;;   make-telnet-stream*
;1;;*
;1;; ADVERTISED METHODS OF TELNET STREAM*
;1;;   :set-echo-preference, :set-transmit-mode,  :send-synch, :send-erase-line, :send-erase-character, *
;1;;   :send-ip, :send-abort-output, :send-are-you-there, :send-user-out-of-band, :send-escape-as-data*
;1;;   :send-telnet-command, :send-telnet-option, :process-command-responses, :set-remote-echo-preference*
;1;;*
;1;; HISTORY:*
;1;;   7/--/85 - KCW - initial version (adapted some code from existing TELNET)*
;1;;   7/23/85 - RLA - continued (moved code from TCP streams, added new code)*
;1;;-------------------------------------------------------------------------------------*

;1;; TELNET COMMAND ESCAPE CHARACTER*

(DEFPARAMETER nvt-iac 255) 

;1;; COMMANDS FOR OPTION NEGOTIATION*

(DEFPARAMETER nvt-will 251) 

(DEFPARAMETER nvt-wont 252) 

(DEFPARAMETER nvt-do 253) 

(DEFPARAMETER nvt-dont 254) 

(DEFPARAMETER negotiation-list (LIST nvt-will nvt-wont nvt-do nvt-dont)) 

;1;;COMMANDS*

(DEFPARAMETER nvt-se 240)    ;1 subnegotiation end*

(DEFPARAMETER nvt-nop 241)   ;1 no operation*

(DEFPARAMETER nvt-dm 242)       ;1 data mark*

(DEFPARAMETER nvt-break 243)   ;1 break*

(DEFPARAMETER nvt-ip 244)      ;1interrupt process*

(DEFPARAMETER nvt-ao 245)      ;1abort output*

(DEFPARAMETER nvt-ayt 246)     ;1are you there?*

(DEFPARAMETER nvt-ec 247)      ;1erase character*

(DEFPARAMETER nvt-el 248)      ;1erase line*

(DEFPARAMETER nvt-ga 249)    ;1 go ahead*

(DEFPARAMETER nvt-sb 250)    ;1 subnegotiation begin*


(DEFPARAMETER nvt-list-character 400)  ;1experimenting*

(DEFPARAMETER nvt-bell 7)      ;1bell*

;1;;OPTIONS*

(DEFPARAMETER nvt-echo 1)    ;1 the echo option*

(DEFPARAMETER nvt-suppress-go-ahead 3)  ;1the suppress go ahead option*


(DEFPARAMETER telnet-default-port 23) 


(DEFPARAMETER boring-urgent-signals (LIST nvt-ec nvt-el))  ;1As opposed to "interesting" urgent signals*


(DEFPARAMETER requested-local '(:requested :local)) 

(DEFPARAMETER requested-remote '(:requested :remote)) 

(DEFPARAMETER requested-nil '(:requested nil)) 

;1;; Signals for AO and IP (must be caught by user process)*

(DEFSIGNAL telnet-stream-abort-output system:network-error () "AO signal") 


(DEFSIGNAL telnet-stream-interrupt-process system:network-error () "IP signal") 


(DEFVAR abort-output-condition ()) 

(DEFVAR interrupt-process-condition ()) 



(DEFVAR *telnet-stream-debug* ()) 

;1;;-------------------------------------------------------------------------------------*


(DEFFLAVOR telnet-stream-mixin
	   ((transmit-mode :line);1VALUES = :character, :line, :stream*
	    (command-mode nil);1t => dont force output after tyo even if in char mode*
	    (remote-echo-preference nil);1VALUES = :local, :remote, :offer, :ask, :never, nil*
	    (remote-echo nil);1VALUES = :local, :remote, nil, (:requested x)*
	    (local-suppress-go-ahead nil)
	    (remote-suppress-go-ahead nil)
	    (split-command nil);1:seen-IAC / number (=> seen command, need option) / nil*
	    (resume-buffer);1used for resuming urgent processing after IP or AO signal*
	    (resume-startix nil)
	    (resume-copyix nil)
	    (resume-endix nil)
	    (resume-urgentlen nil))
	   ()
  (:included-flavors basic-input-stream)
  (:inittable-instance-variables transmit-mode remote-echo-preference)
  (:gettable-instance-variables transmit-mode remote-echo-preference remote-echo)
  (:required-methods :tyi :tyo :next-input-buffer :check-pending-output)) 

;1;; This is the instantiable flavor for a TCP-based telnet stream*

(DEFFLAVOR telnet-stream
	   ()
	   (telnet-stream-mixin
	    ascii-translating-character-stream)) 

;1;; Force output after every character (except for commands) if in character mode, force output after a line feed*
;1;; if in line mode (note that ascii-translation will always stick in a lf with a cr).*
;1; I THINK THIS WILL WORK EVEN WITH THE ASCII TRANSLATION WRAPPER *

(DEFMETHOD (telnet-stream-mixin :after :tyo) (ch)
  (UNLESS command-mode
    (WHEN (OR (eq transmit-mode :character)
	      (AND (char-equal ch #\cr) (EQ transmit-mode :line)))
      (SEND self :force-output)))
  ch) 


(DEFMETHOD (telnet-stream-mixin :around :tyi) (cont mt args &optional ignore &aux ch)
  (SETQ ch (AROUND-METHOD-CONTINUE cont mt args))
  (WHEN (AND ch (EQ remote-echo :local))
    (CASE ch
	    ;1; Assume these were put in the same way - will be (re)translated on other end*
       (#\Backspace (SEND self :send-erase-character))
       (#\Clear-input (SEND self :send-erase-line)) (:otherwise (SEND self :tyo ch))))
  ch) 

;1; This doesn't guarantee that TCP doesn't have pending stuff*

(DEFMETHOD (si:basic-buffered-output-stream :check-pending-output) ()
  (AND si::stream-output-buffer (< si::stream-output-index si::stream-output-limit))) 


(DEFMETHOD (telnet-stream-mixin :set-remote-echo-preference) (preference)
 ;1; DONT MAKE THIS CHECK FOR NOW (DOESN'T REALLY GUARANTEE ANYTHING)*
 ;1; Check for stable state (no pending input or output)*
 ;1x  (IF (OR (SEND self :check-pending-output) (LISTEN self))*
 ;1x     (then *
 ;1x        :ERROR)*
 ;1x     (else*
  (SETQ remote-echo-preference preference)
  (SEND self :negotiate-echo)
  (SEND self :process-command-responses)
  ;1; Return the new state*
  remote-echo;1x     ))*
) 


(DEFMETHOD (telnet-stream-mixin :set-transmit-mode) (mode)
  (WHEN (EQ transmit-mode :character)
    (SEND self :force-output))
  (SETQ transmit-mode mode)) 



(DEFMETHOD (telnet-stream-mixin :negotiate-echo) ()
 ;1; NOTE that this may be called either initially or when user preference changes, so don't assume default is true.*
  (SELECT remote-echo-preference
     (:local (SEND self :send-telnet-option nvt-will nvt-echo ())
      (SEND self :send-telnet-option nvt-dont nvt-echo ()) (SETQ remote-echo requested-local))
     (:offer (SEND self :send-telnet-option nvt-will nvt-echo ())
      (SETQ remote-echo requested-local))
     (:remote (SEND self :send-telnet-option nvt-do nvt-echo ())
      (SEND self :send-telnet-option nvt-wont nvt-echo ()) (SETQ remote-echo requested-remote))
     (:ask (SEND self :send-telnet-option nvt-do nvt-echo ())
      (SETQ remote-echo requested-remote))
     (:never (SEND self :send-telnet-option nvt-dont nvt-echo ())
      (SEND self :send-telnet-option nvt-wont nvt-echo ()) (SETQ remote-echo requested-nil))
     (:otherwise;1nil*
      (ASSERT (NOT (CONSP remote-echo)));1not already in :requested state*
      (SELECT remote-echo (:local (SEND self :send-telnet-option nvt-wont nvt-echo ()))
	 (:remote (SEND self :send-telnet-option nvt-dont nvt-echo ())))
      (WHEN remote-echo
	(SETQ remote-echo requested-nil))));1otherwise leave nil*
  (SEND self :force-output)) 
     

(DEFMETHOD (telnet-stream-mixin :negotiate-options) ()
  (SEND self :send-telnet-option nvt-do nvt-suppress-go-ahead ())
  (SEND self :send-telnet-option nvt-will nvt-suppress-go-ahead ())
  (SEND self :negotiate-echo)
  (SETF local-suppress-go-ahead :requested
	remote-suppress-go-ahead :requested)
  (SEND self :process-command-responses)) 



(DEFMETHOD (telnet-stream-mixin :send-telnet-command) (command &optional (FORCE-OUTPUT t) urgent)
  (WHEN *telnet-stream-debug*
    (PRINT (FORMAT () "STREAM: ~A  SND COMMAND: ~A"
		   self command)))
  (SETQ command-mode t);1disable force-output after every character*
  (WHEN urgent
    (SEND self :turn-on-urgent-mode))
  (SEND self :tyo nvt-iac)
  (SEND self :tyo command)
  (WHEN urgent
    (SEND self :turn-off-urgent-mode))
  (WHEN force-output
    (SEND self :force-output))
  (SETQ command-mode ())) 


(DEFMETHOD (telnet-stream-mixin :send-telnet-option) (command option &optional (FORCE-OUTPUT t))
  (WHEN *telnet-stream-debug*
    (PRINT (FORMAT () "STREAM: ~A  SND COMMAND: ~A  OPTION ~A"
		   self command option)))
  (SETQ command-mode t);1disable force-output after every character*
  (SEND self :tyo nvt-iac)
  (SEND self :tyo command)
  (SEND self :tyo option)
  (WHEN force-output
    (SEND self :force-output))
  (SETQ command-mode ())) 


(DEFMETHOD (telnet-stream-mixin :process-command-responses) ()
 ;1;; Reading the AYT response string will guarantee that any intermediate commands are seen and processed*
  (SEND self :send-are-you-there t)) 		

;1;; These methods are available for commands which might be issued by a user process*


(DEFMETHOD (telnet-stream-mixin :send-are-you-there) (&optional get-response send-synch)
  (SEND self :send-telnet-command nvt-ayt)
  (WHEN send-synch
    (SEND self :send-synch))
  (WHEN get-response
    (READ-LINE self))) 

;1;; SEND-SYNCH*
;1;; The TELNET out-of-band signal.*

(DEFMETHOD (telnet-stream-mixin :send-synch) ()
  (SEND self :send-telnet-command nvt-dm t t)) 


(DEFMETHOD (telnet-stream-mixin :send-abort-output) ()
  (SEND self :turn-on-urgent-mode)		;1do it this way to keep in same buffer*
  (SEND self :send-telnet-command nvt-ao ())
  (SEND self :send-telnet-command nvt-dm ())	;1sync*
  (SEND self :turn-off-urgent-mode)
  (SEND self :force-output)) 		


(DEFMETHOD (telnet-stream-mixin :send-ip) ()
  (SEND self :turn-on-urgent-mode)		;1do it this way to keep in same buffer*
  (SEND self :send-telnet-command nvt-ip ())
  (SEND self :send-telnet-command nvt-dm ())	;1sync*
  (SEND self :turn-off-urgent-mode)
  (SEND self :force-output)) 


(DEFMETHOD (telnet-stream-mixin :send-erase-line) ()
  (SEND self :send-telnet-command nvt-el ()))	;1dont force-output*
 

(DEFMETHOD (telnet-stream-mixin :send-erase-character) ()
  (SEND self :send-telnet-command nvt-ec ()))  ;1dont force-output *


(DEFMETHOD (telnet-stream-mixin :send-escape-as-data) ()
  (SEND self :tyo nvt-iac)
  (SEND self :tyo nvt-iac)) 

;1;; SEND-USER-OUT-OF-BAND*
;1;; Just as the TCP Urgent notification is needed at the TELNET level as an*
;1;; out-of-band signal, so other protocols which make use of TELNET may *
;1;; require a TELNET command which can be viewed as an out-of-band signal*
;1;; at a different level.*
;1;; This method enables a 'different level' protocol, using the TELNET stream,*
;1;; to send such out-of-band signals. *

(DEFMETHOD (telnet-stream-mixin :send-user-out-of-band) (command &optional analog-dm)
  (SEND self :send-ip)				;1will also send-synch *
  (SEND self :string-out command)
  (IF analog-dm
      (SEND self :string-out analog-dm))	;1Send the 'character' (can be a string) analogous to the TELNET DM,*
						;1 which signifies the end of urgent processing.*
  (SEND self :force-output)) 




(DEFMETHOD (telnet-stream-mixin :around :next-input-buffer)
	   (cont mt args &optional ignore	;1(no-hang-p)*
	    &aux rtnbuf startix endix urgentlen copyix data-following-urgent (finished-urgent-data t))
  (LOOP
    (UNLESS data-following-urgent
      (COND
	(resume-startix
	 ;1; in the middle of data processing when we signalled an IP or AO *
	 (SETQ rtnbuf resume-buffer)
	 (SETQ startix resume-startix)
	 (SETQ endix resume-endix)
	 (SETQ urgentlen resume-urgentlen)
	 (SETQ copyix resume-copyix)
	 (SETQ resume-startix ()))		;1clear for next time*
	(t					;1 get next buffer full*
	 (MULTIPLE-VALUE-SETQ (rtnbuf startix endix)
			      (AROUND-METHOD-CONTINUE cont mt args))
	 (SETF urgentlen (COND
			   ((NULL urgent-input-index) 0)
			   (t (- urgent-input-index startix))))
	 (SETQ resume-buffer rtnbuf)
	 (SETQ copyix startix)
	 (SETQ resume-copyix copyix)
	 (WHEN (NOT rtnbuf)
	   (RETURN ())))))
    (SETQ finished-urgent-data (AND finished-urgent-data (ZEROP urgentlen)))
    (COND
      ((OR finished-urgent-data data-following-urgent) (SETQ data-following-urgent ())
						       ;1; strip telnet commands from buffer*
						       (SETQ endix (process-buffer rtnbuf startix endix copyix))
						       ;1; Don't return unless we have something for the user*
						       ;1; (let no-hang wait until there is really nothing - not even commands)*
						       (WHEN (> endix startix)
							 (RETURN (VALUES rtnbuf startix endix))))
      (t
       (MULTIPLE-VALUE-SETQ (finished-urgent-data data-following-urgent)
			    (process-urgent-buffer rtnbuf startix urgentlen endix))
       (WHEN data-following-urgent
	 (SETQ startix data-following-urgent)))))) 



(DEFUN process-buffer (buffer index end-of-data copy-cursor &aux command (command-length 0) option startix last-index)
  (DECLARE (:self-flavor telnet-stream));1(FORMAT t "~%process buffer: datalen=~D, data=" (- end-of-data index))*
  ;1(LOOP for i from index to (1- end-of-data) do (SEND *standard-output* :tyo (AREF buffer i)))*
  (SETQ startix index)
  (SETQ last-index index)
  (LOOP
   (MULTIPLE-VALUE-SETQ (index command-length command option)
     (scan-for-interesting-command buffer last-index end-of-data))
   (SETQ copy-cursor (move-array-portion buffer last-index index copy-cursor))
   ;1(FORMAT t "~%command @~D, len=~D" index command-length)*
   ;1(FORMAT t "~%copy cursor = ~D" copy-cursor)*
   (WHEN (ZEROP command-length)
     (RETURN));1no more commands*
   (SETQ last-index (+ index command-length))
   ;1; set up for resume after interrupt*
   (SETQ resume-startix last-index);1resume even if at end of buffer so can return data copied*
   (SETQ resume-urgentlen 0) (SETQ resume-copyix copy-cursor) (SETQ resume-endix end-of-data)
     (SETQ copy-cursor (process-command buffer copy-cursor command option))
     (SETQ resume-startix ()));1came back ok*
  ;1; If have data and echo state is still (:requested x), then assume x (no negative response)  --- >> OK?*
  (WHEN (AND (> copy-cursor startix) (CONSP remote-echo))
    (SETQ remote-echo (SECOND remote-echo)))
  copy-cursor) 


(DEFUN move-array-portion (buffer startix stopix target &aux length)
  (SETQ length (- stopix startix))
  (UNLESS (EQ startix target)
    (LOOP for i from 0 to (1- length) do
       (SETF (AREF buffer (+ target i)) (AREF buffer (+ startix i)))))
  (+ target length)) 				;1return next available position*



(DEFUN process-urgent-buffer (buffer startix urgent-length end-of-data &aux new-index end-of-urgent-data command-length
  command option)
 ;1; urgent-length is relative to startix*
  (DECLARE (:self-flavor telnet-stream))
  (SETQ end-of-urgent-data
	(LOOP with index = startix with urgent-end = (+ startix urgent-length) do
	   (PROGN
	     (MULTIPLE-VALUE-SETQ (index command-length command option)
	       (scan-for-interesting-command buffer index end-of-data boring-urgent-signals))
	     (WHEN (NULL index)
	       (RETURN ()))
	     ;1; set up for resume in case command causes an interrupt*
	     (SETQ new-index (+ index command-length))
	     (IF (< new-index end-of-data)
	       (SETQ resume-startix new-index)
	       (SETQ resume-startix ()));1dont resume if finished*
	     (SETQ resume-urgentlen (- urgent-length (- new-index startix)))
	     (SETQ resume-endix end-of-data)
	     (process-command buffer index command option)
	     (WHEN (AND (EQ command nvt-dm) (>= new-index urgent-end))
	       (RETURN new-index));1 first IAC/DM at or after end of urgent data marks return to normalcy*
	     (UNLESS resume-startix
	       ())
	     (SETQ index new-index))))
  (SETQ resume-startix ())
  (VALUES end-of-urgent-data;1return whether or not we are finished w/ urgent data (DM found)*
	  (WHEN (AND end-of-urgent-data (< end-of-urgent-data end-of-data))
	    end-of-urgent-data))) 			;1return index past terminating DM where normal data begins*



;1;; SCAN-FOR-INTERESTING-COMMAND*
;1;; Return the index of the next interesting command and its length (plus the command and any associated option).*
;1;; If there are no more interesting commands in the buffer, return the index past the end and command-length of 0*
;1;; If the command is split across a buffer, set up split-command to reflect this (used on subsequent entry)*
;1;; and return the index where the partial command starts but a command-length of 0.*

(DEFUN scan-for-interesting-command (buffer index end &optional list-of-boring-commands &aux (command nil) (option nil) bias
  command-length)
  (DECLARE (:self-flavor telnet-stream));1(FORMAT t "~%scan index=~D, end=~D, split=~D" index end split-command)*
  (LOOP
   (COND
     ((NUMBERP split-command)
      ;1; seen IAC & command, waiting for option*
      (SETQ command split-command) (SETQ bias -2))
     (t
      (COND
	(split-command (SETQ bias -1));1seen IAC, waiting for command*
	(t (SETQ bias 0)
	 (SETQ index
	       (position nvt-iac buffer :start index
		       :end end :test #'CHAR-EQUAL))))
      (UNLESS index
	(RETURN (VALUES end 0)))
      (COND
	((>= (+ index bias 1) end)
	 ;1; split across buffer*				
	 (SETQ split-command :seen-iac) (RETURN (VALUES index 0)))
	(t (SETQ command (AREF buffer (+ index bias 1)))))))
   (SETQ split-command ())
   ;1; Special case IAC / IAC (escape sequence to allow IAC to appear in the data stream)*
   ;1; If some commands are "boring", continue search.  Otherwise, return as a command (to be treated as a noop),*
   ;1; but leave index set so that one IAC will be left in the buffer.  Note that if the first IAC ended a previous*
   ;1; buffer it has in effect already been discarded and so this one can just be treated as a character.*
   (COND
     ((EQ command nvt-iac) (SETQ command-length 2)
			   (UNLESS (OR list-of-boring-commands (EQ bias -1))
			     (RETURN (VALUES (1+ index) 1 nvt-iac ()))))
     (t
      (UNLESS (MEMBER command list-of-boring-commands :test #'EQUAL)
	(COND
	  ((MEMBER command negotiation-list :test #'EQUAL)
	   ;1; command will be followed by an option*				
	   (COND
	     ((>= (+ index bias 2) end)
	      ;1; have to get option from next buffer*
	      (SETQ split-command command) (RETURN (VALUES index 0)))
	     (t (SETQ command-length 3) (SETQ option (AREF buffer (+ index bias 2))))))
	  (t (SETQ command-length 2)))
	(RETURN (VALUES index (+ command-length bias) command option)))))
   (INCF index (+ command-length bias)))) 

;1;; PROCESS-COMMAND*

(DEFUN process-command (buffer cursor command &optional option)
  ;1; buffer and cursor are passed in case some commands may need to manipulate the buffer.*
  ;1; the new (or unmodified) cursor position is returned.*
  (DECLARE (:self-flavor telnet-stream))
  (WHEN *telnet-stream-debug*
    (PRINT (FORMAT () "STREAM: ~A  RCV COMMAND: ~A  OPTION: ~A"
		   self command option)))
  (SELECT command (nvt-do			;1do options*
		   (telnet-stream-do-option option))
	  (nvt-dont				;1dont options*
	   (telnet-stream-dont-option option)) (nvt-will	;1will options*
						(telnet-stream-will-option option))
	  (nvt-wont				;1wont options*
	   (telnet-stream-wont-option option))
	  (nvt-ayt				;1 are you there*
	   (SEND self :string-out (FORMAT () "~A"
					  self))
	   (SEND self :string-out " at EXPLORER host ")
	   (SEND self :line-out (SEND si:local-host :name)) (SEND self :force-output))
	  (nvt-el				;1 erase line *
	   ;1; Originally did this by changing cursor, but that works only on currently-buffered stuff*
	   (SETF (AREF buffer cursor) #\Clear-input) (INCF cursor))
	  (nvt-ec				;1 erase character*
	   ;1; Originally did this by changing cursor, but that works only on currently-buffered stuff*
	   (SETF (AREF buffer cursor) #\Backspace) (INCF cursor))
	  (nvt-ip				;1 interrupt process*
	   (IF interrupt-process-condition
	       (SEND interrupt-process-condition :set-initial-error-message-printed-p ())
	       (SETQ interrupt-process-condition
		     (MAKE-CONDITION 'telnet-stream-interrupt-process
				     "TELNET INTERRUPT-PROCESS SIGNAL WAS NOT HANDLED"
				     :proceed-types ())))
	   (SIGNAL-CONDITION interrupt-process-condition))
	  (nvt-ao				;1 abort output*
	   (IF abort-output-condition
	       (SEND abort-output-condition :set-initial-error-message-printed-p ())
	       (SETQ abort-output-condition
		     (MAKE-CONDITION 'telnet-stream-abort-output
				     "TELNET ABORT-OUTPUT SIGNAL WAS NOT HANDLED"
				     :proceed-types ())))
	   (SIGNAL-CONDITION abort-output-condition))
	  (nvt-ga)				;1 go ahead (ignore this for now)*
	  (nvt-dm)				;1 data mark (noop when not in urgent mode)*
	  (nvt-iac))				;1 IAC / IAC - do nothing (one will be left in buffer)*
  cursor) 



(DEFUN telnet-stream-do-option (option)
  (DECLARE (:self-flavor telnet-stream))
  (SELECT option
    (nvt-echo
     (UNLESS (EQUAL remote-echo :local)
       (SELECT remote-echo-preference
	 (:remote
	  ;1; We will let them do remote echoing but we*
	  ;1; will never to remote echoing.*
	  (SEND self :send-telnet-option nvt-wont nvt-echo))
	 (:local
	  ;1; We will do remote echoing but we never let them *
	  ;1; echo for us.*
	  (UNLESS (EQUAL remote-echo requested-local)
	    (SEND self :send-telnet-option nvt-will nvt-echo))
	  (SETQ remote-echo :local))
	 (:ask
	  ;1; We prefer to do local-echoing, but are flexible*
	  (SETQ remote-echo :remote)		;1do as requested (even if we asked for the opposite)*
	  (SEND self :send-telnet-option nvt-will nvt-echo))
	 (:offer
	  ;1; We prefer to do remote echoing.*
	  (SETQ remote-echo :local)		;1do as requested*
	  (UNLESS (EQUAL remote-echo-preference requested-local)
	    (SEND self :send-telnet-option nvt-will nvt-echo)))
	 (:never (SEND self :send-telnet-option nvt-wont nvt-echo))	;1nope*
	 (nil					;1turned-off, but flexible*
	  (SETQ remote-echo :local)		;1do as requested*
	  (SEND self :send-telnet-option nvt-will nvt-echo))
	 ;1; Else this command request does not change our mode, so we do nothing.*
	 
	 )))					;1NVT-ECHO*
    (nvt-suppress-go-ahead
     (WHEN (NEQ local-suppress-go-ahead t)
       (UNLESS (EQ local-suppress-go-ahead :requested)
	 (SEND self :send-telnet-option nvt-will nvt-suppress-go-ahead))
       ;1; we will now begin suppressing transmission of GAs with *
       ;1; transmitted data characters*
       (SETQ local-suppress-go-ahead t))
     ;1; Else this command request does not change our mode, so we do nothing.*
     )
    (:otherwise (SEND self :send-telnet-option nvt-wont option)))) 


(DEFUN telnet-stream-dont-option (option)
  (DECLARE (:self-flavor telnet-stream))
  (SELECT option
    (nvt-echo
     (SELECTOR remote-echo equal (requested-local (SETQ remote-echo ()))	;1they rejected our request*
	       (:local (SETQ remote-echo ())
		       (SEND self :send-telnet-option nvt-wont nvt-echo))	;1acknowledge change of mode*
	       ((:remote requested-remote nil))))	;1ignore*
    (nvt-suppress-go-ahead (SEND self :send-telnet-option nvt-will nvt-suppress-go-ahead)	;1refuse*
			   (tv:notify () "~A: Other side requested go-aheads, not implemented here"
				      self)	;1x     (WHEN (eq local-suppress-go-ahead t)*
						;1x        (SEND self :send-telnet-option NVT-WONT NVT-SUPPRESS-GO-AHEAD))*
						;1x     (SETQ local-suppress-go-ahead nil)*
			   ;1; Otherwise, this command request does not change our mode,*
			   ;1;so we do nothing.*
			   ))) 


(DEFUN telnet-stream-will-option (option)
  (DECLARE (:self-flavor telnet-stream))
  (SELECT option
    (nvt-echo
     (UNLESS (EQ remote-echo :remote)
       (SELECT remote-echo-preference
	 (:never (SEND self :send-telnet-option nvt-dont nvt-echo))	;1never echo*
	 (nil					;1turned-off, but flexible*
	  (SETQ remote-echo :remote)		;1do as requested*
	  (SEND self :send-telnet-option nvt-do nvt-echo))
	 (:remote
	  (UNLESS (EQUAL remote-echo requested-remote)
	    (SEND self :send-telnet-option nvt-do nvt-echo))
	  (SETQ remote-echo :remote))
	 (:local (SEND self :send-telnet-option nvt-dont nvt-echo))	;1never allow*
	 (:offer				;1this isn't what we wanted but ok*
	  (SETQ remote-echo :remote) (SEND self :send-telnet-option nvt-do nvt-echo))
	 (:ask
	  (UNLESS (EQUAL remote-echo requested-remote)
	    (SEND self :send-telnet-option nvt-do nvt-echo))
	  (SETQ remote-echo :remote)))))
    (nvt-suppress-go-ahead
     (UNLESS (EQ remote-suppress-go-ahead :requested)
       (SEND self :send-telnet-option nvt-do nvt-suppress-go-ahead))
     (SETQ remote-suppress-go-ahead t))
    (:otherwise
     ;1; refuse any other requests because we dont know how to do any thing else*
     (SEND self :send-telnet-option nvt-dont option)))) 


(DEFUN telnet-stream-wont-option (option)
  (DECLARE (:self-flavor telnet-stream))
  (SELECT
    option
    ;1; We may need to do other things in user code depending on *
    ;1; whether we are buffering data to hide the uncertainty period*
    (nvt-echo
     (SELECTOR remote-echo equal (requested-remote (SETQ remote-echo ()))	;1rejected request*
	       (:remote (SETQ remote-echo ())
			(SEND self :send-telnet-option nvt-dont nvt-echo))	;1acknowledge change of mode*
	       ((:local requested-local nil))	;1ignore*
	       ))
    (nvt-suppress-go-ahead
     (WHEN (EQ remote-suppress-go-ahead t)	;1acknowledge change of mode*
       (SEND self :send-telnet-option nvt-dont nvt-suppress-go-ahead))
     (SETQ local-suppress-go-ahead ()))))

;1;*
;1; the following methods are used by peek and the wholine code when the telnet stream is used by an FTP server*
;1;*

(DEFMETHOD (telnet-stream :name) ()
  (SEND (SEND self :foreign-host) :name))


(DEFMETHOD (telnet-stream :host-object)  ()
  (SEND self :foreign-host))


(DEFMETHOD (telnet-stream :peek-host-menu) (&rest args)
  (APPLY 'net:peek-host-menu args))


(DEFMETHOD (telnet-stream :peek-server-connection-menu) (item)
  (IGNORE item)
  (PROCESS-RUN-FUNCTION "Peek Connection Menu" #'ftp-server-control-connection-menu self))


(COMPILE-FLAVOR-METHODS telnet-stream) 

;1;------------------------------------------------------------------------------*

(DEFPARAMETER *default-telnet-stream-input-buffer-size* 256) 

;1;; OPEN-TELNET-STREAM*

(DEFUN open-telnet-stream (host			;1nil to listen*
			   &key (local-port 0)	;1usually for listen only*
			   (remote-port 0)	;1usually for send only*
			   (transmit-mode :line)	;1 :line / :character / :stream*
			   remote-echo-preference	;1 :remote / :local / :offer / :ask / :never / nil*
			   (negotiate-options-on-open host)	;1t for active, nil for passive open*
			   (wait-for-establishment t)
			   (timeout 10)
			   (timeout-after-open (WHEN timeout 300))
			   (error t)		;1 t to signal, nil to return error condition*
			   (input-buffer-size *default-telnet-stream-input-buffer-size*)
			   (number-of-input-buffers 2)
			   &aux (host-name "Unknown") destination-address connection mode stream normal-exit
			   default-timeout-handler timeout-occurred-p)
  "2Establish a TELNET connection and return an object for accessing the data stream*"
  (FLET
    ;1; internal function*
    ((make-telnet-stream (connection transmit-mode remote-echo-preference
				     timeout input-buffer-size number-of-input-buffers
				     &aux stream)
			 (SETQ stream
			       (MAKE-INSTANCE 'telnet-stream :connection connection :transmit-mode transmit-mode
					      :remote-echo-preference remote-echo-preference :timeout timeout
					      :input-buffer-size input-buffer-size
					      :number-of-input-buffers number-of-input-buffers))
			 ;1; insert actual handlers now that stream is known*
			 (SETF (SEND connection :buffer-handler) #'(lambda (LENGTH push-p urgent-p)
								     (SEND stream :buffer-handler length push-p urgent-p)))
			 (SETF (SEND connection :urgent-handler) #'(lambda ()
								     (SEND stream :urgent-handler)))
			 (SETF (SEND connection :process-fin-handler) #'(lambda ()
									  (SEND stream :process-fin-handler)))
			 (SETF (SEND connection :receive-fin-handler) #'(lambda ()
									  (SEND stream :receive-fin-handler)))
			 (SETF (SEND connection :close-complete-handler) #'(lambda ()
									     (SEND stream :close-complete-handler)))
			 (SETF (SEND connection :user-timeout-handler) #'(lambda ()
									   (SEND stream :timeout-handler :user-timeout)))
			 (SETF (SEND connection :condition-handler) #'(lambda (condition-object)
									(SEND stream :condition-handler condition-object)))
			 stream))
    ;1; body of open-telnet-stream*
    (CONDITION-CASE-IF (NOT error) (error-object)
	(PROGN
	 (COND (host
		(SETQ mode :active)
		(SETF host (parse-ip-host-spec host))
		(SETF host-name (SEND host :short-name))
		(SETF destination-address (FIRST (closest-addresses-to-network (get-ip-addresses host))))
		(WHEN (ZEROP remote-port)
		  (SETQ remote-port telnet-default-port)))
	       (t (SETQ mode :passive)
		  (SETF destination-address 0)
		  (WHEN negotiate-options-on-open
		    (FERROR () "CAN'T NEGOTIATE TELNET OPTIONS ON A PASSIVE OPEN"))
		  (WHEN (ZEROP local-port)
		    (SETQ local-port telnet-default-port))))
	 (SETF connection (SEND *tcp-handler* :make-connection nil nil nil nil nil nil nil))
	 (SETF stream (make-telnet-stream connection transmit-mode remote-echo-preference
					  timeout input-buffer-size number-of-input-buffers))
	 ;1; use special handler until connection is established*
	 (SETF default-timeout-handler (SEND connection :user-timeout-handler))
	 (SETF (SEND connection :user-timeout-handler) #'(lambda ()
							   (SETF timeout-occurred-p t)))
	 (UNWIND-PROTECT
	     (PROGN
	       (SEND connection :open local-port remote-port destination-address mode timeout)
	       (SEND stream :load-receives)
	       (IF (NOT (OR (EQ mode :active) wait-for-establishment))
		   (SETF normal-exit t)
		   (DO ()
		       ((CATCH-ERROR-RESTART-EXPLICIT-IF error
							 (host-not-responding-during-connection
							   :retry-connection "Try the connection again.")
			  (SETF timeout-occurred-p nil)
			  (WHEN (NOT (PROCESS-WAIT-WITH-TIMEOUT
				       (COND
					 (*tcp-stream-whostate*)
					 ((NULL host) "TCP Listen")
					 (t
					  (FORMAT () "TCP Connect: ~A" host-name)))
				       ;1; when waiting for active establishment we depend on the tcp user timeout*
				       (IF (OR (EQ mode :active) (NULL timeout))
					   ()
					   (* timeout 60.))
				       #'(lambda (connection)
					   (CASE (SEND connection :state)
					     ((:listen :syn-sent :syn-received) timeout-occurred-p)
					     (otherwise t)))
				       connection))			
			    ;1; when timing out passive establishment we must explicitly call the user-timeout-handler*
			    (FUNCALL (SEND connection :user-timeout-handler)))
			  (IF timeout-occurred-p
			      (FERROR 'host-not-responding-during-connection "Host ~*~A not responding."
				      connection host-name)
			      t))
			(SETF normal-exit t))
		     (WHEN (AND timeout (<= timeout 60))
		       (SETF timeout 60)
		       (SETF (SEND connection :user-timeout) timeout)))))
	   (WHEN (NOT (OR normal-exit (EQ (SEND connection :status) :closed)))
	     (SEND connection :abort)))
	 (SETF (SEND connection :user-timeout-handler) default-timeout-handler)
	 (SETF (SEND stream :timeout) timeout-after-open)
	 (WHEN negotiate-options-on-open (SEND stream :negotiate-options)))
      (ERROR error-object)
      (:no-error stream)))) 
