;;; -*- Mode:Common-Lisp; Package:ZWEI; Base:8; Fonts:(CPTFONT HL12B HL12BI CPTFONT MEDFNB) -*-

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
;1;; Copyright (c) 1982 Massachusetts Institute of Technology *
;1;; Copyright (c) 1985-1989 Texas Instruments Incorporated.  All Rights Reserved.*
;1;;*

;1;; Network message facility*

;1; to be done:*
;1; Improve converse queuing of outgoing messages,*
;1; Commands for moving between major conversations (control-shift-[ or control-shift-]),*
;1; Refuse messages from just certain people*

;1;; HISTORY:*
;;; 04-24-89 DAB Added all function, variables, flavors and instances that are documented to export list.
;1;;   8/11/86 - RLA   - conversion to Common Lisp*
;1;;   2/26/86 - DKM  - use (mail:submit-mail ...) to send mail messages (replaced zwei:send-message-string).*
;1;;                     - changed parse-single-dest to call si:parse-host with no-error-p when just verify-only passed*
;1;;                     - setup-conversation and :my-name? to build domainless conversation names*
;1;;                     - changed :my-name to use string-equal instead of equalp (equalp failed for string differing only in case???)*
;1;;   3/8/87   - DKM - cleaned up :draw for black-line-diagram and header-line-diagram to use*
;1;;                          the :draw-filled-rectange method*
;1;;                     - mixed in w:graphics-mixin into  converse-frame for :draw-filled-rectangle*
;1;;                     - fixed converse-profile to set instead of setq variables.  Cleaned up prompts*
;1;;                       and makes it offer to change current value instead of always asking to set to T.*
;1;;                     - changed init-conversation-without-h and init-conversation-with-h to properly generate*
;1;;                          zmacs nodes and pointers to them.*
;1;;                     - changed com-delete-conversation to remove the deleted conversation node.*
;1;;                     - added a defcom for com-converse-send-with-exit which had a keystroke but no function*
;1;;                     - changed 4*buffer-munged-saved-conversation** to fit in 3 lines.*
;1;;   3/9/87   - DKM - turn off undo stuff by putting :dont in undo-status of *interval*.  Converse *
;1;;                          doesn't lend itself to UNDOing.  It would take some rethinking to support it without*
;1;;                          getting you in trouble.*
;1;;                     - change qsend-get-message to use read-char instead of :tyi.*
;1;;                     - change parse-single-dest to offer to send non-anonomous msg to person logged in.*
;1;;                     - cleaned up text in com-converse-help*
;1;;    3/13/87 - DKM - wrapped (fboundp-eval* ...)1 around references to MAIL:SUBMIT-MAIL, since that guy is not*
;1;;                           defined until later (prevents problem when this is built).*
;1;;    3/20/87 - DKM - made parse-single-dest not try to check who is logged onto a lisp unless a HOST is specified*
;1;;                     - changed CONVERSE-EDIT-AND-SEND-MSG to put the message in the general conversation*
;1;;                       area (TO: at the top) instead of in the conversation of the first address.  This prevents*
;1;;                       QSEND from not being able to switch over to converse due to the prompt for user not logged in.*
;1;;                     - Removed doc strings from rubout handler commands in qsend-get-message so it wil work in*
;1;;                       the cold load stream.  Used by the bug reporting gizzmo too.*
;;;  2/09/88 - LAS - changed call to add-system-key/menu to call modify-system-access-spec
;1;;    4/10/89  JLM   - Changed COM-CONVERSE-DELETE-CONVERSATION to use delete properly and not count on side effects*

(export '(zwei:reply              ;function  ; DAB 04-24-89
	   zwei:*converse-receive-mode*  ;variable
	   zwei:*converse-append-p*      ;variable
           zwei:*converse-beep-count*    ;variable
           zwei:*converse-extra-hosts-to-check* ;variable
           zwei:*converse-end-exits*     ;variable
           zwei:*converse-gagged*        ;variable
	   zwei:*converse-wait-p*        ;variable
	   )
	'zwei)

(defvar 4*converse-receive-mode-documentation**
   "3This variable controls what occurs when you receive a new interactive message.*
	3It has four possible values:*
	3:AUTO means to automatically enter Converse when a message arrives.*
	3:NOTIFY means print a short message on your current window*
	3  when a message arrives, or pop up a small window to tell*
	3  you to Type Terminal 0 S to go to Converse.*
	3:NOTIFY-WITH-MESSAGE is similar to :NOTIFY, but the notification included*
	3  in the message as well as the just the sender's name.  This is the default.*
	3:SIMPLE means to pop up a small window with which you can read*
	3  the message, reply, enter Converse, or do nothing at all.*"
   "2This variable is used for saving space and for modularity in the documentation.*") 

;1;; User options*
   

(defvar 4*converse-receive-mode** :notify-with-message
   "2This variable controls what occurs when you receive a new interactive message.
         It has four possible values:*

	2:AUTO means to automatically enter Converse when a message arrives.*
	2:NOTIFY means print a short message on your current window*
	2  when a message arrives, or pop up a small window to tell*
	2  you to Type Terminal 0 S to go to Converse.*
	2:NOTIFY-WITH-MESSAGE is similar to :NOTIFY, but the notification included*
	2  in the message as well as the just the sender's name.  This is the default.*
	2:SIMPLE means to pop up a small window with which you can read*
	2  the message, reply, enter Converse, or do nothing at all.*") 


(defvar 4*converse-end-exits** nil
   "2If T, typing END in Converse send and exit if NIL (the default), Converse will just send.*") 


(defvar 4*converse-append-p** nil
   "2NIL means to prepend incoming messages into the Converse buffer, T means to append them.*") 


(defvar 4*converse-beep-count** 2 "2Number of beeps to do when a new message arrives.*") 


(defvar 4*converse-extra-hosts-to-check** nil
   "2A list of hosts other than Lisp Machines that are checked to determine if a user is logged in.
This variable only is checked when a username is specified as a destination without a host.*") 


(defvar 4*converse-wait-p** t
   "2T means that we are willing to wait to determine the status of the message we are sending.*") 


(defvar 4*converse-gagged** nil
   "2If this variable is NIL, then you will receive all incoming Converse messages.
If it is not T, then it is assumed to be a string that will be sent to any user
who attempts to send you a message.  
If the value of this variable is T, then all incoming messages are simply rejected.
Usage of the functions QSENDS-ON and QSENDS-OFF is encouraged instead.*") 


(defun 4qsends-off* (&optional (gag-message t))
  "2Refuse messages from other users.
GAG-MESSAGE can be a string which is sent automatically as a reply
to anyone who sends a message here.*"
  (setq *converse-gagged* gag-message)) 


(defun 4qsends-on* ()
  "2Accept messages from other users.*"
  (setq *converse-gagged* nil)) 


(add-initialization "3Allow Converse messages*" '(setq *converse-gagged* nil) nil
		    'system:cold-initialization-list) 


(defvar 4*end-abort-message**
   "3For some weird reason, the Converse who line hasn't updated.
Please make a bug report.*") 


(defvar 4*converse-end-exits-message**
   "3   End sends and exits, Abort just exits, Control-End just sends*") 


(defvar 4*converse-end-just-sends-message**
   "3   End just sends, Abort just exits, Control-End sends and exits*") 

;1;; Next two are obsolete and should be flushed at some point.*


(defvar 4*converse-auto-expose-p** 'obsolete "2T=automatic exposure, NIL=notification*") 


(defvar 4*converse-notify-with-message** 'obsolete
   "2T=notification includes message, NIL=just sender*") 

;1;; When the documentation system comes this won't be needed.*


(defvar 4*y-or-n-converse-options**
   '(*converse-append-p* *converse-end-exits* *converse-wait-p* *converse-gagged*)
   "2Used by CONVERSE-PROFILE to set user's converse options.*") 


(defvar 4*hairy-converse-options**
   '(*converse-receive-mode* *converse-beep-count* *converse-extra-hosts-to-check*)
   "2Used by CONVERSE-PROFILE to set user's converse options.*") 


;1;; Internal variables*


(defvar 4*awaiting-exposure** nil "2Internal variable to tell if we are awaiting exposure.*") 


(defvar 4*converse-list** :unbound "2Used interally to store the conversations.*") 


(defvar 4*converse-frame** :unbound "2Used internally and should be initially unbound.*") 


(defvar 4*converse-comtab** nil
   "2Used for creating a comtab for Converse with the proper extra commands.*") 


(defvar 4*buffer-munged-saved-conversation** ""
   "2Used to store the conversation that regenerate buffer gets rid of.*") 


(defvar 4*systems-dont-upcase-for** '(multics)
   "2List of types of host whose send servers
care about case, and therefore we won't uppercasify what we send them.*") 


(defvar 4*saved-sends** (make-array 100 :element-type 'string-char :leader-list '(0))) 


(defvar 4*last-converse-sender** nil "2Internally used so that we can reply.*") 


(add-initialization "3Clear saved sends*" '(setq *saved-sends* nil) '(:before-cold)) 

;1; This must fit into the three lines normally provided.*

(defparameter 4*converse-buffer-munged-message**
   "3Buffer damaged!  You must do Meta-X Regenerate Buffer to continue using Converse.
This will destroy any text that you have edited in the buffer other than messages already sent and received.
To save that text, you must kill in now, and yank it back later.*") 


(defparameter 4*converse-point-invalid-message**
   "3You're not inside a conversation, please move outside the black lines.*") 



(defun 4converse-profile* (&optional (*query-io* *standard-output*))
  "2Set canonical Converse user options.*"
  (format *query-io*
	  "3~&You are now setting the values of the various Converse Options.
Type ABORT to stop doing so.
Note: Your init file is left unchanged.*")
  (dolist (variable *y-or-n-converse-options*)
    (format *query-io*
	    "3~2%The variable ~A has the value ~A~&~A~2%*"
	    variable (symbol-value variable) (documentation variable 'variable))
    ;1;there are better user interfaces*
    (let ((new-value (not (symbol-value variable))))
       (and (y-or-n-p "3Change this variable to ~a ? *" new-value)
	    (set variable new-value))))
  (format *query-io*
	  "3~2%There are other variables that normally contain values other than ~
simply NIL or T.~&*")
  (cond
    ((y-or-n-p "3Would you like to examine them? *") (terpri *query-io*)
     (let ((doc (y-or-n-p "3Would you like to see their documentation as well? *")))
       (dolist (variable *hairy-converse-options*)
	 (format *query-io* "3~2%Variable: ~A  ~&Value: ~A ~:[ ~;~&Documentation: ~&~A~]*" variable
		 (symbol-value variable) doc (documentation variable 'variable))))))) 



(defflavor 4conver*
	   (first-line				1 *;1first line in conversation*
	    last-line				1 *;1last line in conversation*
	    (who "3Whomever*")			1 *;1with whom are we conversing*
	    my-node				1 *;1node in which this is happening*
	    to-line-del				1 *;1points to diagram after To: area*
	    (with-header-p t)			1 *;1whether or not conversation has header*
	    (append-mode *converse-append-p*)	 ;1T appends msg to end of conversation*
	    (oldmsgs nil))		1  *       ;1all the msgs in this conversation*
	   ()
  (:initable-instance-variables who append-mode with-header-p)
  (:init-keywords :before-line :after-line)
  :gettable-instance-variables) 


(defmethod 4(conver :line-mine?*) (line)
  (eq (line-node line) my-node)) 


(defmethod 4(conver :my-name?*) (name)
  (multiple-value-bind (ignore user host)
      (parse-single-dest name t)
    (setq name (string-append user (if host
				       (string-append "3@*" (send host :name))
				       ""))))
  (string-equal who name)) 


(defmethod 4(conver :init*) (init-plist)
  "2Initializes a conversation with correct To: line and diagrams.
Must put itself between AFTER-LINE and BEFORE-LINE.
If HEADER is nil then use AFTER-LINE for first-line and do not generate a 
conversation header diagram. Don't even bother with nodes for BEFORE-LINE.*"
  (let ((before-line (get init-plist :before-line))
	(after-line (get init-plist :after-line)))
    (if (or (null before-line) (null after-line))
      (ferror nil "3You must supply a before-line and an after-line when creating a CONVER*"))
    (if with-header-p
      (init-conversation-with-h after-line before-line who)
      (init-conversation-without-h after-line before-line)))) 


(defmethod 4(conver :after-to-line-bp*) ()
  "2Return a bp to the line immediately after TO:.  In the case of a headerless
conversation, the bp is to the end of the TO: line.*"
  (let ((l1 (if with-header-p
	      (line-next first-line)
	      first-line)))
    (if with-header-p
      (beg-of-line (line-next l1))
      (when (and (> (line-length l1) 3) (string-equal l1 "3To:*" :end1 3  :end2 3))
	(let ((bp (create-bp l1 3)))
	  (if (eql (bp-char bp) #\Space)
	    (ibp bp))
	  bp))))) 


(defun 4init-conversation-without-h* (after-line before-line &aux line)
  "2Create the node and initial text for the first, special conversation.
It is inserted between AFTER-LINE and BEFORE-LINE,
which should be successive lines.*"
  (declare (:self-flavor conver))
  (setq my-node (make-instance 'node :superior *interval*))	;1generate a new node*
  (setq line (make-diagram-line 'black-line-diagram))		;1make a thin diagram line*
  (insert-within-line after-line 0 "3To: *" 0 4)			;1put in To: line right in after-line*
  (insert-line-with-leader line before-line)			;1add it just after the To: line*
  (setf (line-node after-line) my-node) 			;1update the nodes for this line*
  (setf (line-node line) my-node)				;1  and this one*
  (setf (interval-first-bp my-node) (create-bp after-line 0 :normal))	;1update first-bp to point at first line*
  (setf (interval-last-bp my-node) (create-bp line 0 :moves))	;1update last-bp to point at last line*
  (setf (node-inferiors *interval*) (list my-node))		;1add this node to the inferior list for *interval**
  (setf (node-undo-status *interval*) :dont)
  (setq first-line after-line
	last-line line
	to-line-del line)
  )


(defun 4init-conversation-with-h* (after-line before-line -who- &aux line)
  "2Create the node and initial text for a normal conversation with user -WHO-.
It is inserted between AFTER-LINE and BEFORE-LINE,
which should be successive lines.*"
  (declare (:self-flavor conver))
  (setq my-node (make-instance 'node :superior *interval*))
  ;1; make end of conversation delimiter*
  (setq line (make-diagram-line 'black-line-diagram))
  ;1; splice this diagram between AFTER-LINE and BEFORE-LINE*
  (insert-line-with-leader line before-line)
  (setf (line-node line) my-node)
  (setq last-line line
	to-line-del line)
  ;1; put To: line before end delimiter*
  (insert (create-bp line 0 :normal) (string-append "3To: *" -who- #\Newline #\Newline))
  ;1; make header diagram*
  (setq line (make-diagram-line 'converse-header-diagram :name -who-))
  ;1; splice header between AFTER-LINE and To: line*
  (insert-line-with-leader line (line-next after-line))
  (setf (line-node line) my-node)
  (setq first-line (line-next after-line))
  ;1; update first-bp and last-bp of node to point to start and end lines*
  (send my-node :set-first-bp (create-bp first-line 0))
  (send my-node :set-last-bp (create-bp last-line 0 :moves))
  ;1; splice in MY-NODE (must check for node type)*
  (let* ((previous-node (line-node after-line))
	 (next-node (and previous-node (node-next previous-node))))
    (send my-node :set-previous previous-node)
    (send my-node :set-next next-node)
    (and previous-node (send previous-node :set-next my-node))
    (and next-node (send next-node :set-previous my-node)))
  ;1;add this node to the inferior list for *interval* (in the right spot)*
  (let ((inferiors (node-inferiors *interval*)))
    (send *interval* :set-inferiors (append (list (car inferiors)) (list my-node) (cdr inferiors)))))



(defmacro 4check-conversation-integrity* ()
  "2Check to see that the current conversation is ok, warning if not.*"
  '(if (null (send self ':conversation-good?))
     (converse-barf *converse-buffer-munged-message*))) 


(defmacro 4check-buffer-integrity* ()
  "2Check to see that entire buffer is ok, warning if not.*"
  '(if (null (converse-buffer-good?))
     (converse-barf *converse-buffer-munged-message*))) 


(defmethod 4(conver :add-msg*) (msg &aux line)
  ;1;inserts all the new text before the old To-line-del*
  ;1;and creates a new to-line-del*
  (push msg oldmsgs)
  (check-buffer-integrity)
  (setq line (make-diagram-line 'black-line-diagram))
  (setf (line-node line) my-node)
  (setq append-mode *converse-append-p*)
  
  (cond ((or (eq last-line to-line-del)				;1prepending mode or no msgs yet*
	     (not append-mode))
	 (insert-line-with-leader line to-line-del)	   
	 (setq to-line-del line))
	(t							;1appending mode*
	 (insert-line-with-leader line last-line)))
  
  (insert (create-bp (line-next line) 0)
	  (string-append msg #\Newline))) 


(defmethod 4(conver :get-to-msg*) ()
  "2Returns the string in the To: line area of a conversation.*"
  (check-conversation-integrity)
  ;1; must put L1 and L2 around the text to return*
  (let ((l1 (if with-header-p
	      (line-next first-line)
	      first-line))
	(l2 (line-previous to-line-del)))
    (string-interval (beg-of-line l1) (end-of-line l2) t))) 

;1;; The following will restore a damaged conversation or conversation buffer*


(defmethod 4(conver :restore-to-msg*) ()
  "2Returns the To: line area to is empty state*"
  (check-conversation-integrity)
  ;1; what we do depends on whether or not there is a header for this conversation*
  (if with-header-p
    (restore-to-msg-with-h)
    (restore-to-msg-without-h))) 


(defun 4restore-to-msg-with-h* ()
  "2Cleans out the To: line and following lines, after a message is sent.
This function is called with SELF set to a normal conversation.*"
  (declare (:self-flavor conver))
  ;1; splice out lines between the header and the To: line delimiter*
  (delete-interval (create-bp (line-next first-line) 0) (create-bp to-line-del 0))
  ;1; now put in a new To: line*
  (insert (create-bp to-line-del 0) (string-append "3To: *" who #\Newline #\Newline)))


(defun 4restore-to-msg-without-h* ()
  "2Cleans out the To: line and following lines, after a message is sent.
This function is called with SELF set to a special conversation
(one that is with nobody in particular).*"
  (declare (:self-flavor conver))
  (with-bp (bp (create-bp first-line 0) ':normal)
     (delete-interval bp (end-of-line (line-previous to-line-del)))
     (setq first-line (bp-line bp)))
  (insert-within-line first-line 0 "3To: *" 0 4)) 


(defmethod 4(conver :regen-yourself*) (after-line before-line &aux (toldmsgs oldmsgs))
  "2This is almost the same thing as :INIT except go through all the OLDMSGS 
and add them to the buffer.*"
  (setq append-mode *converse-append-p*)
  (if with-header-p
    (init-conversation-with-h after-line before-line who)
    (init-conversation-without-h after-line before-line))
  (setq oldmsgs nil)
  (dolist (msg (reverse toldmsgs))
    (send self :add-msg msg)))  


(defun 4regen-converse-buffer* (&aux oldlist)
  "2Regenerate the entire contents of the Converse buffer from saved messages.*"
  ;1; try to save what we throw away*
  (setq *buffer-munged-saved-conversation* (car *converse-list*))
  (setq oldlist (reverse (cdr *converse-list*)))
  ;1; clear everything out of the buffer*
  (delete-interval *interval*)
  (insert (interval-first-bp *interval*) #\Newline)
  ;1; make a headerless To: line conversation*
  (let ((c (make-instance 'conver :with-header-p nil
			  :after-line (bp-line (interval-first-bp *interval*))
			  :before-line (bp-line (interval-last-bp *interval*)))))
    ;1; create the conversation list*
    (setq *converse-list* (list c))
    (send *interval* :set-inferiors (list (send c :my-node))))
  ;1; now regenerate all the conversations in order*
  (dolist (conversation oldlist)
    (regen-setup-conversation conversation))
  ;1; move the point to beginning of new buffer*
  (move-bp (point) (end-line (interval-first-bp *interval*)))) 


(defun 4regen-first-conver* ()
  "2Try to regain lost text from regenerating the whole buffer.*"
  (if (variable-boundp *buffer-munged-saved-conversation*)
    (regen-setup-conversation *buffer-munged-saved-conversation*))) 


(defun 4salvage-converse* (&optional severely)
  "2Try to get Converse working again.  You may lose many things in the process, though.
If SEVERELY is T, then regenerate the buffers as well.*"
  ;1; (if ... ask about saving buffer.*
  (send (find-converse-window) :clear-request-queue)
  (if severely
    (send (find-converse-window) :regen-yourself))) 


(defun 4regen-setup-conversation* (conversation &aux line)
  "2Regenerate the text for CONVERSATION from its saved messages.
CONVERSATION should be a normal conversation -- one with a particular other user.*"
  ;1; regenerate a conversation after main to-line conversation*
  (if (cdr *converse-list*)
    (setq line (send (cadr *converse-list*) :first-line))
    (setq line (bp-line (interval-last-bp *interval*))))
  (setq *converse-list*
	(append (list (car *converse-list*)) (list conversation) (cdr *converse-list*)))
  (let ((inferiors (send *interval* :inferiors)))
    (send *interval* :set-inferiors
	  (append (list (car inferiors)) (list (send conversation :my-node)) (cdr inferiors))))
  (send conversation :regen-yourself (line-previous line) line)
  conversation) 


;1;; Diagram stuff*

;1;; The following create a medium width black line diagram*


(defflavor 4black-line-diagram*
	   ()
	   (line-diagram-mixin)) 


(defmethod 4(black-line-diagram :draw*) (ignore sheet &aux height)
  (setq height (floor (w:sheet-line-height sheet) 4))
  (send sheet :draw-filled-rectangle
	(w:sheet-inside-left sheet) (+ (w:sheet-cursor-y sheet) (floor (* height 3) 2))
	(w:sheet-inside-width sheet) height)) 

(defmethod 4(black-line-diagram :string-for-file*) (line)
  line
  "3-----------------------------------------------------------------------*") 


(compile-flavor-methods black-line-diagram) 

;1;; The following creates a converse header diagram*


(defflavor 4converse-header-diagram*
	   (name)
	   (line-diagram-mixin)
  :settable-instance-variables) 


(defmethod 4(converse-header-diagram :draw*) (ignore sheet)
  (let* ((string-width (+ (w:sheet-string-length sheet name 0 nil nil fonts:cptfontb) 100))
	 (bar-width (floor (- (w:sheet-inside-width sheet) string-width) 2))
	 (bar-height (- (w:sheet-line-height sheet) 3))
	 (y-loc (w:sheet-cursor-y sheet))
	 (left-margin-loc (w:sheet-left-margin-size sheet)))
    (send sheet :draw-filled-rectangle
	  left-margin-loc y-loc
	  bar-width bar-height)
    (send sheet :draw-filled-rectangle
	  (+ left-margin-loc bar-width string-width) y-loc
	  bar-width bar-height)
    (send sheet :string-out-explicit name
	  (+ bar-width 50) y-loc
	  (+ left-margin-loc (+ string-width bar-width))
	  nil fonts:cptfontb (w:sheet-char-aluf sheet)))) 



(defmethod 4(converse-header-diagram :string-for-file*) (line)
  line
  (string-append "3============================== *" name "3============================== *")) 


(compile-flavor-methods converse-header-diagram) 



(defflavor 4converse*
	   (*converse-list*;1list of all ongoing conversations*
	    (converse-request-queue nil);1requests from other processes*
	    ;1This is a list of requests, each is cons of function and list of args*
)
	   ()
  (:special-instance-variables *converse-list*)) 


(defmethod 4(converse :clear-request-queue*) ()
  (setq converse-request-queue nil)) 

;1;; Call FIND-CONVERSE-WINDOW with nil to NOT create a converse window *
;1;; even if one DOES NOT exist. (KCW 7-8-85)*

(add-initialization 'clear-converse-queue
		    '(let ((cframe (find-converse-window nil)))
		       (if cframe
			 (send cframe ':clear-request-queue)))
		    '(:before-cold)) 


(defmethod 4(converse :enter-request*) (function &rest args)
  "2Use this message to send a request to the Converse process.
The request is placed on the request queue and the process is awakened
by forcing keyboard input.*"
  (let ((req (cons function (copy-list args))))
    (without-interrupts
     ;1; Put the new request at the end so they are processed in*
     ;1; the order they are requested.*
     (setq converse-request-queue (nconc converse-request-queue (list req))))
    (send self :force-kbd-input '(:execute converse-execute-queue)))) 


(defmethod 4(converse :enter-delayed-request*) (function &rest args)
  "2Similar to :ENTER-REQUEST if the request does not need to be processed 
unlessuntil Converse is awakened for some other reason.*"
  (let ((req (cons function (copy-list args))))
    (without-interrupts
     ;1; Put the new request at the end so they are processed in*
     ;1; the order they are requested.*
     (setq converse-request-queue (nconc converse-request-queue (list req))))))


(defmethod 4(converse :after :expose*) (&rest ignore)
  "2Process the delayed requests when converse is exposed.
Note: delayed requests should not be made when converse is exposed.*"
  (if converse-request-queue
    (send self :force-kbd-input '(:execute converse-execute-queue)))) 


(defun 4converse-execute-queue* ()
  (send *converse-frame* :execute-queued-requests)) 


(defmethod 4(converse :execute-queued-requests*) ()
  (dolist (req converse-request-queue)
    (apply (car req) (cdr req))
    (without-interrupts
     (setq converse-request-queue (delete req (the list converse-request-queue) :test #'eq))))) 


(defun 4setup-conversation* (who &aux conversation line)
  "2Create a conversation with user WHO and insert its text in the buffer.*"
  (check-buffer-integrity)
  (if (cdr *converse-list*)
    (setq line (send (cadr *converse-list*) :first-line))
    (setq line (bp-line (interval-last-bp *interval*))))
  (multiple-value-bind (ignore name host)
      (parse-single-dest who t)
    (setq who (string-append name
			     (if host
				 (string-append "3@*" (send host :name))
				 ""))))
    (setq conversation
	  (make-instance 'conver
			 :who who
			 :after-line (line-previous line)
			 :before-line line))
    (setq *converse-list*
	  (append (list (car *converse-list*)) (list conversation) (cdr *converse-list*)))
    conversation)


;1;; The following  will allow a conversation, or buffer to check its*
;1;; integrity.  For a buffer, that means being able to reach the first and*
;1;; last lines of every conversation, in the same order that they were put*
;1;; in the buffer.  Also, the last line of one conversation must be the first*
;1;; line of another conversation.  A conversation, however, only cares that *
;1;; its to-line-delimiter is between its first line and last line, and that*
;1;; its first line can be reached from the first line in the buffer.*

;1;; the present definition is a little too stringent: all it should really*
;1;; check for is a good 1st conversation and a good neighboring conversation*
;1;; (depending on the direction of the buffer) - more trickieness to come!*


(defun 4converse-buffer-good?* ()
  "2T if the Converse buffer is consistently structured as nodes for conversations.*"
  ;1; first line in buffer must be first line of first message*
  (or (not (variable-boundp *converse-list*))
      ;1;may be checking before we got any messages*
     (and
      (eq (bp-line (interval-first-bp *interval*)) (send (car *converse-list*) :first-line))
      ;1; must be able to get to the last line*
      ;1; check each last line for next line being first line of next conversation*
      (do ((c *converse-list* (cdr c)))
	  ((null (cdr c))
	   t)
	(if (not (line-reachable? (send (car c) :first-line) (send (car c) :last-line)))
	  (return nil))
	(if (neq (line-next (send (car c) :last-line)) (send (cadr c) :first-line))
	  (return nil)))))) 


(defmethod 4(conver :conversation-good?*) ()
  "2Returns whether or not the TO-LINE-DEL is in between the FIRST-LINE and 
LAST-LINE and also if the FIRST-LINE is reachable in the *CONVERSE-WINDOW**"
  (and (line-reachable? (bp-line (interval-first-bp *interval*)) first-line)
     (line-reachable? first-line to-line-del) (line-reachable? to-line-del last-line))) 


(defun 4line-reachable?* (start-line to-reach-line)
  "2Finds out if TO-REACH-LINE is somewhere after START-LINE*"
  (do ()
      (nil)
    (if (null start-line)
      (return nil))
    (if (and (line-next start-line) (neq (line-previous (line-next start-line)) start-line))
      (return nil));1not a broken link?*
    (if (eq start-line to-reach-line)
      (return t))
    (setq start-line (line-next start-line)))) 


;1;; Commands*


(defcom 4com-converse-previous-conversation* "3Move to the previous conversation.*" ()
  ;1; go through all the conversations and find out which one the cursor is in*
   (check-buffer-integrity)
   (let ((conversation
	  (dolist (c *converse-list*)
	    (if (send c :line-mine? (bp-line (point)))
	      (return c)))))
     (if (null conversation)
       (barf *converse-point-invalid-message*))
     (if (bp-< (send conversation :after-to-line-bp) (point))
       (move-bp (point) (send conversation :after-to-line-bp))
       (progn
	 (setq conversation
	       (do ((clist *converse-list* (cdr clist)))
		   ((or (null clist) (eq (cadr clist) conversation))
		    (car clist))))
	 (if conversation
	   (move-bp (point) (send conversation :after-to-line-bp))))))
   dis-bps) 


(defcom 4com-converse-next-conversation* "3Move to the next conversation.*" ()

  ;1; go through all the conversations and find out which one the cursor is in*
   (check-buffer-integrity)
   (let ((conversation
	  (dolist (c *converse-list*)
	    (if (send c :line-mine? (bp-line (point)))
	      (return c)))))
     (if (null conversation)
       (barf *converse-point-invalid-message*))
     (if (bp-< (point) (send conversation :after-to-line-bp))
       (move-bp (point) (send conversation :after-to-line-bp))
       (progn
	 (setq conversation
	       (do ((clist *converse-list* (cdr clist)))
		   ((or (null clist) (eq (car clist) conversation))
		    (cadr clist))))
	 (if conversation
	   (move-bp (point) (send conversation :after-to-line-bp))))))
   dis-bps) 


(defcom 4com-converse-regenerate-buffer* "3Restore the Converse buffer to working condition.*" ()
   (regen-converse-buffer) dis-all) 


(defcom 4com-converse-abort* "3Exit from CONVERSE.*" ()
  (converse-quit)
  dis-none) 


(defcom 4com-converse-handle-end*
	"3Send the current message; default is not to exit Converse.
Exits Converse if ZWEI:*CONVERSE-END-EXITS* is set to T.*" ()
  (condition-case (error)
      (if *converse-end-exits*
	  (converse-send-with-exit)
	  (converse-send-without-exit))
    (net:unknown-host-name
     (converse-problem (send error :report-string))))
  dis-text) 


(defcom 4com-converse-handle-control-end*
	"3Send the current message and (by default) exit Converse.
Doesn't exit if ZWEI:*CONVERSE-END-EXITS* is set to T.*" ()
  (condition-case (error)
      (if (not *converse-end-exits*)
	  (converse-send-with-exit)
	  (converse-send-without-exit))
    (net:unknown-host-name
     (converse-problem (send error :report-string))))
  dis-text) 

(defcom 4com-converse-send-with-exit* "3Send the current message and exit*" ()
    (converse-send-with-exit))

(defun 4(:property *end-abort-message* mode-line-recalculate*) ()
  (setq *end-abort-message*
	(if *converse-end-exits*
	  *converse-end-exits-message*
	  *converse-end-just-sends-message*))) 


(defun 4converse-send-without-exit* ()
  "2Transmit the message without exiting Converse.*"
  (converse-send-msg)
  dis-text) 


(defun 4converse-send-with-exit* ()
  "2Transmit the message and exit Converse.*"
  (if (converse-send-msg)
   ;1;successful*
    (converse-quit))
  dis-text) 


(defcom 4com-converse-delete-conversation* "3Delete the conversation from the buffer.*" ()
   (check-buffer-integrity)
   (let ((conversation (dolist (c *converse-list*)
			 (if (send c :line-mine? (bp-line (point)))
			     (return c)))))
     (if (null conversation)
       (barf *converse-point-invalid-message*))
     ;1; Remove this node from the chain of nodes.*
     (let* ((this-node (send conversation :my-node))
	    (prev-node (and this-node (node-previous this-node)))
	    (next-node (and this-node (node-next this-node))))
       (delete-interval (interval-first-bp this-node)
			(create-bp (line-next (bp-line (interval-last-bp this-node))) 0)
			t)
       (when prev-node
	 (send prev-node :set-next next-node))
       (when next-node
	 (send next-node :set-previous prev-node))
       (setf (send *interval* :inferiors)
	     (delete this-node (the list (send *interval* :inferiors)) :test #'eq)))	;1 jlm 4/10/89*
     (setf *converse-list* (delete conversation (the list *converse-list*) :test #'eq)))
   dis-text) 

(defcom 4com-converse-write-buffer* "3Write the entire buffer (all conversations) into a file.*" ()
   (let ((pathname
	  (read-defaulted-pathname "3Write Converse buffer to file:*" (pathname-defaults))))
     (with-open-file (stream pathname :direction :output :error :reprompt)
       (stream-out-interval stream *interval* nil nil t))
     (format *query-io* "3~&Converse buffer written to ~A.*" pathname))
   dis-none) 

(defcom 4com-converse-write-conversation* "3Write a single conversation into a file.*" ()
  ;1;go through all the conversations and find out which one the cursor is in*
   (check-buffer-integrity)
   (let ((conversation
	  (dolist (c *converse-list*)
	    (if (send c :line-mine? (bp-line (point)))
	      (return c)))))
     (if (null conversation)
       (barf *converse-point-invalid-message*))
     (let ((pathname
	    (read-defaulted-pathname
	     (format nil "3Write conversation with ~A to file:*" (send conversation :who))
	     (pathname-defaults))))
       (with-open-file (stream pathname :direction :output :error :reprompt)
	 (stream-out-interval stream
			      (create-bp (line-previous (send conversation :first-line)) 0)
			      (create-bp (send conversation :last-line) 0) t t))
       (format *query-io* "3~&Conversation with ~A written to ~A.*" (send conversation :who)
	       pathname)))
   dis-none) 


(defcom 4com-converse-append-buffer3 **"3Append the entire buffer (all conversations) to the end of a file.*" ()
   (let ((pathname (read-defaulted-pathname "3Append Converse buffer to:*" (pathname-defaults))))
     (with-open-file-retry (ostream (pathname fs:file-error) :direction :output)
	(with-open-file-case (istream pathname) (fs:file-not-found (typein-line "3(New File)*"))
	   (error (barf "3Error: ~A*" istream))
	   (:no-error (stream-copy-until-eof istream ostream)))
	(stream-out-interval ostream *interval* nil nil t)
	(format *query-io* "3~&Converse buffer appended to end of ~A.*" pathname)))
   dis-none) 


(defcom 4com-converse-append-conversation* "3Appends the conversation to the end of a file.*" ()
  ;1;go through all the conversations and find out which one the cursor is in*
  (check-buffer-integrity)
  (let ((conversation
	  (dolist (c *converse-list*)
	    (if (send c :line-mine? (bp-line (point)))
		(return c)))))
    (if (null conversation)
	(barf *converse-point-invalid-message*))
    (let ((pathname
	    (read-defaulted-pathname
	      (format nil "3Append conversation with ~A to file:*" (send conversation :who))
	      (pathname-defaults)))
	  (bp1 (create-bp (line-previous (send conversation :first-line)) 0))
	  (bp2 (create-bp (send conversation :last-line) 0)))
      (with-open-file-retry (ostream (pathname fs:file-error) :direction :output)
	(with-open-file-case (istream pathname) (fs:file-not-found (typein-line "3(New File)*"))
			     (error (barf "3Error: ~A*" istream))
			     (:no-error (stream-copy-until-eof istream ostream)))
	(stream-out-interval ostream bp1 bp2 t t)
	(format *query-io* "3~&Conversation with ~A appended to end of ~A.*"
		(send conversation :who) pathname))))
  dis-none) 

;1;; Do the :restore-to-msg here to prevent msg losage IF ABORTED. -dkm 6/87*

(defcom 4com-converse-mail-message*
	"3Mail the current message to the specified destination instead of sending it.*" ()
  (multiple-value-bind (dest msg conversation)  ;1added conversation to binding  -dkm 6/87*
      (converse-get-dest-and-msg)
    (converse-send-msg-internal dest msg t)
    (send conversation :restore-to-msg))        ;1don't restore the to: area until msg successfully sent  -dkm 6/87*
  dis-text) 


(defcom 4com-converse-yank-last-msg-text*
   "3Insert the text of the last message we received into the conversation.*" ()
   (insert-moving (point) (last-message-text)) dis-text) 


(defcom 4com-gag-converse* "3Gag Converse if it is not already gagged and ungag it if it is not.*" ()
   (cond
     ((null *converse-gagged*) (setq *converse-gagged* t))
     ;1;later read in a message.*
     (t (setq *converse-gagged* nil)))
   (format *query-io* "3~&Incoming messages now ~:[allowed~;disallowed~].*" *converse-gagged*)
   ;1; update mode line?*
   dis-none) 





(defcom 4com-converse-help* "3Explain how to use Converse.*" ()
   (format t
	   "3~%     You are in Converse, the interactive message editor.

Messages are separated by black lines.  All the messages you send to or
receive from a particular user are grouped into a \"conversation\", with
that user's name at the top.  The thick black lines separate
conversations.  DON'T DELETE THE BLACK LINES; it will cause lossage.

Within the conversation, messages are ordered chronologically.
Users with different user names or at different hosts get distinct
conversations.  If you begin communicating with a user there is no
conversation for, a new conversation is created.

To send a message to FRED@HOST, move to after the \"To:\" at the
beginning of the buffer, type FRED@HOST, type Return, and the message.
Then use End or Control-End to send it.  This creates a new
conversation, which has a template message at the front that you can use
to send FRED more messages.  Then you can use the \"To:\" at the
beginning of the buffer to start new conversations, or use the templates
at the beginning of each conversation to continue that conversation.

You can send a message to more than one person at once.  Use the syntax
FRED@HOST,NOT-FRED@HOST-1,YOU@NAUSEAM.  A separate copy of your message
goes into the conversation for each of the recipients.

If instead of saying FRED@HOST you just say FRED, Converse will try to
find a Lisp Machine that FRED is logged into and send the message there.
If you set the variable ZWEI:*CONVERSE-EXTRA-HOSTS-TO-CHECK* to a list of hosts,
those hosts will be checked when Converse tries to determine where Fred is
logged in.  If Fred is actually logged in somewhere else, you will be given
a menu of hosts to choose from as a destination for your message.

Most, if not all of the usual editor commands can be used to edit your messages.

The following additional commands are provided:
   End                      Send the current message without leaving Converse. (see zwei:*converse-end-exits*)
   Control-End              Send the current message  and exit from Converse.  (see zwei:*converse-end-exits*)
   Abort                    Get rid of the Converse window.
   Control-M                Mail the current message instead of sending it.
   Control-Meta-Y           Yank in the text of the last message received.
   Meta-{*	3            Move to the previous To: line.
   Meta-}*		3    Move to the next To: line.
   M-X Delete Conversation  Delete the current conversation.
   M-X Write Buffer         Write the entire buffer (all conversations) to a file.
   M-X Write Conversation   Write only the current conversation to a file.
   M-X Append Buffer        Append all conversations (buffer) to the end of a file.
   M-X Append Conversation  Append the current conversation to the end of a file.
   M-X Regenerate Buffer    This may be necessary if, during your editing, you
                            damage the structure of the buffer, by editing in
 *			3    or across the black lines.  This command rebuilds*
			3    the structure.  Some error messages may ask you to
 *			3    give this command and try again.*
			3    **NOTE** It will throw away anything you have inserted*
			3    but not sent!  To avoid losing it, kill it now*
			3    and yank it back after you regenerate.

The following commands can be given from a Lisp Listener.
   (qsend)                  Enter Converse.
   (qsend dest)             Read a message from the keyboard and send it to dest.
   (qsend dest message)     Send message to dest.  Note: dest is not evaluated.
   (qsends-off)*	3            Reject all incoming messages.
   (qsends-off message)     Same, but \"message\" is told to anyone who tries sending to you.
   (qsends-on)              Accept further messages.  This commaand un-does (qsends-off) .
   (reply message)          Send message to last user who sent you a message.

The following are user options that you may set in your Lispm init file:

   zwei:*converse-end-exits*
        If nil (the default), END will send message and stay in Converse
        while CTRL-END will send message and exit.   If T, these two keys
        swap their roles.

   zwei:*converse-receive-mode*
~8T~A~%
   zwei:*converse-append-p**
	3If T, new messages are appended to the ends of*
	3conversations.  If false (this is the default), they*
	3are prepended to the beginnings of conversations.

   zwei*:*converse-wait-p* 
   3     If *T3, will* wait 3and* determine the status of the message we are sending.
3        Otherwise, message is sent in the background.

   zwei:*converse-beep-count**
	3The number of times to beep when a message arrives.*
	3The default value is 2.

   zwei:*converse-extra-hosts-to-check*
        A list of other hosts to look for users when no host name is
        specified.  Defaults to NIL.

Type any character to return to Converse top level.
~%*"
	   *converse-receive-mode-documentation*)
   dis-none) 




(defun 4converse-quit* ()
  "2Exist Converse and return to previous window.*"
  (w:deselect-and-maybe-bury-window *converse-frame*)) 

;1;; Do the :restore-to-msg here to prevent msg losage IF ABORTED. -dkm 6/87*

(defun 4converse-send-msg* ()
  "2Figures out what message to transmit, sends it and updates the buffer
Returns non-NIL if the message is successfully sent.*"
  (multiple-value-bind (dest msg conversation)     ;1add conversation to bindings -dkm 6/87*
      (converse-get-dest-and-msg)
    (converse-send-msg-internal dest msg)
    (send conversation :restore-to-msg)))          ;1don't restore to: area until msg sent  -dkm 6/87*

;1;; Don't do :restore-to-msg here as we don't know that the message REALLY*
;1;; was sent.  Added third return value of the conversation for calling routines*
;1;; use to reset the TO conversation after the message has been sent sucessfully. -dkm 6/87*

(defun 4converse-get-dest-and-msg* ()
  "2Find the message that point is in.  Return the parsed destination and
 the body of the message.*"
  (check-buffer-integrity)
  (let ((conversation
	 (dolist (c *converse-list*)
	   (if (send c :line-mine? (bp-line (point)))
	     (return c)))))
    (if (null conversation)
      (converse-barf *converse-point-invalid-message*))
    (let ((msg-with-to (send conversation :get-to-msg)))
      (let ((dest
	     (if (string-equal msg-with-to "3To:*" :end1 3 :end2 3)
	       (string-trim "3 *"
			    (subseq (string msg-with-to) 3
	(position #\Newline (the string (string msg-with-to)) :test #'char-equal)) )
	       (converse-barf "3This message doesn't start with \"To:\".*")))
	    (msg
	     (if (position #\Newline (the string (string msg-with-to)) :test #'char-equal)
	       (subseq (string msg-with-to)
	(1+ (position #\Newline (the string (string msg-with-to)) :test #'char-equal))) 
	       (converse-barf "3This message has no contents, only a \"To:\" line.*"))))
	(or (string-search-not-set '(#\Newline #\Space) msg)
	   (converse-barf "3This message has no contents, only a \"To:\" line.*"))


;1;; We don't want to initialize the to-msg at this point because we don't know if the message*
;1;; really was sent yet (could ABORT after this).  So changed to pass back the conversation*
;1;; we were in so caller can :restore-to-msg himself after message is really sent.  -dkm 6/87*

	;1; Parse now enough to get error for non-ex host*
;	(condition-case (error)
;	    (progn
;            (dolist (dest (parse-commas-into-list dest))
;	      (parse-single-dest dest t))
;	  (system:local-network-error (converse-barf "3~A*" (send error :report-string)))1)*

;	;1; It's valid enough to record on a conversation,*
;	;1; so remove it from the TO-MSG and try sending.*
;	(send conversation :restore-to-msg)

	(values dest msg conversation))))) ;1add conversation to the returned values  -dkm 6/87*


(defun 4converse-send-msg-internal* (destination message &optional mail-p &aux user host lossage)
  "2Within a Converse command, send MESSAGE to DESTINATION, returning T if successful.
MAIL-P says mail the message instead of sending it interactively.
DESTINATION is a string, and it can contain commas, in which case we
will send to multiple people.  We return the list of successful recipients.*"
  (let ((dest-list (parse-commas-into-list destination))
	lossage-reason)
    (or dest-list (setq lossage t))
    (dolist (dest dest-list)
      (multiple-value-setq (dest user host)
       ;1;this now treats each user sperately *
	(parse-single-dest dest))
      (cond
	((null host)
	 (setq lossage t)
	 (converse-problem
	   (string-append "3Message not sent because: *"
			  (setq lossage-reason
				(format nil "3I could not find \"~A\" logged into any host.*" user)))))
	(t
	 (if mail-p
	     (let ((successful
		     (si:fboundp-eval
		       "Mail System is not Installed"
		       "MAIL" "SUBMIT-MAIL" message :to (list dest) :subject "Mail from Converse")))
	       (setq lossage-reason (if (not successful)
					"Mail Address is Invalid"
					NIL)))
	     (setq lossage-reason (send-msg dest message)
		   lossage (or lossage lossage-reason)))))
      (converse-record-msg-sent dest message mail-p lossage-reason lossage-reason t)))
  (not lossage)) 


(defun 4converse-record-msg-sent* (dest message mail-p error-p lossage-reason &optional inside-converse-p)
  (let ((conversation (dolist (c *converse-list*)
			(if (send c :my-name? dest)
			  (return c)))))
    (if (null conversation)
      (setq conversation (setup-conversation dest)))
    (send conversation :add-msg
       (format nil
	       "3Message ~:[~;NOT ~]~:[sent~;mailed~] to ~A (~\\DATIME\\)~@[ because~% ~A~]~%~A*"
	       error-p mail-p dest lossage-reason message))
    (when inside-converse-p
     ;1; move the point to the To: line so user can type there*
      (move-bp (point) (send conversation :after-to-line-bp))
      (must-redisplay *window* dis-text)))) 
    

(defun 4converse-edit-and-send-msg* (destination message)
  "2Initialize a message to DESTINATION with MESSAGE and select Converse to edit it.
DESTINATION is a list of strings; we initialize a message to each string
in the list if there is more than one.*"
  (let ((dest "Whomever"))       ;(parse-single-dest (car (parse-commas-into-list destination)))
    (let ((conversation
	   (or (dolist (c *converse-list*)
		 (if (send c :my-name? dest)
		   (return c)))
	      (setup-conversation dest))))
	  ;1; move the point to the To: line so user can type there*
      (send conversation :restore-to-msg)
      (move-bp (point) (end-line (send conversation :after-to-line-bp)))
      (insert-moving (point) destination)
      (insert-moving (point) #\return)
      (insert-moving (point) message)))
  (must-redisplay *window* dis-text)
  (send (find-converse-window) :select)) 


(defun 4init-converse-comtab* ()
  (if *converse-comtab*
    (ferror nil "3A *CONVERSE-COMTAB* already exists.*"))
  (setq *converse-comtab*
	(set-comtab '*converse-comtab*
		    '(#\End com-converse-handle-end
		      #\c-z com-converse-send-with-exit
		      #\Abort com-converse-abort
		      #\c-end com-converse-handle-control-end
		      #\m-{ com-converse-previous-conversation
		      #\m-} com-converse-next-conversation
		      ;1; #\meta-shift-{ for real.*
		      #\c-m com-converse-mail-message
		      #\c-m-y com-converse-yank-last-msg-text)
		    '(("3Regenerate Buffer*" . com-converse-regenerate-buffer)
		      ("3Delete Conversation*" . com-converse-delete-conversation)
		      ("3Write Buffer*" . com-converse-write-buffer)
		      ("3Write Conversation*" . com-converse-write-conversation)
		      ("3Append Buffer*" . com-converse-append-buffer)
		      ("3Append Conversation*" . com-converse-append-conversation)
		      ("3Gag Converse*" . com-gag-converse))))
  (set-comtab-indirection *converse-comtab* *standard-comtab*)) 



(defparameter 4converse-editor-closure-variables*
   (merge-closure-variable-lists
    '((*com-documentation-alist* (cons '(#\M com-converse-help) *com-documentation-alist*))
      (*major-mode* 'text-mode) (*post-command-hook* '(converse-post-command-hook)))
    top-level-editor-closure-variables)) 


(defun 4converse-post-command-hook* (&optional ignore)
  "2Post-command hook to dump out any pending requests
since they might have come in while in a break loop*"
  (send *converse-frame* :execute-queued-requests)) 


(defprop 4converse-post-command-hook* 100 command-hook-priority) 


(defflavor 4converse-frame*
	   ()
	   (converse
	    w:graphics-mixin
	    zwei-frame
	    ;1; These wouldn't be here at all if there was an activity system.*
	    ;1; Put them at the end for their daemons, don't let them shadow anything.*
	    w:process-mixin
	    w:select-mixin
	    w:minimum-window)
  (:default-init-plist 
    :process '(converse-window-top-level :special-pdl-size 4000 :regular-pdl-size 7640)
    :save-bits t
    :mode-line-list '("3Converse *" (*converse-gagged* "3[Gagged]*") "3(*" *mode-name-list* "3) *" *end-abort-message*)
    :editor-closure-variables converse-editor-closure-variables
    :comtab *converse-comtab*)) 


(defun 4converse-window-top-level* (*converse-frame*)
  "2This is the top-level function for the process*";1  (LET ((*COM-DOCUMENTATION-ALIST**
  ;	1  (CONS '(#/M COM-CONVERSE-HELP) *COM-DOCUMENTATION-ALIST*)))*
  (loop (send *converse-frame* :edit))) 


(defmethod 4(converse-frame :after :init*) (ignore)
  (let ((pane (send self :create-window 'zwei-window-pane :name "3Converse*" :expose-p t)))
    (send self :select-pane pane)
    (send pane :set-base-tick *tick*)
    (system:%using-binding-instances (closure-bindings editor-closure))
    (insert (interval-last-bp *interval*) #\Newline)
    ;1; make a headerless To: line conversation*
    (let ((c (make-instance 'conver
			    :with-header-p nil
			    :after-line (bp-line (interval-first-bp *interval*))
			    :before-line (bp-line (interval-last-bp *interval*)))))
      ;1; create the conversation list*
      (setq *converse-list* (list c)))
    (move-bp (point) (end-line (interval-first-bp *interval*)))))


(defmethod 4(converse-frame :force-kbd-input*) (input)
  (send w:selection-substitute :force-kbd-input input)) 


(defmethod 4(converse-frame :before :expose*) (&rest ignore)
  (unless (w:sheet-exposed-p self)
    (send self :force-kbd-input '(:execute initialize-for-user)))) 
  

(compile-flavor-methods converse-frame) 


(defvar 4*create-initial-converse-window-p** nil) 


(defun 4initialize-converse-command-loop* ()
  "2Initialize the command loop and window for converse*"
  (init-converse-comtab)
  (if *create-initial-converse-window-p*
    (let ((w (w:make-window 'converse-frame :activate-p t)))
      (send (send w :process) :run-reason w)))) 


(add-initialization 'start-converse '(initialize-converse-command-loop) '(:once)) 


;1;; Chaos stuff*


(defun 4converse-receive-msg* (sender msg &optional already-exposed &aux conversation)
  "2When a msg comes in, this is called to put it in the buffer*"
  ;1; If the user has set one of the obsolete flags, gobble it down.*
  (if (neq *converse-auto-expose-p* 'obsolete)
      (setq *converse-receive-mode* (if *converse-auto-expose-p*
					:auto
					:notify)
	    *converse-auto-expose-p* 'obsolete))
  (if (neq *converse-notify-with-message* 'obsolete)
      (setq *converse-receive-mode*
	    (if *converse-notify-with-message*
		:notify-with-message
		:notify)
	    *converse-notify-with-message* 'obsolete))
  ;1; Beep as desired.*
  ;1; If not in :AUTO mode and not exposed, the beeping was done already!*
  (if (or already-exposed (eq *converse-receive-mode* :auto))
      (dotimes (i *converse-beep-count*)
	(beep 'converse-message-received)))
  ;1; find or create the proper conversation for this to go in*
  (setq conversation (dolist (c *converse-list*)
		       (if (send c :my-name? sender)
			   (return c))))
  (if (null conversation)
      (setq conversation (setup-conversation sender)))
  ;1; add the message to the conversation and set up the point for easy reply*
  (send conversation :add-msg msg)
  (unless already-exposed
    (if *converse-append-p*
	(move-bp (point) (create-bp (send conversation :last-line) 0))
	(move-bp (point) (send conversation :after-to-line-bp))))
  (must-redisplay *window* dis-text)
  (cond
    ((member (send *converse-frame* :status) '(:exposed :selected) :test #'eq))
    ((eq *converse-receive-mode* :auto) (send *converse-frame* :select)))
  (typein-line "3Latest message from ~A at ~A*" sender (time:print-current-time nil))) 


(defflavor 4converse-simple-reply-window*
	   (converse-frame)
	   (w:window)
  :settable-instance-variables
  (:default-init-plist :height 620 :save-bits t :name "3Incoming Message*")) 


(defmethod 4(converse-simple-reply-window :who-line-documentation-string*) ()
  '(:mouse-1-1 "3Enter Converse.*" :mouse-2-1 "3Return to previous activities.*")) 


(defmethod 4(converse-simple-reply-window :after :deactivate*) ()
  (array-initialize w:bit-array 0)
  (send self :refresh-margins)) 
		      

(defmethod 4(converse-simple-reply-window :mouse-click*) (buttons ignore ignore)
  (case buttons
    (#\Mouse-l
     (process-run-function "3Select Converse*"
			   #'(lambda (cframe reply-window)
			       (send reply-window :deactivate)
			       (send cframe :select))
			   converse-frame self)
     t)
    (#\Mouse-m (process-run-function "3Kill Reply Window*" self :deactivate) t))) 


(compile-flavor-methods converse-simple-reply-window) 


(defwindow-resource 4converse-simple-reply-window* nil :make-window (converse-simple-reply-window)
   :reusable-when :deactivated) 


(defun 4converse-simple-reply* (converse-frame sender message)
  (using-resource (reply-window converse-simple-reply-window)
     (send reply-window :set-label (format nil "3Message from ~A*" sender))
     (send reply-window :expose nil :clean) (send reply-window :select)
     (unwind-protect (let
		      ((*terminal-io* reply-window))
		      (send reply-window :set-converse-frame converse-frame)
		      (format reply-window "3Message from ~A~%~A*" sender message)
		      (format reply-window
			      "3~&Type Y to Reply, N to do Nothing or C to enter Converse.*")
		      (case (fquery
			'(:choices
			  (((yes "3Reply.*") #\Y #\R #\T #\ #\Space)
			   ((no "3Nothing.*") #\N #\Rubout #\)
			   ((converse "3Go to Converse to read and send messages.*") #\C))
			  :stream *terminal-io* :clear-input t)
			"3Reply? *")
			(no)
			(yes (format t "3~&Type message to send to ~A*" sender)
			 (reply nil sender nil nil))
			;1;make it fast, avoid timing errors*
			(converse (send reply-window :deactivate) (send converse-frame :select))))
       (send reply-window :deactivate)))) 


;1;; If a new window is created then activate it.  Create-p is used by*
;1;; CLEAR-CONVERSE-QUEUE add-initialization. (KCW 7-8-85)*
;1;; This makes up for lack of an activity system, in several ways*

(defun 4find-converse-window* (&optional (create-p t))
  "2Returns the Converse frame.*"
  (let* ((frame
	  (cond
	    ((do ((s w:selected-window (w:sheet-superior s)))
		 ((null s)
		  nil)
	       (and (typep s 'converse-frame) (return s))))
	    ((w:find-window-of-flavor 'converse-frame))
	    (create-p (w:make-window 'converse-frame :activate-p t))))
	 (process (if frame
		    (send frame :process))))
    (or (not frame) (send process :run-reasons) (send process :run-reason frame))
    frame)) 


(defun 4last-message-sender* ()
  "2Return the username of the last person who sent us a message.*";1 (AND (NOT (= 0. (LENGTH *SAVED-SENDS*)))*
  ;1(SUBSEQ (STRING *SAVED-SENDS*) 0 (POSITION #\SPACE (THE STRING (STRING *SAVED-SENDS*)) :TEST #'CHAR-EQUAL)) ))*
  *last-converse-sender*) 


(defun 4last-message-text* ()
  "2Return the text of the last Converse message that we have received.*"
  (and (not (= 0 (length *saved-sends*)))
     (subseq (string *saved-sends*)
	(1+ (position #\Newline (the string (string *saved-sends*)) :test #'char-equal))
	(lisp:search (the string (string (string-append #\Newline #\Newline)))
		       (the string (string *saved-sends*)) :test #'char-equal)) )) 

;1;; Sending messages outside of Converse.*


(defun 4reply* (&optional message (destination *last-converse-sender*) mail-p (wait-p *converse-wait-p*))
  "2Send the message MESSAGE to the person who last sent this machine a message.
With no arguments, reads a message from the terminal.*"
  (qsend destination message mail-p wait-p)) 


(defun 4qsend* (&optional destination message (mail-p nil) (wait-p *converse-wait-p*) &aux switch-to-converse
  destination-list)
  "2Send an interactive message, MESSAGE, to the people in DESTINATION.
If MESSAGE is empty, you will be prompted for it.
If DESTINATION is empty, Converse will be selected.
If MAIL-P is NIL (the default), the message will be sent interactively, 
   otherwise the message will be mailed.
If WAIT-P is NIL, then queue up to message to be soon, but return NIL.
If WAIT-P is T, then wait until we determine the status of the messages sent, 
and return a list of the successful recipients.  The default value of the variable
is contained in the init variable ZWEI:*CONVERSE-WAIT-P*.

This function is expected to be called by a user.
Programs which call it are gauranteed to do something useful only if MESSAGE and
DESTINATION are non-NIL.*"
  (setq destination-list (parse-commas-into-list destination));1dest is now a list.*
  (cond
    ((null destination)
     ;1;person wants to select converse.  ;;maybe ("")*
     (send (find-converse-window) :select))
    ;1;do your thing*
    ((null message)
     (format t
	     "3~%Please enter a message for ~A.~%
You may send the messsage by typing ~:@C, or quit by typing ~:@C.
To yank in the last message, Type ~:@C.
You may also switch to Converse with the text of your message intact: type ~:@C.~2%*"
	     (car destination-list)
	     ;1;this could print out neater.*
	     #\End #\Abort #\c-m-y #\c-m-e)
     (multiple-value-setq (message switch-to-converse)
       (qsend-get-message *standard-input*))
     (cond
       (switch-to-converse
	(setq *awaiting-exposure* t)				;1in case of converse-problem  ;;???? -hdt*
	(send (find-converse-window) :enter-request 'converse-edit-and-send-msg destination message)
	(process-wait "3Expose Converse*" (find-converse-window) :exposed-p)
	(w:await-window-exposure)
	(setq *awaiting-exposure* nil))
       (t
	;1;didn't select converse, just send it.*
	(qsend-force-message destination message mail-p wait-p))))
    (t
     ;1;we got the message without querying user, now just send it*
     (qsend-force-message destination message mail-p wait-p)))) 


(defun 4qsend-get-message* (&optional (stream *standard-input*) ignore end-with-return-ok)
  (if (and (not rubout-handler) (member :rubout-handler (send stream :which-operations) :test #'eq))
    (send stream :rubout-handler
       '((:editing-command #\End #\c-z #\c-c #\c-m-y #\c-m-e))
       #'qsend-get-message stream nil end-with-return-ok)
    (do ((msg (make-array 100 :element-type 'string-char :leader-list '(0)))
	 (ch))
	(nil)
	 ;1;consider efficiency hack for non-control characters*
      (setq ch (read-char stream))
      (and
       (or (and (member ch '(#\End #\c-z #\c-c nil) :test #'eql))
	  (and end-with-return-ok (eql ch #\Newline)))
       ;1;why doesn't this work?*
       (return msg))
      (and (eql ch #\c-m-e)
	   (return (values msg t)))
      (cond
	((eql ch #\c-m-y)
	 (let ((text (last-message-text)))
	   (if (null text)
	     (setq text ""))
	   (string-nconc msg text)
	   (send stream :force-kbd-input text)))
	;1;normal case for a vanilla character*
	(t (vector-push-extend ch msg)))))) 


(defun 4qsend-force-message* (destination message &optional mail-p (wait-p t) &aux rcpts host)
  "2Send a Converse message from outside of Converse.
DESTINATION is a string specifying where to send it,
MESSAGE is a string giving the text,
MAIL-P is non-NIL to mail rather than send interactivelty.
WAIT-P if NIL means return NIL right away while sending in background.
Otherwise we return the list of successful recipients.*"
  (let ((dest-list (parse-commas-into-list destination)))
    (dolist (dest dest-list)
      (multiple-value-setq (dest nil host)
	(parse-single-dest dest))
      (if host
	(push dest rcpts)
	(format t "3~&No host found for recipient ~A.*" dest))))
  (if (null wait-p)
    (progn
      (process-run-function "3Qsend*" 'qsend-force-message-1 rcpts message mail-p)
      nil)
    (qsend-force-message-1 rcpts message mail-p))) 

;1;; Need to get the Converse frame BEFORE calling SEND-MSG.  Otherwise, two converse*
;1;; frames are created if you do a QSEND to yourself. (KCW 7-8-85)*

(defun 4qsend-force-message-1* (rcpts message mail-p &aux success-rcpts (cframe (find-converse-window)))
  (dolist (dest rcpts)
    (let (lossage-reason)
      (if mail-p
	  (let ((successful
		  (si:fboundp-eval
		    "Mail System is not Installed"
		    "MAIL" "SUBMIT-MAIL" message :to (list dest) :subject "Mail from Converse")))
	       (setq lossage-reason (if (not successful)
					"Mail Address is Invalid"
					NIL)))
	  (setq lossage-reason (send-msg dest message)))
      (if (not lossage-reason)
	(push dest success-rcpts))
      (send cframe :enter-delayed-request 'converse-record-msg-sent dest message mail-p
	 lossage-reason lossage-reason)))
  success-rcpts) 

;1;; modularize parsing better*

;1;; Fix so that it calls parse-host with just-verify (won't take you into*
;1;; the error handler when just verifying a host.  Also fix bug where it*
;1;; didn't insure host was non-nil before sending it a :system-type message  6/87*

(defun 4parse-single-dest* (dest &optional just-verify &aux host)
  "2Parse the string DEST as a single destination for a QSEND message.
First value is a string of the form USERNAME@HOSTNAME,
where hostname is the official name of a host,
or NIL if DEST did not specify a host and the person cannot be found.
Also returns just the username as the second value
and just the host as a third value.

If JUST-VERIFY is non-NIL, we do only enough to get an error
if the host specified is nonexistent.*"
  ;1;this code is slightly gross, most parsing has already been done.*
  (let ((@-pos (position #\@ (the string (string dest)) :test #'char-equal)))
    (cond
      ((null @-pos)
       (unless just-verify
	 (setq dest (string-trim "3 *" dest))
	 (setq host
	       (decide-host
		(net:find-hosts-or-lispms-logged-in-as-user
		  dest *converse-extra-hosts-to-check*)
		dest))))
      (t
       (setq host (si:parse-host (string-trim "3 *" (subseq (string dest) (1+ @-pos)) )
				 just-verify)                               ;1pass just-verify  dkm 6/87*
	     dest (string-trim "3 *" (subseq (string dest) 0 @-pos) ))))
    ;1; If not sending to an anonomous person on a lisp machine, and someone else is logged in to host,*
    ;1; Inform them and offer to send to the person logged in.*
    (when (and @-pos
	       host                                                         ;1be sure host is non-nil  dkm 6/87*
	       (not just-verify)
	       (member (send host :system-type) '(:lispm :lmfs))
	       (plusp (length dest)))
      (let ((logged-in-person (net:whois-logged-on-lispm host)))
	(when (and logged-in-person
		   (plusp (length logged-in-person))
		   (not (string-equal dest logged-in-person)))
	  (beep)
	  (if (y-or-n-p "3User ~s is not currently logged into host ~s. ~
                         ~%Send to ~s who is logged in?*"
			 dest (send host :name) logged-in-person)
	       (setq dest logged-in-person)
	       (progn
		 (beep)
		 (unless (y-or-n-p "3~%Send to *user 3~s who is not currently* 3logged in*to host ~s3?*"
				   dest (send host :name) )
		   (signal-condition eh:*abort-object*))))
	       )))

    (values (string-append dest #\@ (WHEN host (SEND host :fully-qualified-name))) dest (if (numberp host)
							  nil
							  host))))


(defun 4parse-commas-into-list* (string)
  "2Given a string such as \"a,b,c\" return the list (\"a\" \"b\" \"c\").
If given a list, return the list.*"
  (if (or (null string) (consp string))
    string
    (let ((comma-pos (position #\, (the string (string string)) :test #'char-equal)))
      (cond
	((not comma-pos) (list (string-trim "3 *" string)))
	(t
	 (append (list (string-trim "3 *" (subseq (string string) 0 comma-pos) ))
		 (parse-commas-into-list (subseq (string string) (1+ comma-pos)) ))))))) 



(defun 4get-official-host-name* (host)
  "2Return the official name (a string) of a host specified in any way you like.*"
  (send (si:parse-host host t) :name)) 


(defun 4decide-host* (hosts user)
  "2Ask the user to pick one of HOSTS, the hosts on which USER is logged in.
If there is only one, the user is not asked.
The selected host is returned.*"
  (cond
    ((null hosts)
     (converse-problem
      (format nil "3I could not find \"~A\" logged into any host.*"
	      (if (consp user)
		(car user)
		user))))
    ((null (cadr hosts)) (car hosts))
    (t
     (w:menu-choose
      (loop for host in hosts collect (cons (string-append user #\@ (string host)) host))
      :label "3Choose a host for outgoing message*")))) 


(defun 4converse-barf* (format-string &rest args)
  "2Print an error message from converse and abort current activity.*"
  (apply 'converse-problem format-string args)
  (throw 'zwei-command-loop
	 nil)) 


(defun 4converse-problem* (format-string &rest args)
  "2Print a warning message from converse, possibly in background (for qsend).
Message goes to *QUERY-IO* (typein window) or as a notification.*"
  (beep 'converse-problem)
  (if (send (find-converse-window) :exposed-p)
    (apply 'format *query-io* format-string args)
    (apply 'w:notify nil (string-append "3Converse is reporting a problem: ~&*" format-string)
	   args))) 


(defun 4send-msg* (destination msg &aux host person)
  "2Actually send MSG to DESTINATION.  This is the internal function that does the work.
DESTINATION must be username@hostname or a host name or number;
 multiple destinations are not allowed here.
Does not record the message sent for Converse display.
Returns NIL if successful, or an error object.*"
  (when *converse-gagged*
    (format *error-output*
	    "3~&[Note: You have gagged incoming messages.  Do (QSENDS-ON) to ungag.]~%*"))
  (cond
    ((and (not (numberp destination))
	  (setq host
		(do ((@-pos
		      (lisp:search (the string (string "3@*")) (the string (string destination))
				   :test #'char-equal)
		      (lisp:search (the string (string "3@*")) (the string (string destination))
				   :start2 (1+ @-pos) :test #'char-equal))
		     (last-@-pos nil @-pos))
		    ((null @-pos)
		     last-@-pos))))
     (setq person (nsubstring destination 0 host)
	   host (si:parse-host (nsubstring destination (1+ host) (length destination))))
	   
     (if (not (member (send host :system-type) *systems-dont-upcase-for* :test #'eq))
       (setq person (string-upcase person))));1other hosts want it in uppercase*
    (t (setq person "3anyone*"
	     host destination)))
  (with-open-stream (stream (chaos:open-stream host (string-append "3SEND *" person) :error nil :direction :output))
    (cond
      ((not (errorp stream)) 
3       *(format stream "3~A@~A ~\\DATIME\\~%*" user-id si:local-host)
       (send stream :string-out msg)
3       *(send stream :close) 
3       *nil)
      (t stream)))) 


(defun 4receive-msg* (&aux conn fline to sender tem (msg ""))
  (setq conn (chaos:listen "3SEND*"))
  (cond
    (*converse-gagged*
     (chaos:reject conn
		   (if (stringp *converse-gagged*)
		     *converse-gagged*
		     (format nil "3~A is not accepting messages.*"
			     (if (member user-id '("" nil) :test #'equal)
			       "3The user*"
			       user-id)))))
    (t
     (let* ((rfc (chaos:get-next-pkt conn)))
       (if (> (length (chaos:pkt-string rfc)) 4)
	 (setq to (subseq (string (chaos:pkt-string rfc)) 5) )
	 (setq to "3unspecified*"))
       (chaos:return-pkt rfc))
     (chaos:accept conn)
     (with-open-stream (cstream (chaos:make-stream conn :direction :input))
       (condition-bind ((system:remote-network-error 'receive-msg-condition-handler))
	  (catch 'connection-closed
	    (setq fline (send cstream :line-in))
	    (cond
	      ((setq tem (position #\@ (the string (string fline)) :test #'char-equal))
	       (setq sender (nsubstring fline 0 tem)))
	      ((setq tem (lisp:search (the string (string "3from *"))
				      (the string (string fline))
				      :test #'char-equal))
	       (setq sender (nsubstring fline
					(+ tem 5)
					(string-search-set '(#\] #\Space) fline (+ tem 5)))))
	      (t (setq sender "")))
	    (let ((host (si:get-host-from-address (chaos:foreign-address conn) :chaos)))
	      (setq sender
		    (string-append sender #\@
				   (if host
				     (send host :name)
				     (format nil "3CHAOS|~O*" (chaos:foreign-address conn))))))
	    (setq msg
		  (format nil "3~A~%~:[~*~;To: ~A~%~]~A*" fline (not (string-equal user-id to)) to
			  (with-output-to-string (stream)
			    (or
			     (catch 'connection-closed
			       (stream-copy-until-eof cstream stream)
			       t)
			     (format stream "3~%... chaos connection trouble.~%*"))))))))
     (when sender
      ;1; Don't bother recording a message if the connection*
      ;1; dropped before we even got the sender's name.*
       (setq *saved-sends* (string-append msg #\Newline #\Newline *saved-sends*))
       ;1; Save the username for the reply macro.*
       (setq *last-converse-sender* sender)
       ;1; If user wants a simple reply window, get it now.*
       ;1; Don't wake up CONVERSE.*
       (let* ((cframe (find-converse-window))
	      (exposedp (send cframe :exposed-p)))
	 (cond
	   ((and (not exposedp)
		 (member *converse-receive-mode* '(:simple :pop-up :notify :notify-with-message)
			 :test #'eq))
	    (dotimes (i *converse-beep-count*)
	      (beep 'converse-message-received))
	    (send cframe :enter-delayed-request 'converse-receive-msg sender msg)
	    (case *converse-receive-mode*
	      ((:simple :pop-up)
	       (process-run-function "3Reply to Message*" 'converse-simple-reply cframe sender msg))
	      ((:notify :notify-with-message)
	       (w:notify cframe "3Converse message received from ~A*"
			  (if (eq *converse-receive-mode* :notify-with-message)
			    (string-right-trim '(#\Newline #\Space) msg)
			    sender)))))
	   (t (send cframe :enter-request 'converse-receive-msg sender msg exposedp)))))))) 


(defun 4receive-msg-condition-handler* (&rest ignore)
  (throw 'connection-closed
	 nil)) 


(defun 4print-sends* (&optional (stream *standard-output*))
  "2Print out all messages received from other users.*"
  (send stream :fresh-line)
  (send stream :string-out *saved-sends*)) 

;1;; Now install it*

(add-initialization "3SEND*" '(process-run-function "3CONVERSE*" #'receive-msg) nil
		    'chaos:server-alist) 

(W:MODIFY-SYSTEM-ACCESS-SPEC 'converse		; make call to new function
			     :ASSIGN-DEFAULTS)

(SETF (SYS:SYSTEM-MADE-P 'CONVERSE) T)

(defun debug-converse-buffer (buffer)
  
  ;;; handy for looking at the current structure of a mail buffer.
  ;;; Changed often depending on what is of interest at the moment
  
  (let (last-line)
    (dolist (conversation (node-inferiors buffer))
      (format t "~%>>> ~S <<<" conversation)
      (when last-line
	(unless (and (eq (line-previous (bp-line (interval-first-bp conversation))) last-line)
		     (eq (line-next last-line) (bp-line (interval-first-bp conversation))))
	  (format t "~%>>> FIRST LINE DOES NOT FOLLOW LAST LINE OF PREVIOUS MESSAGE <<<")))
      (loop
	with last = (bp-line (interval-last-bp conversation))
	for line = (bp-line (interval-first-bp conversation)) then (line-next line)
	do
	(unless line
	  (format t "~%~%>>> RAN OUT OF LINES WITHOUT HITTING INTERVAL-LAST-BP <<<~%")
	  (return))
	(format t "~%~A" line)
	(when (line-bp-list line)
	  (format t "~%>>> BP LIST: ")
	  (pprint (line-bp-list line)))
	(when (eq line last)
	  (setq last-line line)
	  (return))))))
