;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(CPTFONT HL12B CPTFONT); Base:10 -*-

;1;;*			1      RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;*			1TEXAS INSTRUMENTS INCORPORATED.*
;1;;*				1 P.O. BOX 2909*
;1;;*			1      AUSTIN, TEXAS 78769*
;1;;*				1    MS 2151*
;1;;*
;1;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.*

;1;; =====================================================================================*
;1;;                              F T P    U S E R    I N T E R F A C E *
;1;;*
;1;; Author: Rhonda Alexander*
;1;;*
;1;; History: *
;1;;   5/1/85 - Original *
;1;;   8/13/85 - Modified to use new FTP interface*
;1;;*
;1;; External dependencies:*
;1;;   fs:ftp-unfavorable*
;1;;   fs:construct-dir-list*
;1;; =====================================================================================*


;1; This combination flavor creates a temporary window to which output can be done and margin choices added.*

(DEFFLAVOR FTP-STATUS-WINDOW-FLAVOR
	   ()
	   (TV::TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
	    TV:STREAM-MIXIN
	    TV:BORDERS-MIXIN
	    TV:MARGIN-CHOICE-MIXIN
	    TV:BOX-LABEL-MIXIN
	    TV:TOP-LABEL-MIXIN
	    TV:LABEL-MIXIN;1    tv:not-externally-selectable-mixin*
	    TV:SELECT-MIXIN;1    tv:delay-notification-mixin*
	    TV:GRAPHICS-MIXIN
	    TV:MINIMUM-WINDOW)) 

;1;; Keep window from appearing in select menu after ftp has exited but allow mouse click to be handled gracefully*

(DEFMETHOD (FTP-STATUS-WINDOW-FLAVOR :NAME-FOR-SELECTION) ()) 


(DEFCONSTANT *FTP-MORE-LEGEND* "** MORE BELOW - PRESS SPACE BAR TO CONTINUE **") 

(DEFCONSTANT *FTP-NOMORE-LEGEND*
   "** LISTING COMPLETE - PRESS SPACE BAR WHEN FINISHED VIEWING **") 

;1;; redefine this method to use a different string and clear screen in between*

(DEFMETHOD (FTP-STATUS-WINDOW-FLAVOR :MORE-EXCEPTION) ()
  (TV:SHEET-MORE-HANDLER :TYI *FTP-MORE-LEGEND*)
  (SEND SELF :CLEAR-SCREEN)) 


(DEFCONSTANT *STATUS-WINDOW-APPROXIMATE-WIDTH* 72)  ;1used to decide whether or not to center status msg*


(DEFCONSTANT *REQUIRED-LEGEND* '("" "" "** = REQUIRED FIELD")) 


(DEFCONSTANT *REQUIRED-MARK* "**") 


(DEFCONSTANT *PATHNAME-DOC-STRING*
   "File pathname (DO NOT include host).  Working directory defaults may be assumed.
  <L: input a new value from the keyboard,  R: edit this value>"
   ) 


(DEFCONSTANT *DIR-LIST-DOC-STRING*
   "Directory, file, or wildcarded pathname (DO NOT include host).  Working directory defaults may be assumed.
  <L: input a new value from the keyboard,  R: edit this value>"
   ) 


(DEFCONSTANT *DIR-OP-DOC-STRING*
   "Directory pathname (DO NOT include host).  
  <L: input a new value from the keyboard,  R: edit this value>"
   ) 


(DEFCONSTANT *REMOTE-HOST-FRAME*
   '("" (*REMOTE-HOST* "** Remote Host" :STRING) (*USER-ID* "** User Id" :STRING)
     (*PASSWORD* "   Password" :HIDDEN-STRING) (*ACCOUNT* "   Account" :STRING) "")) 


(DEFCONSTANT *FILE-PARMS-FRAME*
   '(""
     (*REP-TYPE* "   Representation Type" :CHOOSE (:ASCII :EBCDIC :IMAGE :BYTE-SIZE))
     (*FILE-FORMAT* "   File Format (only for ASCII or EBCDIC)" :CHOOSE
      (:NON-PRINT :TELNET :CARRIAGE-CONTROL))
     (*LOCAL-BYTE-SIZE* "   Byte Size (only for Byte-Size representation)" :NUMBER)
     (*FILE-STRUCTURE* "   File Structure" :CHOOSE (:FILE :RECORD :PAGE))
     (*TRANSFER-MODE* "   Transfer Mode" :CHOOSE (:STREAM :BLOCK :COMPRESSED)))) 


(DEFCONSTANT *XFER-FRAME*
   (APPEND '("" (*REPLACE-APPEND-OPTION* "   Replace or Append?" :CHOOSE (:REPLACE :APPEND)))
	   *FILE-PARMS-FRAME*
	   '(""
	     (*PRE-ALLOCATE-P* "   Pre-allocate?" :BOOLEAN)
	     (*ALLOCATION-SIZE* "   Allocation (bytes)" :NUMBER)
	     (*ALLOCATION-RECORD-SIZE* "   Maximum record length (bytes)" :NUMBER)
	     ))) 


(DEFCONSTANT *GET-XFER-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*REMOTE-PATHNAME* "** Remote Pathname (source)" :DOCUMENTATION
	      ,*PATHNAME-DOC-STRING* :STRING)
	     (*LOCAL-PATHNAME* "** Local Pathname (destination)" :DOCUMENTATION
	      ,*PATHNAME-DOC-STRING* :STRING))
	   *XFER-FRAME* *REQUIRED-LEGEND*)) 


(DEFCONSTANT *PUT-XFER-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*LOCAL-PATHNAME* "** Local Pathname (source)" :DOCUMENTATION
	      ,*PATHNAME-DOC-STRING* :STRING)
	     (*REMOTE-PATHNAME* "** Remote Pathname (destination)" :DOCUMENTATION
	      ,*PATHNAME-DOC-STRING* :STRING))
	   *XFER-FRAME* *REQUIRED-LEGEND*)) 


(DEFCONSTANT *VIEW-FILE-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*REMOTE-PATHNAME* "** Remote Pathname" :DOCUMENTATION ,*PATHNAME-DOC-STRING*
	      :STRING))
	   *FILE-PARMS-FRAME* *REQUIRED-LEGEND*)) 


;1; Now used only for deletes*

(DEFCONSTANT *ONE-FILE-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*REMOTE-PATHNAME* "** Remote Pathname" :DOCUMENTATION ,*PATHNAME-DOC-STRING*
	      :STRING))
	   *REQUIRED-LEGEND*)) 

;1;; used for change, create, delete directory*

(DEFCONSTANT *DIR-OP-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*DIRECTORY-PATHNAME* "** Directory Pathname" :DOCUMENTATION
	      ,*DIR-OP-DOC-STRING* :STRING))
	   *REQUIRED-LEGEND*)) 


(DEFCONSTANT *DIR-LIST-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*DIRECTORY-PATHNAME* "   Directory Pathname" :DOCUMENTATION
	      ,*DIR-LIST-DOC-STRING* :STRING))
	   *REQUIRED-LEGEND*)) 



(DEFCONSTANT *RENAME-MENU-LAYOUT*
   (APPEND *REMOTE-HOST-FRAME*
	   `((*REMOTE-PATHNAME* "** Old Remote Pathname" :DOCUMENTATION
	      ,*PATHNAME-DOC-STRING* :STRING)
	     (*NEW-PATHNAME* "** New Remote Pathname" :DOCUMENTATION ,*PATHNAME-DOC-STRING*
	      :STRING))
	   *REQUIRED-LEGEND*)) 


(DEFCONSTANT *EXPERT-MENU-LAYOUT*
   (APPEND
    '("" (*REMOTE-HOST* "** Remote host" :STRING) ""
      (*FTP-EXPERT-COMMAND* "** FTP Command (string)" :DOCUMENTATION
       "Enter command as \"<cmd> {<arg 1> ... <arg n>}\"
 This is treated as a string (no evaluation is done)"
       :STRING))
    *REQUIRED-LEGEND*)) 


(DEFCONSTANT *THIRD-PARTY-XFER-MENU-LAYOUT*
   (APPEND
    `("" (*REMOTE-HOST* "** Remote Host (source)" :STRING)
      (*USER-ID* "** User Id (source host)" :STRING)
      (*PASSWORD* "   Password (source host)" :HIDDEN-STRING)
      (*ACCOUNT* "   Account (source host)" :STRING)
      (*REMOTE-PATHNAME* "** Remote Pathname (source file)" :DOCUMENTATION
       ,*PATHNAME-DOC-STRING* :STRING)
      "" (*DEST-HOST* "** Remote Host (destination)" :STRING)
      (*DEST-USER* "   User Id (destination host)" :STRING)
      (*DEST-PASSWORD* "   Password (destination host)" :HIDDEN-STRING)
      (*DEST-ACCOUNT* "   Account (destination host)" :STRING)
      (*DEST-PATHNAME* "** Remote Pathname (destination file)" :DOCUMENTATION
       ,*PATHNAME-DOC-STRING* :STRING))
    *XFER-FRAME* *REQUIRED-LEGEND*)) 


(DEFCONSTANT *NEW-HOST-MENU-LAYOUT*
   (APPEND
    '("" (*NEW-HOST* "** New host name" :STRING)
      (*HOST-ADDRESS* "** IP address"
       :DOCUMENTATION
       "Enter as a number (N, #xN, #oN, #bN) or as IP|<addr> (<addr>=o,o,o,o OR d.d.d.d OR d:d)"
       :STRING))
    *REQUIRED-LEGEND*)) 


(DEFCONSTANT *DESCRIBE-HOST-MENU-LAYOUT*
   (APPEND
    '(""
      (*DESCRIBE-HOST* "** Host name or IP address" :DOCUMENTATION
       "Enter name or address as a number (N, #xN, #oN, #bN) or as IP|<addr> (<addr>=o,o,o,o OR d.d.d.d OR d:d)"
       :STRING))
    *REQUIRED-LEGEND*)) 


(DEFCONSTANT *FTP-NEGATIVE-REPLY* 400) 

(DEFCONSTANT *MENU-ERROR* ()) 


(DEFPARAMETER *NULL-WINDOW-VALUE* (MAKE-ARRAY 1 :ELEMENT-TYPE 'STRING-CHAR)) 	;1initial-value = " " (see also FTPINT version)*


(DEFCONSTANT *CVV-CHOICE-LIST* '("2Do it [�]*" ("Abort [�]" (THROW 'ABORT ())))) 


;1;; Defaults and current values for ftp parameters *

(DEFVAR *REMOTE-HOST* "") 

(DEFVAR *USER-ID* "") 

(DEFVAR *PASSWORD* "") 

(DEFVAR *ACCOUNT* "") 

(DEFVAR *REMOTE-PATHNAME* "") 

(DEFVAR *LOCAL-PATHNAME* "") 

(DEFVAR *NEW-PATHNAME* "") 

(DEFVAR *REPLACE-APPEND-OPTION* :REPLACE) 

(DEFVAR *FILE-STRUCTURE* :FILE) 

(DEFVAR *TRANSFER-MODE* :STREAM) 

(DEFVAR *REP-TYPE* :ASCII) 

(DEFVAR *FILE-FORMAT* :NON-PRINT) 

(DEFVAR *LOCAL-BYTE-SIZE* 8) 

(DEFVAR *PRE-ALLOCATE-P* ()) 

(DEFVAR *ALLOCATION-SIZE* 0) 

(DEFVAR *ALLOCATION-RECORD-SIZE* 0) 

(DEFVAR *DEST-HOST* "") 

(DEFVAR *DEST-USER* "") 

(DEFVAR *DEST-PASSWORD* "") 

(DEFVAR *DEST-ACCOUNT* "") 

(DEFVAR *DEST-PATHNAME* "") 
 

(DEFVAR *DIRECTORY-PATHNAME* ""
   ) 


(DEFVAR *OLD-USER-ID* "") 

(DEFVAR *OLD-PASSWORD* "") 

(DEFVAR *OLD-ACCOUNT* "") 


(DEFVAR *FTP-STATUS-WINDOW* ()) 

(DEFVAR *FTP-DISPLAY-WINDOW* ()) 

(DEFVAR *FTP-WAIT-WINDOW* ()) 

(DEFVAR *FTP-DISPLAY-ABORTED* ()) 

;1;; set by ftp-call*

(DEFVAR *FTPI-CONTROL-CONNECTION* ()) 

(DEFVAR *FTP-STATUS-CODE*) 

(DEFVAR *FTP-STATUS-MSG*) 

(DEFVAR *FTP-INFORMATION*) 

(DEFVAR *FTP-UNFAVORABLE*) 

(DEFVAR *FTP-LAST-COMMAND* ()) 


(DEFVAR *FTP-EXPERT-COMMAND* "") 

(DEFVAR *FTP-STREAM-HANDLER* ()) 


(DEFVAR *GLOBAL-FTP-EXIT*) 		   ;1set by display-status based on user choice*

(DEFVAR *FTP-CONNECTED*) 

(DEFVAR *FTP-USER-LOGGED-IN*) 

(DEFVAR *FTP-OVERRIDE-LOGIN*) 


(DEFVAR *FTP-DEBUG* ()) 		   ;1set this true to see debug output*

(DEFVAR *FTP-STATS* ()) 


(DEFVAR *NEW-HOST* ""
   ) 

(DEFVAR *HOST-ADDRESS* ""
   ) 

(DEFVAR *DESCRIBE-HOST* "") 


(DEFVAR *IN-FTPI* ())  
;1; used for break disabling*

(DEFVAR NEW-SYNCH) 

(DEFVAR NEW-ASYNCH) 

(DEFVAR OLD-SYNCH-CHARS) 

(DEFVAR OLD-ASYNCH-CHARS) 


;1;; Implementation of :HIDDEN-STRING type for CVV*
;1;; rla 10/17/86 - moved here from FTPINT*
(DEFPARAMETER HIDDEN-WINDOW-CHAR #\ESCAPE) 

(DEFUN CVV-PRINT-HIDDEN-STRING (VALUE STREAM)
  (WHEN (STRINGP VALUE)
    (IF (STRING-EQUAL VALUE *NULL-WINDOW-VALUE*)
      (SEND STREAM :STRING-OUT VALUE)
      (LOOP FOR I FROM 1 TO (LENGTH VALUE)
	    DO (SEND STREAM :TYO HIDDEN-WINDOW-CHAR))))) 


(DEFUN CVV-READ-HIDDEN-STRING (STREAM &AUX CH)
  (WITH-OUTPUT-TO-STRING (VALSTREAM)
    (LOOP
      (SETQ CH (READ-CHAR STREAM))
      (WHEN (CHAR-EQUAL CH #\NEWLINE)
	(RETURN))
      (SEND STREAM :BACKWARD-CHAR) (SEND STREAM :CLEAR-CHAR)
      (SEND STREAM :TYO HIDDEN-WINDOW-CHAR) (SEND VALSTREAM :TYO CH)))) 

(DEFPROP :HIDDEN-STRING (CVV-PRINT-HIDDEN-STRING CVV-READ-HIDDEN-STRING ())
   TV:CHOOSE-VARIABLE-VALUES-KEYWORD) 
;1;;--------------------------------*


(ADD-INITIALIZATION "Clear FTP-Window passwords" '(FTP-WINDOW-CLEAR) '(:LOGOUT :NORMAL)) 


(DEFUN FTP-WINDOW-CLEAR ()
  (SETQ *DEST-PASSWORD* ())
  (SETQ *PASSWORD* ())) 

;1;;--------------------------------------------------------------------------------------*
;1;; FTP*
;1;;   This is the top-level routine for the FTP window interface*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP (&OPTIONAL VERBOSE)
  "2Window interface to the TCP/IP File Transfer Protocol*"
  (WHEN *IN-FTPI*
    (FERROR nil "Only one concurrent invocation of FTP Window Interface is allowed"))
  (SETQ *FTP-STATS* VERBOSE)
  (SETQ *GLOBAL-FTP-EXIT* ());1start at nil with every invocation*
  (SETQ *FTP-CONNECTED* ())
  (SETQ *FTP-USER-LOGGED-IN* ())
  (SETQ *FTP-OVERRIDE-LOGIN* ())
  (SETQ *FTPI-CONTROL-CONNECTION* (MAKE-INSTANCE 'FTP-CONTROL-CONNECTION))
  (WHEN (STRING-EQUAL *USER-ID* "")
    (SETQ *USER-ID* USER-ID))
  (UNWIND-PROTECT (PROGN
		    (SETQ *IN-FTPI* T)
		    (DISABLE-BREAK)
		    (FTPI))
		   ;1; GUARANTEED CLEAN-UP*
    (ENABLE-BREAK)
    (SETQ *IN-FTPI* ())
    ;1; log-out to end session*
    (WHEN *FTP-CONNECTED*
      (FTP-CALL2 :QUIT))
    (WHEN *FTP-STATUS-WINDOW*
      (SEND *FTP-STATUS-WINDOW* :DESELECT)
      (SEND *FTP-STATUS-WINDOW* :DEEXPOSE))
    (WHEN *FTP-DISPLAY-WINDOW*
      (SEND *FTP-DISPLAY-WINDOW* :DESELECT)
      (SEND *FTP-DISPLAY-WINDOW* :DEEXPOSE))
    (WHEN (AND *FTP-STATS* *FTP-WAIT-WINDOW*)
      (SEND *FTP-WAIT-WINDOW* :DESELECT)
      (SEND *FTP-WAIT-WINDOW* :DEEXPOSE)))
  "Exited FTP";1return value*
) 

(DEFUN FTPI ()
  (LET* ((ITEM '("View a file at a remote host" :VALUE :VIEW))
	 (FTPI-MAIN-MENU
	  (APPEND
	   '(("" :NO-SELECT NIL) ("HOST OPERATIONS"
				  :NO-SELECT NIL :FONT FONTS:HL12B)
	     ("Describe a TCP/IP host" :VALUE :HOST-DESCRIBE)
	     ("Define a temporary TCP/IP host"
	      :VALUE :HOST-DEFINE :DOCUMENTATION
	      "Create a temporary host (not usable in pathnames);
 (Use NetConfig to create permanent hosts)."
	      )
	     (""
	      :NO-SELECT NIL) ("DIRECTORY OPERATIONS" :NO-SELECT NIL :FONT FONTS:HL12B)
	     ("Create a directory"
	      :VALUE :CREATE-DIR :DOCUMENTATION
	      "** NOTE: some FTP servers may not support this operation")
	     ("Delete a directory"
	      :VALUE :DELETE-DIR :DOCUMENTATION
	      "** NOTE: some FTP servers may not support this operation")
	     ("Set working directory"
	      :VALUE :CHANGE-DIR)
	     ("List names of files in a directory" :VALUE :NAME-LIST :DOCUMENTATION
	      "List only the names of files in a given directory")
	     ("Show directory information for file(s)" :VALUE :DIR-LIST :DOCUMENTATION
	      "List both names and information about the files in a given directory")
	     ("" :NO-SELECT NIL) ("FILE OPERATIONS" :NO-SELECT NIL :FONT FONTS:HL12B))
	   ;1;; VIEW A FILE*
	   (LIST ITEM)
	   '(("Copy a local file to a remote host"
	      :VALUE :SND)
	     ("Copy a file from a remote host to a local file" :VALUE :RCV)
	     ("Copy a file between remote hosts" :VALUE :PROXY-XFER)
	     ("Rename a file at a remote host" :VALUE :RENAME)
	     ("Delete a file at a remote host" :VALUE :DELETE) ("" :NO-SELECT NIL)
	     ("EXPERT MODE" :NO-SELECT NIL :FONT FONTS:HL12B)
	     ("Send explicit low-level command to FTP" :VALUE :EXPERT)
	     ("Toggle VERBOSE option" :VALUE :TOGGLE :DOCUMENTATION
	      "Complement the option for displaying additional information about the progress of FTP operations")
	     ("Reset TCP/IP" :VALUE :RESET :DOCUMENTATION
	      "Reset all TCP/IP components (all connections will be cleared)")
	     ("Break"
	      :VALUE :BREAK :DOCUMENTATION "Temporarily enter a read-eval-print loop"
	      )
	     ("" :NO-SELECT NIL) ("HELP" :NO-SELECT NIL :FONT FONTS:HL12B)
	     ("Introduction to FTP" :VALUE :HELP-INTRO)
	     ("List expert commands" :VALUE :HELP-EXPERT) ("" :NO-SELECT NIL)
	     ("" :NO-SELECT NIL)
	     ("    ** MOVE MOUSE OUT OF WINDOW TO EXIT **    " :NO-SELECT NIL :FONT FONTS:HL12BI))))
	 RESULT)
	 ;1; put up main menu (it calls other routines based on choice)*
    (LOOP
     (MULTIPLE-VALUE-SETQ (RESULT ITEM)
       (W:MENU-CHOOSE FTPI-MAIN-MENU :LABEL "FTP - File Transfer Protocol"))
     ;1; Window system doc says that item is nil when mouse is moved away (exit).  However, when*
     ;1; an item is passed as the cursor position, this doesn't work.  In our case, though, the value*
     ;1; returned is nil only under this condition so we can check that instead.*
     (UNLESS RESULT
       (RETURN ()));1 exit if mouse moved out of window*
     (WHEN *FTP-DEBUG*
       (PRINT (FORMAT () "*** CMD CHOICE=~A" RESULT)))
     ;1; Repeat the sub-menus as long as the user requests a retry after viewing the status menu *
     ;1;   (the submenu routines will return :retry or nil)*
     ;1;   (this is the reason for not using :evals in the menu definition) *
     (CONDITION-CASE ()
	(LOOP (SET-VARIABLE-DEFAULTS)
	   (UNLESS (SELECT RESULT (:HOST-LIST (HOST-LIST)) (:HOST-DEFINE (HOST-DEFINE))
	       (:HOST-DESCRIBE (HOST-DESCRIBE))
	       (:NAME-LIST
		(IF (ONE-FILE-MENU *DIR-LIST-MENU-LAYOUT* "LIST NAMES OF FILES IN A DIRECTORY")
		  (REMOTE-NAME-LIST)
		  ()))
	       (:DIR-LIST
		(IF (ONE-FILE-MENU *DIR-LIST-MENU-LAYOUT* "SHOW DIRECTORY INFORMATION FOR FILE(S)")
		  (REMOTE-DIR-LIST)
		  ()))
	       (:CREATE-DIR
		(IF (ONE-FILE-MENU *DIR-OP-MENU-LAYOUT* "CREATE A DIRECTORY"
				)
		  (REMOTE-DIR-OP :CREATE-DIRECTORY)
		  ()))
	       (:DELETE-DIR
		(IF (ONE-FILE-MENU *DIR-OP-MENU-LAYOUT* "DELETE A DIRECTORY"
				)
		  (REMOTE-DIR-OP :DELETE-DIRECTORY)
		  ()))
	       (:CHANGE-DIR
		(IF (ONE-FILE-MENU *DIR-OP-MENU-LAYOUT* "SET WORKING DIRECTORY"
				)
		  (REMOTE-DIR-OP :CHANGE-DIRECTORY)
		  ()))
	       (:VIEW (REMOTE-VIEW-FILE))
	       (:SND
		(SIMPLE-XFER-MENU 'PUT *PUT-XFER-MENU-LAYOUT*
				  "COPY A LOCAL FILE TO A REMOTE HOST"))
	       (:RCV
		(SIMPLE-XFER-MENU 'GET *GET-XFER-MENU-LAYOUT*
				  "COPY A FILE FROM A REMOTE HOST TO A LOCAL FILE"))
	       (:PROXY-XFER (THIRD-PARTY-XFER-MENU)) (:RENAME (RENAME-MENU))
	       (:DELETE
		(IF (ONE-FILE-MENU *ONE-FILE-MENU-LAYOUT* "DELETE A FILE AT A REMOTE HOST")
		  (REMOTE-DELETE)
		  ()))
	       (:EXPERT (EXPERT-MENU)) (:BREAK (HANDLE-BREAK)) (:RESET (HANDLE-RESET))
	       (:TOGGLE (HANDLE-TOGGLE)) (:HELP-INTRO (DISPLAY-INTRO-HELP))
	       (:HELP-EXPERT (DISPLAY-EXPERT-HELP)))
	     (RETURN ())));1 end do*
	(SYSTEM:ABORT))
     (WHEN *GLOBAL-FTP-EXIT*
       (RETURN ()));1 exit if user requested a top-level exit in status-window*
     );1 end  do*
))  ;1end let, defun *


;1;;--------------------------------------------------------------------------------------*
;1;; SET-VARIABLE-DEFAULTS*
;1;;   - set any null variables to [] for window display*
;1;;--------------------------------------------------------------------------------------*

(DEFUN SET-VARIABLE-DEFAULTS ()
  (DOLIST (V
    '(*REMOTE-HOST* *USER-ID* *PASSWORD* *ACCOUNT* *REMOTE-PATHNAME* *LOCAL-PATHNAME*
      *NEW-PATHNAME* *DEST-HOST* *DEST-USER* *DEST-PASSWORD* *DEST-ACCOUNT* *DEST-PATHNAME*
      *DIRECTORY-PATHNAME* *NEW-HOST* *HOST-ADDRESS* *FTP-EXPERT-COMMAND*))
    (WHEN (OR (NULL (SYMBOL-VALUE V)) (STRING-EQUAL (SYMBOL-VALUE V) ""
					       ))
      (SETF (SYMBOL-VALUE V) *NULL-WINDOW-VALUE*)))) 


;1;;--------------------------------------------------------------------------------------*
;1;; HANDLE-BREAK*
;1;;--------------------------------------------------------------------------------------*

(DEFUN HANDLE-BREAK (&AUX RESULT)
  (ENABLE-BREAK)
  ;1; Second value returned by break is non-nil if abort was pushed to end break*
  (MULTIPLE-VALUE-SETQ (RESULT *GLOBAL-FTP-EXIT*)
    (BREAK "by FTP EXPERT"
	   ))
  (REDISABLE-BREAK)
  ()) 						;1don't retry*

;1;;--------------------------------------------------------------------------------------*
;1;; DISABLE-BREAK*
;1;;   -- Break sticks us in window lock and resume doesn't really*
;1;;--------------------------------------------------------------------------------------*

(DEFUN DISABLE-BREAK (&AUX ELEM BREAK-CHARS)
 ;1; To avoid using these in reset if we get aborted in this routine*
  (SETQ OLD-ASYNCH-CHARS ())
  (SETQ OLD-SYNCH-CHARS ())
  (SETQ NEW-SYNCH (COPY-LIST TV:KBD-INTERCEPTED-CHARACTERS))
  (SETQ NEW-ASYNCH (COPY-LIST TV::KBD-STANDARD-ASYNCHRONOUS-CHARACTERS))
  (SETQ BREAK-CHARS
	(LIST (CHAR-INT #\BREAK) (CHAR-INT #\c-BREAK) (CHAR-INT #\m-BREAK)
	      (CHAR-INT #\c-m-BREAK)))
  (DOLIST (BC BREAK-CHARS)
    (WHEN (SETQ ELEM (ASSOC BC NEW-ASYNCH :TEST #'EQL))
      (SETQ NEW-ASYNCH (DELETE ELEM (THE LIST NEW-ASYNCH) :TEST #'EQUAL)))
    (WHEN (SETQ ELEM (ASSOC BC NEW-SYNCH :TEST #'EQL))
      (SETQ NEW-SYNCH (DELETE ELEM (THE LIST NEW-SYNCH) :TEST #'EQUAL))))
  (SETQ OLD-ASYNCH-CHARS TV::KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)
  (SETQ OLD-SYNCH-CHARS TV:KBD-INTERCEPTED-CHARACTERS)
  (SETQ TV::KBD-STANDARD-ASYNCHRONOUS-CHARACTERS NEW-ASYNCH)
  (SETQ TV:KBD-INTERCEPTED-CHARACTERS NEW-SYNCH)) 

;1;;--------------------------------------------------------------------------------------*
;1;; ENABLE-BREAK*
;1;;--------------------------------------------------------------------------------------*

(DEFUN ENABLE-BREAK ()
  (WHEN OLD-ASYNCH-CHARS
    (SETQ TV::KBD-STANDARD-ASYNCHRONOUS-CHARACTERS OLD-ASYNCH-CHARS))
  (WHEN OLD-SYNCH-CHARS
    (SETQ TV:KBD-INTERCEPTED-CHARACTERS OLD-SYNCH-CHARS))) 

;1;;--------------------------------------------------------------------------------------*
;1;; REDISABLE-BREAK*
;1;;   - when we have already done this once*
;1;;--------------------------------------------------------------------------------------*

(DEFUN REDISABLE-BREAK ()
  (SETQ TV::KBD-STANDARD-ASYNCHRONOUS-CHARACTERS NEW-ASYNCH)
  (SETQ TV:KBD-INTERCEPTED-CHARACTERS NEW-SYNCH)) 

;1;;--------------------------------------------------------------------------------------*
;1;; HANDLE-RESET*
;1;;--------------------------------------------------------------------------------------*

(DEFUN HANDLE-RESET ()
  (SETQ *FTP-CONNECTED* ())
  (SETQ *FTP-USER-LOGGED-IN* ())
  (SETQ *FTPI-CONTROL-CONNECTION* (MAKE-INSTANCE 'FTP-CONTROL-CONNECTION))
  (IP:RESET :ENABLE)
  ()) 						;1don't retry*

;1;;--------------------------------------------------------------------------------------*
;1;; HANDLE-TOGGLE*
;1;;--------------------------------------------------------------------------------------*

(DEFUN HANDLE-TOGGLE (&AUX CURR-VAL NEW-VAL)
  (SETQ CURR-VAL (IF *FTP-STATS*
		   "ON"
		   "OFF"))
  (SETQ NEW-VAL (IF *FTP-STATS*
		  "OFF"
		  "ON"))
  (WHEN (TV::MOUSE-CONFIRM (FORMAT () " VERBOSE option is currently ~A;" CURR-VAL)
		      (FORMAT ()
			      " Click inside box to switch ~A, move mouse outside to leave ~A. "
			      NEW-VAL CURR-VAL))
    (SETQ *FTP-STATS* (NOT *FTP-STATS*)))
  ()) 						;1don't retry*


;1;;--------------------------------------------------------------------------------------*
;1;; ONE-FILE-MENU*
;1;;   This function does the input operations which require one remote pathname.*
;1;;    It returns t if the values were entered successfully, nil if the user specified ABORT *
;1;;--------------------------------------------------------------------------------------*

(DEFUN ONE-FILE-MENU (MENU-LAYOUT MENU-NAME)
  (CATCH 'ABORT
    (TV:CHOOSE-VARIABLE-VALUES MENU-LAYOUT :LABEL MENU-NAME :MARGIN-CHOICES *CVV-CHOICE-LIST*
			       :EXTRA-WIDTH 40 :superior w:mouse-sheet)
    T)) 

;1;;--------------------------------------------------------------------------------------*
;1;; RENAME-MENU*
;1;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN RENAME-MENU ()
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *RENAME-MENU-LAYOUT* :LABEL "RENAME A FILE AT A REMOTE HOST"
				:MARGIN-CHOICES *CVV-CHOICE-LIST* :EXTRA-WIDTH 40
				:superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS *RENAME-MENU-LAYOUT*)
    (UNLESS *FTP-UNFAVORABLE*
      (FTP-REMOTE-RENAME))
    ;1; if all went well, change default remote pathname to new pathname *
    (UNLESS *FTP-UNFAVORABLE*
      (SETQ *REMOTE-PATHNAME* *NEW-PATHNAME*))
    ;1; display status (and return its value (:retry or nil) as the value of this function)*
    ;1; also clear the new pathname unless needed for a retry at the same menu*
    (IF (NULL (DISPLAY-STATUS))
     ;1; then*
      (PROGN
	(SETQ *NEW-PATHNAME* "")
	())
      ;1; else*
      :RETRY))) 

;1;;--------------------------------------------------------------------------------------*
;1;; SIMPLE-XFER-MENU*
;1;   returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*


(DEFUN SIMPLE-XFER-MENU (OP-NAME MENU-LAYOUT MENU-NAME)
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES MENU-LAYOUT :LABEL MENU-NAME :MARGIN-CHOICES *CVV-CHOICE-LIST*
				:EXTRA-WIDTH 40 :superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS MENU-LAYOUT)
    (UNLESS *FTP-UNFAVORABLE*
      (IF (EQ OP-NAME 'PUT)
       ;1; THEN send a local file to a remote host*
	(FTP-SEND-REMOTE-FILE)
	;1; ELSE retrieve a remote file and store locally*
	(FTP-GET-REMOTE-FILE))
      (WHEN (AND *FTP-STATS* (NOT *FTP-UNFAVORABLE*))
	(SETQ *FTP-STATUS-MSG*
	      (STRING-APPEND *FTP-STATUS-MSG*
			     (FORMAT () "~%~%Bytes=~D, Seconds=~D, Baud rate=~D"
				     (THIRD *FTP-INFORMATION*) (FOURTH *FTP-INFORMATION*)
				     (FIFTH *FTP-INFORMATION*))))))
    ;1; display status (and return its value (:retry or nil) as the value of this function)*
    (DISPLAY-STATUS));1 end when*
) 

;1;;--------------------------------------------------------------------------------------*
;1;; THIRD-PARTY-XFER-MENU*
;1;;--------------------------------------------------------------------------------------*


(DEFUN THIRD-PARTY-XFER-MENU ()
 ;1; use the same user id for both hosts unless it has been explicitly changed*
  (WHEN (EMPTY-PARM *DEST-USER*)
    (SETQ *DEST-USER* *USER-ID*)
    (SETQ *DEST-PASSWORD* *PASSWORD*)
    (SETQ *DEST-ACCOUNT* *ACCOUNT*))
  (WHEN (EMPTY-PARM *DEST-HOST*)
    (SETQ *DEST-HOST* *REMOTE-HOST*))
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *THIRD-PARTY-XFER-MENU-LAYOUT* :LABEL
				"COPY A FILE BETWEEN REMOTE HOSTS" :MARGIN-CHOICES
				*CVV-CHOICE-LIST* :EXTRA-WIDTH 40 :HEIGHT 425;1keep this from being scrollable*
				:superior w:mouse-sheet)
     T)
   ;1; use the first user id if the second was left blank*
    (WHEN (EMPTY-PARM *DEST-USER*)
      (SETQ *DEST-USER* *USER-ID*)
      (SETQ *DEST-PASSWORD* *PASSWORD*)
      (SETQ *DEST-ACCOUNT* *ACCOUNT*))
    (VALIDATE-PARMS *THIRD-PARTY-XFER-MENU-LAYOUT*)
    (UNLESS *FTP-UNFAVORABLE*
      (FTP-THIRD-PARTY-FILE-XFER))
    ;1; display status (and return its value (:retry or nil) as the value of this function)*
    (DISPLAY-STATUS))) 

;1;;-------------------------------------------------------------------------------------- *
;1;; REMOTE-DELETE*
;1;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN REMOTE-DELETE ()
  (VALIDATE-PARMS *ONE-FILE-MENU-LAYOUT*)
  (UNLESS (OR *FTP-UNFAVORABLE*
      (TV::MOUSE-CONFIRM
       (FORMAT () " Preparing to delete ~A at host ~A "
	       (STRING-UPCASE *REMOTE-PATHNAME*) (STRING-UPCASE *REMOTE-HOST*))
       "Click inside box to confirm OR move outside to abort"
       )
      )
    (SETQ *FTP-STATUS-CODE* ())
    (SETQ *FTP-UNFAVORABLE* T)
    (SETQ *FTP-STATUS-MSG* "User aborted before delete"
))
  (UNLESS *FTP-UNFAVORABLE*
    (FTP-REMOTE-DELETE))
  ;1; if all went well,  clear synonym (that file no longer exists)*
  (UNLESS *FTP-UNFAVORABLE*
    (SETQ *REMOTE-PATHNAME* ""))
  ;1; display status (and return its value (:retry or nil) as the value of this function)*
  (DISPLAY-STATUS)) 


;1;;--------------------------------------------------------------------------------------*
;1;; REMOTE-VIEW-FILE*
;1;   returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*


(DEFUN REMOTE-VIEW-FILE (&AUX STREAM)
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *VIEW-FILE-MENU-LAYOUT* :LABEL "VIEW A REMOTE FILE"
				:MARGIN-CHOICES *CVV-CHOICE-LIST* :EXTRA-WIDTH 40
				:superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS *VIEW-FILE-MENU-LAYOUT*)
    (UNLESS *FTP-UNFAVORABLE*
      (SETQ *PRE-ALLOCATE-P* ())
      (XFER-SETUP))
    (UNLESS (OR *FTP-UNFAVORABLE* (FTP-CALL2 :OPEN *REMOTE-PATHNAME* :INPUT))
      (UNWIND-PROTECT (CONDITION-CASE
		       ()
		       (PROGN
			 (OPEN-DISPLAY-WINDOW (FORMAT () "Viewing ~A"
						      *REMOTE-PATHNAME*))
			 (SETQ STREAM (SEND *FTPI-CONTROL-CONNECTION* :DATA-CONNECTION))
			 (STREAM-COPY-UNTIL-EOF STREAM *FTP-DISPLAY-WINDOW*)
			 ;1x                   (LOOP with ch*
			 ;1x                       while (SETQ ch (SEND stream :tyi nil))*
			 ;1x                       do (SEND *Ftp-display-window* :tyo ch))*
)
		       (SYSTEM:ABORT (SETQ *FTP-DISPLAY-ABORTED* T)))
	(CLOSE-DISPLAY-WINDOW)
	(FTP-CALL2 :CLOSE :ABORT);1should be ok to always abort*
	;1; don't display status just for aborted display*
	(SETQ *FTP-UNFAVORABLE* ())))
    ;1; display status (and return its value (:retry or nil) as the value of this function)*
    (WHEN *FTP-UNFAVORABLE*
      (DISPLAY-STATUS)));1 end when*
) 


;1;;--------------------------------------------------------------------------------------*
;1;; REMOTE-DIR-OP*
;1;;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN REMOTE-DIR-OP (MSG)
  (VALIDATE-PARMS *DIR-OP-MENU-LAYOUT*)
  (UNLESS *FTP-UNFAVORABLE*
    (FTP-CALL *REMOTE-HOST* MSG (UNLESS (EMPTY-PARM *DIRECTORY-PATHNAME*)
				  *DIRECTORY-PATHNAME*)))
  (DISPLAY-STATUS)) 

;1;;--------------------------------------------------------------------------------------*
;1;; REMOTE-NAME-LIST*
;1;;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN REMOTE-NAME-LIST ()
  (VALIDATE-PARMS *DIR-LIST-MENU-LAYOUT*)
  (UNLESS *FTP-UNFAVORABLE*
    (FTP-REMOTE-DIR-LIST :NLIST))
  (IF *FTP-UNFAVORABLE*
   ;1; THEN display status (and return its value (:retry or nil) as the value of this function)*
    (DISPLAY-STATUS)
    ;1; ELSE display directory listing*
    ;1     (dir-display *ftp-information* :compressed)*
    (DIR-DISPLAY *FTP-INFORMATION* :EXPANDED);1don't pack anymore (these are full pathname strings)*
)) 

;1;;--------------------------------------------------------------------------------------*
;1;; REMOTE-DIR-LIST*
;1;;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN REMOTE-DIR-LIST ()
  (VALIDATE-PARMS *DIR-LIST-MENU-LAYOUT*)
  (UNLESS *FTP-UNFAVORABLE*
    (FTP-REMOTE-DIR-LIST :LIST))
  (IF *FTP-UNFAVORABLE*
   ;1; THEN display status (and return its value (:retry or nil) as the value of this function)*
    (DISPLAY-STATUS)
    ;1; ELSE display directory listing*
    (DIR-DISPLAY *FTP-INFORMATION* :EXPANDED))) 

;1;;--------------------------------------------------------------------------------------*
;1;; DIR-DISPLAY*
;1;;   display-list is a list of strings *
;1;;   display format is :compressed or :expanded*
;1;;--------------------------------------------------------------------------------------*

(DEFUN DIR-DISPLAY (DISPLAY-LIST DISPLAY-FORMAT)
  (CONDITION-CASE ()
     (PROGN
       (OPEN-DISPLAY-WINDOW (STRING-APPEND *REMOTE-HOST* ":" *DIRECTORY-PATHNAME*))
       (IF (EQUAL DISPLAY-FORMAT :EXPANDED)
	;1; THEN output one string per line*
	 (MAPCAR '(GLOBAL:LAMBDA (S)
		    (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT S))
		 DISPLAY-LIST)
	 ;1; ELSE pack multiple strings per line *
	 (DIR-PACK DISPLAY-LIST)))
     (SYSTEM:ABORT (SETQ *FTP-DISPLAY-ABORTED* T)))
  (CLOSE-DISPLAY-WINDOW)) 

;1;;--------------------------------------------------------------------------------------*
;1;; OPEN-DISPLAY-WINDOW*
;1;;--------------------------------------------------------------------------------------*

(DEFUN OPEN-DISPLAY-WINDOW (LABEL)
  (WHEN (NULL *FTP-DISPLAY-WINDOW*)
    (SETQ *FTP-DISPLAY-WINDOW* (MAKE-INSTANCE 'FTP-STATUS-WINDOW-FLAVOR :BLINKER-P ())))
  (SEND *FTP-DISPLAY-WINDOW* :SET-LABEL LABEL)
  (SEND *FTP-DISPLAY-WINDOW* :EXPOSE)
  (SEND *FTP-DISPLAY-WINDOW* :SELECT ())) 

;1;;--------------------------------------------------------------------------------------*
;1;; CLOSE-DISPLAY-WINDOW*
;1;;--------------------------------------------------------------------------------------*

(DEFUN CLOSE-DISPLAY-WINDOW ()
  (IF *FTP-DISPLAY-ABORTED*
    (SETQ *FTP-DISPLAY-ABORTED* ())
    ;1; else*
    (CONDITION-CASE ()
       (PROGN
	 (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT "")
	 (SEND *FTP-DISPLAY-WINDOW* :STRING-OUT *FTP-NOMORE-LEGEND*)
	 (SEND *FTP-DISPLAY-WINDOW* :ANY-TYI))
       (SYSTEM:ABORT)))
  (SEND *FTP-DISPLAY-WINDOW* :DESELECT)
  (SEND *FTP-DISPLAY-WINDOW* :DEEXPOSE)
  (SEND *FTP-DISPLAY-WINDOW* :CLEAR-INPUT)) 


;1;;--------------------------------------------------------------------------------------*
;1;; OPEN-WAIT-WINDOW*
;1;;--------------------------------------------------------------------------------------*

(DEFUN OPEN-WAIT-WINDOW (MSG &AUX EXTRA)
  (WHEN *FTP-STATS*
    (WHEN (NULL *FTP-WAIT-WINDOW*)
      (SETQ *FTP-WAIT-WINDOW*
	    (MAKE-INSTANCE 'FTP-STATUS-WINDOW-FLAVOR :EDGES '(100 100 340 190) :BLINKER-P ()
			   :MORE-P ())))
    (SEND *FTP-WAIT-WINDOW* :SET-LABEL "FTP WAIT"
       )
    ;1; For complete (quoted) commands, dont show arguments*
    (WHEN (STRINGP MSG)
      (SETQ MSG
	    (SUBSEQ MSG 0 (POSITION #\SPACE (THE STRING (STRING MSG)) :TEST #'CHAR-EQUAL))))
    (SELECTOR MSG EQUAL;1make this more understandable in some cases*
       (:STORE (SETQ MSG :SEND-FILE)) (:RETRIEVE (SETQ MSG :RECEIVE-FILE))
       ((:PASSIVE :PORT "STOR" "RETR" "APPE") (SETQ EXTRA MSG)
	(SETQ MSG "Remote setup: 
      "))
       (:GET-REPLY (SETQ MSG :REMOTE-DATA-TRANSFER))
       ((:FORMAT :MODE :STRUCTURE :TYPE :ALLOCATE) (SETQ EXTRA MSG)
	(SETQ MSG "Set file parameters:
      ")))
    (SEND *FTP-WAIT-WINDOW* :EXPOSE-NEAR '(:MOUSE))
    (SEND
     *FTP-WAIT-WINDOW*
     :LINE-OUT " "
     )
    (SEND *FTP-WAIT-WINDOW* :STRING-OUT "  "
       )
    (SEND
     *FTP-WAIT-WINDOW*
     : STRING-OUT
     MSG)
    (WHEN EXTRA
      (SEND *FTP-WAIT-WINDOW* :STRING-OUT EXTRA))
    (SEND *FTP-WAIT-WINDOW* :LINE-OUT " ..."
       ))) 

;1;;--------------------------------------------------------------------------------------*
;1;; CLOSE-WAIT-WINDOW*
;1;;   Don't wait for user input to clear*
;1;;--------------------------------------------------------------------------------------*

(DEFUN CLOSE-WAIT-WINDOW ()
  (WHEN (AND *FTP-WAIT-WINDOW* *FTP-STATS*)
    (SEND *FTP-WAIT-WINDOW* :DEEXPOSE)
    (SEND *FTP-WAIT-WINDOW* :CLEAR-INPUT))) 


;1;;--------------------------------------------------------------------------------------*
;1;; DIR-PACK*
;1;;   Packs a list of strings several per line.  If the strings are relatively short, cols will be aligned.  If not,*
;1;;   alignment will resume with the next new line.  *
;1;;     name-list is a list of strings *
;1;;     col, line-length used on subsequent recursive calls to pass state info*
;1;;--------------------------------------------------------------------------------------*

(DEFUN DIR-PACK (NAME-LIST &OPTIONAL (COL 1) (LINE-LENGTH 0))
  (IF (NULL NAME-LIST)
   ;1; THEN finish the last line*
    (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT "")
    ;1; ELSE output next entry*
    (LET ((COL-WIDTH 30)
	  (ENTRIES-PER-LINE 4))
      (UNLESS (ZEROP LINE-LENGTH)
	(IF (> (SETQ COL (1+ COL)) ENTRIES-PER-LINE)
	 ;1; THEN move to a new line*
	  (PROGN
	    (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT "")
	    (SETQ COL 1)
	    (SETQ LINE-LENGTH 0))
	  ;1; ELSE tab to next col position (if not already past it)*
	  (LET ((TAB-VAL (- (* (- COL 1) COL-WIDTH) LINE-LENGTH))
		(DEFAULT-TAB 5))
	    (WHEN (NOT (> TAB-VAL 0))
	      (SETQ TAB-VAL DEFAULT-TAB))
	    (SEND *FTP-DISPLAY-WINDOW* :INCREMENT-CURSORPOS TAB-VAL 0 :CHARACTER)
	    (SETQ LINE-LENGTH (+ LINE-LENGTH TAB-VAL)))));1 end let, if, unless*
      (SEND *FTP-DISPLAY-WINDOW* :STRING-OUT (CAR NAME-LIST))
      (DIR-PACK (CDR NAME-LIST) COL (+ LINE-LENGTH (LENGTH (CAR NAME-LIST))))));1 end let, if*
) 

;1;;--------------------------------------------------------------------------------------*
;1;; VALIDATE-PARMS*
;1;;   This function takes a cvv menu list and ensures that required parameters (indicated by **) are present.*
;1;    It sets global variables to indicate status and a list of the missing fields.*
;1;;--------------------------------------------------------------------------------------*

(DEFUN VALIDATE-PARMS (MENU-LIST)
  (LET ((ERROR-LIST ""))
	;1; generate the list of missing variables *
    (DOLIST (MITEM MENU-LIST)
      (SETQ ERROR-LIST (STRING-APPEND ERROR-LIST (CHECK-ITEM MITEM))))
    (MULTIPLE-VALUE-SETQ (*FTP-STATUS-CODE* *FTP-STATUS-MSG*)
      (IF (STRING-EQUAL ERROR-LIST "")
       ;1;THEN*
	(VALUES T "")
	;1;ELSE*
	(VALUES *MENU-ERROR* (FORMAT () "These required fields are missing:~%~A" ERROR-LIST)))));1 end let*
  (SETQ *FTP-UNFAVORABLE* (FS::FTP-UNFAVORABLE *FTP-STATUS-CODE*))) 


;1;;--------------------------------------------------------------------------------------*
;1;; EMPTY-PARM*
;1;;--------------------------------------------------------------------------------------*

(DEFUN EMPTY-PARM (P)
  (OR (NULL P) (AND (NUMBERP P) (ZEROP P))
     (AND (STRINGP P) (OR (STRING-EQUAL P ""
					) (STRING-EQUAL P *NULL-WINDOW-VALUE*))))) 

;1;;--------------------------------------------------------------------------------------*
;1;; CHECK-ITEM*
;1;;    checks an item on a cvv list and returns its name if it is a missing required parameter *
;1;;--------------------------------------------------------------------------------------*

(DEFUN CHECK-ITEM (MENU-ITEM)
  (COND
   ;1; subitem is not a list (assume a decorative string) or is not a required parameter*
   ((OR (NOT (CONSP MENU-ITEM))
       (NOT (STRING-EQUAL *REQUIRED-MARK* (SUBSEQ (SECOND MENU-ITEM) 0 2))))
    "")
   ;1; required parm is empty - return its name (minus the leading indicator)*
   ((EMPTY-PARM
	(EVAL (FIRST MENU-ITEM)));1use EVAL to get the variable value instead of its name*
    (FORMAT () "  ~A~%" (SUBSEQ (SECOND MENU-ITEM) 3)))
   ;1; required parm is ok *
   (T "")))  

;1;;--------------------------------------------------------------------------------------*
;1;; DISPLAY-STATUS*
;1;;   This function displays a status message corresponding to global status variables*
;1;;   returns a value (:retry or nil) which indicates what the next action should be.  The*
;1;;   global variable *global-ftp-exit* may also be set to indicate a top-level abort.*
;1;;--------------------------------------------------------------------------------------*
(DEFUN DISPLAY-STATUS (&OPTIONAL (EXPERT-MODE NIL) &AUX CHOICE)
 ;1; 8/29/85 - modified to always allow choice of returning to previous menu*
  (LET* ((CHOICE-ACTION '(NIL CHOICE-BOX-HANDLER NIL NIL))
	 (RETRY-CHOICE "To previous menu")
	 (TOP-CHOICE "To top-level menu")
	 (ABORT-CHOICE "Exit FTP")
	 (CHOICE-LIST
	  (LIST (CONS RETRY-CHOICE CHOICE-ACTION)
		(CONS TOP-CHOICE CHOICE-ACTION)
		(CONS ABORT-CHOICE CHOICE-ACTION))))
    (WHEN (NULL *FTP-STATUS-WINDOW*)
      (SETQ *FTP-STATUS-WINDOW*
	    (MAKE-INSTANCE 'FTP-STATUS-WINDOW-FLAVOR
			   :EDGES `(100
				     100
				     ,(min 800 (send tv:selected-window :width))  ;102-25-88 DAB*
				     300)
			   :BLINKER-P ()
			   :LABEL "FTP STATUS"
			   :MARGIN-CHOICES ())))
    ;1; want margin choices to be dynamic (based on status code) *
    (SEND *FTP-STATUS-WINDOW* :SET-MARGIN-CHOICES CHOICE-LIST)
    ;1; display status information*
    (SEND *FTP-STATUS-WINDOW* :EXPOSE-NEAR '(:MOUSE))
    (CONDITION-CASE ()
       (PROGN;1catch abort here*
	(SEND *FTP-STATUS-WINDOW* :SELECT ())
	(SEND *FTP-STATUS-WINDOW* :LINE-OUT "")
	(WHEN EXPERT-MODE;1 include last command executed in this mode*
	  (SEND *FTP-STATUS-WINDOW* :STRING-OUT-CENTERED
	     (FORMAT () "Last command: ~A" *FTP-LAST-COMMAND*))
	  (SEND *FTP-STATUS-WINDOW* :LINE-OUT "")
	  (SEND *FTP-STATUS-WINDOW* :LINE-OUT ""))
	(SEND *FTP-STATUS-WINDOW* :STRING-OUT-CENTERED
	   (FORMAT () "Status code: ~A" *FTP-STATUS-CODE*))
	(SEND *FTP-STATUS-WINDOW* :LINE-OUT "")
	(SEND *FTP-STATUS-WINDOW* :LINE-OUT "")
	(CENTER-BLOCK *FTP-STATUS-MSG*)
	(WHEN *FTP-UNFAVORABLE*
	  (SEND *FTP-STATUS-WINDOW* :SET-REVERSE-VIDEO-P T)
	  (PROCESS-SLEEP 30 "ERROR"
			 );1wait half a sec*
	  (SEND *FTP-STATUS-WINDOW* :SET-REVERSE-VIDEO-P ()))
	(LOOP UNTIL (CONSP (SETQ CHOICE (SEND *FTP-STATUS-WINDOW* :ANY-TYI)))
	      FINALLY (RETURN (SETQ CHOICE (CADDR CHOICE))));1choice-box-name*
	)
       (SYSTEM:ABORT (SETQ CHOICE ())));1cause to leave program*
    (SEND *FTP-STATUS-WINDOW* :SET-REVERSE-VIDEO-P ())
    (SEND *FTP-STATUS-WINDOW* :DESELECT)
    (SEND *FTP-STATUS-WINDOW* :DEEXPOSE)
    ;1; *** SEEMS TO PREVENT BLOWUP IN CVV WHEN SUBSEQUENTLY RETRYING PREVIOUS OPERATION ****
    (SEND *FTP-STATUS-WINDOW* :CLEAR-INPUT)
    ;1; wait to do select to return result as result of this function*
    (SELECTOR CHOICE STRING-EQUAL
      (RETRY-CHOICE :RETRY)
      (TOP-CHOICE NIL)
      (OTHERWISE (PROGN
		   (SETQ *GLOBAL-FTP-EXIT* T)
		   ())))			;1end select*
))
  ;1 end let, defun*

;1;;--------------------------------------------------------------------------------------*
;1;; CENTER-BLOCK*
;1;;  Takes a string (possibly containing newlines) and centers each line (unless wider than window in which*
;1;   case it does a simple string-out since wraparound doesn't work with centering).*
;1;;  Note - :string-out-centered doesn't seem to work as advertised for multi-line strings.*
;1;;--------------------------------------------------------------------------------------*

(DEFUN CENTER-BLOCK (S)
  (LET ((SLEN (LENGTH S))
	(IX -1)
	STARTIX)
    (LOOP (SETQ STARTIX (1+ IX))
       (UNLESS (SETQ IX
	      (SEARCH (THE STRING (STRING #\NEWLINE)) (THE STRING (STRING S)) :START2 STARTIX
		      :TEST #'CHAR-EQUAL))
	;1; no more newlines - center any trailing text & leave*
	 (WHEN (< STARTIX SLEN)
	   (CENTER-STRING (SUBSEQ S STARTIX SLEN)))
	 (RETURN ()))
       ;1; center this line of the string (don't include return - screws up centering) *
       (CENTER-STRING (SUBSEQ S STARTIX IX))));1 end do,let*
) 

;1;;--------------------------------------------------------------------------------------*
;1;; CENTER-STRING*
;1;;--------------------------------------------------------------------------------------*

(DEFUN CENTER-STRING (S)
  (IF (> (LENGTH S) *STATUS-WINDOW-APPROXIMATE-WIDTH*)
   ;1; THEN too long - don't center*
    (SEND *FTP-STATUS-WINDOW* :STRING-OUT S)
    ;1; ELSE center it*
    (SEND *FTP-STATUS-WINDOW* :STRING-OUT-CENTERED S))
  (SEND *FTP-STATUS-WINDOW* :LINE-OUT "")) 
     
;1;;---------------------------------------------------------------------------------------*
;1;; CHOICE-BOX-HANDLER*
;1;;   This function is called by the window system when a margin choice box is selected*
;1;;    for a status window.  It forces a mouse blip into the input stream which is read by*
;1;;    the any-tyi method in the routine which is handling the status window.*
;1;     A choice-box-descriptor is a list, the first element of which is the name of the choice box.*
;1;;---------------------------------------------------------------------------------------*

(DEFUN CHOICE-BOX-HANDLER (CHOICE-BOX-DESCR IGNORE IGNORE)
  (DECLARE (:SELF-FLAVOR FTP-STATUS-WINDOW-FLAVOR))
  (SEND SELF :FORCE-KBD-INPUT (LIST :CHOICE-BOX SELF (CAR CHOICE-BOX-DESCR)))) 

;1;;--------------------------------------------------------------------------------------*
;1;; EXPERT-MENU*
;1;;    returns :retry or nil for higher-level menu*
;1;;--------------------------------------------------------------------------------------*

(DEFUN EXPERT-MENU (&AUX (EXPERT-FLAG T))
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *EXPERT-MENU-LAYOUT* :LABEL "FTP EXPERT MENU"
				:MARGIN-CHOICES *CVV-CHOICE-LIST* :EXTRA-WIDTH 80
				:superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS *EXPERT-MENU-LAYOUT*)
    (IF *FTP-UNFAVORABLE*
     ;1; then*
      (SETQ EXPERT-FLAG ());1 don't display extra info in status window*
      ;1; else*
      (PROGN
	(SETQ *FTP-OVERRIDE-LOGIN* T);1don't login now *
	
	(FTP-CALL *REMOTE-HOST* :QUOTE *FTP-EXPERT-COMMAND*)
	(SETQ *FTP-OVERRIDE-LOGIN* ())
	;1; DON'T DO THIS*
	;1x                (SETQ *Ftp-user-logged-in* nil)*	1   ; force a later login in case expert changes user id*
))
    ;1; for expert mode, display status no matter what*
    (DISPLAY-STATUS EXPERT-FLAG);1returns :retry or nil for higher-level menu *
)) 

;1;;--------------------------------------------------------------------------------------*
;1;; DISPLAY-INTRO-HELP*
;1;;--------------------------------------------------------------------------------------*

(DEFUN DISPLAY-INTRO-HELP ()
  (CONDITION-CASE ()
     (PROGN
       (OPEN-DISPLAY-WINDOW "INTRODUCTION TO FTP")
       (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT "")
       (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT
	  "FTP (File Transfer Protocol) is a higher-level protocol based on the TCP/IP 
  protocol suite.  In addition to file transfer capabilities, it provides directory
  display functions and file manipulation functions like rename and delete.  It is
  employed automatically and invisibly where appropriate when using standard Lisp
  machine functions like copy-file and dired.  If desired, though, this menu interface
  may be used to explicitly invoke FTP operations."))
     (SYSTEM:ABORT (SETQ *FTP-DISPLAY-ABORTED* T)))
  (CLOSE-DISPLAY-WINDOW)) 

;1;;--------------------------------------------------------------------------------------*
;1;; DISPLAY-EXPERT-HELP*
;1;;--------------------------------------------------------------------------------------*

(DEFUN DISPLAY-EXPERT-HELP ()
  (CONDITION-CASE ()
     (PROGN
       (OPEN-DISPLAY-WINDOW "EXPLICIT FTP COMMANDS (EXPERT MODE)")
       (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT "")
       (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT
	  "The following are FTP server commands which may be explicitly entered in expert mode.
They are sent directly to the server at the host indicated.  The actual command names
are shown in parentheses and arguments follow.  For additional information, see specification
numbers RFC765 or IEN149 of the Defense Advanced Research Projects Agency (DARPA) or other
FTP documentation.  Note that these commands require the use of the control connection only.

>> COMMANDS WHICH USE OR AFFECT A DATA CONNECTION CANNOT BE SAFELY ENTERED IN THIS MODE.

    USER NAME                 (USER)   user-id 
    PASSWORD                  (PASS)   password
    ACCOUNT                   (ACCT)   account
    REINITIALIZE              (REIN)			   
    LOGOUT                    (QUIT)

    REPRESENTATION TYPE       (TYPE)   A|E|I|L {N|T|C}
    FILE STRUCTURE            (STRU)   F|R|P
    TRANSFER MODE             (MODE)   S|B|C
    
    ALLOCATE                  (ALLO)   s {R m} 
    RENAME FROM               (RNFR)   pathname
    RENAME TO                 (RNTO)   pathname
    ABORT                     (ABOR)   
    DELETE                    (DELE)   pathname
    CHANGE WORKING DIRECTORY  (CWD)    pathname
    SITE PARAMETERS           (SITE)   string
    STATUS                    (STAT)   {file name}
    HELP                      (HELP)   {command-name} 
    NOOP                      (NOOP)"))
     (SYSTEM:ABORT (SETQ *FTP-DISPLAY-ABORTED* T)))
  (CLOSE-DISPLAY-WINDOW)) 

;1;; =====================================================================================*
;1;; These are the routines which actually perform the ftp calls.*
;1;; =====================================================================================*

;1;;--------------------------------------------------------------------------------------*
;1;; FTP-REMOTE-RENAME*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-REMOTE-RENAME ()
  (FTP-CALL *REMOTE-HOST* :RENAME *REMOTE-PATHNAME* *NEW-PATHNAME*)) 

;1;;--------------------------------------------------------------------------------------*
;1;; FTP-REMOTE-DELETE*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-REMOTE-DELETE ()
  (FTP-CALL *REMOTE-HOST* :DELETE *REMOTE-PATHNAME*)) 

;1;;--------------------------------------------------------------------------------------*
;1;; FTP-REMOTE-DIR-LIST*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-REMOTE-DIR-LIST (MSG)
  ;1; workaround Apollo bug (don't send :non-print for :ascii, it is the default)*
  (UNLESS (OR (FTP-CALL *REMOTE-HOST* :TYPE :ASCII)
	      (FTP-CALL2 :OPEN *DIRECTORY-PATHNAME* MSG))
    (UNWIND-PROTECT (CONDITION-CASE ()
		       (PROGN
			 (OPEN-WAIT-WINDOW :DATA-TRANSFER)
			 (SETQ *FTP-INFORMATION*
			       (FS::CONSTRUCT-DIR-LIST *FTPI-CONTROL-CONNECTION*))
			 (CLOSE-WAIT-WINDOW)
			 (OPEN-WAIT-WINDOW : CLOSE
					   ))
		       (SYSTEM:ABORT (USER-ABORT-FTP)))
		     ;1; Dont wipe out ftp-information by calling ftp-call*
      (SEND *FTPI-CONTROL-CONNECTION* :CLOSE :ABORT);1should be ok to always say abort*
      (CLOSE-WAIT-WINDOW)))) 


;1;;--------------------------------------------------------------------------------------*
;1;; FTP-GET-REMOTE-FILE*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-GET-REMOTE-FILE ()
  (XFER-SETUP)
  (UNLESS *FTP-UNFAVORABLE*
    (IF (EQUAL *REPLACE-APPEND-OPTION* :APPEND)
      (FTP-CALL2 :RETRIEVE *REMOTE-PATHNAME* *LOCAL-PATHNAME* :APPEND)
      (FTP-CALL2 :RETRIEVE *REMOTE-PATHNAME* *LOCAL-PATHNAME*)))) 

;1;;--------------------------------------------------------------------------------------*
;1;; FTP-SEND-REMOTE-FILE*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-SEND-REMOTE-FILE ()
  (XFER-SETUP)
  (UNLESS *FTP-UNFAVORABLE*
    (IF (EQUAL *REPLACE-APPEND-OPTION* :APPEND)
      (FTP-CALL2 :STORE *LOCAL-PATHNAME* *REMOTE-PATHNAME* :APPEND)
      (FTP-CALL2 :STORE *LOCAL-PATHNAME* *REMOTE-PATHNAME*)))) 

;1;;--------------------------------------------------------------------------------------*
;1;; XFER-SETUP*
;1;;--------------------------------------------------------------------------------------*

(DEFUN XFER-SETUP (&OPTIONAL (CTL-CONN *FTPI-CONTROL-CONNECTION*) (LOGIN T))
  (OR (WHEN LOGIN
	(CHECK-LOGIN *REMOTE-HOST*))
      ;1; workaround Symbolics bug (don't send "mode s", it is the default)*
      (UNLESS (EQ *TRANSFER-MODE* :STREAM) (FTP-CALL3 CTL-CONN :MODE *TRANSFER-MODE*))
       ;1; workaround Symbolics bug (don't send "STRU F", it is the default)*
      (UNLESS (EQ *FILE-STRUCTURE* :FILE) (FTP-CALL3 CTL-CONN :STRUCTURE *FILE-STRUCTURE*))
      (SELECT *REP-TYPE*
	(:BYTE-SIZE (FTP-CALL3 CTL-CONN :TYPE :BYTE-SIZE *LOCAL-BYTE-SIZE*))
	;1; workaround Apollo bug (don't send :non-print for :ascii, it is the default)*
	(:ASCII (FTP-CALL3 CTL-CONN :TYPE *REP-TYPE* (UNLESS (EQ :NON-PRINT *FILE-FORMAT*) *FILE-FORMAT*)))
	(:EBCDIC (FTP-CALL3 CTL-CONN :TYPE *REP-TYPE* *FILE-FORMAT*))
	(T (FTP-CALL3 CTL-CONN :TYPE *REP-TYPE*)))
      (WHEN *PRE-ALLOCATE-P*
	(FTP-CALL3 CTL-CONN :ALLOCATE *ALLOCATION-SIZE* *ALLOCATION-RECORD-SIZE*)))
  *FTP-UNFAVORABLE*) 

;1;;--------------------------------------------------------------------------------------*
;1;; FTP-THIRD-PARTY-XFER*
;1;;   Set up a data transfer between two remote hosts (server A uses the permanent FTPI control connection)*
;1;;*
;1;;        SERVER A (SRC)                       SERVER B (DEST)*
;1;;        -------------                      --------------*
;1;;                                               get-connection*
;1;;                                               login *
;1;;                                               file parameter setup*
;1;;        file parameter setup (auto connect)*
;1;;                                               passive => address and port #*
;1;;        port (+ passive address & port)*
;1;;        retrieve*
;1;;                                               store / append*
;1;;                                               get reply*
;1;;        get reply*
;1;;                                               quit*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-THIRD-PARTY-FILE-XFER (&AUX DEST-CONN REAL-HOST SAVE-CODE SAVE-MSG)
  (SETQ DEST-CONN (MAKE-INSTANCE 'FTP-CONTROL-CONNECTION))
  (UNLESS (OR (PROGN
	 (SETQ REAL-HOST (VALIDATE-HOST *DEST-HOST*))
	 *FTP-UNFAVORABLE*)
      (FTP-CALL3 DEST-CONN :GET-CONNECTION REAL-HOST))
    (UNWIND-PROTECT (OR
		     (XFER-SETUP);1connect, login, & setup for source host*
		     (FTP-CALL3 DEST-CONN :LOGIN *DEST-USER*
				(IF (EMPTY-PARM *DEST-PASSWORD*)
				  ""
				  *DEST-PASSWORD*)
				(UNLESS (EMPTY-PARM *DEST-ACCOUNT*)
				  *DEST-ACCOUNT*))
		     (XFER-SETUP DEST-CONN ());1setup only*
		     (FTP-CALL3 DEST-CONN :PASSIVE)
		     (FTP-CALL2 :PORT (THIRD *FTP-INFORMATION*) (FOURTH *FTP-INFORMATION*))
		     ;1; Use quote format so ftp will return to us without waiting for reply, then get our own replies*
		     (FTP-CALL2 :QUOTE (FORMAT () "RETR ~A"
					       *REMOTE-PATHNAME*))
		     (IF (IF (EQUAL *REPLACE-APPEND-OPTION* :APPEND)
			(FTP-CALL3 DEST-CONN :QUOTE (FORMAT () "APPE ~A"
							    *DEST-PATHNAME*))
			(FTP-CALL3 DEST-CONN :QUOTE (FORMAT ()
							    "STOR ~A"
							    *DEST-PATHNAME*)))
		      ;1; then unfavorable*
		       (PROGN
			;1; save the real message we wan't to display *
			(SETQ SAVE-CODE *FTP-STATUS-CODE*) (SETQ SAVE-MSG *FTP-STATUS-MSG*)
			(FTP-CALL2 :GET-REPLY) (SETQ *FTP-STATUS-CODE* SAVE-CODE)
			(SETQ *FTP-STATUS-MSG* SAVE-MSG))
		       ;1; else *
		       (PROGN
			 (FTP-CALL3 DEST-CONN :GET-REPLY)
			 (FTP-CALL2 :GET-REPLY))))
      (SETQ SAVE-CODE *FTP-STATUS-CODE*)
      (SETQ SAVE-MSG *FTP-STATUS-MSG*)
      ;1; rla 10/31/85 - don't need to do the final ports any more (FTP always sends a port now before opening up a new data conn)*
      ;1x     (ftp-call2 :port)*				1;reset things (ignore errors)*
      ;1x     (ftp-call3 dest-conn :port)*			1;reset things (ignore errors)*
      (FTP-CALL3 DEST-CONN :QUIT)
      (SETQ *FTP-STATUS-CODE* SAVE-CODE)
      (SETQ *FTP-STATUS-MSG* SAVE-MSG)))) 

;1;;-------------------------------------------------------------------------------------- *
;1;; FTP-CALL*
;1;;   checks to see if login is necessary before calling ftp*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-CALL (HOST MSG &REST ARGS)
  (CHECK-LOGIN HOST)
  (UNLESS *FTP-UNFAVORABLE*
    (APPLY #'FTP-CALL2 MSG ARGS))
  *FTP-UNFAVORABLE*)   


;1;;-------------------------------------------------------------------------------------- *
;1;; CHECK-LOGIN*
;1;;-------------------------------------------------------------------------------------- *

(DEFUN CHECK-LOGIN (HOST &AUX CC REAL-HOST)
 ;1; Get a new connection if we're changing hosts or login information *
  (UNLESS (AND (SETQ CC (SEND *FTPI-CONTROL-CONNECTION* :CONTROL-CONNECTION))
      (SETQ CC (EQ (SEND CC :STATUS) :ESTABLISHED)) (STRING-EQUAL HOST *FTP-CONNECTED*)
      (OR *FTP-OVERRIDE-LOGIN*
	 (AND *FTP-USER-LOGGED-IN* (STRING-EQUAL *USER-ID* *OLD-USER-ID*)
	    (STRING-EQUAL *PASSWORD* *OLD-PASSWORD*) (STRING-EQUAL *ACCOUNT* *OLD-ACCOUNT*))))
    (WHEN (AND *FTP-CONNECTED* CC)
      (FTP-CALL2 :QUIT))
    (SETQ *FTP-CONNECTED* ())
    (SETQ *FTP-USER-LOGGED-IN* ());1    (SETQ *directory-pathname* "")*		1can't do this (may have been a new directory list)*
    ;1; go ahead and validate host so we don't die*
    ;1; allow for LM case *
    (SETQ REAL-HOST (VALIDATE-HOST HOST))
    (UNLESS (OR *FTP-UNFAVORABLE* (FTP-CALL2 :GET-CONNECTION REAL-HOST))
      (SETQ *FTP-CONNECTED* HOST)
      (UNLESS (OR *FTP-OVERRIDE-LOGIN*
	  (FTP-CALL2 :LOGIN *USER-ID* (IF (EMPTY-PARM *PASSWORD*)
					""
					*PASSWORD*)
		     (UNLESS (EMPTY-PARM *ACCOUNT*)
		       *ACCOUNT*)))
	(SETQ *OLD-USER-ID* *USER-ID*)
	(SETQ *OLD-PASSWORD* *PASSWORD*)
	(SETQ *OLD-ACCOUNT* *ACCOUNT*)
	(SETQ *FTP-USER-LOGGED-IN* T))))
  *FTP-UNFAVORABLE*) 

;1;;--------------------------------------------------------------------------------------*
;1;; VALIDATE-HOST*
;1;;--------------------------------------------------------------------------------------*

(DEFUN VALIDATE-HOST (HOST &AUX HOSTOBJ)
  (IF (AND (STRINGP HOST) (STRING-EQUAL HOST "LM"))
    (PROGN (SETQ HOSTOBJ SI:LOCAL-HOST))
    (PROGN (SETQ HOSTOBJ (IGNORE-ERRORS (PARSE-IP-HOST-SPEC HOST :DONT-CREATE)))))
  (IF HOSTOBJ
    (PROGN
     (UNLESS (SETQ *FTP-STATUS-CODE* (SEND HOSTOBJ :OPERATION-HANDLED-P :IP-ADDRESSES))
       (SETQ *FTP-STATUS-MSG* "Host is not a TCP/IP host (IP address is unknown)")))
    (PROGN (SETQ *FTP-STATUS-CODE* ()) (SETQ *FTP-STATUS-MSG* "Undefined host")))
  (UNLESS (SETQ *FTP-UNFAVORABLE* (NOT *FTP-STATUS-CODE*))
    (SEND HOSTOBJ :NAME))) 
 
;1;;-------------------------------------------------------------------------------------- *
;1;; FTP-CALL2*
;1;;   calls FTP-CALL3 with standard control connection *
;1;;   (this is to make putting in third-party transfer stuff easier)*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-CALL2 (MSG &REST ARGS)
  (APPLY #'FTP-CALL3 *FTPI-CONTROL-CONNECTION* MSG ARGS)) 


;1;;-------------------------------------------------------------------------------------- *
;1;; FTP-CALL3*
;1;;   calls ftp and sets global variables according to returned parameters.*
;1;;--------------------------------------------------------------------------------------*

(DEFUN FTP-CALL3 (CTL-CONN MSG &REST ARGS &AUX)
  (IF (EQ MSG :QUOTE)
    (SETQ *FTP-LAST-COMMAND* (CAR ARGS))
    (SETQ *FTP-LAST-COMMAND* MSG))
  (WHEN *FTP-DEBUG*
    (PRINT MSG)
    (PRINT ARGS))
  (OPEN-WAIT-WINDOW *FTP-LAST-COMMAND*)
  (CONDITION-CASE (COND-OBJ)
     (PROGN
       (SETQ *FTP-INFORMATION* ());1in case we error off before next setq is completed*
       (SETQ *FTP-INFORMATION* (MULTIPLE-VALUE-LIST (APPLY CTL-CONN MSG ARGS)))
       (SETQ *FTP-STATUS-CODE* (CAR *FTP-INFORMATION*))
       (SETQ *FTP-STATUS-MSG* (SECOND *FTP-INFORMATION*)))
     (OPEN-FAILED (SETQ *FTP-STATUS-CODE* ())
      (SETQ *FTP-STATUS-MSG* (SEND COND-OBJ :FORMAT-STRING)))
     (SYSTEM:ABORT (USER-ABORT-FTP)))
  (CLOSE-WAIT-WINDOW)
  (WHEN *FTP-DEBUG*
    (PRINT *FTP-INFORMATION*))
  (SETQ *FTP-UNFAVORABLE* (FS::FTP-UNFAVORABLE *FTP-STATUS-CODE*));1return value !!*
) 
  

;1;;-------------------------------------------------------------------------------------- *
;1;; USER-ABORT-FTP*
;1;;--------------------------------------------------------------------------------------*

(DEFUN USER-ABORT-FTP ()
  (SETQ *FTP-STATUS-CODE* ())
  (SETQ *FTP-STATUS-MSG* "USER ABORT"
)
  (SETQ *FTP-UNFAVORABLE* T)) 


;1;;-------------------------------------------------------------------------------------- *
;1;; FTPI-UNIMPLEMENTED*
;1;;-------------------------------------------------------------------------------------- *

(DEFUN FTPI-UNIMPLEMENTED ()
  (SETQ *FTP-UNFAVORABLE* T)
  (SETQ *FTP-STATUS-CODE* *MENU-ERROR*)
  (SETQ *FTP-STATUS-MSG* "This command has not been implemented in the FTP window interface")
  (SETQ *FTP-LAST-COMMAND* ())
  (SETQ *FTP-INFORMATION* "")) 


;1;;-------------------------------------------------------------------------------------- *
;1;; HOST-LIST*
;1;;--------------------------------------------------------------------------------------*
;1; CHANGED TO USE NEW REL3 HOST-LIST -- HOST:*HOST-LIST* -- LS 10/24/86*
(DEFUN HOST-LIST ()
  (OPEN-DISPLAY-WINDOW "TCP/IP HOSTS            (SYSTEM TYPE, ALIASES, IP ADDRESS FORMS)"
		       )
  (CONDITION-CASE ()
      (DOLIST (OBJ HOST:*HOST-LIST*)
	      (WHEN (SEND OBJ :SEND-IF-HANDLES :NETWORK-ADDRESS-LIST :IP))
	      (HOST-DISPLAY OBJ))
    (SYSTEM:ABORT (SETQ *FTP-DISPLAY-ABORTED* T)))
  (CLOSE-DISPLAY-WINDOW)
  ()						;1no retry*
  ) 

;1;;-------------------------------------------------------------------------------------- *
;1;; HOST-DISPLAY*
;1;;-------------------------------------------------------------------------------------- *

(DEFUN HOST-DISPLAY (ENTRY &AUX ADDR-LIST (NAME-MAX 24) (STYPE-MAX 38) NAME)
  (WHEN (AND ENTRY (SETQ ADDR-LIST (SEND ENTRY :SEND-IF-HANDLES :IP-ADDRESSES)))
   ;1; name*
    (SEND *FTP-DISPLAY-WINDOW* :STRING-OUT (SETQ NAME (SEND ENTRY :NAME)))
    ;1; system type*
    
    (SEND
     *FTP-DISPLAY-WINDOW* : SET-CURSORPOS NAME-MAX NIL :CHARACTER)
    (SEND *FTP-DISPLAY-WINDOW* :STRING-OUT (SEND ENTRY :SYSTEM-TYPE))
    ;1; aliases*
    (SEND *FTP-DISPLAY-WINDOW* :SET-CURSORPOS STYPE-MAX () :CHARACTER)
    (DOLIST (ALIAS (SEND ENTRY :HOST-NAMES))
      (UNLESS (STRING-EQUAL ALIAS NAME)
	(SEND *FTP-DISPLAY-WINDOW* :STRING-OUT ALIAS)
	(SEND *FTP-DISPLAY-WINDOW* :STRING-OUT "  "
	   )))
    (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT " "
       )
    ;1; address forms*
    (DOLIST (ADDR ADDR-LIST)
      (SEND *FTP-DISPLAY-WINDOW* :STRING-OUT " "
	 )
      (SEND *FTP-DISPLAY-WINDOW* :SET-CURSORPOS NAME-MAX () :CHARACTER)
      (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT (IP-ADDRESS-DISPLAY ADDR)))
    (SEND *FTP-DISPLAY-WINDOW* :LINE-OUT " "
       )
    T);1 => did display the host*
) 


;1;;-------------------------------------------------------------------------------------- *
;1;; HOST-DEFINE*
;1;;-------------------------------------------------------------------------------------- *

(DEFUN HOST-DEFINE (&AUX
  RESULT
  )
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *NEW-HOST-MENU-LAYOUT* :LABEL "DEFINE A TEMPORARY TCP/IP HOST"
				:MARGIN-CHOICES *CVV-CHOICE-LIST* :EXTRA-WIDTH 20
				:superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS *NEW-HOST-MENU-LAYOUT*)
    (UNLESS *FTP-UNFAVORABLE*
     ;1; Probably should do better than this*
      (MULTIPLE-VALUE-SETQ (RESULT *FTP-UNFAVORABLE*)
	(IGNORE-ERRORS (CREATE-IP-HOST *NEW-HOST* *HOST-ADDRESS*)))
      (IF *FTP-UNFAVORABLE*
	(PROGN
	  (SETQ *FTP-STATUS-CODE* ())
	  (SETQ *FTP-STATUS-MSG*
		"Unable to create host (already exists or syntax error in address)"
))
	;1; else*
	(PROGN
	  (SETQ *REMOTE-HOST* *NEW-HOST*)
	  (SETQ *FTP-STATUS-CODE* T)
	  (SETQ *FTP-STATUS-MSG* (FORMAT () "Created ~A"
					 RESULT)))))
    (DISPLAY-STATUS))) 

;1;;-------------------------------------------------------------------------------------- *
;1;; HOST-DESCRIBE*
;1;;-------------------------------------------------------------------------------------- *

(DEFUN HOST-DESCRIBE (&AUX
  RESULT
  )
  (WHEN (EMPTY-PARM *DESCRIBE-HOST*)
    (SETQ *DESCRIBE-HOST* *REMOTE-HOST*))
  (WHEN (CATCH 'ABORT
     (TV:CHOOSE-VARIABLE-VALUES *DESCRIBE-HOST-MENU-LAYOUT* :LABEL
				"DESCRIBE A TCP/IP HOST"
				:MARGIN-CHOICES *CVV-CHOICE-LIST* :EXTRA-WIDTH 20
				:superior w:mouse-sheet)
     T)
    (VALIDATE-PARMS *DESCRIBE-HOST-MENU-LAYOUT*)
    (UNLESS *FTP-UNFAVORABLE*
      (IF (SETQ RESULT (IGNORE-ERRORS (PARSE-IP-HOST-SPEC *DESCRIBE-HOST* :DONT-CREATE)))
	(PROGN
	  (IF (SEND RESULT :OPERATION-HANDLED-P :IP-ADDRESSES)
	    (PROGN
	      (OPEN-DISPLAY-WINDOW
	       "TCP/IP HOST             (SYSTEM TYPE, ALIASES, IP ADDRESS FORMS)"
	       )
	      (HOST-DISPLAY RESULT)
	      (CLOSE-DISPLAY-WINDOW)
	      (SETQ *FTP-STATUS-CODE* T)
	      (SETQ *REMOTE-HOST*
		    (IF (MEMBER *DESCRIBE-HOST* (SEND RESULT :HOST-NAMES) :TEST #'STRING-EQUAL)
		      *DESCRIBE-HOST*
		      (SEND RESULT :NAME))))
	    ;1; else*
	    (PROGN
	      (SETQ *FTP-STATUS-CODE* ())
	      (SETQ *FTP-STATUS-MSG* "Host is not a TCP/IP host (IP address is unknown)"))))
	;1; else*
	(PROGN
	  (SETQ *FTP-STATUS-CODE* ())
	  (SETQ *FTP-STATUS-MSG* "Host does not exist")))
      (SETQ *FTP-UNFAVORABLE* (FS::FTP-UNFAVORABLE *FTP-STATUS-CODE*)))
    (IF *FTP-UNFAVORABLE*
      (DISPLAY-STATUS)
      ;1; ELSE no retry *
      ()))) 

