;;;  -*- Mode:COMMON-LISP; Package:SI; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-


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
;;;
;;;---------------------------------------------------------------------------------
;;;*	1     *	1          Chaosnet Band Transfer System
;;;---------------------------------------------------------------------------------
;;;
;;;
;;;      SI:RECEIVE-BAND -- Transfers a band from Server to User.
;;;      SI:TRANSMIT-BAND -- Transfers a band from User to Server.
;;;      SI:COMPARE-BAND -- Compares a User's band to the Server's Band
;;;      SI:BAND-TRANSFER-SERVER -- Server end for all band operations.
;;;
;;;      SI:BAND-TRANSFER-SERVER-ON -- Variable used to enable or disable
;;;         band serving.
;;;
;;;--------------------------------------------------------------------------------
;;;
;;; Change History:
;;;*
;;; 05-03-89 DAB     Fixed Band-transfer-server to make sure the unit return from decode-unit-argument
;;;                   is valid, otherwise return an error. 
;;; 12-13-88 DAB     Changed transmit and receive bands to check duplicate partition and display menu.
;;; ***************** Rel 5.0 12-12-88 DAB
;;; 11-10-88 BJ    - Change the window size on an MX to 12 so that we do not cause so many retransmissions.
;;; 07-11-88 DAB   - Change calls to tv:notify to fs:notify-nicely. This will not lock up the system when the
;;;                   user is not around to respond. [7533]
;;; 04-07-88 DAB   - Make sure the local unit is a valid unit number and not a closure. 
;;; 04-06-88 DAB   - Do not allow BAND-TRANSFER-SERVER to write over the booted load band.
;;; 21 NOV 86  AB  - Changes needed for 2K page-size now that PAGE-SIZE and
;;;                  a "block" are not the same (page-size is two disk sectors).
;;;                  All disk-io is still in terms of blocks, so just needed to
;;;                  change PAGE-SIZE to DISK-BLOCK-WORD-SIZE.
1;;;
;;; 03 MAR 86  MMG - Conversion to Common Lisp.  Added check in server for writing
;;;                  over booted band and removed Errset.  
;;; 18 DEC 85  MMG - Rewrote Compare-Band to use double-buffering.  Created common
;;;                  functions to be called for screen displays.
;;; 10 DEC 85  MMG - New paint job; replaced obsolete functions and expanded the
;;;                  documentation.
;;; 22 NOV 85  MMG - Overhauled the automatic transmission; creation of a common
;;;                  function called by all partition-net operations.
;;; 05 NOV 85  MMG - Adjusted the carburetor; incomplete packets are now saved and
;;;                  returned to ARRAY-FROM-NET for completion.
;;;                  double-buffering for increased speed.      *
;;; 03.13.87   DAB - Implementated new method of partition name parsing, "name.cpu".
1;;;
;;; Features to be added:
;;;
;;;    - ARRAY-FROM-NET and ARRAY-TO-NET should use %BLT instead of COPY-ARRAY-
;;;      PORTION for increased speed.
;;;    - The unwind-protect in the band transfer server should be wrapped with
;;;      a condition bind that returns errors to the user side.
;;;


;; Global Variables*


(DEFVAR 4BAND-TRANSFER-SERVER-ON* :NOTIFY
   2"NIL - don't allow bands to be read or written.
    T - do allow.
   * 2:NOTIFY - allow, but notify user."*) 



1;;;-------------------------------------------------------------------------------
;;;
;;;     Screen Display Functions.
;;;
;;;-------------------------------------------------------------------------------*


(DEFSUBST 4COMPARE-ARRAYS-AND-SHOW-DIFFERENCES*
	  (LOCAL-ARRAY FROM-ARRAY &OPTIONAL (LIMIT-MESSAGE-P NIL) (INITIAL-BLOCK 0))
  
  2"Compares two art-16b arrays and displays any differences.
   If only three error messages per block are desired, set
   Limit-Message-P to T.  Initial-Block is used to display which 
   block the arrays were taken from (when comparing multiple blocks
   of data)."*
  
  (COND
    ((LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
       (%STRING-EQUAL LOCAL-ARRAY 0 FROM-ARRAY 0 ()))
     T)
    
    1;; Display a message for each differing element.*
    
    (T
     (DO ((INDEX 0 (1+ INDEX))
	  (ERRORS 0)
	  (LIM (MIN (ARRAY-TOTAL-SIZE LOCAL-ARRAY) (ARRAY-TOTAL-SIZE FROM-ARRAY))))
	 ((OR (= INDEX LIM) (WHEN LIMIT-MESSAGE-P
			      (= ERRORS 3)))
	  (FORMAT T 3"~%"*))
       (UNLESS
	 (= (AREF LOCAL-ARRAY INDEX) (AREF FROM-ARRAY INDEX))
	 (FORMAT T 3"~%  Discrepancy at block ~3d, halfword ~3d: "*
		 (+ INITIAL-BLOCK (TRUNCATE (1+ INDEX) (* si:disk-block-word-size 2)))
		 (MOD INDEX (* si:disk-block-word-size 2)))
	 (FORMAT T 3"Value in Local-Part = ~6,'0o; From-Part = ~6,'0o (octal)"*
		 (AREF LOCAL-ARRAY INDEX) (AREF FROM-ARRAY INDEX))
	 (SETQ ERRORS (1+ ERRORS))))
     NIL)))		


(DEFSUBST 4DISPLAY-BLOCK* (BLOCK ORIG-PART-BASE N-HUNDRED)
  
  2"Displays the hundreds of blocks as transferred by the user functions, 
   then returns an updated hudreds-of-blocks count."*

  (LET ((TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100)))
    (UNLESS (= TEM N-HUNDRED)
      (WHEN (ZEROP (MOD N-HUNDRED 20))
	(FORMAT T 3"~% "*))
      (FORMAT T 3"~4d"* TEM))
    TEM)) 1; display-block*




1;;;---------------------------------------------------------------------------------------
;;;
;;;  User Functions -- Receive-Band, Transmit-Band, Compare-Band.
;;;
;;;---------------------------------------------------------------------------------------*

(defvar *mx-receive-band-window-size* 13)
(defvar *exp-receive-band-window-size* 36)


(DEFUN RECEIVE-BAND (FROM-MACHINE FROM-PART TO-UNIT TO-PART
				  &OPTIONAL (SUBSET-START 0) SUBSET-N-BLOCKS
				  &AUX CONN PKT STR REMOTE-PART-SIZE (WINDOW (if (si:mx-p)
										 *mx-receive-band-window-size*
										 *exp-receive-band-window-size* ))
				  FROM-UNIT PART-COMMENT PART-BASE PART-SIZE
				  user-type partition-name-string);03.13.87 DAB
  
  "Read the FROM-PART partition  from FROM-MACHINE into local partition TO-PART.
   If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part of the partition 
   to transfer.  SUBSET-START and SUBSET-N-BLOCKS are in hundreds of blocks.  If a 
   transfer dies, use the last number it printed  as SUBSET-START, to
   resume where it left off.  FROM-MACHINE is a string consisting of the name of the
   remote machine or the remote machine, colon, remote unit number.  If no unit number
   is specified the default unit of the REMOTE machine is used."
  (SETQ SUBSET-START (* SUBSET-START 100))
  (WHEN SUBSET-N-BLOCKS
	(SETQ SUBSET-N-BLOCKS (* SUBSET-N-BLOCKS 100)))
  (UNWIND-PROTECT
      (PROGN
	(SETQ TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT "receive-band"))
	(when (closurep to-unit)   ;03-02-88 DAB Check to make sure it is a local unit.
	  (ferror () "TO-UNIT must be a local unit number."))
	(MULTIPLE-VALUE-SETQ (PART-BASE PART-SIZE NIL to-part nil partition-name-string)  ;03.19.87 DAB      
			     (FIND-DISK-PARTITION-FOR-WRITE TO-PART () TO-UNIT))
	(MULTIPLE-VALUE-SETQ (nil nil nil nil nil from-part) ;12-13-88 DAB
			     (FIND-DISK-PARTITION-FOR-read from-PART () from-machine))
	;; Parse machine and unit number.
	
	(LET ((TEM (POSITION #\: (THE STRING (STRING FROM-MACHINE)) :TEST #'CHAR-EQUAL)))
	  (UNLESS (NULL TEM)
		  (SETQ FROM-UNIT (READ-FROM-STRING FROM-MACHINE () () :START (1+ TEM)))
		  (SETQ FROM-MACHINE (SUBSEQ (STRING FROM-MACHINE) 0 TEM) )))
	
	;; If the partition was found and the user ok'd writing to it . . .
        (setf (values from-part user-type) ;03.13.87 DAB
	      (parse-partition-name from-part :keyword))
	(WHEN PART-BASE	      
	      ;; Connect to remote machine.
	      (SETQ CONN
		    (CHAOS:CONNECT FROM-MACHINE
				   (FORMAT () "BAND-TRANSFER READ ~a ~d ~d ~a ~a ~a" FROM-PART
					   FROM-UNIT 
					   (LIST SUBSET-START
						 (IF SUBSET-N-BLOCKS
						     (MIN SUBSET-N-BLOCKS PART-SIZE)
						     (- PART-SIZE SUBSET-START)))
					   nil nil user-type)   ;03.12.87 DAB
				   WINDOW))
	      
	      ;; Receive packet containing size and comment.
	      
	      (SETQ PKT (CHAOS:GET-NEXT-PKT CONN))
	      
	      ;; Get the data in the form of a string, fetch the partition size, then
	      ;; return the packet.
	      
	      (SETQ STR (CHAOS:PKT-STRING PKT))
	      (SETQ REMOTE-PART-SIZE (READ-FROM-STRING STR T))
	      (CHAOS:RETURN-PKT PKT)
	      
	      ;; Verify that the local partition is large enough.
	      
	      (WHEN (> REMOTE-PART-SIZE PART-SIZE)
		    (RETURN-FROM RECEIVE-BAND
		      (FORMAT T "~% Does not fit in local partition, ~d > ~d"
			      REMOTE-PART-SIZE PART-SIZE)))
	      
	      ;; Revise partition size to be the actual size, then fetch
	      ;; the partition comment.
	      
	      (SETQ PART-SIZE REMOTE-PART-SIZE)
	      (SETQ PART-COMMENT
		    (READ-FROM-STRING STR () () :START
				      (1+
					(POSITION #\SPACE (THE STRING (STRING STR))
						  :TEST #'CHAR-EQUAL))))
	      
	      ;; Everything looks good.  Let's transfer the partition.
	      
	      (FORMAT T
		      "~& Receiving partition ~s on unit ~d from ~a ~
                   into partition ~s unit ~d:~%  ~d blocks, ~s~%"
		      FROM-PART 
		      (or FROM-UNIT "*DEFAULT-DISK-UNIT*")  ; DAB 05-03-89
		      FROM-MACHINE partition-name-string TO-UNIT PART-SIZE ;03.19.87 DAB
		      PART-COMMENT)
	      
	      ;; Put "Incomplete Copy" into the comment field so that you can
	      ;; tell that the data is incomplete.
	      
	      (UPDATE-PARTITION-COMMENT  partition-name-string "Incomplete Copy" TO-UNIT ) ;03.19.87 DAB 
	      
	      ;; Transfer the data from Chaos to the partition.
	      
	      (PARTITION-NET-TRANSFER TO-UNIT PART-BASE PART-SIZE CONN T () T
				      SUBSET-START SUBSET-N-BLOCKS)
	      
	      ;; Transfer complete, close net connection.
	      
	      (CHAOS::CLOSE-CONN CONN "Done")
	      
	      ;; Set partition comment.
	      
	      (UNLESS SUBSET-N-BLOCKS
		      (UPDATE-PARTITION-COMMENT partition-name-string  PART-COMMENT TO-UNIT ))))
    
    ;; Unwind-protect-forms
    
    (DISPOSE-OF-UNIT TO-UNIT)
    (WHEN CONN
	  (CHAOS:REMOVE-CONN CONN)))
  (RETURN-FROM RECEIVE-BAND
    (IF PART-BASE
	(PROGN
	  (FORMAT T "~%") T)))
  ())
 



(DEFUN TRANSMIT-BAND (FROM-PART FROM-UNIT TO-MACHINE TO-PART
		      &OPTIONAL (SUBSET-START 0) SUBSET-N-BLOCKS
		      &AUX CONN TO-UNIT PART-BASE PART-SIZE PART-COMMENT
		      user-type  partition-name-string) ;03.13.87 DAB
  
  "Write the FROM-PART partition on FROM-UNIT to TO-PART on TO-MACHINE.
   If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part of the partition
   to transfer. SUBSET-START and SUBSET-N-BLOCKS should be specified in hundreds of
   blocks. If a transfer dies, use the last number it printed as SUBSET-START to resume 
   where it left off.  FROM-MACHINE is a string consisting of the name of the remote
   machine or the remote machine, colon, remote unit number.  If no unit number is
   specified, the default unit of the TO-MACHINE is used."
  
  (DECLARE)
  (BLOCK XMIT-BODY
    (SETQ SUBSET-START (* SUBSET-START 100))
    (WHEN SUBSET-N-BLOCKS
      (SETQ SUBSET-N-BLOCKS (* SUBSET-N-BLOCKS 100)))
    (UNWIND-PROTECT
	(PROGN
	  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT "transmit-band"))
	  (when (closurep FROM-unit)   ;03-02-88 DAB Check to make sure it is a local unit.
	    (ferror () "FROM-UNIT must be a local unit number."))
	  (MULTIPLE-VALUE-SETQ (PART-BASE PART-SIZE nil from-part nil partition-name-string) ;03.12.87 DAB
			       (FIND-DISK-PARTITION-FOR-READ FROM-PART () FROM-UNIT))
	  (MULTIPLE-VALUE-SETQ (nil nil nil nil nil to-PART) ;10.20.88 DAB
	    (FIND-DISK-PARTITION-FOR-READ to-PART () to-machine))
	  ;; Parse machine and unit number.
	  
	  (LET ((TEM
		  (POSITION #\: (THE STRING (STRING TO-MACHINE)) :TEST #'CHAR-EQUAL)))
	    (WHEN TEM
	      (SETQ TO-UNIT (READ-FROM-STRING TO-MACHINE () () :START (1+ TEM)))
	      (SETQ TO-MACHINE (SUBSEQ (STRING TO-MACHINE) 0 TEM) )))
	  (SETQ PART-SIZE (MEASURED-SIZE-OF-PARTITION  partition-name-string FROM-UNIT) ;03.19.87 DAB
		PART-COMMENT (PARTITION-COMMENT  partition-name-string FROM-UNIT))
	  
	  ;; Connect to the remote machine.
	  (setf (values to-part user-type) ;03.16.87 DAB Check for "name.cpu"
		(parse-partition-name to-part :keyword)) ;03.13.87 DAB
	  (SETQ CONN
		(CHAOS:CONNECT TO-MACHINE
			       (FORMAT () "BAND-TRANSFER WRITE ~a ~d ~d ~d ~s ~a"
				       TO-PART TO-UNIT
				       (LIST SUBSET-START
					     (IF SUBSET-N-BLOCKS
						 (MIN PART-SIZE SUBSET-N-BLOCKS)
						 (- PART-SIZE SUBSET-START)))
				       PART-SIZE PART-COMMENT
				       user-type))) ;03.13.87 DAB
	  
	  ;; Display the transfer of the partition.
	  
	  (FORMAT T
		  "~& Transmitting partition ~s on unit ~d. to ~
                partition ~s on unit ~d. on ~a:~%  ~d blocks, ~s~%"
		  partition-name-string FROM-UNIT TO-PART
		  (or TO-UNIT "*DEFAULT-DISK-UNIT*")  ; DAB 05-03-89
		  TO-MACHINE PART-SIZE ;03.19.87 DAB
		  PART-COMMENT)
	  
	  ;; Transfer the partition via Chaos.
	  
	  (PARTITION-NET-TRANSFER FROM-UNIT PART-BASE PART-SIZE CONN () () T
				  SUBSET-START SUBSET-N-BLOCKS)
	  
	  ;; Data transfer complete, finish and close net connection.
	  
	  (CHAOS::FINISH-CONN CONN)
	  (CHAOS::CLOSE-CONN CONN "Done"))
      
      ;; Unwind protect forms
      (DISPOSE-OF-UNIT FROM-UNIT)
      (WHEN CONN
	(CHAOS:REMOVE-CONN CONN)))
    (FORMAT T "~%")
    (RETURN-FROM XMIT-BODY T)
    ()))





(DEFUN COMPARE-BAND (FROM-MACHINE FROM-PART LOCAL-PART
		     &OPTIONAL (LOCAL-UNIT *DEFAULT-DISK-UNIT*) (SUBSET-START 0) SUB-N-BLOCKS
		     &AUX CONN PKT STR (WINDOW (if (si:mx-p)
						   *mx-receive-band-window-size*
						   *exp-receive-band-window-size* ))
		     FROM-UNIT PART-COMMENT
		     REMOTE-PART-SIZE PART-BASE LOCAL-PART-SIZE user-type partition-name-string)
  
  "Compare the FROM-PART partition from FROM-MACHINE with our partition LOCAL-PART.
   If SUBSET-START or SUB-N-BLOCKS is specified, they say which part
   of the partition to compare.  They are measured in hundreds of blocks.
   If the operation dies, use the last number it printed as SUBSET-START, to resume
   where it left off.  FROM-MACHINE is a string consisting of the name of the remote
   machine or the remote machine, colon, remote unit number.  If no unit number is
   specified, the default unit of the REMOTE machine is used."
  
  (DECLARE)
  (BLOCK COMPARE-BODY
    (SETQ SUBSET-START (* SUBSET-START 100))
    (WHEN SUB-N-BLOCKS
      (SETQ SUB-N-BLOCKS (* SUB-N-BLOCKS 100)))
    (UNWIND-PROTECT
	(PROGN
	  (SETQ LOCAL-UNIT (DECODE-UNIT-ARGUMENT LOCAL-UNIT "compare-band"))
	  (when (closurep Local-unit)   ;03-02-88 DAB Check to make sure it is a local unit.
	    (ferror () "LOCAL-UNIT must be a local unit number."))
	  (MULTIPLE-VALUE-SETQ (PART-BASE LOCAL-PART-SIZE nil local-part nil partition-name-string) ;03.19.87 
	    (FIND-DISK-PARTITION-FOR-READ LOCAL-PART () LOCAL-UNIT))
	  (MULTIPLE-VALUE-SETQ (nil nil nil nil nil from-part)    ;12-13-88 DAB
	    (FIND-DISK-PARTITION-FOR-READ from-PART () from-machine))
	  ;; Parse machine and unit number.
	  
	  (LET ((TEM
		  (POSITION #\: (THE STRING (STRING FROM-MACHINE)) :TEST #'CHAR-EQUAL)))
	    (UNLESS (NULL TEM)
	      (SETQ FROM-UNIT (READ-FROM-STRING FROM-MACHINE () () :START (1+ TEM)))
	      (SETQ FROM-MACHINE (SUBSEQ (STRING FROM-MACHINE) 0 TEM) )))
	  
	  ;; Connect to remote machine.
	  (setf (values from-part user-type)
		(parse-partition-name from-part :keyword)) ;03.13.87 DAB
	  (SETQ CONN
		(CHAOS:CONNECT FROM-MACHINE
			       (FORMAT () "BAND-TRANSFER READ ~a ~d ~d ~a ~a ~a" FROM-PART
				       FROM-UNIT
				       (LIST SUBSET-START
					     (IF SUB-N-BLOCKS
						 (MIN SUB-N-BLOCKS LOCAL-PART-SIZE)
						 (- LOCAL-PART-SIZE SUBSET-START)))
				       nil nil user-type) ;03.12.87 DAB
			       WINDOW))
	  
	  ;; Receive packet containing size and comment.
	  
	  (SETQ PKT (CHAOS:GET-NEXT-PKT CONN))
	  
	  ;; Get the data in the form of a string, fetch the partition size, then
	  ;; return the packet.
	  
	  (SETQ STR (CHAOS:PKT-STRING PKT))
	  (SETQ REMOTE-PART-SIZE (LET ((*READ-BASE* 10)
				       (*print-BASE* 10))
				   (READ-FROM-STRING STR T)))
	  (CHAOS:RETURN-PKT PKT)
	  (SETQ LOCAL-PART-SIZE REMOTE-PART-SIZE)
	  
	  ;; Fetch the partition comment.
	  
	  (SETQ PART-COMMENT
		(READ-FROM-STRING STR () () :START
				  (1+
				    (POSITION #\SPACE (THE STRING (STRING STR)) :TEST
					      #'CHAR-EQUAL))))
	  
	  ;; Everything looks good.  Let's compare the partitions.
	  
	  (FORMAT T
		  "~& Comparing partition ~s on unit ~d. with ~
                   partition ~s on unit ~d. from ~a:~%  ~d blocks, ~s~%"
		   partition-name-string LOCAL-UNIT FROM-PART FROM-UNIT FROM-MACHINE ;03.19.87 DAB
		  LOCAL-PART-SIZE PART-COMMENT)
	  (PARTITION-NET-TRANSFER LOCAL-UNIT PART-BASE LOCAL-PART-SIZE CONN () T T
				  SUBSET-START SUB-N-BLOCKS)
	  
	  ;; Comparison complete, close net connection.
	  
	  (CHAOS::CLOSE-CONN CONN "Done"))
      
      ;; Unwind-protect-forms
      (DISPOSE-OF-UNIT LOCAL-UNIT)
      (WHEN CONN
	(CHAOS:REMOVE-CONN CONN)))
    (FORMAT T "~%")
    (RETURN-FROM COMPARE-BODY T)
    ()))
  


1;;;------------------------------------------------------------------------------------
;;;
;;;  Server Functions -- Band-Transfer-Server.
;;;
;;;------------------------------------------------------------------------------------
;;;
;;;  RFC is:  "BAND-TRANSFER READ//WRITE BAND UNIT SUBSET SIZE COMMENT* USER/CPU1" +++* ;03.16.87 DAB
1;;;  Unit of NIL means use *default-disk-unit* on the server machine. +++

;;; *BJ* For *RWF**
(DEFVAR 4BAND-TRANSFER-DEBUG-SERVER* ()
  2"Set non-nil to throw server into error handler on errors"*) 

(DEFUN BAND-TRANSFER-SERVER ()
  (let (PKT
	 WRITE-P
	 PART-COMMENT
	 SUB-START
	 SUB-N
	 CONN
	 STR
	 RFC
	 LOCAL-UNIT
	 part-name ;03.16.87 DAB
	 user-type ;03.12.87 DAB
	 (WINDOW (if (si:mx-p)
		     *mx-receive-band-window-size*
		     *exp-receive-band-window-size* ))
	 PART-BASE PART-SIZE  partition-name-string
	 (error-message nil)  ;06-21-88 DAB
	 (Despose-unit t))  ; DAB 05-03-89
    (UNWIND-PROTECT
	(CONDITION-CASE-IF (NOT BAND-TRANSFER-DEBUG-SERVER) (ERR)
	    (PROGN
	      (SETQ CONN (CHAOS::LISTEN "BAND-TRANSFER" WINDOW))
	      (SETQ STR (CHAOS::PKT-STRING (CHAOS::READ-PKTS CONN)))
	      (SETQ RFC
		    (LET ((*READ-BASE* 10.))
		      (READ-FROM-STRING (STRING-APPEND "(" STR ")") T)))
	      ;; Fetch unit number if specified, otherwise local default.
	      (SETf (values LOCAL-UNIT Despose-unit) ;DAB 05-03-89 The 2nd arg will tell us when to Dispose.
		    (DECODE-UNIT-ARGUMENT
		      (OR (FOURTH RFC) *DEFAULT-DISK-UNIT*) "band server"))
	      ;; Check if band transfer serving is enabled.
	      (UNLESS (OR BAND-TRANSFER-SERVER-ON
			  (MEMBER USER-ID '(NIL "") :TEST #'EQUAL))
		      (RETURN-FROM BAND-TRANSFER-SERVER
			(CHAOS::REJECT CONN (FORMAT () "This machine is in use by ~A" USER-ID))))
	      ;; Check if logical unit is a valid unit.
	      (UNLESS (or (integerp local-unit) (closurep local-unit))
		;;if a bad pack name was given to decode-unit-argument the function return the name as a 
                ;;symbol. We need to send error message and make sure we dont try to despose the unit.
		(setf  Despose-unit nil)  ; DAB 05-03-89 In this case 
		(RETURN-FROM BAND-TRANSFER-SERVER
		  (CHAOS::REJECT CONN (FORMAT () "Unknown disk unit ~A" local-unit))))
	      
	      (setq part-name (THIRD RFC)) ;03.16.87 DAB
	      (when (eighth rfc) ;03.16.87 DAB CPU?
		(setq user-type (si:intern (eighth rfc) :keyword))	   ;04.30.87 DAB
	    	(setq part-name (string-append part-name "." user-type)))  ;04.30.87 DAB
	      ;; Locate the partition in question.
	      (condition-case (condition)   ;04-04-88 DAB
		(MULTIPLE-VALUE-setq (PART-BASE PART-SIZE nil nil nil partition-name-string) ;03.19.87 DAB
		    (FIND-DISK-PARTITION-FOR-READ part-name () LOCAL-UNIT () "lod" nil))
		(error (setf error-message (send condition :report-string)))) ;06-21-88 DAB 
		;; Check if specified partition is there.
		(UNLESS PART-BASE
		  (RETURN-FROM BAND-TRANSFER-SERVER
		    (CHAOS::REJECT CONN
				   error-message)))   ;06-21-88 DAB
		;; Decode subset specification if present.
		(WHEN (FIFTH RFC)
		      (SETQ SUB-START (FIRST (FIFTH RFC)))
		      (SETQ SUB-N (SECOND (FIFTH RFC))))
		;; Decode band transfer request.
		(COND ((STRING-EQUAL (SECOND RFC) "READ")
		       (SETQ WRITE-P ())
		       (SETQ PART-COMMENT (PARTITION-COMMENT  partition-name-string LOCAL-UNIT)));03.19.87
		      
		      ((STRING-EQUAL (SECOND RFC) "WRITE")
		       (SETQ WRITE-P T)
		       (when (AND (EQ local-UNIT *DEFAULT-DISK-UNIT*)  ;04-04-88 DAB
				  (STRING-EQUAL part-NAME *LOADED-BAND*))
			  	(RETURN-FROM BAND-TRANSFER-SERVER
				  (CHAOS::REJECT CONN
					    (FORMAT
					      ()
					      "~& Do not attempt to write into the current band. This will crash the system and corrupt the current band.")
					    )))
  
		       (IF (AND (> (SIXTH RFC) PART-SIZE) (> (+ SUB-START SUB-N) PART-SIZE))
			   (RETURN-FROM BAND-TRANSFER-SERVER
			     (CHAOS::REJECT CONN
					    (FORMAT () "Partition too small, ~d > ~d" (SIXTH RFC)
						    PART-SIZE))))
		       (SETQ PART-SIZE (SIXTH RFC))
		       (SETQ PART-COMMENT (STRING (SEVENTH RFC))))
		      
		      (T 
			(RETURN-FROM BAND-TRANSFER-SERVER  ; transfer not READ or WRITE.
			  (CHAOS::REJECT CONN
					 (FORMAT () "Illegal band transfer operation (~a)"
						 (SECOND RFC))))))
		;; Validate subset specification.
		(WHEN (AND SUB-START
			   (OR (MINUSP SUB-START)
			       (MINUSP SUB-N)
			       (AND WRITE-P (> (+ SUB-START SUB-N) PART-SIZE))))
		      (RETURN-FROM BAND-TRANSFER-SERVER
			(CHAOS::REJECT CONN (FORMAT NIL "Subset outside of partition.~
                                                         Part-size = ~d Start = ~d Transfer count = ~d"
					       PART-SIZE SUB-START SUB-N))))
		;; Everything looks good. Accept connection.
		(CHAOS::ACCEPT CONN)
		;; If required, notify user of band transfer activity.
		(WHEN (EQ BAND-TRANSFER-SERVER-ON :NOTIFY)
		      (PROCESS-RUN-FUNCTION
			"Notify"
			'fs:NOTIFY-nicely ()      ;07-11-88 DAB
			"BAND-TRANSFER-SERVER: ~:[Read~;Write~] of partition ~s on unit ~d. by ~a"
			WRITE-P  partition-name-string LOCAL-UNIT;03.19.87 DAB
			(CHAOS::HOST-DATA (CHAOS::FOREIGN-ADDRESS CONN))))
		
		;; Put status in the WHO line.
		(SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN "BAND-TRANSFER")
		;; If writing to disk, update part-comment; otherwise, send packet
		;; containing size and comment.
		(IF WRITE-P
		    (UPDATE-PARTITION-COMMENT  partition-name-string "Incomplete Copy" LOCAL-UNIT);03.19.87 
		    (PROGN
		      (SETQ PART-SIZE (MEASURED-SIZE-OF-PARTITION  partition-name-string LOCAL-UNIT));03.19.87
		      (SETQ PKT (CHAOS::GET-PKT))
		      (CHAOS::SET-PKT-STRING PKT (FORMAT () "~d ~s" PART-SIZE PART-COMMENT))
		      (CHAOS::SEND-PKT CONN PKT)))
		;; Transfer the partition over the net.
		(PARTITION-NET-TRANSFER LOCAL-UNIT PART-BASE PART-SIZE CONN
					WRITE-P () () SUB-START SUB-N)
		;; Data transfer complete, finish and close net connection.
		(IF WRITE-P
		    (UPDATE-PARTITION-COMMENT  partition-name-string PART-COMMENT LOCAL-UNIT));03.19.87 DAB
	      )				   ; progn
	  (ERROR
	   (PROGN
	     (SEND TV:WHO-LINE-FILE-STATE-SHEET :DELETE-SERVER CONN)
	     (IF (EQ (CHAOS::STATE CONN) 'RFC-RECEIVED-STATE)
		 (CHAOS::REJECT CONN (SEND ERR :REPORT-STRING))
		 (CHAOS::LOS-CONN CONN (SEND ERR :REPORT-STRING)))))
	  (:NO-ERROR
	    (PROGN
	      (CHAOS::FINISH-CONN CONN)
	      (SEND TV:WHO-LINE-FILE-STATE-SHEET :DELETE-SERVER CONN)
	      (CHAOS::CLOSE-CONN CONN "Done"))))   ; condition-case
      ;; Unwind protect forms.
      (when  Despose-unit (DISPOSE-OF-UNIT LOCAL-UNIT))	    ; DAB 05-03-89
      ))
  ())

(ADD-INITIALIZATION 3"BAND-TRANSFER"*
		    '(PROCESS-RUN-FUNCTION
		       '(:NAME 3"BAND-TRANSFER Server"* :PRIORITY -1)
		       'BAND-TRANSFER-SERVER)
		    () 'CHAOS:SERVER-ALIST)
 



1;;;-----------------------------------------------------------------------------------
;;;
;;; Driver functions; these perform all I/O between partitions and Chaosnet.
;;;
;;;-----------------------------------------------------------------------------------*


(DEFUN 4ARRAY-TO-NET* (BUF CONN &OPTIONAL (NHWDS (ARRAY-TOTAL-SIZE BUF)) (OPCODE 192.) (BUF-OFFSET 0))
  2"  Takes data from the array BUF and sends it over the connection CONN."*
  (DO ()
      ((NOT (ARRAY-INDIRECT-P BUF)))
    (AND (ARRAY-INDEX-OFFSET BUF) (INCF BUF-OFFSET (ARRAY-INDEX-OFFSET BUF)))
    (SETQ BUF (ARRAY-INDIRECT-TO BUF)))
  (DO ((I 0 (+ I N))				1; N is the number of halfwords put in each packet. *
       (N CHAOS::MAX-DATA-WORDS-PER-PKT)
       PKT)
      ((>= I NHWDS))
    1;; When the rest of the buffer has less than a packet's worth of data, *
    1;; copy only that data to the packet.  Get a packet to put the data in.*
    (SETQ N (MIN N (- NHWDS I)))
    (SETQ PKT (CHAOS::GET-PKT))
    1;; Copy the next portion of the array to the packet.  Set the number of*
    1;; bytes, then send the packet over the connection.*
						1;    (COPY-ARRAY-PORTION BUF I (+ I N)*
						1;*			1PKT CHAOS:FIRST-DATA-WORD-IN-PKT*
						1;*			1(+ CHAOS:FIRST-DATA-WORD-IN-PKT N))*
    (WITHOUT-INTERRUPTS
      (%BLT
	(%MAKE-POINTER-OFFSET DTP-FIX BUF
			      (+ (FLOOR (+ I BUF-OFFSET) 2) (ARRAY-DATA-OFFSET BUF)))
	(%MAKE-POINTER-OFFSET DTP-FIX PKT
			      (+ (FLOOR CHAOS::FIRST-DATA-WORD-IN-PKT 2) (ARRAY-DATA-OFFSET PKT)))
	(CEILING (MAX 0 N) 2) 1))
    (SETF (CHAOS::PKT-NBYTES PKT) (* N 2))
    (CHAOS::SEND-PKT CONN PKT OPCODE))			1; do*
  )

(DEFUN 4ARRAY-FROM-NET* (BUF CONN &OPTIONAL (NHWDS (ARRAY-TOTAL-SIZE BUF)) PKT PKT-PTR &AUX BUFLIM
			   (PKT-OFFSET CHAOS::FIRST-DATA-WORD-IN-PKT) PKT-START DATA-LEN (BUF-OFFSET 0))
  2"  Reads data from the connection CONN and copies it to array BUF."*
  (DO ()
      ((NOT (ARRAY-INDIRECT-P BUF)))
    (AND (ARRAY-INDEX-OFFSET BUF) (INCF BUF-OFFSET (ARRAY-INDEX-OFFSET BUF)))
    (SETQ BUF (ARRAY-INDIRECT-TO BUF)))
  (DO ((I 0 (+ I DATA-LEN)))
      1;; If the last packet did not fit, return it with a pointer*
      1;; to its uncopied data, otherwise return a nil packet.*
      ((>= I NHWDS) (VALUES PKT PKT-PTR))
    1;; Unless an unfinished packet was passed, get a new packet*
    1;; and set up a pointer to its data.*
    (UNLESS PKT
      (SETQ PKT (CHAOS::GET-NEXT-PKT CONN)
	    PKT-PTR PKT-OFFSET))
    1;; Setup pointers to the buffer space and the packet data, then*
    1;; copy from packet to buffer.    *
    (SETQ DATA-LEN (- (CHAOS::PKT-NWORDS PKT) PKT-PTR)
	  PKT-START PKT-PTR
	  BUFLIM (MIN NHWDS (+ I DATA-LEN))
	  PKT-PTR (+ PKT-START (- BUFLIM I)))	1;    (COPY-ARRAY-PORTION PKT PKT-START PKT-PTR *
						1;*			1BUF I BUFLIM)*
    (WITHOUT-INTERRUPTS
      (%BLT (%MAKE-POINTER-OFFSET DTP-FIX PKT (+ (FLOOR PKT-START 2) (ARRAY-DATA-OFFSET PKT)))
	    (%MAKE-POINTER-OFFSET DTP-FIX BUF
				  (+ (FLOOR (+ BUF-OFFSET I) 2) (ARRAY-DATA-OFFSET BUF)))
	    (CEILING (MAX 0 (- BUFLIM I)) 2) 1))
    1;; If the entire packet's data fits in the buffer,*
    1;; return it to Chaos.*
    (WHEN (= BUFLIM (+ I DATA-LEN))
      (CHAOS::RETURN-PKT PKT)
      (SETQ PKT ()))))




(DEFUN 4PARTITION-NET-TRANSFER* (LOCAL-UNIT PART-BASE PART-SIZE CONN WRITE-P
			       &OPTIONAL (COMPARE-P NIL) (DISPLAY NIL) (SUBSET-START 0)
			       SUBSET-N-BLOCKS
			       &AUX RQB-A RQB-B BUF ORIG-PART-BASE PKT PKT-PTR TOP
			       (QUANTUM 17) (N-HUNDRED 0))
  
  2"Performs a double-buffered transfer between the Chaos connection CONN and
   the local partition specified by Local-Unit, Part-Base, and Part-Size.
   Write-P is set to T when writing to the local partition, Nil when reading.
   Compare-P is set to T when only comparing a local partition to a remote one.
   Display is set to T for a screen display of the transfer (user end). Subset-
   Start and Subset-N-Blocks are specified for partition subset transfers."*
  
  (UNLESS SUBSET-START
    (SETQ SUBSET-START 0))
  
  1;; When doing a subset-transfer, offset the partition-base and adjust*
  1;; the partition size.*
  
  (SETQ ORIG-PART-BASE PART-BASE)
  (SETQ PART-BASE (+ PART-BASE SUBSET-START)
	PART-SIZE (- PART-SIZE SUBSET-START))
  (WHEN SUBSET-N-BLOCKS
    (SETQ PART-SIZE (MIN PART-SIZE SUBSET-N-BLOCKS)))
  (SETQ TOP (+ PART-BASE PART-SIZE))
  
  (UNWIND-PROTECT
      (PROGN

	1;; Fetch two disk request blocks for double-buffering.*
	(SETQ RQB-A (GET-DISK-RQB (min QUANTUM part-size))) ;03.19.87 DAB
	(UNLESS COMPARE-P (SETQ RQB-B (GET-DISK-RQB (min QUANTUM part-size)))) ;03.19.87 DAB
	(SETQ BUF (MAKE-ARRAY (ARRAY-TOTAL-SIZE (RQB-BUFFER RQB-A))
			      :ELEMENT-TYPE '(UNSIGNED-BYTE 16)))
	
	1;; Boogie chillun*
	
	(DO ((BLOCK PART-BASE (+ BLOCK QUANTUM))
	     (LAST-RQB-SIZE NIL RQB-SIZE)
	     RQB-SIZE)
	    
	    ((>= BLOCK TOP)
	     (WHEN (AND (NOT COMPARE-P) LAST-RQB-SIZE)
	       (WHEN DISPLAY
		 (SETQ N-HUNDRED (DISPLAY-BLOCK BLOCK ORIG-PART-BASE N-HUNDRED)))
	       (IF WRITE-P
		   (WAIT-AND-CHECK-ERRORS RQB-B)
		   (ARRAY-TO-NET (RQB-BUFFER RQB-A) CONN (* LAST-RQB-SIZE si:disk-block-word-size 2)))))
	  
	  (WHEN DISPLAY
	    (SETQ N-HUNDRED (DISPLAY-BLOCK BLOCK ORIG-PART-BASE N-HUNDRED)))
	  (SETQ RQB-SIZE (MIN QUANTUM (- TOP BLOCK)))
	  (COND
	    (COMPARE-P  1; Used to compare bands.*
	     
	     1;; When comparing partitions, read a disk buffer while receiving*
	     1;; a buffer from the net, then compare the the two buffers.*
	     
	     (DISK-READ RQB-A LOCAL-UNIT BLOCK RQB-SIZE () 0 T)
	     (MULTIPLE-VALUE-SETQ (PKT PKT-PTR)
				  (ARRAY-FROM-NET BUF CONN (* RQB-SIZE si:disk-block-word-size 2)
						  PKT PKT-PTR))
	     (WAIT-AND-CHECK-ERRORS RQB-A)
	     (COMPARE-ARRAYS-AND-SHOW-DIFFERENCES (RQB-BUFFER RQB-A) BUF T
						  (- BLOCK PART-BASE)))
	    (WRITE-P    1; Used to receive bands.*
	     
	     1;; When writing to a partition, read a buffer from the net,  *
	     1;; then write the buffer to disk while reading the next buffer*
	     1;; from the net (returning any unfinished packets).*
	     
	     (MULTIPLE-VALUE-SETQ (PKT PKT-PTR)
				  (ARRAY-FROM-NET
				    (RQB-BUFFER RQB-A) CONN (* RQB-SIZE si:disk-block-word-size 2) PKT
				    PKT-PTR))
	     (WHEN LAST-RQB-SIZE
	       (WAIT-AND-CHECK-ERRORS RQB-B))
	     (SETQ BUF RQB-A
		   RQB-A RQB-B
		   RQB-B BUF)
	     (DISK-WRITE RQB-B LOCAL-UNIT BLOCK RQB-SIZE () 0 T))
	    (T          1; Used to transmit bands.*
	     
	     1;; When reading from a partition, read the first buffer, then*
	     1;; read the next buffer while sending the previous buffer over*
	     1;; the net.  Don't forget to send the last buffer (see above).*
	     
	     (DISK-READ RQB-B LOCAL-UNIT BLOCK RQB-SIZE () 0 T)
	     (WHEN LAST-RQB-SIZE
	       (ARRAY-TO-NET (RQB-BUFFER RQB-A) CONN (* LAST-RQB-SIZE si:disk-block-word-size 2)))
	     (WAIT-AND-CHECK-ERRORS RQB-B)
	     (SETQ BUF RQB-B
		   RQB-B RQB-A
		   RQB-A BUF)))))
    
    1;; Unwind-protect forms*
    
    (WHEN RQB-A
      (RETURN-DISK-RQB RQB-A))
    (WHEN RQB-B
      (RETURN-DISK-RQB RQB-B))))

















