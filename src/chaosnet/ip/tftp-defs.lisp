;;; -*- Mode: COMMON-LISP; Package: IP; Base: 10.; Fonts:(COURIER MEDFNB TR12BI) -*-

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

;1; DEFCONSTANTS for TFTP*

(DEFCONSTANT tftp-process-name "TFTP Server") 

;1; Window Constants*


(DEFCONSTANT *tftp-valid-modes-list* '(:netascii :octet :xld)
   "2The list of valid tftp transfer modes.*"
   ) 

(DEFCONSTANT *tftp-default-mode* :netascii
   "2The default file transfer mode.*"
   ) 

(DEFCONSTANT *tftp-default-window*
   (LIST '(source "Source file     " :string) '(destination "Destination file" :string)
	 (LIST 'mode "Mode of transfer" :choose *tftp-valid-modes-list*))) 

;1;  Packet sizes.*

(DEFCONSTANT tftp-maximum-data-size 512
   "2Max size of TFTP data.*"
   ) 

(DEFCONSTANT tftp-data-header-size 4
   "2Size of Data Header. Includes Op and block num.*"
   ) 

(DEFCONSTANT tftp-error-header-size 4
   "2Size of Error packet header.  Has Op and Errorcode.*"
   ) 

(DEFCONSTANT tftp-request-header-size 2
   "2Size of the Request packet 'header'. Includes opcode. *"
   ) 

(DEFCONSTANT tftp-ack-packet-size 4
   "2Size of an Ack packet.*"
   ) 

(DEFCONSTANT tftp-data-packet-size (+ tftp-maximum-data-size tftp-data-header-size)
   "2The maximum size of a data packet.*"
   ) 

(DEFCONSTANT tftp-maximum-header-size tftp-data-header-size
   "2The maximum header for a TFTP packet.*"
   ) 

(DEFCONSTANT tftp-minimum-header-size tftp-request-header-size
   "2The minimum header for a TFTP packet.*"
   ) 

(DEFCONSTANT tftp-maximum-packet-size (+ tftp-maximum-data-size tftp-maximum-header-size)
   "2The maximum packet size.*"
   ) 

;1;General constants*

(DEFPARAMETER *tftp-connection* ()) 

(DEFCONSTANT tftp-port 69
   "2Well Known TFTP port.*"
   ) 

(DEFCONSTANT tftp-block-offset 2
   "2Location of block number relative to array beginning*"
   ) 

(DEFCONSTANT tftp-error-code-offset 2
   "2Location of error code number relative to the array beginning*"
   ) 

(DEFCONSTANT tftp-data-offset tftp-data-header-size
   "2Start of the data relative to the array beginning. also for error pkts.*"
   ) 

(DEFCONSTANT tftp-error-text-offset tftp-error-header-size
   "2Start of the error text string.*"
   ) 

(DEFCONSTANT tftp-packet-type 'art-8b
   "2The element type of a TFTP packet.*"
   ) 

(DEFCONSTANT tftp-string-delimiter 0
   "2Delimiter for the filename and mode in a request packet.*"
   ) 

(DEFCONSTANT tftp-number-of-timeouts 5
   "2Number time outs while waiting to receive a packet*"
   ) 

(DEFCONSTANT tftp-number-timeouts-last-data 2
   "2Number of times to timeout after sending the last data packet before quitting with succuess.*"
   ) 

(DEFCONSTANT tftp-number-of-resends 5
   "2Number times to resend a packet which gets lost on the network.*"
   ) 

;1; Packet Op codes.*


(DEFCONSTANT tftp-rrq 1
   "2Read Request*"
   ) 

(DEFCONSTANT tftp-wrq 2
   "2Write request*"
   ) 

(DEFCONSTANT tftp-data 3
   "2Data packet*"
   ) 

(DEFCONSTANT tftp-ack 4
   "2Acknowledge packet*"
   ) 

(DEFCONSTANT tftp-error 5
   "2Error packet*"
   ) 

(DEFCONSTANT tftp-max-opcode-number tftp-error
   "2The maximum number for a valid opcode.*"
   ) 

;1; Error Codes specified by TFTP.*

(DEFCONSTANT tftp-error-text 0
   "2This error is not defined and the error message should be examined.*"
   ) 

(DEFCONSTANT tftp-file-not-found 1
   "2File not found.*"
   ) 

(DEFCONSTANT tftp-access-violation 2
   "2Access violation of a directory or of the file specified.*"
   ) 

(DEFCONSTANT tftp-allocation-exceeded 3
   "2Disk is full or the allocation is exceeded.*"
   ) 

(DEFCONSTANT tftp-illegal-operation 4
   "2An illegal TFTP operation.*"
   ) 

(DEFCONSTANT tftp-unknown-transfer-id 5
   "2Unknown transfer ID.*"
   ) 

(DEFCONSTANT tftp-file-exists 6
   "2File already exists.*"
   ) 

(DEFCONSTANT tftp-no-such-user 7
   "2No such user.*"
   ) 

;1; Common Messages to the user*


(DEFCONSTANT tftp-data-size-exceeded-message
   "The data exceeded the maximum data size of 512. bytes.") 

(DEFCONSTANT tftp-too-many-timeouts-message "Too many timeouts.") 

(DEFCONSTANT tftp-too-many-resends-message "Too many resends.") 

(DEFCONSTANT tftp-timeout-message "Timing out.") 

(DEFCONSTANT tftp-terminate-connection-message "Terminating this connection.") 

(DEFCONSTANT tftp-resends-lost-on-network-message
   (FORMAT ()
	   "The current packet has been lost on the network ~
                           for ~D transmission attempts."
	   tftp-number-of-resends)) 


;1; DEFPARAMETERS for TFTP.*

(DEFPARAMETER tftp-port-connection ()
   "2The connection object used by LISTEN-ON-TFTP-PORT.*"
   ) 

(DEFPARAMETER *tftp-connections-list* ()
   "2The list of current tftp-connection objects in use.*"
   ) 

(DEFPARAMETER *tftp-debug-mode* ()
   "2Print debug messages if non-nill.*"
   ) 
;1; TFTP-TIMEOUT was 900. but taht was too long to wait.*

(DEFPARAMETER tftp-timeout 8.
    "2seconds*")

(DEFPARAMETER tftp-dally-time 1
   "2Time to dally on the last packet sent.*"
   ) 

(DEFPARAMETER *tftp-return* ()) 

(DEFPARAMETER translation-list '(8 9 10 12 127 0)) 

(DEFPARAMETER notify-on-security-violations ()) 

(DEFPARAMETER tftp-default-pathname "LM: SITE; TFTP-LOST-FILE.LISP"
   "2User settable default local file name.*") 

;1; Variables for a peekaboo list.*

(DEFPARAMETER *tftp-unknown-error-count* 0) 

(DEFPARAMETER *tftp-file-not-found-count* 0) 

(DEFPARAMETER *tftp-access-violation-count* 0) 

(DEFPARAMETER *tftp-allocaton-exceeded-count* 0) 

(DEFPARAMETER *tftp-illegal-operation-count* 0) 

(DEFPARAMETER *tftp-file-already-exists-count* 0) 

(DEFPARAMETER *tftp-no-such-user-count* 0) 

(DEFPARAMETER *tftp-timeout-count* 0) 

(DEFPARAMETER *tftp-bad-packet-size-count* 0) 

(DEFPARAMETER *tftp-received-bad-mode-count* 0) 

(DEFPARAMETER *tftp-unexpected-packet-count*
   0) 

(DEFPARAMETER *tftp-unknown-transfer-id-count* 0) 

;1; MACROS for TFTP*

;1; Common Messages to the user*


(DEFMACRO tftp-start-transfer-message (local remote direction mode)
  `(LET ()
     (MULTIPLE-VALUE-SETQ (source destination)
       (IF (EQUAL ,direction :read)
	   (VALUES ,local ,remote)
	   (VALUES ,remote ,local)))
     (FORMAT () "~&TFTP:  Starting file transfer of  ~A  to  ~A  MODE: ~A"
	     (IF (STREAMP source)
		 (SEND (PATHNAME source) :string-for-printing)
		 source)
	     (IF (STREAMP destination)
		 (SEND (PATHNAME destination) :string-for-printing)
		 destination)
	     ,mode))) 

(DEFMACRO tftp-security-violation-message (address)
  `(FORMAT ()
	   "The request was rejected because the host address ~A violates security restrictions."
	   ,address)) 

(DEFMACRO tftp-bad-data-size-message (data-length)
  `(FORMAT () "Bad data length ~A." ,data-length)) 

;1;; RLA 9/18/86 - remove this definitions as it is superseded several lines later*
;1(DEFMACRO TFTP-BAD-PACKET-SIZE-MESSAGE (PACKET-LENGTH &OPTIONAL (MIN TFTP-MINIMUM-HEADER-SIZE)*	1   *
;					1(MAX TFTP-MAXIMUM-PACKET-SIZE))*
;1  `(FORMAT ()*
;	1   "A packet of size ~A was received.~*
;1                           The packet size should be between ~A and ~A."*
;	1   ,PACKET-LENGTH ,MIN ,MAX)) *
;1----*

(DEFMACRO tftp-file-access-violation-message (file)
  `(FORMAT () "Access violation ~A ~A." (IF (EQUAL operation :read)
					  "reading from"
					  "writing to")
	   ,file)) 

(DEFMACRO tftp-illop-message (opcode)
  `(FORMAT () "Received an illegal TFTP operation. code = ~D." ,opcode)) 

(DEFMACRO tftp-bad-packet-size-message (pkt-length)
  `(FORMAT () "The received packet of ~D bytes exceeded the maximum packet size of ~D bytes."
	   ,pkt-length tftp-data-packet-size)) 

(DEFMACRO tftp-bad-mode-message (mode)
  `(FORMAT () "The request packet has an unknown mode, ~A." ,mode)) 


(DEFMACRO tftp-success-message (local remote direction)
  `(LET ()
     (MULTIPLE-VALUE (source destination)
       (IF (EQUAL ,direction :read)
	   (VALUES ,local ,remote)
	   (VALUES ,remote ,local)))
     (FORMAT () "~&TFTP:  Successful transfer of     ~A  to  ~A"
	     (IF (STREAMP source)
		 (SEND (PATHNAME source) :string-for-printing)
		 source)
	     (IF (STREAMP destination)
		 (SEND (PATHNAME destination) :string-for-printing)
		 destination)))) 


(DEFMACRO tftp-bad-packet-type-message (type)
  `(FORMAT () "Packet received had an unexpected packet type, ~A." ,type)) 


(DEFMACRO tftp-unexpected-packet-message (type number enum)
  `(FORMAT ()
	   "~&An unexpected ~A packet with block number ~D was received ~
              when the block number ~D was expected.~&"
	   ,type ,number ,enum)) 


(DEFMACRO tftp-too-many-timeouts ()	   ;1Message header to specify that there were too many*
					   ;1timeouts while waiting to receive a packet.*
  '(FORMAT () "~A:  ~A  ~A" (SEND si:local-host :name-as-file-computer)
	   tftp-too-many-timeouts-message tftp-terminate-connection-message)) 


(DEFMACRO tftp-too-many-resends ()	   ;1Message header to specify that *
					   ;1the packet is lost before reaching the remote host every time it is sent.*
  '(FORMAT () "~A:  ~A  ~A " (SEND si:local-host :name-as-file-computer)
	   tftp-resends-lost-on-network-message tftp-terminate-connection-message)) 

;1;;*
;1;; DEFMACROS for the functions and methods*
;1;;*

;1; TFTP-EXTRACT-ERROR-CODE*
;1; Extract the error code number from a TFTP error packet.*

(DEFMACRO tftp-extract-error-code (pkt)
  `(+ (ASH (AREF ,pkt tftp-error-code-offset) 8) (AREF ,pkt (1+ tftp-error-code-offset)))) 


;1; TFTP-SET-PACKET-OPCODE*

(DEFUN tftp-set-packet-opcode (pkt op)
  ;1;Write the opcode number into the out going packet.*
  (SETF (AREF pkt 0) (LDB (BYTE 8 8) op))
  (SETF (AREF pkt 1) (LDB (byte 8 0) op)))


;1; TFTP-SET-PACKET-BLOCK-NUMBER*

(DEFUN tftp-set-packet-block-number (pkt block)
  ;1;Write the block number into the out going packet.*
  (SETF (AREF pkt tftp-block-offset) (LOGAND 255 (ASH block -8)))
  (SETF (AREF pkt (1+ tftp-block-offset)) (LOGAND 255 block))) 

;1; TFTP-SET-PACKET-STRING*
;1; Write a string into a TFTP packet at the specified offset*

(DEFUN tftp-set-packet-string (pkt string &optional (offset 0))
  (LOOP for name-index from 0 below (LENGTH string) do
	(SETF (AREF pkt (+ name-index offset)) (AREF string name-index))
	finally (INCF offset name-index))
  (SETF (AREF pkt offset) 0)		   ;1 Put in delimiter.*
  (INCF offset)				   ;1 Increment for delimiter.*
  ) 

;1; TFTP-GET-OPCODE*
;1; Returns the opcode of a TFTP packet.*

(DEFMACRO tftp-get-opcode (pkt)		   ;1Gets the op code from the TFTP packet. Returns the op code.*
  `(+ (ASH (AREF ,pkt 0) 8) (AREF ,pkt 1))) 

;1; TFTP-GET-BLOCK-NUMBER*
;1; Returns the block number for a TFTP packet.*

(DEFMACRO tftp-get-block-number (pkt)
  ;1;Gets the Block number from a TFTP packet. Returns the block number.*
  `(+ (ASH (AREF ,pkt tftp-block-offset) 8) (AREF ,pkt (1+ tftp-block-offset)))) 

;1; TFTP-GET-BINARY-BYTE*
;1; Returns a 16 bit byte from the packet*

(DEFMACRO tftp-get-binary-byte (pkt index)
  `(+ (ASH (AREF ,pkt ,index) 8) (AREF ,pkt (1+ ,index)))) 


;1;; TFTP-OPCODE-P*
;1;; Check if the opcode is valid.*

(DEFMACRO tftp-opcode-p (opcode)
  `(AND (> ,opcode 0) (<= ,opcode tftp-max-opcode-number))) 
 
;1;; TFTP-MODE-P*
;1;; Check if the mode is valid.*

(DEFMACRO tftp-mode-p (mode)
  `(MEMBER ,mode *tftp-valid-modes-list* :TEST #'EQ)) 

;1;; TFTP-VALID-REMOTE-ADDRESS-P*
;1;; Check an incoming packet's address for validity.*

(DEFMACRO tftp-valid-remote-address-p (pkt-addr remote-addr)
  `(EQUAL ,remote-addr ,pkt-addr)) 

;1;; TFTP-VALID-LOCAL-FILE-P*
;1;; The filename can be specified as a string, pathname or stream.*

(DEFMACRO tftp-valid-local-file-p (filename)
  `(OR (STRINGP ,filename) (PATHNAMEP ,filename) (STREAMP ,filename))) 

;1;; TFTP-VALID-REMOTE-FILE-P*
;1;; The filename can be specified as a string, pathname. *

(DEFMACRO tftp-valid-remote-file-p (filename)
  `(OR (STRINGP ,filename) (PATHNAMEP ,filename))) 

;1;; TFTP-GET-FILENAME-STRING*

(DEFMACRO tftp-get-filename-string (path)
  `(SUBSEQ ,path (1+ (POSITION #\: ,path)) (LENGTH ,path))) 


;1;; TFTP-PACKET-TYPE-STRING *
;1;;*
;1;; Return a text string describing*
;1;; an opcode number.*
;1;;*

(DEFMACRO tftp-packet-type-string (opcode)
  `(SELECT ,opcode (tftp-error "error") (tftp-data "data") (tftp-ack "acknowledgement")
	   (tftp-rrq "read request") (tftp-wrq "write request") (otherwise "unknown"))) 


;1;; TFTP-DEFAULT-ERROR-STRING *
;1;; *
;1;; Return the default error string*
;1;; for a given error code.*
;1;;*

(DEFMACRO tftp-default-error-string (error-code)
  `(SELECT ,error-code (tftp-file-not-found "File not found.")
	   (tftp-access-violation "Access violation of a directory or of the file specified.")
	   (tftp-allocation-exceeded "Disk is full or the allocation is exceeded.")
	   (tftp-illegal-operation "An illegal TFTP operation.")
	   (tftp-unknown-transfer-id "Unknown transfer ID.")
	   (tftp-file-exists "File already exists.") (tftp-no-such-user "No such user.")
	   (otherwise "Unknown error, No text sent with packet."))) 

;1;; TFTP-CONNECTION*
;1;; The TFTP flavor definition*
;1;;*

(DEFFLAVOR tftp-connection
	   ((operation nil)
	    ;1; Read or write as seen from local file.*
	    (local-port 0)
	    (local-address 0)
	    (remote-port 0)
	    ;1; Port number of remote machine.*
	    (remote-address 0)
	    ;1; IP address of the remote machine.*
	    (local-filename "")
	    ;1; Local file as a String, Pathname, or Stream.*
	    (remote-filename "")
	    ;1; Remote file pathname as a string.*
	    (remote-hostname "")
	    (block-number 0)
	    ;1; Number of the last block acked or received.*
	    (mode *tftp-default-mode*)
	    ;1; Mail not allowed.*
	    (network-port nil)
	    ;1; This tftp connection's instance of a UDP port.*
	    (process nil)
	    ;1; The process, if any, this is part of.*
	    (STREAM ())
	    (current-packet nil)
	    (current-packet-length 0)
	    (network-packet nil)
	    (network-packet-length 0)
	    (xfasl-packet nil))
	   ()
  ;1; When instance variables are settable then*
  ;1; they are automatically gettable and inittable*
  :settable-instance-variables
  (:documentation "TFTP Connection flavor is instantiated for each TFTP request."))

(export 'ip:tftp 'ticl)