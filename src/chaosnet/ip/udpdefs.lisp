;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

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

(DEFCONSTANT udp-header-size 8)


(DEFCONSTANT *udp-protocol* 17)


(DEFPARAMETER *udp-debug-mode* ())


(DEFVAR *udp-server-alist* nil "2Alist of (PORT-NUMBER FORM-TO-EVALUATE) for creating UDP servers.
   Entries are put on with ADD-INITIALIZATION, and removed with DELETE-INITIALIZATION.
   The form is evaluated in the background task when an incoming packet matches the port number
   and there is no port available for it.*")


(DEFPARAMETER *udp-server-packet-timeout* (* 30 60) "2Timeout on server response to packet in 1/60 second units.*")


(DEFVAR *udp-receive-list* () "2This is the list of receive ports currently on this machine.*")  


(DEFFLAVOR udp-error ()
	   (sys:network-error))

(DEFSIGNAL port-reset udp-error () "port has been reset must not be used.")


(DEFSIGNAL port-is-invalid udp-error () "port number is invalid")


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP-HEADER is the template for the UDP header information generated and placed in front of the*
;1;; user data on a send and found at the beginning of the data on receives to and from IP.  The*
;1;; data is handled in an ART-8B array.  Since the header fields are not eight bits long, some*
;1;; contortions have to be gone through with the field definitions.  The DEFSUBSTs that follow*
;1;; this DEFSTRUCT combine the fields to make it eisier to access the header fields.  The*
;1;; DEFSETFs and DEFUNs make it easier to update the header fields.*
;1;;*

(DEFSTRUCT (udp-header (:type :array) :size-symbol (:conc-name udp-header-) (:callable-constructors nil)
		       (:alterant alter-udp-header) (:predicate nil) (:copier nil))
	                                   ;1MSB - most significant byte*
					   ;1LSB - least significant byte*
  source-port-msb		           ;1 source port number*
  source-port-lsb
  destination-port-msb			   ;1 destination port number*
  destination-port-lsb
  length-msb				   ;1 length of the packet*
  length-lsb
  checksum-msb				   ;1 checksum*
  checksum-lsb) 

;1;;*
;1;; The following DEFSUBSTs combine the two octet fields of the header into one variable*
;1;; for ease of access.*
;1;;*

(DEFSUBST udp-header-source-port (array)
  (+ (ASH (udp-header-source-port-msb array) 8) (udp-header-source-port-lsb array))) 


(DEFSUBST udp-header-destination-port (array)
  (+ (ASH (udp-header-destination-port-msb array) 8) (udp-header-destination-port-lsb array))) 


(DEFSUBST udp-header-length (array)
  (+ (ASH (udp-header-length-msb array) 8) (udp-header-length-lsb array))) 


(DEFSUBST udp-header-checksum (array)
  (+ (ASH (udp-header-checksum-msb array) 8) (udp-header-checksum-lsb array))) 

;1;;*
;1;; The following DEFSETFs and corresponding DEFUNs permit the multiple octet fields of the*
;1;; header to be updated with a single SETF form.*
;1;;*

(DEFSETF udp-header-source-port set-udp-header-source-port) 


(DEFSETF udp-header-destination-port set-udp-header-destination-port) 


(DEFSETF udp-header-length set-udp-header-length) 


(DEFSETF udp-header-checksum set-udp-header-checksum) 

;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; The UDP-PORT object is used by the user program to interface with the UDP-HANDLER.*
;1;; *

(DEFFLAVOR udp-port
	   ((port-number 0)
	    (reset-occured-p nil)
	    (received-packet nil)
	    (receive-timeout nil)
	    (sender-port 0)
	    (sender-address 0)
	    (receiver-address 0)
	    (packets-received 0)
	    (bytes-received 0)
	    (packets-transmitted 0)
	    (bytes-transmitted 0)
	    (last-sent-time nil)
	    (last-received-time nil)
	    (last-buffered-time nil)
	    (total-buffered-duration 0)
	    (total-send-time 0)
	    (total-receive-time 0)
	    (checksum-errors 0)
	    (pre-buffered-receives 0)
	    (receive-overruns 0)
	    (last-receiver-process nil)
	    (last-sender-process nil)
	    (who-state ""))
	   ()
  :outside-accessible-instance-variables
  :settable-instance-variables) 


(DEFUN set-udp-header-source-port (array &rest value)
  (SETF value (CAR value))
  (SETF (udp-header-source-port-msb array) (ASH value -8))
  (SETF (udp-header-source-port-lsb array) value)) 


(DEFUN set-udp-header-destination-port (array &rest value)
  (SETF value (CAR value))
  (SETF (udp-header-destination-port-msb array) (ASH value -8))
  (SETF (udp-header-destination-port-lsb array) value)) 


(DEFUN set-udp-header-length (array &rest value)
  (SETF value (CAR value))
  (SETF (udp-header-length-msb array) (ASH value -8))
  (SETF (udp-header-length-lsb array) value)) 


(DEFUN set-udp-header-checksum (array &rest value)
  (SETF value (CAR value))
  (SETF (udp-header-checksum-msb array) (ASH value -8))
  (SETF (udp-header-checksum-lsb array) value)) 


(DEFRESOURCE udp-ports ()
  :constructor (MAKE-INSTANCE 'udp-port)
  :initializer (SEND object :initialize))


 
(DEFMACRO udp-packet-size (packet)
  "2Array leader accessor for slot 0.*"
  `(ARRAY-LEADER ,packet 0))


(DEFMACRO udp-packet-index (packet)
  "2Array leader accessor for slot 1.*"
  `(ARRAY-LEADER ,packet 1))


(DEFMACRO udp-packet-source-address (packet)
  "2Array leader accessor for slot 2.*"
  `(ARRAY-LEADER ,packet 2))


(DEFMACRO udp-packet-destination-address (packet)
  "2Array leader accessor for slot 3.*"
  `(ARRAY-LEADER ,packet 3))


(DEFMACRO udp-packet-time (packet)
  "2Array leader accessor for slot 4.*"
  `(ARRAY-LEADER ,packet 4))


(DEFPARAMETER *udp-maximum-packet-size* *max-data-bytes*)


(DEFRESOURCE udp-packet (length &key source-port source-address destination-port destination-address)
  :constructor (PROGN
		 source-address
		 (MAKE-ARRAY *udp-maximum-packet-size*
			     :element-type '(MOD 256)
			     :leader-list '(0 1 2 3 4)
			     :area *ip-area*))
  :matcher t
  :initializer (PROGN
		 (WHEN (> length *udp-maximum-packet-size*)
		   (DEALLOCATE-RESOURCE 'udp-packet object)
		   (FERROR 'udp-error "packet too large"))
		 (SETF (udp-packet-size object) length)
		 (SETF (udp-packet-index object) udp-header-size)
		 (SETF (udp-packet-source-address object) source-address)
		 (SETF (udp-packet-destination-address object) destination-address)
		 (SETF (udp-packet-time object) nil)
		 (DO ((i 0 (1+ i)))
		     ((= i udp-header-size))
		   (SETF (AREF object i) 0))
		 (WHEN source-port (SETF (udp-header-source-port object) source-port))
		 (WHEN destination-port (SETF (udp-header-destination-port object) destination-port))))