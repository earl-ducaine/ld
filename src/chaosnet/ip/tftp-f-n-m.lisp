;;; -*- Mode: COMMON-LISP; Package: IP; Base: 10.; Fonts:(COURIER MEDFNB TR12BI) -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;			TEXAS INSTRUMENTS INCORPORATED.
;;;				 P.O. BOX 2909
;;;			      AUSTIN, TEXAS 78769
;;;				    MS 2151
;;;
;;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.
1;;;
;;; Methods used by TFTP-CONNECTION
;;;

;;; INITIALIZE  
;;; This is normally used by the initializer when a packet is used from
;;; the resource, but can be used to reinitialize any TFTP-CONNECTION instance.*

(DEFMETHOD (tftp-connection :initialize) (port rem-port rem-addr local-file remote-file
					  remote-host op m proc np npl)
  2"Initializes the TFTP connection when an object in the resource is being reused."*
  (SETF operation op)
  (SETF network-port port)
  (SETF local-port (IF port
		       (SEND port :port-number)))
  (SETF local-address (get-ip-address))
  (SETF remote-port rem-port)
  (SETF remote-address rem-addr)
  (SETF local-filename local-file)
  (SETF remote-filename remote-file)
  (SETF remote-hostname remote-host)
  (SETF block-number 0)
  (SETF mode m)
  (SETF process proc)
  (SETF stream nil)
  (SETF current-packet (ALLOCATE-RESOURCE 'tftp-packets))
  (SETF current-packet-length tftp-maximum-packet-size)
  (SETF network-packet np)
  (SETF network-packet-length npl)
  (SETF xfasl-packet ())) 


1;;; PRIMARY-TRANSFER 
;;;
;;; Attempt to create a TFTP connection by sending a file transfer request.
;;; If the request is approved continue the file transfer, otherwise signal 
;;; the error condition.
;;;*

(DEFMETHOD (tftp-connection :primary-transfer) (&aux result)
  2"Attempt to create a connection by sending a file transfer request.  If the request is approved continue
    the file transfer, otherwise indicate the reason for failure. "*
  (SEND self :make-connection)
  ;1; Request a file transfer*
  1;; < COMPLETE TRANSFER >*
  1;; Received an approval to continue the file transfer.  Complete the file transfer.*
  (WHEN (ERRORP (SETF result (SEND self :tftp-complete-transfer)))
    (SIGNAL-CONDITION result)))
1      
;;; MAKE-CONNECTION  
;;;
;;; Create the initial connection.  If the file transfer is not approved
;;; then signal the condition with FERROR.
;;; *

(DEFMETHOD (tftp-connection :make-connection) ()
  2"Establishes the initial connection.  "*
  1;; < MAKE REQUEST PACKET >*
  1;; Make the request packet and set the packet length.*
  (SETF current-packet-length (SEND self :make-request-packet))
  1;; <TFTP-GET-NEXT-PACKET >*
  1;; Send the request packet to the remote host until a valid packet is*
  1;; received or an error occurs (too many timeouts, resends or an unexpected packet.)*
  1;; Continue the file transfer only if a valid responce is received.*
  (SEND self :tftp-get-next-packet)) 


1;; LISTEN-ON-TFTP-PORT  
;;*

(DEFMETHOD (tftp-connection :listen-on-tftp-port) (&aux pkt-length rem-port
						   rem-addr pkt-filename pkt-mode pkt-opcode)
  2"Modification of the normal :RECEIVE.  Starts a secondary TFTP for a RRQ or WRQ.
   Picks the port number, makes sure that it is not in use, makes another instance
   of TFTP-CONNECTION and cranks off the secondary process."*
  (LOOP
    (PROGN
      (MULTIPLE-VALUE-SETQ (pkt-length rem-port rem-addr)
			   (SEND network-port :receive-data network-packet))
      (WHEN pkt-length
	1;; Log on as a file server if the machine is*
	1;; not in use.*
	
	(WHEN (OR (NULL user-id)
		  (EQUAL user-id ""))
	  (LOGIN "File Server" 'lm t)
	  (fs::print-server-login-exegesis))
	1;; Check the request packet.*
	
	(MULTIPLE-VALUE-SETQ (pkt-filename pkt-mode pkt-opcode)
			     (SEND self :check-request-packet rem-port rem-addr pkt-length))
	1;; Process the request.*
	
	(WHEN pkt-mode
	  (PROCESS-RUN-FUNCTION
	    tftp-process-name
	    'tftp-secondary
	    network-packet pkt-length pkt-opcode
	    pkt-filename pkt-mode rem-port rem-addr)
	  ;1; get new network-packet, old one passed to tftp-secondary*
	  (SETF network-packet (ALLOCATE-RESOURCE 'tftp-packets))
	  ))))) 


1;;; CHECK-REQUEST-PACKET
;;;
;;; Check if the network packet received is a valid request packet.
;;;*

(DEFMETHOD (tftp-connection :check-request-packet) (rem-port rem-addr pkt-length &aux valid-request)
  2"Check if the packet is a valid request packet."*
  (LET ((opcode (tftp-get-opcode network-packet)) (receive-types (LIST tftp-rrq tftp-wrq))
	(valid-opcodes (LIST tftp-rrq tftp-wrq tftp-data tftp-ack tftp-error)) pkt-filename
	pkt-mode)
    1;; Get filename and mode if a request packet is received*
    (WHEN (MEMBER opcode receive-types)
      (SETF pkt-filename (tftp-get-local-file-name network-packet)
	    pkt-mode (tftp-get-mode network-packet)))
    (SETF valid-request
	  (COND
	    ((EQUAL opcode tftp-error)
	     (IF *tftp-debug-mode*
		 (tv:notify () "TFTP SERVER: ~A"
			    (tftp-process-error-packet network-packet rem-addr)))
	     nil)			   1;Process error packets*
	    ((NOT (access-permitted rem-addr))
	     (SEND self :send-error-packet tftp-error-text
		   (tftp-security-violation-message rem-addr) rem-port rem-addr)
	     (IF notify-on-security-violations
		 (tv:notify () "~&TFTP:  The host address ~A was refused access." rem-addr))
	     nil)
	    1;; Does remote host have security clearance?*
	    ((OR (> pkt-length tftp-data-packet-size) (< pkt-length tftp-request-header-size))
	     (SEND self :send-error-packet tftp-error-text
		   (tftp-bad-packet-size-message pkt-length) rem-port rem-addr)
	     (INCF *tftp-bad-packet-size-count*) nil)	   1; Packet size check*
	    1;; It would be better to let the open signal the error*
	    1;; because it would be more specific.*
	    ((AND (EQUAL opcode tftp-rrq) (NULL (PROBE-FILE pkt-filename)))
	     (SEND self :send-error-packet tftp-file-not-found
		   (tftp-default-error-string tftp-file-not-found) rem-port rem-addr)
	     (INCF *tftp-file-not-found-count*) nil)	   1; File not available for reading.*
	    ((AND pkt-mode (NOT (tftp-mode-p pkt-mode)))
	     (SEND self :send-error-packet tftp-error-text (tftp-bad-mode-message pkt-mode)
		   rem-port rem-addr)
	     (INCF *tftp-received-bad-mode-count*) nil)	   1; Bad mode.*
	    ((MEMBER opcode (LIST tftp-data tftp-ack)) nil)
	    1; Ignore all extraneous data, or ack packets*
	    ((NOT (MEMBER opcode valid-opcodes))
	     (SEND self :send-error-packet tftp-illegal-operation (tftp-illop-message opcode)
		   rem-port rem-addr)
	     (INCF *tftp-illegal-operation-count*) nil)	   1; Illegal operation.*
	    (t t)			   ;1valid request packet*
	    ))
    (IF valid-request
	(VALUES pkt-filename pkt-mode opcode)))) 

#|
1;;; MOVED body to function TFTP-SERVER KCW 9-24-85*		

(DEFMETHOD (TFTP-CONNECTION :START-TFTP-PORT) ()
2  "Starts up the process to be held in the operation of the connection."*
  (SETF PROCESS
	(MAKE-PROCESS "TFTP Server" :WARM-BOOT-ACTION () :PRIORITY 25 :REGULAR-PDL-SIZE 5000
		      :SPECIAL-PDL-SIZE 5000))
  (SEND PROCESS :PRESET 'START-TFTP-LISTEN SELF)
  (PROCESS-RESET-AND-ENABLE PROCESS)) 
|#


(DEFMETHOD (tftp-connection :disable) ()
2  "Disables the tftp-listen process."*
  (WHEN (NOT (NULL process))
    (SEND process :kill))) 

1;;; SECONDARY-TRANSFER 
;;; 
;;; Choose whether to use the local file as the source or destination of the file
;;; transfer.
;;;*

(DEFMETHOD (tftp-connection :secondary-transfer) ()
  2"Choose the appropriate action for the file transfer.  If the local file is the destination
    of the file transfer, get the first data packet before completing the transfer."*
  (WHEN *tftp-debug-mode*
    (tv:notify () "SECONDARY TRANSFER: "))
  (SELECT operation
    (:read (SEND self :tftp-complete-transfer))
    (:write (SEND self :secondary-write)))) 

1;;; SECONDARY-WRITE
;;;*

(DEFMETHOD (tftp-connection :secondary-write) ()
  (SETF current-packet-length (SEND self :make-ack-packet))
  (WHEN (EQUAL (SEND self :tftp-get-next-packet) :packet)
    (SEND self :tftp-complete-transfer)
    (IF *tftp-debug-mode*
	(tv:notify () "TRANSFER COMPLETED")))) 



1;;; TFTP-COMPLETE-TRANSFER2 *
;;;
;;; Create a file stream for the local file.  The direction of* 1the file stream
;;; will be determined by the instance variable 'operation' with the value of
;;; 'READ or 'WRITE. Keyword 'READ indicates that the source file is the local
;;; file and keyword 'WRITE specifies that the local file as the destination file.
;;; The element-type will be determined by the transfer mode and the filename is
;;; specified by the instance variable 'local-filename'.
;;;*

(DEFMETHOD (tftp-connection :tftp-complete-transfer) ()
  2"A stream is created which can process the appropriate file io--determined by the
    direction of the transfer. The direction should be eithor :READ or :WRITE. 
    If the stream is created with no errors, then the transmission is processed.
    The success or failure of the transmission is returned to the caller."*
  (WHEN *tftp-debug-mode*
    (tv:notify () "COMPLETE TRANSFER: MODE = ~A " mode))
  ;; SETUP the fat array if doing an XFASL transfer
  (WHEN (EQUAL mode :xfasl)
    (SETF xfasl-packet
	  (allocate-xfasl-packet (IF (EQUAL operation :read)
				     current-packet
				     network-packet))))
  ;; Open the local file
  (WITH-OPEN-FILE-CASE
    (local-file-stream local-filename
		       :direction (IF (EQUAL operation :read)
				      :input	   1; read data from a file*
				      :output	   1; write data to a file*
				      )
		       :characters (EQUAL mode :netascii)
		       :byte-size (IF (EQUAL mode :xfasl)
				      16
				      8))  1;End of options*
    1;; <TFTP ERROR CODE 0.>*
    (fs:device-not-found
     (SEND self :send-error-packet tftp-error-text "The device was not found.")
     local-file-stream)
    (fs:file-open-for-output
     (SEND self :send-error-packet tftp-error-text "The file was open for output.")
     local-file-stream)
    1;; < TFTP ERROR CODE 1. >*
    (fs:file-not-found
     (SEND self :send-error-packet tftp-file-not-found "")
     local-file-stream)
    1;; < TFTP ERROR CODE 6. >*
    (fs:file-already-exists
     (SEND self :send-error-packet tftp-file-exists "")
     local-file-stream)
    1;; < TFTP ERROR CODE 2. >*
    (fs:directory-not-found
     (SEND self :send-error-packet tftp-access-violation "The directory was not found.")
     local-file-stream)
    (fs:access-error
     (SEND self :send-error-packet tftp-access-violation "")
     local-file-stream)
    (fs:not-available
     (SEND self :send-error-packet tftp-access-violation "File is not available.")
     local-file-stream)
    (fs:file-lookup-error
     (SEND self :send-error-packet tftp-access-violation "File lookup error.")
     local-file-stream)
    (fs:creation-failure
     (SEND self :send-error-packet tftp-access-violation "Could not create file. ")
     local-file-stream)
    1;; < TFTP ERROR CODE 3. >*
    (fs:no-more-room
     (SEND self :send-error-packet tftp-allocation-exceeded "")
     local-file-stream)
    (fs:file-error
     (SEND self :send-error-packet tftp-error-text "Error occurred while opening the file")
     local-file-stream)
    1;; < SUCCESSFUL OPEN >*
    1;; Complete the transmission.*
    (:no-error (WHEN *tftp-debug-mode*
		 (tv:notify () "SUCCESSFUL OPEN"))
	       (SETF stream local-file-stream)
	       (IF (EQUAL (SEND stream :direction) :input)
		   (SEND self :read)	   1; local file is the source file.*
		   (PROGN
		     (WHEN (EQUAL (SEND current-process :name) tftp-process-name)
		       (SEND stream :change-properties () :author tftp-process-name))
		     (SEND self :write)))))) 
	     

1;;; WRITE 
;;;
;;; Transfer a file with the local file as the destination file.* 1Write data to
;;; the local file stream.  Send the remote host an acknowledgement for every data
;;; packet successfully written. When the data-length is less than 512. bytes terminate
;;; the file transfer with success.
;;;*

(DEFMETHOD (tftp-connection :write) ()
  2"Write data to the local file stream. Send the remote host an acknowledgement for every
    data packet successfully written. When the data-length is less than 512. bytes or on failure,
    return the status of the transmission. "*
  (LOOP do
	(WHEN *tftp-debug-mode*
	  (tv:notify () "PACKET NUMBER = ~A  PACKET LENGTH = ~A"
		     (tftp-get-block-number network-packet) network-packet-length))
	1;; <WRITE DATA>*
	1;; Write the data received from the remote host to the stream*
	1;; There are three possible states: :DONE,2 *and :CONTINUE*
	do
	(CASE (tftp-write-data-to-stream network-packet network-packet-length)
	  (:continue
	   (INCF block-number)		   1; Set the connection's block number.*
	   1;; <ACKNOWLEDGE DATA PACKET>*
	   1;; Acknowledge the data packet. Get the next data packet*
	   1;; from the remote host or the current transmission status.*
	   (SETF current-packet-length (SEND self :make-ack-packet))
	   1;;<TFTP-GET-NEXT-PACKET>*
	   1;; Get the next valid packet from the remote host.*
	   1;; The possible states: *
	   (SEND self :tftp-get-next-packet))
	  (:done (INCF block-number)	   1; Set the connection's block number.*
		 (WHEN *tftp-debug-mode*
		   (tv:notify () "WRITE: SENDING LAST ACK PACKET"))
		 1;; <ACKNOWLEDGE LAST DATA PACKET>*
		 1;; Send the last ACK packet only once   *
		 1;; (as required) before quitting.*
		 (RETURN (SEND self :tftp-send-last-ack-packet)))))) 
1  
;;; TFTP-SEND-LAST-ACK-PACKET
;;;
;;; Send the last acknowledgement packet to the remote host.
;;; Dally a while to make sure that the packet was not lost
;;; on the network. 
;;; It is assumed that one timeout period is sufficient time
;;; to dally before quitting with success.
;;;*

(DEFMETHOD (tftp-connection :tftp-send-last-ack-packet) ()
  2"Send the last acknowledgement packet to the remote host.  Dally a while
    to make sure they received our packet."*
  (SETF current-packet-length (SEND self :make-ack-packet))
  (LOOP for resends = 0 then (INCF resends) with receive-types =
	(LIST tftp-data)
	1;; < TOO MANY RESENDS >*
	1;; Check if exceeded the maximum number of resends.*
	1;; Send the remote host an error packet and *
	1;; terminate the connection.*
	do
	(WHEN (>= resends tftp-number-of-resends)
	  (SEND self :send-error-packet tftp-error-text (tftp-too-many-resends))
	  (FERROR :terminate "~&LAST ACK PACKET: ~A ~&" (tftp-too-many-resends)))
	1;; < SEND and DALLY A WHILE >*
	1;; Dally a while before closing the connection. If no responce is received*
	1;; from the remote host* 1(status = :TIMEOUT), then the file transfer was successful.*
	do (SEND self :transmit-tftp-packet current-packet tftp-ack-packet-size) do
	(CASE (SEND self :receive receive-types tftp-dally-time)
	  (:packet
	   1;; < VALID PACKET >*
	   1;; Check the validity of the packet.* 1If the status is :PACKET then*
	   1;; resend the current packet. Status values are:*
	   1;; :VALID, :RESEND*
	   (WHEN (EQUAL (SEND self :valid-packet network-packet receive-types) :valid)
	     (WHEN *tftp-debug-mode*
	       (tv:notify () "LAST ACK PACKET: SUCCESS"))
	     (RETURN :success)))
	  (:timeout (WHEN *tftp-debug-mode*
		      (tv:notify () "LAST ACK PACKET: SUCCESS WITH TIMEOUT"))
		    (RETURN :success))))) 
1  
;;; READ 
;;;
;;; Process a transmission with the local file as the source file. 
;;; Read data from the local file in 512. blocks.
;;; Send the data to the remote host and receive acknowledgement.
;;; Resend any lost packets.
;;;*

(DEFMETHOD (tftp-connection :read) ()
  2"Transfer a local file to a remote system.  Read data as 512. byte data blocks from
   the local file stream.  Send the data to the remote host and receive acknowledgement.
   Resend any lost packets."*
  (LOOP
    1;; <READ DATA>*
    1;;* 1Get the data from the stream.*
    1;; TFTP-READ-DATA-FROM-STREAM returns following status:*
    1;; :CONTINUE :DONE :TERMINATE*
    do (SETF current-packet-length (tftp-read-data-from-stream current-packet)) do
    (WHEN *tftp-debug-mode*
      (tv:notify () "READ: packet-length ~A packet-number ~A" current-packet-length
		 (tftp-get-block-number current-packet)))
    do
    (COND
      1;; <DONE>*
      1;; If we are done, transmit the last data packet.  Dally a while before*
      1;; quitting in* 1case the remote host* 1did not receive the last data packet.*
      1;; Quit with success if no responce is received because the remote host*
      1;; is not required to acknowledge the last data packet.*
      ((AND (>= current-packet-length tftp-data-header-size)
	    (< current-packet-length tftp-maximum-packet-size))
       (WHEN *tftp-debug-mode*
	 (tv:notify () "~&READ: LAST DATA PACKET"))
       (RETURN (SEND self :tftp-send-last-data-packet)))
      1;; <TRANSMIT>*
      1;; Transmit the current package. Send the data packet until we receive*
      1;; a valid ACK packet from the* 1remote host.*
      ((= current-packet-length tftp-data-packet-size)
       (SEND self :make-data-packet)
       1;; <TFTP-GET-NEXT-PACKET>*
       1;; Get the next valid packet from the remote host. The possible returned*
       1;; status values:  :VALID :TERMINATE :ILLOP :ERROR :TIMEOUT :RESEND*
       (SEND self :tftp-get-next-packet))
      1;; oops*
      (t
       (FERROR :terminate "~&TFTP-READ-DATA-FROM-STREAM: ~A~&"
	       (tftp-bad-data-size-message current-packet-length)))))) 
1  
;;; TFTP-SEND-LAST-DATA-PACKET
;;;
;;; Send the last data packet to the remote host.* 1An acknowledgement packet
;;; is expected. If we timeout while waiting to receive the acknowlegement
;;; packet, assume that the packet was lost on the network and that the remote
;;; host did not dally before terminating the connection. 
;;; Therefore, a :TIMEOUT status will result in a successful transmission.
;;;*

(DEFMETHOD (tftp-connection :tftp-send-last-data-packet) ()
  2"Send the last data packet to the remote host.  If timeout on receive, assume the
    file transfer was successful."*
  (SEND self :make-data-packet)
  (LOOP
    for resends = 0 then (INCF resends) with receive-types =
    (LIST tftp-ack)
    1;; < TOO MANY RESENDS >*
    1;; Check if exceeded the maximum number of resends.*
    1;; Send the remote host an error packet and terminate.*
    do
    (WHEN (>= resends tftp-number-of-resends)
      (SEND self :send-error-packet tftp-error-text (tftp-too-many-resends))
      (FERROR :terminate "~&LAST DATA PACKET: ~A~&" (tftp-too-many-resends)))
    1;; < TFTP-GET-NETWORK-PACKET >*
    1;; Send the last data packet. Get expected acknowledgement packet*
    1;; from the remote host.*
    do
    (SEND self :tftp-get-network-packet receive-types tftp-number-timeouts-last-data)
    1;; < VALID-PACKET >*
    1;; Check the packet received from the network.  If the status*
    1;; is :RESEND, need to resend the current packet.*
    1;; If the packet is valid (status = :VALID), terminate the*
    1;; connection with success.*
    do
    (WHEN (EQUAL (SEND self :valid-packet network-packet receive-types) :valid)
      (WHEN *tftp-debug-mode*
	(tv:notify () "LAST DATA PACKET: SUCCESS"))
      (RETURN :success)))) 
	
1  
;;; TFTP-GET-NEXT-PACKET* 
1;;;
;;; Send the current packet. Get a responce from the network. 
;;; If the status is not valid, then resend the current packet (try again).
;;; If resend the maximum times allowed and no valid responce is received,
;;; then terminate the connection. Otherwise, return the status.
;;; *

(DEFMETHOD (tftp-connection :tftp-get-next-packet) ()
  2"Send the current packet. Get a responce from the network. 
    If the received packet is not valid, then resend the current packet (try again).
    If resend the maximum times allowed and no valid responce is received, then
    terminate the connection. Otherwise, return the status. "*
  (IF *tftp-debug-mode*
      (tv:notify () "TFTP-GET-NEXT-PACKET"))
  (LOOP for resends = 0 then (INCF resends) with receive-types =
	(SELECT (tftp-get-opcode current-packet) ((tftp-data tftp-wrq) (LIST tftp-ack))
		((tftp-ack tftp-rrq) (LIST tftp-data)))
	1;; < TOO MANY RESENDS >*
	1;; If exceeded the maximum number of resends, then terminate the connection.*
	do
	(WHEN (>= resends tftp-number-of-resends)
	  (SEND self :send-error-packet tftp-error-text (tftp-too-many-resends))
	  (FERROR :terminate "~&~A~&" (tftp-too-many-resends)))
	do
	(CASE (SEND self :tftp-get-network-packet receive-types)
					   1; Send and receive my next packet from the network.*
	  1;; < TOO MANY TIMEOUTS >*
	  1;; If we timeout the maximum number of times before receiving*
	  1;; any responce from the remote host, notify the remote host*
	  1;; of the error condition and terminate the connection.*
	  (:timeout (SEND self :send-error-packet tftp-error-text (tftp-too-many-timeouts))
		    (FERROR :terminate "~&~A~&" (tftp-too-many-timeouts)))
	  1;; < VALID PACKET >*
	  1;; If we receive a packet, then check the validity of the packet.*
	  1;; Return values are: :VALID and :SEND. If :SEND is returned then*
	  1;; need to resend the current packet.*
	  (:packet
	   (IF (EQUAL (SEND self :valid-packet network-packet receive-types) :valid)
	       (RETURN :packet)))))) 
	1    

;;; TFTP-GET-NETWORK-PACKET2   *
;;; Get a packet from the network port. If the :RECEIVE method returns a 
;;; :TIMEOUT status, attempt to send the current packet again. If we timeout
;;; the maximum number of times before receiving a responce, terminate
;;; the connection. Check any received packets and if ok return the status.  Otherwise,
;;; terminate the connection.*

(DEFMETHOD (tftp-connection :tftp-get-network-packet) (receive-pkt-types &optional (max-number-of-timeouts tftp-number-of-timeouts))
  2"Get my next packet from the network."*
  (LOOP for timeouts = 0 then (INCF timeouts) when (>= timeouts max-number-of-timeouts) return
	:timeout do
	(SEND self :transmit-tftp-packet current-packet current-packet-length)
	1;; < GET NEXT PACKET >*
	1;; Get the next packet form the network port.*
	1;; If :PACKET is returned, then return.  Otherwise, resend the*
	1;; current packet.*
	do
	(WHEN (EQUAL (SEND self :receive receive-pkt-types)
		     :packet)
	  (WHEN *tftp-debug-mode*
	    (tv:notify () "NETWORK: PACKET"))
	  (RETURN :packet)))) 


1;;; RECEIVE
;;;
;;; Receive a packet (UDP) from UDP.
;;; Do error checking on the received packet
;;; to make sure it is valid.  If the packet is
;;; valid return the data and data length to the calling procedure 
;;; else return error.
;;;*

(DEFMETHOD (tftp-connection :receive) (packet-type-list &optional (timeout tftp-timeout))
  2"Receives a packet from UDP.
   Returns the following:
     pkt        - The tftp packet or nil if there was an error or the handler timed out.
    pkt-length - The length of the tftp packet or nil if no packet is returned.
    Status      - The status of the receive.
    Message     - The message which explains the status."*
  (LET (pkt-length
	rem-port
	rem-addr
	status)
    (MULTIPLE-VALUE-SETQ (pkt-length rem-port rem-addr status)
			 (SEND network-port :receive-data network-packet timeout))
    1;; < TIMEOUT >*
    1;; Timed out while waiting to receive a packet from the remote host*
    1;; Return status :TIMEOUT.*
    (COND
      ((EQUAL status ()) (WHEN *tftp-debug-mode*
			   (tv:notify () "RECEIVE: TIMEOUT"))
			 :timeout)
      1;; < PACKET >*
      1;; Received a packet from the network. Check to see if it is valid.*
      1;; Return any valid packets. Otherwise, return status indicating the error.*
      (t (SETF network-packet-length pkt-length)
	 (WHEN *tftp-debug-mode*
	   (tv:notify () "RECEIVE: PACKET"))
	 (SEND self :check-received-packet packet-type-list rem-port rem-addr))))) 

1;;; CHECK-RECEIVED-PACKET
;;;
;;; Check the network packet. If a packet has bad address or port then send
;;; an error packet to the errant host and get my next network packet.*

(DEFMETHOD (tftp-connection :check-received-packet) (packet-type-list rem-port rem-addr)
  2"Check if the received packet is bad."*
  (COND
    1;; < ERROR PACKET >*
    1;; The received packet was an ERROR packet.*
    1;; Process the error packet and terminate the*
    1;; transfer without success.*
    ((EQUAL (tftp-get-opcode network-packet) tftp-error)
     (FERROR :terminate "~A" (tftp-process-error-packet network-packet remote-hostname)))
    1;; < NOT MY PACKET >*
    1;; Check remote port and remote address.*
    ((OR (NOT (SEND self :valid-remote-port-p rem-port))
	 (NOT (tftp-valid-remote-address-p rem-addr remote-address)))
     (INCF *tftp-unknown-transfer-id-count*)
     1;; < NOTIFY UNKNOWN HOST of their mistake >*
     1;; Send error packet to the unknown host and*
     1;; proceed listening to the network port*
     1;; as if nothing happened.*
     (SEND self :send-error-packet tftp-unknown-transfer-id
	   (FORMAT () "Packet sent to wrong port ~A or address ~A" rem-port rem-addr))
     (WHEN *tftp-debug-mode*
       (tv:notify ()
		  "CHECK RECEIVED PACKET:  Packet sent to wrong port ~A~
                             (my port ~A) or address ~A (my address ~A)."
		  rem-port remote-port rem-addr remote-address))
     1;; < LISTEN TO NETWORK >*
     1;; Listen for the my next network packet, returning any values returned.*
     (SEND self :receive packet-type-list))
    1;; < BAD PACKET >*
    1;; Received a TFTP packet yet it was not of the expected type.*
    ((NOT (MEMBER (tftp-get-opcode network-packet) packet-type-list))
     (SEND self :received-bad-packet (tftp-get-opcode network-packet)))
    1;; < BAD SIZE >*
    1;; Packet size is a bad size.  Send back an error packet*
    1;; and terminate the transfer. *
    ((OR (> network-packet-length tftp-maximum-packet-size)
	 (< network-packet-length tftp-minimum-header-size))
     (INCF *tftp-bad-packet-size-count*)
     (SEND self :send-error-packet tftp-error-text
	   (tftp-bad-packet-size-message network-packet-length))
     (FERROR :terminate "~&~A~&" (tftp-bad-packet-size-message network-packet-length)))
    1;; < GOOD PACKET >*
    (t :packet))) 


1;;; RECEIVED-BAD-PACKET
;;;
;;; Packet was of unexpected type or an illegal type.
;;;*

(DEFMETHOD (tftp-connection :received-bad-packet) (opcode)
  (COND
    1;; < UNEXPECTED PACKET TYPE >*
    1;; A packet was received that was not the expected packet type.*
    1;; Send an error message to the remote host and terminate the*
    1;; file transfer.*
    ((tftp-opcode-p opcode) (INCF *tftp-unexpected-packet-count*)
			    (SEND self :send-error-packet tftp-error-text
				  (tftp-bad-packet-type-message (tftp-packet-type-string opcode)))
			    (FERROR :terminate "~&~A~&" (tftp-bad-packet-type-message
							  (tftp-packet-type-string opcode))))
    1;; < ILLEGAL PACKET TYPE >*
    1;; The received packet was of an unknown type.*
    1;; Send an error packet to the remote host*
    1;; and terminate the transfer.*
    (t (INCF *tftp-illegal-operation-count*)
       (SEND self :send-error-packet tftp-illegal-operation (tftp-illop-message opcode))
       (FERROR :terminate "~&~A~&" (tftp-illop-message opcode))))) 

1;;; VALID-REMOTE-PORT-P
;;;
;;; Check an incoming packet's port for validity. If this is the first packet
;;; received then reset the remote-port to the packet's tid.*

(DEFMETHOD (tftp-connection :valid-remote-port-p) (pkt-tid)
  (COND
    1;; < ESTABLISHING A CONNECTION TID >*
    1;; If the remote port (TID) is the TFTP default port number, then we*
    1;; are establishing a connection and any port number between #xFF and*
    1;; #xFFFF is valid.*
    ((EQL remote-port tftp-port)
     (IF (AND (> pkt-tid 255) (<= pkt-tid 65535))
	 (SETF remote-port pkt-tid)))
    1;; < MY REMOTE HOST'S TID >*
    1;; Otherwise, the connection has been established and a valid tid exists.*
    1;; Therefore, the port number must equal to the remote host's tid.*
    ((EQUAL pkt-tid remote-port) t))) 

1;;; VALID-PACKET2  *
;;; Determine if the packet is valid* 1for the present state of the transfer.
;;; A DATA packet is valid if the DATA block number is one greater than the
;;; current block number. If the DATA block number is equal to the current
;;; block number, then we need to resend the current packet.  Otherwise, the
;;; DATA packet is  unexpected. An ACK packet is valid if the ACK block number
;;; is equal to the current block number. If the ACK block number is one less
;;; than the current block number, we need to resend the current packet.
;;; Otherwise, the ACK packet is unexpected. 
;;; Does not handle TFTP request packets or TFTP error packets.
;;;
;;; Variables
;;; pkt            should be a TFTP data or acknowledgement packet
;;; pkt-type-list  values of '(TFTP-DATA) or '(TFTP-ACK)
;;;*

(DEFMETHOD (tftp-connection :valid-packet) (pkt pkt-types-list)
  2"Determine if the packet is valid for the present state of the tranfer."*
  (LET* ((pkt-type (CAR pkt-types-list))
	 (pkt-block-number (tftp-get-block-number pkt))
	 (expected-block-number
	   (SELECT pkt-type (tftp-ack block-number) (tftp-data (1+ block-number)) (otherwise -1))))
    (COND
      1;; < VALID PACKET >*
      1;; A valid packet should contain the same block number as the *
      1;; expected block number.*
      ((EQL pkt-block-number expected-block-number) :valid)
      1;; < RESEND >*
      1;; They never received the current packet so we need to resend.*
      ((EQL (1- expected-block-number) pkt-block-number) :resend)
      1;; < UNEXPECTED PACKET >*
      1;; This is an unexpected packet* 1when* 1the block number of the packet*
      1;; is anything other than one less than the expected block number.*
      1;; Send an error packet to remote host then terminate the connection.*
      (t
       (SEND self :send-error-packet tftp-error-text
	     (tftp-unexpected-packet-message pkt-type pkt-block-number expected-block-number))
       (FERROR :terminate "~&~A~&"
	       (tftp-unexpected-packet-message pkt-type pkt-block-number expected-block-number)))))) 

1;; MAKE-REQUEST-PACKET2 *
;;
;; Make a packet which requests a tftp connection.
;; All request packets contain an opcode which defines the direction
;; of the transfer TFTP-RRQ (read a file from a remote host) 
;; or TFTP-WRQ (write a file to a remote host).
;;*

(DEFMETHOD (tftp-connection :make-request-packet) ()
  2"Create the request packet to start the tftp connection."*
  1;; < OPCODE >*
  1;; Write the opcode into the packet.*
  (tftp-set-packet-opcode current-packet (IF (EQUAL operation :read)
					     tftp-wrq
					     tftp-rrq))
  (LET* ((filename
	   (IF (STRINGP remote-filename)
	       remote-filename		   1;Unknown system type, use string*
	       (IF (STREAMP remote-filename)
		   (SEND (PATHNAME remote-filename) :string-for-host)
		   (SEND remote-filename :string-for-host))	   1;known system type, use pathname*
	       ))
	 (mode-offset
	   1;; < FILENAME >*
	   1;;* 1Write the filename into the packet. This is also the offset to the *
	   1;; location of the mode string in the packet.*
	   (tftp-set-packet-string current-packet filename tftp-request-header-size)))
    1;; < MODE >*
    1;;* 1Write the mode into the packet. This is also the packet length.*
    (tftp-set-packet-string current-packet (SYMBOL-NAME mode) mode-offset))) 


1;;; MAKE-ERROR-PACKET2 *
;;;
;;; Create an error packet.  The type of error is specified by the the error
;;; code number.  The error message is a text string for human consumption.
;;; The message is optional for all error codes execpt TFTP-ERRTXT which indicates 
;;; that a unique text string has been sent.
;;;*

(DEFMETHOD (tftp-connection :make-error-packet) (error-code message)
  2"Create the packet to send if an error occurs that should be reported.
   Returns the packet and the packet-length."*
  (tftp-set-packet-opcode current-packet tftp-error)	   1; Write the opcode into the packet*
  (tftp-set-packet-block-number current-packet error-code) 1; Write the error code into the packet*
  (tftp-set-packet-string current-packet message tftp-error-header-size)   1;* 1Write the message into the packet*
  ) 
    
1;; SEND-ERROR-PACKET 
;;
;; Method to send an error packet.
;;*

(DEFMETHOD (tftp-connection :send-error-packet) (ERROR message
						 &optional (rem-port remote-port) (rem-addr remote-address))
  2"Creates and sends an error packet to a remote host."*
  (SETF current-packet-length (SEND self :make-error-packet error message))
  (SEND self :transmit-tftp-packet current-packet current-packet-length rem-port rem-addr)) 

	
1;;; MAKE-DATA-PACKET 
;;;2 *
;;; Create a data packet for transmission to the remote host. The packet contains
;;; the data opcode and block number. The data is sent in blocks of 512. bytes.
;;;   *

(DEFMETHOD (tftp-connection :make-data-packet) (&optional pkt-length)
  2"Create and fill the data packet to be sent to UDP."*
  (tftp-set-packet-opcode current-packet tftp-data)
  ;1; Write the opcode into the packet*
  (tftp-set-packet-block-number current-packet (INCF block-number))
  ;1; Write the block number into the packet*
  pkt-length) 

1;;; MAKE-ACK-PACKET2  *
;;;
;;; Create an acknowledgement packet.  This packet contains the
;;; opcode TFTP-ACK and the block number of the data packet being
;;; acknowledged.
;;;*

(DEFMETHOD (tftp-connection :make-ack-packet) ()
  2"Create the request packet to start the connection."*
  (tftp-set-packet-opcode current-packet tftp-ack) 1; Write the opcode in to the packet*
  (tftp-set-packet-block-number current-packet block-number)	   1; Write the block number into the packet*
  tftp-ack-packet-size) 

1;;; TRANSMIT-TFTP-PACKET
;;;
;;; Send the packet to the network port (UDP is the default).  It is assumed* 1that
;;; the name or internet address of the remote host is known by this tftp connection
;;; and that the selected TFTP network port is listening for tftp packets.
;;;*

(DEFMETHOD (tftp-connection :transmit-tftp-packet) (pkt &optional (LENGTH (LENGTH pkt))
						    (rem-p remote-port) (rem-a remote-address))
  2"Sends a packet on down to UDP.  This assumes that the remote tftp port
1 * and host name (or internet address ) are already known. 
   The UDP header is added to the assembled TFTP packet."*
  (IF *tftp-debug-mode*
      (tv:notify () "SENDING UDP A PACKET packet-length ~A" length))
  (SEND network-port :transmit-data :destination-host rem-a :destination-port rem-p :data pkt
	:length length :source-host local-address :source-port local-port)) 


(COMPILE-FLAVOR-METHODS tftp-connection) 
