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

(DEFMETHOD (udp-port :initialize) ()
  (SETQ reset-occured-p nil
	received-packet nil
	receive-timeout nil
	sender-port 0
	sender-address 0
	receiver-address 0
	packets-received 0
	bytes-received 0
	packets-transmitted 0
	bytes-transmitted 0
	last-sent-time nil
	last-received-time nil
	last-buffered-time nil
	total-buffered-duration 0
	total-send-time 0
	total-receive-time 0
	checksum-errors 0
	pre-buffered-receives 0
	receive-overruns 0
	last-receiver-process ()
	last-sender-process ()))


(DEFMETHOD (udp-port :transmit-data) 
           (&key
	    destination-port
	    destination-host		   ;1 name, address, or host object*
	    (data nil)			   ;1 an array or string*
	    (length (IF data (IF (ARRAY-HAS-FILL-POINTER-P data)
				 (FILL-POINTER data)
				 (ARRAY-LENGTH data))
			0))		   ;1 number of bytes*
	    (source-port port-number)
	    source-host			   ;1 name, address, or host object*
	    &aux packet)
  "2Transmit UDP data to the specified destination socket.
:DESTINATION-PORT (REQUIRED)  Destination port number.
:DESTINATION-HOST (REQUIRED)  Name, address, or host object of destination.
:DATA (OPTIONAL)  ART-8B data array (default nil implies no data).
:LENGTH (OPTIONAL)  Length of data (default to fill pointer, or data array length if no fill pointer).
:SOURCE-PORT (OPTIONAL)  Number of the source port (default to value of port-number).
:SOURCE-HOST (OPTIONAL)  Source host address (default to (closest-local-address destination-host)).*"
  (LET* ((default-cons-area *ip-area*))
    (WHEN (NOT (INTEGERP destination-host))               
      (SETF destination-host (FIRST (closest-addresses-to-network (get-ip-addresses destination-host)))))
    (WHEN (NOT (INTEGERP source-host))               
      (SETF source-host (closest-local-address destination-host)))
    (WHEN (NOT (valid-udp-port-p source-port))
      (FERROR 'port-is-invalid
	      "The specified source port, ~a, is not a valid port number."
	      source-port))
    (WHEN (AND (INTEGERP destination-port) (NOT (valid-udp-port-p destination-port)))
      (FERROR 'port-is-invalid
	      "The specified destination port, ~a, is not a valid port number."
	      destination-port))
    (UNWIND-PROTECT
	(PROGN
	  (SETF packet (ALLOCATE-RESOURCE 'udp-packet *udp-maximum-packet-size*
					  :source-port source-port
					  :source-address source-host
					  :destination-port destination-port
					  :destination-address destination-host))
	  (WHEN (PLUSP length)
	    (WHEN (> length (- *udp-maximum-packet-size* udp-header-size))
	      (FERROR 'udp-error "Maximum UDP length exceeded"))
	    (copy-pkt data packet length 0 udp-header-size))
	  (SEND self :send-output-buffer packet (+ length udp-header-size)))
      (WHEN packet (DEALLOCATE-RESOURCE 'udp-packet packet)))))


(DEFMETHOD (udp-port :new-output-buffer) (&key (source-port port-number)
					  (source-address (IF (local-broadcast-address-p receiver-address)
							      (FIRST ip:my-addresses)
							      receiver-address))
					  (destination-port sender-port)
					  (destination-address sender-address))
  "2Returns three values: output buffer, starting index, ending index.*"
  (VALUES (ALLOCATE-RESOURCE 'udp-packet *udp-maximum-packet-size*
		     :source-port source-port :source-address source-address
		     :destination-port destination-port :destination-address destination-address)
	  udp-header-size
	  *udp-maximum-packet-size*))


(DEFMETHOD (udp-port :send-output-buffer) (packet &optional new-packet-index)
  "2This method expects the following to be initialized properly:
udp-header-source-port, udp-packet-source-address, udp-header-destination-port, udp-packet-destination-address.
NEW-PACKET-INDEX (OPTIONAL) If non-nil, the new value of the packet index.
  If nil (the default), udp-packet-index is assumed to be set up properly.*"
  (WHEN reset-occured-p (FERROR 'port-reset "Port ~s has been reset" self))
  (WHEN new-packet-index (SETF (udp-packet-index packet) new-packet-index))
  (SETF last-sender-process si:current-process)
  (SETF last-sent-time (TIME))
  (INCF packets-transmitted)
  (INCF bytes-transmitted (- (udp-packet-index packet) udp-header-size))
  (SETF (udp-header-length packet) (udp-packet-index packet))
  (SETF (udp-header-checksum packet) 0)
  (SETF (udp-header-checksum packet)
	(ip-ones-complement-checksum packet (udp-packet-index packet)
				     (udp-packet-destination-address packet)
				     (udp-packet-source-address packet)
				     (udp-header-length packet) *udp-protocol*))
  (WHEN *udp-debug-mode*
    (FORMAT t "~&UDP PACKET TRANSMITTED:")
    (print-udp-header packet))
  (SEND *ip-handler* :transmit packet :udp (udp-packet-destination-address packet)
	(udp-packet-index packet) (udp-packet-source-address packet)))


(DEFMETHOD (udp-port :discard-output-buffer) (packet)
  (DEALLOCATE-RESOURCE 'udp-packet packet))


(DEFMETHOD (udp-port :receive-data) (data-buffer &optional timeout
				     &aux packet start end)
  "2Receive UDP data.
DATA-BUFFER - buffer in which to copy received data.
TIMEOUT (Optional) - seconds to wait for data.  Default is nil (wait forever).
Return values are: bytes returned in DATA-BUFFER, source port, source address, data-returned-p.
  Data-returned-p is true when data has been returned in DATA-BUFFER.*"
  (SETF receive-timeout timeout)
  (UNWIND-PROTECT
      (PROGN
	(MULTIPLE-VALUE-SETQ (packet start end) (SEND self :next-input-buffer))
	(WHEN packet
	  (copy-pkt packet data-buffer (MIN (- end start) (ARRAY-TOTAL-SIZE data-buffer)) udp-header-size)
	  (VALUES (MIN (- end start) (ARRAY-TOTAL-SIZE data-buffer))
		  (udp-header-source-port packet)
		  (udp-packet-source-address packet)
		  t)))
    (WHEN packet (DEALLOCATE-RESOURCE 'udp-packet packet))))


(DEFMETHOD (udp-port :next-input-buffer) (&optional no-hang-p)
  "2Returns three values: input-buffer, starting index, ending index.*"
  (LET* ((default-cons-area *ip-area*))
    (COND (received-packet (INCF pre-buffered-receives))
	  ((NOT no-hang-p)
	   (PROCESS-WAIT-WITH-TIMEOUT who-state (WHEN receive-timeout (* receive-timeout 60.))
				      #'(lambda (port)
					  (OR (SEND port :reset-occured-p)
					      (SEND port :received-packet)))
				      self)))
    (COND (reset-occured-p
	   (FERROR 'port-reset "Port ~s has been reset" self))
	  (received-packet
	   (SETF last-received-time (TIME))
	   (INCF total-buffered-duration (- last-received-time last-buffered-time)) 
	   (INCF packets-received)
	   (INCF bytes-received (- (udp-packet-size received-packet) (udp-packet-index received-packet)))
	   (SETF last-receiver-process si:current-process)
	   (MULTIPLE-VALUE-PROG1
	     (VALUES received-packet
		     (udp-packet-index received-packet)
		     (udp-packet-size received-packet))
	     (SETF received-packet nil))))))

  
(DEFMETHOD (udp-port :discard-input-buffer) (buffer)
  (DEALLOCATE-RESOURCE 'udp-packet buffer))


;1;------*
;1; The following methods are part of the generic datagram interface.*
;1;*
(DEFMETHOD (udp-port :get-next-pkt) (&optional no-hang-p &aux pkt start end)
  "2This method is used as part of the generic datagram interface to return the next packet on input.*"
  (WHEN (MULTIPLE-VALUE-SETQ (pkt start end) (SEND self :next-input-buffer no-hang-p))
    (VALUES pkt (SEND self :pkt-string pkt start end))))

(DEFMETHOD (udp-port :packet-present-p) ()
  "2This method is used as part of the generic datagram interface to detect arrival of a packet.*"
  (OR reset-occured-p received-packet))


(DEFMETHOD (udp-port :return-input-pkt) (pkt)
  "2This method is used as part of the generic datagram interface to discard an input packet.*"
  (SEND self :discard-input-buffer pkt))


(DEFMETHOD (udp-port :get-empty-pkt) (&aux pkt start end)
  "2This method is used as part of the generic datagram interface to return an empty output packet.*"
  (MULTIPLE-VALUE-SETQ (pkt start end) (SEND self :new-output-buffer))
  (VALUES pkt (SEND self :pkt-string pkt start start end)))


(DEFMETHOD (udp-port :send-pkt) (pkt nbytes)
  "2This method is used as part of the generic datagram interface to transmit a packet.*"
  (SETF (udp-header-source-port pkt) port-number
	(udp-packet-source-address pkt) receiver-address
	(udp-header-destination-port pkt) sender-port
	(udp-packet-destination-address pkt) sender-address)
  (INCF (udp-packet-index pkt) nbytes)
  (SEND self :send-output-buffer pkt)
  (SEND self :return-output-pkt pkt))


(DEFMETHOD (udp-port :return-output-pkt) (pkt)
  "2This method is used as part of the generic datagram interface to discard an output packet.*"
  (SEND self :discard-output-buffer pkt))


(DEFMETHOD (udp-port :reject) (&optional reason-string)
  "2This method is used as part of the generic datagram interface to reject the connection.*"
  (DECLARE (IGNORE reason-string))
  (SEND *udp-handler* :return-port self))


(DEFMETHOD (udp-port :return-connection) ()
  "2This method is used as part of the generic datagram interface to return a connection.*"
  (SEND *udp-handler* :return-port self))


(DEFMETHOD (udp-port :answer) (pkt nbytes)
  "2This method is used as part of the generic datagram interface to transmit a packet and return the connection.*"
  (SEND self :send-pkt pkt nbytes)
  (SEND *udp-handler* :return-port self))


(DEFMETHOD (udp-port :pkt-string) (pkt data-start data-end &optional (packet-end data-end))
  "2This method is used as part of the generic datagram interface to return a string overlaying packet data.*"
  (MAKE-ARRAY (- packet-end data-start) :element-type 'STRING-CHAR :area *ip-area*
	      :leader-list `(,(- data-end data-start))
	      :displaced-to pkt :displaced-index-offset data-start))

;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP-GET-PORT-NUMBER generates a UDP unique, local, port number.  It uses a random*
;1;; number generator to calculate a number between 255 and 65536.  The number is random*
;1;; instead of sequential to reduce the possibility of erroneously accepting a package*
;1;; after a system reboot.  If sequential port numbers are used, the machine could be*
;1;; sending data on port 256, crash, come up and use port 256 again for other tranfers but*
;1;; actually be receiving data intended for port 256 before the crash.  UDP will check the*
;1;; number generated against its receive list to be sure it is not presently in use. *
;1;; There is a theoretical chance that if process A gets port X and at the time process B*
;1;; comes along and miraculously also gets port X A does not have a receive pending then*
;1;; both will be using the same port.  I am not going to lose any sleep over this*
;1;; possibility and I suggest you not worry about it either.*
;1;; *
;1;; The port number is returned as the value of the function.*
;1;;*

(DEFPARAMETER *udp-smallest-preset-port* 255) 

(DEFPARAMETER *max-port-seed* (- 65536 *udp-smallest-preset-port*)) 


(DEFUN udp-get-port-number ()
  (+ (RANDOM *max-port-seed*) *udp-smallest-preset-port*))


(COMPILE-FLAVOR-METHODS udp-port)