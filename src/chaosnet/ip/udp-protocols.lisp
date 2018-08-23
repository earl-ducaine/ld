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

;1------*
;1 UDP Discard Protocol (RFC863)*
;
(add-server discard-port-number
	    '(PROCESS-RUN-FUNCTION '(:name "UDP Discard Server") 'udp-discard-server)
	    '*udp-server-alist*)


(DEFUN udp-discard-server (&aux port)
  "2UDP Discard Protocol server, discards all datagrams received.*"
  (UNWIND-PROTECT
      (PROGN
	(SETF port (SEND *udp-handler* :get-port discard-port-number))
	(LOOP (SEND port :discard-input-buffer (SEND port :next-input-buffer))))
    (WHEN port (SEND *udp-handler* :return-port port))))


(DEFUN udp-discard (host &optional (repetitions 1) (size 512)
		    &aux destination port output-buffer length)
  "2Send random sized datagrams  filled with ripple patterns to the discard server on the destination host.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default of 1).
SIZE Maximum octets of data sent (optional, default of 512).*"
  (SETF destination (FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
  (UNWIND-PROTECT
      (PROGN
	(SETF port (SEND *udp-handler* :get-port))
	(SETF output-buffer (MAKE-ARRAY size :element-type '(MOD 256) :area *ip-area*))
	(DOTIMES (i repetitions)
	  (SETF length (1+ (RANDOM (ARRAY-DIMENSION output-buffer 0))))
	  (DOTIMES (index length)
	    (SETF (AREF output-buffer index) (+ i index)))
	  (FORMAT t "~%Sending datagram ~d, size ~d octet~:p" (+ i 1) length)
	  (SEND port :transmit-data :destination-host destination :destination-port discard-port-number
		:data output-buffer :length length)))
    (WHEN port (SEND *udp-handler* :return-port port))))



;1------*
;1 UDP Echo Protocol (RFC862)*
;
(add-server echo-port-number
	    '(PROCESS-RUN-FUNCTION '(:name "UDP Echo Server") 'udp-echo-server)
	    '*udp-server-alist*)


(DEFUN udp-echo-server ()
  "2UDP Echo Protocol server, echoes all datagrams sent back to the sender.*"
  (LET (port)
    (UNWIND-PROTECT
	(PROGN
	  (SETF port (SEND *udp-handler* :get-port echo-port-number))
	  (LOOP
	    (MULTIPLE-VALUE-BIND (buffer beg end) (SEND port :next-input-buffer)
	      (IGNORE beg)
	      (SETF (udp-header-source-port buffer)
		    (PROG1
		      (udp-header-destination-port buffer)
		      (SETF (udp-header-destination-port buffer) (udp-header-source-port buffer))))
	      (SETF (udp-packet-source-address buffer)
		    (PROG1
		      (udp-packet-destination-address buffer)
		      (SETF (udp-packet-destination-address buffer) (udp-packet-source-address buffer))))
	      (SEND port :send-output-buffer buffer end)	      
	      (SEND port :discard-output-buffer buffer))))
      (WHEN port (SEND *udp-handler* :return-port port)))))


(DEFUN udp-echo (host &optional (repetitions 1) (size 512) (timeout 10)
		 &aux port destination output-buffer output-length
		 input-buffer input-length source-port source-address
		 start-time finish-time (total-bytes 0) (timed-out 0))
  "2Send random sized datagrams filled with ripple pattern to the echo server on the destination host.
Echo data returned from the destination host is checked for integrity.  Every odd numbered datagram is 
sent as urgent data, and is checked for return as urgent data.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default 1).
SIZE Maximum octets of echo data (optional, default 512).
TIMEOUT Timeout in seconds awaiting echo response (optional, default 10).*"
  (UNWIND-PROTECT
      (PROGN
	(SETF port (SEND *udp-handler* :get-port))
	(SETF (SEND port :receive-timeout) 10)
	(SETF output-buffer (MAKE-ARRAY size :element-type '(MOD 256) :area *ip-area*))
	(SETF input-buffer (MAKE-ARRAY size :element-type '(MOD 256) :area *ip-area*))
	(SETF destination
	      (FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
	(SETF start-time (TIME))
	(DOTIMES (i repetitions)
	  ;1; send datagram*
	  (SETF output-length (1+ (RANDOM size)))
	  (DOTIMES (index output-length)
	    (SETF (AREF output-buffer index) (+ i index)))
	  (FORMAT t "~%Sending datagram ~d, size ~d octet~:p " (+ i 1) output-length)
	  (INCF total-bytes output-length)
	  (SEND port :transmit-data :destination-host destination :destination-port echo-port-number
		:data output-buffer :length output-length)
	  ;1; receive repsonse*
	  (MULTIPLE-VALUE-SETQ (input-length source-port source-address)
	    (SEND port :receive-data input-buffer timeout))
	  ;1; validate response*
	  (COND ((NULL input-length)
		 (FORMAT t "  Echo packet timed out.")
		 (INCF timed-out))
		((NOT (EQL echo-port-number source-port))
		 (FORMAT t "  Echo received from other than echo servers port"))
		(t (FORMAT t "  Received echo response datagram ~d" (+ i 1))
		   (WHEN (NOT (EQL source-address destination))
		     (FORMAT t "~%    Echo received from other than requested host, ~
                                source address = ~a destination address = ~a"
			     source-address destination))
		   (WHEN (NOT (EQL input-length output-length))
		     (FERROR 'length-error
			     "Length of data sent, ~a, unequal to length of data received, ~a"
			     output-length input-length))
		   (DOTIMES (index input-length nil)
		     (WHEN (NOT (EQUAL (AREF input-buffer index) (AREF output-buffer index)))
		       (FERROR 'data-error "Data sent does not match data received"))))))
	;1; print completion message*
	(SETF finish-time (TIME))
	(FORMAT t "~%~%Packets Transmitted = ~d., Echoed = ~d., Timed-out = ~d., ~
                               Bytes Transmitted = ~d., Rate = ~d. BPS"
		repetitions (- repetitions timed-out) timed-out total-bytes
		(TRUNCATE (* 2 8 60 total-bytes) (TIME-DIFFERENCE finish-time start-time))))   
    (WHEN port (SEND *udp-handler* :return-port port))))



;1------*
;1 UDP Character Generator Protocol (RFC864)*
;
(add-server chargen-port-number
	    '(PROCESS-RUN-FUNCTION '(:name "UDP Character Generator Server") 'udp-character-generator-server)
	    '*udp-server-alist*)


(DEFUN udp-character-generator-server (&aux port input-buffer output-buffer output-length)
  "2UDP Character Generator Protocol server, returns a random sized datagram filled with random data
upon reception of any datagram.*"
  (UNWIND-PROTECT
      (PROGN
	(SETF port (SEND *udp-handler* :get-port chargen-port-number))
	(SETF output-buffer (MAKE-ARRAY 1024 :element-type '(MOD 256) :area *ip-area*))
	(LOOP
	  (SETF input-buffer (SEND port :next-input-buffer))	    
	  (SETF output-length (RANDOM (ARRAY-DIMENSION output-buffer 0)))
	  (DOTIMES (index output-length)
	    (SETF (AREF output-buffer index) (+ (RANDOM 94) 32)))
	  (SEND port :transmit-data :destination-host (udp-packet-source-address input-buffer)
		:destination-port (udp-header-source-port input-buffer)
		:data output-buffer :length output-length)
	  (SEND port :discard-input-buffer input-buffer)))
    (WHEN port (SEND *udp-handler* :return-port port))))


(DEFUN udp-character-generator (host &optional (repetitions 1) (size 512)
				&aux port buffer length destination)
  "2Sends datagrams of a random size filled with random data to the character generator server on the destination host.
Receives replies from the remote server.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default 1).
SIZE Maximum octets of data sent (optional, default 1472).*"
  (SETF destination (FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
  (UNWIND-PROTECT
      (PROGN
	(SETF port (SEND *udp-handler* :get-port))
	(SETF buffer (MAKE-ARRAY size :element-type '(MOD 256) :area *ip-area*))	
	(DOTIMES (index (ARRAY-DIMENSION buffer 0))
	  (SETF (AREF buffer index) (+ (RANDOM 94) 32)))
	(DOTIMES (i repetitions)
	  (SETF length (RANDOM (ARRAY-DIMENSION buffer 0)))
	  (FORMAT t "~%Sending datagram ~d, size ~d octet~:p" (+ i 1) length)
	  (SEND port :transmit-data :destination-host destination
		:destination-port chargen-port-number :data buffer :length length)
	  (MULTIPLE-VALUE-BIND (input-buffer start end) (SEND port :next-input-buffer)
	    (SEND port :discard-input-buffer input-buffer)
	    (FORMAT t "  Received response datagram ~d, size ~d octet~:p" (+ i 1) (- end start)))))
    (WHEN port (SEND *udp-handler* :return-port port)))) 
