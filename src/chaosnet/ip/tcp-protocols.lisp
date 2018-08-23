;;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(MEDFNT HL12B TR12BI) -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986,1988 Texas Instruments Incorporated.  All rights reserved.*

;1;; ----------------------------------------------------------------------*
;1;; 02-24-88 DAB Added a timeout optiuonal argument to remote-eval. It would time*
;1;;              out if the form took more than 5 minutes to return a value. Who-calls is an example.*

(DEFCONSTANT discard-port-number 9 "2Port number of Discard Protocol*") 

(DEFCONSTANT echo-port-number 7 "2Port number of Echo Protocol*")

(DEFCONSTANT chargen-port-number 19 "2Port number of Character Generator Protocol*")

(DEFPARAMETER eval-port-number 250
  "2The choice of port number in the well-known range was arbitrary*")


;1------*
;1 TCP Discard Protocol (RFC863)*
;
(add-to-server-list discard-port-number 'tcp-discard-server "2TCP Discard Server*")

(DEFUN tcp-discard-server (&aux connection (buffered-octets 0) fin-received-p)
  "2TCP Discard Protocol server, discards all data received.*"
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection discard-port-number 0 0 :passive
					      ;1; buffer handler function*
					      #'(lambda (length ignore ignore)
						  (INCF buffered-octets length))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ()
						  (SETF fin-received-p t))
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function (none supplied)*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	;1; indicate we will send no data*
	(SEND connection :close)
	;1; receive data until FIN received*
	(DO ((buffer (MAKE-ARRAY 512 :element-type '(unsigned-byte 8)))
	     length)
	    (fin-received-p)
	  (IF (SETF length (SEND connection :receive (ARRAY-DIMENSION buffer 0)))
	      (INCF buffered-octets length)
	      (PROCESS-WAIT "TCP Receive" #'(lambda ()
					      (OR (EQ :closed (SEND connection :state))
						  fin-received-p
						  (PLUSP buffered-octets)))))
	  (WHEN (ZEROP buffered-octets)
	    (RETURN))
	  (SETF length (MIN (ARRAY-DIMENSION buffer 0) buffered-octets))
	  (SEND connection :get-multiple-octets buffer length)
	  (DECF buffered-octets length))
	;1; await close completion*
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection*
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 



(DEFUN tcp-discard (host &optional (repetitions 1) (size 512) &aux destination-address connection)
  "2Send random sized buffers filled with ripple patterns to the discard server on the destination host.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default of 1).
SIZE Maximum octets of data sent (optional, default of 512).*"
  (SETF destination-address
	(FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection 0 discard-port-number destination-address :active
					      ;1; buffer handler function*
					      #'(lambda (ignore ignore ignore))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ())
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function (none supplied)*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	;1; queue a receive for the fin segment*
	(SEND connection :receive 1)
	;1; send data*
	(DO ((i 1 (1+ i))
	     (output-buffer (MAKE-ARRAY size :element-type '(unsigned-byte 8)))
	     length)
	    ((> i repetitions))
	  (SETF length (1+ (RANDOM (ARRAY-DIMENSION output-buffer 0))))
	  (DOTIMES (index length)
	    (SETF (AREF output-buffer index) (+ i index)))
	  (FORMAT t "~%Sending buffer ~d, size ~d octet~:p" i length)
	  (SEND connection :send output-buffer length t ()))
	;1; insure we have left syn-sent state before closing the connection*
	(PROCESS-WAIT "TCP Connect"
		      #'(lambda (conn)
			  (OR (EQ :closed (SEND connection :state))
			      (NOT (EQ (SEND conn :state) :syn-sent))))
		      connection)
	;1; indicate data transmission complete and await close completion*
	(SEND connection :close)
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection*
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 


;1------*
;1 TCP Echo Protocol (RFC862)*
;
(add-to-server-list echo-port-number 'tcp-echo-server "2TCP Echo Server*")

(DEFUN tcp-echo-server (&aux connection (buffered-octets 0) buffer-pushed-p buffer-urgent-p fin-received-p)
  "2TCP Echo Protocol server, echoes all data received back to sender with exactly the same push and urgent characteristics.*"
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection echo-port-number 0 0 :passive
					      ;1; buffer handler function - set buffer attributes atomically*
					      #'(lambda (length push-p urgent-p)
						  (WITHOUT-INTERRUPTS
						    (INCF buffered-octets length)
						    (SETF buffer-pushed-p push-p)
						    (SETF buffer-urgent-p urgent-p)))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ()
						  (SETF fin-received-p t))
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function (none supplied)*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	;1; receive data and echo back exactly as received*
	(DO ((buffer (MAKE-ARRAY 8192 :element-type '(unsigned-byte 8)))
	     length)
	    (fin-received-p)
	  (MULTIPLE-VALUE-BIND (length push-p urgent-p)
	      (SEND connection :receive (ARRAY-DIMENSION buffer 0))
	    (COND
	      (LENGTH (INCF buffered-octets length)
		      (SETF buffer-pushed-p push-p
			    buffer-urgent-p urgent-p))
	      (t
	       (PROCESS-WAIT "TCP Receive" #'(lambda ()
					       (OR (EQ :closed (SEND connection :state))
						   fin-received-p
						   (PLUSP buffered-octets)))))))
	  (WHEN (PLUSP buffered-octets)
	    (SETF length (MIN (ARRAY-DIMENSION buffer 0) buffered-octets))
	    (SEND connection :get-multiple-octets buffer length)
	    (SEND connection :send buffer length buffer-pushed-p buffer-urgent-p)
	    (DECF buffered-octets length)))
	;1; indicate transmission complete and await close completion*
	(SEND connection :close)
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection*
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 



(DEFUN tcp-echo (host &optional (repetitions 1) (size 512)
		 &aux destination-address connection (buffered-octets 0)
		 receive-complete-p buffer-urgent-p fin-received-p)
  "2Send random sized buffers filled with ripple pattern to the echo server on the destination host.
Echo data returned from the destination host is checked for integrity.  Every odd numbered buffer is 
sent as urgent data, and is checked for return as urgent data.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default 1).
SIZE Maximum octets of echo data (optional, default 512).*"
  (SETF destination-address
	(FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection 0 echo-port-number destination-address :active
					      ;1; buffer handler function - set buffer attributes atomically*
					      #'(lambda (length ignore urgent-p)
						  (WITHOUT-INTERRUPTS
						    (SETF receive-complete-p t)
						    (INCF buffered-octets length)
						    (SETF buffer-urgent-p urgent-p)))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ()
						  (SETF fin-received-p t))
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	(DO ((i 1 (1+ i))
	     (input-buffer (MAKE-ARRAY size :element-type '(unsigned-byte 8)))
	     (output-buffer (MAKE-ARRAY size :element-type '(unsigned-byte 8)))
	     input-length
	     output-length)
	    ((OR (> i repetitions) fin-received-p))
	  ;1; send a buffer*
	  (SETF output-length (1+ (RANDOM size)))
	  (DOTIMES (index output-length)
	    (SETF (AREF output-buffer index) (+ i index)))
	  (FORMAT t "~%Sending buffer ~d, size ~d octet~:p " i output-length)
	  (SEND connection :send output-buffer output-length t (ODDP i))
	  ;1; receive a response*
	  (DO (warning-given-p)
	      (nil)
	    (SETF receive-complete-p nil)
	    (WHEN (OR fin-received-p (>= buffered-octets output-length)) (RETURN))
	    (MULTIPLE-VALUE-BIND (input-length ignore urgent-p)
		(SEND connection :receive (- output-length buffered-octets))
	      (COND
		(input-length
		 (INCF buffered-octets input-length)
		 (SETF buffer-urgent-p urgent-p))
		(t (PROCESS-WAIT "TCP Receive"  #'(lambda ()
						    (OR (EQ :closed (SEND connection :state))
							fin-received-p
							receive-complete-p))))))
	    (WHEN (AND (NOT warning-given-p)
		       (OR (AND (ODDP i) (NOT buffer-urgent-p))
			   (AND (EVENP i) buffer-urgent-p)))
	      (SETF warning-given-p t)
	      (FORMAT t
		      "~%    Warning: Output buffer ~:[does not contain~;contains~] urgent data, ~
                            input buffer ~:[was not~;was~] urgent data"
		      (ODDP i) buffer-urgent-p))
	    (WHEN (< buffered-octets output-length)
	      (FORMAT t "~%    Unexpected push received")))
	  (WHEN (ZEROP buffered-octets)
	    (RETURN))
	  ;1; validate response*
	  (FORMAT t "  Received echo response buffer ~d" i)
	  (SETF input-length (SEND connection :get-multiple-octets input-buffer output-length))
	  (UNLESS (EQL input-length output-length)
	    (FERROR 'length-error "Length of data received does not match length sent"))
	  (DOTIMES (index input-length nil)
	    (WHEN (NOT (EQUAL (AREF input-buffer index) (AREF output-buffer index)))
	      (FERROR 'data-error "Data sent does not match data received")))
	  (DECF buffered-octets input-length))
	(WHEN fin-received-p
	  (FERROR 'fin-received "connection prematurely closed by remote"))
	;1; indicate data transmission complete*
	(SEND connection :close)
	;1; queue a receive for the FIN segment and wait for close completion*
	(SEND connection :receive 1)
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection*
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 



(DEFUN tcp-stream-echo (host &optional (repetitions 1) (size 512) (characters t)
			&aux warning-given-p inchar outchar)
  "2Send random sized buffers filled with ripple pattern to the echo server on the destination host
via a TCP-STREAM connection.  Echo data returned from the destination host is checked for integrity.
Every odd numbered buffer is sent as urgent data, and is checked for return as urgent data.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS (Optional) Number of iterations (optional, default 1).
SIZE (Optional) Maximum octets of echo data (optional, default 512).
CHARACTERS (Optional) Either t (characters, the default), nil (binary), or :ascii (ascii characters).*"
  (WITH-OPEN-STREAM (tcp-stream (open-stream host
					     :remote-port echo-port-number
					     :characters characters
					     :timeout nil
					     :number-of-input-buffers 2
					     :input-buffer-size 8192))
    (DO ((i 1 (1+ i))
	 (input-buffer
	   (IF characters
	       (MAKE-ARRAY size :element-type '(unsigned-byte 8) :fill-pointer size)
	       (MAKE-ARRAY size :element-type '(unsigned-byte 16) :fill-pointer size)))
	 length)
	((> i repetitions))
      (SETF length (1+ (RANDOM size)))
      (SETF (FILL-POINTER input-buffer) 0)
      (CASE characters
	    (:ascii (FORMAT t "~&Sending buffer ~d, size ~d ascii characters" i length))
	    (nil (FORMAT t "~&Sending buffer ~d, size ~d binary characters" i length))
	    (otherwise (FORMAT t "~&Sending buffer ~d, size ~d octet~:p" i length)))
      (WHEN (ODDP i)
	(SEND tcp-stream :turn-on-urgent-mode))
      ;1; send a buffer*
      (DOTIMES (index length)
	(SEND tcp-stream :tyo
	      (IF characters
		  (IF (EQ characters :ascii)
		      (NTH (MOD (+ i index) 6)
			   (LIST #\Newline #\Page #\Linefeed #\Tab #\Backspace #\Rubout))
		      (MOD (+ i index) 256))
		  (+ i index))))
      (WHEN (SEND tcp-stream :send-urgent-mode-p)
	(SEND tcp-stream :turn-off-urgent-mode))
      (SEND tcp-stream :force-output)
      ;1; receive response*
      (SETF warning-given-p ())
      (DO ()
	  ((EQL (FILL-POINTER input-buffer) length)
	   (FORMAT t "  Received echo response buffer ~d" i))
	(VECTOR-PUSH (SEND tcp-stream :tyi :error) input-buffer)
	(WHEN (AND
		(OR
		  (AND (ODDP i)
		       (OR (NULL (SEND tcp-stream :urgent-input-index))
			   (> (SEND tcp-stream :stream-input-index) (SEND tcp-stream :urgent-input-index))))
		  (AND (EVENP i) (SEND tcp-stream :urgent-input-index)
		       (<= (SEND tcp-stream :stream-input-index) (SEND tcp-stream :urgent-input-index))))
		(NOT warning-given-p))
	  (SETF warning-given-p t)
	  (FORMAT t
		  "~&    Warning: Output buffer ~:[was not~;was~] urgent data, ~
                            input buffer ~:[was not~;was~] urgent data~%"
		  (ODDP i) (EVENP i))))
      (UNLESS (EQL (FILL-POINTER input-buffer) length)
	(FERROR 'length-error "Length of data received does not match length sent"))
      (DOTIMES (index length)
	(WHEN (NOT
		(EQUAL (SETF inchar (AREF input-buffer index))
		       (SETF outchar
			     (IF characters
				 (IF (EQ characters :ascii)
				     (NTH (MOD (+ i index) 6)
					  (LIST (CHAR-CODE #\Newline) (CHAR-CODE #\Page)
						(CHAR-CODE #\Linefeed) (CHAR-CODE #\Tab)
						(CHAR-CODE #\Backspace) (CHAR-CODE #\Rubout)))
				     (MOD (+ i index) 256))
				 (+ i index)))))
	  (FERROR 'data-error "Data sent: ~d, does not match data received: ~d, index: ~d"
		  outchar inchar index)))))) 
      

;1------*
;1 TCP Character Generator Protocol (RFC864)*
;
(add-to-server-list chargen-port-number 'tcp-character-generator-server "2TCP Character Generator Server*")

(DEFUN tcp-character-generator-server (&aux connection buffer-pushed-p (buffered-octets 0) fin-received-p)
  "2TCP Character Generator Protocol server, returns a random sized buffer filled with random data
upon reception of any buffer.*"
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection chargen-port-number 0 0 :passive
					      ;1; buffer handler function - set buffer attributes atomically*
					      #'(lambda (length push-p ignore)
						  (WITHOUT-INTERRUPTS
						    (INCF buffered-octets length)
						    (SETF buffer-pushed-p push-p)))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ()
						  (SETF fin-received-p t))
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	;1; receive data and return response when PSH is received*
	(DO ((input-buffer (MAKE-ARRAY 512 :element-type '(unsigned-byte 8)))
	     (output-buffer (MAKE-ARRAY 8192 :element-type '(unsigned-byte 8)))
	     length)
	    (fin-received-p)
	  (SETF buffer-pushed-p ())
	  (DO ()
	      (buffer-pushed-p)
	    (MULTIPLE-VALUE-BIND (LENGTH push-p ignore)
		(SEND connection :receive (ARRAY-DIMENSION input-buffer 0))
	      (COND
		(LENGTH (INCF buffered-octets length) (SETF buffer-pushed-p push-p))
		(t
		 (PROCESS-WAIT "TCP Receive" #'(lambda ()
						 (OR (EQ :closed (SEND connection :state))
						     fin-received-p
						     (PLUSP buffered-octets)))))))
	    (WHEN (ZEROP buffered-octets)
	      (RETURN))
	    (SETF length (MIN (ARRAY-DIMENSION input-buffer 0) buffered-octets))
	    (SEND connection :get-multiple-octets input-buffer length)
	    (DECF buffered-octets length))
	  (WHEN buffer-pushed-p
	    (SETF length (1+ (RANDOM (ARRAY-DIMENSION output-buffer 0))))
	    (DOTIMES (index length)
	      (SETF (AREF output-buffer index) (+ (RANDOM 94) 32)))
	    (SEND connection :send output-buffer length t ())))
	;1; indicate data transmission complete and wait for close completion*
	(SEND connection :close)
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection*
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 



(DEFUN tcp-character-generator (host &optional (repetitions 1) (size 512)
				&aux destination-address connection received-length
				buffer-pushed-p (buffered-octets 0) fin-received-p)
  "2Sends buffers of a random size filled with random data to the character generator server on the destination host.
Receives replies from the remote server.
HOST The destination host.  May be a symbol, Host object, IP address.
REPETITIONS Number of iterations (optional, default 1).
SIZE Maximum octets of data sent (optional, default 512).*"
  (SETF destination-address
	(FIRST (closest-addresses-to-network (get-ip-addresses (parse-ip-host-spec host)))))
  (UNWIND-PROTECT
      (PROGN
	(SETF connection (open-tcp-connection 0 chargen-port-number destination-address :active
					      ;1; buffer handler function - set buffer attributes atomically*
					      #'(lambda (length push-p ignore)
						  (WITHOUT-INTERRUPTS
						    (INCF buffered-octets length)
						    (INCF received-length length)
						    (SETF buffer-pushed-p push-p)))
					      ;1; urgent handler function*
					      #'(lambda ())
					      ;1; receive fin handler function*
					      #'(lambda ()
						  (SETF fin-received-p t))
					      ;1; close complete handler function*
					      #'(lambda ()
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection)))
					      ;1; user timeout handler function (none specified)*
					      ()
					      ;1; condition handler function*
					      #'(lambda (condition)
						  ;1; returning nil from this handler will decline to handle the condition*
						  (SETF connection
							(SEND *tcp-handler* :dummy-connection condition))
						  ())
					      :timeout nil))
	;1; send data and receive responses*
	(DO ((i 1 (1+ i))
	     (input-buffer (MAKE-ARRAY 512 :element-type '(unsigned-byte 8)))
	     (output-buffer (MAKE-ARRAY size :element-type '(unsigned-byte 8)))
	     send-length)
	    ((OR (> i repetitions) fin-received-p))
	  (SETF send-length (1+ (RANDOM (ARRAY-DIMENSION output-buffer 0))))
	  (DOTIMES (index send-length)
	    (SETF (AREF output-buffer index) (+ (RANDOM 94) 32)))
	  (FORMAT t "~%Sending buffer ~d, size ~d octet~:p" i send-length)
	  (SEND connection :send output-buffer send-length t ())
	  (SETF received-length 0
		buffer-pushed-p ())
	  (DO (LENGTH)
	      (buffer-pushed-p)
	    (MULTIPLE-VALUE-BIND (LENGTH push-p ignore)
		(SEND connection :receive (ARRAY-DIMENSION input-buffer 0))
	      (COND
		(LENGTH (INCF buffered-octets length) (INCF received-length length)
			(SETF buffer-pushed-p push-p))
		(t
		 (PROCESS-WAIT "TCP Receive" #'(lambda ()
						 (OR (EQ :closed (SEND connection :state))
						     fin-received-p
						     (PLUSP buffered-octets)))))))
	    (WHEN (ZEROP buffered-octets)
	      (RETURN))
	    (WHEN buffer-pushed-p
	      (FORMAT t "  Received response buffer ~d, size ~d octet~:p" i
		      received-length))
	    (SETF length (MIN (ARRAY-DIMENSION input-buffer 0) buffered-octets))
	    (SEND connection :get-multiple-octets input-buffer length)
	    (DECF buffered-octets length)))
	(WHEN fin-received-p
	  (FERROR 'fin-received "connection prematurely closed by remote"))
	;1; indicate data transmission complete*
	(SEND connection :close)
	;1; queue a receive for the FIN segment and wait for close completion*
	(SEND connection :receive 1)
	(PROCESS-WAIT "TCP Finish" #'(lambda ()
				       (EQ :closed (SEND connection :state)))))
    ;1; when thrown out, abort the connection    *
    (WHEN (AND connection (NOT (EQ :closed (SEND connection :state))))
      (SEND connection :abort)))) 



;1------*
;1 TCP Eval Server (this is not defined in a RFC document)*
;
(add-to-server-list eval-port-number 'tcp-eval-server "2TCP Eval Server*")

(DEFVAR eval-server-on t "2indicates how attempts to connect to eval servers are handled*")



(DEFUN eval-server-on (&optional (mode t))
  "2Allow remote connections to this machine's EVAL server.
   MODE can be:
   T              means always allow EVAL server requests,
   :NOTIFY        means allow them but notify the user,
   NIL            means never to allow them, and
   :NOT-LOGGED-IN means allow them when no one is logged in.*"
  (SETF eval-server-on mode))



(DEFUN tcp-eval-server ()
  "2Evaluate forms passed across a tcp stream and return their values.*"
  (CONDITION-CASE ()
      (WITH-OPEN-STREAM (stream (open-stream nil :local-port eval-port-number :characters :ascii
					     :timeout nil))
	(COND      
	  ((AND (EQ eval-server-on :not-logged-in) (NOT (MEMBER user-id '(nil "") :test #'EQUAL)))
	   (FORMAT stream "This machine is in use by ~A, try again later." user-id)
	   (RETURN-FROM tcp-eval-server nil))
	  ((NULL eval-server-on)
	   (RETURN-FROM tcp-eval-server nil))
	  ((EQ eval-server-on :notify)
	   (PROCESS-RUN-FUNCTION "Notify" 'tv:notify ()
				 "TCP Eval Server being used by ~A" (SEND stream :foreign-host))
	   (PROCESS-ALLOW-SCHEDULE)))
	(LET ((generic-server (MAKE-INSTANCE 'net:generic-peek-bs-server :stream stream)))
	  (SEND tv:who-line-file-state-sheet :add-server generic-server "TCP EVAL" si:current-process))
	(DO* ((*terminal-io* stream)	   ;1 Don't blow away machine on lossage*
	      (input (READ stream nil 'quit) (READ stream nil 'quit)))
	     ((AND (SYMBOLP input) (STRING-EQUAL (SYMBOL-NAME input) "Quit"))
	      nil)
	  (CATCH-ERROR
	    (DOLIST (value (MULTIPLE-VALUE-LIST (EVAL input)))
	      (FORMAT stream "~s~%" value))
	    t)
	  (WRITE-CHAR #\End stream)
	  (SEND stream :force-output)))
    (sys:network-error)))



(DEFUN remote-eval (host &optional timeout &aux form)
  "2Evaluate forms on remote host.
HOST - May be a symbol, Host object, IP address.
An error occurs if no response has occured from HOST for TIMEOUT (seconds). The default is 300. *"
  (SETF host (parse-ip-host-spec host))
  (WITH-OPEN-STREAM (tcp-stream (open-stream host :remote-port eval-port-number :characters :ascii))
    (FORMAT t "~%~%Eval Service ~A.~
               ~%Type in forms to be evaluated on remote host.  To quit press ABORT.~%" host)
    (when timeout (send tcp-stream :set-timeout timeout))  ;102-24-88 DAB*
    (LOOP
      (SETQ form (READ))
      (TERPRI)
      (FORMAT tcp-stream "~s~%" form)
      (SEND tcp-stream :force-output)
      (WHEN (AND (SYMBOLP form)
		 (STRING-EQUAL (SYMBOL-NAME form) "Quit"))
	(RETURN ()))
      (DO ((CHAR (TYI tcp-stream) (TYI tcp-stream)))
	  ((CHAR-EQUAL char #\end))
	(SEND *standard-output* :tyo char)))))