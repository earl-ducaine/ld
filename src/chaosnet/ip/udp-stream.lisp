;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(CPTFONT HL12B TR12BI) -*-

;1;;                        Restricted Rights Legend*

;1;;  Use, duplication, or disclosure by the Government is subject to*
;1;;  restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;;  Technical Data and Computer Software clause at 52.227-7013.*

;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;*
;1;;  Copyright (C) 1986, 1988 Texas Instruments Incorporated.  All rights reserved.*

;1;;-------------------------------------------------------------------------------------*
;1;;                       U D P    S T R E A M S*
;1;;*
;1;; ADVERTISED FUNCTIONS:*
;1;;   open-udp-stream*
;1;;*
;1;; ADVERTISED METHODS OF STREAMS:*
;1;;    All streams:       :close*
;1;;    Input streams:    :next-input-buffer, :discard-input-buffer*
;1;;    Output streams:  :new-output-buffer, :send-output-buffer, :discard-output-buffer*
;1;;*
;1;; Note:  Since UDP streams have no specific eof, we map a timeout into an eof indication*
;1;;*
;1;;-------------------------------------------------------------------------------------*

;1;------*
;1; This is included in all udp streams, input or output*
;1;*
(DEFFLAVOR udp-basic-stream
	   ((source-port 0)				;1; arguments to open-udp-stream*
	    (source-address 0)
	    (destination-port 0)
	    (destination-address 0)
	    (eof nil)				;1; timeout on receive occured, consider as eof*
	    port)				;1; udp-port object*
	   ()
  (:included-flavors stream)
  (:settable-instance-variables source-port source-address destination-port destination-address)
  (:inittable-instance-variables port))


(DEFMETHOD (udp-basic-stream :network-type) ()
  :udp)


(DEFMETHOD (udp-basic-stream :foreign-host) ()
  (si:get-host-from-address (IF (PLUSP destination-address)
				destination-address
				(SEND port :sender-address))
			    :ip))


(DEFMETHOD (udp-basic-stream :set-timeout) (new-timeout)
  (SETF (SEND port :receive-timeout) new-timeout))


(DEFMETHOD (udp-basic-stream :timeout) ()
  (SEND port :receive-timeout))


(DEFMETHOD (udp-basic-stream :close) (&optional abort-p)
  (IGNORE abort-p)
  (SEND *udp-handler* :return-port port))


(DEFMETHOD (udp-basic-stream :server-current-p) ()
  (NOT (SEND port :reset-occured-p)))


;1;------*

(DEFFLAVOR udp-basic-input-stream
	   ()
	   (udp-basic-stream)
  (:included-flavors si:basic-buffered-input-stream))

;1;------*

(DEFFLAVOR udp-basic-output-stream
	   ()
	   (udp-basic-stream)
  (:included-flavors si:basic-buffered-output-stream))


(DEFMETHOD (udp-basic-output-stream :before :close) (&optional abort-p)
  (UNLESS abort-p (SEND self :force-output)))


;1;------*

(DEFFLAVOR udp-character-input-stream-mixin
	   ()
	   (udp-basic-input-stream)
  (:included-flavors udp-basic-stream si:basic-buffered-input-stream)) 


(DEFMETHOD (udp-character-input-stream-mixin :element-type) ()
  'STRING-CHAR)


(DEFMETHOD (udp-character-input-stream-mixin :next-input-buffer) (no-hang-p)
  (UNLESS eof
    (MULTIPLE-VALUE-BIND (packet start end) (SEND port :next-input-buffer no-hang-p)
      (COND (packet
	     (SETF destination-port (udp-header-source-port packet))
	     (SETF destination-address (udp-packet-source-address packet)))
	    ((NOT no-hang-p)
	     (SETF eof t)))
      (VALUES packet start end))))


(DEFMETHOD (udp-character-input-stream-mixin :discard-input-buffer) (packet)
  (SEND port :discard-output-buffer packet))

;1;------*

(DEFFLAVOR udp-ascii-translating-input-stream-mixin
	   ((last-char-cr-p nil))
	   (udp-basic-input-stream)
  (:included-flavors udp-character-input-stream-mixin)) 


(DEFMETHOD (udp-ascii-translating-input-stream-mixin :around :next-input-buffer) (cont mt args ignore)
  (MULTIPLE-VALUE-BIND (buffer start end) (AROUND-METHOD-CONTINUE cont mt args)
    (WHEN buffer
      (DO ((i start (1+ i))
	   (j start (1+ j))
	   (old-last-char-cr-p (PROG1
				 last-char-cr-p
				 (SETF last-char-cr-p ())))
	   look)
	  ((EQL i end)
	   (SETF end j))
	(SETF (AREF buffer j)
	      (CASE (AREF buffer i)
		    (8 (CHAR-CODE #\Backspace))
		    (9 (CHAR-CODE #\Tab))
		    (10 (WHEN (AND (EQL i start) old-last-char-cr-p)
			  (DECF j))
			(CHAR-CODE #\Linefeed))
		    (12 (CHAR-CODE #\Page))
		    (13 (IF (AND (NOT (EQL (SETF look (1+ i)) end)) (EQL (AREF buffer look) 10))
			    (SETF i look)
			    (SETF last-char-cr-p (EQL look end)))
			(CHAR-CODE #\Newline))
		    (127 (CHAR-CODE #\Rubout))
		    (t (AREF buffer i))))))
    (VALUES buffer start end))) 

;1;------*
;1; binary streams have an effective character size of 2 octets*
;1;*

(DEFFLAVOR udp-binary-input-stream-mixin
	   ((saved-octet nil))
	   (udp-basic-input-stream)
  (:included-flavors udp-basic-stream si:basic-buffered-input-stream)) 


(DEFMETHOD (udp-binary-input-stream-mixin :element-type) ()
  '(unsigned-byte 8))


(DEFMETHOD (udp-binary-input-stream-mixin :next-input-buffer) (no-hang-p)
  (UNLESS eof
    (MULTIPLE-VALUE-BIND (packet start end) (SEND port :next-input-buffer no-hang-p)
      (COND (packet
	     (SETF destination-port (udp-header-source-port packet))
	     (SETF destination-address (udp-packet-source-address packet))
	     (WHEN saved-octet
	       (COPY-ARRAY-PORTION packet
				   start
				   end
				   packet
				   (1- start)
				   (1- end))
	       (DECF end)
	       (DECF start 2)
	       (SETF (AREF packet start) saved-octet)
	       (SETF saved-octet ()))
	     (WHEN (ODDP (- end start))
	       (DECF end)
	       (SETF saved-octet (AREF packet end)))
	     (VALUES
	       (MAKE-ARRAY (TRUNCATE (- end start) 2)
			   :displaced-to packet
			   :displaced-index-offset (TRUNCATE start 2)
			   :element-type '(unsigned-byte 16)
			   :area *ip-area*)
	       start
	       end))
	    ((NOT no-hang-p)
	     (SETF eof t)
	     nil))))) 


(DEFMETHOD (udp-binary-input-stream-mixin :discard-input-buffer) (displaced-buffer)
  (SEND port :discard-input-buffer (si:array-indirect-to displaced-buffer)))


;1;-----*

(DEFFLAVOR udp-character-output-stream-mixin
	   ()
	   (udp-basic-output-stream)
  (:included-flavors udp-basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (udp-character-output-stream-mixin :element-type) ()
  'STRING-CHAR) 


(DEFMETHOD (udp-character-output-stream-mixin :new-output-buffer) ()
  (SEND port :new-output-buffer
	:source-port source-port :source-address source-address
	:destination-port destination-port :destination-address destination-address))


(DEFMETHOD (udp-character-output-stream-mixin :send-output-buffer) (packet index)
  (SEND port :send-output-buffer packet index))


(DEFMETHOD (udp-character-output-stream-mixin :discard-output-buffer) (packet)
  (SEND port :discard-output-buffer packet))

;1------*

(DEFFLAVOR udp-ascii-translating-output-stream-mixin
	   ()
	   (udp-basic-output-stream)
  (:included-flavors udp-basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (udp-ascii-translating-output-stream-mixin :new-output-buffer) (&aux buffer)
  (SETF buffer (ALLOCATE-RESOURCE 'tcp-stream-ascii-buffer))
  (VALUES buffer 0 (ARRAY-DIMENSION buffer 0))) 


(DEFMETHOD (udp-ascii-translating-output-stream-mixin :send-output-buffer) (buffer index &aux packet j end)
   ;1; allocate new packet  *
  (MULTIPLE-VALUE-SETQ (packet j end)
    (SEND port :new-output-buffer
	  :source-port source-port :source-address source-address
	  :destination-port destination-port :destination-address destination-address))
  ;1; translate entire buffer*
  (DO ((i 0 (1+ i)))
      (nil)
    (WHEN (OR (= i index) (= j end))
      (SEND port :send-output-buffer packet j)
      (WHEN (= i index)
	(RETURN))
      ;1; allocate new packet*
      (MULTIPLE-VALUE-SETQ (packet j end)
	(SEND port :new-output-buffer
	      :source-port source-port :source-address source-address
	      :destination-port destination-port :destination-address destination-address)))
    ;1; translate one character*
    (CASE (AREF buffer i)
      (#.(CHAR-CODE #\Backspace) (SETF (AREF packet j) 8))
      (#.(CHAR-CODE #\Tab) (SETF (AREF packet j) 9))
      (#.(CHAR-CODE #\Linefeed) (SETF (AREF packet j) 10))
      (#.(CHAR-CODE #\Page) (SETF (AREF packet j) 12))
      (#.(CHAR-CODE #\Newline)
	 ;1; insure room for <cr>/<lf>*
	 (COND ((= (1+ j) end)
		(DECF end)
		(DECF i)
		(DECF j))
	       (t (SETF (AREF packet j) 13)
		  (SETF (AREF packet (INCF j)) 10))))
      (#.(CHAR-CODE #\Rubout) (SETF (AREF packet j) 127))
      (otherwise (SETF (AREF packet j) (AREF buffer i))))
    (INCF j))
  (DEALLOCATE-RESOURCE 'tcp-stream-ascii-buffer buffer))


(DEFMETHOD (udp-ascii-translating-output-stream-mixin :discard-output-buffer) (buffer)
  (DEALLOCATE-RESOURCE 'tcp-stream-ascii-buffer buffer)) 

;1;-----*
;1; binary streams have an effective character size of 2 octets*
;1;*

(DEFFLAVOR udp-binary-output-stream-mixin
	   ()
	   (udp-basic-output-stream)
  (:included-flavors udp-basic-stream si:basic-buffered-output-stream)) 


(DEFMETHOD (udp-binary-output-stream-mixin :element-type) ()
  '(unsigned-byte 8)) 


(DEFMETHOD (udp-binary-output-stream-mixin :new-output-buffer) ()
  (MULTIPLE-VALUE-BIND (packet start end) (SEND port :new-output-buffer)
    (VALUES (MAKE-ARRAY (TRUNCATE (- end start) 2)
			:displaced-to packet
			:displaced-index-offset (TRUNCATE start 2)
			:element-type '(unsigned-byte 16)
			:area *ip-area*)
	    (TRUNCATE start 2)
	    (TRUNCATE end 2)))) 


(DEFMETHOD (udp-binary-output-stream-mixin :send-output-buffer) (binary-packet index)
  (SEND port :send-output-buffer (si:array-indirect-to binary-packet) (* index 2))) 


(DEFMETHOD (udp-binary-output-stream-mixin :discard-output-buffer) (binary-packet)
  (SEND port :discard-output-buffer (si:array-indirect-to binary-packet)))


;1;------*
;1; Now the instantiable flavors*

(DEFFLAVOR udp-input-character-stream
	   ()
	   (udp-character-input-stream-mixin
	    udp-basic-input-stream
	    si:buffered-input-character-stream)) 


(DEFFLAVOR udp-output-character-stream
	   ()
	   (udp-character-output-stream-mixin
	    udp-basic-output-stream
	    si:buffered-output-character-stream)) 


(DEFFLAVOR udp-character-stream
	   ()
	   (udp-character-input-stream-mixin
	    udp-character-output-stream-mixin
	    udp-basic-input-stream
	    udp-basic-output-stream
	    si:buffered-character-stream)) 

;1; This is to make the EVAL server work*

(DEFMETHOD (udp-character-stream :beep) (&optional ignore)) 


(COMPILE-FLAVOR-METHODS udp-input-character-stream udp-output-character-stream udp-character-stream) 

;1;------*

(DEFFLAVOR udp-ascii-translating-input-character-stream
	   ()
	   (udp-ascii-translating-input-stream-mixin
	    udp-basic-input-stream
	    si:buffered-input-character-stream)) 


(DEFFLAVOR udp-ascii-translating-output-character-stream
	   ()
	   (udp-ascii-translating-output-stream-mixin
	    udp-basic-output-stream
	    si:buffered-output-character-stream)) 


(DEFFLAVOR udp-ascii-translating-character-stream
	   ()
	   (udp-ascii-translating-input-stream-mixin
	    udp-ascii-translating-output-stream-mixin
	    udp-basic-input-stream
	    udp-basic-output-stream
	    si:buffered-character-stream)) 


(COMPILE-FLAVOR-METHODS udp-ascii-translating-input-character-stream
   udp-ascii-translating-output-character-stream udp-ascii-translating-character-stream) 

;1;------*

(DEFFLAVOR udp-input-binary-stream
	   ()
	   (udp-binary-input-stream-mixin
	    udp-basic-input-stream
	    si:buffered-input-stream)) 


(DEFFLAVOR udp-output-binary-stream
	   ()
	   (binary-output-stream-mixin
	    udp-basic-output-stream
	    si:buffered-output-stream)) 


(DEFFLAVOR udp-binary-stream
	   ()
	   (udp-binary-input-stream-mixin
	    udp-binary-output-stream-mixin
	    udp-basic-input-stream
	    udp-basic-output-stream
	    si:buffered-stream)) 


(COMPILE-FLAVOR-METHODS udp-input-binary-stream udp-output-binary-stream udp-binary-stream) 


;1;------*

(DEFUN make-udp-stream (port-object &key host (remote-port 0) (remote-address 0)
			(direction :bidirectional) (characters t) (timeout 10)
			&aux host-name)
  "2Create a UDP stream object.
PORT-OBJECT The UDP port object.
:HOST (OPTIONAL) The host to 'connect' to.  May be a name, IP address, or host object.  If nil, the open is passive (listen).
:REMOTE-PORT (Optional) The remote UDP port on the specified host.  May be 0 (default) for passive opens.
:REMOTE-ADDRESS (used only if :host is nil)  IP address of the remote host. 
:DIRECTION (Optional) :Input, :output, or :bidirectional (default).
:CHARACTERS (Optional) Either t (characters, the default), nil (binary), or :ascii (ascii characters).
:TIMEOUT  (Optional) The timeout (in seconds) used in :next-input-buffer.
  Nil indicates no timeout.  Default is 10 seconds.*"
  (SETF (SEND port-object :receive-timeout) timeout)
  (WHEN host
    (SETF host (parse-ip-host-spec host))
    (SETF host-name (SEND host :short-name))	      
    (SETF remote-address (FIRST (closest-addresses-to-network (get-ip-addresses host)))))
  (CASE direction
    (:input
     (MAKE-INSTANCE
       (COND
	 ((EQ characters :ascii) 'udp-ascii-translating-input-character-stream)
	 (characters 'udp-input-character-stream)
	 (t 'udp-input-binary-stream))
       :port port-object
       :source-port (send port-object :port-number) :source-address (closest-local-address remote-address)
       :destination-port remote-port :destination-address remote-address))
    (:output
     (MAKE-INSTANCE
       (COND
	 ((EQ characters :ascii) 'udp-ascii-translating-output-character-stream)
	 (characters 'udp-output-character-stream)
	 (t 'udp-output-binary-stream))
       :port port-object
       :source-port (send port-object :port-number) :source-address (closest-local-address remote-address)
       :destination-port remote-port :destination-address remote-address))
    (:bidirectional
     (MAKE-INSTANCE
       (COND
	 ((EQ characters :ascii) 'udp-ascii-translating-character-stream)
	 (characters 'udp-character-stream)
	 (t 'udp-binary-stream))
       :port port-object
       :source-port (send port-object :port-number) :source-address (closest-local-address remote-address)
       :destination-port remote-port :destination-address remote-address)))) 
