;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;1;;                           RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;                                 MS 2151*

;1;; Copyright (C) 1986,1988 Texas Instruments Incorporated. All rights reserved.*
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP (datagram)*
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; The following methods may be used to operate on the datagram connection in a *
;1;; generic way.  Datagram connections are transaction request/response in nature, *
;1;; where the request and response are single packets.*
;1;;   :get-next-pkt &optional no-hang-p*
;1;;      Get next input packet.  Returns packet, packet string.*
;1;;   :packet-present-p*
;1;;      Returns t if the response packet is available, otherwise nil.*
;1;;   :return-input-pkt pkt*
;1;;      Release input packet.*
;1;;   :get-empty-pkt*
;1;;      Get an output packet.  Returns packet, packet string.*
;1;;   :send-pkt pkt nbytes*
;1;;      Send the current output packet, nbytes is the number of bytes of data.*
;1;;      Do not return the output packet pkt after calling :send-pkt.*
;1;;   :return-output-pkt pkt*
;1;;      Release output packet.*
;1;;   :return-connection*
;1;;      Return the connection.*
;1;;   :answer pkt &optional new-index*
;1;;      Same as send-pkt, but atomically return the connection.*
;1;;      Do not return the output packet pkt after calling :answer.*
;1;;   :reject &optional reason-string*
;1;;      Reject and return the connection.  Usually only called by listening servers.*

(defun 4UDP-CONNECT-FUNCTION* (host logical-contact-name connection
			       &key connect-string
			       &allow-other-keys
			       &aux pkt pkt-string)
  (declare (ignore connection))
  (let ((address (first (send host :network-address-list :ip)))
	(port-number (net:translate-logical-contact-name logical-contact-name :UDP))
	port)
    
    (setf port (send *udp-handler* :get-port 0 0 port-number address))
    (when connect-string
      (multiple-value-setq (pkt pkt-string) (send port :get-empty-pkt))
      (copy-array-contents connect-string pkt-string)
      (send port :send-pkt pkt (length connect-string)))
    port))


(defun 4UDP-LISTEN-FUNCTION* (logical-contact-name connection
			      &key (timeout 600) (error t)
			      &allow-other-keys
			      &aux port)
  (declare (ignore connection))
  (let ((port-number (net:translate-logical-contact-name logical-contact-name :UDP)))
    (when (and port-number (send *udp-handler* :connection-pending-p port-number))

      (setf port (send *udp-handler* :get-port port-number))
      (process-wait-with-timeout "3UDP Listen*" timeout port :packet-present-p)
      (cond ((send port :reset-occured-p)
	     (setf port (make-condition 'port-reset "3Port ~s has been reset*" port)))
	    ((not (send port :received-packet))
	     (setf port (make-condition 'host-not-responding-during-connection
					"3Host not responding during connection.*" port))))
      (if (and error (errorp port))
	  (signal-condition port)
	  port))))


(defun 4UDP-CONNECTION-POSSIBLE-P-FUNCTION* (host-a &optional (host-b si:local-host) logical-contact-name)
  "2Return T if it is possible to create a connection from host-a to host-b*"
  (let* ((host-a (si:parse-host host-a t t))
	 (host-b (si:parse-host host-b t t)))
    (and host-a
	 host-b
	 (or (not logical-contact-name) (net:translate-logical-contact-name logical-contact-name :udp))
	 (send host-a :ip-addresses)
	 (send host-b :ip-addresses))))


(defun 4UDP-ADD-SERVER-FUNCTION* (logical-contact-name form &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :UDP)))
    (when contact-name
      (add-server contact-name
		  (if (symbolp form)
		      (list 'quote form)
		      form)
		  '*udp-server-alist*))))


(defun 4UDP-DELETE-SERVER-FUNCTION* (logical-contact-name &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :UDP)))
    (when contact-name
      (delete-server contact-name '*udp-server-alist*))))


(net:define-medium :UDP :connection-steps '((:network :ip))
  :medium-desirability .40
  :implementation-desirability .40
  :superior-medium-list '(:datagram)
  :connect-function 'udp-connect-function
  :connection-possible-p-function 'udp-connection-possible-p-function
  :add-server-function 'udp-add-server-function
  :delete-server-function 'udp-delete-server-function
  :listen-function 'udp-listen-function
  )

;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; UDP Streams *
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*

(defun 4UDP-STREAM-CONNECT-FUNCTION* (host logical-contact-name connection &rest args
				      &key (stream-type :character-stream) (timeout 600)
				      &allow-other-keys)
  (let ((port-number (net:translate-logical-contact-name logical-contact-name :UDP))
	(stream-type (net:find-stream-type-flavor stream-type :udp-stream))
	(address (FIRST (send host :network-address-list :ip))))
    
    (setf connection (make-udp-stream connection
				      :host address
				      :remote-port port-number
				      :direction (first stream-type)
				      :characters (second stream-type)
				      :timeout (and timeout (floor timeout 60))))))


(defun 4UDP-STREAM-LISTEN-FUNCTION* (logical-contact-name connection
				     &key (stream-type :character-stream) (timeout 600)
				     &allow-other-keys)
  (ignore logical-contact-name)
  (let ((stream-type (net:find-stream-type-flavor stream-type :udp-stream)))

    (make-udp-stream connection
		     :remote-port (send connection :sender-port)
		     :remote-address (send connection :sender-address)
		     :direction (first stream-type)
		     :characters (second stream-type)
		     :timeout (and timeout (floor timeout 60)))))


(net:define-medium :UDP-STREAM :connection-steps '((:medium :udp))
  :medium-desirability .50
  :implementation-desirability .50
  :superior-medium-list '(:byte-stream)
  :connect-function 'udp-stream-connect-function
  :listen-function 'udp-stream-listen-function
  )

;1;;*
;1;; Stream types*
;1;;*

(net:define-stream-type :ascii-translating-input-character-stream '(:input :ascii) :udp-stream)

(net:define-stream-type :ascii-translating-output-character-stream '(:output :ascii) :udp-stream)

(net:define-stream-type :ascii-translating-character-stream '(:bidirectional :ascii) :udp-stream)

(net:define-stream-type :input-character-stream '(:input t) :udp-stream)

(net:define-stream-type :output-character-stream '(:output t) :udp-stream)

(net:define-stream-type :character-stream '(:bidirectional t) :udp-stream)

(net:define-stream-type :input-binary-stream '(:input nil) :udp-stream)

(net:define-stream-type :output-binary-stream '(:output nil) :udp-stream)

(net:define-stream-type :binary-stream '(:bidirectional nil) :udp-stream)

