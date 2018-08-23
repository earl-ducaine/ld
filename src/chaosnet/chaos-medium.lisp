;;; -*- Mode:Common-Lisp; Package:CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

;;; Restricted rights legend

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chaos (packet-stream)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following methods may be used to operate on the packet-stream connection
;;; in a generic way, sending and receiving packets.
;;;   :get-next-pkt &optional no-hang-p
;;;      Get next input packet.  Returns packet, packet string.
;;;   :packet-present-p
;;;      Returns t if the an input packet is available, otherwise nil.
;;;   :return-input-pkt pkt
;;;      Release input packet.
;;;   :get-empty-pkt
;;;      Get an output packet.  Returns packet, packet string.
;;;   :send-pkt pkt nbytes
;;;      Send the current output packet, nbytes is the number of bytes of data.
;;;      Do not return the output packet pkt after calling :send-pkt.
;;;   :return-output-pkt pkt
;;;      Release output packet.
;;;   :close-connection &optional abort-reason-string
;;;      Close and optionally abort the connection.

(in-package :chaos)

(defun chaos-connect-function (host logical-contact-name connection
				  &key window-size (timeout (* 15 60))  ;; 07-15-88 dab increased timeout to 15s
				  connect-string (error t)
				  &allow-other-keys)
  (declare (ignore connection))
  (let* ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos))
	 (host-object (si:parse-host host nil nil))
	 (host-addresses (send host-object :network-address-list :chaos))
	 ;; since window size is specified in bytes and chaos window
	 ;; size is specified in packets assume that the average
	 ;; packet is 1/3 full and generate a number from there.
	 (chaos-window-size (if window-size
				(floor window-size (floor max-data-bytes-per-pkt 3))
				default-window-size)))
    (when contact-name
      (do ((address host-addresses (rest address)))
	  ((null address))
	(when net:*medium-notify*
	  (format t "~&trying ~a" (first address)))
	(condition-case-if (second address) (condition)
	    (let ((conn (condition-case-if (not error) (condition)
			    (funcall 'connect
				     (first address)
				     (if (or (null connect-string) (string-equal connect-string ""))
					 contact-name
					 (string-append contact-name #\space connect-string))
				     chaos-window-size timeout)
			  (sys:network-error condition))))
	      (return-from chaos-connect-function conn))
	  (net:network-error))))))

(defun connect (address &rest rest)
  (declare (ignore rest))
  (cond
    ((eql address :error)
     (error (make-condition 'net:network-error)))
    (t address)))

;; If we've supressed network errors (error nil) or there is at least
;; one aditional address don't generate an error, allowing us to try
;; each address, i.e. only generate an error when we've tried all
;; addresses and error suppression is off (error t).
(defun chaos-connect-function-alt ()
  (when net:*medium-notify*
    (format t "~&trying ~a" (first address)))

  (let (chaos-window-size
	timeout
	connect-string
	contact-name
	(error t)
	(address '(:error 1)))
    (let ((conn
	   (handler-case
	       (funcall 'connect
			(first address)
			(if (or (null connect-string) (string-equal connect-string ""))
			    contact-name
			    (concatenate 'string contact-name #\space connect-string))
			chaos-window-size
			timeout)
	     (net:network-error (condition)
	       (cond
		 ((or (second address) (not error))
		  ;; Suppress error
		  )
		 (t
		  ;; resigna the error
		  (error condition)))))))
	  conn)))

;; (handler-case (signal condition)
;;      (warning () "Lots of smoke, but no fire.")
;;      ((or arithmetic-error control-error cell-error stream-error)
;;         (condition)
;;        (format nil "~S looks especially bad." condition))
;;      (serious-condition (condition)
;;        (format nil "~S looks serious." condition))
;;      (condition () "Hardly worth mentioning.")))

(defun chaos-connect-function-alt (host logical-contact-name connection
				  &key window-size (timeout (* 15 60))  ;; 07-15-88 dab increased timeout to 15s
				  connect-string (error t)
				    &allow-other-keys)
  (let (condition)
    (condition-case-if (second address) ()
		       (let ((conn (condition-case-if (not error) (condition)
						    (funcall 'connect
							     (first address)
							     (if (or (null connect-string) (string-equal connect-string ""))
								 contact-name
								 (string-append contact-name #\space connect-string))
							     chaos-window-size timeout)
						    (sys:network-error condition))))
		       (return-from chaos-connect-function conn))
		     (sys:network-error))))))

;(defun CHAOS-CONNECTION-POSSIBLE-P-FUNCTION (host-a host-b)
;  "Return T if it is possible to create a connection from host-a to host-b"

;  ;; Need to also look for bridge connections
;  (let ((host-a-chaos-addresses (chaos-addresses host-a))
;	(host-b-chaos-addresses (chaos-addresses host-b)))
;    (dolist (host-a-address host-a-chaos-addresses)
;      (dolist (host-b-address host-b-chaos-addresses)
;	(when (eql (subnet host-a-address)
;		   (subnet host-b-address))
;	  (return-from chaos-connection-possible-p-function t))))))

;; For now just assume connection is possible.
(defun CHAOS-CONNECTION-POSSIBLE-P-FUNCTION (host-a &optional (host-b si:local-host) logical-contact-name)
  "Return T if it is possible to create a connection from host-a to host-b"
  (declare (special chaos:routing-table))
  (let* ((host-a (si:parse-host host-a t t))
	 (host-b (si:parse-host host-b t t))
	 host-a-addrs)
    (and host-a
	 host-b
	 (or (not logical-contact-name) (net:translate-logical-contact-name logical-contact-name :chaos))
	 (setf host-a-addrs (send host-a :chaos-addresses))
	 (send host-b :chaos-addresses)
	 (or (not (eq host-b si:local-host))
	     (dolist (addr host-a-addrs)
	       (when (aref chaos:routing-table (subnet addr))
		 ;; Found a routing table entry to get to it.
		 (return t)))))))

(defun CHAOS-ADDRESSES (host)
  "Return the list of chaos-addresses for host."
  (let ((host-obj (si:parse-host host t)))
    (when host-obj (send host-obj :send-if-handles :chaos-addresses))))

(defun CHAOS-ADD-SERVER-FUNCTION (logical-contact-name form &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos)))
    (when contact-name
      (add-initialization (format nil "~a" contact-name)
			  (if (symbolp form)
			      (list 'quote form)
			      form) nil 'server-alist))))

(defun CHAOS-DELETE-SERVER-FUNCTION (logical-contact-name &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos)))
    (when contact-name
      (delete-initialization (format nil "~a" contact-name) nil 'server-alist))))

(defun CHAOS-LISTEN-FUNCTION (logical-contact-name connection
				 &key window-size (error t)
				 &allow-other-keys)
  (declare (ignore connection))
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos))
	connection pkt str pos
	(chaos-window-size (if window-size
				(floor window-size (floor max-data-bytes-per-pkt 3))
				default-window-size)))

    (when (and contact-name (connection-pending contact-name))
      (condition-case-if (not error) (condition)
	  (unwind-protect
	      (progn
		(setf connection (listen contact-name chaos-window-size))
		;; get the rfc arguments before accepting
		(unless (errorp connection)
		  (setf (getf (conn-plist connection) 'chaos:rfc-arguments)
			(progn
			  (multiple-value-setq (pkt str) (send connection :get-next-pkt))
			  (subseq str (if (setf pos (position #\space str)) (1+ pos) (length str)))))
		  (accept connection))
		connection)
	    (when (and connection pkt) (send connection :return-input-pkt pkt)))
	(sys:network-error condition)))))


(defun CONNECTION-PENDING (contact-name)
  "Return T if there is an RFC pending for contact-name"
  (do ((pkt pending-rfc-pkts (pkt-link pkt)))
      ((null pkt))
    (when (string-equal (contact-name-from-rfc pkt) contact-name)
      (return t))))


(net:define-medium :CHAOS :connection-steps '((:network :chaos))
  :medium-desirability .8
  :implementation-desirability .8
  :superior-medium-list '(:packet-stream)
  :connect-function 'chaos-connect-function
  :listen-function 'chaos-listen-function
  :connection-possible-p-function 'chaos-connection-possible-p-function
  :add-server-function 'chaos-add-server-function
  :delete-server-function 'chaos-delete-server-function
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chaos-simple (datagram)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following methods may be used to operate on the datagram connection in a
;;; generic way.  Datagram connections are transaction request/response in nature,
;;; where the request and response are single packets.
;;;   :get-next-pkt &optional no-hang-p
;;;      Get next input packet.  Returns packet, packet string.
;;;   :packet-present-p
;;;      Returns t if the response packet is available, otherwise nil.
;;;   :return-input-pkt pkt
;;;      Release input packet.
;;;   :get-empty-pkt
;;;      Get an output packet.  Returns packet, packet string.
;;;   :send-pkt pkt nbytes
;;;      Send the current output packet, nbytes is the number of bytes of data.
;;;      Do not return the output packet pkt after calling :send-pkt.
;;;   :return-output-pkt pkt
;;;      Release output packet.
;;;   :return-connection
;;;      Return the connection.
;;;   :answer pkt nbytes
;;;      Same as send-pkt, but atomically return the connection.
;;;      Do not return the output packet pkt after calling :answer.
;;;   :reject &optional reason-string
;;;      Reject and return the connection.  Usually only called by listening servers.

(defun CHAOS-SIMPLE-CONNECT-FUNCTION (host logical-contact-name connection
					  &key window-size connect-string (error t)
					  &allow-other-keys)
  (declare (ignore connection))
  (let* ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos))
	 (host-object (si:parse-host host nil nil))
	 (address (first (send host-object :network-address-list :chaos)))

	 ;; Since window size is specified in bytes and chaos window size is specified in
	 ;; packets assume that the average packet is 1/3 full and generate a number from there.
	 (chaos-window-size (if window-size
				(floor window-size (floor max-data-bytes-per-pkt 3))
				default-window-size)))

    (when contact-name
      (condition-case-if (not error) (condition)
	  ;; this operation should not wait for establishment
	  (funcall 'open-connection
		   address
		   (if (or (null connect-string) (string-equal connect-string ""))
		       contact-name
		       (string-append contact-name #\space connect-string))
		   chaos-window-size)
	(sys:network-error condition)))))


(defun CHAOS-SIMPLE-LISTEN-FUNCTION (logical-contact-name connection
					 &key (error t)
					 &allow-other-keys)
  (declare (ignore connection))
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :chaos)))

    (when (and contact-name (connection-pending contact-name))
      (condition-case-if (not error) (condition)
	  (listen contact-name)
	(sys:network-error condition)))))


(net:define-medium :CHAOS-SIMPLE :connection-steps '((:network :chaos))
  :medium-desirability .75
  :implementation-desirability .75
  :superior-medium-list '(:datagram)
  :connect-function 'chaos-simple-connect-function
  :listen-function 'chaos-simple-listen-function
  :connection-possible-p-function 'chaos-connection-possible-p-function
  :add-server-function 'chaos-add-server-function
  :delete-server-function 'chaos-delete-server-function
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chaos-stream (byte-stream)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CHAOS-STREAM-CONNECT-FUNCTION (host logical-contact-name connection
					       &rest args &key stream-type
					       &allow-other-keys)
  (declare (ignore host logical-contact-name))
  (when connection
	(make-instance (net:find-stream-type-flavor stream-type :chaos-stream)
		       :connection connection)))


(defun CHAOS-STREAM-LISTEN-FUNCTION (logical-contact-name connection &rest args
							      &key stream-type
							      &allow-other-keys)
  (declare (ignore logical-contact-name))
  (when connection
	(make-instance (net:find-stream-type-flavor stream-type :chaos-stream)
		       :connection connection)))


(net:define-medium :CHAOS-STREAM :connection-steps '((:medium :chaos))
  :medium-desirability .85
  :implementation-desirability .85
  :superior-medium-list '(:byte-stream)
  :listen-function 'chaos-stream-listen-function
  :connect-function 'chaos-stream-connect-function)

;;;
;;; Stream types
;;;


(net:define-stream-type :ascii-translating-input-character-stream
			 'ascii-translating-input-character-stream  :chaos-stream)

(net:define-stream-type :ascii-translating-output-character-stream
			 'ascii-translating-output-character-stream :chaos-stream)

(net:define-stream-type :ascii-translating-character-stream
			 'ascii-translating-character-stream        :chaos-stream)

(net:define-stream-type :input-character-stream
			 'input-character-stream                    :chaos-stream)

(net:define-stream-type :output-character-stream
			 'output-character-stream                   :chaos-stream)

(net:define-stream-type :character-stream
			 'character-stream                          :chaos-stream)

(net:define-stream-type :input-binary-stream
			 'input-binary-stream                       :chaos-stream)

(net:define-stream-type :output-binary-stream
			 'output-binary-stream                      :chaos-stream)

(net:define-stream-type :binary-stream
			 'binary-stream                             :chaos-stream)
