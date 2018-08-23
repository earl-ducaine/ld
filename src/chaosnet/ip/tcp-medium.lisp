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

;1;;*
;1;; Auxiliary methods*
;1;;*
(defflavor 4tcp-datagram-connection*
	   ()
	   (character-stream)
  )

(defmethod 4(tcp-datagram-connection :pkt-string*) (pkt data-start data-end &optional (packet-end data-end))
  "2Returns a string overlaying the data portion of the packet.*"
  (make-array (- packet-end data-start) :element-type 'string-char :area *ip-area*
	      :leader-list `(,(- data-end data-start))
	      :displaced-to pkt :displaced-index-offset data-start))

(defmethod 4(tcp-datagram-connection :get-next-pkt*) (&optional no-hang-p)
  "2Get next input packet.  Returns packet, packet string.*"
  (when (send self :setup-next-input-buffer no-hang-p)
    (multiple-value-prog1
      (values si:stream-input-buffer
	      (send self :pkt-string si:stream-input-buffer si:stream-input-index si:stream-input-limit))
      (setf si:stream-input-index si:stream-input-limit))))

(defmethod 4(tcp-datagram-connection :packet-present-p*) ()
  "2Returns t if the response packet is available, otherwise nil.*"
  (plusp pending-octet-count))

(defmethod 4(tcp-datagram-connection :return-input-pkt*) (pkt)
  "2Release input packet.*"
  (send self :discard-input-buffer pkt))

(defmethod 4(tcp-datagram-connection :get-empty-pkt*) ()
  "2Get an output packet.  Returns packet, packet string.*"
  (send self :setup-new-output-buffer)
  (values si:stream-output-buffer
	  (send self :pkt-string si:stream-output-buffer si:stream-output-index
		si:stream-output-index si:stream-output-limit)))

(defmethod 4(tcp-datagram-connection :send-pkt*) (pkt nbytes)
  "2Send the current output packet, new-index indicates ending index of data.
Do not return the output packet pkt after calling :send-pkt.*"
  (incf si:stream-output-index nbytes)
  (send self :force-output)
  (send self :return-output-pkt pkt))

(defmethod 4(tcp-datagram-connection :return-connection*) ()
  "2Return the connection.*"
  (send self :close :abort))

(defmethod 4(tcp-datagram-connection :close-connection*) ()
  "2Return the connection.*"
  (send self :return-connection))

(defmethod 4(tcp-datagram-connection :reject*) (&optional reason-string)
  "2Reject and return the connection.  Usually only called by listening servers.*"
  (declare (ignore reason-string))
  (send self :close :abort))

(defmethod 4(tcp-datagram-connection :answer*) (pkt &optional new-index)
  "2Same as send-pkt, but atomically return the connection.
Do not return the output packet pkt after calling :answer.*"
  (send self :send-pkt pkt new-index)
  (send self :close))

(defmethod 4(tcp-datagram-connection :return-output-pkt*) (pkt)
  "2Release output packet.*"
  (send self :discard-output-buffer pkt))

(compile-flavor-methods tcp-datagram-connection)


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; TCP (packet-stream)*
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; The following methods may be used to operate on the packet-stream connection *
;1;; in a generic way, sending and receiving packets.*
;1;;   :get-next-pkt &optional no-hang-p*
;1;;      Get next input packet.  Returns packet, packet string.*
;1;;   :packet-present-p*
;1;;      Returns t if the an input packet is available, otherwise nil.*
;1;;   :return-input-pkt pkt*
;1;;      Release input packet.*
;1;;   :get-empty-pkt*
;1;;      Get an output packet.  Returns packet, packet string.*
;1;;   :send-pkt pkt nbytes*
;1;;      Send the current output packet, nbytes is the number of bytes of data.*
;1;;      Do not return the output packet pkt after calling :send-pkt.*
;1;;   :return-output-pkt pkt*
;1;;      Release output packet.*
;1;;   :close &optional abort-reason-string*
;1;;      Close and optionally abort the connection.  *
;1;;      Close does not return the connection.*
;1;;   :return-connection*
;1;;      Return the connection.*

(defun 4TCP-CONNECT-FUNCTION* (host logical-contact-name connection
			        &key connect-string (timeout 600) (error t)
				(timeout-after-open 18000) window-size
				&allow-other-keys)
  (let ((port (net:translate-logical-contact-name logical-contact-name :tcp))
	(addresses (send host :network-address-list :ip))
	(*tcp-stream-instantiator*
	  #'(lambda (connection timeout input-buffer-size number-of-input-buffers)
	      (make-instance 'tcp-datagram-connection
			     :connection connection :timeout timeout :input-buffer-size input-buffer-size
			     :number-of-input-buffers number-of-input-buffers)))
	pkt pkt-string)
    
    (when port
      (do ((address addresses (rest addresses)))
	  ((null address))
	(condition-case-if (second address) ()
	    (progn
	     (setf connection
		   ;1; this operation should not wait for establishment*
		   (open-stream (first address)
				:remote-port port
				:wait-for-establishment nil
				:direction :bidirectional
				:characters t
				:timeout (and timeout (floor timeout 60))
				:timeout-after-open (and timeout-after-open (floor timeout-after-open 60))
				:input-buffer-size (or window-size *default-tcp-stream-input-buffer-size*)
				:error error
				))
	     (unless (errorp connection)
	       (when (and connect-string (not (string-equal connect-string "")))
		 (multiple-value-setq (pkt pkt-string) (send connection :get-empty-pkt))
		 (copy-array-contents connect-string pkt-string)
		 (send connection :send-pkt pkt (length pkt-string))))
	     (return-from tcp-connect-function connection))
	  (sys:network-error nil))))))


(defun 4TCP-LISTEN-FUNCTION* (logical-contact-name connection
			      &key window-size (timeout 600) (error t)
			      (timeout-after-open 18000)
			      &allow-other-keys)
  (declare (ignore connection))
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :tcp))
	(*tcp-stream-instantiator*
	  #'(lambda (connection timeout input-buffer-size number-of-input-buffers)
	      (make-instance 'tcp-datagram-connection
			     :connection connection :timeout timeout :input-buffer-size input-buffer-size
			     :number-of-input-buffers number-of-input-buffers))))
    
    (when (and contact-name (send *tcp-handler* :connection-pending-p contact-name 0 0))
      (open-stream nil
		   :local-port contact-name
		   :direction :bidirectional
		   :characters t
		   :timeout (and timeout (floor timeout 60))
		   :timeout-after-open (and timeout-after-open (floor timeout-after-open 60))
		   :input-buffer-size (or window-size *default-tcp-stream-input-buffer-size*)
		   :error error))))


(defun 4TCP-CONNECTION-POSSIBLE-P-FUNCTION* (host-a &optional (host-b si:local-host) logical-contact-name)
  "2Return T if it is possible to create a connection from host-a to host-b*"
  (let* ((host-a (si:parse-host host-a t t))
	 (host-b (si:parse-host host-b t t)))
    (and host-a
	 host-b
	 (or (not logical-contact-name) (net:translate-logical-contact-name logical-contact-name :tcp))
	 (send host-a :ip-addresses)
	 (send host-b :ip-addresses))))


(defun 4TCP-ADD-SERVER-FUNCTION* (logical-contact-name form &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :tcp)))
    (when contact-name
      (add-server contact-name
		  (if (symbolp form)
		      (list 'quote form)
		      form)
		  '*tcp-server-alist*))))


(defun 4TCP-DELETE-SERVER-FUNCTION* (logical-contact-name &rest ignore)
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :tcp)))
    (when contact-name
      (delete-server contact-name '*tcp-server-alist*))))


(net:define-medium :TCP :connection-steps '((:network :ip))
  :medium-desirability .65
  :implementation-desirability .65
  :superior-medium-list '(:packet-stream)
  :connect-function 'tcp-connect-function
  :listen-function 'tcp-listen-function
  :connection-possible-p-function 'tcp-connection-possible-p-function
  :add-server-function 'tcp-add-server-function
  :delete-server-function 'tcp-delete-server-function
  )


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; TCP-simple (datagram)*
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
;1;;   :answer pkt nbytes*
;1;;      Same as send-pkt, but atomically return the connection.*
;1;;      Do not return the output packet pkt after calling :answer.*
;1;;   :reject &optional reason-string*
;1;;      Reject and return the connection.  Usually only called by listening servers.*

(defun 4TCP-SIMPLE-CONNECT-FUNCTION* (host logical-contact-name connection
				       &key connect-string (timeout 600) (error t)
				       (timeout-after-open 18000) window-size
				       &allow-other-keys)
  (let ((port (net:translate-logical-contact-name logical-contact-name :tcp))
	(address (first (send host :network-address-list :ip)))
	(*tcp-stream-instantiator*
	  #'(lambda (connection timeout input-buffer-size number-of-input-buffers)
	      (make-instance 'tcp-datagram-connection
		:connection connection :timeout timeout :input-buffer-size input-buffer-size
		:number-of-input-buffers number-of-input-buffers)))
	pkt pkt-string)
    
    (when port
      (setf connection
	    ;1; this operation should not wait for establishment*
	    (open-stream address
			 :remote-port port
			 :wait-for-establishment nil
			 :direction :bidirectional
			 :characters t
			 :timeout (and timeout (floor timeout 60))
			 :timeout-after-open (and timeout-after-open (floor timeout-after-open 60))
			 :input-buffer-size (or window-size *default-tcp-stream-input-buffer-size*)
			 :error error
			 ))
      (unless (errorp connection)
	(when (and connect-string (not (string-equal connect-string "")))
	  (multiple-value-setq (pkt pkt-string) (send connection :get-empty-pkt))
	  (copy-array-contents connect-string pkt-string)
	  (send connection :send-pkt pkt (length pkt-string))))
      connection)))

(net:define-medium :TCP-SIMPLE :connection-steps '((:network :ip))
  :medium-desirability .55
  :implementation-desirability .55
  :superior-medium-list '(:datagram)
  :connect-function 'tcp-simple-connect-function
  :listen-function 'tcp-listen-function
  :connection-possible-p-function 'tcp-connection-possible-p-function
  :add-server-function 'tcp-add-server-function
  :delete-server-function 'tcp-delete-server-function
  )


;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*
;1;; Tcp-stream*
;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*

(defun 4TCP-STREAM-CONNECT-FUNCTION* (host logical-contact-name connection
				       &key connect-string (stream-type :character-stream) (timeout 600)
				       (error t) (timeout-after-open 18000) window-size
				       &allow-other-keys)
  (let ((port (net:translate-logical-contact-name logical-contact-name :tcp))
	(stream-type (net:find-stream-type-flavor stream-type :tcp-stream))
	(addresses (send host :network-address-list :ip)))
    
    (when port
      (do ((address addresses (rest addresses)))
	  ((null address))
	(condition-case-if (second address) ()
	    (progn
	     (setf connection
		   (open-stream (first address)
				:remote-port port
				:direction (first stream-type)
				:characters (second stream-type)
				:timeout (and timeout (floor timeout 60))
				:timeout-after-open (and timeout-after-open (floor timeout-after-open 60))
				:input-buffer-size (or window-size *default-tcp-stream-input-buffer-size*)
				:error error
				))
	     (unless (errorp connection)
	       (when (and connect-string (not (string-equal connect-string "")))
		 (write-string connect-string connection)
		 (send connection :force-output)))
	     (return-from tcp-stream-connect-function connection))
	  (sys:network-error))))))


(defun 4TCP-STREAM-LISTEN-FUNCTION* (logical-contact-name connection
				      &key window-size (stream-type :character-stream) (timeout 600) (error t)
				      (timeout-after-open 18000)
				      &allow-other-keys)
  (declare (ignore connection))
  (let ((contact-name (net:translate-logical-contact-name logical-contact-name :tcp))
	(stream-type (net:find-stream-type-flavor stream-type :tcp-stream)))
    
    (when (and contact-name (send *tcp-handler* :connection-pending-p contact-name 0 0))
      (open-stream nil
		   :local-port contact-name
		   :direction (first stream-type)
		   :characters (second stream-type)
		   :timeout (and timeout (floor timeout 60))
		   :timeout-after-open (and timeout-after-open (floor timeout-after-open 60))
		   :input-buffer-size (or window-size *default-tcp-stream-input-buffer-size*)
		   :error error))))


(net:define-medium :TCP-STREAM :connection-steps '((:network :ip))
  :medium-desirability .75
  :implementation-desirability .75
  :superior-medium-list '(:byte-stream)
  :connect-function 'tcp-stream-connect-function
  :listen-function 'tcp-stream-listen-function
  :connection-possible-p-function 'tcp-connection-possible-p-function
  :add-server-function 'tcp-add-server-function
  :delete-server-function 'tcp-delete-server-function
  )

;1;;*
;1;; Stream types*
;1;;*

(net:define-stream-type :ascii-translating-input-character-stream '(:input :ascii) :tcp-stream)

(net:define-stream-type :ascii-translating-output-character-stream '(:output :ascii) :tcp-stream)

(net:define-stream-type :ascii-translating-character-stream '(:bidirectional :ascii) :tcp-stream)

(net:define-stream-type :input-character-stream '(:input t) :tcp-stream)

(net:define-stream-type :output-character-stream '(:output t) :tcp-stream)

(net:define-stream-type :character-stream '(:bidirectional t) :tcp-stream)

(net:define-stream-type :input-binary-stream '(:input nil) :tcp-stream)

(net:define-stream-type :output-binary-stream '(:output nil) :tcp-stream)

(net:define-stream-type :binary-stream '(:bidirectional nil) :tcp-stream)