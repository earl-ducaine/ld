;;; -*- Mode:COMMON-LISP; Package: FILE-SYSTEM; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;

;;; Copyright (C) 1980, Massachusetts Institute of Technology
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.

;;; 04-06-88 DAB    fixes the problem of version limiting erroring off during close when the
;;;                 file is delete protected.
;;  09-10-87 DAB    Added code to support (OPEN "...." :Direction :IO) in QFILE.

;;;
;;; Qfile streams
;;;

(defflavor file-stream-mixin (host-unit status) (si:property-list-mixin si:file-stream-mixin)
   (:initable-instance-variables host-unit)) 


(defmethod (file-stream-mixin :qfaslp) ()
  (getf si:property-list :qfaslp)) 


(defmethod (file-stream-mixin :truename) ()
  (getf si:property-list :truename)) 


(defmethod (file-stream-mixin :length) ()
  (getf si:property-list :length)) 


(defmethod (file-stream-mixin :properties) (&optional (error-p t))
  (if (eq status :closed)
    (properties-chaos :file (send self :truename) error-p)
    (properties-chaos :stream self error-p))) 

;;; Flavors that really have an open connection
;;; STATUS is one of
;;;  :OPEN - a file is currently open on this channel
;;;  :CLOSED - no file is open, but the channel exists
;;;  :EOF - a file is open, but is at its end (no more data available).
;;;  :SYNC-MARKED - a mark that was requested has been received
;;;  :ASYNC-MARKED - an asynchronous (error) mark has been received

(defflavor file-data-stream-mixin ((status ':open)
				   data-connection
				   file-handle
				   chaos:connection)
	   (file-stream-mixin)
  (:included-flavors si:file-data-stream-mixin)
  (:settable-instance-variables status)
  (:gettable-instance-variables file-handle)
  (:initable-instance-variables data-connection)) 


(defflavor file-input-stream-mixin (chaos:input-packet)
	   (file-data-stream-mixin)
   (:included-flavors si:input-file-stream-mixin)) 


(defflavor file-output-stream-mixin ()
	   (file-data-stream-mixin)
   (:required-methods :send-pkt-buffer)
   (:included-flavors si:output-file-stream-mixin)) 


(defflavor file-iO-stream-mixin (chaos:output-packet
				 direct-file-id
				 (real-output-pointer-base 0))
	   (file-output-stream-mixin)
   (:included-flavors si:IO-file-stream-mixin)
   (:required-methods :send-pkt-buffer))

(defmethod (file-io-stream-mixin :before :init) (ignore)
  (setq direct-file-id (getf sys:property-list :direct-file-id)))

(defmethod (file-data-stream-mixin :before :init) (ignore)
  (let ((direction (send self :direction))
	(host (host-unit-host host-unit)))
    (send host :register-stream self)
    (if (eq direction :io)        ; 09-02-87 DAB IO Support
	(progn
	  (setf (data-stream data-connection :input) self)
	  (setf (data-stream data-connection :output) self)
	  (setf file-handle nil))
	(setf (data-stream data-connection direction) self)
	(setf file-handle (data-handle data-connection direction)))
    (setf chaos:connection (data-connection data-connection))))

;;; Stream version of host unit :COMMAND, supplies file handle itself.
;;; MARK-P is just T or NIL.

(defmethod (file-data-stream-mixin :command) (mark-p com &rest strings &aux pkt success string)
  (declare (values string success))
  (multiple-value-setq (pkt success string)
    (apply host-unit :command mark-p self () com strings))
  (let ((default-cons-area background-cons-area))
    (setq string (string-append string)))
  (if pkt
    (chaos:return-pkt pkt))
  (values string success)) 


(defmethod (file-data-stream-mixin :safe-to-use-p) ()
  (if (neq (chaos:state (host-unit-control-connection host-unit)) 'chaos:open-state)
    (setq status :closed))
  (eq status :open)) 


(defmethod (file-data-stream-mixin :close) (&optional abortp)
  (let ((host (host-unit-host host-unit)))
    (cond
      ((eq status :closed) nil)
      ((or (null (host-unit-control-connection host-unit))
	   (neq (chaos:state (host-unit-control-connection host-unit)) 'chaos:open-state))
       (setq status :closed) t)
      (t (send self :real-close abortp)))
    (send host :deregister-stream self))) 


(defmethod (file-input-stream-mixin :real-close) (abortp &aux success string)
  abortp
  (if (neq status :eof)
    (multiple-value-setq (string success)
      (send self :command t "CLOSE"))
    (progn
      (funcall host-unit :command t self t "CLOSE")
      (setq success t)))
  (funcall host-unit :free-data-connection data-connection :input)
  (setq status :closed)
  (cond
    (success
     ;; For sake of Twenex, look for new truename after a rename.
     (if (and string (position #\Newline (the string (string string)) :test #'char-equal))
       (ignore-errors
	(setq si:property-list
	      (nconc (read-file-property-list-string string "CLOSE" pathname) si:property-list))))
     t)
    (string (file-process-error-new string self '(:no-action) () :close)))) 

(defmethod (file-output-stream-mixin :send-output-buffer) (&rest args)
  (loop doing
     (case status
       ((:open :eof)
	(process-wait "File Output"
		      #'(lambda (stat connection)
			  (or (eq (car stat) :async-marked) (chaos:may-transmit connection)
			     (neq (chaos:state connection) 'chaos:open-state)))
		      (locate-in-instance self 'status) chaos:connection)
	(and (neq (chaos:state chaos:connection) 'chaos:open-state)
	   (chaos:report-bad-connection-state chaos:connection "output to file data connection"))
	(and (neq status :async-marked) (return (apply self :send-pkt-buffer args))))
       (:async-marked (file-process-output-async-mark))
       (:closed (ferror 'stream-closed "Attempt to output to ~S, which is closed." self))
       (otherwise
	(ferror 'stream-invalid "Attempt to output to ~S, which is in illegal state ~S." self
		status))))) 

;;; Sent from inside the interrupt function, change our status and remember error message.

(defmethod (file-output-stream-mixin :async-mark) (pkt)
  (setf (getf si:property-list 'async-mark-pkt) pkt)
  (setq status :async-marked)) 


(defmethod (file-input-stream-mixin :read-until-synchronous-mark) ()
  (loop until (eq status :sync-marked) as pkt = (file-next-read-pkt () t) when pkt do
     (chaos:return-pkt pkt) finally (setq status :open))) 


(defmethod (file-input-stream-mixin :get-next-input-pkt) (&optional no-hang-p)
  (loop when (eq status :eof) return () thereis
     (setq chaos:input-packet (file-next-read-pkt no-hang-p ())))) 


(defun file-next-read-pkt (no-hang-p for-sync-mark-p)
  (declare (:self-flavor file-input-stream-mixin))
  (case (if for-sync-mark-p
     :eof
     status)
    ((:open :eof)
     (let ((pkt (chaos:get-next-pkt chaos:connection no-hang-p "File Input")))
       (cond
	 (pkt
	  (select (chaos:pkt-opcode pkt)
		  ;; Received some sort of data, return it
	     ((%file-binary-opcode %file-character-opcode) pkt)
	     ;; No data, but a synchronous mark
	     (%file-synchronous-mark-opcode (setq status :sync-marked) (chaos:return-pkt pkt)
	      nil)
	     ;; Received an asynchronous mark, meaning some sort of error condition
	     (%file-asynchronous-mark-opcode (setq status :async-marked)
	      (or for-sync-mark-p (file-process-async-mark pkt)) (chaos:return-pkt pkt) nil)
	     ;; EOF received, change channel state and return
	     (%file-eof-opcode (setq status :eof) (chaos:return-pkt pkt) nil)
	     ;; Connection closed or broken with message
	     ((chaos:cls-op chaos:los-op)
	      (chaos:report-bad-connection-state chaos:connection "read file data from"))
	     ;; Not a recognized opcode, huh?
	     (otherwise
	      (ferror () "Receieved data packet (~S) with illegal opcode for ~S." pkt self)))))))
    (:closed (ferror 'stream-closed "Attempt to read from ~S, which is closed." self))
    ((:async-marked :sync-marked)
     (ferror 'stream-invalid "Attempt to read from ~S, which is in a marked state." self))
    (otherwise
     (ferror 'stream-invalid "Attempt to read from ~S, which is in illegal state ~S." self
	     status)))) 


(defmethod (file-output-stream-mixin :write-synchronous-mark) ()
  (let-globally ((status ':eof));In case :ASYNC-MARK now
     (send self :force-output));Send any partial buffer
  (chaos:send-pkt chaos:connection (chaos:get-pkt) %file-synchronous-mark-opcode)) 


(defun file-process-output-async-mark ()
  (declare (:self-flavor file-output-stream-mixin))
  (let ((pkt (car (remprop (locf si:property-list) 'async-mark-pkt))))
    (or pkt (ferror () "Output stream ~S in ASYNC-MARKED state, but no async mark pkt." self))
    (unwind-protect (file-process-async-mark
		     pkt)
      (chaos:return-pkt pkt)))) 


(defun file-process-async-mark (pkt)
  (let ((string
	 (nsubstring (chaos:pkt-string pkt)
		     (1+
		      (position #\Space (the string (string (chaos:pkt-string pkt))) :test
				#'char-equal)))))
    (file-process-error-new string self '(:no-action)));Process error allowing proceeding
  ;; If user says to continue, attempt to do so.
  (send self :continue)) 

(defmethod (file-output-stream-mixin :real-close) (abortp &aux success string)
  ;; Closing an open output channel.  Finish sending the data.
  (and (eq status ':open) (send self ':eof))
  ;; If aborting out of a file-writing operation before normal :CLOSE,
  ;; delete the incomplete file.  Don't worry if it gets an error.
  ;; This used to send a delete message, but now it sends a close
  ;; with optional parameter for abort.
  (multiple-value-setq (string success)
		       (send self ':command t (if abortp "CLOSE T" "CLOSE")))
  (funcall host-unit ':free-data-connection data-connection ':output)
  (setq status ':closed)
  (cond (success
	 (ignore-errors (setq si:property-list
			      (nconc
				(read-file-property-list-string string "CLOSE" si:pathname
								'((:creation-date) (:creation-time)
								  (:length t) (:lispm-length t)))
				si:property-list)))
	 t)
	(t
	 (send self :close :abort)
	 (file-process-error-new string self '(:no-action) nil ':close))))


(defmethod (file-data-stream-mixin :delete) (&optional (error-p t) &aux success string)
  (file-operation-retry
   (case status
     ((:open :eof :sync-marked :async-marked)
      (multiple-value-setq (string success)
	(send self :command () "DELETE"))
      (or success (file-process-error-new string self () (not error-p) :delete)))
     (otherwise (ferror () "~S in illegal state for delete." self))))) 


(defmethod (file-data-stream-mixin :rename) (new-name &optional (error-p t) &aux success string)
  (file-operation-retry
   (case status
     ((:open :eof :sync-marked :async-marked)
      (multiple-value-setq (string success)
	(send self :command () "RENAME" #\Newline (file-print-pathname new-name) #\Newline))
      (cond
	(success
	 ;; If there is a second line coming from the file server,
	 ;; it is the new truename.
	 (let* ((from
		 (search (the string (string #\Newline)) (the string (string string)) :test
			 #'char-equal))
		truename-string)
	   (cond
	     (from
	      (setq truename-string
		    (subseq (string string) (1+ from)
	(search (the string (string #\Newline)) (the string (string string)) :start2 (1+ from)
		:test #'char-equal)) )
	      (send self :putprop
		 (parse-pathname truename-string (send (get self :truename) :host)) :truename))))
	 (setq pathname new-name) (funcall tv:who-line-file-state-sheet :clobbered) t)
	(t (file-process-error-new string self () (not error-p) :rename))))
     (otherwise (ferror () "~S in illegal state for rename." self))))) 


(defmethod (file-data-stream-mixin :change-properties) (error-p &rest properties &aux success string)
  (file-operation-retry
   (case status
     ((:open :eof :sync-marked :async-marked)
      (multiple-value-setq (string success)
	(send self :command () (change-properties-string properties)))
      (or success (file-process-error-new string self () (not error-p) :change-properties)))
     (otherwise (ferror () "~S in illegal state for change properties." self))))) 


(defmethod (file-data-stream-mixin :continue) (&aux success string)
  (cond
    ((eq status :async-marked) (setf status :open)
     (multiple-value-setq (string success)
       (send self :command () "CONTINUE"))
     (cond
       ((null success) (setq status :async-marked) (file-process-error-new string self)))))) 	;not proceedable


(defmethod (file-input-stream-mixin :set-buffer-pointer) (new-pointer &aux string success)
  (case status
    ((:open :eof) (and (eq status :eof) (setq status :open))
     (multiple-value-setq (string success)
       (send self :command t "FILEPOS " (format () "~D" new-pointer)))
     (or success (file-process-error-new string self () () :set-pointer));Cannot proceed
     new-pointer)
    (otherwise (ferror () ":SET-POINTER attempted on ~S which is in state ~S." self status)))) 


(defmethod (file-output-stream-mixin :finish) ()
  (do ()
      ((chaos::conn-finished-p chaos:connection))
    (process-wait "File Finish"
		  #'(lambda (conn stat)
		      (or (chaos::conn-finished-p conn) (eq (car stat) :async-marked)))
		  chaos:connection (locate-in-instance self 'status))
    (and (eq status :async-marked) (file-process-output-async-mark)))) 


(defmethod (file-output-stream-mixin :eof) ()
  (send self :force-output)
  (chaos:send-pkt chaos:connection (chaos:get-pkt) chaos:eof-op)
  (setq status :eof)
  (send self :finish)) 


;;; File IO streams

(defmethod (file-iO-stream-mixin :real-close) (abortp &aux pkt success string)
  abortp
  (and (eq status ':open) (send self :eof))
  (setf file-handle (data-handle data-connection :input))
  (multiple-value-setq (pkt success string)
    (funcall host-unit :command () file-handle  () "READ " direct-file-id "" ))
  (when pkt (chaos:return-pkt pkt))   ;unbind
  (setf file-handle (data-handle data-connection :output))
  (multiple-value-setq (pkt success string)
    (funcall host-unit :command () file-handle  () "DIRECT-OUTPUT " direct-file-id "" ))
  (when pkt (chaos:return-pkt pkt))   ;unbind
  (send self :read-until-synchronous-mark)   ;from input half of channel
  (send self :read-until-synchronous-mark)   ;from output half of channel
  (multiple-value-setq (pkt success string)
    (funcall host-unit :command t direct-file-id () "CLOSE " (if abortp "T" "")))
  (when pkt (chaos:return-pkt pkt))
  (setq success t)
  (funcall host-unit :free-data-connection data-connection :input)
  (funcall host-unit :free-data-connection data-connection :output)
  (setq status :closed)
  (cond
    (success
     ;; For sake of Twenex, look for new truename after a rename.
     (if (and string (position #\Newline (the string (string string)) :test #'char-equal))
	 (ignore-errors
	   (setq si:property-list
		 (nconc (read-file-property-list-string string "CLOSE" pathname) si:property-list))))
     t)
    (string (file-process-error-new string self '(:no-action) () :close))))

(defmethod (file-io-stream-mixin :read-until-synchronous-mark) ()
  (loop until (eq status :sync-marked) as pkt = (send self :file-next-read-pkt () t) when pkt do
	(chaos:return-pkt pkt) finally (setq status :open)))

(defmethod (file-io-stream-mixin :get-next-input-pkt) (&optional no-hang-p)
  (setq status :open)
  (loop when (eq status :eof)  return ()
	thereis
	(setq chaos:output-packet (send self :file-next-read-pkt no-hang-p ()))
	))

(defmethod (file-io-stream-mixin :file-next-read-pkt) (no-hang-p for-sync-mark-p)
  (case (if for-sync-mark-p
	    :eof
	    status)
    ((:open :eof)
     (let ((pkt (chaos:get-next-pkt chaos:connection no-hang-p "File Input")))
       (cond
	 (pkt
	  (select (chaos:pkt-opcode pkt)
	    ;; Received some sort of data, return it
	    ((%file-binary-opcode %file-character-opcode)  pkt)
	    ;; No data, but a synchronous mark
	    (%file-synchronous-mark-opcode (setq status :sync-marked) (chaos:return-pkt pkt)
					   nil)
	    ;; Received an asynchronous mark, meaning some sort of error condition
	    (%file-asynchronous-mark-opcode (setq status :async-marked)
					    (or for-sync-mark-p
						(file-process-async-mark pkt))
					    (chaos:return-pkt pkt) nil)
	    ;; EOF received, change channel state and return
	    (%file-eof-opcode (setq status :eof) (chaos:return-pkt pkt) nil)
	    ;; Connection closed or broken with message
	    ((chaos:cls-op chaos:los-op)
	     (chaos:report-bad-connection-state chaos:connection "read file data from"))
	    ;; Not a recognized opcode, huh?
	    (otherwise
	     (ferror () "Receieved data packet (~S) with illegal opcode for ~S." pkt self)))))))
    (:closed (ferror 'stream-closed "Attempt to read from ~S, which is closed." self))
    (:async-marked (file-process-output-async-mark))
    (:sync-marked (setq status :OPEN) (chaos:return-pkt (chaos:get-next-pkt chaos:connection)))
    (otherwise
     (ferror 'stream-invalid "Attempt to read from ~S, which is in illegal state ~S." self
	     status))))

(defmethod (file-IO-stream-mixin :around :send-output-buffer) (cont mt arg &rest ignore)
  (setq status :open)
  (let (pkt string success)
    (setf file-handle (data-handle data-connection :output))
    (multiple-value-setq (pkt success string)
      (funcall host-unit :command () file-handle  () "DIRECT-OUTPUT " direct-file-id " T"
	       " " (format nil "~d" (or real-output-pointer-base 0))))
    (when pkt (chaos:return-pkt pkt)))
  (apply 'funcall-with-mapping-table cont mt arg))

(defmethod (file-IO-stream-mixin :after :send-output-buffer) (&rest ignore)
  (chaos:return-pkt (chaos:get-next-pkt chaos:connection))
  (setf real-output-pointer-base  sys:output-pointer-base))



(defmethod (file-IO-stream-mixin :around :next-input-buffer) (cont mt arg &rest ignore)
  (let (pkt string success set-pointer-position)
    (setf file-handle (data-handle data-connection :input))
    (setf set-pointer-position (getf arg :set-pointer-position))
    (multiple-value-setq (pkt success string)
      (if set-pointer-position
	  (funcall host-unit :command () file-handle  () "READ " direct-file-id " T"
		   (format nil " ~d" set-pointer-position))
	  (funcall host-unit :command () file-handle  () "READ " direct-file-id " T" )))
    (when pkt (chaos:return-pkt pkt)))
  (setf real-output-pointer-base  sys:output-pointer-base)
  (funcall-with-mapping-table cont mt arg)
  )

(defmethod (file-IO-stream-mixin :after :next-input-buffer) (&rest ignore)
  (let* ((sync-pkt (chaos:get-next-pkt chaos:connection))
	 (eof? (= (chaos:pkt-opcode sync-pkt) chaos:eof-op)))
    (chaos:return-pkt sync-pkt)
    (when eof? (chaos:return-pkt (chaos:get-next-pkt chaos:connection)))))

(defmethod (file-io-stream-mixin :set-buffer-pointer) (new-pointer &aux pkt string success)
  (case status
    ((:open :eof) (and (eq status :eof) (setq status :open))
		  (send self :force-output)
		  (setf file-handle (data-handle data-connection :input))
		  ;Cannot proceed
		  (multiple-value-setq (pkt success string)
		    (funcall host-unit :command () "" () "FILEPOS " (format nil "~d " new-pointer)
			     direct-file-id))
		  (when pkt (chaos:return-pkt pkt))
		  new-pointer)
    (otherwise (ferror () ":SET-POINTER attempted on ~S which is in state ~S." self status))))


(defmethod (file-IO-stream-mixin  :finish) ()
  (process-wait "File Finish"
		#'(lambda (conn stat)
		    (or (chaos::conn-finished-p conn) 
			(eq (car stat) :async-marked)))
		chaos:connection (locate-in-instance self 'status)))

(defmethod (file-IO-stream-mixin :eof) ()
  (send self :force-output)
  (setq status :eof)
  (send self :finish)) 



(defflavor file-character-stream-mixin () (file-data-stream-mixin)) 


(defmethod (file-character-stream-mixin :element-type) ()
  'string-char) 


(defflavor file-binary-stream-mixin () (file-data-stream-mixin)) 


(defmethod (file-binary-stream-mixin :set-byte-size) (new-byte-size)
  (check-arg new-byte-size
     (and (numberp new-byte-size) (> new-byte-size 0) (<= new-byte-size 16))
     "A positive number less than or equal to 16.")
  (send self :command t "SET-BYTE-SIZE "
     (format () "~D ~D" new-byte-size (send self :read-pointer)))
  new-byte-size) 


(defmethod (file-binary-stream-mixin :element-type) ()
  'unsigned-byte) 


(defflavor file-input-character-stream-mixin ()
   (file-input-stream-mixin file-character-stream-mixin)) 


(defflavor file-input-binary-stream-mixin () (file-input-stream-mixin file-binary-stream-mixin)) 


(defflavor file-input-signed-binary-stream-mixin (current-byte-size)
   (file-input-stream-mixin file-binary-stream-mixin)
   (:required-flavors si:basic-buffered-input-stream) :inittable-instance-variables) 


(defmethod (file-input-signed-binary-stream-mixin :after :init) (ignore)
  (setq current-byte-size (getf si:property-list :byte-size))) 


(defmethod (file-input-signed-binary-stream-mixin :after :set-byte-size) (new-byte-size)
  (setq current-byte-size new-byte-size)) 


(defmethod (file-input-signed-binary-stream-mixin :element-type) ()
  `(signed-byte ,current-byte-size)) 


(defmethod (file-input-signed-binary-stream-mixin :around :tyi) (cont mt args &rest ignore)
  (let ((byte (around-method-continue cont mt args)))
    (when byte
      (if (ldb-test (byte 1 (1- current-byte-size)) byte)
	(- byte (lsh 1 current-byte-size))
	byte)))) 


(defmethod (file-input-signed-binary-stream-mixin :string-in) (eof string &optional (start 0) end)
  (or end (setq end (array-total-size string)))
  (loop while (< start end) while
     (loop until (and si::stream-input-buffer (< si::stream-input-index si::stream-input-limit));Out of input, get some more
	until (send self :setup-next-input-buffer) do
	(and eof (ferror 'end-of-file-1 "End of file on ~S." self)) return () finally (return t))
     as amt = (min (- end start) (- si::stream-input-limit si::stream-input-index)) do
     (copy-array-portion si::stream-input-buffer si::stream-input-index
			 (setq si::stream-input-index (+ si::stream-input-index amt)) string
			 start (setq start (+ start amt)))
     ;; Sign-extend each byte.
     (do ((i start (1+ i))
	  (end1 (+ start amt)))
	 ((= i end1))
       (let ((byte (aref string i)))
	 (if (ldb-test (byte 1 (1- current-byte-size)) byte)
	   (setf (aref string i) (- byte (lsh 1 current-byte-size))))))
     finally (and (array-has-leader-p string) (store-array-leader start string 0))
     (return (values start (null si::stream-input-buffer))))) 


(defflavor file-input-phony-character-stream-mixin ()
   (file-input-stream-mixin file-character-stream-mixin)) 


(defmethod (file-input-phony-character-stream-mixin :element-type) ()
  'global:character) 


(defmethod (file-input-phony-character-stream-mixin :around :tyi) (cont mt args &rest ignore)
  (let ((ch1 (around-method-continue cont mt args))
	(ch2 (around-method-continue cont mt args))
	(ch3 (around-method-continue cont mt args))
	(ch4 (around-method-continue cont mt args)))
    (dpb ch4 (byte 8 24) (dpb ch3 (byte 8 16) (dpb ch2 (byte 8 8) ch1))))) 


(defflavor file-output-character-stream-mixin ()
   (file-output-stream-mixin file-character-stream-mixin)) 


(defflavor file-output-phony-character-stream-mixin () (file-output-binary-stream-mixin)) 


(defmethod (file-output-phony-character-stream-mixin :element-type) ()
  'global:character) 


(defmethod (file-output-phony-character-stream-mixin :around :tyo) (cont mt args char)
  args
  (funcall-with-mapping-table cont mt :tyo (ldb (byte 8 0) char))
  (funcall-with-mapping-table cont mt :tyo (ldb (byte 8 8) char))
  (funcall-with-mapping-table cont mt :tyo (ldb (byte 8 16) char))
  (funcall-with-mapping-table cont mt :tyo (ldb (byte 8 24) char))) 


(defflavor file-output-binary-stream-mixin ()
   (file-output-stream-mixin file-binary-stream-mixin)) 


(defmethod (file-output-character-stream-mixin :send-pkt-buffer) chaos:send-character-pkt) 


(defmethod (file-output-binary-stream-mixin :send-pkt-buffer) chaos:send-binary-pkt) 



(defflavor file-IO-character-stream-mixin ()   ;dab
	   (file-IO-stream-mixin file-character-stream-mixin))


(defflavor file-IO-binary-stream-mixin () (file-IO-stream-mixin
					   file-output-binary-stream-mixin
					   file-binary-stream-mixin))





(defflavor file-input-character-stream ()
   (file-input-character-stream-mixin si:input-file-stream-mixin
    chaos:character-input-stream-mixin si:buffered-input-character-stream)) 


(defflavor file-input-phony-character-stream ()
   (file-input-phony-character-stream-mixin si:input-file-stream-mixin
    chaos:binary-input-stream-mixin si:buffered-tyi-input-stream)) 


(defflavor file-output-character-stream ()
   (file-output-character-stream-mixin si:output-file-stream-mixin
    chaos:character-output-stream-mixin si:buffered-output-character-stream)) 


(defflavor file-output-phony-character-stream ()
   (file-output-phony-character-stream-mixin si:output-file-stream-mixin
    chaos:binary-output-stream-mixin si:buffered-tyo-output-stream)) 


(defflavor file-input-binary-stream ()
   (file-input-binary-stream-mixin si:input-file-stream-mixin chaos:binary-input-stream-mixin
    si:buffered-input-stream)) 


(defflavor file-input-signed-binary-stream ()
   (file-input-signed-binary-stream-mixin si:input-file-stream-mixin
    chaos:binary-input-stream-mixin si:basic-buffered-input-stream)) 


(defflavor file-output-binary-stream ()
   (file-output-binary-stream-mixin si:output-file-stream-mixin chaos:binary-output-stream-mixin
    si:buffered-output-stream)) 



(defflavor file-IO-character-stream ()  ;dab
	   (file-IO-character-stream-mixin
	     si:IO-file-stream-mixin
	     chaos:character-io-stream-mixin 
	     file-output-character-stream
	     si:buffered-IO-character-stream))

(defmethod (file-IO-character-stream :direction)
	   ()
  :IO)



(defflavor file-IO-binary-stream ()
	   (file-IO-binary-stream-mixin si:IO-file-stream-mixin
	    chaos:binary-IO-stream-mixin si:buffered-IO-stream))

(defmethod (file-IO-binary-stream :direction)
	   ()
  :IO)



(defflavor file-probe-stream ((status ':closed)) (file-stream-mixin stream)
   (:gettable-instance-variables status) (:init-keywords :data-connection)) ;Will be NIL, but makes life easier


(defmethod (file-probe-stream :direction) ()
  ()) 


(defflavor file-directory-stream () (file-input-character-stream)) 

(defmethod 4(file-directory-stream :read-directory-stream-entry*) ()
  (read-directory-stream-entry self pathname))

(compile-flavor-methods file-input-character-stream file-input-binary-stream
   file-input-signed-binary-stream file-input-phony-character-stream
   file-output-character-stream file-output-binary-stream file-output-phony-character-stream
   file-probe-stream file-directory-stream  file-IO-character-stream file-IO-binary-stream )
