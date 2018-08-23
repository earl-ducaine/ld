;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT MEDFNB HL12BI); Base:10 -*-

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

(defvar x) 

(defvar y) 

(defvar z) 


(defun make-ip-test-pkt (&key &optional (fo 0) (mf nil) (df nil) (ds 24)
			 (security nil) (loose 0) (strict 0) (record 0)
                         (stream ()) (timestamp 0) (data-seed nil) (so nil))
  "2Makes an IP test packet.*"
  ;1; FO is fragment offset if any in actual bytes into the packet.*
  ;1;     This should be divisable by 8.*
  ;1; MF is the more fragments flag, default nil.*
  ;1; DFis the don't fragment flag, default nil.*
  ;1; ds is the size of the data to put in.*
  ;1; SECURITY is for a security option.*
  ;1; LOOSE is for a loose-source-record route option, number to record.*
  ;1; Strict is for a strict-source-record-route option, number to record.*
  ;1; RECORD is for a record route option, number to record.*
  ;1; Stream is for a SATNET stream option.*
  ;1; TIMESTAMP is for a timestamp option, number of timestampes.*
  ;1; Data-seed is a character to use rather than get random characters.*
  (let* ((data-l ds)
	 (op-l (+ 3 (* 4 loose)))
	 (op-st (+ 3 (* 4 strict)))
	 (op-rec (+ 3 (* 4 record)))
	 (op-time (+ 4 (* 4 timestamp)))
	 (options
	  (+ (if security
	       11
	       0)
	     (if (zerop loose)
	       0
	       op-l)
	     (if (zerop strict)
	       0
	       op-st)
	     (if (zerop record)
	       0
	       op-rec)
	     (if stream
	       4
	       0)
	     (if (zerop timestamp)
	       0
	       op-time)))
	 ;1;Option size in bytes.*
	 pkt
	 header
	 pkt-data
	 pkt-options
	 data
	 srro
	 stro
	 to
	 (option-offset 0))
    (setf pkt (make-array (+ 10 (ceiling (1+ options) 2) (ceiling data-l 2)) :type :art-16b))
    (setf header (setf (ip-header-length pkt) (+ 5 (ceiling options 4))))
    (FORMAT t "~&MAKE-IP-PKT: Pkt = ~A, length = ~A, header = ~A, Options = ~A, Data-l = ~A"
	    pkt (+ 10 (ceiling (1+ options) 2) (ceiling data-l 2)) header options data-l)
    (setf pkt-options (make-ip-option-data pkt))
    (setf (ip-total-length pkt) (+ data-l (* 4 header)))
    (setf data (make-array data-l :type :art-8b))
    (loop for i from 0 below data-l do
       (setf (aref data i)
	     (if data-seed
	       (character data-seed)
	       (if (zerop (mod i 10))
		 i
		 (character (+ 48 (mod i 80)))))))
    (FORMAT t "~&Packet-data = ~A, Data length = ~A" data data-l)
    (set-ip-total-length pkt (+ data-l (* header 4)))
    (setf pkt-data (make-ip-data pkt))
    (loop for i from 0 below data-l do (setf (aref pkt-data i) (aref data i)))
    (setf (ip-version pkt) 4)
    (setf (ip-precedence pkt) 0)
    (setf (ip-delay pkt) 0)
    (setf (ip-thruput pkt) 0)
    (setf (ip-reliability pkt) 0)
    (setf (ip-tos-reserved pkt) 0)
    (set-ip-identification pkt 65261)
    (setf (ip-flags-reserved pkt) 0)
    (setf (ip-dont-fragment-p pkt) df)
    (setf (ip-more-fragments-p pkt) mf)
    ;1;Set to more fragments.*
    (set-ip-fragment-offset pkt
       	 (/ fo 8))
    (FORMAT t "~&Fragment offset = ~A, Set in packet as = ~A" fo (ip-fragment-offset pkt))
    (setf (ip-time-to-live pkt) 255)
    (setf (ip-protocol pkt) 127)
    (set-ip-header-checksum pkt 0)
    (set-ip-src-addr pkt (FIRST my-addresses))
    (set-ip-dst-addr pkt (FIRST my-addresses))
    (when (not (zerop loose))
      (setf srro
	    (make-array op-l :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (incf option-offset op-l)
      (setf (srr-type srro) ip-loose-source-and-record-route-option)
      (setf (srr-len srro) (- op-l 3))
      (setf (srr-pointer srro) 4))
    (when (not (zerop strict))
      (setf srro
	    (make-array op-st :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (incf option-offset op-st)
      (setf (srr-type srro) ip-strict-source-and-record-route-option)
      (setf (srr-len srro) (- op-st 3))
      (setf (srr-pointer srro) 4))
    (when (not (zerop record))
      (setf srro
	    (make-array op-rec :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (FORMAT t "~&RECORD Option: Pkt = ~A, size = ~A, Displaced to ~A, offset ~A"
	      srro op-rec pkt-options option-offset)
      (incf option-offset op-rec)
      (setf (srr-type srro) ip-record-route-option)
      (setf (srr-len srro) (- op-rec 3))
      (setf (srr-pointer srro) 4))
    (when stream
      (setf stro
	    (make-array 2 :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (incf option-offset 4)
      (setf (stream-type stro) ip-stream-identifier-option)
      (setf (stream-len stro) ip-stream-identifier-length)
      (setf (stream-id-high stro) 222)
      (setf (stream-id-low stro) 173))
    (when (not (zerop timestamp))
      (setf to
	    (make-array op-time :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (incf option-offset op-time)
      (setf (time-type to) ip-time-stamp-option)
      (setf (time-len to) op-time)
      (setf (time-pointer to) 5)
      (setf (time-overflow to) 0)
      (setf (time-flag to) 0))
    ;1;Timestamps only.*
    (when security
      (setf so
	    (make-array 11 :element-type '(unsigned-byte 8) :displaced-to pkt-options
			:displaced-index-offset option-offset))
      (FORMAT t "~&SECURITY Option: Pkt = ~A, size = ~A, Displaced to ~A, offset ~A"
	      srro op-rec pkt-options option-offset)
      (incf option-offset 11)
      (setf (security-type so) 130)
      (setf (security-len so) 11)
      (setf (security-security-high so) 241);1Confidential*
      (setf (security-security-low so) 53)
      (setf (security-compartments-high so) 0)
      (setf (security-compartments-low so) 0)
      (setf (security-handling-h so) 0)
      (setf (security-handling-l so) 0)
      (setf (security-tcc-high so) 0)
      (setf (security-tcc-mid so) 0)
      (setf (security-tcc-lsb so) 0))
    ;1;Make it an end option.*
    (FORMAT t "~& Option size = ~A, Curret offset = ~A"
	    options option-offset)
    (loop for h from option-offset below (ceiling options 4) do (setf (aref pkt-options h) 0))
    ;1;Fill with padding.*
    (FORMAT t "~&Make checksum for pkt with header size of ~A"
	    (* 2 header))
    (setf (ip-header-checksum pkt) (ip-ones-complement-checksum pkt (* 2 header)))
    pkt)) 


(defun make-fragment-set ()
  (setf x (make-ip-test-pkt :fo 0 :lf 1 :ds 48 :data-seed "A"
			    ))
  (setf y (make-ip-test-pkt :fo 48 :lf 1 :ds 48 :data-seed "B"
			    ))
  (setf z (make-ip-test-pkt :fo 96 :lf 0 :ds 48 :data-seed "C"
			    ))) 	   



(defun print-packet (pkt &key &optional (stream *terminal-io*) (size 0))
  "2This dumps an art-16b array to the stream stream.  Bytes are swapped in a 16bit word.
    If no size is given than the size of the array is used.*"
  (let (char1
	char2
	len)
    (setf len (if (zerop size) (length pkt) (/ size 2)))
    (FORMAT stream "~%~%"
	    )
    (do ((i 0))
	((>= i len))
      (FORMAT stream "~&I = ~16,4,'0r  D = "
	      (* 2 i))
      (loop for h from 0 to 7 unless (>= (+ i h) len) do
	 (FORMAT stream "~16,4,'0r "
		 (net:swap-bytes (aref pkt (+ i h)))))
      (FORMAT stream "~55T"
	      )
      (loop for h from 0 to 7 unless (>= (+ i h) len) do
	 (setf char1 (ldb (byte 8 0) (aref pkt (+ i h)))) and do
	 (setf char2 (ldb (byte 8 8) (aref pkt (+ i h)))) and do
	 (FORMAT stream "~c~c "
		 (if (or (< char1 32) (>= char1 128))
		   0
		   char1)
		 (if (or (< char2 32) (>= char2 128))
		   0
		   char2))
	 finally (incf i h)))
    (FORMAT stream "~%"
	    ))) 


(defun print-list-of-packets (&optional fragments)
  (loop for h in fragments do (print-packet h :size (* 2 (length h))))) 


(defun print-data (pkt &key &optional (stream *terminal-io*) (size 0))
  "2This dumps an art-8b array to the stream stream.
    If no size is given than the size of the array is used.*"
  (let ((len (if (zerop size)
	       (length pkt)
	       size))
	char1)
    (FORMAT stream "~%~%"
	    )
    (do ((i 0))
	((>= i len))
      (FORMAT stream "~&I = ~16,4,'0r  D = "
	      i)
      (loop for h from 0 to 15 unless (>= (+ i h) len) do
	 (FORMAT stream "~16,2,'0r "
		 (aref pkt (+ i h))))
      (FORMAT stream "~65T"
	      )
      (loop for h from 0 to 15 unless (>= (+ i h) len) do (setf char1 (aref pkt (+ i h))) and do
	 (FORMAT stream "~c "
		 (if (or (< char1 32) (>= char1 128))
		   0
		   char1)) finally
	 (incf i h)))
    (FORMAT stream "~%"
	    ))) 
