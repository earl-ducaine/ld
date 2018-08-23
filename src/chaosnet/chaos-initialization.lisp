;;; -*- Mode:Common-Lisp; Package:CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb;  -*-

1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;------------------------------------------------------------------------------
;;*	1     Initialize all of the data of the NCP routines
;;------------------------------------------------------------------------------*


(defun 4INITIALIZE-NCP-ONCE* ()          1;7/10/84 RAF*
  2"Execute Load-Time initializations, done only once."*
  (setq prototype-conn (make-conn)    1; Connection list*
	recent-headers (make-array '(128 9)
				   :element-type '(unsigned-byte 16)
				   :area chaos-area))
	1;; Array of 200 most recent packet transactions each row*
	1;; containing the eight header words of the packet and the*
	1;; time at which the record was made.*

  1;; Make 10 connections now so they're all on the same page.*
  (dotimes (i 8)
    (push (make-conn) free-conn-list))) 

(defun 4INITIALIZE-NCP-BEFORE-COLD* ()
  (chaos:reset nil t)
  (setf chaos-servers-enabled nil
	CHAOS-SERVERS-ENABLED NIL
	BAD-PKT-LIST ()
	LOS-PKTS ()
	CURRENT-LOS-PKT-COUNT 0
	MAX-LOS-PKTS-TO-KEEP 16
	RECENT-HEADERS-POINTER 0)
  (INITIALIZATIONS 'NET-BEFORE-COLD-INIT-LIST T))

(DEFUN 4INITIALIZE-NCP-SYSTEM* ()1;TI*
  2"Execute Initializations for every cold & warm boot."*
  (setf chaos-servers-enabled nil)
  (RESET nil t)
  (SETUP-MY-ADDRESS)1;Initialize my address & SI:Local-Host.*
  (INITIALIZATIONS 'NET-SYSTEM-INIT-LIST T))


(DEFUN 4INITIALIZE-NCP-COLD* ()1;7/10/84 RAF*
  2"Execute Cold-Boot initializations (after SYSTEM initializations)."*
  1;; Debugging aids which keep records into the past (a short way).*
  (SETQ BAD-PKT-LIST ()1;List of strings describing packets received in error*

	LOS-PKTS ()1;LOS PKTs received from the network linked through PKT-LINK.*
	CURRENT-LOS-PKT-COUNT 0
	MAX-LOS-PKTS-TO-KEEP 161;Maximum number of LOS packets to keep on LOS-PKTS*
			     1;There may actually be more but they will be used by allocator*

	RECENT-HEADERS-POINTER 0))

(defun 4initialize-ncp-warm* () 1;TI*
  (setup-my-address)
  (initializations 'net-warm-init-list t)
  (enable))


(ADD-INITIALIZATION 3"Initialize Chaosnet"* '(INITIALIZE-NCP-ONCE) '(ONCE))

(ADD-INITIALIZATION 3"Initialize Chaosnet"* '(INITIALIZE-NCP-BEFORE-COLD) nil
		    'HOST:*NETWORK-BEFORE-COLD-INITIALIZATION-LIST*)

(ADD-INITIALIZATION 3"Initialize Chaosnet"* '(INITIALIZE-NCP-SYSTEM) '(NORMAL)
		    'HOST:*NETWORK-SYSTEM-INITIALIZATION-LIST*)

(ADD-INITIALIZATION 3"Initialize Chaosnet"* '(INITIALIZE-NCP-cold) nil
		    'HOST:*NETWORK-COLD-INITIALIZATION-LIST*)

(ADD-INITIALIZATION 3"Initialize Chaosnet"* '(initialize-ncp-warm)
		    :Head-of-List 'HOST:*NETWORK-WARM-INITIALIZATION-LIST*) 


(ADD-INITIALIZATION 3"Reset Chaosnet"* '(reset net:4*net-reset-enable-p**) nil
		    'NET:*RESET-INITIALIZATION-LIST*)


(add-initialization "Initialize Subnets" '(initialize-ether-chaos-subnets) '(:site-option :normal))
