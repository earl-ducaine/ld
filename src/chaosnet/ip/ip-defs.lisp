;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT MEDFNB TR12BI); Base:10 -*-

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

;1;; Globally available IP data structures.*

;1;========================================================================*
;1; IP packet header.  For definitions of the fields see*
;1;    DARPA INTERNET PROTOCOL RFC791 section 3 pp11-15*
;1;*
;1; In this structure items prefaced with "RAW-IP-" are the physical items*
;1;   in the array. Items prefaced with "IP-" are the logical items with*
;1;   byte swapping done as appropriate.*
;1;*
;1; Fields in this structure are byte swapped.*

(DEFSTRUCT (ip-header (:make-array (:type :art-16b)) (:conc-name "RAW-IP-") (:constructor nil)
		      :size-symbol (:callable-constructors nil) (:alterant alter-ip-header) (:predicate nil)
		      (:copier nil) (:type :array))
	   ((version 260)
	    (header-length 4)
	    (precedence 835)
	    (delay 769)
	    (thruput 705)
	    (reliability 641)
	    (tos-reserved 514))
  (total-length)
  (identification)
  ((flags-reserved 449)
   (dont-fragment 385)
   (more-fragments 321)
   (fragment-offset-lsb 520)
   (fragment-offset-msb 5))		   ;1 this is handled specially, because it is a split field*
  ((time-to-live 8) (protocol 520))
  (header-checksum)
  (src-addr-msw)
  (src-addr-lsw)
  (dst-addr-msw)
  (dst-addr-lsw))
 

(PROCLAIM '(inline shuffle-word))
(DEFUN shuffle-word (word)
  (net:swap-bytes word))
 

(PROCLAIM '(inline swap-long))
(DEFUN swap-long (long)
  (DPB (LDB (BYTE 8 0) long) (BYTE 8 8)
       (DPB (LDB (BYTE 8 16) long) (BYTE 8 24)
	    (DPB (LDB (BYTE 8 24) long) (BYTE 8 16) (LDB (BYTE 8 8) long))))) 


(DEFF-MACRO ip-version 'raw-ip-version) 

(DEFF-MACRO ip-header-length 'raw-ip-header-length) 

(DEFF-MACRO ip-precedence 'raw-ip-precedence) 

(DEFF-MACRO ip-delay 'raw-ip-delay) 

(DEFF-MACRO ip-thruput 'raw-ip-thruput) 

(DEFF-MACRO ip-reliability 'raw-ip-reliability) 

(DEFF-MACRO ip-tos-reserved 'raw-ip-tos-reserved) 



(DEFMACRO ip-total-length (packet)
  `(net:swap-bytes (raw-ip-total-length ,packet))) 


(DEFMACRO set-ip-total-length (packet value)
  `(SETF (raw-ip-total-length ,packet) (net:swap-bytes ,value))) 


(DEFSETF ip-total-length set-ip-total-length) 



(DEFUN ip-identification (packet)
  (net:swap-bytes (raw-ip-identification packet))) 


(DEFUN set-ip-identification (packet value)
  (SETF (raw-ip-identification packet) (net:swap-bytes value))) 


(DEFSETF ip-identification set-ip-identification) 



(DEFF-MACRO ip-flags-reserved 'raw-ip-flags-reserved) 

(DEFF-MACRO ip-dont-fragment 'raw-ip-dont-fragment)

(DEFMACRO ip-dont-fragment-p (packet)
  `(= (ip-dont-fragment ,packet) 1))

(DEFSETF ip-dont-fragment-p (packet) (value)
  `(PROGN
     (SETF (ip-dont-fragment ,packet) (IF ,value 1 0))
     ,value))


(DEFF-MACRO ip-more-fragments 'raw-ip-more-fragments)

(DEFMACRO ip-more-fragments-p (packet)
  `(= (ip-more-fragments ,packet) 1))

(DEFSETF ip-more-fragments-p (packet) (value)
  `(PROGN
     (SETF (ip-more-fragments ,packet) (IF ,value 1 0))
     ,value))


(DEFMACRO ip-fragment-offset (header)
  `(LOGIOR (raw-ip-fragment-offset-lsb ,header) (ROT (raw-ip-fragment-offset-msb ,header) 8))) 


(DEFMACRO set-ip-fragment-offset (packet value)
  `(SETF (raw-ip-fragment-offset-lsb ,packet) (LDB 8 ,value)
	 (raw-ip-fragment-offset-msb ,packet) (LDB 517 ,value))) 


(DEFSETF ip-fragment-offset set-ip-fragment-offset) 



(DEFF-MACRO ip-time-to-live 'raw-ip-time-to-live) 

(DEFF-MACRO ip-protocol 'raw-ip-protocol) 



(DEFMACRO ip-header-checksum (packet)
  `(net:swap-bytes (raw-ip-header-checksum ,packet))) 


(DEFMACRO set-ip-header-checksum (packet value)
  `(SETF (raw-ip-header-checksum ,packet) (net:swap-bytes ,value))) 


(DEFSETF ip-header-checksum set-ip-header-checksum) 



(DEFMACRO ip-src-addr (packet)
  `(LOGIOR (net:swap-bytes (raw-ip-src-addr-lsw ,packet))
	   (ASH (net:swap-bytes (raw-ip-src-addr-msw ,packet)) 16))) 


(DEFMACRO set-ip-src-addr (packet value)
  `(PROGN
     (SETF (raw-ip-src-addr-lsw ,packet) (net:swap-bytes (LDB 16 ,value)))
     (SETF (raw-ip-src-addr-msw ,packet) (net:swap-bytes (LDB 1040 ,value))))) 


(DEFSETF ip-src-addr set-ip-src-addr) 


(DEFMACRO ip-dst-addr (packet)
  `(LOGIOR (net:swap-bytes (raw-ip-dst-addr-lsw ,packet))
	   (ASH (net:swap-bytes (raw-ip-dst-addr-msw ,packet)) 16))) 


(DEFMACRO set-ip-dst-addr (packet value)
  `(PROGN
     (SETF (raw-ip-dst-addr-lsw ,packet) (net:swap-bytes (LDB 16 ,value)))
     (SETF (raw-ip-dst-addr-msw ,packet) (net:swap-bytes (LDB 1040 ,value))))) 


(DEFSETF ip-dst-addr set-ip-dst-addr) 


(DEFMACRO ip-source-address-from-packet (pkt)
  `(+ (ASH (raw-ip-src-addr-msw ,pkt) 16) (raw-ip-src-addr-lsw ,pkt))) 

;1;============================================================*
;1; Added 5/2/85 RVL*
;1;Constants needed*

;1; Addressing type constants.*
;1; Type a is the MSbit*

(DEFCONSTANT ip-class-a 0
   "2IP class a address. 1 bit type, 7 bits net, 24 bits host.*"
   ) 

;1; Type b is the two MSbits*

(DEFCONSTANT ip-class-b 2
   "2IP class b address. 2 bits type 10 binary. 7 bits net, 16 bits host.*"
   ) 

;1; Type c is the three MSbits*

(DEFCONSTANT ip-class-c 6
   "2IP class c address. 3 bits type 110 binary. 21 bits net. 8 bits host.*"
   ) 

;1; Extended the the three MSbits.*

(DEFCONSTANT ip-extended-address 7
   "2IP extended address. 3 bits type 111 binary.*"
   ) 


;1;==============================================================*
;1; options items for IP headers.*
;1; *
;1; For definitions of these options refer to RFC791 pp17-23*
;1;*
;1; Options codes.*
;1;*

(DEFCONSTANT ip-option-type-copied-flag (BYTE 1 7))

(DEFCONSTANT ip-option-type-class (BYTE 2 5))

(DEFCONSTANT ip-option-type-number (BYTE 5 0))

(DEFCONSTANT ip-end-of-option-list 0
   "2End of the options list.*"
   ) 

(DEFCONSTANT ip-no-operation-option 1
   "2No Operation. Used between options to fill to 32 bit boundary.*"
   ) 

(DEFCONSTANT ip-security-option 130
   "2Security option.*"
   ) 

(DEFCONSTANT ip-security-option-size 11
   "2Size of the security option in the option length field.*"
   ) 

(DEFCONSTANT ip-loose-source-and-record-route-option 131) 

(DEFCONSTANT ip-strict-source-and-record-route-option 137) 

(DEFCONSTANT ip-record-route-option 7
   "2Record of the route taken.*"
   ) 

(DEFCONSTANT ip-stream-identifier-option 88
   "2Way to pass on SATNET stream identifier.*"
   ) 

(DEFCONSTANT ip-stream-identifier-length 4
   "2Size of the Identifier option in the option length*"
   ) 

(DEFCONSTANT ip-time-stamp-option 68) 
;1;Time stamp flags for use in the flag field of the Timestamp operation.*
;1; See p22 in RFC791.,*

(DEFCONSTANT ip-time-stamp-only 1) 

(DEFCONSTANT ip-time-stamp-register-all 1) 

(DEFCONSTANT ip-time-stamp-prespecified 3) 


;1;Security S field options.  See Page 17 of RFC791.*
;1; These are byte swapped.*

(DEFCONSTANT ip-ss-unclassified 0) 

(DEFCONSTANT ip-ss-confidential 61749) 

(DEFCONSTANT ip-ss-efto 30874) 

(DEFCONSTANT ip-ss-mmmm 48205) 

(DEFCONSTANT ip-ss-prog 24102) 

(DEFCONSTANT ip-ss-restricted 44819) 

(DEFCONSTANT ip-ss-secret 55176) 

(DEFCONSTANT ip-ss-top-secret 27589) 

;1; Templates for options.*


(DEFSTRUCT (ip-security (:make-array (:type :art-8b)) (:conc-name "SECURITY-") (:constructor nil)
			:size-symbol (:callable-constructors nil) (:alterant alter-ip-security) (:predicate nil)
			(:copier nil) (:type :array))
  "IP Security option construct.  Must be copied on fragmentation."
  ;1;Type and len are byte swapped for the packet.*
  (type)
  ;1;Type of option see IP-Security-option.*
  (len)
  ;1;length of option see IP-Security-option-size.*
  (security-high)
  ;1;S field. byte swap when getting it in or out.*
  (security-low)
  (compartments-high)
  (compartments-low)
  (handling-h)
  (handling-l)
  (tcc-high)
  ;1;First 8 bits of the transmission control code.*
  (tcc-mid)
  ;1;Next 8 bits of the tcc.*
  (tcc-lsb)) 



(DEFSTRUCT (ip-source-and-record-route (:make-array (:type :art-8b)) (:conc-name "SRR-")
				       (:constructor nil) :size-symbol (:callable-constructors nil)
				       (:alterant alter-ip-source-and-record-route)
				       (:predicate nil) (:copier nil) (:type :array))
  "Source and Record route construct.  Loose source and record route option and the
   strict source and record route option are copied on fragmentation.  The record route
   option is put only in the first fragment."
  (type)
  ;1;Type of option*
  (len)
  ;1; Length of the option (type, len, pointer, (route data - 3)).*
  (pointer)
  ;1;The pointer to the route data.*
  (rd1)
  ;1;The first octet of route data.*
  ) 



(DEFSTRUCT (ip-stream-id (:make-array (:type :art-8b)) (:conc-name "STREAM-") (:constructor nil)
			 :size-symbol (:callable-constructors nil)
			 (:alterant alter-ip-stream-id) (:predicate nil)
			 (:copier nil) (:type :array))
  "SATNET stream id option.  Must be copied on fragmentation."
  (type)
  (len)
  (id-high)
  ;1;Identification of the SATNET stream.*
  (id-low)) 



(DEFSTRUCT (ip-timestamp (:make-array (:type :art-8b)) (:conc-name "TIME-") (:constructor nil)
			 :size-symbol (:callable-constructors nil)
			 (:alterant alter-ip-timestamp) (:predicate nil)
			 (:copier nil) (:type :array))
  "Time stamp option. Put only in the first fragment.
   Appears at most once in a datagram."
  (type)
  (len)
  (pointer)
  ((overflow 260) (flag 4))) 

;1;From the Ip-Server file.*


(DEFVAR *test-ip-protocol-id* 127 "2Testing purposes only.*") 


(DEFPARAMETER *ip-ethernet-type* 8
   "2IP Packet type for Ethernet. #x0800 byte swapped. RFC 894.*") 


(DEFVAR *ip-protocol-handler-alist* ()) 


(DEFVAR *udp-protocol-ip-id* 17)   ;1set this correctly*


(DEFVAR *ip-pkt-sent-list* () "2ip pkts sent*") 


(DEFVAR *ip-pkt-rvcd-list* () "2ip pkts rvcd*") 

(DEFVAR *ip-transmit-enabled* ())     ;19/24/85 - RWF - overriding enable mechanism added*

(DEFVAR *ip-receive-enabled* ()) 

;1;; DEBUG MODE!!!!!!*

(DEFVAR *ip-debug-mode* ())     ;19/24/85 - RWF - minimal debug output as default*

(DEFVAR *receive-debug-mode* ())     ;1  *

(DEFVAR *send-debug-mode* ())

;1;; If *RECORD-PACKET-MODE* T, will copy IP-PKT into history buffer*
;1;; along with information concerning time and mode.*
;1;; IP:Print-Recent-Packet-Headers used to display information*
(DEFVAR *record-packet-mode* ()) 

;1;; VERSION NUMBER*

(DEFCONSTANT ip-version 4) 

(DEFCONSTANT *min-header-bytes* 20) 

(DEFCONSTANT *min-header-32-bit* (/ *min-header-bytes* 4)) 

(DEFCONSTANT *max-header-bytes* 60 "2maximum number of bytes that can appear in header.*") 

(DEFCONSTANT *max-header-32-bit* (/ *max-header-bytes* 4)) 

;1; TIME TO LIVE*

(DEFCONSTANT *max-time-to-live* 50)

(DEFPARAMETER ip-maximum-routing-time 30)

(DEFVAR *max-ethernet-packet-bytes* 1500 "2Number of bytes in an Ethernet packet, less the 14 byte Ethernet header*")

(DEFVAR *max-arpanet-packet-bytes* 1004 "2MTU on the Arpanet*")

(DEFVAR *max-data-bytes* 8800 "2Maximum number of bytes of data that can be sent in any packet.*")

(DEFVAR *max-total-packet-bytes* (+ *max-header-bytes* *max-data-bytes* ))

(DEFVAR *max-total-packet-16-bit* (TRUNCATE *max-total-packet-bytes* 2))

(DEFVAR *min-data-bytes* 8) 

(DEFVAR *ip-protocol-mapping* '((:icmp . 1) (:tcp . 6) (:udp . 17))) 

;1;;IP Identification counter.*

(DEFVAR *ip-id-counter* (MOD (time:microsecond-time) 65536)) 

;1;; Other Machines*
;1;; A list of machines that do not respond to ARPs.*
;1;;   Each item in the list is a list of the following.*
;1;;      1. The IP address of the machine.*
;1;;      2. List of names and aliases of the machine.*
;1;;      3. Ethernet address of the machine.*
;1;;*
(DEFVAR other-machines nil "2List of machines that do not respond to ARPS.*")

(DEFVAR *proxy-arp-switch* nil "2When t, answer IP arp requests for remote networks in a proxy manner.*")

(DEFFLAVOR ip-error
	   ()
	   (system:network-error)) 

(DEFSIGNAL class-d-error ip-error ()
   "Invalid address") 

(DEFSIGNAL incomplete-routing-table ip-error () "Incomplete routing table") 

(DEFCONSTANT *ip-broadcast-address* #xFFFFFFFF)

(DEFPARAMETER *USE-BLT-P* T "2t if safe to blt packet contents*")

;1;list containing gateway addresses and their associated masks*
(DEFPARAMETER *gateway-addr-mask-list* '())

(DEFVAR *gateway-host-p* nil "2non-nil if si:local-host is a gateway*")

(DEFVAR *network-mask-list* nil)

(DEFPARAMETER *checksum-action* ())

(DEFVAR *ip-area* si:working-storage-area) 
