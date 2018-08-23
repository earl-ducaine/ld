;;-*- Mode:COMMON-LISP; Package:Net; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*-

;1;;                           RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;                     TEXAS INSTRUMENTS INCORPORATED.*
;1;;                              P.O. BOX 2909*
;1;;                           AUSTIN, TEXAS 78769*
;1;;                                 MS 2151*
;1;;*
;1;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.*
;1;*

(shadow 'file-stream-mixin 'fs)
(defpackage 4"3CHAOS*"* (:use "3TICL*" "3LISP*" "3SYS*")
  (:shadow "3OPEN*" "3STATUS*" "3CLOSE*" "3LISTEN*" "3FINISH*"))

(export 'fs:host-unit-lifetime 'fs)

(export 'chaos:(accept ans-op answer answer-string answered-state assure-enabled close-conn cls-op
		       cls-received-state conn-foreign-address conn-plist conn-read-pkts conn-state
		       conn-window-available connect contact-name dat-op data-available	disable
		       enable eof-op fast-answer-string	finish-conn first-data-word-in-pkt foreign-state
		       forward-all get-next-pkt	get-pkt	host-data host-down-state inactive-state
		       interrupt-function listen listening-state los-op	los-received-state lsn-op
		       make-stream may-transmit	open-foreign-connection	open-state open-stream
		       opn-op pkt-link pkt-nbytes pkt-opcode pkt-string	print-all-pkts print-conn
		       print-pkt reject	remove-conn reset return-pkt rfc-op rfc-received-state
		       rfc-sent-state send-pkt send-string send-unc-pkt	server-alist set-pkt-string
		       simple wait
		       eval-server-on remote-eval  ; DAB 04-24-89
		       find-hosts-or-lispms-logged-in-as-user shout notify-all-lms notify  ; DAB 04-24-89
		       ) 'chaos)

;1; Note that *datalink1 must be loaded before chaosnet.*
(defsystem 4Chaosnet*
  (:name             "3Chaosnet*")
  (:short-name       "3Chaos*")
  (:pathname-default "3SYS: CHAOSNET;*")
  (:patchable        "3SYS: PATCH.CHAOSNET;*" PATCH)
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")     
  (:module medium    "3CHAOS-MEDIUM*")
  (:module chsdefs   "3CHAOS-DEFS*")
  (:module chsmain   "3CHAOS-NCP*")
  (:module chsuser   "3CHAOS-USER*")
  (:module addr-res  "3chaos-arp*")
  (:module chsroute  "3CHAOS-ROUTING*")
  (:module who       "3chaos-who-am-i*")
  (:module init      "3Chaos-initialization*")
  (:component-systems qfile
		  band-transfer
		  debug-servers
		  eval
		  hostat
		  ;time
		  )
  (:compile-load chsdefs)
  (:compile-load-init chsmain 
		  (chsdefs) 
		  (:fasload chsdefs) 
		  (:fasload chsdefs))
  (:compile-load-init chsuser 
		  (chsdefs chsmain) 
		  (:fasload chsdefs chsmain) 
		  (:fasload chsdefs chsmain))
  (:compile-load-init init 
		  (chsdefs chsmain)
		  (:fasload chsdefs chsmain)
		  (:fasload chsdefs chsmain))
  (:compile-load-init chsroute
		  (chsdefs chsmain)
		  (:fasload chsdefs chsmain)
		  (:fasload chsdefs chsmain))
  (:compile-load-init addr-res
		  (chsdefs) 
		  (:fasload chsdefs) 
		  (:fasload chsdefs))
  (:compile-load-init medium
		  (chsdefs) 
		  (:fasload chsdefs) 
		  (:fasload chsdefs))
  (:compile-load-init who
		  (chsdefs) 
		  (:fasload chsdefs) 
		  (:fasload chsdefs)))


(defsystem 4chaosnet-window*
  ;1; Requires that the Network-support, Window-system and Zmacs have been loaded.*
  (:short-name "3chaos-window*")
  (:pathname-default       "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet-window.lisp*")
  (:module chaos-menus     "3CHAOS-MENUS*")
  (:component-systems      converse
		       notify
		       hostat-window)
  (:compile-load chaos-menus))

(defsystem 4chaosnet-peek*
  ;1; Requires that the Peek, Network-support, Window-system and Zmacs have been loaded.*
  (:short-name "3chaos-peek*")
  (:pathname-default       "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet-peek.lisp*")
  (:module chaos-peek      ("3peekn*" "3peekfs*"))
  (:compile-load chaos-peek))

;1;;*
;1;; Component defsystems.*
;1;;*

(defsystem 4qfile*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module definitions "3qfile-definitions*")
  (:module streams "3qfile-streams*")
  (:module source ("3qfile-server*" "3qfile*"))
  (:compile-load definitions)
  (:compile-load-init streams 
		  (definitions) 
		  (:fasload definitions)
		  (:fasload definitions))
  (:compile-load-init source  
		  (definitions streams) 
		  (:fasload definitions streams)
		  (:fasload definitions streams)))

(defsystem 4band-transfer*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module source ("3band*" "3remote-disk*"))
  (:compile-load source))

(defsystem 4debug-servers*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module source "3debug-servers*")
  (:compile-load source))

(defsystem 4eval*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module source "3eval*")
  (:compile-load source))

;(defsystem 4time*
;  (:pathname-default "3SYS: CHAOSNET;*")
;  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
;  (:module source "3time*")
;  (:compile-load source))

(defsystem 4hostat*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module source "3hostat*")
  (:compile-load source))

(defsystem 4hostat-window*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:module window "3hostat-window*")
  (:compile-load window))

(defsystem 4converse*
  (:name "3Converse*")
  (:documentation "3Send or receive messages from another Explorer monitor*")
  (:default-menu-column :PROGRAMS)
  (:default-system-key #\C)
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:instance-type    :FLAVOR)
  (:instance-finder  ZWEI::CONVERSE-FRAME)
  (:instance-creator T)
  (:module source "3conver*")
  (:compile-load source))


(defsystem 4notify*
  (:pathname-default "3SYS: CHAOSNET;*")
  (:warnings-pathname-default "3sys:cwarns;chaosnet.lisp*")
  (:component-systems converse)
  (:module source "3notify*")
  (:compile-load source))





