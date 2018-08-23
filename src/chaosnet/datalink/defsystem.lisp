;; -*- Mode:Common-Lisp; Package:User; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                           RESTRICTED RIGHTS LEGEND
;;;
;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.

;
;;  Maintenance History:
;;
;;  16 SEP 87    MMG -- Original
;;  26 JAN 88     ab -- Add MAC-ENC file.   
;;  05 APR 89     ls -- Moved mac-enc to seperate defsystem because requires package TB
;;                      this package isn't needed on Explorers
;;
;;          NETWORK DATALINK LAYER -- This must be compiled after the Namespace and 
;;          Network-Support systems have been loaded:


(Defpackage "ETHERNET" (:Use "TICL" "LISP" "SYS") (:Nicknames "Ethernet" "ethernet"))

(DEFSYSTEM DATALINK
   (:NAME             "Network Data-Link Layer")
   (:PATHNAME-DEFAULT "SYS:DATALINK;")
   (:PATCHABLE        "SYS:PATCH.DATALINK;" PATCH)    ;added patch file name KTN 6/30
   (:WARNINGS-PATHNAME-DEFAULT
                      "SYS:CWARNS;DATALINK.LISP")
;  (:INITIAL-STATUS    :RELEASED)

   (:MODULE	       Defs	       	"Definitions")            ; Datalink definitions
   (:MODULE	       Enet	       	"Ethernet")               ; Ethernet definitions
;  (:MODULE            Starlan          "Starlan"                 ; Starlan definitions
;  (:MODULE            Token            "Token-Ring"              ; Token Ring definitions
   (:MODULE            Nubus-Enc        "NuBus-Enc")              ; NuBus Ethernet Controller
   (:MODULE            Nupie-Enc        "Nupie-Enc")              ; Nupi/E Ethernet Controller
;  (:MODULE            Ccb-Enc          "Ccb-Enc")                ; CCB Ethernet Controller
   (:MODULE	       Init		"Initializations")        ; Controller Initializations

   (:COMPILE-LOAD      Defs)
   (:COMPILE-LOAD-INIT Enet
			      (Defs)
			      (:Fasload Defs)
			      (:Fasload Defs))
   (:COMPILE-LOAD-INIT NuBus-Enc
		              (Defs Enet)
			      (:Fasload Defs Enet)
			      (:Fasload Defs Enet))
   (:COMPILE-LOAD-INIT Nupie-Enc
		              (Defs Enet)
			      (:Fasload Defs Enet)
			      (:Fasload Defs Enet))

   (:COMPILE-LOAD-INIT Init
			      (Defs Enet Nupie-Enc NuBus-Enc)
			      (:Fasload Defs Enet Nupie-Enc NuBus-Enc)
			      (:Fasload Defs Enet Nupie-Enc NuBus-Enc))
    
   ) ; Datalink


;;            Menus and Peek functionality for Datalink -- This must be compiled
;;            after the Datalink and Network-Support systems have been loaded:


(DEFSYSTEM DATALINK-DISPLAYS
   (:NAME             "Network Data-Link Displays")
   (:PATHNAME-DEFAULT "SYS:DATALINK;")
   (:PATCHABLE        "SYS:PATCH.DATALINK-DISP;" PATCH)
   (:WARNINGS-PATHNAME-DEFAULT
                      "SYS:CWARNS;DATALINK-DISPLAYS.LISP")
   (:INITIAL-STATUS    :RELEASED)

   (:MODULE            Menus
		              "Menus") ; Network Controller Menus
   (:MODULE            Peek
		              "Peek")  ; Network Controller Peek
   (:COMPILE-LOAD      Menus)
   (:COMPILE-LOAD      Peek)
   ) ; Datalink Displays


(DEFSYSTEM mx-datalink
  (:NAME "mx-datalink")
  (:pathname-default "SYS: datalink;")
  (:patchable "SYS: PATCH.MX-datalink;" PATCH) ;03.05.87 DAB
  (:warnings-pathname-default "SYS: CWARNS; MX-datalink.lisp") ;03.05.87 DAB
  (:module mac-enc "mac-enc")
  (:compile-load mac-enc)
  )