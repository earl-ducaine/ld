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

(defsystem datalink
   (:name             "network data-link layer")
   (:pathname-default "sys:datalink;")
   (:patchable        "sys:patch.datalink;" patch)    ;added patch file name ktn 6/30
   (:warnings-pathname-default
                      "sys:cwarns;datalink.lisp")
;  (:initial-status    :released)

   (:module	       defs	       	"definitions")            ; datalink definitions
   (:module	       enet	       	"ethernet")               ; ethernet definitions
;  (:module            starlan          "starlan"                 ; starlan definitions
;  (:module            token            "token-ring"              ; token ring definitions
   (:module            nubus-enc        "nubus-enc")              ; nubus ethernet controller
   (:module            nupie-enc        "nupie-enc")              ; nupi/e ethernet controller
;  (:module            ccb-enc          "ccb-enc")                ; ccb ethernet controller
   (:module	       init		"initializations")        ; controller initializations

   (:compile-load      defs)
   (:compile-load-init enet
			      (defs)
			      (:fasload defs)
			      (:fasload defs))
   (:compile-load-init nubus-enc
		              (defs enet)
			      (:fasload defs enet)
			      (:fasload defs enet))
   (:compile-load-init nupie-enc
		              (defs enet)
			      (:fasload defs enet)
			      (:fasload defs enet))

   (:compile-load-init init
			      (defs enet nupie-enc nubus-enc)
			      (:fasload defs enet nupie-enc nubus-enc)
			      (:fasload defs enet nupie-enc nubus-enc))
    
   )


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
