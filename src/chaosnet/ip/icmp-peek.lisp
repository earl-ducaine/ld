;;; -*- Mode:Common-Lisp; Package:IP; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

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

;1;; Modified to fit into 604x432 window on MicroExplorer or 688x432 window on Explorer*
;1;; Made everything output to a single column - 11/24/87 CAT*
(DEFUN ICMP-PEEK (IGNORE)
  "2Top level of the PEEK display of IP*"
  (LIST ()
	(TV:SCROLL-PARSE-ITEM "ICMP Activity at "
			      '(:function time:print-current-time (nil)))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-PACKETS-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. ICMP Packet~:p Sent ")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-PACKETS-RECEIVED (,*ICMP-HANDLER*) NIL
	   (" ~12D. ICMP Packet~:p Received ")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-PACKETS-DISCARDED (,*ICMP-HANDLER*) NIL
	   (" ~12D. Number of ICMP packet~:p discarded.    ")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-REDIRECTS-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Number of ICMP redirect~:p sent.")))
	(TV:SCROLL-PARSE-ITEM
	`(:FUNCTION ICMP-HANDLER-REDIRECTS-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Number of ICMP redirect~:p received.")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ECHO-REQ-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Echo request packet~:p sent.")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ECHO-REQ-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Echo request packet~:p received,")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ECHO-REPLY-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Echo reply packet~:p sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ECHO-REPLY-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Echo reply packet~:p received.")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DESTINATION-UNREACHABLES-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Destination unreachable packet~:p sent.")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-NET-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Net unreachable sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-HOST-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Host unreachable sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-PROTOCOL-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Protocol unreachable sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-PORT-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Port unreachable sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-FRAGMENTATION-NEEDED-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Fragmentation needed sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-SOURCE-ROUTE-FAILED-SENT (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Source route unreachable sent,")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DESTINATION-UNREACHABLES-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Destination unreachable packet~:p received.")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-NET-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Net unreachable received,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-HOST-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Host unreachable received,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-PROTOCOL-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Protocol unreachable received,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-PORT-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Port unreachable received,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-FRAGMENTATION-NEEDED-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Fragmentation needed received,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-DU-SOURCE-ROUTE-FAILED-RCVD (,*ICMP-HANDLER*) NIL
	   ("   ~12D. Source route unreachable received,")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TIME-REQUESTS-REPLIED (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time request~:p sent,")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TIME-REQUESTS-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time repl~@p received")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-PARAMETER-PROBLEMS-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Parameter problem~:p sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-PARAMETER-PROBLEMS-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Parameter problem~:p received")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TTL-EXCEEDED-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time to live exceeded sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TTL-EXCEEDED-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time to live exceeded received")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TIME-TO-REASSEMBLE-EXCEEDED-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time to reassemble exceeded sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-TIME-TO-REASSEMBLE-EXCEEDED-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Time to reassemble exceeded received")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-S-QUENCH-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Source quench~:p sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-S-QUENCH-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Source quench~:p received")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ADDRESS-MASK-REQUEST-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Address mask request~:p sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ADDRESS-MASK-REQUEST-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Address mask request~:p received")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ADDRESS-MASK-REPLY-SENT (,*ICMP-HANDLER*) NIL
	   (" ~12D. Address mask repl~@p sent")))
	(TV:SCROLL-PARSE-ITEM
	 `(:FUNCTION ICMP-HANDLER-ADDRESS-MASK-REPLY-RCVD (,*ICMP-HANDLER*) NIL
	   (" ~12D. Address mask repl~@p received"))))) 
