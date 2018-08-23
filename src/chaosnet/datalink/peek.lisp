;;         -*- Mode:Common-Lisp; Package:Net; Base:10.; Fonts:(cptfont); -*-

;;;
;;;                                  RESTRICTED RIGHTS LEGEND
;;;
;;;        Use, duplication, or disclosure by the Government is subject to
;;;        restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;        Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                            TEXAS INSTRUMENTS INCORPORATED.
;;;                                     P.O. BOX 2909
;;;                                  AUSTIN, TEXAS 78769
;;;                                        MS 2151

;;;        Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.


;;-------------------------------------------------------------------------------------------- 
;;                              NETWORK CONTROLLER PEEK INTERFACE
;;-------------------------------------------------------------------------------------------- 

(Defvar CONTROLLER-HEADER-STRING
   "Type    Slot Subnet      #-In     #-Out   Aborted      Lost FCS-Error   Timeout   Too-Big")

(Defun PEEK-A-BOO-PARSE (counter)
  "PEEK Function to produce PARSE-ITEM for each item on PEEK-A-BOO-LIST."
  (if (consp counter)
    (tv:scroll-parse-item `(:function eval (,(car counter)) nil ("~@10A  " 10 t))
			  `(:string ,(second counter) 45))
    (tv:scroll-parse-item `(:function symbol-value (,counter) nil ("~@10A  " 10 t))
			  `(:string ,(string counter) 45))))

(Defun PEEK-CONTROLLER-DATA (cont)
  "PEEK function to produce PARSE-ITEM  for each controller."
  (tv:scroll-parse-item `(:string ,(send cont :board-type) 8)
			`(:function ,cont (:slot) nil ("~16,4R" 4 t))
			`(:function ,cont (:subnet) nil ("~7D" 7 t))
			`(:function ,cont (:pkts-received) nil ("~10D" 10 t))
			`(:function ,cont (:pkts-transmitted) nil ("~10D" 10 t))
			`(:function ,cont (:jam-count) nil ("~10D" 10 t))
			`(:function ,cont (:pkts-lost) nil ("~10D" 10 t))
			`(:function ,cont (:fcs-errors) nil ("~10D" 10 t))
			`(:function ,cont (:transmit-time-outs) nil ("~10D" 10 t))
			`(:function ,cont (:pkts-too-big-to-receive) nil ("~10D" 10 t)))) 

(Defun ETHERNET-PEEK (ignore)
  "Displays ethernet meters, and routing table"
  (list nil  
	(tv:scroll-parse-item "")
        (tv:scroll-parse-item Net:controller-header-string)
        (tv:scroll-maintain-list #'(lambda () net:controller-list)
                                 #'net:peek-controller-data)
	(tv:scroll-parse-item "")
        (tv:scroll-parse-item "")
	(tv:scroll-parse-item "        ============ Ethernet Meters ============")
        (tv:scroll-parse-item "")
	(tv:scroll-maintain-list #'(lambda () net:controller-meters)
                                 #'net:peek-a-boo-parse)))
        

(Add-To-Peek-Menu "ETHERNET" Net:Ethernet-Peek "Display the ETHERNET statistics")



