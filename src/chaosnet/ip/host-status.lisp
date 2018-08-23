;; -*- Mode: Common-Lisp; Package: HOST; Base: 10 -*-
;;;
;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.
;;================================================================================
;;================================================================================
;; IP STATUS SERVICE

(defflavor IP-STATUS-SERVICE
	   ((name :IP-STATUS)
	    (desirability .65))
	   (service-implementation-mixin)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(define-service-implementation 'IP-STATUS-SERVICE)

(defmethod (IP-STATUS-SERVICE :STATUS) (host)
  (list :IP (ip:icmp-echo host 2 10 nil)))
