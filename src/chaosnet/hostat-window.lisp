;;; -*- Mode:Common-Lisp; Package::CHAOS; Base:10; Fonts: Courier, Medfnt, Medfnb, Courier, Medfnb; -*- 



1;;;                           RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;; Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.*

1;;------------------------------------------------------------------------------
;;                           KBD HOSTAT service routines
;;------------------------------------------------------------------------------*


(defun 4kbd-hostat* ()
  2"In a temporary window, show the user the status of various chaosnet hosts."*
  (using-resource (window tv::pop-up-finger-window)
     (setf (tv:sheet-truncate-line-out-flag window) 1) (funcall window :set-label 3"Hostat"*)
     (funcall window :set-process current-process)
     (tv:window-call (window :deactivate)
	(let ((*terminal-io* window))
	  (setq tv::kbd-terminal-time ())1;Window configuration stable.*
	  (hostat)
	  (tv::await-user-typeahead window))))) 


(when (fboundp 'tv::add-terminal-key)
  (funcall 'tv::add-terminal-key #\H '(kbd-hostat) 3"Show status of CHAOSnet hosts"*:typeahead))
