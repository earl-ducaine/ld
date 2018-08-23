;;;  -*- Mode:Common-Lisp; Package:Fs; Base:10; Fonts: Medfnt, Medfnb, Courier, Medfnb; -*-

1;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.
;;; Copyright (c) 1981 Massachusetts Institute of Technology 


;; Chaosnet FILE SYSTEM Peek Functions
;; 10/1/86 rla - split out network and chaos parts into NET;PEEKFS and CHAOS;PEEKFS*


;; called by QFILE method at bottom of this file
(defun file-host-peek-file-system (host)
  (tv:scroll-maintain-list `(lambda ()
			      ',(SEND fs:*QFILE-SERVICE* :HOST-UNITS host))
			   `(lambda (host-unit)
			      (peek-file-system-host-unit host-unit)))) 


(defun 4peek-file-system-host-unit* (unit &optional (indent 2))
  2"Generate a scroll item describing a host unit"*
  (list ()
	(tv:scroll-parse-item :mouse
			      `(nil :menu-choose
				(3"Host-unit operations"*
				 (3"Reset"* :eval (send ,unit ':reset) :documentation
				  3"Reset this host-unit."*)
				 (3"Describe"* :eval
				  (let ((terminal-io typwin))
				    (send ,self :force-kbd-input '(describe ,unit)))
				  :documentation 3"Describe this host-unit."*)
				 (3"Inspect"* :eval
				  (let ((terminal-io typwin))
				    (send ,self :force-kbd-input '(inspect ,unit)))
				  :documentation 3"Inspect this host-unit."*))
				:documentation 3"Menu of things to do to this host-unit."*
				:bindings ((typwin ',(send self :typeout-window))))
			      (format () 3"~*VT3Host unit ~A, control connection in "* indent unit)
			      `(:function
				,(function
				  (lambda (unit)
				    (let ((conn (host-unit-control-connection unit)))
				      (if conn
					(symbol-name (chaos:state conn))
					3"NONEXISTANT-STATE"*))))
				(,unit)))
	(tv:scroll-maintain-list
	 `(lambda ()
	    (peek-file-system-host-unit-next-stream (cons (host-unit-data-connections ',unit) nil)
						    t))
	 `(lambda (stream)
	    (send stream ':peek-file-system (+ 2 ,indent)))
	 ()
	 #'(lambda (state &aux ns stream)
	     (multiple-value-setq (ns stream)
	       (peek-file-system-host-unit-next-stream state))
	     (values stream ns (null (peek-file-system-host-unit-next-stream ns t))))))) 


(defun 4peek-file-system-host-unit-next-stream* (state &optional dont-step &aux stream flag ns)
  2"Returns new state and next stream.  If DONT-STEP is specified, returns the current
state if there is a stream available, else NIL"*
  (setq flag (cdr state))
  (do ((s (car state) (cdr s)))
      ((null s)
       (setq ns ()))
    (setq ns s)
    (and (null flag) (setq stream (data-stream (car s) :input)) (neq stream t)
       (return (setq flag t)))
    (setq flag ())
    (and (setq stream (data-stream (car s) :output)) (neq stream t) (return (setq ns (cdr ns)))))
  (and (not (symbolp stream))
     (values (if dont-step
	       state
	       (progn
		 (rplaca state ns)
		 (rplacd state flag)))
	     stream))) 


;; QFILE
(defmethod (qfile :peek-file-system) (host)
  (fs:file-host-peek-file-system host))

;(compile-flavor-methods qfile)  ;12-18-87 DAB Why is this here?
