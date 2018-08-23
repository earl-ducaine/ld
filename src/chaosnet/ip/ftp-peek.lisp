;;; -*- Mode:Common-Lisp; Package: Ip; Fonts:(COURIER MEDFNB TR12BI); Base:10 -*-

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
;1;; Copyright (C) 1986, 1988 Texas Instruments Incorporated. All rights reserved.*
;1;*
;1; Support for peek servers*
;1;*

(DEFUN peek-servers-ftp (sg &optional (indent 4))
  "2Used in the SERVER-DESC-FUNCTION on the SERVERS-LIST of WHO-LINE-FILE-STATE-SHEET*"
  (LET ((control-connection (SYMEVAL-IN-STACK-GROUP 'control-connection sg)))
    (LIST ()
	  (tv:scroll-parse-item
	    (AND indent (> indent 0) (FORMAT () "~V@T" indent))
	    `(:function ,#'SYMEVAL-IN-STACK-GROUP
			(user-name ,sg) nil ("User: ~:[not logged in~;~:*~a~]")))
	  (tv:scroll-maintain-list
	    `(lambda (&aux data-connection)
	       (WHEN (SETF data-connection (SYMEVAL-IN-STACK-GROUP 'data-connection ,sg))
		 (LIST data-connection)))
	    `(lambda (data-connection)
	       (peek-servers-ftp-data-connection
		 ,control-connection data-connection ,sg ,indent))))))


(DEFUN peek-servers-ftp-data-connection (control-connection data-connection sg &optional (indent 0))
  (LIST ()
	(tv:scroll-parse-item
	  (AND indent (> indent 0) (FORMAT () "~V@T" indent))
	  "Data connection: "
	  `(:mouse
	     (nil :menu-choose
		  ("FTP data connection operations"
		   ("Close" :eval (SEND ,control-connection :close)
		    :documentation "Close (QUIT) this connection")
		   ("Abort" :eval (SEND ,control-connection :close :abort)
		    :documentation "Abort this connection")
		   ("Describe" :eval
		    (LET ((terminal-io typwin))
		      (SEND ,tv:selected-window :force-kbd-input '(DESCRIBE ,data-connection)))
		    :documentation "Describe this connection")
		   ("Inspect" :eval
		    (LET ((terminal-io typwin))
		      (SEND ,tv:selected-window :force-kbd-input '(INSPECT ,data-connection)))
		    :documentation "Inspect this connection"))
		  :documentation "Menu of things to do to this FTP data connection"
		  :bindings ((typwin ',tv:selected-window)))
	     :string ,(FORMAT nil "~a" data-connection)))
	(tv:scroll-maintain-list
	  `(lambda (&aux file-stream)
	     (WHEN (SETF file-stream (SYMEVAL-IN-STACK-GROUP 'file-stream ,sg))
	       (LIST file-stream)))
	  `(lambda (file-stream)
	     (SEND file-stream :peek-file-system (+ ,indent 2))))))


(DEFUN ftp-server-control-connection-menu (control-connection)
  (LET ((*terminal-io* (SEND tv:selected-window :typeout-window))
	(peek-window (COPY-LIST tv:selected-window)))
    (LET ((choice
	    (w:menu-choose
	      '(("Close" :value :close :documentation "Close the connection.")
		("Abort" :value :abort :documentation "Abort the connection.")
		("Describe" :value describe :documentation "Describe the connection.")
		("Inspect" :value inspect :documentation "Inspect the connection."))
	      :label (FORMAT () "~S" control-connection))))
      (CASE choice
	((:close :abort) (SEND control-connection :close (EQ choice :abort)))
	((DESCRIBE inspect) (SEND peek-window :force-kbd-input `(,choice ,control-connection)))))))


;1;============================================================*
;1;; --- STUFF FOR FTP PEEK FILE STATUS*

;1; Called by (:method ftp :peek-file-system)*

(DEFUN fs:peek-file-system-ftp (host &optional (indent 2))
  (LIST ()
	(tv:scroll-maintain-list
	  `(global:lambda () (GET ,host :active-ftp-control-connections))
	  `(global:lambda (unit) (peek-file-system-ftp-connection ,host unit ,indent)))))
					

(DEFUN peek-file-system-ftp-connection (host unit &optional (indent 0))
  "2Generate a scroll item describing an ftp control connection unit.*"
  (LIST ()
	(tv:scroll-parse-item
	  :leader 1
	  :mouse
	  `(nil :menu-choose
		("FTP connection operations"
		 ("Free" :eval (fs:free-ftp-connection ,unit)
		  :documentation "Free (deallocate) this connection for re-use")
		 ("Close" :eval (SEND ,unit :quit) :documentation
		  "Close (QUIT) this connection")
		 ("Abort" :eval (SEND ,unit :close-connections)
		  :documentation "Abort this connection")
		 ("Delete" :eval (SEND fs:*ftp-service* :delete-control-connection ,host ,unit)
		  :documentation "Delete this connection from the list (no closing done)")
		 ("Describe" :eval
		  (LET ((terminal-io typwin))
		    (SEND ,tv:selected-window :force-kbd-input '(DESCRIBE ,unit)))
		  :documentation "Describe this connection")
		 ("Inspect" :eval
		  (LET ((terminal-io typwin))
		    (SEND ,tv:selected-window :force-kbd-input '(INSPECT ,unit)))
		  :documentation "Inspect this connection"))
		:documentation "Menu of things to do to this FTP connection"
		:bindings ((typwin ',(SEND tv:selected-window :typeout-window))))
	  (AND indent (> indent 0) (FORMAT () "~V@THost unit ~A" indent unit))
	  `(:function
	     ,(FUNCTION
		(lambda (unit)
		  (LET ((ctl-conn (WHEN unit (SEND unit :control-connection))))
		    (WHEN ctl-conn
		      (SEND ctl-conn :status)))))
	     (,unit) nil (", control connection ~:[nonexistant~;in ~:*~a state~]")))
	(tv:scroll-maintain-list
	  `(lambda (&aux in-use)
	     (WHEN (SETF in-use (SEND ,unit :in-use))
	       (LIST in-use)))
	  `(lambda (in-use)
	     (LIST ()
		   (COND ((TYPEP in-use 'fs:ftp-data-stream-mixin)
			  (SEND in-use :peek-file-system (+ 2 ,indent)))
			 ((TYPEP in-use 'fs:ftp-directory-stream)
			  (peek-ftp-directory-stream in-use (+ 2 ,indent)))
			 (t
			  (tv:scroll-parse-item
			    (FORMAT nil "~V@THost unit not reserved by a stream" (+ 2 ,indent)))))))))) 


(DEFUN peek-ftp-directory-stream (STREAM &optional (indent 0))
  (tv:scroll-parse-item
    (AND indent (> indent 0) (FORMAT () "~V@T" indent))
    "Directory "
    (SEND (SEND stream :pathname) :string-for-printing)))



