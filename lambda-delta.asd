;;; -*- lisp -*-

;;; This file was written by Earl DuCaine, it and all Lisp files in
;;; this repo are released under the MIT license.  Other files are
;;; noted independantly or, if not, are covered by the terms contained
;;; in the COPYING file in the root directory, i.e. the GPL.

(asdf:defsystem :lambda-delta
  :depends-on (:clx :trivial-gray-streams :cl-fad :iterate)
  :serial t
  :components
  ((:module lisp
	    :depends-on (:package)
	    :components
	    ((:file package)
	     (:file driver)
	     (:file build)))))
