;;; -*- mode: lisp ; syntax: ansi-common-lisp -*-

(proclaim '(optimize debug))
(format t "starting swank~%")
(finish-output)

;; standard quicklisp init file, since we'll be launching ecl without ~/.eclrc
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(when (probe-file  "/tmp/slime.2565")
  (delete-file "/tmp/slime.2565"))

(ql:quickload :swank :verbose t)

(funcall (read-from-string "swank-loader:init"))
(format t "starting swank~%")
(finish-output)
(funcall (read-from-string "swank:start-server")
         "/tmp/slime.2565")

(format t "swank ended!~%")
(finish-output)
