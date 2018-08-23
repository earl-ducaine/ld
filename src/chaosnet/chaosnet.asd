
(asdf:defsystem :chaosnet
  :depends-on (;; alexandria
	       ;; ;;cffi
	       ;; uiop
	       ;; bordeaux-threads
	       ;; cl-aa
	       ;; cl-aa-misc
	       ;; cl-fad
	       ;; closer-mop
	       ;; trivial-features
	       ;; s-xml
	       ;; split-sequence
	       ;; cffi-libffi
	       )
  :license "MIT-ish"
  :author "VA"
  :description "MIT CHAOS Protocol, original Lisp"
  :components
  ((:file "package")
   (:file "chaos-misc-utils")
   (:file "chaos-defs")
   (:file "chaos-arp")))
