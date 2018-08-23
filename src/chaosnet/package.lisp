
(defpackage :net
  (:use :common-lisp)
  (:export
   *chaos-arp-requests-received*
   *medium-notify*
   check-arg
   ether-subnets
   chaos-subnet
   network-error))

(defpackage :ethernet
  (:use :common-lisp :net)
  (:export
   *my-address*))

(defpackage :chaos
  (:use :common-lisp :net))
