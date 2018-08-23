
(defpackage :net
  (:use :common-lisp)
  (:export
   *chaos-arp-requests-received*
   *medium-notify*
   *network-interfaces*
   add-address-info
   check-arg
   ether-subnets
   chaos-subnet
   network-error
   network-interface))

(defpackage :ethernet
  (:use :common-lisp :net)
  (:export
   *my-address*
   *3com-ethernet-interface*
   *3com-owner*
   setup-3com))

(defpackage :chaos
  (:use :common-lisp :net))
