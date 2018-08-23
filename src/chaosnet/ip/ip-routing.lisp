;;; -*- Mode:Common-Lisp; Package:IP; Base:10; Fonts:(MEDFNT MEDFNB TR12BI) -*-

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
;1;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.*
;1;;*
;1;--------------------------------------------------------------------*
;1;;               IP-routing-table*
;1;;*
;1;; The table is a list of GATEWAYS and BRIDGES.*
;1;;*
;1;; This list is the routing table.  This is a list due to the large number*
;1;; of possible entries and the low number of actual entries to the *
;1;; table.  The contents of each entry is also a list and contain the*
;1;; following information:*
;1;;    Network - The network number.  Network number expressed as a 24 fixnum.*
;1;;              This includes the code for the address type.*
;1;;    Controller    - The controller object for the specific gateway.*
;1;;    Address - The list of IP gateway addresses of the gateways to the network.*
;1;;    Ttl     - A good guess at the time to use for Time to live.*
;1;;    Mask    - Subnet mask; if not subnetted will be default mask*
;1;;    Last    - The last host used.  This is to allow using other*
;1;;              hosts other than the last one used if there is a problem.*
;1;;*
;1;--------------------------------------------------------------------*


(DEFSTRUCT (ip-routing-list-type (:type list) (:conc-name "IP-ROUTING-") :size-symbol)
  (network)
  (controller)
  (address)
  (ttl)
  (mask)
  (LAST)) 

;1;;=============================================*
;1;; *
;1;; Resource for IP-ROUTING-LIST*
;1;;*

(DEFRESOURCE ip-routing-list
	     (&optional (net nil) (controller nil) (addr nil) (tl nil) (mask nil))
  "2The resource for the IP-ROUTING-LIST.*"
  :constructor (make-ip-routing-list-type) :initializer
  (SETF (ip-routing-network object) net
	(ip-routing-controller object) controller
	(ip-routing-address object) addr
	(ip-routing-ttl object) tl
	(ip-routing-mask object) mask)
  :free-list-size 5) 


(DEFUN allocate-ip-route (&optional (net nil) (controller nil) (addr nil) (tl nil) (mask nil))
  (ALLOCATE-RESOURCE 'ip-routing-list net controller addr tl mask)) 

(DEFVAR ip-routing-list () "2The Routing List for networks connected to this machine.*")

(DEFUN free-ip-route (route-entry)
  "2Deallocates the route-entry and deletes it from the from IP-ROUTING-LIST.*"
  (SETF ip-routing-list (DELETE route-entry ip-routing-list))
  ;1; Delete the entry from the list.*
  (DEALLOCATE-RESOURCE 'ip-routing-list route-entry)) 
;1;;*
;1;;================================================= *


(DEFVAR ip-routing-table ()
   "2Contains the Network information for sending to other nets.*"
   ) 

;1; Items to use in Peek (later).*

(DEFPARAMETER ip-pkts-bad-version 0 "2Number of packets with bad version number.*"
   ) 

(DEFPARAMETER ip-pkts-cant-reach-network 0 "2No. packets unable to reach their network.*"
   )  
 
;1;;==================================================*
;1;;*
;1;; LOCAL-BROADCAST-ADDRESS-P*
;1;;*
;1;; Returns t if the ip address is a local broadcast. *
;1;;*


(DEFUN local-broadcast-address-p (host-address)
  "2Returns T if the given address is a local broadcast.*"
  (COND
    ((OR (= host-address #xFFFFFFFF) (= host-address #x00000000)) t)
    (t
     (DOLIST (route-entry ip-routing-table)
       (WHEN (OR (= (LOGAND host-address (ip-routing-mask route-entry))
		    (LOGAND (ip-routing-network route-entry) (ip-routing-mask route-entry)))
		 (= (LOGAND host-address (get-default-mask host-address))
		    (LOGAND (ip-routing-network route-entry)
			    (get-default-mask (ip-routing-network route-entry))))) 
	 (RETURN
	   (OR (ZEROP (LOGAND host-address (LOGXOR #xFFFFFFFF (ip-routing-mask route-entry))))
	       (= #xFFFFFFFF
		  (LOGIOR (LOGAND host-address (LOGXOR #xFFFFFFFF (ip-routing-mask route-entry)))
			  (ip-routing-mask route-entry)))))))))	   ;1cond*
  )					   ;1defun*

;1;;==================================================*
;1;;*
;1;; GET NETWORK NUMBER*
;1;;*
;1;; Returns the IP network number of the given host.*
;1;; Host is assumed to be the full IP address.*
;1;;*

(DEFUN get-network-number (host)
  "2Returns the IP network number of the given host address as a 32-bit integer.
Note that this does not include the portion of the local address assigned to a subnet.*"
  (LET* ((host-address (COND ((NUMBERP host) host)
			     ((STRINGP host) (get-ip-address host))
			     ((SYMBOLP host) (get-ip-address host))
			     (t (FERROR 'invalid-host-argument-format 
					"~&This specification of a host is invalid: ~a" 
					host)))))
    (LOGAND (get-default-mask host-address) host-address))) 

;1;;==================================================*
;1;;*
;1;; GET DEFAULT MASK*
;1;;*
;1;; Returns the full 32 bit default mask of the*
;1;; given host.  Host is assumed to be the full IP*
;1;; address.*
;1;;*

(DEFUN get-default-mask (host-address &aux (host host-address))
  "2Returns the full 32 bit default mask of the given host.
    Host is assumed to be the full IP address.*"
  (COND
    ((OR (= #xFFFFFFFF host)
	 (= #x00000000 host))
     #xFFFFFFFF)
    ((ZEROP (LDB (BYTE 1 31) host))
     ;1;Class A address.*
     #xFF000000)
    ((ZEROP (LDB (BYTE 1 30) host))
     ;1;Class B address.*
     #xFFFF0000)
    ((ZEROP (LDB (BYTE 1 29) host))
     ;1;Class C address.*
     #xFFFFFF00)
    (t
     (ferror
        ' class-d-error "Invalid Host address for ~a" host-address)
     )))                         ;1;Class D reserved for later.*
	 

;1;;==============================================*
;1;;*
;1;; CLOSEST ADDRESSES TO NETWORK*
;1;;*
;1;; This returns the list of addresses that are closest*
;1;; to the addresses of the local host.  If none match *
;1;; pick the first one.*
;1;;*


(DEFUN closest-addresses-to-network (address-list)
  "2Returns a list of addresses that are the same networks as the local host.*"
  (COND ((MAPCAN #'(lambda (a &aux routing-entry)
		     (WHEN (AND (SETF routing-entry (get-routing-entry a))
				(EQ :direct (ip-routing-address (get-routing-entry a))))
		       (LIST a)))
		 address-list))
	(t (LIST (FIRST address-list)))))

;1;;==================================*


(DEFUN build-gateway-masks (&optional (local-only t))
  "2Acquires the subnet mask information from the namespace and calculates values
   for *network-mask-list* and *gateway-addr-mask-list* needed for building the
   routing table.*"
  (LET ((network-addr nil)
	(network-mask nil)
	(network-mask-list))
    (setf *network-mask-list* nil)
    (setf *gateway-addr-mask-list* nil)
    ;1; retrieve the mask information for associated with each network, if subnetted*
    (SETF network-mask-list 
	  (LOOP FOR lookup in name:*namespace-search-list*
		FOR value = (name:lookup-attribute-value
			      "INTERNET" :network
			      :internet-subnet-masks
			      :namespace lookup
			      :local local-only)
		UNTIL value
		FINALLY (RETURN value)))
    ;1; Insure that the network/mask information is parsed, in case of dotted decimal*
    (DOLIST (network-mask-pair network-mask-list)
      (SETF network-addr (FUNCALL (get :ip :network-address-parser) (FIRST network-mask-pair) NIL))
      (SETF network-mask (FUNCALL (get :ip :network-address-parser) (SECOND network-mask-pair) NIL))
      (PUSH (LIST network-addr network-mask) *network-mask-list*))
    ;1; Create *gateway-addr-mask-list* to be used in building the routing table*
    (DOLIST (host-obj (host:get-gateway-hosts :ip local-only))
      (DOLIST (addr (SEND host-obj :network-address-list :ip))
	(PUSH (LIST host-obj addr 
		    (DOLIST (network-mask *network-mask-list* (get-default-mask addr))
		      (WHEN (EQL (get-network-number addr)
				 (get-network-number (first network-mask)))
			(RETURN (second network-mask)))))
	      *gateway-addr-mask-list*)))
    )					   ;1 let*
  )					   ;1 defun*

;1;;==========================================*

(DEFUN subnet-bit-translation (network-address subnet-bits)
  "2Provides the actual subnet bit translations.*"
  (LET ((subnet-mask nil))
    (SETF network-address (FUNCALL (GET :ip :network-address-parser) network-address NIL))
    (WHEN (NULL subnet-bits)
      (SETF subnet-bits 0))
    (COND
      ((ZEROP network-address)
       (FERROR 'ip-error "Invalid network address for ~a "network-address))
      ((< network-address #x80000000)	   ;1class A address*
       (COND
	 ((<= 0 subnet-bits 23)
	  (SETF subnet-mask (+ #xFF000000 (* (1- (EXPT 2 subnet-bits)) (EXPT 2 (- 24 subnet-bits))))))
	 (t (FERROR 'ip-error "Number of Subnet bits specified is invalid for class A "))))
      ((< network-address #xC0000000)	   ;1class B address*
       (COND
	 ((<= 0 subnet-bits 15)
	  (SETF subnet-mask (+ #xFFFF0000 (* (1- (EXPT 2 subnet-bits)) (EXPT 2 (- 16 subnet-bits))))))
	 (t (FERROR 'ip-error "Number of Subnet bits specified is invalid for class B "))))
      ((< network-address #xE0000000)	   ;1class C address*
       (COND
	 ((<= 0 subnet-bits 7)
	  (SETF subnet-mask (+ #xFFFFFF00 (* (1- (EXPT 2 subnet-bits)) (EXPT 2 (- 8 subnet-bits))))))
	 (t (FERROR 'ip-error "Number of Subnet bits specified is invalid for class C "))))
      (t (FERROR 'class-d-error "Invalid class type for Network address ~a " network-address))
      )					   ;1cond*					
    subnet-mask)			   ;1let*
  )					   ;1defun*

;1;;===========================================================*

(DEFUN verify-mask (network-address network-mask)
  "2Verify that the network-address and network-mask are valid.
The possible values returned from this function are:

:INVALID-MASK
:INVALID-NETWORK-ADDRESS
T  =  no error*"

  (SETF network-address (FUNCALL (GET :ip :network-address-parser) network-address NIL))
  (SETF network-mask (FUNCALL (GET :ip :network-address-parser) network-mask NIL))
  (COND
    ((ZEROP network-address)
     :invalid-network-address)
    ((ZEROP network-mask)
     :invalid-mask)
    ((< network-address #x80000000)	   ;1 class A address*
     (COND
       ((<= #xFF000000 network-mask #xFFFFFFFE) T)
       (T :invalid-mask)))
    ((< network-address #xC0000000)	   ;1 class B address*
     (COND
       ((<= #xFFFF0000 network-mask #xFFFFFFFE) T)
       (T :invalid-mask)))
    ((< network-address #xE0000000)	   ;1 class C address*
     (COND
       ((<= #xFFFFFF00 network-mask #xFFFFFFFE) T)
       (T :invalid-mask)))
    (T :invalid-network-address)))

;1;;====================================================*
;1;;*
;1;; Reset-IP-routing-table - Resets Routing table to default state.*
;1;;    Function Build-Gateway-Masks used to provide a list of directly*
;1;;    attached gateways and their proper subnet mask.*
;1;;   *

(DEFUN reset-ip-routing-table (&optional (local-only t) (gateway-addr-mask-list nil))
  "2Flushes out old data and sets the table to the default state.
If the net is a member of the local net then put :DIRECT in for address.*"
  (LET (my-addr-list)
    (disable)
    (SETF ip-routing-table ())
    (setup-my-address)
    (SETF my-addr-list (sort ip:my-addresses #'<))
    (CLEAR-RESOURCE 'ip-routing-list () ())
    (setq net::controller-list  ;112-14-87 DAB*
	  (SORT net::controller-list #'(lambda (x y)
					 (< (SEND x :slot) (SEND y :slot)))))
    (COND (gateway-addr-mask-list)
	  (T (build-gateway-masks local-only)))
    ;1; Set all local (sub)nets as :direct*
    (LOOP for addr in my-addr-list
	  for cont in net::controller-list
	  as default-mask = (get-default-mask addr)
	  ;1; find a gateway on same network (*not* subnet)*
	  as routing-mask = (DOLIST (network-mask *network-mask-list* default-mask)
			      (WHEN (EQL (LOGAND default-mask addr)
					 (LOGAND default-mask (first network-mask)))
				(RETURN (second network-mask))))
          do (PUSH (allocate-ip-route addr cont :direct *max-time-to-live* routing-mask)
		   ip-routing-table))
    ;1; determine if si:local-host is an ip gateway*
    (SETF *gateway-host-p* (MEMBER si:local-host *gateway-addr-mask-list*
				   :key #'(lambda (g-a-m) (FIRST g-a-m))))
    ;1; do remote (sub)nets.*
    ;1; Process each entry in the routing table, adding new entries, until no more can be added.*
    (DO* ((idx 0 (1+ idx))
	  (rte (NTH idx ip-routing-table) (NTH idx ip-routing-table)))
	 ((OR (NULL rte)
	      ;1; if local-host is not a gateway, consider only directly attached gateways *
	      (AND (NOT *gateway-host-p*)
		   (NOT (EQ :direct (ip-routing-address rte))))))
      ;1; add new routing table entry for each*
      (MAPC #'(lambda (g-a-m)
		;1; retain only remote addresses not currently in routing table*
		(UNLESS (DOLIST (route-entry ip-routing-table)
			  (WHEN (EQL (LOGAND (SECOND g-a-m) (THIRD g-a-m))
				     (LOGAND (ip-routing-network route-entry)
					     (ip-routing-mask route-entry)))
			    (RETURN t)))
		  (PUSH-END (allocate-ip-route
			      (SECOND g-a-m)
			      (SECOND rte)
			      (IF (EQ :direct (ip-routing-address rte))
				  (DOLIST (addr (get-ip-addresses (FIRST g-a-m)))
				    (WHEN (EQL (LOGAND (ip-routing-network rte) (ip-routing-mask rte))
					       (LOGAND addr (ip-routing-mask rte)))
				      (RETURN addr)))
				  (ip-routing-address rte))
			      *max-time-to-live*
			      (THIRD g-a-m))
			    ip-routing-table)))
	    ;1; get all (sub)nets reachable through all gateways on this (sub)net*
	    (MAPCAN
	      ;1; get all (sub)nets reachable through this gateway*
	      #'(lambda (g)
		  (MAPCAN #'(lambda (g-a-m)
			      (WHEN (EQL g (FIRST g-a-m))
				(LIST g-a-m)))
			  *gateway-addr-mask-list*))
	      ;1; get gateways on this (sub)net*
	      (MAPCAN #'(lambda (g-a-m)
			  (WHEN (EQL (LOGAND (ip-routing-network rte) (ip-routing-mask rte))
				     (LOGAND (SECOND g-a-m) (THIRD g-a-m)))
			    (LIST (FIRST g-a-m))))
		      *gateway-addr-mask-list*))))
    (enable)
    ip-routing-table))
 
;1;;===========================================*
;1;;*
;1;; GET-ROUTING-ENTRY*
;1;;*
;1;; Returns the gateway routing entry for the given network*
;1;; address.  If no local gateway is found, scan for a remote*
;1;; gateway to use.  If no remote gateway matches, provide*
;1;; any remote gateway entry.  *

(DEFUN get-routing-entry (destination)
  "2Returns the gateway routing entry for the given network address.*"
  (BLOCK get-routing-entry
    (WHEN destination
      (UNLESS ip-routing-table
	(FERROR 'incomplete-routing-table "IP Routing table, incomplete please reset and try again."))
      (DOLIST (route-entry ip-routing-table)
	(WHEN (EQL (LOGAND destination (ip-routing-mask route-entry))
		   (LOGAND (ip-routing-network route-entry) (ip-routing-mask route-entry)))
	  (RETURN-FROM get-routing-entry route-entry)))
      ;1; If no gateway found, provide any gateway entry*
      (DOLIST (route-entry ip-routing-table)
	(UNLESS (EQ (ip-routing-address route-entry) :direct)
	  (RETURN-FROM get-routing-entry route-entry)))
      (RETURN-FROM get-routing-entry nil))))  

  
;1;;===============================================*
;1;;*
;1;; IP GET CONTROLLER*
;1;;*
;1;; Given an IP address this returns the appropriate*
;1;; controller on this machine for that address.*
;1;;*

(DEFUN ip-get-controller (host-addr)
  "2Returns the controller that is appropriate for this address on this machine.*"
  (LET* ((net-pos (POSITION host-addr my-addresses)))
    (WHEN net-pos
      (NTH net-pos net::controller-list)))) 

;1;;===============================================*
;1;;*
;1;;          Address translation functions.*
;1;;*
;1;; Returns the ethernet address of the host in*
;1;; question or the ethernet address of the *
;1;; gateway that is the first hop to this host.*
;1;;*
(DEFUN ip-get-host-ethernet-address (host &optional (controller (FIRST net:controller-list)))
  "2Returns the Ethernet address for this IP address.
    Host may be a string, ip-address, or host object.*"
  (LET* ((host-addr (COND ((NUMBERP host) host)
			  ((STRINGP host) (get-ip-address host))
			  ((SYMBOLP host) (get-ip-address host))
			  (t (FERROR 'invalid-host-argument-format 
				     "~&This specification of a host is invalid: ~a" 
				     host))))
	 (self controller)
	 (translation (ASSOC host-addr ethernet:ip-ether-address-translations :test #'EQ))
	 ether-addr
	 )
    (COND (translation (SECOND translation))
	  ((NOT (NUMBERP host-addr)) nil)  ;1Translation in hand.*
	  ((MEMBER host-addr my-addresses)
	   (send self :ethernet-address))
	  ((local-broadcast-address-p host-addr)   ;1Broadcast address*
	   ethernet:*ethernet-broadcast-address*)
	  (t
	   (SETF ether-addr (ethernet:get-ip-ethernet-address host-addr))
	   ;1; Wait a short bit to allow the reception of the ARP.*
	   (WHEN (NULL ether-addr)	   ;1Check again.*
	     (PROCESS-SLEEP 10)
	     (SETF ether-addr
		   (SECOND (ASSOC host-addr ethernet:ip-ether-address-translations :test 'EQUAL)))
	     (UNLESS ether-addr
	       (SETF ether-addr (check-other-machines host))))
	   ether-addr)) 
    ))

;1;*
;1; Other Machines is a place for machines that cannot respond to the ARP*
;1;   protocol.  This is checked when all else fails.*

(DEFUN check-other-machines (host)
  "2Checks OTHER-MACHINES to see if the host is there.  Returns the ethernet address if there.*"
  (LET ((host-info
	  (COND
	    ((NUMBERP host) (ASSOC host other-machines :test 'EQUAL))
	    ((OR (STRINGP host) (SYMBOLP host))
	     (LOOP for h in other-machines if (MEMBER host (SECOND h) :test 'STRING-EQUAL) do
		   (RETURN h) finally (RETURN ())))
	    (t nil))))
    (IF host-info
	(THIRD host-info)
	()))) 


(DEFUN print-ip-and-ethernet-address (host)
  (LET* ((addr (get-ip-address host))
	 (controller (ip-get-controller addr))
	 (e-net (ip-get-host-ethernet-address host controller)))
    (FORMAT t "~&These are the ways of showing the IP address for ~A." host)
    (FORMAT t "~&     Bignum   = ~16,8,'0r" addr)
    (FORMAT t "~&     Decimal  = ~3,'0d.~3,'0d.~3,'0d.~3,'0d" (LDB (BYTE 8 24) addr)
	    (LDB (BYTE 8 16) addr) (LDB (BYTE 8 8) addr) (LDB (BYTE 8 0) addr))
    (FORMAT t "~&     Octal    = ~3,'0o,~3,'0o,~3,'0o,~3,'0o" (LDB (BYTE 8 24) addr)
	    (LDB (BYTE 8 16) addr) (LDB (BYTE 8 8) addr) (LDB (BYTE 8 0) addr))
    (FORMAT t "~&     Net:node = ~d:~d - Address type = ~A" (get-network-number addr)
	    (COND
	      ((> addr 3221225472) (LDB (BYTE 8 0) addr))
	      ;1;Type C address*
	      ((> addr 2147483648) (LDB (BYTE 16 0) addr))
	      ;1;Type B address*
	      (t (LDB (BYTE 24 0) addr)))
	    ;1;Type A address*
	    (COND
	      ((> addr 3221225472) "C")
	      ;1;Type C address*
	      ((> addr 2147483648) "B")
	      ;1;Type B address*
	      (t "A")))
    ;1;Type A address*
    (IF (NULL e-net)
	(FORMAT t "~&ETHERNET address not found.")
	(PROGN
	  (FORMAT t "~%~%The Ethernet address for ~A is:" host)
	  (FORMAT t "~&      Bignum     = ~16,12,'0r" e-net)
	  (FORMAT t "~&      Decimal    = ~3,'0d.~3,'0d.~3,'0d.~3,'0d.~3,'0d.~3,'0d"
		  (LDB (BYTE 8 40) e-net) (LDB (BYTE 8 32) e-net) (LDB (BYTE 8 24) e-net)
		  (LDB (BYTE 8 16) e-net) (LDB (BYTE 8 8) e-net) (LDB (BYTE 8 0) e-net))
	  (FORMAT t "~&      Octal      = ~3,'0o,~3,'0o,~3,'0o,~3,'0o,~3,'0o,~3,'0o"
		  (LDB (BYTE 8 40) e-net) (LDB (BYTE 8 32) e-net) (LDB (BYTE 8 24) e-net)
		  (LDB (BYTE 8 16) e-net) (LDB (BYTE 8 8) e-net) (LDB (BYTE 8 0) e-net)))))) 
