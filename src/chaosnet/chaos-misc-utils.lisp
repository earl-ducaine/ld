
(in-package :net)

(defparameter *medium-notify* t)

;; Metering aids:
(defvar *peek-a-boo-list* '()
  "List of meters to be displayed by peek")

;;; This belongs in network-support:
(defmacro defvar-for-peek-a-boo (symbol value &optional (doc "some network variable"))
  `(progn 'compile
          (defvar ,symbol ,value ,doc)
          (pushnew '((symbol-value ',symbol),doc) *peek-a-boo-list* :test #'equal)))

(defmacro haulong (int)
    `(integer-length ,int))

(define-condition network-error (error) ())

;; (defmacro check-type (arg-name type &optional type-string)
;;   "Generate an error unless (TYPEP ARG-NAME 'TYPE).
;;   TYPE-STRING is a string to use in the error message, such as \"a
;;   list\".  If you omit it, it will be computed from TYPE."
;;   `(do () ((typep ,arg-name ',type))
;;      (setq ,arg-name
;; 	   (cerror '(:argument-value) nil 'wrong-type-argument
;; 		   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
;; 		   ',type ,arg-name ',arg-name
;; 		   ,(or type-string `(si::type-pretty-name ',type))))))

;;; (CHECK-ARG <VARIABLE> <PREDICATE> <MESSAGE>), for example:
;;; (CHECK-ARG STRING STRINGP "a string") signals an error if STRING is not a string.
;;; The error signals condition :WRONG-TYPE-ARGUMENT with arguments
;;; which are STRINGP (the predicate), the value of STRING (the losing value),
;;; the name of the argument (STRING), and the string "a string".
;;; If you try to proceed and do not supply a valid string to replace it,
;;; the error happens again.
;;; The second form may be the name of a predicate function, or it may be a full
;;; predicate form, as in:
;;; (CHECK-ARG A (AND (NUMBERP A) (< A 10.) (> A 0.)) "a number from one to ten" ONE-TO-TEN)
;;; ONE-TO-TEN is a symbol for the "type" which the argument failed to be.
;;; It is used instead of the second argument (the predicate) when signalling the error,
;;; since the second argument is not a suitable symbol.
;;; The value returned by CHECK-ARG is the argument's (original or respecified) value.
;;; In general, the condition :WRONG-TYPE-ARGUMENT is signalled with arguments
;;;    (1) A symbol for the desired type (NIL if not supplied)
;;;    (2) The bad value
;;;    (3) The name of the argument
;;;    (4) A string for the desired type.
(defmacro check-arg (arg-name predicate type-string &optional error-type-name)
  "Generate error if the value of ARG-NAME doesn't satisfy PREDICATE.
   PREDICATE is a function name (a symbol) or an expression to
   compute.  TYPE-STRING is a string to use in the error message, such
   as \"a list\".  ERROR-TYPE-NAME is a keyword that tells condition
   handlers what type was desired.  This macro is somewhat obsolete:
   you should probably be using CHECK-TYPE instead."
  (and (null error-type-name)
       (symbolp predicate)
       (setq error-type-name predicate))
  `(do ()
      (,(if (symbolp predicate)
	    `(,predicate ,arg-name)
	  predicate)
       ',arg-name)
     (setq ,arg-name
	   (cerror '(:argument-value) nil 'wrong-type-argument
		   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		   ',error-type-name ,arg-name ',arg-name ',type-string))))





;; (handler-bind
;;
;; (defmacro condition-case-if (cond-form variables body-form &rest clauses)
;;   "Like condition-case, but establishes condition handlers only if cond-form evaluates non-nil.
;;    refer to the documentation of condition-case for more information."
;;   ;; we don't use &body in the real arglist to avoid overriding
;;   ;; the special form of indentation on *initial-lisp-indent-offset-alist*
;;   (declare (arglist cond-form variables body-form &body clauses))
;;   (let* ((all-conditions
;; 	   (mapcan #'(lambda (clause)
;; 		       (macro-type-check-warning 'condition-case-if (car clause))
;; 		       (if (eq (car clause) ':no-error) nil
;; 			   (if (consp (car clause)) (copylist* (car clause))
;; 			       (cons (car clause) nil ))))
;; 		    clauses))
;; 	 (var (or (car variables) (gensym)))
;; 	 (no-error-clause (assoc :no-error clauses :test #'eq))
;; 	 (tag (gensym)))
;;     (if (null (cdr all-conditions))
;; 	(setq all-conditions (car all-conditions)))
;;     (if no-error-clause
;; 	(once-only (cond-form)
;; 	`(let ,variables
;; 	   (catch-continuation-if ,cond-form ',tag
;; 	       #'(lambda (,var)
;; 		  (select-memq (send ,var ':condition-names)
;; 		    . ,(remove no-error-clause (the list clauses) :test #'eq)))
;; 	       #'(lambda () . ,(cdr no-error-clause))
;; 	     (condition-bind-if ,cond-form ((,all-conditions 'condition-case-throw ',tag))
;; 	       (multiple-value-setq ,variables ,body-form)))) )
;; 	(once-only (cond-form)
;; 	`(catch-continuation-if ,cond-form ',tag
;; 	     #'(lambda (,var)
;; 		(select-memq (send ,var ':condition-names)
;; 		  . ,clauses))
;; 	     ()
;; 	   (condition-bind-if ,cond-form ((,all-conditions 'condition-case-throw ',tag))
;; 	     ,body-form)) )
;; 	)))
