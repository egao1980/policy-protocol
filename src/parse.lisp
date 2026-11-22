(in-package #:policy-protocol)

;;; Building policies. The S-expression DSL (DEFPOLICY, PARSE-POLICY), rule
;;; lists (MAKE-POLICY) and data (LOAD-POLICY) all end in the same model;
;;; POLICY-FORM is its canonical data form, the same whatever the source.
;;;
;;; DSL:  (defpolicy NAME (OPTION...) BODY...)
;;;   NAME     a symbol (policy name: its SYMBOL-NAME downcased) or a string.
;;;   OPTIONS  a plist: :combining (:forbid-overrides | :last-match), :default
;;;            (:permit | :ask | :forbid). Unknown option or bad value:
;;;            POLICY-SYNTAX-ERROR with USE-VALUE (the option's value).
;;;   BODY     rules (model.lisp), and at most one (default EFFECT), which is
;;;            the same as the :default option (both given: syntax error).
;;; Heads (DEFPOLICY, PERMIT, ASK, FORBID, DEFAULT, NOT, AND, OR) are compared
;;; by SYMBOL-NAME, so a form read in any package works.
;;;
;;; Canonical form (POLICY-FORM), keys always present and in this order:
;;;   (:name NAME-OR-NIL :combining MODE :default EFFECT-OR-NIL
;;;    :rules ((:id ID :effect EFFECT :action GLOB :resource PATTERN
;;;             :when CONDITION-OR-NIL :obligations (OBLIGATION...)
;;;             :feedback STRING-OR-NIL) ...))
;;; with PATTERN, CONDITION and OBLIGATION canonical (model.lisp). Only
;;; strings, integers, NIL and the protocol's own keywords occur in it.
;;;
;;; DEFPOLICY expands into a call of PARSE-POLICY on the quoted form at load
;;; (run) time, so an invalid policy signals POLICY-SYNTAX-ERROR with its
;;; restarts when the DEFPOLICY form is evaluated, never at macroexpansion.
;;; It registers the policy under NAME (FIND-POLICY) and returns it.

(defvar *policies* (make-hash-table :test 'eq)
  "DEFPOLICY's registry: symbol → POLICY.")

(defun find-policy (name)
  "The policy DEFPOLICY registered under the symbol NAME, or NIL."
  (check-type name symbol)
  (values (gethash name *policies*)))

(defun (setf find-policy) (policy name)
  (check-type name symbol)
  (check-type policy (or null policy layered-policy))
  (if policy
      (setf (gethash name *policies*) policy)
      (progn (remhash name *policies*) nil)))

(defmacro defpolicy (name (&rest options) &body body)
  "Define the policy NAME (see the file header): parse at run time, register
under NAME, return the POLICY."
  (check-type name symbol)
  `(setf (find-policy ',name)
         (parse-policy '(defpolicy ,name ,options ,@body))))

(defun %not-implemented (name)
  (error "policy-protocol: ~S is not implemented yet (L1 contract stub)" name))

(defun parse-policy (form)
  "The POLICY described by FORM, a (DEFPOLICY NAME (OPTION...) BODY...) list
(see the file header). Restarts: SKIP-RULE and USE-VALUE per rule, USE-VALUE
per option (conditions.lisp)."
  (declare (ignore form))
  (%not-implemented 'parse-policy))

(defun make-policy (&key name (combining :forbid-overrides) default rules)
  "A POLICY from keyword arguments: NAME (string, symbol or NIL), COMBINING,
DEFAULT, RULES (DSL rule forms or POLICY-RULEs, in order). Same checks and
restarts as PARSE-POLICY."
  (declare (ignore name combining default rules))
  (%not-implemented 'make-policy))

(defun make-layered-policy (&rest layers)
  "A LAYERED-POLICY of LAYERS, outer first. A layer is a POLICY, a
LAYERED-POLICY (flattened in place) or a symbol naming one (FIND-POLICY).
Anything else signals POLICY-SYNTAX-ERROR."
  (declare (ignore layers))
  (%not-implemented 'make-layered-policy))

(defun policy-form (policy)
  "POLICY's canonical data form (see the file header). For a POLICY only."
  (declare (ignore policy))
  (%not-implemented 'policy-form))

(defun make-policy-request (&key action resources context)
  "A POLICY-REQUEST (model.lisp: REQUESTS). RESOURCES NIL means (\"*\").
Invalid parts signal POLICY-REQUEST-ERROR."
  (declare (ignore action resources context))
  (%not-implemented 'make-policy-request))
