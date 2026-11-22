(in-package #:policy-protocol)

;;; Conditions (CLHS 9). Root POLICY-ERROR. logic-protocol's conditions
;;; (LOGIC-NO-BACKEND, LOGIC-BUDGET-EXCEEDED, ...) pass through
;;; EVALUATE-POLICY unwrapped, with their own restarts.
;;;
;;; Restarts, and who establishes them:
;;;
;;;   SKIP-RULE  PARSE-POLICY, MAKE-POLICY, LOAD-POLICY, around each rule, for
;;;              a POLICY-SYNTAX-ERROR in that rule: leave the rule out and go
;;;              on with the rest. Offered only for errors in a rule.
;;;   USE-VALUE  (value) the same functions, three sites:
;;;              - around each rule: VALUE is a replacement rule (a DSL rule
;;;                form such as (permit :action "x"), or a POLICY-RULE). It is
;;;                checked in the offending rule's place; when it is invalid
;;;                too, POLICY-SYNTAX-ERROR is signalled again for it.
;;;              - around each option (:combining, :default, the name): VALUE
;;;                replaces the option's value, and is checked again.
;;;              - LOAD-POLICY around the whole source, for a source that
;;;                cannot be read (bad JSON / TOML, a reader error, #. with
;;;                *READ-EVAL* NIL, not exactly one form, not a DEFPOLICY form):
;;;                VALUE is returned as LOAD-POLICY's result.
;;;              The analysis GFs' default methods, for
;;;              POLICY-ANALYSIS-UNAVAILABLE: VALUE is the GF's result.

(define-condition policy-error (error)
  ((policy :initarg :policy :initform nil :reader policy-error-policy
           :documentation "The policy concerned, or NIL (while parsing)."))
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Policy error"))))

(define-condition policy-syntax-error (policy-error)
  ((form :initarg :form :initform nil :reader policy-error-form
         :documentation "The offending datum: a rule form, an option value, a
condition, or the source text that could not be read.")
   (reason :initarg :reason :initform "" :reader policy-error-reason
           :documentation "A sentence saying what is wrong."))
  (:report (lambda (c s)
             (format s "Invalid policy form ~S: ~A"
                     (policy-error-form c) (policy-error-reason c)))))

(define-condition policy-request-error (policy-error)
  ((form :initarg :form :initform nil :reader policy-error-form
         :documentation "The offending request, or the part of it at fault.")
   (reason :initarg :reason :initform "" :reader policy-error-reason))
  (:report (lambda (c s)
             (format s "Invalid policy request ~S: ~A"
                     (policy-error-form c) (policy-error-reason c)))))

(define-condition policy-format-unsupported (policy-error)
  ((format :initarg :format :initform nil :reader policy-error-format))
  (:report (lambda (c s)
             (format s "No policy data reader for format ~S~@[ (load policy-protocol/settings for :json and :toml)~]"
                     (policy-error-format c)
                     (member (policy-error-format c) '(:json :toml))))))

(define-condition policy-analysis-unavailable (policy-error)
  ((operation :initarg :operation :initform nil :reader policy-error-operation
              :documentation "The analysis GF's name, e.g. POLICY-CONFLICTS."))
  (:report (lambda (c s)
             (format s "~S needs an analysis backend (the SAT bridge, logic-protocol/sat, lands in policy-protocol 0.2.0)"
                     (policy-error-operation c)))))

(defun skip-rule (&optional condition)
  "Invoke the SKIP-RULE restart (see the file header) for CONDITION, if any is
active; else return NIL."
  (let ((r (find-restart 'skip-rule condition)))
    (when r (invoke-restart r))))
