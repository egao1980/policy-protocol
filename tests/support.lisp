(in-package #:policy-protocol/tests)

;;; Shared fixtures. Policies are built inside tests (never at load time), so
;;; the system loads on the contract stubs and each test fails on its own.
;;; Logic backend: the test-op binds logic-protocol:*LOGIC-BACKEND* to a
;;; logic-backend-datalog backend.

;;; The brief's plan-mode example (logic.md, "Surface sketches").
(defparameter *plan-mode-form*
  '(defpolicy plan-mode (:combining :forbid-overrides)
    (permit :action "bash" :when (sandbox :confined)
            :obligation (:run :confined))
    (ask    :action "bash" :when (not (sandbox :confined))
            :feedback "The sandbox is unavailable; this command runs unconfined.")
    (forbid :action "bash"
            :when (and (setting :sandbox.failIfUnavailable) (not (sandbox :confined)))
            :feedback "Sandbox required (sandbox.failIfUnavailable) and unavailable.")))

(defun plan-mode ()
  (parse-policy *plan-mode-form*))

(defun req (action resources &optional context)
  "A request plist; RESOURCES a string or a list."
  (list :action action
        :resources (if (listp resources) resources (list resources))
        :context context))

(defun decide (policy request &rest keys)
  (apply #'evaluate-policy policy request keys))

(defun effect (policy request &rest keys)
  (decision-effect (apply #'evaluate-policy policy request keys)))

(defun rule-id (decision)
  (let ((r (decision-rule decision)))
    (and r (policy-rule-id r))))

(defun policy-of (combining &rest rules)
  "An unnamed policy of DSL RULES."
  (make-policy :combining combining :rules rules))

(defun contains (needle haystack)
  (and (stringp haystack) (search needle haystack) t))
