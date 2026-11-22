(in-package #:policy-protocol)

;;; Evaluation (semantics: model.lisp). Runs on logic-protocol; never loads a
;;; SAT solver.

(defun policy-program (policy)
  "POLICY's compiled logic-protocol PROGRAM, made on first use with
logic-protocol:*LOGIC-BACKEND* (LOGIC-NO-BACKEND when there is none) and
the same object on every later call."
  (declare (ignore policy))
  (%not-implemented 'policy-program))

(defgeneric evaluate-policy (policy request &key budget obligations)
  (:documentation "The DECISION of POLICY (a POLICY, a LAYERED-POLICY or a
symbol naming one) for REQUEST (a POLICY-REQUEST or a request plist).
BUDGET: a logic-protocol budget designator for this request's logic work.
OBLIGATIONS: the obligation kinds the caller honours (default
*POLICY-OBLIGATIONS*). See model.lisp."))

(defmethod evaluate-policy (policy request &key budget obligations)
  (declare (ignore policy request budget obligations))
  (%not-implemented 'evaluate-policy))

(defun decision-why-not (decision rule)
  "NIL when RULE (a POLICY-RULE or a rule id) decided its layer in DECISION;
else a POLICY-WHY-NOT (model.lisp: WHY NOT). An id no layer has signals
POLICY-ERROR."
  (declare (ignore decision rule))
  (%not-implemented 'decision-why-not))
