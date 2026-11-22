(in-package #:policy-protocol)

;;; Static analysis (brief: L2). Declared here; the methods that answer come
;;; with the SAT bridge (logic-protocol/sat) in policy-protocol 0.2.0, loaded
;;; lazily, so evaluating a policy never loads a solver. Until then each
;;; default method signals POLICY-ANALYSIS-UNAVAILABLE with a USE-VALUE
;;; restart whose value is the GF's result.
;;;
;;;   (policy-conflicts policy)          → list of (permit-rule forbid-rule witness)
;;;   (policy-unreachable-rules policy)  → rules that can never decide
;;;   (policy-subsumes-p a b)            → boolean, witness: every request B
;;;                                         permits, A permits
;;;   (policy-equivalent-p a b)          → boolean, witness

(defun %analysis-unavailable (operation policy)
  (restart-case (error 'policy-analysis-unavailable :operation operation :policy policy)
    (use-value (value)
      :report "Return a value instead of the analysis result."
      :interactive (lambda ()
                     (format *query-io* "Value to return: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      value)))

(defgeneric policy-conflicts (policy)
  (:documentation "Pairs of a permit and a forbid rule of POLICY that both apply
to some request, each with a witness request.")
  (:method (policy)
    (%analysis-unavailable 'policy-conflicts policy)))

(defgeneric policy-unreachable-rules (policy)
  (:documentation "The rules of POLICY that decide no request.")
  (:method (policy)
    (%analysis-unavailable 'policy-unreachable-rules policy)))

(defgeneric policy-subsumes-p (a b)
  (:documentation "True when every request B permits A permits; second value a
witness when not.")
  (:method (a b)
    (declare (ignore b))
    (%analysis-unavailable 'policy-subsumes-p a)))

(defgeneric policy-equivalent-p (a b)
  (:documentation "True when A and B decide every request alike; second value a
witness when not.")
  (:method (a b)
    (declare (ignore b))
    (%analysis-unavailable 'policy-equivalent-p a)))
