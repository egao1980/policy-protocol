(in-package #:policy-protocol)

;;; POLICY-EXPLAIN: the trace as text, for people and for the model.
;;;
;;; The text is English prose, one line per sentence, and always contains:
;;;   - the effect, downcased ("permit", "ask", "forbid", "no decision");
;;;   - for a rule decision: the deciding rule's id, its resource pattern
;;;     unless it is "*" (a glob as a quoted string, ~S; other patterns as
;;;     their canonical form, ~S), the deciding resource as a quoted string
;;;     (~S), and its condition when it has one, rendered in the canonical
;;;     lower-case surface form: atoms (rel arg...) with names bare and
;;;     integers as digits, (not C), (and C...), (or C...); e.g.
;;;     "(not (sandbox confined))";
;;;   - the feedback, when the decision has any;
;;;   - for a layered policy: the deciding layer's name (when it has one);
;;;   - for a default: the word "default";
;;;   - for :no-decision: the words "no rule";
;;;   - for unmet obligations: the word "obligation" and each unmet kind.

(defun %render-condition (c)
  "Canonical condition C in the lower-case surface form."
  (case (and (keywordp (first c)) (first c))
    (:not (format nil "(not ~A)" (%render-condition (second c))))
    ((:and :or) (format nil "(~(~A~)~{ ~A~})" (first c) (mapcar #'%render-condition (rest c))))
    (t (format nil "(~{~A~^ ~})" c))))

(defun %effect-text (effect)
  (if (eq effect :no-decision) "no decision" (string-downcase (symbol-name effect))))

(defun %write-explanation (decision s)
  (let ((rule (decision-rule decision))
        (layer (decision-layer decision)))
    (format s "Decision: ~A.~%" (%effect-text (decision-effect decision)))
    (when (and layer (policy-name layer))
      (format s "Deciding layer: ~A.~%" (policy-name layer)))
    (cond (rule
           (format s "Deciding rule: ~A (~A) on resource ~S.~%"
                   (policy-rule-id rule) (%effect-text (policy-rule-effect rule))
                   (decision-resource decision))
           (let ((pattern (policy-rule-resource rule)))
             (unless (equal pattern "*")
               (format s "Resource pattern: ~S.~%" pattern)))
           (let ((c (%rule-canonical-condition rule)))
             (when c
               (format s "Condition: ~A.~%" (%render-condition c)))))
          ((decision-default-p decision)
           (format s "No rule decided; the layer's default effect applies.~%"))
          (t
           (format s "There is no rule that decides this request; the caller's default applies.~%")))
    (when (decision-unmet-obligations decision)
      (format s "Unmet obligation kinds (the caller does not honour them):~{ ~A~}.~%"
              (remove-duplicates (mapcar #'first (decision-unmet-obligations decision))
                                 :test #'equal :from-end t)))
    (when (decision-feedback decision)
      (format s "Feedback: ~A~%" (decision-feedback decision)))))

(defun policy-explain (decision &optional stream)
  "Render DECISION's trace (see the file header). With STREAM, write there and
return NIL; else return the string."
  (if stream
      (progn (%write-explanation decision stream) nil)
      (with-output-to-string (s) (%write-explanation decision s))))
