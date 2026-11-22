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

(defun policy-explain (decision &optional stream)
  "Render DECISION's trace (see the file header). With STREAM, write there and
return NIL; else return the string."
  (declare (ignore decision stream))
  (%not-implemented 'policy-explain))
