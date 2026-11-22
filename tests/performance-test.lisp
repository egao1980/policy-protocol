(in-package #:policy-protocol/tests)

;;; Per-request cost: a policy runs on every tool call. ~200 rules, each
;;; request evaluated with :ASSUME on the one compiled program, under a small
;;; step budget (logic-protocol budget designator: an integer is a step
;;; count).

(defparameter *request-budget* 20000
  "Steps one request may take with a 200-rule policy.")

(defun big-policy-rules ()
  (append
   (loop for i below 100
         collect `(permit :id ,(format nil "p~D" i) :action ,(format nil "tool-~D" i)
                          :resource ,(format nil "dir-~D/*" i)))
   (loop for i below 60
         collect `(forbid :id ,(format nil "f~D" i) :action ,(format nil "tool-~D" (* 2 i))
                          :resource ,(format nil "dir-~D/secret*" (* 2 i))
                          :feedback "secret"))
   (loop for i below 40
         collect `(ask :id ,(format nil "a~D" i) :action ,(format nil "tool-~D" i)
                       :resource (:prefix ,(format nil "dir-~D/tmp" i))
                       :when (and (mode :plan) (not (sandbox :confined)))))))

(defun big-requests ()
  (loop for i below 200
        for tool = (mod (* 7 i) 110)
        collect (req (format nil "tool-~D" tool)
                     (list (format nil "dir-~D/~[a.lisp~;secret.key~;tmp/x~;b.lisp~]" tool (mod i 4))
                           (format nil "dir-~D/c.lisp" tool))
                     (case (mod i 3)
                       (0 '((mode :plan)))
                       (1 '((mode :plan) (sandbox :confined)))
                       (t '())))))

(defun expected-effect (request)
  "The decision by hand, for the same rules (forbid-overrides)."
  (destructuring-bind (&key action resources context) request
    (let ((i (parse-integer action :start 5))
          (plan (and (member '(mode :plan) context :test #'equal)
                     (not (member '(sandbox :confined) context :test #'equal)))))
      (flet ((effect-of (r)
               (let ((dir (format nil "dir-~D/" i)))
                 (flet ((under (prefix) (eql 0 (search prefix r))))
                   (cond ((and (evenp i) (< i 120) (under (format nil "~Asecret" dir))) :forbid)
                         ((and plan (< i 40) (under (format nil "~Atmp" dir))) :ask)
                         ((and (< i 100) (under dir)) :permit)
                         (t nil))))))
        (let ((effects (mapcar #'effect-of resources)))
          (cond ((member :forbid effects) :forbid)
                ((member :ask effects) :ask)
                ((member nil effects) :no-decision)
                (t :permit)))))))

(deftest two-hundred-rules-under-a-small-budget
  (let* ((p (make-policy :name "big" :rules (big-policy-rules)))
         (program (policy-program p))
         (base (logic-protocol:program-base-facts program))
         (wrong 0))
    (ok (= 200 (length (policy-rules p))))
    (dolist (r (big-requests))
      (unless (eq (decision-effect (evaluate-policy p r :budget *request-budget*))
                  (expected-effect r))
        (incf wrong)))
    (ok (zerop wrong) (format nil "~D of 200 decisions wrong" wrong))
    (ok (eq program (policy-program p)) "compiled once, never rebuilt per request")
    (ok (equal base (logic-protocol:program-base-facts program))
        "requests are assumptions: the program's base facts are unchanged")))

(deftest the-budget-is-enforced
  (let ((p (make-policy :name "big" :rules (big-policy-rules)))
        (r (req "tool-4" '("dir-4/a.lisp"))))
    (policy-program p)
    (ok (signals (evaluate-policy p r :budget 1) 'logic-protocol:logic-budget-exceeded))
    (ok (eq :permit (decision-effect
                     (handler-bind ((logic-protocol:logic-budget-exceeded
                                      (lambda (c) (declare (ignore c)) (continue))))
                       (evaluate-policy p r :budget 1))))
        "CONTINUE finishes with the right decision")
    (ok (eq :permit (decision-effect (evaluate-policy p r :budget *request-budget*)))
        "a failed request leaves the policy usable")))
