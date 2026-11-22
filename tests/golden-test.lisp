(in-package #:policy-protocol/tests)

;;; ai-agent-protocol's permission semantics reproduced with :last-match
;;; policies (brief: golden-permissions-test). The harness below is what
;;; ai-agent-protocol's AGENT-PERMISSION becomes in L4a: one :last-match
;;; layer per rule list, the tool default as that layer's :default, and the
;;; policies as inner layers that only tighten.

(defun golden->effect (e)
  (ecase e (:allow :permit) (:ask :ask) (:deny :forbid)))

(defun effect->golden (e)
  (ecase e (:permit :allow) (:ask :ask) (:forbid :deny) (:no-decision nil)))

(defun golden-rule-form (spec id)
  "ai-agent-protocol's (ACTION RESOURCE EFFECT &key feedback) as a DSL rule."
  (destructuring-bind (action resource effect &key feedback) spec
    `(,(ecase effect (:allow 'permit) (:ask 'ask) (:deny 'forbid))
      :id ,id :action ,action :resource ,resource
      ,@(and feedback (list :feedback feedback)))))

(defun golden-rules (specs prefix)
  (loop for spec in specs
        for i from 0
        collect (golden-rule-form spec (format nil "~A~D" prefix i))))

(defun golden-layer (name rule-forms &optional default)
  (make-policy :name name :combining :last-match :default default :rules rule-forms))

(defun golden-agent-decision (row)
  "AGENT-PERMISSION's decision for ROW (golden-data.lisp) through policies."
  (destructuring-bind (&key rules settings approved policies tool-default action
                         resources &allow-other-keys)
      row
    (let* ((request (list :action action :resources resources))
           (base-rules (append (golden-rules rules "r") (golden-rules settings "s")))
           (base (golden-layer "rules" base-rules (golden->effect tool-default))))
      ;; An ask is answered by the saved and "always" rules, appended last;
      ;; only an allow or a deny answers it.
      (when (and approved (eq :ask (decision-effect (evaluate-policy base request))))
        (let ((answered (golden-layer "approved"
                                      (append base-rules (golden-rules approved "a")))))
          (when (member (decision-effect (evaluate-policy answered request))
                        '(:permit :forbid))
            (setf base answered))))
      (evaluate-policy
       (apply #'make-layered-policy base
              (loop for p in policies
                    for k from 0
                    when p
                      collect (golden-layer (format nil "policy-~D" k)
                                            (golden-rules p (format nil "p~D-" k)))))
       request))))

(deftest golden-rules-effect
  (dolist (case *golden-rules-effect-cases*)
    (destructuring-bind (name rules action resources effect index) case
      (let ((d (ignore-errors
                (evaluate-policy (golden-layer "rules" (golden-rules rules "r"))
                                 (list :action action :resources resources)))))
        (ok (and d (eq (effect->golden (decision-effect d)) effect)) name)
        (ok (and d (equal (rule-id d) (and index (format nil "r~D" index))))
            (format nil "~A: deciding rule" name))))))

(deftest golden-agent-permission
  (dolist (row *golden-agent-cases*)
    (let ((name (getf row :name))
          (d (ignore-errors (golden-agent-decision row))))
      (ok (and d (eq (effect->golden (decision-effect d)) (getf row :effect))) name)
      (ok (and d (equal (rule-id d) (getf row :rule)))
          (format nil "~A: deciding rule ~S" name (getf row :rule)))
      (when (getf row :feedback)
        (ok (and d (equal (decision-feedback d) (getf row :feedback)))
            (format nil "~A: feedback" name)))
      (when (getf row :resource)
        (ok (and d (equal (decision-resource d) (getf row :resource)))
            (format nil "~A: deciding resource" name))))))

(deftest golden-denied-call-is-explained
  ;; L4a's acceptance test: policy-explain names the deciding rule and
  ;; resource for a denied call.
  (let* ((row (first *golden-agent-cases*))
         (text (policy-explain (golden-agent-decision row))))
    (ok (contains "forbid" text))
    (ok (contains "r1" text))
    (ok (contains "\"secret/key.pem\"" text))
    (ok (contains "Secrets are off limits" text))))
