(in-package #:policy-protocol/tests)

;;; The brief's plan-mode example over every combination of
;;; (sandbox :confined) and (setting :sandbox.failIfUnavailable).

(defun plan-context (confined fail)
  (append (and confined '((sandbox :confined)))
          (and fail '((setting :sandbox.failIfUnavailable)))))

(deftest-parametrize plan-mode-matrix
    ((confined fail effect id feedback)
     (t   nil :permit "rule-0" nil)
     (t   t   :permit "rule-0" nil)
     (nil nil :ask    "rule-1" "The sandbox is unavailable; this command runs unconfined.")
     (nil t   :forbid "rule-2" "Sandbox required (sandbox.failIfUnavailable) and unavailable."))
  (let ((d (decide (plan-mode) (req "bash" "make test" (plan-context confined fail)))))
    (ok (eq (decision-effect d) effect))
    (ok (equal (rule-id d) id))
    (ok (equal (decision-feedback d) feedback))
    (ok (equal (decision-obligations d) (and (eq effect :permit) '(("run" "confined")))))))

(deftest plan-mode-other-tools
  (ok (eq :no-decision (effect (plan-mode) (req "read" "src/a.lisp" (plan-context nil t))))))

(deftest plan-mode-is-registered-by-defpolicy
  (let ((p (eval *plan-mode-form*)))
    (ok (typep p 'policy))
    (ok (eq p (find-policy 'plan-mode)))
    (ok (equal "plan-mode" (policy-name p)))
    (ok (= 3 (length (policy-rules p))))))

(deftest context-names-are-case-insensitive
  ;; :sandbox.failIfUnavailable reads as SANDBOX.FAILIFUNAVAILABLE; the
  ;; settings file spells it sandbox.failIfUnavailable. Same name.
  (dolist (context '(((setting "sandbox.failIfUnavailable"))
                     ((setting :|sandbox.failIfUnavailable|))
                     ((setting "SANDBOX.FAILIFUNAVAILABLE"))
                     ((|SETTING| :sandbox.failIfUnavailable))))
    (ok (eq :forbid (effect (plan-mode) (req "bash" "make" context)))
        (format nil "~S" context)))
  (ok (eq :permit (effect (plan-mode) (req "bash" "make" '(("sandbox" "Confined")))))))

(deftest condition-operators
  (let ((p (policy-of :forbid-overrides
                      '(permit :id "or" :action "a" :when (or (mode :plan) (mode :review)))
                      '(forbid :id "and-not" :action "a"
                        :when (and (mode :plan) (not (sandbox :confined)) (network :on)))
                      '(ask :id "arity" :action "b" :when (limit 3)))))
    (ok (eq :permit (effect p (req "a" "x" '((mode :review))))))
    (ok (eq :no-decision (effect p (req "a" "x" '((mode :default))))))
    (ok (eq :forbid (effect p (req "a" "x" '((mode :plan) (network :on))))))
    (ok (eq :permit (effect p (req "a" "x" '((mode :plan) (network :on) (sandbox :confined))))))
    (ok (eq :ask (effect p (req "b" "x" '((limit 3))))) "integers compare with =")
    (ok (eq :no-decision (effect p (req "b" "x" '((limit 4))))))
    (ok (eq :no-decision (effect p (req "b" "x" '((limit 3 4)))))
        "same name, other arity: no match")))
