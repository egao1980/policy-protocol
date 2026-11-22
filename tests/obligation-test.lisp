(in-package #:policy-protocol/tests)

;;; Obligations (model.lisp: OBLIGATIONS): "permit, provided"; fail closed.

(defun obliging ()
  (policy-of :forbid-overrides
             '(permit :id "run" :action "bash" :obligation (:run :confined))
             '(permit :id "notify" :action "deploy"
               :obligations ((:run :confined) (:notify "ops")))
             '(permit :id "both" :action "bash" :resource "make *"
               :obligation (:run :confined) :obligations ((:log :audit 3)))))

(deftest obligations-are-returned
  (let ((d (decide (obliging) (req "bash" "ls"))))
    (ok (eq :permit (decision-effect d)))
    (ok (equal '(("run" "confined")) (decision-obligations d)))
    (ok (null (decision-unmet-obligations d))))
  (ok (equal '(("run" "confined")) (policy-rule-obligations (first (policy-rules (obliging)))))
      "canonical: names as downcased strings"))

(deftest obligations-collect-without-duplicates
  ;; "run" and "both" apply to "make all"; :obligation comes before :obligations.
  (let ((d (decide (obliging) (req "bash" "make all") :obligations '(:run :log))))
    (ok (eq :permit (decision-effect d)))
    (ok (equal '(("run" "confined") ("log" "audit" 3)) (decision-obligations d)))))

(deftest unknown-obligation-forbids
  (let ((d (decide (obliging) (req "deploy" "prod"))))
    (ok (eq :forbid (decision-effect d)) "the default honours only :run")
    (ok (null (decision-obligations d)))
    (ok (equal '(("notify" "ops")) (decision-unmet-obligations d)))
    (ok (equal "notify" (rule-id d)) "the permit rule stays the deciding rule")
    (ok (contains "notify" (decision-feedback d)))
    (ok (contains "obligation" (policy-explain d)))))

(deftest honoured-obligations
  (let ((d (decide (obliging) (req "deploy" "prod") :obligations '(:run "Notify"))))
    (ok (eq :permit (decision-effect d)))
    (ok (equal '(("run" "confined") ("notify" "ops")) (decision-obligations d))))
  (let ((*policy-obligations* '(:run :notify)))
    (ok (eq :permit (effect (obliging) (req "deploy" "prod"))) "*policy-obligations*"))
  (let ((*policy-obligations* '()))
    (ok (eq :forbid (effect (obliging) (req "bash" "ls"))) "nothing honoured")))

(deftest obligations-across-layers
  (let* ((outer (make-policy :name "outer"
                             :rules '((permit :action "bash" :obligation (:run :confined)))))
         (inner (make-policy :name "inner"
                             :rules '((permit :action "bash" :obligation (:log :audit))
                                      (permit :action "bash" :obligation (:run :confined)))))
         (d (decide (make-layered-policy outer inner) (req "bash" "ls")
                    :obligations '(:run :log))))
    (ok (eq :permit (decision-effect d)))
    (ok (equal '(("run" "confined") ("log" "audit")) (decision-obligations d))
        "outer first, inner permits add theirs, no duplicates")))

(deftest only-permits-carry-obligations
  (let ((d (decide (policy-of :forbid-overrides
                              '(permit :action "bash" :obligation (:run :confined))
                              '(ask :action "bash"))
                   (req "bash" "ls"))))
    (ok (eq :ask (decision-effect d)))
    (ok (null (decision-obligations d)))))
