(in-package #:policy-protocol/tests)

;;; Traces, proofs, why-not and POLICY-EXPLAIN (model.lisp: DECISIONS, WHY
;;; NOT; explain.lisp).

(defun printed (x)
  (with-standard-io-syntax
    (let ((*print-readably* nil)) (prin1-to-string x))))

(deftest trace-names-the-deciding-rule
  (let* ((p (plan-mode))
         (d (decide p (req "bash" "make test" '((sandbox :confined))))))
    (ok (eq (decision-rule d) (first (policy-rules p))))
    (ok (eq (decision-layer d) p) "a plain policy is its own layer")
    (ok (equal "make test" (decision-resource d)))
    (ok (= 1 (length (decision-trace d))))
    (let ((e (first (decision-trace d))))
      (ok (eq (trace-policy e) p))
      (ok (eq :permit (trace-effect e)))
      (ok (eq (trace-rule e) (decision-rule d)))
      (ok (equal "make test" (trace-resource e)))
      (ng (trace-default-p e))
      (ok (equal (list (first (policy-rules p))) (trace-applicable-rules e))))))

(deftest trace-lists-every-applicable-rule
  (let* ((p (plan-mode))
         (d (decide p (req "bash" "make" '((setting :sandbox.failIfUnavailable)))))
         (e (first (decision-trace d))))
    (ok (eq :forbid (decision-effect d)))
    (ok (equal (mapcar #'policy-rule-id (trace-applicable-rules e)) '("rule-1" "rule-2")))))

(deftest decision-proof-is-a-logic-proof
  (let ((d (decide (plan-mode) (req "bash" "make test" '((sandbox :confined))))))
    (ok (typep (decision-proof d) 'logic-protocol:proof))
    (ok (eq (decision-proof d) (trace-proof (first (decision-trace d)))))
    (let ((base (printed (logic-protocol:proof-base-facts (decision-proof d)))))
      (ok (contains "\"bash\"" base) "the request's action is a base fact of the proof")
      (ok (contains "confined" base) "so is the context fact the condition used")))
  (testing "no proof without a deciding rule"
    (ok (null (decision-proof (decide (plan-mode) (req "read" "x")))))))

(deftest why-not-reasons
  (let* ((p (plan-mode))
         (confined (decide p (req "bash" "make" '((sandbox :confined)))))
         (unconfined (decide p (req "bash" "make" '((setting :sandbox.failIfUnavailable)))))
         (other (decide p (req "read" "x" '((sandbox :confined))))))
    (testing "the deciding rule has no why-not"
      (ok (null (decision-why-not confined "rule-0")))
      (ok (null (decision-why-not unconfined (third (policy-rules p))))))
    (testing ":condition names the first false conjunct, as written"
      (let ((w (decision-why-not confined "rule-1")))
        (ok (eq :condition (policy-why-not-reason w)))
        (ok (equal '(not (sandbox :confined)) (policy-why-not-literal w)))
        (ok (eq (second (policy-rules p)) (policy-why-not-rule w))))
      (let ((w (decision-why-not confined "rule-2")))
        (ok (eq :condition (policy-why-not-reason w)))
        (ok (equal '(setting :sandbox.failIfUnavailable) (policy-why-not-literal w)))))
    (testing ":overridden: it applied, a stricter rule decided"
      (let ((w (decision-why-not unconfined "rule-1")))
        (ok (eq :overridden (policy-why-not-reason w)))
        (ok (null (policy-why-not-literal w)))))
    (testing ":action"
      (ok (eq :action (policy-why-not-reason (decision-why-not other "rule-0")))))))

(deftest why-not-resource-and-last-match
  (let* ((p (policy-of :last-match
                       '(forbid :id "all" :action "bash")
                       '(permit :id "git" :action "bash" :resource "git *")
                       '(ask :id "docs" :action "bash" :resource "docs/*")))
         (d (decide p (req "bash" "git log"))))
    (ok (eq :permit (decision-effect d)))
    (ok (eq :overridden (policy-why-not-reason (decision-why-not d "all")))
        "an earlier match a later one replaced")
    (ok (eq :resource (policy-why-not-reason (decision-why-not d "docs")))))
  (ok (signals (decision-why-not (decide (plan-mode) (req "bash" "x")) "no-such-rule")
               'policy-error)))

(deftest why-not-in-layers
  (let* ((outer (make-policy :name "user" :rules '((permit :id "u" :action "edit"))))
         (inner (make-policy :name "project" :rules '((forbid :id "j" :action "edit" :resource "secret/*"))))
         (d (decide (make-layered-policy outer inner) (req "edit" "src/a"))))
    (ok (eq :permit (decision-effect d)))
    (ok (null (decision-why-not d "u")))
    (ok (eq :resource (policy-why-not-reason (decision-why-not d "j"))))))

(deftest explain-text
  (let ((p (plan-mode)))
    (let ((text (policy-explain (decide p (req "bash" "make" '((setting :sandbox.failIfUnavailable)))))))
      (ok (stringp text))
      (ok (contains "forbid" text))
      (ok (contains "rule-2" text))
      (ok (contains "\"make\"" text) "the deciding resource")
      (ok (contains "(setting sandbox.failifunavailable)" text) "the condition, canonical")
      (ok (contains "(not (sandbox confined))" text))
      (ok (contains "Sandbox required" text) "the feedback"))
    (let ((text (policy-explain (decide p (req "bash" "make" '((sandbox :confined)))))))
      (ok (contains "permit" text))
      (ok (contains "rule-0" text))
      (ok (contains "(sandbox confined)" text)))
    (ok (contains "no rule" (policy-explain (decide p (req "read" "x")))))
    (ok (contains "default" (policy-explain (decide (make-policy :default :forbid
                                                                 :rules '((permit :action "read")))
                                                    (req "write" "x")))))
    (ok (contains "\"secret/*\"" (policy-explain
                                  (decide (policy-of :forbid-overrides
                                                     '(forbid :id "s" :action "edit" :resource "secret/*"))
                                          (req "edit" "secret/k"))))
        "a resource pattern other than * is shown")
    (testing "a layered decision names its layer"
      (let ((text (policy-explain
                   (decide (make-layered-policy (make-policy :name "user-layer"
                                                             :rules '((permit :action "edit")))
                                                (make-policy :name "project-layer"
                                                             :rules '((ask :id "careful" :action "edit"))))
                           (req "edit" "x")))))
        (ok (contains "project-layer" text))
        (ok (contains "careful" text))))
    (testing "to a stream"
      (let ((d (decide p (req "bash" "make" '((sandbox :confined))))))
        (ok (equal (policy-explain d)
                   (with-output-to-string (s)
                     (ok (null (policy-explain d s))))))))))
