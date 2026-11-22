(in-package #:policy-protocol/tests)

;;; Invalid policies and requests: CLHS 9 conditions with SKIP-RULE and
;;; USE-VALUE (conditions.lisp). Every HANDLER-BIND sits inside OK: Rove's OK
;;; catches errors itself.

(deftest-parametrize invalid-rules
    ((rule)
     ('(allow :action "x"))                       ; unknown effect
     ('(permit :action))                          ; missing value
     ('(permit :action "x" :resourse "y"))        ; unknown key
     ('(permit :action 42))                       ; action not a string
     ('(permit :resource (:regex "a.*")))         ; unknown pattern kind
     ('(permit :resource (:prefix)))
     ('(permit :when (sandbox ?state)))           ; no logic variables
     ('(permit :when (sandbox (nested list))))
     ('(permit :when (and)))                      ; AND needs an operand
     ('(permit :when (not (a) (b))))
     ('(permit :when "sandbox"))
     ('(permit :obligation "run"))                ; obligation is a list
     ('(forbid :action "x" :obligation (:run :confined))) ; only permits carry obligations
     ('(ask :feedback 7))
     ('(permit :id 3))
     (:permit)
     ("permit"))
  (ok (signals (make-policy :rules (list '(permit :id "ok" :action "a") rule))
               'policy-syntax-error))
  (ok (equal rule (handler-case (make-policy :rules (list rule))
                    (policy-syntax-error (c) (policy-error-form c))))
      "FORM is the offending rule"))

(deftest invalid-options
  (ok (signals (make-policy :combining :first-match) 'policy-syntax-error))
  (ok (signals (make-policy :default :maybe) 'policy-syntax-error))
  (ok (signals (parse-policy '(defpolicy p (:combining :last-match :color :red))) 'policy-syntax-error))
  (ok (signals (parse-policy '(defpolicy p (:default :ask) (default :forbid))) 'policy-syntax-error)
      ":default and (default ...) both")
  (ok (signals (parse-policy '(defpolicy p () (default :ask) (default :forbid))) 'policy-syntax-error))
  (ok (signals (parse-policy '(define-policy p ())) 'policy-syntax-error))
  (ok (signals (make-policy :rules '((permit :id "a") (forbid :id "a"))) 'policy-syntax-error)
      "duplicate ids"))

(deftest skip-rule-restart
  (let (p)
    (ok (setf p (handler-bind ((policy-syntax-error #'skip-rule))
                  (make-policy :rules '((permit :action "a")
                                        (allow :action "b")
                                        (forbid :action "c"))))))
    (ok (= 2 (length (policy-rules p))))
    (ok (equal '("rule-0" "rule-1") (mapcar #'policy-rule-id (policy-rules p)))
        "ids count the rules kept")
    (ok (eq :forbid (effect p (req "c" "x")))))
  (ok (= 1 (length (policy-rules
                    (handler-bind ((policy-syntax-error #'skip-rule))
                      (parse-policy '(defpolicy p () (permit :action "a") (permit :action 1))))))))
  (let (seen offered)
    (ok (eq :forbid-overrides
            (policy-combining
             (handler-bind ((policy-syntax-error
                              (lambda (c)
                                (setf seen t offered (and (find-restart 'skip-rule c) t))
                                (use-value :forbid-overrides))))
               (make-policy :combining :first-match :rules '((permit :action "a")))))))
    (ok (and seen (not offered)) "an option error offers no SKIP-RULE")))

(deftest use-value-restart
  (testing "a replacement rule"
    (let (p)
      (ok (setf p (handler-bind ((policy-syntax-error
                                   (lambda (c) (declare (ignore c))
                                     (use-value '(ask :id "fixed" :action "b")))))
                    (make-policy :rules '((permit :action "a") (allow :action "b"))))))
      (ok (equal '("rule-0" "fixed") (mapcar #'policy-rule-id (policy-rules p))))
      (ok (eq :ask (effect p (req "b" "x"))))))
  (testing "an invalid replacement is signalled again"
    (let ((count 0))
      (ok (= 1 (length (policy-rules
                        (handler-bind ((policy-syntax-error
                                         (lambda (c)
                                           (incf count)
                                           (if (= count 1)
                                               (use-value '(allow :action "still-bad"))
                                               (skip-rule c)))))
                          (make-policy :rules '((permit :action "a") (allow :action "b"))))))))
      (ok (= 2 count))))
  (testing "a replacement option value"
    (ok (eq :last-match
            (policy-combining
             (handler-bind ((policy-syntax-error
                              (lambda (c) (declare (ignore c)) (use-value :last-match))))
               (make-policy :combining :first-match :rules '((permit :action "a"))))))))
  (testing "from LOAD-POLICY data, per rule"
    (ok (= 1 (length (policy-rules
                      (handler-bind ((policy-syntax-error #'skip-rule))
                        (load-policy "{\"rules\": [{\"effect\": \"permit\", \"action\": \"a\"},
                                                   {\"effect\": \"allow\", \"action\": \"b\"}]}"
                                     :format :json))))))))
(deftest defpolicy-signals-at-run-time
  ;; Macroexpansion never signals: the error comes, with its restarts, when
  ;; the DEFPOLICY form runs.
  (let ((form '(defpolicy policy-test-broken () (allow :action "x"))))
    (ok (macroexpand-1 form))
    (ok (signals (eval form) 'policy-syntax-error))
    (ok (typep (handler-bind ((policy-syntax-error #'skip-rule)) (eval form)) 'policy))))

(deftest-parametrize invalid-requests
    ((request)
     ('(:resources ("x")))                         ; no action
     ('(:action bash :resources ("x")))            ; action not a string
     ('(:action "bash" :resources ("x" 3)))
     ('(:action "bash" :resource 3))
     ('(:action "bash" :context ((sandbox ?s))))
     ('(:action "bash" :context (sandbox)))
     ('(:action "bash" :colour :red))
     ("bash"))
  (ok (signals (evaluate-policy (plan-mode) request) 'policy-request-error)))

(deftest request-forms
  (let ((p (policy-of :forbid-overrides '(permit :action "edit" :resource "src/*"))))
    (ok (eq :permit (effect p '(:action "edit" :resource "src/a"))))
    (ok (eq :permit (effect p (make-policy-request :action "edit" :resources '("src/a")))))
    (ok (equal '("*") (request-resources (make-policy-request :action "edit"))))
    (ok (eq :no-decision (effect p '(:action "edit"))) "no resources means (\"*\")")))

(deftest no-logic-backend
  (let ((logic-protocol:*logic-backend* nil))
    (ok (signals (evaluate-policy (plan-mode) (req "bash" "x"))
                 'logic-protocol:logic-no-backend))))

