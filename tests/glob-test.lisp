(in-package #:policy-protocol/tests)

;;; Resource patterns (model.lisp: RESOURCE PATTERNS). The glob contract is
;;; ai-agent-protocol's PERMISSION-GLOB-MATCH-P, for resources and actions.

(defun matches-p (pattern resource)
  "Does a permit rule with resource PATTERN apply to RESOURCE?"
  (eq :permit (effect (policy-of :forbid-overrides
                                 `(permit :action "t" :resource ,pattern))
                      (req "t" resource))))

(defun action-matches-p (glob action)
  (eq :permit (effect (policy-of :forbid-overrides `(permit :action ,glob))
                      (req action "x"))))

(deftest glob-resource-golden
  (dolist (case *golden-glob-cases*)
    (destructuring-bind (pattern string expected) case
      (ok (eq (ignore-errors (matches-p pattern string)) expected)
          (format nil "~S ~:[does not match~;matches~] ~S" pattern expected string)))))

(deftest glob-action-golden
  (dolist (case *golden-glob-cases*)
    (destructuring-bind (pattern string expected) case
      (ok (eq (ignore-errors (action-matches-p pattern string)) expected)
          (format nil "action ~S ~:[does not match~;matches~] ~S" pattern expected string)))))

(deftest glob-explicit-form
  (ok (matches-p '(:glob "src/*") "src/a/b.lisp"))
  (ng (matches-p '(:glob "src/*") "lib/a.lisp")))

(deftest-parametrize prefix-pattern
    ((pattern resource expected)
     ("git " "git status" t)
     ("git " "git " t)
     ("git " "gitk" nil)
     ("git " "Git status" nil)
     ("src\\" "src/a" nil))
  (ok (eq (matches-p `(:prefix ,pattern) resource) expected)))

(deftest-parametrize path-pattern
    ((pattern resource expected)
     ("src" "src" t)
     ("src" "src/a/b.lisp" t)
     ("src" "src\\win\\x" t)
     ("src/" "src/a" t)
     ("src/" "src" t)
     ("src" "srcx/a" nil)
     ("src" "lib/src" nil)
     ("/" "/etc/passwd" t)
     ("/" "etc" nil))
  (ok (eq (matches-p `(:path ,pattern) resource) expected)))

(deftest-parametrize host-pattern
    ((pattern resource expected)
     ("example.com" "example.com" t)
     ("example.com" "api.example.com" t)
     ("example.com" "a.b.example.com" t)
     ("example.com" "badexample.com" nil)
     ("example.com" "example.com.evil" nil)
     ("example.com" "example.org" nil))
  (ok (eq (matches-p `(:host ,pattern) resource) expected)))

(deftest pattern-canonical-form
  (let ((p (policy-of :forbid-overrides
                      '(permit :action "a" :resource "x*")
                      '(permit :action "a" :resource (:glob "y*"))
                      '(permit :action "a" :resource (:prefix "git "))
                      '(permit :action "a" :resource (:path "src"))
                      '(permit :action "a" :resource (:host "example.com"))
                      '(permit :action "a"))))
    (ok (equal (mapcar #'policy-rule-resource (policy-rules p))
               '("x*" "y*" (:prefix "git ") (:path "src") (:host "example.com") "*")))))
