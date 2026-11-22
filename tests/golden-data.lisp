(in-package #:policy-protocol/tests)

;;; Golden data: ai-agent-protocol's permission tests as tables, in
;;; ai-agent-protocol's own vocabulary (rules are its list form
;;; (ACTION RESOURCE EFFECT &key feedback), effects :allow / :ask / :deny,
;;; NIL = no decision). Ported from ai-agent-protocol origin/main 27d846d
;;; (0.14.0): tests/permissions-test.lisp and
;;; tests/permission-approvals-test.lisp; rows marked :source
;;; "permissions.lisp" follow src/permissions.lisp where no test pins the
;;; case. golden-test.lisp evaluates them with :last-match policies; L4a
;;; switches ai-agent-protocol over with these same tables.
;;;
;;; Deciding-rule ids: the harness names the agent's rules "r0" "r1" ...,
;;; run-settings rules "s0" ..., saved / "always" rules "a0" ..., and rule I
;;; of policy layer K "pK-I".

;;; permission-glob-matching: (PATTERN STRING MATCHES)
(defparameter *golden-glob-cases*
  '(("*" "" t)
    ("*" "src/a/b.lisp" t)                ; * crosses /
    ("src/*" "src/a/b.lisp" t)
    ("src/*" "test/a.lisp" nil)
    ("*.lisp" "src/a.lisp" t)
    ("*.lisp" "src/a.lisp.bak" nil)       ; whole value
    ("a?c" "abc" t)
    ("a?c" "ac" nil)
    ("git status *" "git status --short" t)
    ("git status *" "git status" t)       ; trailing " *" matches the bare command
    ("git status *" "git statusx" nil)
    ("src/*" "src\\win\\path" t)          ; backslashes are slashes
    ("*a" "*ba" t)))

;;; permission-rules-last-match-wins and agent-permission-direct:
;;; (NAME RULES ACTION RESOURCES EFFECT DECIDING-RULE-INDEX-OR-NIL)
(defparameter *golden-rules-effect-cases*
  '(("last match: allow" (("*" "*" :ask) ("bash" "*" :deny) ("bash" "git *" :allow))
     "bash" ("git log") :allow 2)
    ("last match: deny" (("*" "*" :ask) ("bash" "*" :deny) ("bash" "git *" :allow))
     "bash" ("rm -rf /") :deny 1)
    ("last match: ask" (("*" "*" :ask) ("bash" "*" :deny) ("bash" "git *" :allow))
     "edit" ("x") :ask 0)
    ("deny on any resource wins" (("edit" "*" :allow) ("edit" "secret/*" :deny) ("edit" "docs/*" :ask))
     "edit" ("src/a" "secret/k" "docs/x") :deny 1)
    ("then an ask" (("edit" "*" :allow) ("edit" "secret/*" :deny) ("edit" "docs/*" :ask))
     "edit" ("src/a" "docs/x") :ask 2)
    ("then allow" (("edit" "*" :allow) ("edit" "secret/*" :deny) ("edit" "docs/*" :ask))
     "edit" ("src/a" "src/b") :allow 0)
    ("undecided resource + allow -> no decision" (("edit" "src/*" :allow))
     "edit" ("src/a" "lib/b") nil nil)
    ("no rule for the action -> no decision" (("edit" "src/*" :allow))
     "read" ("src/a") nil nil)
    ("direct: ls allowed" (("bash" "*" :ask) ("bash" "ls *" :allow))
     "bash" ("ls -la") :allow 1)
    ("direct: make asks" (("bash" "*" :ask) ("bash" "ls *" :allow))
     "bash" ("make") :ask 0)
    ("direct: bare ls allowed" (("bash" "*" :ask) ("bash" "ls *" :allow))
     "bash" ("ls") :allow 1)
    ;; :source "permissions.lisp": no resources means ("*")
    ("no resources: * only" (("edit" "src/*" :allow))
     "edit" () nil nil)
    ("no resources: deny *" (("edit" "*" :deny))
     "edit" () :deny 0)))

;;; Agent-level decisions (AGENT-PERMISSION / %RESOURCES-EFFECT): the agent's
;;; rules then the run settings' rules (one rule list, last match wins); no
;;; decision → the tool's default; an :ask is answered by the saved and
;;; "always" rules appended last, only when that gives :allow or :deny; then
;;; each policy layer (context :permission-policy, then *permission-policy*)
;;; may only tighten. A function policy is written as the rule list it
;;; behaves as.
(defparameter *golden-agent-cases*
  '((:name "deny is a tool error with feedback"
     :test "permission-deny-is-a-tool-error-with-feedback"
     :rules (("edit" "*" :allow)
             ("edit" "secret/*" :deny :feedback "Secrets are off limits; edit files under src/ only."))
     :tool-default :allow :action "edit" :resources ("src/a.lisp" "secret/key.pem")
     :effect :deny :rule "r1" :feedback "Secrets are off limits; edit files under src/ only."
     :resource "secret/key.pem")
    (:name "run settings layer comes last"
     :test "permission-run-settings-layer-comes-last"
     :rules (("edit" "*" :deny)) :settings (("edit" "src/*" :allow))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :allow :rule "s0")
    (:name ":ask is the approval pause"
     :test "permission-ask-pauses-and-a-rejection-carries-feedback"
     :rules (("edit" "*" :ask))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule "r0")
    (:name "no rule: the tool's default ask"
     :test "permission-deny-restart-takes-feedback"
     :tool-default :ask :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule nil)
    (:name "no rule: the tool's default allow"
     :source "permissions.lisp"
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :allow :rule nil)
    (:name "a rule overrides the tool default"
     :test "permission-rule-overrides-the-tool-default"
     :rules (("edit" "src/*" :allow))
     :tool-default :ask :action "edit" :resources ("src/a.lisp")
     :effect :allow :rule "r0")
    (:name "policy turns allow into ask"
     :test "permission-policy-only-tightens"
     :rules (("edit" "*" :allow)) :policies ((("edit" "src/*" :ask)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule "p0-0")
    (:name "policy allow cannot loosen a deny"
     :test "permission-policy-only-tightens"
     :rules (("edit" "*" :deny)) :policies ((("*" "*" :allow)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :deny :rule "r0")
    (:name "*permission-policy* hard-denies"
     :test "permission-policy-only-tightens"
     :rules (("edit" "*" :allow)) :policies (() (("edit" "*" :deny)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :deny :rule "p1-0")
    (:name "a policy returning allow leaves an ask alone"
     :test "permission-policy-only-tightens"
     :rules (("edit" "*" :ask)) :policies ((("*" "*" :allow)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule "r0")
    (:name "a policy permit does not loosen the tool's default ask"
     :source "permissions.lisp"
     :policies ((("*" "*" :allow)))
     :tool-default :ask :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule nil)
    (:name "a policy ask tightens the tool's default allow"
     :source "permissions.lisp"
     :policies ((("edit" "*" :ask)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule "p0-0")
    (:name "an always rule allows a later matching call"
     :test "approve-always-allows-later-matching-calls"
     :rules (("edit" "*" :ask)) :approved (("edit" "src/a.lisp" :allow))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :allow :rule "a0")
    (:name "a call on another resource still asks"
     :test "approve-always-allows-later-matching-calls"
     :rules (("edit" "*" :ask)) :approved (("edit" "src/a.lisp" :allow))
     :tool-default :allow :action "edit" :resources ("src/b.lisp")
     :effect :ask :rule "r0")
    (:name "a saved rule answers a later run"
     :test "approve-always-saves-to-the-rule-store"
     :rules (("edit" "*" :ask)) :approved (("edit" "src/a.lisp" :allow))
     :tool-default :ask :action "edit" :resources ("src/a.lisp")
     :effect :allow :rule "a0")
    (:name "policy deny wins over a saved allow"
     :test "saved-allow-never-overrides-a-deny"
     :rules (("edit" "*" :ask)) :approved (("edit" "*" :allow))
     :policies ((("edit" "*" :deny)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :deny :rule "p0-0")
    (:name "an agent-layer deny wins over a saved allow"
     :test "saved-allow-never-overrides-a-deny"
     :rules (("edit" "*" :ask) ("edit" "secret/*" :deny)) :approved (("edit" "*" :allow))
     :tool-default :allow :action "edit" :resources ("secret/k")
     :effect :deny :rule "r1")
    (:name "an always rule does not loosen a policy ask"
     :test "saved-allow-never-overrides-a-deny"
     :rules (("edit" "*" :allow)) :approved (("edit" "src/a.lisp" :allow))
     :policies ((("edit" "*" :ask)))
     :tool-default :allow :action "edit" :resources ("src/a.lisp")
     :effect :ask :rule "p0-0")
    (:name "saved rules leaving a resource undecided do not answer the ask"
     :source "permissions.lisp"
     :rules (("edit" "src/*" :ask)) :approved (("edit" "src/a" :allow))
     :tool-default :allow :action "edit" :resources ("src/a" "lib/b")
     :effect :ask :rule "r0")))
