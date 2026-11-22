(in-package #:policy-protocol)

;;; The policy model (brief: logic.md, "policy-protocol" and "Policy
;;; combining", locked). Everything below is normative for 0.1.0.
;;;
;;; NAMES. In conditions, context facts and obligations every symbol or
;;; string constant is a NAME, compared case-insensitively: it is normalized
;;; to (STRING-DOWNCASE (STRING x)). So :sandbox.failIfUnavailable,
;;; SANDBOX.FAILIFUNAVAILABLE and "sandbox.failIfUnavailable" are the same
;;; name, "sandbox.failifunavailable". Integers stay integers (compared with
;;; =). Nothing a policy or a request holds is ever interned: data loaded from
;;; JSON / TOML / sexp text stays strings (see load.lisp). Actions and
;;; resources are NOT names: they are case-sensitive strings.
;;;
;;; RULES. (EFFECT &key id action resource when obligation obligations
;;; feedback), EFFECT one of PERMIT, ASK, FORBID (by SYMBOL-NAME, any
;;; package); the policy's effect keywords are :PERMIT, :ASK, :FORBID.
;;;   :id          string; default "rule-<i>", I its 0-based position among
;;;                the policy's rules (after SKIP-RULE removals). Ids are
;;;                unique within a policy (a duplicate is a syntax error).
;;;   :action      a glob (string) on the tool / action name; default "*".
;;;   :resource    a resource pattern (below); default "*".
;;;   :when        a condition (below) over the request's context; default:
;;;                always true.
;;;   :obligation  one obligation (KIND ARG...), e.g. (:run :confined);
;;;   :obligations a list of them. Both may be given; :obligation comes
;;;                first. Only PERMIT rules carry obligations.
;;;   :feedback    string: what the model is told on an ask / forbid.
;;; Any other key, a missing value, a value of the wrong type, an unknown
;;; effect: POLICY-SYNTAX-ERROR (restarts SKIP-RULE, USE-VALUE).
;;;
;;; RESOURCE PATTERNS (canonical form in parentheses):
;;;   "glob" / (:glob "glob")  ("glob") whole-value glob, ai-agent-protocol's
;;;            PERMISSION-GLOB-MATCH-P exactly: * any run (including / and the
;;;            empty run), ? one character, any other character itself;
;;;            backslashes count as slashes on both sides; a pattern ending in
;;;            " *" also matches the bare command ("git status *" matches
;;;            "git status"). Actions use the same glob.
;;;   (:prefix "git ")  ((:prefix "git ")) the resource starts with the
;;;            string (case-sensitive, no backslash rewriting).
;;;   (:path "src")  ((:path "src")) after rewriting backslashes to slashes on
;;;            both sides and dropping one trailing "/" from the pattern
;;;            (unless it is "/"): the resource equals the path or starts with
;;;            path + "/". "src" matches "src", "src/a/b" and "src\\a", not
;;;            "srcx/a" or "lib/src". "/" matches every resource starting "/".
;;;   (:host "example.com")  ((:host "example.com")) the resource is the
;;;            host or a subdomain of it: equal, or ends with "." + host.
;;;            Case-sensitive (callers pass lower-case host names); no URL
;;;            parsing: the resource is a host name.
;;;
;;; CONDITIONS (canonical form in parentheses):
;;;   (REL ARG...)    (("rel" "arg"...)) holds when the request's context has
;;;                   a fact with the same name and arity and equal arguments
;;;                   (names normalized). REL is a symbol other than NOT, AND,
;;;                   OR; ARGs are symbols, strings or integers. No logic
;;;                   variables (?x), no nested lists.
;;;   (not C)         ((:not C))
;;;   (and C C...)    ((:and C...)) at least one operand
;;;   (or C C...)     ((:or C...)) at least one operand
;;; NOT / AND / OR by SYMBOL-NAME.
;;;
;;; OBLIGATIONS. (KIND ARG...), KIND and ARGs names or integers; canonical
;;; form: the normalized list, ("run" "confined"). A permit decision carries
;;; its obligations; the caller must honour each one or treat the decision as
;;; a forbid. EVALUATE-POLICY's :OBLIGATIONS (default *POLICY-OBLIGATIONS*)
;;; lists the kinds the caller honours (names); a permit carrying any other
;;; kind becomes :FORBID (fail closed): DECISION-UNMET-OBLIGATIONS lists the
;;; unhonoured obligations, DECISION-OBLIGATIONS is NIL, DECISION-FEEDBACK
;;; names each unmet kind, DECISION-RULE stays the permit rule.
;;;
;;; REQUESTS. A POLICY-REQUEST, or a plist (:action A :resource R) /
;;; (:action A :resources (R...)) with optional :context (FACT...). ACTION is
;;; a string; resources are strings (NIL or no resources means ("*"), as in
;;; ai-agent-protocol); a context fact is a ground atom (REL ARG...) as in
;;; conditions. Anything else signals POLICY-REQUEST-ERROR.
;;;
;;; ONE POLICY, ONE REQUEST. A rule APPLIES TO a resource when its action
;;; glob matches the action, its resource pattern matches that resource, and
;;; its condition holds. A resource is UNDECIDED when no rule applies to it.
;;; Strictness: :forbid > :ask > :permit.
;;;   :forbid-overrides (default) Per resource, the strictest effect of the
;;;       rules applying to it. The policy's effect is the strictest over the
;;;       resources, except that when it would be :permit and some resource
;;;       is undecided it is :no-decision. The DECIDING RESOURCE is the first
;;;       resource (request order) whose effect is the policy's effect; the
;;;       DECIDING RULE the first rule (policy order) applying to it with
;;;       that effect. Obligations of a :permit: those of every permit rule
;;;       applying to any resource, policy order, without EQUAL duplicates.
;;;   :last-match  Per resource, the effect of the LAST rule (policy order)
;;;       applying to it. Across resources exactly as above (strictest;
;;;       :permit with an undecided resource → :no-decision) — this is
;;;       ai-agent-protocol's PERMISSION-RULES-EFFECT. The deciding resource is
;;;       the first one reaching the policy's effect, the deciding rule its
;;;       last match. Obligations of a :permit: the last match of each
;;;       resource, resource order, without EQUAL duplicates.
;;;   No rule applies to any resource, or the :no-decision above: the
;;;   policy's :DEFAULT effect when it has one (DECISION-DEFAULT-P true, no
;;;   deciding rule, no obligations), else :NO-DECISION (the caller applies
;;;   its own default).
;;;
;;; LAYERS. MAKE-LAYERED-POLICY takes layers outer first (a layered policy
;;; given as a layer is flattened in place). Each layer is evaluated on its
;;; own (with its own combining mode and default); then, starting from the
;;; outermost layer's effect, each inner layer can only TIGHTEN: an inner
;;; :ask or :forbid replaces the running effect when it is stricter (or the
;;; running effect is :no-decision); an inner :permit or :no-decision never
;;; changes it. So an inner permit cannot turn an outer :no-decision (the
;;; caller's default, which may be an ask) into a permit. The DECIDING LAYER
;;; is the outermost layer whose own effect equals the result (NIL when the
;;; result is :no-decision); its deciding rule, resource, feedback and
;;; DEFAULT-P are the decision's. A plain POLICY is its own only layer:
;;; DECISION-LAYER is the policy itself, or NIL for :no-decision. Obligations of a :permit
;;; result: those of every layer whose own effect is :permit, outer first,
;;; without EQUAL duplicates.
;;;
;;; DECISIONS. DECISION-EFFECT is :permit, :ask, :forbid or :no-decision.
;;; DECISION-FEEDBACK is the deciding rule's feedback for an ask / forbid
;;; (NIL for a permit, a default or no decision), or the unmet-obligation
;;; text. DECISION-PROOF is the deciding layer's TRACE-PROOF. DECISION-TRACE
;;; is one POLICY-TRACE per layer, outer first (one entry for a plain
;;; policy): that layer's own effect, deciding rule, deciding resource,
;;; DEFAULT-P, the rules that applied to some resource (policy order), and
;;; TRACE-PROOF: a logic-protocol PROOF of the deciding rule applying (its
;;; base facts include the request facts it used), NIL when no rule decided.
;;;
;;; WHY NOT. (DECISION-WHY-NOT decision rule), RULE a POLICY-RULE or a rule
;;; id (searched in the layers, outer first): NIL when RULE is its layer's
;;; deciding rule. Otherwise a POLICY-WHY-NOT whose REASON is the first of:
;;;   :action      its action glob does not match the action;
;;;   :resource    its resource pattern matches none of the resources;
;;;   :condition   its condition is false; LITERAL is the first top-level
;;;                conjunct that is false, as written in the rule (a
;;;                condition that is not an AND is its own only conjunct);
;;;   :overridden  it applied, and another rule (or a stricter effect)
;;;                decided.
;;;
;;; EVALUATION is on logic-protocol: a policy compiles once (POLICY-PROGRAM,
;;; on first use, with logic-protocol:*LOGIC-BACKEND*) to a program in the
;;; analysable subset (stratified Datalog, built-ins =, /=, <, glob, prefix
;;; only). Each request is evaluated with :ASSUME (the request's facts hold
;;; for that call only), so the program is never rebuilt or changed by a
;;; request: same object, same base facts afterwards. EVALUATE-POLICY's
;;; :BUDGET (a logic-protocol budget designator) bounds the request's logic
;;; work (not the one-time compilation); LOGIC-BUDGET-EXCEEDED and
;;; LOGIC-NO-BACKEND propagate with their restarts (CONTINUE finishes the
;;; evaluation with the right decision).

(defvar *policy-obligations* '(:run)
  "Obligation kinds (names) the caller honours: the default of
EVALUATE-POLICY's :OBLIGATIONS. A permit carrying another kind is a forbid.")

(defclass policy ()
  ((name :initarg :name :initform nil :reader policy-name
         :documentation "A string (a DEFPOLICY symbol's name, downcased) or NIL.")
   (combining :initarg :combining :initform :forbid-overrides :reader policy-combining
              :documentation ":FORBID-OVERRIDES or :LAST-MATCH.")
   (default :initarg :default :initform nil :reader policy-default
            :documentation "NIL, :PERMIT, :ASK or :FORBID.")
   (rules :initarg :rules :initform '() :reader policy-rules
          :documentation "POLICY-RULEs in order.")
   (program :initform nil :accessor %policy-program
            :documentation "The compiled logic-protocol program, or NIL until first use.")
   (extra :initform nil :accessor %policy-extra
          :documentation "Free for the implementation (caches)."))
  (:documentation "One policy layer: rules, a combining mode, an optional default."))

(defmethod print-object ((p policy) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~@[~A ~]~S, ~D rule~:P"
            (policy-name p) (policy-combining p) (length (policy-rules p)))))

(defclass layered-policy ()
  ((layers :initarg :layers :initform '() :reader layered-policy-layers
           :documentation "POLICY objects, outer first, flattened."))
  (:documentation "Layers where an inner layer only tightens (see the header)."))

(defmethod policy-name ((p layered-policy)) nil)

(defclass policy-rule ()
  ((id :initarg :id :reader policy-rule-id)
   (effect :initarg :effect :reader policy-rule-effect)
   (action :initarg :action :initform "*" :reader policy-rule-action)
   (resource :initarg :resource :initform "*" :reader policy-rule-resource
             :documentation "The canonical resource pattern.")
   (condition :initarg :condition :initform nil :reader policy-rule-condition
              :documentation "The condition as written (NIL: always true).")
   (obligations :initarg :obligations :initform '() :reader policy-rule-obligations
                :documentation "Canonical obligations, in order.")
   (feedback :initarg :feedback :initform nil :reader policy-rule-feedback)
   (form :initarg :form :initform nil :reader policy-rule-form
         :documentation "The rule as written (a DSL form, or the canonical form
for data sources)."))
  (:documentation "One permit / ask / forbid rule."))

(defmethod print-object ((r policy-rule) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~A ~S ~S ~S" (policy-rule-id r) (policy-rule-effect r)
            (policy-rule-action r) (policy-rule-resource r))))

(defclass policy-request ()
  ((action :initarg :action :reader request-action)
   (resources :initarg :resources :initform '("*") :reader request-resources)
   (context :initarg :context :initform '() :reader request-context))
  (:documentation "One tool call: an action, the resources it touches, context facts."))

(defclass decision ()
  ((effect :initarg :effect :reader decision-effect)
   (obligations :initarg :obligations :initform '() :reader decision-obligations)
   (unmet :initarg :unmet-obligations :initform '() :reader decision-unmet-obligations)
   (feedback :initarg :feedback :initform nil :reader decision-feedback)
   (rule :initarg :rule :initform nil :reader decision-rule)
   (layer :initarg :layer :initform nil :reader decision-layer
          :documentation "The deciding layer (a POLICY), or NIL.")
   (resource :initarg :resource :initform nil :reader decision-resource)
   (default-p :initarg :default-p :initform nil :reader decision-default-p)
   (proof :initarg :proof :initform nil :reader decision-proof)
   (trace :initarg :trace :initform '() :reader decision-trace)
   (request :initarg :request :initform nil :reader %decision-request))
  (:documentation "The result of EVALUATE-POLICY (see the header)."))

(defmethod print-object ((d decision) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~S~@[ by ~A~]" (decision-effect d)
            (and (decision-rule d) (policy-rule-id (decision-rule d))))))

(defclass policy-trace ()
  ((policy :initarg :policy :reader trace-policy)
   (effect :initarg :effect :reader trace-effect)
   (rule :initarg :rule :initform nil :reader trace-rule)
   (resource :initarg :resource :initform nil :reader trace-resource)
   (default-p :initarg :default-p :initform nil :reader trace-default-p)
   (proof :initarg :proof :initform nil :reader trace-proof)
   (applicable :initarg :applicable :initform '() :reader trace-applicable-rules))
  (:documentation "One layer's part of a decision."))

(defclass policy-why-not ()
  ((rule :initarg :rule :reader policy-why-not-rule)
   (reason :initarg :reason :reader policy-why-not-reason
           :documentation ":ACTION, :RESOURCE, :CONDITION or :OVERRIDDEN.")
   (literal :initarg :literal :initform nil :reader policy-why-not-literal))
  (:documentation "Why a rule did not decide (see the header)."))

(defmethod print-object ((w policy-why-not) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~A ~S~@[ ~S~]" (policy-rule-id (policy-why-not-rule w))
            (policy-why-not-reason w) (policy-why-not-literal w))))
