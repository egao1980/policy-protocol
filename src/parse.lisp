(in-package #:policy-protocol)

;;; Building policies. The S-expression DSL (DEFPOLICY, PARSE-POLICY), rule
;;; lists (MAKE-POLICY) and data (LOAD-POLICY) all end in the same model;
;;; POLICY-FORM is its canonical data form, the same whatever the source.
;;;
;;; DSL:  (defpolicy NAME (OPTION...) BODY...)
;;;   NAME     a symbol (policy name: its SYMBOL-NAME downcased) or a string.
;;;   OPTIONS  a plist: :combining (:forbid-overrides | :last-match), :default
;;;            (:permit | :ask | :forbid). Unknown option or bad value:
;;;            POLICY-SYNTAX-ERROR with USE-VALUE (the option's value).
;;;   BODY     rules (model.lisp), and at most one (default EFFECT), which is
;;;            the same as the :default option (both given: syntax error).
;;; Heads (DEFPOLICY, PERMIT, ASK, FORBID, DEFAULT, NOT, AND, OR) are compared
;;; by SYMBOL-NAME, so a form read in any package works.
;;;
;;; Canonical form (POLICY-FORM), keys always present and in this order:
;;;   (:name NAME-OR-NIL :combining MODE :default EFFECT-OR-NIL
;;;    :rules ((:id ID :effect EFFECT :action GLOB :resource PATTERN
;;;             :when CONDITION-OR-NIL :obligations (OBLIGATION...)
;;;             :feedback STRING-OR-NIL) ...))
;;; with PATTERN, CONDITION and OBLIGATION canonical (model.lisp). Only
;;; strings, integers, NIL and the protocol's own keywords occur in it.
;;;
;;; DEFPOLICY expands into a call of PARSE-POLICY on the quoted form at load
;;; (run) time, so an invalid policy signals POLICY-SYNTAX-ERROR with its
;;; restarts when the DEFPOLICY form is evaluated, never at macroexpansion.
;;; It registers the policy under NAME (FIND-POLICY) and returns it.

(defvar *policies* (make-hash-table :test 'eq)
  "DEFPOLICY's registry: symbol → POLICY.")

(defun find-policy (name)
  "The policy DEFPOLICY registered under the symbol NAME, or NIL."
  (check-type name symbol)
  (values (gethash name *policies*)))

(defun (setf find-policy) (policy name)
  (check-type name symbol)
  (check-type policy (or null policy layered-policy))
  (if policy
      (setf (gethash name *policies*) policy)
      (progn (remhash name *policies*) nil)))

(defmacro defpolicy (name (&rest options) &body body)
  "Define the policy NAME (see the file header): parse at run time, register
under NAME, return the POLICY."
  (check-type name symbol)
  `(setf (find-policy ',name)
         (parse-policy '(defpolicy ,name ,options ,@body))))


;;; Small predicates and names

(defun %proper-list-p (x)
  (loop for tail = x then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %head-p (x name)
  "Is X a non-NIL symbol named NAME (any package)?"
  (and x (symbolp x) (string= (symbol-name x) name)))

(defun %name (x)
  "A name (model.lisp: NAMES): integers stay, symbols and strings downcase."
  (if (integerp x) x (string-downcase (string x))))

(defun %name-datum-p (x)
  "Can X be a name or integer argument: a string, an integer, or a non-NIL
symbol that is not a logic variable?"
  (logic-protocol:logic-constant-p x))

(defun %syntax-error (form reason &rest args)
  (error 'policy-syntax-error :form form :reason (apply #'format nil reason args)))

(defun %request-error (form reason &rest args)
  (error 'policy-request-error :form form :reason (apply #'format nil reason args)))

(defun %effect-keyword (x)
  "The effect keyword for X (a symbol by name, or a keyword), or NIL."
  (and x (symbolp x)
       (cdr (assoc (symbol-name x) '(("PERMIT" . :permit) ("ASK" . :ask) ("FORBID" . :forbid))
                   :test #'string=))))

;;; Conditions, patterns, obligations: validation and canonical forms

(defun %canonical-condition (c fail)
  "The canonical form of condition C (written or canonical). FAIL is called
with a reason string when C is invalid; it does not return."
  (labels ((walk (c)
             (unless (and (consp c) (%proper-list-p c))
               (funcall fail (format nil "~S is not a condition (a list)" c)))
             (let ((head (first c)))
               (cond ((%head-p head "NOT")
                      (unless (= (length c) 2)
                        (funcall fail "(not C) takes exactly one condition"))
                      (list :not (walk (second c))))
                     ((or (%head-p head "AND") (%head-p head "OR"))
                      (unless (rest c)
                        (funcall fail (format nil "(~(~A~) ...) needs at least one operand"
                                              (symbol-name head))))
                      (cons (if (%head-p head "AND") :and :or) (mapcar #'walk (rest c))))
                     ((and (or (stringp head)
                               (and head (symbolp head) (not (logic-protocol:logic-variable-p head))))
                           (every #'%name-datum-p (rest c)))
                      (mapcar #'%name c))
                     (t (funcall fail (format nil "~S is not an atom (REL ARG...) of names and integers" c)))))))
    (walk c)))

(defun %condition-conjuncts (c)
  "The top-level conjuncts of condition C (written or canonical)."
  (if (and (consp c) (%head-p (first c) "AND"))
      (rest c)
      (list c)))

(defun %canonical-pattern (p fail)
  (cond ((stringp p) p)
        ((and (consp p) (%proper-list-p p) (= (length p) 2) (stringp (second p))
              (symbolp (first p)) (first p))
         (let ((kind (symbol-name (first p))))
           (cond ((string= kind "GLOB") (second p))
                 ((string= kind "PREFIX") (list :prefix (second p)))
                 ((string= kind "PATH") (list :path (second p)))
                 ((string= kind "HOST") (list :host (second p)))
                 (t (funcall fail (format nil "unknown resource pattern kind ~S" (first p)))))))
        (t (funcall fail (format nil "~S is not a resource pattern" p)))))

(defun %canonical-obligation (o fail)
  (unless (and (consp o) (%proper-list-p o) (every #'%name-datum-p o))
    (funcall fail (format nil "~S is not an obligation (KIND ARG...)" o)))
  (mapcar #'%name o))

;;; Rules

(defparameter +rule-keys+ '(:id :action :resource :when :obligation :obligations :feedback))

(defun %parse-rule (form)
  "FORM (a DSL rule form or a POLICY-RULE) checked: a POLICY-RULE, or a plist
of initargs whose :ID may be NIL (assigned later). Signals POLICY-SYNTAX-ERROR
with FORM the whole rule."
  (when (typep form 'policy-rule)
    (return-from %parse-rule form))
  (flet ((fail (reason) (%syntax-error form "~A" reason)))
    (unless (and (consp form) (%proper-list-p form))
      (fail "a rule is a list (EFFECT &key ...)"))
    (let ((effect (%effect-keyword (first form)))
          (plist (rest form)))
      (unless effect
        (fail (format nil "unknown effect ~S (PERMIT, ASK or FORBID)" (first form))))
      (unless (evenp (length plist))
        (fail "a key has no value"))
      (loop for (k nil) on plist by #'cddr
            for seen on plist by #'cddr
            do (unless (member k +rule-keys+)
                 (fail (format nil "unknown key ~S" k)))
               (when (member k (cddr seen))
                 (fail (format nil "key ~S given twice" k))))
      (destructuring-bind (&key id (action "*") (resource "*") when
                             (obligation nil obligation-p) obligations feedback)
          plist
        (unless (or (null id) (stringp id)) (fail ":id must be a string"))
        (unless (stringp action) (fail ":action must be a string (a glob)"))
        (unless (or (null feedback) (stringp feedback)) (fail ":feedback must be a string"))
        (unless (%proper-list-p obligations) (fail ":obligations must be a list"))
        (let* ((resource (%canonical-pattern resource #'fail))
               (canonical (and when (%canonical-condition when #'fail)))
               (all (append (and obligation-p (list obligation)) obligations))
               (obligations (mapcar (lambda (o) (%canonical-obligation o #'fail)) all)))
          (declare (ignore canonical))
          (when (and obligations (not (eq effect :permit)))
            (fail "only PERMIT rules carry obligations"))
          (list :id id :effect effect :action action :resource resource
                :condition when :obligations obligations :feedback feedback :form form))))))

(defun %rule-with-restarts (form)
  "FORM parsed with SKIP-RULE and USE-VALUE (a replacement, checked again).
Returns the parse or :SKIP."
  (loop
    (restart-case (return (%parse-rule form))
      (skip-rule ()
        :report "Leave this rule out and go on with the rest."
        (return :skip))
      (use-value (value)
        :report "Use another rule in its place."
        :interactive (lambda ()
                       (format *query-io* "Replacement rule: ")
                       (force-output *query-io*)
                       (list (read *query-io*)))
        (setf form value)))))

(defun %rule-id (parsed index)
  (if (typep parsed 'policy-rule)
      (policy-rule-id parsed)
      (or (getf parsed :id) (format nil "rule-~D" index))))

(defun %parse-rules (forms &key (parse #'%rule-with-restarts))
  "The POLICY-RULEs for FORMS, with the per-rule restarts. PARSE turns one
source rule into a parse or :SKIP."
  (unless (%proper-list-p forms)
    (%syntax-error forms "the rules must be a list"))
  ;; ENTRIES: (SOURCE . PARSE), kept rules in order.
  (let ((entries (loop for f in forms
                       for parsed = (funcall parse f)
                       unless (eq parsed :skip) collect (cons f parsed))))
    ;; Ids: defaults count the kept rules; a duplicate is an error in the
    ;; later rule, with the same restarts.
    (loop
      (let ((seen (make-hash-table :test 'equal))
            (dup nil))
        (loop for e in entries for i from 0
              for id = (%rule-id (cdr e) i)
              do (if (gethash id seen)
                     (progn (setf dup e) (return))
                     (setf (gethash id seen) t)))
        (unless dup (return))
        (let ((outcome
                (restart-case (%syntax-error (car dup) "duplicate rule id ~S"
                                             (%rule-id (cdr dup) (position dup entries)))
                  (skip-rule ()
                    :report "Leave this rule out and go on with the rest."
                    :skip)
                  (use-value (value)
                    :report "Use another rule in its place."
                    (cons value (%rule-with-restarts value))))))
          (if (or (eq outcome :skip) (eq (cdr outcome) :skip))
              (setf entries (remove dup entries))
              (setf entries (substitute outcome dup entries))))))
    (loop for (nil . parsed) in entries for i from 0
          collect (if (typep parsed 'policy-rule)
                      parsed
                      (destructuring-bind (&key id effect action resource condition
                                             obligations feedback form)
                          parsed
                        (make-instance 'policy-rule
                                       :id (or id (format nil "rule-~D" i))
                                       :effect effect :action action :resource resource
                                       :condition condition :obligations obligations
                                       :feedback feedback :form form))))))

;;; Options

(defun %checked-option (value check)
  "VALUE checked by CHECK (which returns the canonical value or signals
POLICY-SYNTAX-ERROR), with USE-VALUE: a replacement, checked again."
  (loop
    (restart-case (return (funcall check value))
      (use-value (new)
        :report "Use another value for this option."
        :interactive (lambda ()
                       (format *query-io* "Option value: ")
                       (force-output *query-io*)
                       (list (read *query-io*)))
        (setf value new)))))

(defun %check-combining (v)
  (if (member v '(:forbid-overrides :last-match))
      v
      (%syntax-error v "the combining mode is :FORBID-OVERRIDES or :LAST-MATCH")))

(defun %check-default (v)
  (if (member v '(nil :permit :ask :forbid))
      v
      (%syntax-error v "the default is :PERMIT, :ASK, :FORBID or NIL")))

(defun %check-name (v)
  (cond ((null v) nil)
        ((stringp v) v)
        ((symbolp v) (string-downcase (symbol-name v)))
        (t (%syntax-error v "a policy name is a symbol or a string"))))

(defun %build-policy (name combining default rule-forms &key (parse #'%rule-with-restarts))
  (let ((name (%checked-option name #'%check-name))
        (combining (%checked-option combining #'%check-combining))
        (default (%checked-option default #'%check-default)))
    (make-instance 'policy :name name :combining combining :default default
                           :rules (%parse-rules rule-forms :parse parse))))

;;; Front functions

(defun parse-policy (form)
  "The POLICY described by FORM, a (DEFPOLICY NAME (OPTION...) BODY...) list
(see the file header). Restarts: SKIP-RULE and USE-VALUE per rule, USE-VALUE
per option (conditions.lisp)."
  (unless (and (consp form) (%proper-list-p form) (%head-p (first form) "DEFPOLICY")
               (>= (length form) 3) (%proper-list-p (third form)))
    (%syntax-error form "expected (DEFPOLICY NAME (OPTION...) BODY...)"))
  (destructuring-bind (name options &rest body) (rest form)
    (let ((combining :forbid-overrides) (default nil) (default-given nil) (rules '()))
      (unless (evenp (length options))
        (%syntax-error options "the options are a plist (:KEY VALUE ...)"))
      (loop for (k v) on options by #'cddr
            do (case k
                 (:combining (setf combining v))
                 (:default (setf default v default-given t))
                 (t (restart-case (%syntax-error k "unknown option ~S" k)
                      (use-value (value)
                        :report "Ignore this option."
                        (declare (ignore value)))))))
      (dolist (item body)
        (if (and (consp item) (%head-p (first item) "DEFAULT"))
            (let ((effect (if (and (%proper-list-p item) (= (length item) 2))
                              (second item)
                              (restart-case (%syntax-error item "expected (default EFFECT)")
                                (use-value (value) :report "Use this default effect." value)))))
              (if default-given
                  (setf default
                        (restart-case (%syntax-error item "the default is given twice")
                          (use-value (value) :report "Use this default effect." value)))
                  (setf default effect default-given t)))
            (push item rules)))
      (%build-policy name combining default (nreverse rules)))))

(defun make-policy (&key name (combining :forbid-overrides) default rules)
  "A POLICY from keyword arguments: NAME (string, symbol or NIL), COMBINING,
DEFAULT, RULES (DSL rule forms or POLICY-RULEs, in order). Same checks and
restarts as PARSE-POLICY."
  (%build-policy name combining default rules))

(defun make-layered-policy (&rest layers)
  "A LAYERED-POLICY of LAYERS, outer first. A layer is a POLICY, a
LAYERED-POLICY (flattened in place) or a symbol naming one (FIND-POLICY).
Anything else signals POLICY-SYNTAX-ERROR."
  (labels ((flat (x)
             (typecase x
               (policy (list x))
               (layered-policy (copy-list (layered-policy-layers x)))
               (symbol (let ((p (and x (find-policy x))))
                         (if p
                             (flat p)
                             (%syntax-error x "no policy is named ~S" x))))
               (t (%syntax-error x "a layer is a POLICY, a LAYERED-POLICY or a policy name")))))
    (make-instance 'layered-policy :layers (mapcan #'flat layers))))

(defun %rule-canonical-condition (rule)
  (let ((c (policy-rule-condition rule)))
    (and c (%canonical-condition
            c (lambda (reason) (%syntax-error c "~A" reason))))))

(defun policy-form (policy)
  "POLICY's canonical data form (see the file header). For a POLICY only."
  (check-type policy policy)
  (list :name (policy-name policy)
        :combining (policy-combining policy)
        :default (policy-default policy)
        :rules (mapcar (lambda (r)
                         (list :id (policy-rule-id r)
                               :effect (policy-rule-effect r)
                               :action (policy-rule-action r)
                               :resource (policy-rule-resource r)
                               :when (%rule-canonical-condition r)
                               :obligations (copy-tree (policy-rule-obligations r))
                               :feedback (policy-rule-feedback r)))
                       (policy-rules policy))))

;;; Requests

(defun %canonical-fact (f)
  (unless (and (consp f) (%proper-list-p f)
               (let ((rel (first f)))
                 (or (stringp rel)
                     (and rel (symbolp rel) (not (logic-protocol:logic-variable-p rel)))))
               (every #'%name-datum-p (rest f)))
    (%request-error f "a context fact is a ground atom (REL ARG...) of names and integers"))
  (mapcar #'%name f))

(defun make-policy-request (&key action resources context)
  "A POLICY-REQUEST (model.lisp: REQUESTS). RESOURCES NIL means (\"*\").
Invalid parts signal POLICY-REQUEST-ERROR."
  (unless (stringp action)
    (%request-error action "the action must be a string"))
  (unless (and (%proper-list-p resources) (every #'stringp resources))
    (%request-error resources "the resources must be a list of strings"))
  (unless (%proper-list-p context)
    (%request-error context "the context must be a list of facts"))
  (make-instance 'policy-request
                 :action action
                 :resources (if resources (copy-list resources) (list "*"))
                 :context (remove-duplicates (mapcar #'%canonical-fact context)
                                             :test #'equal :from-end t)))

(defun %coerce-request (request)
  (cond ((typep request 'policy-request) request)
        ((and (consp request) (%proper-list-p request) (evenp (length request)))
         (loop for (k nil) on request by #'cddr
               do (unless (member k '(:action :resource :resources :context))
                    (%request-error request "unknown request key ~S" k)))
         (destructuring-bind (&key action (resource nil resource-p) resources context)
             request
           (when (and resource-p (getf request :resources))
             (%request-error request "give :resource or :resources, not both"))
           (when (and resource-p (not (stringp resource)))
             (%request-error resource "the resource must be a string"))
           (make-policy-request :action action
                                :resources (if resource-p (list resource) resources)
                                :context context)))
        (t (%request-error request "a request is a POLICY-REQUEST or a plist (:action A :resources (R...))"))))
