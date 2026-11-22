(in-package #:policy-protocol)

;;; Evaluation (semantics: model.lisp). Runs on logic-protocol; never loads a
;;; SAT solver.
;;;
;;; COMPILATION (Design A, .team-solve/notes.md). Relations are fixed symbols
;;; of this package; data only ever becomes string, integer or keyword
;;; arguments. A policy rule is identified by its 0-based position I, a
;;; request resource by its position P.
;;;
;;; Request facts (only ever :ASSUMEd):
;;;   (%req-action RAW SLASHED)          SLASHED: backslashes → slashes
;;;   (%req-resource P RAW SLASHED)
;;;   (%ctx "rel" arg...)                each context fact, names normalized
;;;   (%req-host-suffix P SUF)           SUF: the text after each "." of RAW
;;;                                      (only for policies with a :host rule)
;;; Per rule I: (%action I), (%resource I P), (%holds I N) for condition node
;;; N (0 = the whole condition, 1..k = its top-level conjuncts in order),
;;; (%applies I E P). Combining (fixed per mode) derives (%winner I E P),
;;; (%res-effect P E), (%effect E), (%decides I E P) and (%default D); one
;;; output relation (%out TAG A B C) makes each layer's evaluation a single
;;; QUERY. Every derived fact depends positively on a request fact, so the
;;; model is empty at rest; negation only reads lower strata, for every
;;; policy (conditions are in negation normal form over %ctx).

;;; Strings and patterns

(defun %slash (s) (substitute #\/ #\\ s))

(defun %wild-p (s) (find-if (lambda (c) (member c '(#\* #\?))) s))

(defun %glob-bodies (glob atom-fn)
  "Bodies (lists of literals), one of which holds iff ai-agent-protocol's
PERMISSION-GLOB-MATCH-P holds for GLOB. ATOM-FN maps the term for the slashed
request string to the request atom."
  (let* ((g (%slash glob))
         (n (length g))
         (bodies '()))
    (cond ((string= g "*") (push (list (funcall atom-fn '?s)) bodies))
          ((not (%wild-p g)) (push (list (funcall atom-fn g)) bodies))
          (t (push (list (funcall atom-fn '?s) `(glob ,g ?s)) bodies)))
    ;; The bare command: compared literally, as PERMISSION-GLOB-MATCH-P does.
    (when (and (> n 2) (string= " *" g :start2 (- n 2)))
      (push (list (funcall atom-fn (subseq g 0 (- n 2)))) bodies))
    (nreverse bodies)))

(defun %path-pattern (p)
  (let ((p (%slash p)))
    (if (and (> (length p) 1) (char= (char p (1- (length p))) #\/))
        (subseq p 0 (1- (length p)))
        p)))

(defun %resource-bodies (pattern)
  "Bodies binding ?P (the resource position) for canonical PATTERN."
  (if (stringp pattern)
      (%glob-bodies pattern (lambda (s) `(%req-resource ?p ?r ,s)))
      (destructuring-bind (kind arg) pattern
        (ecase kind
          (:prefix `(((%req-resource ?p ?r ?s) (prefix ,arg ?r))))
          (:path (let ((p (%path-pattern arg)))
                   (if (string= p "/")
                       `(((%req-resource ?p ?r ?s) (prefix "/" ?s)))
                       `(((%req-resource ?p ?r ,p))
                         ((%req-resource ?p ?r ?s) (prefix ,(concatenate 'string p "/") ?s))))))
          (:host `(((%req-resource ?p ,arg ?s))
                   ((%req-host-suffix ?p ,arg))))))))

(defun %condition-rules (i conjuncts)
  "Rules for (%holds I N): node 0 is the AND of nodes 1..k (the CONJUNCTS,
canonical); inner nodes are numbered after k. Negation is pushed down to the
%ctx leaves, each gated by (%action I)."
  (let ((next (1+ (length conjuncts)))
        (out '()))
    (labels ((node (c neg id)
               (case (and (keywordp (first c)) (first c))
                 (:not (node (second c) (not neg) id))
                 ((:and :or)
                  (let ((kids (loop repeat (length (rest c))
                                    collect (prog1 next (incf next)))))
                    (mapc (lambda (x k) (node x neg k)) (rest c) kids)
                    (if (eq (eq (first c) :and) (not neg))
                        (push `(rule (%holds ,i ,id) :- ,@(mapcar (lambda (k) `(%holds ,i ,k)) kids))
                              out)
                        (dolist (k kids)
                          (push `(rule (%holds ,i ,id) :- (%holds ,i ,k)) out)))))
                 (t (let ((a `(%ctx ,@c)))
                      (push `(rule (%holds ,i ,id) :- (%action ,i) ,(if neg `(not ,a) a))
                            out))))))
      (loop for c in conjuncts for j from 1 do (node c nil j))
      (push `(rule (%holds ,i 0) :- ,@(loop for j from 1 to (length conjuncts)
                                            collect `(%holds ,i ,j)))
            out)
      (nreverse out))))

(defun %rule-rules (i rule)
  (let ((effect (policy-rule-effect rule))
        (condition (%rule-canonical-condition rule)))
    (append
     (loop for b in (%glob-bodies (policy-rule-action rule)
                                  (lambda (s) `(%req-action ?a ,s)))
           collect `(rule (%action ,i) :- ,@b))
     (loop for b in (%resource-bodies (policy-rule-resource rule))
           collect `(rule (%resource ,i ?p) :- (%action ,i) ,@b))
     (when condition
       (%condition-rules i (%condition-conjuncts condition)))
     (list `(rule (%applies ,i ,effect ?p) :- (%resource ,i ?p)
                  ,@(when condition `((%holds ,i 0))))))))

(defparameter +forbid-overrides-rules+
  '((rule (%has ?p ?e) :- (%applies ?i ?e ?p))
    (rule (%res-effect ?p :forbid) :- (%has ?p :forbid))
    (rule (%res-effect ?p :ask) :- (%has ?p :ask) (not (%has ?p :forbid)))
    (rule (%res-effect ?p :permit) :- (%has ?p :permit) (not (%has ?p :ask)) (not (%has ?p :forbid)))
    (rule (%winner ?i ?e ?p) :- (%applies ?i ?e ?p) (%res-effect ?p ?e))))

(defparameter +last-match-rules+
  '((rule (%later ?i ?p) :- (%applies ?i ?e ?p) (%applies ?j ?f ?p) (< ?i ?j))
    (rule (%winner ?i ?e ?p) :- (%applies ?i ?e ?p) (not (%later ?i ?p)))
    (rule (%res-effect ?p ?e) :- (%winner ?i ?e ?p))))

(defparameter +combining-rules+
  '((rule (%decided ?p) :- (%res-effect ?p ?e))
    (rule (%undecided) :- (%req-resource ?p ?r ?s) (not (%decided ?p)))
    (rule (%some ?e) :- (%res-effect ?p ?e))
    (rule (%effect :forbid) :- (%some :forbid))
    (rule (%effect :ask) :- (%some :ask) (not (%some :forbid)))
    (rule (%effect :permit) :- (%some :permit) (not (%some :ask)) (not (%some :forbid))
          (not (%undecided)))
    (rule (%cand-res ?p) :- (%effect ?e) (%res-effect ?p ?e))
    (rule (%later-res ?p) :- (%cand-res ?p) (%cand-res ?q) (< ?q ?p))
    (rule (%dec-res ?p) :- (%cand-res ?p) (not (%later-res ?p)))
    (rule (%cand-rule ?i ?e ?p) :- (%dec-res ?p) (%effect ?e) (%winner ?i ?e ?p))
    (rule (%not-first-rule ?i) :- (%cand-rule ?i ?e ?p) (%cand-rule ?j ?e ?p) (< ?j ?i))
    (rule (%decides ?i ?e ?p) :- (%cand-rule ?i ?e ?p) (not (%not-first-rule ?i)))
    (rule (%out :applies ?i ?e ?p) :- (%applies ?i ?e ?p))
    (rule (%out :winner ?i ?e ?p) :- (%winner ?i ?e ?p))
    (rule (%out :decides ?i ?e ?p) :- (%decides ?i ?e ?p))))

(defun %default-rules (default)
  `((rule (%decided-by-rule) :- (%effect ?e))
    (rule (%default ,default) :- (%req-action ?a ?s) (not (%decided-by-rule)))
    (rule (%out :default ,default 0 0) :- (%default ,default))))

(defun %compile-policy (policy)
  "The logic-protocol rule forms for POLICY."
  (append
   (loop for r in (policy-rules policy) for i from 0 append (%rule-rules i r))
   (ecase (policy-combining policy)
     (:forbid-overrides +forbid-overrides-rules+)
     (:last-match +last-match-rules+))
   +combining-rules+
   (when (policy-default policy) (%default-rules (policy-default policy)))))

(defun %policy-cache (policy key compute)
  (let ((cell (getf (%policy-extra policy) key '%none)))
    (if (eq cell '%none)
        (let ((v (funcall compute)))
          (setf (getf (%policy-extra policy) key) v)
          v)
        cell)))

(defun %rule-vector (policy)
  (%policy-cache policy :rules (lambda () (coerce (policy-rules policy) 'vector))))

(defun %has-host-p (policy)
  (%policy-cache policy :host
                 (lambda ()
                   (and (some (lambda (r) (let ((p (policy-rule-resource r)))
                                            (and (consp p) (eq (first p) :host))))
                              (policy-rules policy))
                        t))))

(defun policy-program (policy)
  "POLICY's compiled logic-protocol PROGRAM, made on first use with
logic-protocol:*LOGIC-BACKEND* (LOGIC-NO-BACKEND when there is none) and
the same object on every later call."
  (check-type policy policy)
  (or (%policy-program policy)
      ;; LOGIC-NO-BACKEND (USE-BACKEND) comes out of MAKE-PROGRAM unwrapped;
      ;; the slot is set only once the program exists.
      (setf (%policy-program policy)
            (logic-protocol:make-program :rules (%compile-policy policy) :budget :unlimited))))

;;; Requests as facts

(defun %request-facts (policy request)
  (let ((action (request-action request)))
    (append
     (list (list '%req-action action (%slash action)))
     (loop for r in (request-resources request) for p from 0
           collect (list '%req-resource p r (%slash r)))
     (mapcar (lambda (f) (cons '%ctx f)) (request-context request))
     (when (%has-host-p policy)
       (loop for r in (request-resources request) for p from 0
             nconc (loop for dot = (position #\. r) then (position #\. r :start (1+ dot))
                         while dot
                         collect (list '%req-host-suffix p (subseq r (1+ dot)))))))))

;;; One layer

(defun %dedupe-equal (list)
  (remove-duplicates list :test #'equal :from-end t))

(defun %evaluate-layer (policy request budget)
  "→ (values POLICY-TRACE PERMIT-OBLIGATIONS). Effect, deciding resource and
rule, default and applicable rules are the rows of one QUERY on the compiled
program; Lisp maps positions to objects."
  (let* ((program (policy-program policy))
         (facts (%request-facts policy request))
         (rows (mapcar (lambda (b) (mapcar #'cdr b))
                       (logic-protocol:query program '(%out ?tag ?a ?b ?c)
                                             :assume facts :budget budget)))
         (rules (%rule-vector policy))
         (resources (request-resources request)))
    (flet ((tagged (tag) (remove tag rows :key #'first :test-not #'eq)))
      (let* ((decides (first (tagged :decides)))
             (default (first (tagged :default)))
             (effect (cond (decides (third decides))
                           (default (second default))
                           (t :no-decision)))
             (i (second decides))
             (p (fourth decides)))
        (values
         (make-instance 'policy-trace
                        :policy policy
                        :effect effect
                        :rule (and decides (aref rules i))
                        :resource (and decides (nth p resources))
                        :default-p (and (not decides) default t)
                        :applicable (map 'list (lambda (k) (aref rules k))
                                         (sort (remove-duplicates (mapcar #'second (tagged :applies)))
                                               #'<))
                        :proof (and decides
                                    (logic-protocol:explain program `(%applies ,i ,effect ,p)
                                                            :assume facts)))
         (when (and decides (eq effect :permit))
           (let ((winners (remove :permit (tagged :winner) :key #'third :test-not #'eq)))
             (%dedupe-equal
              (loop for row in (sort (copy-list winners)
                                     (lambda (a b)
                                       (if (eq (policy-combining policy) :last-match)
                                           (or (< (fourth a) (fourth b))
                                               (and (= (fourth a) (fourth b)) (< (second a) (second b))))
                                           (< (second a) (second b)))))
                    append (copy-tree (policy-rule-obligations (aref rules (second row)))))))))))))

;;; Layers

(defun %rank (effect)
  (ecase effect (:permit 0) (:ask 1) (:forbid 2)))

(defun %unmet-feedback (unmet)
  (format nil "Unmet obligation~P: the caller does not honour ~{~A~^, ~}."
          (length unmet) (%dedupe-equal (mapcar #'first unmet))))

(defun %decide (layers request budget obligations)
  (let* ((request (%coerce-request request))
         (traces '())
         (permit-obligations '()))
    (dolist (layer layers)
      (multiple-value-bind (trace obs) (%evaluate-layer layer request budget)
        (push trace traces)
        (push obs permit-obligations)))
    (setf traces (nreverse traces)
          permit-obligations (nreverse permit-obligations))
    (let ((result :no-decision))
      (loop for tr in traces
            for first = t then nil
            for e = (trace-effect tr)
            do (when (or first
                         (and (member e '(:ask :forbid))
                              (or (eq result :no-decision) (> (%rank e) (%rank result)))))
                 (setf result e)))
      (let* ((decider (and (not (eq result :no-decision))
                           (find result traces :key #'trace-effect)))
             (rule (and decider (trace-rule decider)))
             (obs (and (eq result :permit)
                       (%dedupe-equal
                        (loop for tr in traces for o in permit-obligations
                              when (eq (trace-effect tr) :permit) append o))))
             (honoured (mapcar #'%name obligations))
             (unmet (remove-if (lambda (o) (member (first o) honoured :test #'equal)) obs)))
        (make-instance 'decision
                       :effect (if unmet :forbid result)
                       :obligations (if unmet '() obs)
                       :unmet-obligations unmet
                       :feedback (cond (unmet (%unmet-feedback unmet))
                                       ((and rule (member result '(:ask :forbid)))
                                        (policy-rule-feedback rule)))
                       :rule rule
                       :layer (and decider (trace-policy decider))
                       :resource (and decider (trace-resource decider))
                       :default-p (and decider (trace-default-p decider))
                       :proof (and decider (trace-proof decider))
                       :trace traces
                       :request request)))))

(defgeneric evaluate-policy (policy request &key budget obligations)
  (:documentation "The DECISION of POLICY (a POLICY, a LAYERED-POLICY or a
symbol naming one) for REQUEST (a POLICY-REQUEST or a request plist).
BUDGET: a logic-protocol budget designator for this request's logic work.
OBLIGATIONS: the obligation kinds the caller honours (default
*POLICY-OBLIGATIONS*). See model.lisp."))

(defmethod evaluate-policy ((policy policy) request
                            &key budget (obligations *policy-obligations*))
  (%decide (list policy) request budget obligations))

(defmethod evaluate-policy ((policy layered-policy) request
                            &key budget (obligations *policy-obligations*))
  (%decide (layered-policy-layers policy) request budget obligations))

(defmethod evaluate-policy ((policy symbol) request
                            &key budget (obligations *policy-obligations*))
  (let ((p (and policy (find-policy policy))))
    (unless p
      (error 'policy-error :policy policy))
    (evaluate-policy p request :budget budget :obligations obligations)))

(defmethod evaluate-policy (policy request &key budget obligations)
  (declare (ignore request budget obligations))
  (error 'policy-error :policy policy))

;;; Why not

(defun decision-why-not (decision rule)
  "NIL when RULE (a POLICY-RULE or a rule id) decided its layer in DECISION;
else a POLICY-WHY-NOT (model.lisp: WHY NOT). An id no layer has signals
POLICY-ERROR."
  (let (found trace)
    (dolist (tr (decision-trace decision))
      (let ((r (find-if (lambda (x) (if (typep rule 'policy-rule)
                                        (eq x rule)
                                        (and (stringp rule) (string= (policy-rule-id x) rule))))
                        (policy-rules (trace-policy tr)))))
        (when r (setf found r trace tr) (return))))
    (unless found
      (error 'policy-error :policy (and (decision-trace decision)
                                        (trace-policy (first (decision-trace decision))))))
    (when (eq found (trace-rule trace))
      (return-from decision-why-not nil))
    (let* ((policy (trace-policy trace))
           (program (policy-program policy))
           (facts (%request-facts policy (%decision-request decision)))
           (i (position found (policy-rules policy))))
      (flet ((holds (pattern)
               (logic-protocol:holds-p program pattern :assume facts :budget :unlimited))
             (why (reason &optional literal)
               (make-instance 'policy-why-not :rule found :reason reason :literal literal)))
        (cond ((not (holds `(%action ,i))) (why :action))
              ((not (holds `(%resource ,i ?p))) (why :resource))
              ((and (policy-rule-condition found) (not (holds `(%holds ,i 0))))
               (why :condition
                    (loop for c in (%condition-conjuncts (policy-rule-condition found))
                          for j from 1
                          unless (holds `(%holds ,i ,j)) return c)))
              (t (why :overridden)))))))
