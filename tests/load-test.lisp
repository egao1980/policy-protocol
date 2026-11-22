(in-package #:policy-protocol/tests)

;;; LOAD-POLICY: the same model from sexp, JSON and TOML text (load.lisp);
;;; POLICY-TO-JSON round trip; untrusted input is never evaluated or interned.

(defparameter *plan-mode-sexp*
  "(defpolicy plan-mode (:combining :forbid-overrides)
  (permit :action \"bash\" :when (sandbox :confined)
          :obligation (:run :confined))
  (ask    :action \"bash\" :when (not (sandbox :confined))
          :feedback \"The sandbox is unavailable; this command runs unconfined.\")
  (forbid :action \"bash\"
          :when (and (setting :sandbox.failIfUnavailable) (not (sandbox :confined)))
          :feedback \"Sandbox required (sandbox.failIfUnavailable) and unavailable.\"))")

(defparameter *plan-mode-json*
  "{\"name\": \"plan-mode\",
 \"combining\": \"forbid-overrides\",
 \"rules\": [
  {\"effect\": \"permit\", \"action\": \"bash\", \"when\": [\"sandbox\", \"confined\"],
   \"obligations\": [[\"run\", \"confined\"]]},
  {\"effect\": \"ask\", \"action\": \"bash\", \"when\": {\"not\": [\"sandbox\", \"confined\"]},
   \"feedback\": \"The sandbox is unavailable; this command runs unconfined.\"},
  {\"effect\": \"forbid\", \"action\": \"bash\",
   \"when\": {\"and\": [[\"setting\", \"sandbox.failIfUnavailable\"],
                      {\"not\": [\"sandbox\", \"confined\"]}]},
   \"feedback\": \"Sandbox required (sandbox.failIfUnavailable) and unavailable.\"}]}")

(defparameter *plan-mode-toml*
  "name = \"plan-mode\"
combining = \"forbid-overrides\"

[[rules]]
effect = \"permit\"
action = \"bash\"
when = [\"sandbox\", \"confined\"]
obligations = [[\"run\", \"confined\"]]

[[rules]]
effect = \"ask\"
action = \"bash\"
when = { not = [\"sandbox\", \"confined\"] }
feedback = \"The sandbox is unavailable; this command runs unconfined.\"

[[rules]]
effect = \"forbid\"
action = \"bash\"
when = { and = [[\"setting\", \"sandbox.failIfUnavailable\"], { not = [\"sandbox\", \"confined\"] }] }
feedback = \"Sandbox required (sandbox.failIfUnavailable) and unavailable.\"
")

(defparameter *plan-mode-canonical*
  '(:name "plan-mode" :combining :forbid-overrides :default nil
    :rules ((:id "rule-0" :effect :permit :action "bash" :resource "*"
             :when ("sandbox" "confined") :obligations (("run" "confined")) :feedback nil)
            (:id "rule-1" :effect :ask :action "bash" :resource "*"
             :when (:not ("sandbox" "confined")) :obligations ()
             :feedback "The sandbox is unavailable; this command runs unconfined.")
            (:id "rule-2" :effect :forbid :action "bash" :resource "*"
             :when (:and ("setting" "sandbox.failifunavailable") (:not ("sandbox" "confined")))
             :obligations ()
             :feedback "Sandbox required (sandbox.failIfUnavailable) and unavailable."))))

(defparameter *plan-mode-requests*
  (loop for confined in '(nil t)
        append (loop for fail in '(nil t)
                     append (loop for action in '("bash" "read")
                                  collect (req action "make" (plan-context confined fail))))))

(defun decisions (policy)
  (mapcar (lambda (r)
            (let ((d (evaluate-policy policy r)))
              (list (decision-effect d) (rule-id d) (decision-obligations d)
                    (decision-feedback d))))
          *plan-mode-requests*))

(deftest fixtures-decode
  ;; Sanity: the fixtures are valid JSON / TOML for the stack's decoders.
  (ok (hash-table-p (json-protocol:decode *plan-mode-json*)))
  (ok (hash-table-p (toml-protocol:decode *plan-mode-toml*))))

(deftest canonical-form
  (ok (equal *plan-mode-canonical* (policy-form (plan-mode)))))

(deftest load-sexp-json-toml-agree
  (let ((reference (decisions (plan-mode))))
    (dolist (case (list (list :sexp *plan-mode-sexp*)
                        (list :json *plan-mode-json*)
                        (list :toml *plan-mode-toml*)))
      (destructuring-bind (format text) case
        (let ((p (ignore-errors (load-policy text :format format))))
          (ok (and p (equal *plan-mode-canonical* (policy-form p)))
              (format nil "~S: canonical form" format))
          (ok (and p (equal reference (decisions p)))
              (format nil "~S: same decisions" format)))))))

(deftest load-from-pathname-and-stream
  (dolist (case (list (list "json" *plan-mode-json*) (list "toml" *plan-mode-toml*)
                      (list "lisp" *plan-mode-sexp*)))
    (destructuring-bind (type text) case
      (uiop:with-temporary-file (:pathname path :stream out :type type)
        (write-string text out)
        :close-stream
        (ok (equal *plan-mode-canonical* (policy-form (load-policy path)))
            (format nil "format from the pathname type ~S" type)))))
  (with-input-from-string (in *plan-mode-json*)
    (ok (equal *plan-mode-canonical* (policy-form (load-policy in :format :json))))))

(deftest json-round-trip
  (let* ((p (parse-policy
             '(defpolicy round-trip (:combining :last-match :default :ask)
               (permit :id "git" :action "bash" :resource (:prefix "git ")
                :when (or (mode :plan) (and (mode :code) (not (sandbox :off))))
                :obligations ((:run :confined) (:log :audit 3)))
               (forbid :action "edit" :resource (:path "secret") :feedback "No.")
               (ask :action "fetch" :resource (:host "example.com"))
               (permit :action "read" :resource "*.lisp"))))
         (json (policy-to-json p))
         (back (load-policy json :format :json)))
    (ok (stringp json))
    (ok (equal (policy-form p) (policy-form back)))
    (ok (equal json (policy-to-json back)) "deterministic")
    (dolist (r (list (req "bash" "git status" '((mode :plan)))
                     (req "bash" "git status" '((mode :code) (sandbox :off)))
                     (req "edit" "secret/k")
                     (req "fetch" "api.example.com")
                     (req "read" "a.lisp")
                     (req "write" "x")))
      (ok (eq (effect p r) (effect back r)) (format nil "~S" r)))))

(defvar *policy-read-eval-probe* nil)

(deftest sexp-is-never-evaluated
  (setf *policy-read-eval-probe* nil)
  (let ((text "(defpolicy evil ()
                 (permit :action #.(progn (setf policy-protocol/tests::*policy-read-eval-probe* t) \"x\")))"))
    (ok (signals (load-policy text :format :sexp) 'policy-syntax-error))
    (ok (null *policy-read-eval-probe*) "#. did not run")
    (ok (eq :fallback (handler-bind ((policy-syntax-error
                                       (lambda (c) (declare (ignore c)) (use-value :fallback))))
                        (load-policy text :format :sexp)))
        "USE-VALUE returns a value instead")))

(deftest unreadable-sources
  (dolist (case '((:sexp "(defpolicy broken (")
                  (:sexp "")
                  (:sexp "(defpolicy a ()) (defpolicy b ())")
                  (:sexp "(+ 1 2)")
                  (:json "{\"rules\": [")
                  (:json "[1, 2]")
                  (:toml "rules = [")))
    (destructuring-bind (format text) case
      (ok (signals (load-policy text :format format) 'policy-syntax-error)
          (format nil "~S ~S" format text)))))

(defun symbol-anywhere-p (name)
  "Is a symbol named NAME (any case) present in some package?"
  (some (lambda (pkg)
          (or (find-symbol name pkg)
              (find-symbol (string-upcase name) pkg)))
        (list-all-packages)))

(deftest names-are-not-interned
  (let ((json "{\"rules\": [{\"effect\": \"forbid\", \"action\": \"bash\",
                \"when\": [\"zqxSetting7781\", \"zqxValue7781\"],
                \"obligations\": []}]}"))
    (ng (symbol-anywhere-p "zqxSetting7781"))
    (let ((p (load-policy json :format :json)))
      (ok (eq :forbid (effect p (req "bash" "x" '(("zqxsetting7781" "ZQXVALUE7781")))))))
    (ng (symbol-anywhere-p "zqxSetting7781") "JSON names stay strings")
    (ng (symbol-anywhere-p "zqxValue7781")))
  (let ((before (let (l) (do-symbols (s *package*) (push s l)) (length l))))
    (load-policy "(defpolicy zqx-policy-7782 () (permit :action \"a\" :when (zqx-relation-7782 :on)))"
                 :format :sexp)
    (ok (= before (let (l) (do-symbols (s *package*) (push s l)) (length l)))
        "no symbol interned in the caller's package")
    (ng (some (lambda (pkg) (and (not (eq pkg (find-package :keyword)))
                                 (find-symbol "ZQX-RELATION-7782" pkg)))
              (list-all-packages))
        "nor in any other package")))

(deftest unknown-format
  (ok (signals (load-policy "x: 1" :format :yaml) 'policy-format-unsupported)))
