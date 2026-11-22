(defsystem "policy-protocol"
  :version "0.1.0"
  :description "CLOS policy protocol for cl-stack: permit / forbid / ask, layers, obligations, explained decisions on logic-protocol (Track L)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "logic-protocol" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "model")
               (:file "parse")
               (:file "evaluate")
               (:file "explain")
               (:file "load")
               (:file "analysis"))
  :in-order-to ((test-op (test-op "policy-protocol/tests"))))

(defsystem "policy-protocol/settings"
  :description "Policies as settings data: JSON (json-protocol) and TOML (toml-protocol) loading, JSON writing"
  :depends-on ("policy-protocol" "json-protocol" "toml-protocol")
  :pathname "src/settings"
  :serial t
  :components ((:file "settings")))

(defsystem "policy-protocol/tests"
  :description "Contract tests: ai-agent-protocol golden permissions (:last-match), combining and layers, plan mode, obligations, traces, data loading, invalid policies, analysis stubs, per-request cost"
  :depends-on ("policy-protocol" "policy-protocol/settings"
               (:version "logic-backend-datalog" "0.1.0")
               "json-backend-jzon" "toml-backend-tomlet" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "golden-data")
               (:file "golden-test")
               (:file "glob-test")
               (:file "combining-test")
               (:file "plan-mode-test")
               (:file "obligation-test")
               (:file "trace-test")
               (:file "load-test")
               (:file "syntax-test")
               (:file "analysis-test")
               (:file "performance-test"))
  :perform (test-op (o c)
             ;; Policies evaluate on the default Datalog backend.
             (let ((ok (progv (list (find-symbol* :*logic-backend* :logic-protocol))
                           (list (symbol-call :logic-backend-datalog :make-datalog-backend))
                         (symbol-call :rove :run c))))
               (unless ok
                 (error "tests failed for ~A" (component-name c))))))
