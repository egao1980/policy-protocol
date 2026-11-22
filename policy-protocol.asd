(defsystem "policy-protocol"
  :version "0.0.0"
  :description "CLOS policy protocol for cl-stack: permit / forbid / ask, layers, obligations, explained decisions on logic-protocol (Track L)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "policy-protocol/tests"))))

(defsystem "policy-protocol/tests"
  :depends-on ("policy-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
