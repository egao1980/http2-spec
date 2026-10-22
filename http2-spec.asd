(defsystem "http2-spec"
  :version "0.1.0"
  :description "RFC 9113 HTTP/2 stream states + flow control as a defspec (CLOS protocol + Quint model), with an MBT driver for the http2 fork"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "spec-protocol" "0.1.1"))
  ;; http2/core needs the OCI mgl-pax macros; scripts/ci/pre-install.lisp evicts the QL dummy.
  :properties (:cl-repo (:ci (:with ("mgl-pax" "dref" "autoload"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "derive")
               (:file "stream-state")
               (:file "variants"))
  :in-order-to ((test-op (test-op "http2-spec/tests"))))

(defsystem "http2-spec/fork"
  :description "spec-protocol.mbt driver replaying http2-spec traces against http2/core (in-process client/server pair)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http2-spec" (:version "http2/core" "2.1.2"))
  :pathname "src"
  :components ((:file "fork-driver")))

(defsystem "http2-spec/tests"
  :depends-on ("http2-spec" "http2-spec/fork" (:version "spec-backend-quint" "0.1.1")
               "process-backend-uiop" "json-backend-jzon" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "model-test")
               (:file "quint-test")
               (:file "fork-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
