(defpackage #:http2-spec/tests
  (:use #:cl #:rove #:http2-spec)
  (:import-from #:spec-protocol
                #:find-spec #:spec-equal #:map-get #:make-set #:check-invariants
                #:spec-action-disabled #:spec-invariant-violation #:spec-conformance-divergence
                #:divergence-variable #:divergence-action #:divergence-expected #:divergence-actual
                #:result-status #:result-traces #:result-passed #:result-failed #:result-errors
                #:result-tool #:result-stdout)
  (:import-from #:spec-backend-quint #:quint-available-p #:java-available-p #:use-quint-backend))
(in-package #:http2-spec/tests)

(defun fixture (name)
  (asdf:system-relative-pathname "http2-spec" (format nil "tests/fixtures/~a" name)))

(defun fixture-text (name)
  "Read a fixture as UTF-8. Drop CR so a Windows checkout (CRLF) still matches LF emit."
  (with-open-file (in (fixture name) :external-format :utf-8)
    (let* ((text (make-string (file-length in)))
           (n (read-sequence text in)))
      (remove #\Return (subseq text 0 n)))))

(defun fixture-traces (prefix)
  "Frozen ITF traces tests/fixtures/PREFIX-*.itf.json, sorted by name."
  (mapcar (lambda (p) (cons (pathname-name p) (spec-protocol.itf:read-itf p)))
          (sort (directory (merge-pathnames (format nil "~a-*.itf.json" prefix) (fixture "")))
                #'string< :key #'namestring)))

(defmacro with-quint (&body body)
  "Run BODY only when the quint CLI is installed (QUINT_BIN or PATH); otherwise SKIP."
  `(if (quint-available-p)
       (progn ,@body)
       (skip "quint not installed")))

(defmacro with-java (&body body)
  `(if (and (quint-available-p) (java-available-p))
       (progn ,@body)
       (skip "JAVA_HOME (JDK >= 17) not set")))

(defun fresh (&optional (spec 'http2-stream-state))
  "An initialised HTTP2-STREAM-STATE with SPEC's instance constants."
  (let ((s (apply #'make-instance 'http2-stream-state
                  (loop for (sym . value) in (spec-protocol.mbt:instance-consts spec)
                        collect (intern (symbol-name sym) :keyword) collect value))))
    (init s)
    s))

(defun st (s p id) (map-get (map-get (h2-streams s) p) id))
(defun win (s p id) (map-get (map-get (h2-window s) p) id))
(defun cwin (s p) (map-get (h2-conn-window s) p))

(defun disabled-p (thunk)
  "True when THUNK signals SPEC-ACTION-DISABLED."
  (handler-case (progn (funcall thunk) nil)
    (spec-action-disabled () t)))

(setup
  (process-backend-uiop:use-uiop-process-backend)
  (json-backend-jzon:use-jzon-backend)
  (use-quint-backend))
