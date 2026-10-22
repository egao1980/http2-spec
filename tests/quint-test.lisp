(in-package #:http2-spec/tests)

;;; The Quint side: every test skips without the quint CLI (QUINT_BIN or PATH).

(defparameter *specs* '(http2-stream-state http2-fork-spec http2-buggy-concurrency-spec))

(deftest http2-golden
  (ok (string= (fixture-text "http2.qnt") (spec-protocol:emit 'http2-stream-state))
      "emitted Quint matches tests/fixtures/http2.qnt (regenerate with FREEZE-FIXTURES, never by hand)"))

(deftest typecheck-specs
  (with-quint
    (dolist (spec *specs*)
      (let ((r (spec-protocol:typecheck spec)))
        (ok (eq :ok (result-status r)) (format nil "~a: ~{~a~^; ~}" spec (result-errors r)))))))

(deftest quint-runs
  (with-quint
    (let ((r (spec-protocol:run-tests 'http2-stream-state)))
      (ok (eq :ok (result-status r)))
      (ok (equal '("openAndFinishTest" "refusedAboveLimitTest") (sort (copy-list (result-passed r)) #'string<))))))

(deftest simulation-keeps-invariants
  (with-quint
    (loop for spec in '(http2-stream-state http2-fork-spec)
          do (loop for step in '(nil "rogueStep")
                   for seed from 21
                   do (let ((r (spec-protocol:simulate spec :step step :invariants *invariants* :seed seed
                                                            :n-traces 2 :max-steps 20 :max-samples 500)))
                        (ok (eq :ok (result-status r)) (format nil "~a ~a" spec (or step "step")))
                        (dolist (tr (result-traces r))
                          (ok (eq :ok (nth-value 1 (replay-on-clos spec tr))) "trace replays on CLOS")))))))

(deftest buggy-variant-is-caught
  (with-quint
    (let ((r (spec-protocol:simulate 'http2-buggy-concurrency-spec
                                     :step "rogueStep" :invariants '(concurrency-limit-5-1-2)
                                     :seed 7 :max-steps 20 :max-samples 2000)))
      (ok (eq :violation (result-status r)) "CONCURRENCY-LIMIT-5-1-2 fails on the buggy receiver")
      (ok (= 1 (length (result-traces r))))
      (ok (signals (replay-on-clos 'http2-buggy-concurrency-spec (first (result-traces r)))
                   'spec-conformance-divergence)
          "the correct CLOS protocol diverges from the buggy trace"))
    (let ((r (spec-protocol:simulate 'http2-buggy-concurrency-spec
                                     :invariants *invariants* :seed 7 :max-steps 20 :max-samples 200)))
      (ok (eq :ok (result-status r)) "with compliant senders only, the receiver bug is unobservable"))))

(deftest verify-bounded-with-apalache
  (with-java
    (let ((r (spec-protocol:verify 'http2-stream-state :invariants *invariants* :max-steps 2)))
      (ok (eq :ok (result-status r)) (result-stdout r))
      (ok (eq :apalache (result-tool r))))))

;;; Fixture generation (run by hand with quint installed; the tests above only read them)

(defun freeze (prefix traces)
  (loop for tr in traces
        for i from 0
        do (spec-protocol.itf:write-itf tr (fixture (format nil "~a-~d.itf.json" prefix i)))))

(defun freeze-fixtures ()
  "Rewrite tests/fixtures: the golden http2.qnt and the ITF traces the replay tests read."
  (with-open-file (out (fixture "http2.qnt") :direction :output :if-exists :supersede
                                             :external-format :utf-8)
    (write-string (spec-protocol:emit 'http2-stream-state) out))
  ;; regression-* traces are pinned evidence of fork bugs found by FORK-DRIVER; keep them
  (dolist (old (directory (merge-pathnames "*.itf.json" (fixture ""))))
    (unless (uiop:string-prefix-p "regression-" (pathname-name old))
      (delete-file old)))
  (flet ((sim (spec step seed n &rest keys)
           (let ((r (apply #'spec-protocol:simulate spec
                           (append keys (list :step step :seed seed :n-traces n
                                              :max-steps 25 :max-samples (max n 100))))))
             (result-traces r))))
    (freeze "model-step" (sim 'http2-stream-state nil 1 2 :invariants *invariants*))
    (freeze "model-rogue" (sim 'http2-stream-state "rogueStep" 2 2 :invariants *invariants*))
    (freeze "fork-step" (sim 'http2-fork-spec nil 3 4 :invariants *invariants*))
    (freeze "fork-rogue" (sim 'http2-fork-spec "rogueStep" 4 6 :invariants *invariants*))
    (freeze "buggy-concurrency" (sim 'http2-buggy-concurrency-spec "rogueStep" 7 1
                                     :invariants '(concurrency-limit-5-1-2) :max-samples 2000))))
