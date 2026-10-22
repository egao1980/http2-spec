(in-package #:http2-spec/tests)

;;; The CLOS side of HTTP2-STREAM-STATE: RFC 9113 scenarios, then frozen Quint traces replayed
;;; through spec-protocol.mbt's CLOS-DRIVER (own-trace conformance, no quint needed).

(deftest open-send-and-close-5-1
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (ok (eq :open (st s "client" 1)))
    (ok (eq :open (st s "server" 1)))
    (ok (= 3 (map-get (h2-next-stream-id s) "client")))
    (ok (= 2 (win s "client" 1)) "send window = peer's SETTINGS_INITIAL_WINDOW_SIZE")
    (data s "client" 1 2 t)
    (ok (eq :half-closed-local (st s "client" 1)))
    (ok (eq :half-closed-remote (st s "server" 1)))
    (ok (= 0 (win s "client" 1)))
    (ok (= 0 (cwin s "client")) "DATA counts against the connection window too")
    (headers s "server" 1 t)
    (ok (eq :closed (st s "client" 1)))
    (ok (eq :closed (st s "server" 1)))
    (ok (eq :no-error (h2-outcome s)))))

(deftest new-stream-closes-lower-idle-streams-5-1-1
  (let ((s (fresh)))
    (headers s "client" 3 nil)
    (ok (eq :closed (st s "client" 1)))
    (ok (eq :closed (st s "server" 1)))
    (ok (eq :idle (st s "client" 5)))
    (ng (may-send-headers s "client" 1) "identifiers cannot be reused")
    (headers s "client" 1 nil)
    (ok (equal (cons :connection-error +stream-closed+) (h2-outcome s)))))

(deftest only-clients-open-streams
  (let ((s (fresh)))
    (ok (disabled-p (lambda () (headers s "server" 2 nil))))
    (ok (disabled-p (lambda () (headers s "client" 2 nil))) "even ids belong to the server")))

(deftest refused-above-max-concurrent-streams-5-1-2
  (let ((s (fresh)))
    (settings s "server" 2 1)
    (headers s "client" 1 nil)
    (ng (may-send-headers s "client" 3) "a compliant client waits")
    (headers s "client" 3 nil)
    (ok (equal (cons :stream-error +refused-stream+) (h2-outcome s)))
    (ok (eq :closed (st s "server" 3)))
    (ok (eq :closed (st s "client" 3)))
    (ok (eq :open (st s "client" 1)))
    (ok (= 5 (map-get (h2-next-stream-id s) "client")) "a refused id is still used")))

(deftest data-on-half-closed-remote-is-stream-closed-6-1
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (data s "client" 1 0 t)
    (ng (may-send-data s "client" 1))
    (data s "client" 1 1 nil)
    (ok (equal (cons :stream-error +stream-closed+) (h2-outcome s)))
    (ok (eq :closed (st s "server" 1)))
    (ok (= 1 (cwin s "client")) "the rejected frame still counts against the connection window")))

(deftest frames-on-idle-are-connection-errors-5-1
  (let ((s (fresh)))
    (data s "client" 1 0 t)
    (ok (equal (cons :connection-error +protocol-error+) (h2-outcome s)))
    (ok (disabled-p (lambda () (goaway s "client"))) "nothing follows a connection error")))

(deftest window-update-overflow-6-9-1
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (window-update s "server" 1 (h2-max-window s))
    (ok (equal (cons :stream-error +flow-control-error+) (h2-outcome s)))
    (ok (eq :closed (st s "client" 1))))
  (let ((s (fresh)))
    (ng (may-send-window-update s "server" 0 (h2-max-window s)))
    (window-update s "server" 0 (h2-max-window s))
    (ok (equal (cons :connection-error +flow-control-error+) (h2-outcome s))))
  (let ((s (fresh)))
    (window-update s "server" 0 1)
    (ok (= 3 (cwin s "client")))))

(deftest settings-adjust-open-windows-6-9-2
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (data s "client" 1 1 nil)
    (settings s "server" 0 2)
    (ok (= -1 (win s "client" 1)) "a window may go negative")
    (ok (= 1 (cwin s "client")) "the connection window is not adjusted")
    (settings s "server" 3 2)
    (ok (= 2 (win s "client" 1))))
  (let ((s (fresh)))
    (settings s "server" (1+ (h2-max-window s)) 2)
    (ok (equal (cons :connection-error +flow-control-error+) (h2-outcome s)))))

(deftest push-promise-reserves-8-4
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (push-promise s "server" 1 2)
    (ok (eq :reserved-local (st s "server" 2)))
    (ok (eq :reserved-remote (st s "client" 2)))
    (ok (= 0 (active-count s "server" "server")) "reserved streams are not active")
    (headers s "server" 2 nil)
    (ok (eq :half-closed-remote (st s "server" 2)))
    (ok (eq :half-closed-local (st s "client" 2))))
  (let ((s (fresh 'http2-fork-spec)))
    (headers s "client" 1 nil)
    (ng (may-push s "server"))
    (push-promise s "server" 1 2)
    (ok (equal (cons :connection-error +protocol-error+) (h2-outcome s))
        "SETTINGS_ENABLE_PUSH = 0 makes a push a connection error")))

(deftest rst-stream-closes-both-views
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (rst-stream s "server" 1)
    (ok (eq :closed (st s "server" 1)))
    (ok (eq :closed (st s "client" 1)))
    (window-update s "client" 1 1)
    (ok (eq :no-error (h2-outcome s)) "WINDOW_UPDATE on a closed stream is ignored")))

(deftest goaway-stops-new-streams
  (let ((s (fresh)))
    (goaway s "server")
    (ok (disabled-p (lambda () (headers s "client" 1 nil))))))

(deftest invariants-catch-corruption
  (let ((s (fresh)))
    (headers s "client" 1 nil)
    (ok (check-invariants s))
    (setf (h2-next-stream-id s) (spec-protocol:map-put (h2-next-stream-id s) "client" 1))
    (ok (signals (check-invariants s :invariants '(stream-ids-monotonic-5-1-1)) 'spec-invariant-violation))))

;;; Frozen traces (regenerate with FREEZE-FIXTURES in quint-test.lisp, never by hand)

(defun replay-on-clos (spec trace)
  (spec-protocol.mbt:replay-trace
   (spec-protocol.mbt:make-clos-driver 'http2-stream-state
                                       :consts (spec-protocol.mbt:instance-consts spec))
   trace))

(deftest clos-replays-own-traces
  (loop for (prefix spec) in '(("model" http2-stream-state) ("fork" http2-fork-spec)
                                     ("regression" http2-fork-spec))
        for traces = (fixture-traces prefix)
        do (ok traces (format nil "~a fixtures present" prefix))
           (loop for (name . trace) in traces
                 do (multiple-value-bind (sut status) (replay-on-clos spec trace)
                      (ok (and (typep sut 'http2-stream-state) (eq status :ok)) name)))))

(deftest buggy-concurrency-trace-diverges
  ;; The buggy model admits a stream above SETTINGS_MAX_CONCURRENT_STREAMS; the real protocol
  ;; refuses it, so the outcome diverges at that HEADERS.
  (loop for (name . trace) in (fixture-traces "buggy-concurrency")
        do (let ((c (handler-case (progn (replay-on-clos 'http2-buggy-concurrency-spec trace) nil)
                      (spec-conformance-divergence (c) c))))
             (ok c name)
             (when c
               (ok (string= "headers" (divergence-action c)))
               (ok (string= "outcome" (divergence-variable c)))
               (ok (eq :no-error (divergence-expected c)))
               (ok (equal (cons :stream-error +refused-stream+) (divergence-actual c)))))))
