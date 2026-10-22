(in-package #:http2-spec/tests)

;;; FORK-DRIVER against http2/core. Frozen traces need no quint; fresh simulations do.
;;;
;;; tests/fixtures/regression-*.itf.json are the first traces that diverged on the unfixed fork
;;; (http2 2.1.1), one per bug; each one now conforms:
;;;   settings-payload-offset             SETTINGS reader asserted its payload starts at offset 0
;;;   initial-window-adjust-6-9-2         SETTINGS_INITIAL_WINDOW_SIZE did not adjust open streams
;;;   stream-window-overflow-6-9-1        stream WINDOW_UPDATE past 2^31-1 was accepted
;;;   connection-window-overflow-6-9-1    connection WINDOW_UPDATE past 2^31-1 was accepted
;;;   push-promise-refused-8-4            PUSH_PROMISE raised a Lisp error, not PROTOCOL_ERROR
;;;   initial-window-too-large-6-5-2      oversized initial window gave PROTOCOL_ERROR, not FLOW_CONTROL
;;;   max-concurrent-streams-5-1-2        streams above SETTINGS_MAX_CONCURRENT_STREAMS were admitted
;;;                                       (once refused, core called http2/server's LOG-CLOSED-STREAM)

(defun replay-on-fork (trace &optional (driver (http2-spec.fork:make-fork-driver)))
  "Replay TRACE on DRIVER. Returns status (:ok / :aborted), the divergence or NIL, and the driver."
  (handler-case (values (nth-value 1 (spec-protocol.mbt:replay-trace driver trace)) nil driver)
    (spec-conformance-divergence (c) (values :diverged c driver))))

(defun divergence-text (c)
  (and c (format nil "~a/~a expected ~s, fork ~s" (divergence-action c) (divergence-variable c)
                 (divergence-expected c) (divergence-actual c))))

(deftest fork-conforms-to-frozen-traces
  (loop for prefix in '("fork" "regression")
        for traces = (fixture-traces prefix)
        do (ok traces (format nil "~a fixtures present" prefix))
           (loop for (name . trace) in traces
                 do (multiple-value-bind (status c driver) (replay-on-fork trace)
                      (ok (member status '(:ok :aborted)) (or (divergence-text c) name))
                      (when (eq status :aborted)
                        (ok (http2-spec.fork:fork-driver-notes driver)
                            (format nil "~a: an aborted replay names its RFC alternative" name)))))))

(deftest regression-fixtures-cover-each-fork-bug
  (ok (= 7 (length (fixture-traces "regression")))))

(defclass unlimited-fork-driver (http2-spec.fork:fork-driver) ()
  (:documentation "Fork driver whose endpoints never enforce their own MAX_CONCURRENT_STREAMS."))

(defmethod spec-protocol.mbt:mbt-apply :before ((d unlimited-fork-driver) sut action picks state)
  (declare (ignore sut action picks state))
  (dolist (c (list (http2-spec.fork:driver-client d) (http2-spec.fork:driver-server d)))
    (setf (http2/core::get-max-concurrent-streams c) nil)))

(deftest driver-detects-an-implementation-bug
  (destructuring-bind ((name . trace)) (fixture-traces "regression-max-concurrent-streams")
    (multiple-value-bind (status c)
        (replay-on-fork trace (make-instance 'unlimited-fork-driver :spec 'http2-fork-spec))
      (ok (eq :diverged status) name)
      (when c
        (ok (string= "headers" (divergence-action c)))
        (ok (string= "outcome" (divergence-variable c)))
        (ok (equal (cons :stream-error +refused-stream+) (divergence-expected c)))
        (ok (eq :no-error (divergence-actual c)))))))

(deftest fork-conforms-to-fresh-simulations
  (with-quint
    (loop for step in '(nil "rogueStep")
          for seed from 31
          do (let ((r (spec-protocol:simulate 'http2-fork-spec :step step :seed seed
                                                               :n-traces 10 :max-steps 25
                                                               :max-samples 25)))
               (ok (eq :ok (result-status r)) (or step "step"))
               (dolist (tr (result-traces r))
                 (multiple-value-bind (status c) (replay-on-fork tr)
                   (ok (member status '(:ok :aborted)) (or (divergence-text c) "conforms"))))))))
