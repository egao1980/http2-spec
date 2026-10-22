(in-package #:http2-spec)

;;; Quint-only variants of HTTP2-STREAM-STATE (no CLOS expansion; same declarations otherwise).

(derive-spec 'http2-stream-state 'http2-fork-spec
             :module "Http2Fork" :instance-name "Http2ForkTest"
             ;; The http2 fork's client always announces SETTINGS_ENABLE_PUSH = 0 and has no
             ;; push support, so its conformance traces come from a push-disabled peer.
             :bindings '((push-enabled nil)))

(derive-spec 'http2-stream-state 'http2-buggy-concurrency-spec
             :module "Http2BuggyConcurrency" :instance-name "Http2BuggyConcurrencyTest"
             ;; Deliberately broken: the receiver admits every new stream, ignoring its own
             ;; SETTINGS_MAX_CONCURRENT_STREAMS. CONCURRENCY-LIMIT-5-1-2 must catch it.
             :decls '((action headers ((p str) (id int) (es bool))
                       (:guard live (headers-allowed p id))
                       (setf prev-streams streams
                             last-frame (rec (kind "HEADERS") (sender p) (id id)))
                       (let ((s (view p id))
                             (q (peer p)))
                         (if (in s (set :idle :reserved-local))
                             (all (setf streams (set-view (close-idle-below streams p id) p id (after-headers s es))
                                        outcome :no-error)
                                  (if (= s :idle)
                                      (setf next-stream-id (put next-stream-id p (+ id 2))
                                            window (set-window (set-window window p id (get initial-window-size q))
                                                               q id (get initial-window-size p)))))
                             (let ((r (receive-reaction (view q id) "HEADERS")))
                               (match r
                                 (:no-error (setf streams (set-view streams p id (after-headers s es))
                                                  outcome :no-error))
                                 ((stream-error c) (setf streams (set-view streams p id :closed)
                                                         outcome r))
                                 (_ (setf outcome r)))))))))
