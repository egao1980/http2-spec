(defpackage #:http2-spec
  (:use #:cl)
  (:export
   ;; the spec: class, accessors, actions
   #:http2-stream-state
   #:h2-streams #:h2-window #:h2-conn-window #:h2-next-stream-id
   #:h2-initial-window-size #:h2-max-concurrent-streams #:h2-goaway-sent
   #:h2-outcome #:h2-last-frame #:h2-prev-streams
   #:h2-stream-ids #:h2-max-window #:h2-default-window #:h2-default-max-streams
   #:h2-push-enabled
   #:init #:headers #:data #:window-update #:rst-stream #:push-promise #:goaway #:settings
   ;; sender-side discipline (what a compliant endpoint may send)
   #:may-send-headers #:may-send-data #:may-send-window-update #:may-send-rst
   #:may-push #:may-send-settings
   #:view #:active-count #:peer #:initiator #:mirror
   #:stream-state #:outcome
   #:+protocol-error+ #:+flow-control-error+ #:+stream-closed+ #:+refused-stream+ #:+cancel+
   #:*invariants*
   #:windows-bounded-6-9-1 #:stream-ids-monotonic-5-1-1 #:concurrency-limit-5-1-2
   #:legal-transitions-5-1 #:closed-is-terminal #:data-only-when-open-6-1
   ;; derived Quint-only variants
   #:derive-spec #:http2-fork-spec #:http2-buggy-concurrency-spec))
