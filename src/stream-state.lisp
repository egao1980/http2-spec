(in-package #:http2-spec)

;;; RFC 9113 stream states (§5.1), stream identifiers (§5.1.1), concurrency (§5.1.2) and flow
;;; control (§5.2, §6.9) as one DEFSPEC: a CLOS protocol and a Quint module.
;;;
;;; Both endpoints are modelled; every action is one frame sent by endpoint P and processed by its
;;; peer in the same step (in-order delivery, no frames in flight). Each endpoint keeps its own
;;; view of every stream, so a receiver that rejects a frame can disagree with the sender.
;;; Actions only carry the structural preconditions of a frame; whether a compliant endpoint may
;;; send it is MAY-SEND-* (used by STEP). ROGUE-STEP drops that discipline and exercises the
;;; receiver's mandatory reactions: STREAM_CLOSED / PROTOCOL_ERROR on wrong states (§5.1, §6.1),
;;; FLOW_CONTROL_ERROR on window overflow (§6.9.1, §6.9.2), REFUSED_STREAM above
;;; SETTINGS_MAX_CONCURRENT_STREAMS (§5.1.2), PROTOCOL_ERROR on unwanted pushes (§6.5.2, §8.4).
;;;
;;; Choices where the RFC allows several reactions (replay drivers must accept the others):
;;; any frame other than WINDOW_UPDATE / RST_STREAM on a closed stream is a connection error
;;; STREAM_CLOSED (§5.1 "closed", MAY); a refused stream uses REFUSED_STREAM (§5.1.2 also allows
;;; PROTOCOL_ERROR); every stream error may be escalated to a connection error (§5.4).
;;; Only clients open streams with HEADERS and only servers push (§5.1 "idle", §8.4).
;;; DEFAULT-WINDOW stands for the 65,535-octet initial windows of §6.9.2 so that small domains
;;; reach the window limits.

(defconstant +protocol-error+ 1)
(defconstant +flow-control-error+ 3)
(defconstant +stream-closed+ 5)
(defconstant +refused-stream+ 7)
(defconstant +cancel+ 8)

(spec-protocol:defspec http2-stream-state
    (:module "Http2"
     :accessor-prefix "h2-"
     :doc "RFC 9113 §5.1 stream states and §6.9 flow control, both endpoints, atomic delivery."
     :instance ("Http2Test"
                (stream-ids (set 1 2 3 4 5))
                (max-window 2147483647)
                (default-window 2)
                (default-max-streams 2)
                (data-sizes (set 0 1 2))
                (increments (set 1 2 2147483647))
                (window-settings (set 0 1 2 3))
                (stream-limits (set 0 1 2))
                (push-enabled t)))

  (const stream-ids (set int))
  (const max-window int)
  (const default-window int)
  (const default-max-streams int)
  (const data-sizes (set int))
  (const increments (set int))
  (const window-settings (set int))
  (const stream-limits (set int))
  (const push-enabled bool)

  (type stream-state (or :idle :reserved-local :reserved-remote :open
                         :half-closed-local :half-closed-remote :closed))
  (type outcome (or :no-error (stream-error int) (connection-error int)))
  (type frame (rec (kind str) (sender str) (id int)))

  (var streams (map str (map int stream-state)))
  (var window (map str (map int int)))
  (var conn-window (map str int))
  (var next-stream-id (map str int))
  (var initial-window-size (map str int))
  (var max-concurrent-streams (map str int))
  (var goaway-sent (map str bool))
  (var outcome outcome)
  (var last-frame frame)
  (var prev-streams (map str (map int stream-state)))

  (pure-val peers (set "client" "server"))
  (pure-val protocol-error 1)
  (pure-val flow-control-error 3)
  (pure-val stream-closed 5)
  (pure-val refused-stream 7)

  (pure-def peer ((p str)) (:returns str)
    (if (= p "client") "server" "client"))

  (pure-def initiator ((id int)) "§5.1.1: clients initiate odd-numbered streams, servers even-numbered."
    (:returns str)
    (if (= (% id 2) 1) "client" "server"))

  (pure-def mirror ((s stream-state)) "The peer's name for the same stream state."
    (:returns stream-state)
    (match s
      (:reserved-local :reserved-remote)
      (:reserved-remote :reserved-local)
      (:half-closed-local :half-closed-remote)
      (:half-closed-remote :half-closed-local)
      (_ s)))

  (pure-def active ((s stream-state)) "§5.1.2: streams counted against SETTINGS_MAX_CONCURRENT_STREAMS."
    (:returns bool)
    (in s (set :open :half-closed-local :half-closed-remote)))

  (pure-def sending ((s stream-state)) "The endpoint may still send DATA on the stream."
    (:returns bool)
    (in s (set :open :half-closed-remote)))

  (pure-def receiving ((s stream-state)) "§6.1: DATA is accepted only in open and half-closed (local)."
    (:returns bool)
    (in s (set :open :half-closed-local)))

  (pure-def has-send-window ((s stream-state)) "§6.9.2: windows a SETTINGS_INITIAL_WINDOW_SIZE change adjusts (active and not yet active)."
    (:returns bool)
    (in s (set :open :half-closed-remote :reserved-local)))

  (pure-def after-headers ((s stream-state) (es bool)) (:returns stream-state)
    (match s
      (:idle (if es :half-closed-local :open))
      (:reserved-local (if es :closed :half-closed-remote))
      (:open (if es :half-closed-local :open))
      (:half-closed-remote (if es :closed :half-closed-remote))
      (_ s)))

  (pure-def after-data ((s stream-state) (es bool)) (:returns stream-state)
    (if es
        (match s
          (:open :half-closed-local)
          (:half-closed-remote :closed)
          (_ s))
        s))

  (pure-def receive-reaction ((s stream-state) (kind str)) "§5.1: the receiver's reaction to HEADERS or DATA on a stream it holds in state S."
    (:returns outcome)
    (match s
      (:idle (connection-error protocol-error))
      (:reserved-local (connection-error protocol-error))
      (:reserved-remote (if (= kind "HEADERS") :no-error (connection-error protocol-error)))
      (:half-closed-remote (stream-error stream-closed))
      (:closed (connection-error stream-closed))
      (_ :no-error)))

  (pure-def legal-transition ((a stream-state) (b stream-state)) "§5.1 state diagram, plus the implicit idle -> closed of §5.1.1."
    (:returns bool)
    (or (= a b)
        (match a
          (:idle t)
          (:reserved-local (in b (set :half-closed-remote :closed)))
          (:reserved-remote (in b (set :half-closed-local :closed)))
          (:open (in b (set :half-closed-local :half-closed-remote :closed)))
          (:half-closed-local (= b :closed))
          (:half-closed-remote (= b :closed))
          (_ nil))))

  (pure-def set-view ((m (map str (map int stream-state))) (p str) (id int) (s stream-state))
    "P's view of ID becomes S and the peer's view its mirror."
    (:returns (map str (map int stream-state)))
    (put (put m p (put (get m p) id s))
         (peer p) (put (get m (peer p)) id (mirror s))))

  (pure-def set-window ((w (map str (map int int))) (p str) (id int) (v int))
    (:returns (map str (map int int)))
    (put w p (put (get w p) id v)))

  (val live
    (match outcome
      ((connection-error c) nil)
      (_ t)))

  (def view ((p str) (id int)) (:returns stream-state)
    (get (get streams p) id))

  (def active-count ((q str) (opener str)) "Streams initiated by OPENER that are active in Q's view."
    (:returns int)
    (size (filter stream-ids (lambda (j) (and (= (initiator j) opener)
                                              (active (get (get streams q) j)))))))

  (def close-idle-below ((m (map str (map int stream-state))) (opener str) (id int))
    "§5.1.1: first use of ID implicitly closes OPENER's idle streams with lower identifiers."
    (:returns (map str (map int stream-state)))
    (map-of ((q peers))
      (map-of ((j stream-ids))
        (if (and (= (initiator j) opener) (< j id) (= (get (get m q) j) :idle))
            :closed
            (get (get m q) j)))))

  (def settings-overflow ((p str) (iws int)) "§6.5.2 / §6.9.2: the new initial window, or a window it adjusts, exceeds 2^31-1."
    (:returns bool)
    (or (> iws max-window)
        (exists stream-ids
                (lambda (j) (and (has-send-window (get (get streams (peer p)) j))
                                 (> (+ (get (get window (peer p)) j) (- iws (get initial-window-size p)))
                                    max-window))))))

  (def adjusted-windows ((p str) (delta int)) (:returns (map str (map int int)))
    (put window (peer p)
         (map-of ((j stream-ids))
           (if (has-send-window (get (get streams (peer p)) j))
               (+ (get (get window (peer p)) j) delta)
               (get (get window (peer p)) j)))))

  ;; Structural preconditions: a frame the model can express at all.

  (def headers-allowed ((p str) (id int)) (:returns bool)
    (if (= (view p id) :idle)
        (and (= p "client") (= (initiator id) "client")
             (>= id (get next-stream-id "client"))
             (not (get goaway-sent "server")))
        t))

  (def data-allowed ((p str) (id int) (n int) (es bool)) "§6.9.1: a sender never exceeds either window; an empty END_STREAM frame is always allowed."
    (:returns bool)
    (or (and (= n 0) es)
        (and (<= n (get (get window p) id)) (<= n (get conn-window p)))))

  (def push-allowed ((p str) (id int) (promised int)) (:returns bool)
    (and (sending (view p id))
         (or (= p "client") (= (initiator id) (peer p)))
         (= (view p promised) :idle)
         (= (initiator promised) p)
         (>= promised (get next-stream-id p))
         (not (get goaway-sent (peer p)))))

  ;; Sender discipline: what a compliant endpoint may send.

  (def may-send-headers ((p str) (id int)) "§5.1.2: endpoints MUST NOT exceed the limit set by their peer."
    (:returns bool)
    (let ((s (view p id)))
      (or (sending s)
          (and (in s (set :idle :reserved-local))
               (< (active-count p p) (get max-concurrent-streams (peer p)))))))

  (def may-send-data ((p str) (id int)) (:returns bool)
    (sending (view p id)))

  (def may-send-window-update ((p str) (id int) (n int)) "§6.9.1: a WINDOW_UPDATE never pushes the peer's window past 2^31-1."
    (:returns bool)
    (if (= id 0)
        (<= (+ (get conn-window (peer p)) n) max-window)
        (and (in (view p id) (set :open :half-closed-local :reserved-remote))
             (<= (+ (get (get window (peer p)) id) n) max-window))))

  (def may-send-rst ((p str) (id int)) (:returns bool)
    (not (in (view p id) (set :idle :closed))))

  (def may-push ((p str)) (:returns bool)
    (and (= p "server") push-enabled))

  (def may-send-settings ((p str) (iws int)) (:returns bool)
    (not (settings-overflow p iws)))

  (action init
    (setf streams (map-of ((q peers)) (map-of ((j stream-ids)) :idle))
          prev-streams (map-of ((q peers)) (map-of ((j stream-ids)) :idle))
          window (map-of ((q peers)) (map-of ((j stream-ids)) 0))
          conn-window (map-of ((q peers)) default-window)
          next-stream-id (map-lit ("client" 1) ("server" 2))
          initial-window-size (map-of ((q peers)) default-window)
          max-concurrent-streams (map-of ((q peers)) default-max-streams)
          goaway-sent (map-of ((q peers)) nil)
          outcome :no-error
          last-frame (rec (kind "") (sender "") (id 0))))

  (action headers ((p str) (id int) (es bool))
    (:guard live (headers-allowed p id))
    (setf prev-streams streams
          last-frame (rec (kind "HEADERS") (sender p) (id id)))
    (let ((s (view p id))
          (q (peer p)))
      (if (in s (set :idle :reserved-local))
          (if (>= (active-count q p) (get max-concurrent-streams q))
              (all (setf streams (set-view (close-idle-below streams p id) p id :closed)
                         outcome (stream-error refused-stream))
                   (if (= s :idle)
                       (setf next-stream-id (put next-stream-id p (+ id 2)))))
              (all (setf streams (set-view (close-idle-below streams p id) p id (after-headers s es))
                         outcome :no-error)
                   (if (= s :idle)
                       (setf next-stream-id (put next-stream-id p (+ id 2))
                             window (set-window (set-window window p id (get initial-window-size q))
                                                q id (get initial-window-size p))))))
          (let ((r (receive-reaction (view q id) "HEADERS")))
            (match r
              (:no-error (setf streams (set-view streams p id (after-headers s es))
                               outcome :no-error))
              ((stream-error c) (setf streams (set-view streams p id :closed)
                                      outcome r))
              (_ (setf outcome r)))))))

  (action data ((p str) (id int) (n int) (es bool))
    (:guard live (data-allowed p id n es))
    (setf prev-streams streams
          last-frame (rec (kind "DATA") (sender p) (id id)))
    (let ((r (receive-reaction (view (peer p) id) "DATA")))
      (match r
        (:no-error (setf streams (set-view streams p id (after-data (view p id) es))
                         window (set-window window p id (- (get (get window p) id) n))
                         conn-window (put conn-window p (- (get conn-window p) n))
                         outcome :no-error))
        ;; §6.9: a frame in error still counts against the connection window
        ((stream-error c) (setf streams (set-view streams p id :closed)
                                conn-window (put conn-window p (- (get conn-window p) n))
                                outcome r))
        (_ (setf outcome r)))))

  (action window-update ((p str) (id int) (n int))
    (:guard live (or (= id 0) (/= (view p id) :idle)))
    (setf prev-streams streams
          last-frame (rec (kind "WINDOW_UPDATE") (sender p) (id id)))
    (let ((q (peer p)))
      (if (= id 0)
          (if (> (+ (get conn-window q) n) max-window)
              (setf outcome (connection-error flow-control-error))
              (setf conn-window (put conn-window q (+ (get conn-window q) n))
                    outcome :no-error))
          (match (view q id)
            (:closed (setf outcome :no-error))
            (:reserved-remote (setf outcome (connection-error protocol-error)))
            (_ (if (> (+ (get (get window q) id) n) max-window)
                   (setf streams (set-view streams p id :closed)
                         outcome (stream-error flow-control-error))
                   (setf window (set-window window q id (+ (get (get window q) id) n))
                         outcome :no-error)))))))

  (action rst-stream ((p str) (id int))
    (:guard live (/= (view p id) :idle))
    (setf prev-streams streams
          last-frame (rec (kind "RST_STREAM") (sender p) (id id))
          streams (set-view streams p id :closed)
          outcome :no-error))

  (action push-promise ((p str) (id int) (promised int))
    (:guard live (push-allowed p id promised))
    (setf prev-streams streams
          last-frame (rec (kind "PUSH_PROMISE") (sender p) (id id)))
    (if (or (= p "client") (not push-enabled))
        (setf outcome (connection-error protocol-error))
        (setf streams (set-view (close-idle-below streams p promised) p promised :reserved-local)
              next-stream-id (put next-stream-id p (+ promised 2))
              window (set-window window p promised (get initial-window-size (peer p)))
              outcome :no-error)))

  (action goaway ((p str))
    (:guard live (not (get goaway-sent p)))
    (setf prev-streams streams
          last-frame (rec (kind "GOAWAY") (sender p) (id 0))
          goaway-sent (put goaway-sent p t)
          outcome :no-error))

  (action settings ((p str) (iws int) (mcs int))
    (:guard live)
    (setf prev-streams streams
          last-frame (rec (kind "SETTINGS") (sender p) (id 0)))
    (if (settings-overflow p iws)
        (setf outcome (connection-error flow-control-error))
        (setf window (adjusted-windows p (- iws (get initial-window-size p)))
              initial-window-size (put initial-window-size p iws)
              max-concurrent-streams (put max-concurrent-streams p mcs)
              outcome :no-error)))

  (invariant windows-bounded-6-9-1
    (forall peers (lambda (q) (and (<= (get conn-window q) max-window)
                                   (forall stream-ids (lambda (j) (<= (get (get window q) j) max-window)))))))

  (invariant stream-ids-monotonic-5-1-1
    (forall peers (lambda (q) (forall stream-ids
                                      (lambda (j) (iff (/= (get (get streams q) j) :idle)
                                                       (< j (get next-stream-id (initiator j)))))))))

  (invariant concurrency-limit-5-1-2
    (forall peers
            (lambda (q) (forall stream-ids
                                (lambda (j) (implies (and (= (initiator j) (peer q))
                                                          (not (active (get (get prev-streams q) j)))
                                                          (active (get (get streams q) j)))
                                                     (<= (active-count q (peer q))
                                                         (get max-concurrent-streams q))))))))

  (invariant legal-transitions-5-1
    (forall peers (lambda (q) (forall stream-ids
                                      (lambda (j) (legal-transition (get (get prev-streams q) j)
                                                                    (get (get streams q) j)))))))

  (invariant closed-is-terminal
    (forall peers (lambda (q) (forall stream-ids
                                      (lambda (j) (implies (= (get (get prev-streams q) j) :closed)
                                                           (= (get (get streams q) j) :closed)))))))

  (invariant data-only-when-open-6-1
    ;; OR, not IMPLIES: the stream lookup is only defined for DATA frames (id 0 is the connection)
    (or (/= (field last-frame kind) "DATA")
        (/= outcome :no-error)
        (receiving (get (get prev-streams (peer (field last-frame sender)))
                        (field last-frame id)))))

  (:spec-only
   (action step-with ((rogue bool))
     (any
      (nondet (p (one-of peers))
        (nondet (id (one-of (filter stream-ids (lambda (i) (and (headers-allowed p i)
                                                                (or rogue (may-send-headers p i)))))))
          (nondet (es (one-of (set t nil)))
            (headers p id es))))
      (nondet (p (one-of peers))
        (nondet (id (one-of (filter stream-ids (lambda (i) (or rogue (may-send-data p i))))))
          (nondet (es (one-of (set t nil)))
            (nondet (n (one-of (filter data-sizes (lambda (k) (data-allowed p id k es)))))
              (data p id n es)))))
      (nondet (p (one-of peers))
        (nondet (id (one-of (filter (union (set 0) stream-ids)
                                    (lambda (i) (or (= i 0) (/= (view p i) :idle))))))
          (nondet (n (one-of (filter increments (lambda (k) (or rogue (may-send-window-update p id k))))))
            (window-update p id n))))
      (nondet (p (one-of peers))
        (nondet (id (one-of (filter stream-ids (lambda (i) (and (/= (view p i) :idle)
                                                                (or rogue (may-send-rst p i)))))))
          (rst-stream p id)))
      (nondet (p (one-of (filter peers (lambda (x) (or rogue (may-push x))))))
        (nondet (id (one-of stream-ids))
          (nondet (promised (one-of (filter stream-ids (lambda (i) (push-allowed p id i)))))
            (push-promise p id promised))))
      (nondet (p (one-of peers))
        (goaway p))
      (nondet (p (one-of peers))
        (nondet (iws (one-of (filter (union window-settings (set (+ max-window 1)))
                                     (lambda (v) (or rogue (may-send-settings p v))))))
          (nondet (mcs (one-of stream-limits))
            (settings p iws mcs))))))
   (action step (step-with nil))
   (action rogue-step (step-with t))
   (run open-and-finish-test
     (expect (then (then (then init (headers "client" 1 nil)) (data "client" 1 2 t))
                   (headers "server" 1 t))
             (and (= (get (get streams "client") 1) :closed)
                  (= (get (get window "client") 1) 0)
                  (= (get conn-window "client") 0))))
   (run refused-above-limit-test
     (expect (then (then (then (then init (settings "server" 2 1)) (headers "client" 1 nil))
                         (headers "client" 3 nil))
                   (data "server" 1 1 nil))
             (and (= (get (get streams "server") 3) :closed)
                  (= (get (get streams "client") 1) :open))))))

(defparameter *invariants*
  '(windows-bounded-6-9-1 stream-ids-monotonic-5-1-1 concurrency-limit-5-1-2
    legal-transitions-5-1 closed-is-terminal data-only-when-open-6-1)
  "Every invariant of HTTP2-STREAM-STATE, named after its RFC 9113 section.")
