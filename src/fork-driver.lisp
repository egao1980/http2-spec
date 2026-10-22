(defpackage #:http2-spec.fork
  (:use #:cl)
  ;; White-box driver: it reaches into http2/core internals (stream lists, windows,
  ;; frame writers), so it names them with an explicit nickname instead of importing.
  (:local-nicknames (#:h2 #:http2/core)
                    (#:sp #:spec-protocol)
                    (#:mbt #:spec-protocol.mbt)
                    (#:itf #:spec-protocol.itf)
                    (#:model #:http2-spec))
  (:export #:fork-driver #:make-fork-driver #:fork-driver-notes
           #:driver-client #:driver-server))
(in-package #:http2-spec.fork)

;;; Replays HTTP2-STREAM-STATE traces against the http2 fork: an in-process client/server pair
;;; whose frames are moved by hand between write buffers, one model action at a time.
;;;
;;; - A frame a compliant endpoint may send (MAY-SEND-* on the pre-state) goes through the fork's
;;;   own sender API, so sender-side state changes are the fork's. Any other frame is built as raw
;;;   octets: the fork is only the receiver for misbehaving peers.
;;; - The receiver's reaction is the condition it signals, or else the RST_STREAM / GOAWAY it
;;;   writes back. Its replies are then delivered to the sender.
;;; - Observed: both stream views, send windows (only where the model has one), connection
;;;   windows, next stream ids, applied SETTINGS_INITIAL_WINDOW_SIZE and the outcome.
;;; - RFC-allowed alternatives to the model's reactions are accepted: PROTOCOL_ERROR for
;;;   REFUSED_STREAM (§5.1.2), a stream error for the connection error on closed streams (§5.1),
;;;   and any stream error escalated to a connection error (§5.4) — the trace is then aborted,
;;;   since the fork's connection is gone while the model's stream error is not fatal.

(defclass test-stream (h2::http2-stream) ()
  (:documentation "Stream that discards DATA (no automatic WINDOW_UPDATE): the trace sends those."))

(defmethod h2::apply-data-frame ((stream test-stream) payload start end)
  (declare (ignore payload start end)))

(defclass fork-client (h2::client-http2-connection h2::write-buffer-connection-mixin) ())
(defclass fork-server (h2::server-http2-connection h2::write-buffer-connection-mixin) ())

(defclass fork-driver (mbt:mbt-driver)
  ((spec :initarg :spec :reader fork-driver-spec
         :documentation "Spec whose instance constants the traces were generated with.")
   (consts :reader fork-driver-consts)
   (client :accessor driver-client)
   (server :accessor driver-server)
   (pre-state :accessor fork-driver-pre-state :initform nil
              :documentation "ITF state before the action being applied.")
   (expected-streams :accessor expected-streams :initform nil)
   (outcome :accessor fork-outcome :initform :no-error)
   (notes :accessor fork-driver-notes :initform '()
          :documentation "Accepted RFC alternatives, newest first: (step-action . text).")))

(defmethod initialize-instance :after ((d fork-driver) &key)
  (setf (slot-value d 'consts) (mbt:instance-consts (fork-driver-spec d))))

(defun make-fork-driver (&optional (spec 'model:http2-fork-spec))
  (make-instance 'fork-driver :spec spec))

(defun const (d name)
  (cdr (assoc name (fork-driver-consts d) :test #'sp:name=)))

(defun conn (d p) (if (string= p "client") (driver-client d) (driver-server d)))

;;; Moving octets

(defun take-output (connection)
  "The octets CONNECTION queued, as a simple octet vector; its buffer is emptied."
  (let* ((buffer (h2::get-to-write connection))
         (octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))
    (setf (fill-pointer buffer) 0)
    octets))

(defun frame-errors (octets)
  "Outcome named by the RST_STREAM / GOAWAY frames in OCTETS, or NIL."
  (loop with start = 0
        while (<= (+ start 9) (length octets))
        do (let ((len (+ (ash (aref octets start) 16) (ash (aref octets (+ start 1)) 8)
                         (aref octets (+ start 2))))
                 (type (aref octets (+ start 3))))
             (flet ((u32 (i) (+ (ash (aref octets i) 24) (ash (aref octets (+ i 1)) 16)
                                (ash (aref octets (+ i 2)) 8) (aref octets (+ i 3)))))
               (case type
                 (3 (return (cons :stream-error (u32 (+ start 9)))))
                 (7 (let ((code (u32 (+ start 13))))
                      (unless (zerop code) (return (cons :connection-error code))))))
               (incf start (+ 9 len))))))

(defun receive (connection octets)
  "Process OCTETS on CONNECTION. Returns the outcome it signalled, or NIL."
  (when (plusp (length octets))
    (handler-bind ((warning #'muffle-warning))
      (handler-case (progn (h2::process-frames connection octets) nil)
        (h2::go-away (c)
          (let ((code (h2::get-error-code c)))
            (unless (or (null code) (eql code 0) (eq code :no-error))
              (cons :connection-error code))))
        (h2::connection-error (c) (cons :connection-error (h2::get-code c)))
        (h2::http-stream-error (c) (cons :stream-error (h2::get-code c)))
        (error (c) (cons :lisp-error (format nil "~(~a~): ~a" (type-of c) c)))))))

(defun deliver (d sender)
  "Move SENDER's queued frames to its peer, then the replies back, until both are quiet.
Returns the receiver's outcome."
  (let* ((receiver (if (eq sender (driver-client d)) (driver-server d) (driver-client d)))
         (frames (take-output sender))
         (signalled (receive receiver frames))
         (replies (take-output receiver))
         (outcome (or signalled (frame-errors replies) :no-error)))
    (receive sender replies)
    (loop repeat 4
          for a = (take-output sender) and b = (take-output receiver)
          while (or (plusp (length a)) (plusp (length b)))
          do (receive receiver a) (receive sender b))
    outcome))

(defun raw-frame (d p type flags stream-id payload)
  "Queue a frame P's fork API would not send, as raw octets on P's connection."
  (let ((buffer (make-array (+ 9 (length payload)) :element-type '(unsigned-byte 8))))
    (h2::write-frame-header-to-vector buffer 0 (length payload) type flags stream-id nil)
    (replace buffer payload :start1 9)
    (h2::queue-frame (conn d p) buffer)))

(defun u32-octets (n)
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i below 4 do (setf (aref v i) (ldb (byte 8 (* 8 (- 3 i))) n)))
    v))

(defun header-block (d p)
  (let ((context (h2::get-compression-context (conn d p))))
    (prog1 (coerce (h2::compile-headers (header-block-headers p) context)
                   '(simple-array (unsigned-byte 8) (*)))
      (setf (h2::get-updates-needed context) nil))))

;;; Observation

(defun fork-stream (connection id)
  (find id (h2::get-streams connection) :key #'h2::get-stream-id))

(defparameter *states*
  '(("IDLE" . :idle) ("OPEN" . :open) ("CLOSED" . :closed)
    ("RESERVED/LOCAL" . :reserved-local) ("RESERVED/REMOTE" . :reserved-remote)
    ("HALF-CLOSED/LOCAL" . :half-closed-local) ("HALF-CLOSED/REMOTE" . :half-closed-remote)))

(defun stream-view (connection id)
  "The model's name for CONNECTION's state of stream ID. Streams the fork dropped are idle
above its id watermark and closed below (see HTTP2/CORE:CLOSE-HTTP2-STREAM)."
  (let ((s (fork-stream connection id)))
    (cond (s (cdr (assoc (symbol-name (h2::get-state s)) *states* :test #'string=)))
          ((h2::is-our-stream-id connection id)
           (if (>= id (h2::get-id-to-use connection)) :idle :closed))
          (t (if (> id (h2::get-last-id-seen connection)) :idle :closed)))))

(defun peer-map (fn)
  (sp:make-map (cons "client" (funcall fn "client")) (cons "server" (funcall fn "server"))))

(defun stream-map (d fn)
  (apply #'sp:make-map (loop for id in (sp:set-elements (const d 'stream-ids))
                             collect (cons id (funcall fn id)))))

(defmethod mbt:mbt-observe ((d fork-driver) sut)
  (declare (ignore sut))
  (flet ((peer-of (p) (conn d (model:peer p))))
    (list (cons "streams" (peer-map (lambda (p) (stream-map d (lambda (id) (stream-view (conn d p) id))))))
          (cons "window" (peer-map (lambda (p)
                                     (stream-map d (lambda (id)
                                                     (let ((s (fork-stream (conn d p) id)))
                                                       (and s (h2::get-peer-window-size s))))))))
          (cons "connWindow" (peer-map (lambda (p) (h2::get-peer-window-size (conn d p)))))
          (cons "nextStreamId" (peer-map (lambda (p) (h2::get-id-to-use (conn d p)))))
          (cons "initialWindowSize" (peer-map (lambda (p) (h2::get-initial-peer-window-size (peer-of p)))))
          (cons "outcome" (fork-outcome d)))))

(defun refused-alternative-p (expected actual)
  "§5.1.2: a refused stream may be reset with REFUSED_STREAM or PROTOCOL_ERROR."
  (and (equal expected (cons :stream-error model:+refused-stream+))
       (equal actual (cons :stream-error model:+protocol-error+))))

(defmethod mbt:mbt-compare ((d fork-driver) variable expected actual)
  (cond
    ((string= variable "window")
     ;; only windows of streams the sender can still send on are defined in the fork
     (loop for p in '("client" "server")
           always (loop for id in (sp:set-elements (const d 'stream-ids))
                        for state = (sp:map-get (sp:map-get (expected-streams d) p) id)
                        always (or (not (member state '(:open :half-closed-remote :reserved-local)))
                                   (eql (sp:map-get (sp:map-get expected p) id)
                                        (sp:map-get (sp:map-get actual p) id))))))
    ((string= variable "outcome")
     (or (sp:spec-equal expected actual) (refused-alternative-p expected actual)))
    (t (call-next-method))))

;;; Setup

(defmethod mbt:mbt-init ((d fork-driver) state)
  (let ((w (const d 'default-window)))
    (flet ((make (class)
             (make-instance class :stream-class 'test-stream
                                  :window-size w :peer-window-size w
                                  :initial-window-size w :initial-peer-window-size w
                                  :max-concurrent-streams (const d 'default-max-streams))))
      (setf (driver-client d) (make 'fork-client)
            (driver-server d) (make 'fork-server)
            (fork-outcome d) :no-error
            (expected-streams d) (itf:state-value state "streams")
            (fork-driver-notes d) '()
            (fork-driver-pre-state d) state)))
  ;; connection preface: the client's magic octets, then both SETTINGS and their ACKs
  (let* ((octets (take-output (driver-client d)))
         (preface (length h2::+client-preface-start+)))
    (h2::parse-client-preface (driver-server d) (subseq octets 0 preface))
    (receive (driver-server d) (subseq octets preface)))
  (deliver d (driver-server d))
  (deliver d (driver-client d))
  d)

;;; Actions

(defun pick (picks name) (cdr (assoc name picks)))

(defun shadow-model (d)
  "A CLOS HTTP2-STREAM-STATE holding the pre-state, to ask the MAY-SEND-* discipline."
  (mbt:mbt-init (mbt:make-clos-driver 'model:http2-stream-state :consts (fork-driver-consts d))
                (fork-driver-pre-state d)))

(defun sendable-stream (d p id states)
  (let ((s (fork-stream (conn d p) id)))
    (and s (member (symbol-name (h2::get-state s)) states :test #'string=) s)))

(defun send-headers (d m p id es)
  (let ((c (conn d p)))
    (cond ((eq (model:view m p id) :idle)
           (setf (h2::get-id-to-use c) id)
           (h2::send-headers (h2::create-new-local-stream c) (header-block-headers p) :end-stream es))
          ((and (model:may-send-headers m p id)
                (sendable-stream d p id '("OPEN" "HALF-CLOSED/REMOTE" "RESERVED/LOCAL")))
           (h2::send-headers (fork-stream c id) (header-block-headers p) :end-stream es))
          (t (raw-frame d p 1 (logior 4 (if es 1 0)) id (header-block d p))))))

(defun header-block-headers (p)
  (if (string= p "client")
      '((:method "GET") (:scheme "https") (:path "/") (:authority "http2-spec"))
      '((:status "200"))))

(defun send-data (d m p id n es)
  "Send DATA. Returns the octets a raw frame still owes the sender's connection window."
  (let ((s (and (model:may-send-data m p id)
                (sendable-stream d p id '("OPEN" "HALF-CLOSED/REMOTE")))))
    (if s
        (progn (h2::write-data-frame s (make-array n :element-type '(unsigned-byte 8)) :end-stream es)
               0)
        (progn (raw-frame d p 0 (if es 1 0) id (make-array n :element-type '(unsigned-byte 8)))
               n))))

(defun send-window-update (d m p id n)
  (let ((target (cond ((not (model:may-send-window-update m p id n)) nil)
                      ((= id 0) (conn d p))
                      (t (fork-stream (conn d p) id)))))
    (if target
        (h2::write-window-update-frame target n)
        (raw-frame d p 8 0 id (u32-octets n)))))

(defun send-rst (d p id)
  (let ((s (fork-stream (conn d p) id)))
    (if s
        (progn (h2::write-rst-stream-frame s h2::+cancel+)
               ;; RST_STREAM closes the stream for its sender too (§5.1)
               (h2::close-http2-stream s 'reset))
        (raw-frame d p 3 0 id (u32-octets h2::+cancel+)))))

(defun send-push (d p id promised)
  (raw-frame d p 5 4 id (concatenate '(simple-array (unsigned-byte 8) (*))
                                     (u32-octets promised) (header-block d p))))

(defun send-settings (d p iws mcs)
  (let ((c (conn d p)))
    (when (<= iws (const d 'max-window))
      (setf (h2::get-initial-window-size c) iws))
    (setf (h2::get-max-concurrent-streams c) mcs)
    (h2::write-settings-frame c `((:initial-window-size . ,iws) (:max-concurrent-streams . ,mcs)))))

(defun escalation-p (expected actual)
  "§5.4: any stream error may be treated as a connection error (§5.1: and vice versa on closed
streams, where the model picked the connection error)."
  (or (and (consp expected) (eq (car expected) :stream-error)
           (consp actual) (eq (car actual) :connection-error)
           (or (eql (cdr expected) (cdr actual)) (refused-alternative-p expected (cons :stream-error (cdr actual)))))
      (and (equal expected (cons :connection-error model:+stream-closed+))
           (equal actual (cons :stream-error model:+stream-closed+)))))

(defmethod mbt:mbt-apply ((d fork-driver) sut action picks state)
  (declare (ignore sut))
  (let ((m (shadow-model d))
        (p (pick picks :p))
        (raw-data 0))
    (setf (expected-streams d) (itf:state-value state "streams"))
    (cond
      ((string= action "headers") (send-headers d m p (pick picks :id) (pick picks :es)))
      ((string= action "data")
       (setf raw-data (send-data d m p (pick picks :id) (pick picks :n) (pick picks :es))))
      ((string= action "windowUpdate") (send-window-update d m p (pick picks :id) (pick picks :n)))
      ((string= action "rstStream") (send-rst d p (pick picks :id)))
      ((string= action "pushPromise") (send-push d p (pick picks :id) (pick picks :promised)))
      ((string= action "goaway") (h2::write-goaway-frame (conn d p) 0 h2::+no-error+ #()))
      ((string= action "settings") (send-settings d p (pick picks :iws) (pick picks :mcs)))
      (t (error 'sp:spec-error :message (format nil "fork driver: unmapped action ~a" action))))
    (setf (fork-outcome d) (deliver d (conn d p))
          (fork-driver-pre-state d) state)
    ;; a sender accounts raw DATA against its connection window like the fork's API does
    ;; (§6.9: also when the frame is in error), unless the connection itself failed
    (unless (and (consp (fork-outcome d)) (eq (car (fork-outcome d)) :connection-error))
      (decf (h2::get-peer-window-size (conn d p)) raw-data))
    (let ((expected (itf:state-value state "outcome")))
      (when (escalation-p expected (fork-outcome d))
        (push (cons (format nil "~d ~a" (itf:state-index state) action)
                    (format nil "expected ~s, fork ~s" expected (fork-outcome d)))
              (fork-driver-notes d))
        (invoke-restart 'sp:abort-trace)))
    d))
