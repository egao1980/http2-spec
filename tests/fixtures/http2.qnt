module Http2 {
  const streamIds: Set[int]
  const maxWindow: int
  const defaultWindow: int
  const defaultMaxStreams: int
  const dataSizes: Set[int]
  const increments: Set[int]
  const windowSettings: Set[int]
  const streamLimits: Set[int]
  const pushEnabled: bool
  type StreamState = Idle | ReservedLocal | ReservedRemote | Open | HalfClosedLocal | HalfClosedRemote | Closed
  type Outcome = NoError | StreamError(int) | ConnectionError(int)
  type Frame = { kind: str, sender: str, id: int }
  var streams: (str -> (int -> StreamState))
  var window: (str -> (int -> int))
  var connWindow: (str -> int)
  var nextStreamId: (str -> int)
  var initialWindowSize: (str -> int)
  var maxConcurrentStreams: (str -> int)
  var goawaySent: (str -> bool)
  var outcome: Outcome
  var lastFrame: Frame
  var prevStreams: (str -> (int -> StreamState))
  pure val peers = Set("client", "server")
  pure val protocolError = 1
  pure val flowControlError = 3
  pure val streamClosed = 5
  pure val refusedStream = 7
  pure def peer(p: str): str = (if ((p == "client")) "server" else "client")
  /// §5.1.1: clients initiate odd-numbered streams, servers even-numbered.
  pure def initiator(id: int): str = (if (((id % 2) == 1)) "client" else "server")
  /// The peer's name for the same stream state.
  pure def mirror(s: StreamState): StreamState = match s {
    | ReservedLocal => ReservedRemote
    | ReservedRemote => ReservedLocal
    | HalfClosedLocal => HalfClosedRemote
    | HalfClosedRemote => HalfClosedLocal
    | _ => s
  }
  /// §5.1.2: streams counted against SETTINGS_MAX_CONCURRENT_STREAMS.
  pure def active(s: StreamState): bool = s.in(Set(Open, HalfClosedLocal, HalfClosedRemote))
  /// The endpoint may still send DATA on the stream.
  pure def sending(s: StreamState): bool = s.in(Set(Open, HalfClosedRemote))
  /// §6.1: DATA is accepted only in open and half-closed (local).
  pure def receiving(s: StreamState): bool = s.in(Set(Open, HalfClosedLocal))
  /// §6.9.2: windows a SETTINGS_INITIAL_WINDOW_SIZE change adjusts (active and not yet active).
  pure def hasSendWindow(s: StreamState): bool = s.in(Set(Open, HalfClosedRemote, ReservedLocal))
  pure def afterHeaders(s: StreamState, es: bool): StreamState = match s {
    | Idle => (if (es) HalfClosedLocal else Open)
    | ReservedLocal => (if (es) Closed else HalfClosedRemote)
    | Open => (if (es) HalfClosedLocal else Open)
    | HalfClosedRemote => (if (es) Closed else HalfClosedRemote)
    | _ => s
  }
  pure def afterData(s: StreamState, es: bool): StreamState = (if (es) match s {
    | Open => HalfClosedLocal
    | HalfClosedRemote => Closed
    | _ => s
  } else s)
  /// §5.1: the receiver's reaction to HEADERS or DATA on a stream it holds in state S.
  pure def receiveReaction(s: StreamState, kind: str): Outcome = match s {
    | Idle => ConnectionError(protocolError)
    | ReservedLocal => ConnectionError(protocolError)
    | ReservedRemote => (if ((kind == "HEADERS")) NoError else ConnectionError(protocolError))
    | HalfClosedRemote => StreamError(streamClosed)
    | Closed => ConnectionError(streamClosed)
    | _ => NoError
  }
  /// §5.1 state diagram, plus the implicit idle -> closed of §5.1.1.
  pure def legalTransition(a: StreamState, b: StreamState): bool = ((a == b) or match a {
    | Idle => true
    | ReservedLocal => b.in(Set(HalfClosedRemote, Closed))
    | ReservedRemote => b.in(Set(HalfClosedLocal, Closed))
    | Open => b.in(Set(HalfClosedLocal, HalfClosedRemote, Closed))
    | HalfClosedLocal => (b == Closed)
    | HalfClosedRemote => (b == Closed)
    | _ => false
  })
  /// P's view of ID becomes S and the peer's view its mirror.
  pure def setView(m: (str -> (int -> StreamState)), p: str, id: int, s: StreamState): (str -> (int -> StreamState)) = m.set(p, m.get(p).set(id, s)).set(peer(p), m.get(peer(p)).set(id, mirror(s)))
  pure def setWindow(w: (str -> (int -> int)), p: str, id: int, v: int): (str -> (int -> int)) = w.set(p, w.get(p).set(id, v))
  val live = match outcome {
    | ConnectionError(c) => false
    | _ => true
  }
  def view(p: str, id: int): StreamState = streams.get(p).get(id)
  /// Streams initiated by OPENER that are active in Q's view.
  def activeCount(q: str, opener: str): int = streamIds.filter((j => ((initiator(j) == opener) and active(streams.get(q).get(j))))).size()
  /// §5.1.1: first use of ID implicitly closes OPENER's idle streams with lower identifiers.
  def closeIdleBelow(m: (str -> (int -> StreamState)), opener: str, id: int): (str -> (int -> StreamState)) = peers.mapBy((q => streamIds.mapBy((j => (if (((initiator(j) == opener) and (j < id) and (m.get(q).get(j) == Idle))) Closed else m.get(q).get(j))))))
  /// §6.5.2 / §6.9.2: the new initial window, or a window it adjusts, exceeds 2^31-1.
  def settingsOverflow(p: str, iws: int): bool = ((iws > maxWindow) or streamIds.exists((j => (hasSendWindow(streams.get(peer(p)).get(j)) and ((window.get(peer(p)).get(j) + (iws - initialWindowSize.get(p))) > maxWindow)))))
  def adjustedWindows(p: str, delta: int): (str -> (int -> int)) = window.set(peer(p), streamIds.mapBy((j => (if (hasSendWindow(streams.get(peer(p)).get(j))) (window.get(peer(p)).get(j) + delta) else window.get(peer(p)).get(j)))))
  def headersAllowed(p: str, id: int): bool = (if ((view(p, id) == Idle)) ((p == "client") and (initiator(id) == "client") and (id >= nextStreamId.get("client")) and not(goawaySent.get("server"))) else true)
  /// §6.9.1: a sender never exceeds either window; an empty END_STREAM frame is always allowed.
  def dataAllowed(p: str, id: int, n: int, es: bool): bool = (((n == 0) and es) or ((n <= window.get(p).get(id)) and (n <= connWindow.get(p))))
  def pushAllowed(p: str, id: int, promised: int): bool = (sending(view(p, id)) and ((p == "client") or (initiator(id) == peer(p))) and (view(p, promised) == Idle) and (initiator(promised) == p) and (promised >= nextStreamId.get(p)) and not(goawaySent.get(peer(p))))
  /// §5.1.2: endpoints MUST NOT exceed the limit set by their peer.
  def maySendHeaders(p: str, id: int): bool = {
    val s = view(p, id)
    (sending(s) or (s.in(Set(Idle, ReservedLocal)) and (activeCount(p, p) < maxConcurrentStreams.get(peer(p)))))
  }
  def maySendData(p: str, id: int): bool = sending(view(p, id))
  /// §6.9.1: a WINDOW_UPDATE never pushes the peer's window past 2^31-1.
  def maySendWindowUpdate(p: str, id: int, n: int): bool = (if ((id == 0)) ((connWindow.get(peer(p)) + n) <= maxWindow) else (view(p, id).in(Set(Open, HalfClosedLocal, ReservedRemote)) and ((window.get(peer(p)).get(id) + n) <= maxWindow)))
  def maySendRst(p: str, id: int): bool = not(view(p, id).in(Set(Idle, Closed)))
  def mayPush(p: str): bool = ((p == "server") and pushEnabled)
  def maySendSettings(p: str, iws: int): bool = not(settingsOverflow(p, iws))
  action init = all {
    streams' = peers.mapBy((q => streamIds.mapBy((j => Idle)))),
    prevStreams' = peers.mapBy((q => streamIds.mapBy((j => Idle)))),
    window' = peers.mapBy((q => streamIds.mapBy((j => 0)))),
    connWindow' = peers.mapBy((q => defaultWindow)),
    nextStreamId' = Map("client" -> 1, "server" -> 2),
    initialWindowSize' = peers.mapBy((q => defaultWindow)),
    maxConcurrentStreams' = peers.mapBy((q => defaultMaxStreams)),
    goawaySent' = peers.mapBy((q => false)),
    outcome' = NoError,
    lastFrame' = { kind: "", sender: "", id: 0 },
  }
  action headers(p: str, id: int, es: bool): bool = all {
    live,
    headersAllowed(p, id),
    all {
      prevStreams' = streams,
      lastFrame' = { kind: "HEADERS", sender: p, id: id },
    },
    {
      val s = view(p, id)
      val q = peer(p)
      if (s.in(Set(Idle, ReservedLocal))) if ((activeCount(q, p) >= maxConcurrentStreams.get(q))) all {
        all {
          streams' = setView(closeIdleBelow(streams, p, id), p, id, Closed),
          outcome' = StreamError(refusedStream),
        },
        if ((s == Idle)) nextStreamId' = nextStreamId.set(p, (id + 2)) else all {
          nextStreamId' = nextStreamId,
        },
        window' = window,
      } else all {
        all {
          streams' = setView(closeIdleBelow(streams, p, id), p, id, afterHeaders(s, es)),
          outcome' = NoError,
        },
        if ((s == Idle)) all {
          nextStreamId' = nextStreamId.set(p, (id + 2)),
          window' = setWindow(setWindow(window, p, id, initialWindowSize.get(q)), q, id, initialWindowSize.get(p)),
        } else all {
          window' = window,
          nextStreamId' = nextStreamId,
        },
      } else all {
        {
          val r = receiveReaction(view(q, id), "HEADERS")
          match r {
            | NoError => all {
              streams' = setView(streams, p, id, afterHeaders(s, es)),
              outcome' = NoError,
            }
            | StreamError(c) => all {
              streams' = setView(streams, p, id, Closed),
              outcome' = r,
            }
            | _ => all {
              outcome' = r,
              streams' = streams,
            }
          }
        },
        window' = window,
        nextStreamId' = nextStreamId,
      }
    },
    connWindow' = connWindow,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
    goawaySent' = goawaySent,
  }
  action data(p: str, id: int, n: int, es: bool): bool = all {
    live,
    dataAllowed(p, id, n, es),
    all {
      prevStreams' = streams,
      lastFrame' = { kind: "DATA", sender: p, id: id },
    },
    {
      val r = receiveReaction(view(peer(p), id), "DATA")
      match r {
        | NoError => all {
          streams' = setView(streams, p, id, afterData(view(p, id), es)),
          window' = setWindow(window, p, id, (window.get(p).get(id) - n)),
          connWindow' = connWindow.set(p, (connWindow.get(p) - n)),
          outcome' = NoError,
        }
        | StreamError(c) => all {
          streams' = setView(streams, p, id, Closed),
          connWindow' = connWindow.set(p, (connWindow.get(p) - n)),
          outcome' = r,
          window' = window,
        }
        | _ => all {
          outcome' = r,
          window' = window,
          streams' = streams,
          connWindow' = connWindow,
        }
      }
    },
    nextStreamId' = nextStreamId,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
    goawaySent' = goawaySent,
  }
  action windowUpdate(p: str, id: int, n: int): bool = all {
    live,
    ((id == 0) or (view(p, id) != Idle)),
    all {
      prevStreams' = streams,
      lastFrame' = { kind: "WINDOW_UPDATE", sender: p, id: id },
    },
    {
      val q = peer(p)
      if ((id == 0)) all {
        if (((connWindow.get(q) + n) > maxWindow)) all {
          outcome' = ConnectionError(flowControlError),
          connWindow' = connWindow,
        } else all {
          connWindow' = connWindow.set(q, (connWindow.get(q) + n)),
          outcome' = NoError,
        },
        streams' = streams,
        window' = window,
      } else all {
        match view(q, id) {
          | Closed => all {
            outcome' = NoError,
            window' = window,
            streams' = streams,
          }
          | ReservedRemote => all {
            outcome' = ConnectionError(protocolError),
            window' = window,
            streams' = streams,
          }
          | _ => if (((window.get(q).get(id) + n) > maxWindow)) all {
            streams' = setView(streams, p, id, Closed),
            outcome' = StreamError(flowControlError),
            window' = window,
          } else all {
            window' = setWindow(window, q, id, (window.get(q).get(id) + n)),
            outcome' = NoError,
            streams' = streams,
          }
        },
        connWindow' = connWindow,
      }
    },
    nextStreamId' = nextStreamId,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
    goawaySent' = goawaySent,
  }
  action rstStream(p: str, id: int): bool = all {
    live,
    (view(p, id) != Idle),
    prevStreams' = streams,
    lastFrame' = { kind: "RST_STREAM", sender: p, id: id },
    streams' = setView(streams, p, id, Closed),
    outcome' = NoError,
    window' = window,
    connWindow' = connWindow,
    nextStreamId' = nextStreamId,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
    goawaySent' = goawaySent,
  }
  action pushPromise(p: str, id: int, promised: int): bool = all {
    live,
    pushAllowed(p, id, promised),
    all {
      prevStreams' = streams,
      lastFrame' = { kind: "PUSH_PROMISE", sender: p, id: id },
    },
    if (((p == "client") or not(pushEnabled))) all {
      outcome' = ConnectionError(protocolError),
      window' = window,
      nextStreamId' = nextStreamId,
      streams' = streams,
    } else all {
      streams' = setView(closeIdleBelow(streams, p, promised), p, promised, ReservedLocal),
      nextStreamId' = nextStreamId.set(p, (promised + 2)),
      window' = setWindow(window, p, promised, initialWindowSize.get(peer(p))),
      outcome' = NoError,
    },
    connWindow' = connWindow,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
    goawaySent' = goawaySent,
  }
  action goaway(p: str): bool = all {
    live,
    not(goawaySent.get(p)),
    prevStreams' = streams,
    lastFrame' = { kind: "GOAWAY", sender: p, id: 0 },
    goawaySent' = goawaySent.set(p, true),
    outcome' = NoError,
    streams' = streams,
    window' = window,
    connWindow' = connWindow,
    nextStreamId' = nextStreamId,
    initialWindowSize' = initialWindowSize,
    maxConcurrentStreams' = maxConcurrentStreams,
  }
  action settings(p: str, iws: int, mcs: int): bool = all {
    live,
    all {
      prevStreams' = streams,
      lastFrame' = { kind: "SETTINGS", sender: p, id: 0 },
    },
    if (settingsOverflow(p, iws)) all {
      outcome' = ConnectionError(flowControlError),
      maxConcurrentStreams' = maxConcurrentStreams,
      initialWindowSize' = initialWindowSize,
      window' = window,
    } else all {
      window' = adjustedWindows(p, (iws - initialWindowSize.get(p))),
      initialWindowSize' = initialWindowSize.set(p, iws),
      maxConcurrentStreams' = maxConcurrentStreams.set(p, mcs),
      outcome' = NoError,
    },
    streams' = streams,
    connWindow' = connWindow,
    nextStreamId' = nextStreamId,
    goawaySent' = goawaySent,
  }
  val windowsBounded_6_9_1 = peers.forall((q => ((connWindow.get(q) <= maxWindow) and streamIds.forall((j => (window.get(q).get(j) <= maxWindow))))))
  val streamIdsMonotonic_5_1_1 = peers.forall((q => streamIds.forall((j => (streams.get(q).get(j) != Idle).iff((j < nextStreamId.get(initiator(j))))))))
  val concurrencyLimit_5_1_2 = peers.forall((q => streamIds.forall((j => ((initiator(j) == peer(q)) and not(active(prevStreams.get(q).get(j))) and active(streams.get(q).get(j))).implies((activeCount(q, peer(q)) <= maxConcurrentStreams.get(q)))))))
  val legalTransitions_5_1 = peers.forall((q => streamIds.forall((j => legalTransition(prevStreams.get(q).get(j), streams.get(q).get(j))))))
  val closedIsTerminal = peers.forall((q => streamIds.forall((j => (prevStreams.get(q).get(j) == Closed).implies((streams.get(q).get(j) == Closed))))))
  val dataOnlyWhenOpen_6_1 = ((lastFrame.kind != "DATA") or (outcome != NoError) or receiving(prevStreams.get(peer(lastFrame.sender)).get(lastFrame.id)))
  action stepWith(rogue: bool): bool = any {
    {
      nondet p = peers.oneOf()
      nondet id = streamIds.filter((i => (headersAllowed(p, i) and (rogue or maySendHeaders(p, i))))).oneOf()
      nondet es = Set(true, false).oneOf()
      headers(p, id, es)
    },
    {
      nondet p = peers.oneOf()
      nondet id = streamIds.filter((i => (rogue or maySendData(p, i)))).oneOf()
      nondet es = Set(true, false).oneOf()
      nondet n = dataSizes.filter((k => dataAllowed(p, id, k, es))).oneOf()
      data(p, id, n, es)
    },
    {
      nondet p = peers.oneOf()
      nondet id = Set(0).union(streamIds).filter((i => ((i == 0) or (view(p, i) != Idle)))).oneOf()
      nondet n = increments.filter((k => (rogue or maySendWindowUpdate(p, id, k)))).oneOf()
      windowUpdate(p, id, n)
    },
    {
      nondet p = peers.oneOf()
      nondet id = streamIds.filter((i => ((view(p, i) != Idle) and (rogue or maySendRst(p, i))))).oneOf()
      rstStream(p, id)
    },
    {
      nondet p = peers.filter((x => (rogue or mayPush(x)))).oneOf()
      nondet id = streamIds.oneOf()
      nondet promised = streamIds.filter((i => pushAllowed(p, id, i))).oneOf()
      pushPromise(p, id, promised)
    },
    {
      nondet p = peers.oneOf()
      goaway(p)
    },
    {
      nondet p = peers.oneOf()
      nondet iws = windowSettings.union(Set((maxWindow + 1))).filter((v => (rogue or maySendSettings(p, v)))).oneOf()
      nondet mcs = streamLimits.oneOf()
      settings(p, iws, mcs)
    },
  }
  action step = stepWith(false)
  action rogueStep = stepWith(true)
}

module Http2Test {
  import Http2(streamIds = Set(1, 2, 3, 4, 5), maxWindow = 2147483647, defaultWindow = 2, defaultMaxStreams = 2, dataSizes = Set(0, 1, 2), increments = Set(1, 2, 2147483647), windowSettings = Set(0, 1, 2, 3), streamLimits = Set(0, 1, 2), pushEnabled = true).*
  run openAndFinishTest = init.then(headers("client", 1, false)).then(data("client", 1, 2, true)).then(headers("server", 1, true)).expect(((streams.get("client").get(1) == Closed) and (window.get("client").get(1) == 0) and (connWindow.get("client") == 0)))
  run refusedAboveLimitTest = init.then(settings("server", 2, 1)).then(headers("client", 1, false)).then(headers("client", 3, false)).then(data("server", 1, 1, false)).expect(((streams.get("server").get(3) == Closed) and (streams.get("client").get(1) == Open)))
}
