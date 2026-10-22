# http2-spec

RFC 9113 (HTTP/2) stream states, stream identifiers, concurrency and flow control as one
[`spec-protocol`](https://github.com/egao1980/spec-protocol) `defspec`: a CLOS protocol and a
Quint model from the same form, plus a model-based-testing driver that replays Quint traces
against the [`egao1980/http2`](https://github.com/egao1980/http2) fork.

| System | Contents |
|---|---|
| `http2-spec` | `http2-stream-state` defspec, the fork-shaped variant `http2-fork-spec` (push disabled) and the deliberately wrong `http2-buggy-concurrency-spec` |
| `http2-spec/fork` | `http2-spec.fork:fork-driver`, a `spec-protocol.mbt:mbt-driver` over an in-process `http2/core` client/server pair |
| `http2-spec/tests` | Rove suite: RFC scenarios on CLOS, own-trace conformance, Quint typecheck/simulate/verify, fork replay |

## Model

- **State**: per-peer stream views (`idle`, `reserved-*`, `open`, `half-closed-*`, `closed`),
  stream and connection send windows, next stream id per peer, `initialWindowSize`,
  `maxConcurrentStreams`, `goawaySent`, the last frame and its outcome
  (`:no-error`, `(:stream-error . code)`, `(:connection-error . code)`).
- **Actions**: `headers` (± END_STREAM), `data` (± END_STREAM), `windowUpdate` (stream 0 is the
  connection), `rstStream`, `pushPromise`, `goaway`, `settings` (with the §6.9.2 window delta).
  `step` only lets each endpoint send what a compliant sender may (`may-send-*`); `rogueStep`
  also sends frames a misbehaving peer would, so the receiver's reactions are exercised.
- **Invariants**: `windowsBounded_6_9_1`, `streamIdsMonotonic_5_1_1`, `concurrencyLimit_5_1_2`,
  `legalTransitions_5_1`, `closedIsTerminal`, `dataOnlyWhenOpen_6_1`.
- Finite domains: stream ids 1–5, default windows of 2 octets, DATA of 0–2 octets, increments
  {1, 2, 2^31-1} and initial-window settings {0–3, 2^31} so the overflow edges are reachable,
  stream limits 0–2.
  `http2-buggy-concurrency-spec` admits streams above the limit; Quint finds the
  `concurrencyLimit_5_1_2` violation and the correct CLOS protocol diverges from its trace.

## Fork driver

```lisp
(asdf:load-system "http2-spec/fork")
(spec-protocol.mbt:replay-trace (http2-spec.fork:make-fork-driver)
                                (spec-protocol.itf:read-itf #p"trace.itf.json"))
```

Frames a compliant sender may send go through the fork's API; anything else is injected as raw
octets, so the fork is only ever the receiver for misbehaviour. The receiver's reaction is the
condition it signals or the RST_STREAM / GOAWAY it writes. RFC-allowed alternatives are accepted
and recorded in `fork-driver-notes`: PROTOCOL_ERROR for REFUSED_STREAM, a stream error for the
connection error on closed streams, and a stream error escalated to a connection error (§5.4,
the replay is then aborted). Every other mismatch is an implementation bug: the model is not bent.

### Divergences found (http2 2.1.1, fixed in 2.1.2)

| Trace (`tests/fixtures/`) | RFC 9113 | Fork behaviour |
|---|---|---|
| `regression-settings-payload-offset` | §6.5 | SETTINGS reader asserted its payload starts at offset 0 |
| `regression-initial-window-adjust-6-9-2` | §6.9.2 | SETTINGS_INITIAL_WINDOW_SIZE did not adjust open streams |
| `regression-stream-window-overflow-6-9-1` | §6.9.1 | stream WINDOW_UPDATE past 2^31-1 accepted |
| `regression-connection-window-overflow-6-9-1` | §6.9.1 | connection WINDOW_UPDATE past 2^31-1 accepted |
| `regression-push-promise-refused-8-4` | §8.4 | PUSH_PROMISE raised a Lisp error, not PROTOCOL_ERROR |
| `regression-initial-window-too-large-6-5-2` | §6.5.2 | PROTOCOL_ERROR instead of FLOW_CONTROL_ERROR |
| `regression-max-concurrent-streams-5-1-2` | §5.1.2 | SETTINGS_MAX_CONCURRENT_STREAMS not enforced; once enforced, core called `http2/server`'s logger |

## Why a separate repository

The fork tracks [zellerin/http2](https://github.com/zellerin/http2) and stays rebasable: its
tests are fiasco, it ships through a `cl-stack-systems` import, and it should not depend on
`spec-protocol` / `spec-backend-quint` / Rove. The fork is fully drivable from outside
(`write-buffer-connection-mixin`, `process-frames`, frame writers, observable stream and window
state), so the spec, driver and traces live here and are reusable against other HTTP/2
implementations.

## Tests

```sh
CL_REPO_SYSTEM=http2-spec ros -l ../.lisp-workspace/scripts/oci-test-system.lisp
```

Quint tests skip without `QUINT_BIN` / `quint` on PATH; Apalache verification also needs
`JAVA_HOME` (JDK ≥ 17). Frozen fixtures are rewritten by `(http2-spec/tests::freeze-fixtures)`,
which keeps the pinned `regression-*` traces.
