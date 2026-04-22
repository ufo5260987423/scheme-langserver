## scheme-langserver log replay debugging tools

`tests/log-debug.sps` and `tests/parallel-log-debug.sps` are **log replay utilities**. They read a previously captured LSP session log and feed it back into a fresh server instance, allowing you to reproduce crashes or misbehavior offline without a real language client.

---

### 1. Log file format

Both scripts expect a plain-text log at `~/ready-for-analyse.log`. The log is produced by the server's own debug logging and follows a simple line-oriented convention:

- **`read-message`** — marks the start of an incoming LSP request/notification. The next line(s) contain the actual JSON-RPC payload.
- **`send-message`** *(parallel mode only)* — marks the end of a multi-line batch payload. All lines between the preceding `read-message` and this marker are concatenated (with `\n`) into a single message.
- Everything else is ignored.

Because JSON-RPC messages arrive with a `Content-Length` header in real I/O, the scripts reconstruct that header automatically:

```
Content-Length: <byte-length>\r\n\r\n<json-payload>
```

---

### 2. `tests/log-debug.sps` — single-threaded replay

| Aspect | Behaviour |
|--------|-----------|
| Mode | **Single-threaded** (`enable-multi-thread? #f`) |
| Batch handling | No `send-message` support; each `read-message` is followed by exactly one payload line |
| Server args | `(init-server input-port output-port log-port #f #t 'r6rs)` |
| Expected result | Server **does not** reach shutdown state (`server-shutdown?` → `#f`) |

Use this when you want a deterministic, linear replay without engine preemption or request-queue concurrency.

---

### 3. `tests/parallel-log-debug.sps` — multi-threaded replay

| Aspect | Behaviour |
|--------|-----------|
| Mode | **Multi-threaded** (`enable-multi-thread? #t`) |
| Batch handling | Supports multi-line payloads delimited by `send-message` |
| Server args | `(init-server input-port output-port log-port #t #t 'r6rs #t)` |
| Expected result | Server **does** reach shutdown state (`server-shutdown?` → `#t`) |

This version runs the full threaded pipeline: thread-pool, request-queue, interval timer for diagnostic publishing, and engine-based time slicing. Use it to reproduce race conditions or cancellation bugs that only appear in multi-threaded mode.

---

### 4. Typical workflow

1. Capture a failing session from a real editor client. The server writes debug logs to `~/scheme-langserver.log` when debug mode is enabled.
2. Copy or symlink the log to `~/ready-for-analyse.log`.
3. Run the appropriate replay script:
   ```bash
   source .akku/bin/activate
   scheme --script tests/log-debug.sps          # single-threaded
   scheme --script tests/parallel-log-debug.sps # multi-threaded
   ```
4. The script creates `~/scheme-langserver.log` (server output log) and `~/scheme-langserver.out` (raw server stdout) for inspection.

---

### 5. Differences at a glance

| | `log-debug.sps` | `parallel-log-debug.sps` |
|---|---|---|
| Threading | `#f` | `#t` |
| Debug mode | `#t` (6th arg) | `#t` (7th arg) |
| Batch (`send-message`) | ❌ | ✅ |
| Shutdown assertion | `#f` | `#t` |

---

### 6. Notes

- Both scripts are **not** run by `test.sh` automatically. They are standalone diagnostic tools that require a manually prepared `~/ready-for-analyse.log`.
- The `Content-Length` header is reconstructed in-memory; the original log does **not** need to contain it.
- If the log ends with a `shutdown` request, the parallel version will observe `server-shutdown?` becoming `#t`, whereas the single-threaded version currently expects `#f`.
