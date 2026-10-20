#!chezscheme
(library (scheme-langserver analysis identifier ir-auto-route)
  ;; Controlled parallel phase for route 3 (doc/analysis/chez-sc-expand-records-switch.md
  ;; §9): for ufo-match-family call sites, try Chez's own expander IR first and
  ;; silently fall back to the route-2 rule on ANY failure. Everything is gated
  ;; behind the SLSCHEME_IR_ROUTE environment variable: with the gate off this
  ;; module is inert and route 2 behavior is bit-for-bit unchanged.
  (export
    ir-auto-route:wrap-rule
    ir-auto-route:stats
    ir-auto-route:reset-stats!)
  (import
    (chezscheme)

    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier chez-ir))

;; gate + cached expansion environment. A fresh (environment ...) is immutable
;; and rejects the DEFINE forms we must expand ("invalid definition in
;; immutable environment", measured), and is strict about free identifiers.
;; The interaction environment is mutable and lenient -- but only works when
;; the host process imports (ufo-match). Probe it once with a tiny match
;; expansion; on failure the IR path stays off (silent fallback) until the
;; host imports ufo-match (one line in the server entry script).
;Gate: ON by default since 2026-10-20 (single-thread canary dominance + 14
;false-positive diagnoses eliminated + threaded soak 6/6 clean). Opt out with
;SLSCHEME_IR_ROUTE=0 (or "off") -- e.g. while bisecting an analysis regression.
(define private:gate-value (getenv "SLSCHEME_IR_ROUTE"))
(define private:gate?
  (not (and private:gate-value
         (or (string=? private:gate-value "0")
           (string=? private:gate-value "off")))))
(define private:env-mutex (make-mutex))
(define private:env-cache (box 'unset)) ; 'unset | 'unavailable | <env>

(define (private:env)
  (with-mutex private:env-mutex
    (let ([cached (unbox private:env-cache)])
      (cond
        [(not (eq? cached 'unset)) (and (not (eq? cached 'unavailable)) cached)]
        [else
          (let ([built
              (guard (e [else 'unavailable])
                (let ([env (interaction-environment)])
                  ;NOTE the probe must NOT use the `_` wildcard: ufo-match
                  ;rejects it ("misplaced aux keyword") in every mode -- a
                  ;pre-existing quirk unrelated to env availability. A `_`
                  ;probe falsely reported 'unavailable in COMPILED binaries
                  ;(measured 2026-10-20) and silently disabled the IR path
                  ;there. [x x] probes macro resolution only.
                  (sc-expand '(match 1 [x x]) env #t)
                  env))])
            (set-box! private:env-cache built)
            (and (not (eq? built 'unavailable)) built))]))))

;; annotated-forms cache keyed by the document TEXT (not the path): parsing
;; the in-memory text directly (sfd constructed over the string) keeps the IR
;; path correct for unsaved/dirty buffers -- a disk-based design would fall
;; back to route 2 for every edited file, i.e. throughout an editing session.
(define private:forms-cache (make-hashtable string-hash string=?))
(define private:forms-mutex (make-mutex))

(define (private:forms-from-text text path)
  (let ([port (open-input-string text)]
      [sfd (make-source-file-descriptor path (open-bytevector-input-port (string->utf8 text)))])
    (let loop ([bfp 0] [acc '()])
      (let-values ([(datum efp) (get-datum/annotations port sfd bfp)])
        (if (eof-object? datum)
          (begin (close-port port) (reverse acc))
          (loop efp (cons datum acc)))))))

(define (private:forms text path)
  (with-mutex private:forms-mutex
    ;bounded: an editing session produces a new text per change notification;
    ;without a cap the strong-keyed cache would retain every revision
    (when (> (hashtable-size private:forms-cache) 64)
      (hashtable-clear! private:forms-cache))
    (let ([cached (hashtable-ref private:forms-cache text #f)])
      (or cached
        (let ([parsed (private:forms-from-text text path)])
          (hashtable-set! private:forms-cache text parsed)
          parsed)))))

;; stats: (hits fallback-total-ms hits-total-ms). Mutex-guarded: threaded step
;; invokes rules from worker threads.
(define private:stats-mutex (make-mutex))
(define private:stats (box '(0 0 0)))

(define (ir-auto-route:stats)
  (with-mutex private:stats-mutex (unbox private:stats)))

(define (ir-auto-route:reset-stats!)
  (with-mutex private:stats-mutex (set-box! private:stats '(0 0 0))))

(define (private:bump! hits-ms)
  ;hits-ms: (hit? . milliseconds)
  (with-mutex private:stats-mutex
    (let ([s (unbox private:stats)])
      (set-box! private:stats
        (list
          (+ (car s) (if (car hits-ms) 1 0))
          (+ (cadr s) (if (car hits-ms) 0 1))
          (+ (caddr s) (cdr hits-ms)))))))

(define (private:synthesize! call-site-refs document call-node)
  ;; Materialize macro-derived bindings from IR ground truth, mimicking the
  ;; route-2 attachment surface: the reference resolves from the macro call
  ;; node (ancestor of every use) AND is exported at the anchor node, like
  ;; let-bindings (regist-as-identifier-reference). Only bindings INSIDE the
  ;; call span are macro-derived; ordinary bindings (lambda params) already
  ;; exist via the normal rules. Returns the number of synthesized references.
  (let ([seen (make-hashtable equal-hash equal?)]
      [inserted 0])
    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
            [ref-bias (cadr entry)]
            [binding-bias (caddr entry)]
            [macro-derived?
              (and binding-bias
                (>= binding-bias (index-node-start call-node))
                (< binding-bias (index-node-end call-node)))])
          ;ref-bias also must be at/inside the call: entry came from this
          ;call site's form, but nested forms can contribute refs -- the call
          ;span filter keeps each synthesis attached to its own call node.
          (when (and macro-derived?
                  (>= ref-bias (index-node-start call-node))
                  (< ref-bias (index-node-end call-node)))
            (unless (hashtable-ref seen (list name binding-bias) #f)
              (hashtable-set! seen (list name binding-bias) #t)
              (let* ([picked (pick-index-node-from (document-index-node-list document) binding-bias)]
                  [anchor (if (index-node? picked) picked call-node)]
                  [ref (make-identifier-reference name document anchor call-node '() 'variable '() '())])
                (identifier-reference-speculative?-set! ref #t)
                (index-node-references-export-to-other-node-set! anchor
                  (append (index-node-references-export-to-other-node anchor) `(,ref)))
                (index-node-references-import-in-this-node-set! call-node
                  (sort-identifier-references
                    (cons ref (index-node-references-import-in-this-node call-node))))
                (set! inserted (+ inserted 1)))))))
      call-site-refs)
    inserted))

(define (private:try document call-node)
  ;; Returns #t when the IR path handled the call site, #f to fall back.
  (let ([env (private:env)])
    (and env
      (guard (e [else (when (getenv "REPLAY_TRACE") (fprintf (console-output-port) "[ir-auto-route] miss: ~a at-bias=~a irritants=~a~%" (if (condition? e) (condition-message e) e) (index-node-start call-node) (if (condition? e) (condition-irritants e) '()))) (private:bump! (cons #f 0)) #f])
        (let* ([path (uri->path (document-uri document))]
            [text (document-text document)]
            [forms (private:forms text path)]
            [form (chez-ir-find-define-containing forms (index-node-start call-node))]
            [_ (or form (raise 'form-not-found))]
            [t0 (current-time 'time-monotonic)]
            [ir (chez-ir-expand form env)]
            [refs (chez-ir-call-site-refs ir path)]
            [inserted (private:synthesize! refs document call-node)]
            [t1 (current-time 'time-monotonic)]
            [ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)])
          (if (positive? inserted)
            (begin (private:bump! (cons #t ms)) #t)
            (begin (private:bump! (cons #f ms)) #f)))))))

(define (ir-auto-route:wrap-rule route2-rule)
  ;; Wrap a route-2 rule procedure (the expansion-generator->rule lambda) so
  ;; the IR path is attempted first when the gate is on. Silent on any IR
  ;; failure: the route-2 rule runs exactly as before.
  (lambda (root-file-node root-library-node document index-node)
    (if (and private:gate? (chez-ir-available?))
      (or (private:try document index-node)
        (route2-rule root-file-node root-library-node document index-node))
      (route2-rule root-file-node root-library-node document index-node))))
)
