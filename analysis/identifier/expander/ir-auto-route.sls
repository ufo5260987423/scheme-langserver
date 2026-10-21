#!chezscheme
(library (scheme-langserver analysis identifier expander ir-auto-route)
  ;; IR auto-route (route 3; doc/analysis/ir-auto-route-walkthrough.md): for
  ;; macro call sites dispatched by the router (ufo-match family, match-index-node),
  ;; expand with Chez's own expander and synthesize references from the IR.
  ;; Unconditional since 2026-10-21 -- the underlying Chez APIs are being
  ;; upstreamed for documentation, so no opt-out gate remains.
  (export
    ir-auto-route:try-expand
    ir-auto-route:stats
    ir-auto-route:reset-stats!
    ir-auto-route:env-source)
  (import
    (chezscheme)

    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier expander chez-ir))

;; Expansion environment, cached process-wide. Bootstrap order (v2,
;; 2026-10-20):
;;  1. probe interaction-environment -- fast path when the host imported
;;     (ufo-match) (e.g. run.ss in script mode). Mutable and lenient: expands
;;     defines directly.
;;  2. locate the ufo-match SOURCE and (load …) it: explicit load registers
;;     the library in Chez's global library manager and makes it resolvable
;;     in BOTH script and compiled binaries (measured with compile-chez-program
;;     probes). Release binaries no longer depend on the host import, and the
;;     expansion uses the WORKSPACE'S OWN macro copy (no version skew).
;;     Sources, in order:
;;       a. the workspace's own (ufo-match) library-node (walk-library)
;;       b. a CHEZSCHEMELIBDIRS scan for ufo-match.{sls,chezscheme.sls,ss}
;;     A fresh (environment ...) is IMMUTABLE -- rejects top-level define and
;;     is strict about free identifiers -- so the fresh-env mode expands via
;;     private:expand-fresh's let-wrapper + free-variable parameter growth.
;;  3. on failure cache 'unavailable (permanent silent fallback to route 2).
;; Probe verification is TWO-SIDED: interaction envs are LENIENT -- an unbound
;; `match` expands to ($top-level-value 'match) with NO error (false positive
;; for naive probes, and the `_` wildcard is rejected by ufo-match in every
;; mode). Real expansion consumes the keyword: no $top-level-value lookup,
;; and the failure thunk carries (quote match). The probe is '(match 1 [x x]).
;NO GATE: the IR path is unconditional since 2026-10-21 (the user is
;preparing an upstream PR to document the underlying Chez APIs -- sc-expand's
;records? switch, #%$record-type-descriptor, the Lsrc IR -- at which point
;the undocumented-access rationale for an opt-out disappears). The only
;remaining guard is chez-ir-available?'s feature self-check, which degrades
;to "no auto-resolution" on Chez builds lacking the APIs.
(define private:env-mutex (make-mutex))
(define private:env-source (box 'unset))  ; 'unset | 'interaction | (loaded . path) | 'unavailable

(define (ir-auto-route:env-source)
  (with-mutex private:env-mutex (unbox private:env-source)))

(define (private:string-index str ch)
  (let loop ([i 0])
    (cond
      [(= i (string-length str)) #f]
      [(char=? (string-ref str i) ch) i]
      [else (loop (+ i 1))])))

(define (private:split-on str ch)
  (let loop ([s str] [acc '()])
    (let ([idx (private:string-index s ch)])
      (if idx
        (loop (substring s (+ idx 1) (string-length s))
          (cons (substring s 0 idx) acc))
        (reverse (cons s acc))))))

(define (private:string-contains? str pat)
  (let ([slen (string-length str)]
      [plen (string-length pat)])
    (let loop ([i 0])
      (cond
        [(> (+ i plen) slen) #f]
        [(string=? (substring str i (+ i plen)) pat) #t]
        [else (loop (+ i 1))]))))

(define (private:probe-in env head-symbol)
  (guard (e [else #f])
    (let ([ir (sc-expand `(,head-symbol 1 [x x]) env #t)])
      (and (record? ir)
        ;negative: real expansion CONSUMES the keyword, so the lenient
        ;($top-level-value 'head-symbol) shape must be absent -- the check
        ;that works for every macro (recursive macros like match only recur
        ;with OTHER keywords, e.g. match-next).
        (not (chez-ir-global-lookup? ir (symbol->string head-symbol)))
        ;positive: keep the strict marker for the match family (its failure
        ;thunk carries (quote match)); for other macros the negative check
        ;alone is the best available signal.
        (or (not (eq? head-symbol 'match))
          (chez-ir-contains-quoted-symbol? ir 'match))))))

(define (private:probe-interaction head-symbol)
  (private:probe-in (interaction-environment) head-symbol))

(define (private:fresh-env library-identifier)
  (guard (e [else #f])
    (environment '(chezscheme) library-identifier)))

(define (private:libdirs-source library-identifier)
  (let ([spec (getenv "CHEZSCHEMELIBDIRS")]
      [base (symbol->string (car (reverse library-identifier)))])
    (and spec
      (find file-exists?
        (apply append
          (map
            (lambda (dir)
              (map (lambda (ext) (string-append dir "/" base ext))
                '(".sls" ".chezscheme.sls" ".ss")))
            (private:split-on spec #\:)))))))

(define (private:import-source document call-node head-symbol)
  ;;the macro-library document THIS call site's head resolves to -- the
  ;;precise import target. Beats guessing among multiple copies of the same
  ;;library in one workspace (e.g. test fixtures of old variants): a
  ;;workspace can contain several, and the consumer's import resolution
  ;;knows the right one.
  (guard (e [else #f])
    (let* ([refs (find-available-references-for document call-node head-symbol)]
        [paths
          (filter
            (lambda (p) (and p (file-exists? p)))
            (map
              (lambda (r)
                (let ([doc (identifier-reference-document r)])
                  (and doc
                    (guard (e2 [else #f])
                      (uri->path (document-uri doc))))))
              refs))])
      (and (not (null? paths)) (car paths)))))

(define (private:find-source document call-node root-library-node head-symbol library-identifier)
  (or (private:import-source document call-node head-symbol)
    (and (library-node? root-library-node)
      (let ([node (walk-library library-identifier root-library-node)])
        (and (not (null? node))
          (find file-exists?
            (map file-node-path (library-node-file-nodes node))))))
    (private:libdirs-source library-identifier)))

;Env cache: 'interaction -> env | 'failed; <source path> -> env | 'failed.
;Per-source caching matters when one server process analyzes several
;workspaces with DIFFERENT ufo-match copies -- each gets its own env.
(define private:env-cache-ht (make-hashtable equal-hash equal?))  ; keys: 'interaction | source path string

;Bootstrap returns (env . mode), mode in (interaction | fresh).
(define (private:bootstrap-interaction head-symbol)
  (let ([fast (hashtable-ref private:env-cache-ht 'interaction 'unset)])
    (cond
      [(eq? fast 'unset)
        (let ([env (and (private:probe-interaction head-symbol) (interaction-environment))])
          (hashtable-set! private:env-cache-ht 'interaction (or env 'failed))
          (when env (set-box! private:env-source 'interaction))
          (and env (cons env 'interaction)))]
      [(eq? fast 'failed) #f]
      [else (cons fast 'interaction)])))

(define (private:bootstrap-source document call-node root-library-node head-symbol library-identifier)
  (let ([source (private:find-source document call-node root-library-node head-symbol library-identifier)])
    (when (getenv "REPLAY_TRACE")
      (fprintf (console-output-port) "[ir-auto-route] bootstrap-source: ~a import=~a~%"
        source
        (guard (e ['err]) (private:import-source document call-node head-symbol))))
    (and source
      ;cache key pairs source WITH the library it must provide -- different
      ;libraries from the same directory (ufo-match vs ufo-match-steer) get
      ;distinct envs
      (let* ([key (cons source library-identifier)]
          [cached (hashtable-ref private:env-cache-ht key 'unset)])
        (cond
          [(eq? cached 'unset)
            (let ([env
                  (guard (e [else #f])
                    (load source)
                    (let ([built (private:fresh-env library-identifier)])
                      (and built (private:probe-in built head-symbol) built)))])
              (hashtable-set! private:env-cache-ht key (or env 'failed))
              (set-box! private:env-source
                (if env (cons 'loaded source) 'unavailable))
              (and env (cons env 'fresh)))]
          [(eq? cached 'failed) #f]
          [else (cons cached 'fresh)])))))

(define (private:token-after message start)
  ;;first whitespace-delimited token at/after start, or #f
  (let ([len (string-length message)])
    (let skip ([i start])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref message i) #\space) (skip (+ i 1))]
        [else
          (let take ([j i])
            (if (or (>= j len) (char=? (string-ref message j) #\space))
              (substring message i j)
              (take (+ j 1))))]))))

(define (private:find-substring str pat)
  ;;first index of pat in str, or #f
  (let ([slen (string-length str)]
      [plen (string-length pat)])
    (let loop ([i 0])
      (cond
        [(> (+ i plen) slen) #f]
        [(string=? (substring str i (+ i plen)) pat) i]
        [else (loop (+ i 1))]))))

(define (private:expand-blocked-identifier-from-message message)
  ;;Extract the identifier that blocked strict-env expansion, or #f. Two
  ;;failure classes, both neutralized by binding the identifier as a wrapper
  ;;parameter (opaquing it -- we only need the MATCH call's facts, not the
  ;;surrounding define's):
  ;;  "attempt to reference unbound identifier helper at char 33 ..." -> helper
  ;;  "misplaced aux keyword (except c ...)" -> except   -- CHEZSCHEME ITSELF
  ;;    declares some names (e.g. except) auxiliary keywords, so user code
  ;;    using them as function names cannot be sc-expanded in ANY
  ;;    chezscheme-containing env (measured 2026-10-21; previously masked by
  ;;    an accidental shadow in one host's import surface).
  ;;The token is read immediately AFTER the phrase (skipping "(" for the aux
  ;;case) -- messages continue with position info, so the LAST token would be
  ;;the filename.
  (let ([len (string-length message)])
    (define (token-after-phrase phrase skip-open-paren?)
      (let ([i (private:find-substring message phrase)])
        (and i
          (let skip ([j (+ i (string-length phrase))])
            (cond
              [(>= j len) #f]
              [(or (char=? (string-ref message j) #\space)
                  (and skip-open-paren? (char=? (string-ref message j) #\()))
                (skip (+ j 1))]
              [else
                (let take ([k j])
                  (if (or (>= k len)
                        (char=? (string-ref message k) #\space)
                        (char=? (string-ref message k) #\())
                    (string->symbol (substring message j k))
                    (take (+ k 1))))])))))
    (or (token-after-phrase "misplaced aux keyword" #t)
      (token-after-phrase "unbound identifier" #f))))

(define (private:wrap-form form extra-params)
  ;;(let () <form> #f) -- internal defines are accepted even in immutable envs
  ;;(measured), and the trailing #f satisfies "no expressions in body". The
  ;;wrapper is fabricated AS AN ANNOTATION so every INNER source position
  ;;survives intact (measured: refs keep their original biases). extra-params
  ;;add a lambda capturing free identifiers for the strict fresh env.
  (let* ([src (and (annotation? form) (annotation-source form))]
      [inner (list 'let '() form #f)]
      [body (if (null? extra-params)
              inner
              (list 'lambda (reverse extra-params) inner))])
    (if src
      (make-annotation body src body)
      body)))

(define (private:expand-fresh env form)
  ;;Fresh envs are strict about free identifiers (siblings defined elsewhere
  ;;in the user's library). Grow a (lambda (free ...) ...) parameter list
  ;;driven by the "unbound identifier" errors, bounded at 64 attempts.
  (let loop ([params '()] [attempts 0])
    (guard (e [else
        (let ([unbound (and (condition? e)
                        (private:expand-blocked-identifier-from-message
                          (call-with-string-output-port
                            (lambda (p) (display-condition e p)))))])
          (if (and unbound (< attempts 64) (not (memq unbound params)))
            (loop (cons unbound params) (+ attempts 1))
            (raise e)))])
      (sc-expand (private:wrap-form form params) env #t))))

(define (private:env document call-node root-library-node head-symbol library-identifier)
  (with-mutex private:env-mutex
    ;SOURCE FIRST: the fresh env built from the workspace's own macro copy
    ;is deterministic -- it contains exactly (chezscheme) + <the head's
    ;library>, so unrelated macros visible in the host interaction env
    ;(measured: a transitively-visible ufo-try shadowed a user FUNCTION
    ;named `except` with its auxiliary keyword and broke expansion of an
    ;unrelated define, 2026-10-21) can never leak in. The interaction env
    ;stays only as a last-resort fallback when no source is locatable.
    (or (private:bootstrap-source document call-node root-library-node head-symbol library-identifier)
      (private:bootstrap-interaction head-symbol))))

;; (env . mode) pair accessors: mode in (interaction | fresh)
(define (private:env-mode env-pair) (cdr env-pair))

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

(define (private:try root-library-node document call-node)
  ;; Returns #t when the IR path handled the call site, #f otherwise (the
  ;; call site then gets no expansion-derived references).
  ;; The head symbol and its library come from the call node itself -- the
  ;; mechanism is macro-agnostic (ufo-match family, match-index-node, ...).
  (let* ([head-symbol (index-node-expression (car (index-node-children call-node)))]
      [head-refs (find-available-references-for document call-node head-symbol)]
      [library-identifier
        (and (not (null? head-refs))
          (let ([lib (identifier-reference-library-identifier (car head-refs))])
            (and (not (null? lib)) lib)))])
    (and library-identifier
      (let ([env-pair (private:env document call-node root-library-node head-symbol library-identifier)])
    (and env-pair
      (guard (e [else (when (getenv "REPLAY_TRACE") (fprintf (console-output-port) "[ir-auto-route] miss: ~a in ~a at-bias=~a~%" (call-with-string-output-port (lambda (p) (display-condition e p))) (document-uri document) (index-node-start call-node))) (private:bump! (cons #f 0)) #f])
        (let* ([path (uri->path (document-uri document))]
            [text (document-text document)]
            [forms (private:forms text path)]
            [form (chez-ir-find-define-containing forms (index-node-start call-node))]
            [_ (or form (raise 'form-not-found))]
            [t0 (current-time 'time-monotonic)]
            [ir (if (eq? (private:env-mode env-pair) 'fresh)
                  (private:expand-fresh (car env-pair) form)
                  (chez-ir-expand form (car env-pair)))]
            [refs (chez-ir-call-site-refs ir path)]
            [inserted (private:synthesize! refs document call-node)]
            [t1 (current-time 'time-monotonic)]
            [ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)])
          (if (positive? inserted)
            (begin (private:bump! (cons #t ms)) #t)
            (begin (private:bump! (cons #f ms)) #f)))))))))

(define (ir-auto-route:try-expand)
  ;; The IR-only rule for the router's ufo-match arm (route 2 retired). On
  ;; ANY failure -- gate off, feature probe failed, env unavailable, miss --
  ;; the call site simply gets no expansion-derived references; the stats and
  ;; REPLAY_TRACE diagnostics carry the reason. root-file-node is accepted
  ;; for the rule-calling convention and unused.
  (lambda (root-file-node root-library-node document index-node)
    (and (chez-ir-available?)
      (private:try root-library-node document index-node))))
)
