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
    (scheme-langserver analysis identifier chez-ir))

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
;Gate: ON by default since 2026-10-20 (single-thread canary dominance + 14
;false-positive diagnoses eliminated + threaded soak 6/6 clean). Opt out with
;SLSCHEME_IR_ROUTE=0 (or "off") -- e.g. while bisecting an analysis regression.
(define private:gate-value (getenv "SLSCHEME_IR_ROUTE"))
(define private:gate?
  (not (and private:gate-value
         (or (string=? private:gate-value "0")
           (string=? private:gate-value "off")))))
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

(define (private:probe-in env)
  (guard (e [else #f])
    (let ([ir (sc-expand '(match 1 [x x]) env #t)])
      (and (record? ir)
        (not (chez-ir-global-lookup? ir "match"))
        (chez-ir-contains-quoted-symbol? ir 'match)))))

(define (private:probe-interaction)
  (private:probe-in (interaction-environment)))

(define (private:fresh-env)
  (guard (e [else #f])
    (environment '(chezscheme) '(ufo-match))))

(define (private:libdirs-source)
  (let ([spec (getenv "CHEZSCHEMELIBDIRS")])
    (and spec
      (find file-exists?
        (apply append
          (map
            (lambda (dir)
              (map (lambda (name) (string-append dir "/" name))
                '("ufo-match.sls" "ufo-match.chezscheme.sls" "ufo-match.ss")))
            (private:split-on spec #\:)))))))

(define (private:import-source document call-node)
  ;;the ufo-match document THIS call site's `match` head resolves to -- the
  ;;precise import target. Beats guessing among multiple (ufo-match) copies
  ;;in one workspace (e.g. test fixtures of old variants): a workspace can
  ;;contain several, and the consumer's import resolution knows the right one.
  (guard (e [else #f])
    (let* ([refs (find-available-references-for document call-node 'match)]
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

(define (private:find-source document call-node root-library-node)
  (or (private:import-source document call-node)
    (and (library-node? root-library-node)
      (let ([node (walk-library '(ufo-match) root-library-node)])
        (and (not (null? node))
          (find file-exists?
            (map file-node-path (library-node-file-nodes node))))))
    (private:libdirs-source)))

;Env cache: 'interaction -> env | 'failed; <source path> -> env | 'failed.
;Per-source caching matters when one server process analyzes several
;workspaces with DIFFERENT ufo-match copies -- each gets its own env.
(define private:env-cache-ht (make-hashtable equal-hash equal?))  ; keys: 'interaction | source path string

;Bootstrap returns (env . mode), mode in (interaction | fresh).
(define (private:bootstrap-interaction)
  (let ([fast (hashtable-ref private:env-cache-ht 'interaction 'unset)])
    (cond
      [(eq? fast 'unset)
        (let ([env (and (private:probe-interaction) (interaction-environment))])
          (hashtable-set! private:env-cache-ht 'interaction (or env 'failed))
          (when env (set-box! private:env-source 'interaction))
          (and env (cons env 'interaction)))]
      [(eq? fast 'failed) #f]
      [else (cons fast 'interaction)])))

(define (private:bootstrap-source document call-node root-library-node)
  (let ([source (private:find-source document call-node root-library-node)])
    (when (getenv "REPLAY_TRACE")
      (fprintf (console-output-port) "[ir-auto-route] bootstrap-source: ~a import=~a~%"
        source
        (guard (e ['err]) (private:import-source document call-node))))
    (and source
      (let ([cached (hashtable-ref private:env-cache-ht source 'unset)])
        (cond
          [(eq? cached 'unset)
            (let ([env
                  (guard (e [else #f])
                    (load source)
                    (let ([built (private:fresh-env)])
                      (and built (private:probe-in built) built)))])
              (hashtable-set! private:env-cache-ht source (or env 'failed))
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

(define (private:unbound-identifier-from-message message)
  ;;"...unbound identifier helper at char 33 of t.ss" -> 'helper, else #f.
  ;;The identifier is the token immediately FOLLOWING the phrase -- messages
  ;;continue with position info, so the LAST token would be the filename.
  (let ([phrase "unbound identifier"]
      [len (string-length message)])
    (and (private:string-contains? message phrase)
      (let find ([i 0])
        (cond
          [(> (+ i (string-length phrase)) len) #f]
          [(string=? (substring message i (+ i (string-length phrase))) phrase)
            (let ([token (private:token-after message (+ i (string-length phrase)))])
              (and token (string->symbol token)))]
          [else (find (+ i 1))])))))

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
  ;;driven by the "unbound identifier" errors, bounded at 8 attempts.
  (let loop ([params '()] [attempts 0])
    (guard (e [else
        (let ([unbound (and (condition? e)
                        (private:unbound-identifier-from-message
                          (call-with-string-output-port
                            (lambda (p) (display-condition e p)))))])
          (if (and unbound (< attempts 8) (not (memq unbound params)))
            (loop (cons unbound params) (+ attempts 1))
            (raise e)))])
      (sc-expand (private:wrap-form form params) env #t))))

(define (private:env document call-node root-library-node)
  (with-mutex private:env-mutex
    (or (private:bootstrap-interaction)
      (private:bootstrap-source document call-node root-library-node))))

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
  ;; Returns #t when the IR path handled the call site, #f to fall back.
  (let ([env-pair (private:env document call-node root-library-node)])
    (and env-pair
      (guard (e [else (when (getenv "REPLAY_TRACE") (fprintf (console-output-port) "[ir-auto-route] miss: ~a at-bias=~a~%" (call-with-string-output-port (lambda (p) (display-condition e p))) (index-node-start call-node))) (private:bump! (cons #f 0)) #f])
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
            (begin (private:bump! (cons #f ms)) #f)))))))

(define (ir-auto-route:wrap-rule route2-rule)
  ;; Wrap a route-2 rule procedure (the expansion-generator->rule lambda) so
  ;; the IR path is attempted first when the gate is on. Silent on any IR
  ;; failure: the route-2 rule runs exactly as before.
  (lambda (root-file-node root-library-node document index-node)
    (if (and private:gate? (chez-ir-available?))
      (or (private:try root-library-node document index-node)
        (route2-rule root-file-node root-library-node document index-node))
      (route2-rule root-file-node root-library-node document index-node))))
)
