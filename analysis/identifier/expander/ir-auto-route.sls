#!chezscheme
(library (scheme-langserver analysis identifier expander ir-auto-route)
  ;; IR auto-route (route 3; doc/analysis/ir-auto-route-walkthrough.md): for
  ;; macro call sites dispatched by the router (ufo-match family, match-index-node),
  ;; expand with Chez's own expander and synthesize references from the IR.
  ;; Unconditional since 2026-10-21 -- the underlying Chez APIs are being
  ;; upstreamed for documentation, so no opt-out gate remains.
  (export
    ir-auto-route:enabled?
    ir-auto-route:try-expand
    ir-auto-route:stats
    ir-auto-route:reset-stats!
    ir-auto-route:env-source
    ir-auto-route:call-site-flows
    ir-auto-route:expand-call-site
    ir-auto-route:type-stats
    ir-auto-route:reset-type-stats!)
  (import
    (chezscheme)
    (only (srfi :13 strings) string-contains string-index)

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
;OPT-IN SINCE 2026-10-29: the whole mechanism (identifier-side auto-route AND
;type-side macro inference) sits behind the ir-auto-route:enabled? parameter,
;DEFAULT #f (off). run.ss -a/--auto-expand enable turns it on; init-server's
;optional auto-expand? argument sets the parameter before the thread pool is
;created (forked workers inherit it). Tests and log-replay scripts that
;exercise the mechanism set the parameter explicitly.
(define ir-auto-route:enabled? (make-parameter #f))

(define private:env-mutex (make-mutex))
(define private:env-source (box 'unset))  ; 'unset | 'interaction | (loaded . path) | 'unavailable

(define (ir-auto-route:env-source)
  (with-mutex private:env-mutex (unbox private:env-source)))

;;split-on keeps its own name (SRFI-13 has no string-split), but delegates
;;the search to SRFI-13's string-index (a char criterion is a drop-in for
;;the old private:string-index).
(define (private:split-on str ch)
  (let loop ([s str] [acc '()])
    (let ([idx (string-index s ch)])
      (if idx
        (loop (substring s (+ idx 1) (string-length s))
          (cons (substring s 0 idx) acc))
        (reverse (cons s acc))))))

(define (private:probe-in env head-symbol)
  ;;Probe that expansion in `env` really engages the head macro. The probe
  ;;form is ufo-match-shaped -- adequate for the match family (and any macro
  ;;accepting literals/vectors), but arbitrary macros may reject it; since
  ;;the 2026-10-29 generic arm it is ADVISORY at the bootstrap-source call
  ;;site and strict only for the interaction fallback.
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

(define (private:deep-strip o)
  (let ([s (if (annotation? o) (annotation-stripped o) o)])
    (if (pair? s)
      (cons (private:deep-strip (car s)) (private:deep-strip (cdr s)))
      s)))

(define (private:doc-import-sets forms)
  ;;the (import …) clause of the document's library form, as datum import
  ;;sets -- exactly what the user's code compiles against. '() for script
  ;;files or anything unusual. Including these in the expansion env gives
  ;;auxiliary-keyword patterns their exact semantics (the user's ?/:_/**1
  ;;share bindings with the macro's aux definitions) instead of the retry
  ;;machinery opaquing them as plain pattern variables.
  (guard (e [else '()])
    (let* ([lib (find (lambda (f)
                (let ([s (private:deep-strip f)])
                  (and (pair? s) (eq? 'library (car s)))))
              forms)]
        [clause (and lib
                (find (lambda (sub)
                  (and (pair? sub) (eq? 'import (car sub))))
                  (cdr (private:deep-strip lib))))])
      (if clause (cdr clause) '()))))

(define (private:build-env library-identifier import-sets workspace-root)
  ;;EXACT semantics: chezscheme + the head's library + the document's own
  ;;imports (head library deduped). The workspace root is prepended to
  ;;library-directories so project-internal imports resolve; any failure
  ;;(an un-loadable import) is caught by the caller's guard and falls back
  ;;to the minimal env.
  (guard (e [else #f])
    (let ([sets (cons library-identifier
                (filter (lambda (spec) (not (equal? spec library-identifier)))
                  import-sets))])
      (parameterize ([library-directories
          (if (and workspace-root (file-directory? workspace-root))
            (cons (cons workspace-root workspace-root) (library-directories))
            (library-directories))])
        (apply environment (cons '(chezscheme) sets))))))

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

(define (private:bootstrap-source document call-node root-library-node import-sets workspace-root head-symbol library-identifier)
  (let ([source (private:find-source document call-node root-library-node head-symbol library-identifier)])
    (when (getenv "REPLAY_TRACE")
      (fprintf (console-output-port) "[ir-auto-route] bootstrap-source: ~a import=~a~%"
        source
        (guard (e ['err]) (private:import-source document call-node head-symbol))))
    (and source
      ;cache key: source x library x document imports -- env content depends
      ;on all three
      (let* ([key (list source library-identifier import-sets)]
          [cached (hashtable-ref private:env-cache-ht key 'unset)])
        (cond
          [(eq? cached 'unset)
            (let ([env
                  (guard (e [else #f])
                    (load source)
                    (let ([built (or (private:build-env library-identifier import-sets workspace-root)
                                    (private:fresh-env library-identifier))])
                      (and built
                        (begin
                          ;ADVISORY probe: private:probe-in uses a
                          ;ufo-match-shaped probe form that arbitrary macros
                          ;need not accept (the 2026-10-29 generic arm reaches
                          ;macros with any syntax). With a source-bootstrapped
                          ;env the head is bound BY CONSTRUCTION (load +
                          ;environment succeeded), so a probe failure must not
                          ;reject the env; the real expansion remains the
                          ;arbiter (miss -> negative cache). The interaction
                          ;fallback keeps the strict two-sided probe -- that
                          ;is where lenient false-positives bit (2026-10-21).
                          (unless (private:probe-in built head-symbol)
                            (when (getenv "REPLAY_TRACE")
                              (fprintf (console-output-port) "[ir-auto-route] advisory probe miss: ~a in ~a~%"
                                head-symbol source)))
                          built))))])
              (hashtable-set! private:env-cache-ht key (or env 'failed))
              (set-box! private:env-source
                (if env (cons 'loaded source) 'unavailable))
              (and env (cons env 'fresh)))]
          [(eq? cached 'failed) #f]
          [else (cons cached 'fresh)])))))

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
      (let ([i (string-contains message phrase)])
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

(define private:fresh-params-cache (make-eq-hashtable))
(define private:fresh-params-cache-mutex (make-mutex))

(define (private:expand-fresh env form)
  ;;Fresh envs are strict about free identifiers (siblings defined elsewhere
  ;;in the user's library). Grow a (lambda (free ...) ...) parameter list
  ;;driven by the "unbound identifier" errors, bounded at 64 attempts.
  ;;Each attempt re-sc-expands the WHOLE wrapped form, so the retry count
  ;;is the cost multiplier.
  ;;
  ;;PARAMS MEMO: the discovered params are keyed by (form identity x env
  ;;identity) -- the forms cache returns the SAME annotated form objects
  ;;to both the identifier and the type pass and across refreshes until
  ;;the text changes, and envs are cached per source x library x imports.
  ;;Sibling call sites in the same define (and the second pass over it)
  ;;then start from the full param list and succeed on the FIRST
  ;;attempt: measured 2026-10-28 on this repo, 225 fresh expansions with
  ;;196 retries (~60% of all fresh sc-expand time was re-discovering the
  ;;same free identifiers). Mutex-guarded: threaded init runs these on
  ;;worker threads.
  (define (attempt params)
    (sc-expand (private:wrap-form form params) env #t))
  (let ([cached (with-mutex private:fresh-params-cache-mutex
                  (hashtable-ref private:fresh-params-cache form #f))])
    (if (and cached (eq? (car cached) env))
      (attempt (cdr cached))
      (let loop ([params '()] [attempts 0])
        (guard (e [else
            (let ([unbound (and (condition? e)
                            (private:expand-blocked-identifier-from-message
                              (call-with-string-output-port
                                (lambda (p) (display-condition e p)))))])
              (if (and unbound (< attempts 64) (not (memq unbound params)))
                (loop (cons unbound params) (+ attempts 1))
                (raise e)))])
          (let ([ir (attempt params)])
            (with-mutex private:fresh-params-cache-mutex
              (hashtable-set! private:fresh-params-cache form (cons env (reverse params))))
            ir))))))

(define (private:env document call-node root-library-node import-sets workspace-root head-symbol library-identifier)
  (with-mutex private:env-mutex
    ;SOURCE FIRST: the fresh env built from the workspace's own macro copy
    ;is deterministic -- it contains exactly (chezscheme) + <the head's
    ;library>, so unrelated macros visible in the host interaction env
    ;(measured: a transitively-visible ufo-try shadowed a user FUNCTION
    ;named `except` with its auxiliary keyword and broke expansion of an
    ;unrelated define, 2026-10-21) can never leak in. The interaction env
    ;stays only as a last-resort fallback when no source is locatable.
    (or (private:bootstrap-source document call-node root-library-node import-sets workspace-root head-symbol library-identifier)
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

;;doc-import-sets cache: same text-keyed + bounded policy as forms. The
;;sets are a pure function of the document text, and the deep-strip walk
;;cost ~0.2ms PER MACRO CALL SITE (measured: ~70% of a failing expansion
;;attempt; with thousands of call sites in vendored .akku libraries this
;;alone made the type pass the whole-repo bottleneck)
(define private:import-sets-cache (make-hashtable string-hash string=?))
(define private:import-sets-mutex (make-mutex))

(define (private:doc-import-sets-cached text forms)
  (with-mutex private:import-sets-mutex
    (when (> (hashtable-size private:import-sets-cache) 64)
      (hashtable-clear! private:import-sets-cache))
    (let ([cached (hashtable-ref private:import-sets-cache text #f)])
      (or cached
        (let ([sets (private:doc-import-sets forms)])
          (hashtable-set! private:import-sets-cache text sets)
          sets)))))

;;negative cache for expand-call-site: call sites that cannot expand (no
;;enclosing define, or dead env bootstrap) skip the whole attempt on
;;repeat. Keyed by document + bias with TEXT-IDENTITY invalidation (the
;;didChange path mutates the document in place, so an unchecked entry
;;would outlive its text); success deletes the entry because env
;;availability can change without the user text changing (the macro
;;library itself may be edited). Mutex-guarded: threaded init runs the
;;type pass on worker threads and unsynchronized eq-hashtable writes are
;;the documented bucket-corruption hazard.
(define private:negative-cache (make-eq-hashtable))
(define private:negative-cache-mutex (make-mutex))

(define (private:negative? document bias)
  (with-mutex private:negative-cache-mutex
    (let ([entry (hashtable-ref private:negative-cache document #f)])
      (and entry
        (eq? (document-text document) (car entry))
        (hashtable-contains? (cdr entry) bias)))))

(define (private:negative! document bias)
  (with-mutex private:negative-cache-mutex
    (let* ([text (document-text document)]
        [entry (hashtable-ref private:negative-cache document #f)]
        [ht (if (and entry (eq? text (car entry)))
              (cdr entry)
              (let ([ht (make-eq-hashtable)])
                (hashtable-set! private:negative-cache document (cons text ht))
                ht))])
      (hashtable-set! ht bias #t))))

(define (private:unnegative! document bias)
  (with-mutex private:negative-cache-mutex
    (let ([entry (hashtable-ref private:negative-cache document #f)])
      (when (and entry (eq? (document-text document) (car entry)))
        (hashtable-delete! (cdr entry) bias)))))

;; stats: (hits fallback-total-ms hits-total-ms). Mutex-guarded: threaded step
;; invokes rules from worker threads.
(define private:stats-mutex (make-mutex))
(define private:stats (box '(0 0 0)))

(define (ir-auto-route:type-stats)
  ;;(type-flow hits type-flow misses total milliseconds), mutex-guarded
  (with-mutex private:type-stats-mutex (unbox private:type-stats)))

(define (ir-auto-route:reset-type-stats!)
  (with-mutex private:type-stats-mutex (set-box! private:type-stats (list 0 0 0))))

(define (ir-auto-route:stats)
  (with-mutex private:stats-mutex (unbox private:stats)))

(define (ir-auto-route:reset-stats!)
  (with-mutex private:stats-mutex (set-box! private:stats '(0 0 0))))

(define private:type-stats (box (list 0 0 0)))
(define private:type-stats-mutex (make-mutex))

(define (private:bump-type! hits-ms)
  (with-mutex private:type-stats-mutex
    (let ([s (unbox private:type-stats)])
      (set-box! private:type-stats
        (list
          (+ (car s) (if (car hits-ms) 1 0))
          (+ (cadr s) (if (car hits-ms) 0 1))
          (+ (caddr s) (cdr hits-ms)))))))

(define (private:bump! hits-ms)
  ;hits-ms: (hit? . milliseconds)
  (with-mutex private:stats-mutex
    (let ([s (unbox private:stats)])
      (set-box! private:stats
        (list
          (+ (car s) (if (car hits-ms) 1 0))
          (+ (cadr s) (if (car hits-ms) 0 1))
          (+ (caddr s) (cdr hits-ms)))))))

(define (private:merge-use-less-bindings refs bindings)
  ;;refs:     ((name ref-bias binding-bias) ...) -- use-driven
  ;;bindings: ((name binding-bias) ...)          -- binding-driven
  ;;Add a (name #f binding-bias) entry for every binding no use entry
  ;;covers, so never-used macro-derived bindings (an unused match pattern
  ;;variable) still materialize into references and show up in completion.
  (let ([covered (make-hashtable equal-hash equal?)])
    (for-each
      (lambda (entry)
        (let ([binding-bias (caddr entry)])
          (when binding-bias
            (hashtable-set! covered (cons (car entry) binding-bias) #t))))
      refs)
    (let loop ([bs bindings]
        [acc '()])
      (cond
        [(null? bs) (append refs (reverse acc))]
        [(hashtable-ref covered (cons (caar bs) (cadar bs)) #f)
          (loop (cdr bs) acc)]
        [else
          (loop (cdr bs)
            (cons (list (caar bs) #f (cadar bs)) acc))]))))

(define (private:synthesize! call-site-refs document call-node)
  ;; Materialize macro-derived bindings from IR ground truth. Entries are
  ;; grouped by (name . binding-bias); each group becomes ONE reference,
  ;; exported at the binding-position anchor (the goto-definition target)
  ;; and imported at the NARROWEST scope that is an ancestor of every use:
  ;; the direct child of the call node containing the binding (for
  ;; clause-shaped macros, the clause itself), falling back to the call
  ;; node. Per-clause scoping keeps sibling-clause variables out of each
  ;; other's bodies (match's c in [(a b) ...] [(a b c) ...] is invisible
  ;; in the first clause) and one reference per group removes the duplicate
  ;; candidates the shared call-node scope produced. Use-less entries
  ;; (ref-bias #f) synthesize bindings that are never referenced. A group
  ;; whose anchor already exports the same identifier is skipped: the
  ;; normal identifier rules registered that binding (e.g. a lambda
  ;; parameter inside the clause body), so synthesizing would duplicate it.
  ;; Only bindings INSIDE the call span are macro-derived. Returns the
  ;; number of synthesized references.
  (let* ([groups (make-hashtable equal-hash equal?)]
      [group-keys '()]
      [inserted 0])
    (for-each
      (lambda (entry)
        (let ([name (car entry)]
            [ref-bias (cadr entry)]
            [binding-bias (caddr entry)])
          ;the binding must live inside THIS call site's span (nested forms
          ;contribute refs of their own; the span filter keeps each
          ;synthesis attached to its own call node); uses outside the span
          ;are dropped, preserving the per-entry filter of the old loop
          (when (and binding-bias
                  (>= binding-bias (index-node-start call-node))
                  (< binding-bias (index-node-end call-node))
                  (or (not ref-bias)
                    (and (>= ref-bias (index-node-start call-node))
                      (< ref-bias (index-node-end call-node)))))
            (let ([key (cons name binding-bias)])
              (let ([uses (hashtable-ref groups key #f)])
                (if uses
                  (when ref-bias (set-cdr! uses (cons ref-bias (cdr uses))))
                  (begin
                    (hashtable-set! groups key (cons #f (if ref-bias `(,ref-bias) '())))
                    (set! group-keys (cons key group-keys)))))))))
      call-site-refs)
    (for-each
      (lambda (key)
        (let* ([name (car key)]
            [binding-bias (cdr key)]
            [uses (cdr (hashtable-ref groups key #f))]
            [picked (pick-index-node-from (document-index-node-list document) binding-bias)]
            [anchor (if (index-node? picked) picked call-node)]
            ;scope: the direct child of the call node whose span contains
            ;the binding AND every use (for a clause-shaped macro this is
            ;the clause, so sibling clauses never see the variable); any
            ;violation falls back to the call node, which is an ancestor of
            ;every in-span use by construction
            [child (find (lambda (c)
                           (and (<= (index-node-start c) binding-bias)
                             (< binding-bias (index-node-end c))))
                     (index-node-children call-node))]
            [scope (if (and child
                           (for-all (lambda (u)
                                      (and (<= (index-node-start child) u)
                                        (< u (index-node-end child))))
                                    uses))
                     child
                     call-node)])
          (unless (find (lambda (r)
                          (eq? name (identifier-reference-identifier r)))
                  (index-node-references-export-to-other-node anchor))
            (let ([ref (make-identifier-reference name document anchor call-node '() 'variable '() '())])
              (identifier-reference-speculative?-set! ref #t)
              (index-node-references-export-to-other-node-set! anchor
                (append (index-node-references-export-to-other-node anchor) `(,ref)))
              (index-node-references-import-in-this-node-set! scope
                (sort-identifier-references
                  (cons ref (index-node-references-import-in-this-node scope))))
              (set! inserted (+ inserted 1))))))
      (reverse group-keys))
    inserted))

;Rebuilt-IR-node budget for ONE expansion on the IDENTIFIER side, enforced
;in chez-ir-call-site-facts' record-walk via chez-ir-rebuild-budget (the
;broad macro arm of 2026-10-29 exposes the walk to arbitrary user macros;
;the type side's cap in macro-expansion.sls guards the REBUILD, this one
;guards the WALK). Keep the two limits in sync.
(define private:max-expansion-nodes 100000)

(define (private:try root-file-node root-library-node document call-node)
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
      (let* ([path (uri->path (document-uri document))]
          [text (document-text document)]
          [forms (private:forms text path)]
          [import-sets (private:doc-import-sets-cached text forms)]
          [workspace-root (and root-file-node (file-node-path root-file-node))]
          [env-pair (private:env document call-node root-library-node import-sets workspace-root head-symbol library-identifier)])
    (and env-pair
      (guard (e [else (when (getenv "REPLAY_TRACE") (fprintf (console-output-port) "[ir-auto-route] miss: ~a in ~a at-bias=~a~%" (call-with-string-output-port (lambda (p) (display-condition e p))) (document-uri document) (index-node-start call-node))) (private:bump! (cons #f 0)) #f])
        (let* ([form (chez-ir-find-define-containing forms (index-node-start call-node))]
            [_ (or form (raise 'form-not-found))]
            [t0 (current-time 'time-monotonic)]
            [ir (parameterize ([chez-ir-rebuild-budget (cons 0 private:max-expansion-nodes)])
                  (if (eq? (private:env-mode env-pair) 'fresh)
                    (private:expand-fresh (car env-pair) form)
                    (chez-ir-expand form (car env-pair))))]
            [refs (let-values ([(r b) (chez-ir-call-site-facts ir path)])
                    (private:merge-use-less-bindings r b))]
            [inserted (private:synthesize! refs document call-node)]
            [t1 (current-time 'time-monotonic)]
            [ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)])
          (if (positive? inserted)
            (begin (private:bump! (cons #t ms)) #t)
            (begin (private:bump! (cons #f ms)) #f)))))))))

(define (ir-auto-route:expand-call-site root-library-node document call-node)
  ;;Expand the macro call site with Chez's own expander and return the
  ;;RECONSTRUCTED annotated s-expression of the enclosing define form
  ;;(chez-ir->annotated-sexp restores the positions $uncprep strips). This
  ;;is the TYPE side's window into the expansion: the type generator runs
  ;;over the reconstructed tree and results map back by source position.
  ;;Any failure = #f with a type-stats miss (see REPLAY_TRACE).
  ;;Order matters: the enclosing-define lookup runs BEFORE the env
  ;;bootstrap because most call sites cannot expand at all (measured:
  ;;define-auxiliary-keyways bodies, internal macro calls inside
  ;;define-syntax/lets across vendored libraries -- thousands of them),
  ;;and find-define-containing is the cheapest deterministic reject.
  (let ([bias (index-node-start call-node)])
    (and (not (private:negative? document bias))
      (let* ([head-symbol (index-node-expression (car (index-node-children call-node)))]
          [head-refs (find-available-references-for document call-node head-symbol)]
          [library-identifier
            (and (not (null? head-refs))
              (let ([lib (identifier-reference-library-identifier (car head-refs))])
                (and (not (null? lib)) lib)))])
        (and library-identifier
          (let ([path (uri->path (document-uri document))])
            (guard (e [else
                      (private:negative! document bias)
                      (when (getenv "REPLAY_TRACE")
                        (fprintf (console-output-port) "[ir-auto-route] expand-call-site miss: ~a in ~a at-bias=~a~%"
                          (call-with-string-output-port (lambda (p) (display-condition e p)))
                          (document-uri document) (index-node-start call-node)))
                      (private:bump-type! (cons #f 0))
                      #f])
              (let* ([text (document-text document)]
                  [forms (private:forms text path)]
                  [form (chez-ir-find-define-containing forms bias)]
                  [_ (or form (raise 'form-not-found))]
                  [import-sets (private:doc-import-sets-cached text forms)]
                  [env-pair (private:env document call-node root-library-node import-sets #f head-symbol library-identifier)])
                (if env-pair
                  (let* ([t0 (current-time 'time-monotonic)]
                      [ir (if (eq? (private:env-mode env-pair) 'fresh)
                            (private:expand-fresh (car env-pair) form)
                            (chez-ir-expand form (car env-pair)))]
                      [rebuilt (chez-ir->annotated-sexp ir)]
                      [t1 (current-time 'time-monotonic)]
                      [ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)])
                    (private:unnegative! document bias)
                    (private:bump-type! (cons #t ms))
                    rebuilt)
                  (begin
                    (private:negative! document bias)
                    #f))))))))))

(define (ir-auto-route:call-site-flows root-library-node document call-node)
  ;;Type-flow EXTRACTION service. NOT on any production path: the type
  ;;side types macros with generator rules over source syntax (type
  ;;router family arms). This entry survives as the DIFFERENTIAL-TEST
  ;;ORACLE for those rules -- its facts are true by construction (Chez's
  ;;own expansion), so tests can assert the hand-written rules cover what
  ;;the IR proves is bound. Returns ((binding-bias . flow) ...),
  ;;vocabulary-neutral positional facts, or #f on any failure (with a
  ;;type-stats miss; see REPLAY_TRACE). root-library-node may be #f
  ;;(source location falls back to the import target / libdirs scan).
  (let* ([head-symbol (index-node-expression (car (index-node-children call-node)))]
      [head-refs (find-available-references-for document call-node head-symbol)]
      [library-identifier
        (and (not (null? head-refs))
          (let ([lib (identifier-reference-library-identifier (car head-refs))])
            (and (not (null? lib)) lib)))])
    (and library-identifier
      (let ([path (uri->path (document-uri document))])
        (guard (e [else
                  (when (getenv "REPLAY_TRACE")
                    (fprintf (console-output-port) "[ir-auto-route] type-flow miss: ~a in ~a at-bias=~a~%"
                      (call-with-string-output-port (lambda (p) (display-condition e p)))
                      (document-uri document) (index-node-start call-node)))
                  (private:bump-type! (cons #f 0))
                  #f])
          (let* ([text (document-text document)]
              [forms (private:forms text path)]
              [import-sets (private:doc-import-sets-cached text forms)]
              [env-pair (private:env document call-node root-library-node import-sets #f head-symbol library-identifier)])
            (and env-pair
              (let* ([form (chez-ir-find-define-containing forms (index-node-start call-node))]
                  [_ (or form (raise 'form-not-found))]
                  [t0 (current-time 'time-monotonic)]
                  [ir (if (eq? (private:env-mode env-pair) 'fresh)
                        (private:expand-fresh (car env-pair) form)
                        (chez-ir-expand form (car env-pair)))]
                  [flows (chez-ir-call-site-type-flows ir path)]
                  [t1 (current-time 'time-monotonic)]
                  [ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)])
              (private:bump-type! (cons (pair? flows) ms))
              flows))))))))

(define (ir-auto-route:try-expand)
  ;; The IR-only rule for the router's ufo-match arm (route 2 retired). On
  ;; ANY failure -- gate off, feature probe failed, env unavailable, miss --
  ;; the call site simply gets no expansion-derived references; the stats and
  ;; REPLAY_TRACE diagnostics carry the reason. root-file-node is accepted
  ;; for the rule-calling convention and unused.
  (lambda (root-file-node root-library-node document index-node)
    (and (ir-auto-route:enabled?)
      (chez-ir-available?)
      (private:try root-file-node root-library-node document index-node))))
)
