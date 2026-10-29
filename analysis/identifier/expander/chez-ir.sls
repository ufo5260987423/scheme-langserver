#!chezscheme
(library (scheme-langserver analysis identifier expander chez-ir)
  ;; The #!chezscheme directive on the first line is required: this file
  ;; uses #%$ kernel primitives, which are rejected in #!r6rs mode, and
  ;; the directive must immediately precede the (library ...) form. No
  ;; other project library needs this, so it stays isolated here.
  (export
    chez-ir-available?
    chez-ir-read-annotated-file
    chez-ir-find-annotated-form
    chez-ir-find-define-containing
    chez-ir-expand
    chez-ir-call-site-facts
    chez-ir-call-site-refs
    chez-ir-call-site-bindings
    chez-ir-call-site-type-flows
    chez-ir->annotated-sexp
    chez-ir-rebuild-budget
    chez-ir-global-lookup?
    chez-ir-contains-quoted-symbol?)
  (import (chezscheme))

;;; chez-ir: extract ground-truth identifier resolution from Chez's own
;;; macro expander via the undocumented (sc-expand x env #t) switch,
;;; which returns the parsed Lsrc IR with per-expression / per-reference
;;; / per-binding source locations.
;;;
;;; See doc/analysis/chez-sc-expand-records-switch.md for the full story.
;;; WARNING: everything below relies on undocumented Chez internals
;;; (sc-expand 3rd argument, #%$record-type-descriptor, nongenerative
;;; record uids, nanopass record type names). Isolate all access here;
;;; a Chez upgrade only breaks this file. The hard "unrecognized record"
;;; error raised by the public record-type-descriptor on kernel records
;;; is NOT catchable (it bypasses the exception system), so we must use
;;; #%$record-type-descriptor everywhere and never touch annotation
;;; records with the public procedure.

;; A user record for the feature self-check only.
(define-record-type private:probe
  (fields a))

;; Probe result is process-lifetime constant -- memoize so per-call-site
;; rule invocations (ir-auto-route:try-expand) do not re-run sc-expand.
;; Concurrent first calls may both probe; the probe is idempotent and the
;; box converges on the same value.
(define private:available-cache (box 'unset))

(define (chez-ir-available?)
  ;; Two probes: the 3-arity sc-expand (argument-count errors are
  ;; catchable), and #%$record-type-descriptor on a user record.
  (let ([cached (unbox private:available-cache)])
    (if (not (eq? cached 'unset))
      cached
      (let ([result
          (and (guard (e [#t #f])
                 (sc-expand '(quote x) (interaction-environment) #t)
                 #t)
            (guard (e [#t #f])
              (eq? 'private:probe
                (record-type-name (#%$record-type-descriptor (make-private:probe 1))))
              #t))])
        (set-box! private:available-cache result)
        result))))

(define (chez-ir-read-annotated-file path)
  ;; Read all top-level forms with annotations. The bfp protocol of
  ;; get-datum/annotations is required for multi-byte characters; the
  ;; first call takes bfp 0, each subsequent call the previous efp.
  (let ([port (open-input-file path)]
      [sfd (make-source-file-descriptor path (open-file-input-port path))])
    (let loop ([bfp 0] [acc '()])
      (let-values ([(datum efp) (get-datum/annotations port sfd bfp)])
        (if (eof-object? datum)
          (begin (close-port port) (reverse acc))
          (loop efp (cons datum acc)))))))

(define (private:strip o)
  (if (annotation? o) (annotation-stripped o) o))

(define (private:deep-strip o)
  ;; get-datum/annotations wraps every node (including atoms) in an
  ;; annotation, so a single strip is not enough for shape comparison.
  (let ([s (private:strip o)])
    (if (pair? s)
      (cons (private:deep-strip (car s)) (private:deep-strip (cdr s)))
      s)))

(define (chez-ir-find-annotated-form predicate annotated)
  ;; Depth-first search over an annotated datum. predicate receives the
  ;; stripped form of each compound pair. Returns the annotated subform
  ;; whose stripped shape satisfies predicate, or #f.
  (let ([seen (make-eq-hashtable)])
    (let search ([o annotated])
      (cond
        [(eq-hashtable-ref seen o #f) #f]
        [(annotation? o) (search (annotation-expression o))]
        [(not (pair? o)) #f]
        [else
          (eq-hashtable-set! seen o #t)
          (let ([stripped (private:deep-strip o)])
            (if (and (pair? stripped) (predicate stripped))
              o
              (let ([here (search (car o))])
                (if here here (search (cdr o))))))]))))

(define (chez-ir-find-define-containing annotated bias)
  ;; Smallest (define ...) form whose source span contains bias, descending by
  ;; span containment. Expanding the enclosing DEFINE -- not the library form --
  ;; matters: library top-level expansion defers definitions to compile-time
  ;; parts and yields an empty runtime IR (measured: sc-expand of a library form
  ;; returns a single Lsrc:quote). Spans are memoized per call (pair spans are
  ;; merged from children; only annotated nodes carry real source objects).
  (let ([memo (make-eq-hashtable)])
    (define (span o)
      (let ([cached (eq-hashtable-ref memo o 'unset)])
        (if (not (eq? cached 'unset))
          cached
          (let ([s
              (cond
                [(annotation? o)
                  (let ([src (annotation-source o)])
                    (and (source-object? src)
                      (cons (source-object-bfp src) (source-object-efp src))))]
                [(pair? o)
                  (let ([a (span (car o))] [d (span (cdr o))])
                    (cond
                      [(and a d) (cons (min (car a) (car d)) (max (cdr a) (cdr d)))]
                      [a]
                      [d]
                      [else #f]))]
                [else #f])])
            (eq-hashtable-set! memo o s)
            s))))
    (define (contains? s)
      (and s (<= (car s) bias) (>= (cdr s) bias)))
    (let loop ([o annotated] [best #f])
      (cond
        [(annotation? o) (loop (annotation-expression o) best)]
        [(not (pair? o)) best]
        [else
          (let* ([s (span o)]
              [here? (and (contains? s)
                      (eq? 'define (private:strip (car o))))]
              [best2 (if here? o best)])
            (let child-loop ([kids o] [b best2])
              (cond
                [(null? kids) b]
                ;improper tail (dotted template syntax like (pat . body)): the
                ;tail is itself a node and may still contain the bias
                [(not (pair? kids)) (loop kids b)]
                [else
                  (child-loop (cdr kids)
                    (if (contains? (span (car kids)))
                      (loop (car kids) b)
                      b))])))]))))

(define chez-ir-expand
  (case-lambda
    [(annotated-datum) (chez-ir-expand annotated-datum (interaction-environment))]
    [(annotated-datum env)
      ;; The switch: records? = #t skips the $uncprep unparsing and
      ;; returns the parsed Lsrc IR with source locations intact.
      (sc-expand annotated-datum env #t)]))

(define (private:ancestor-rtd rtd name)
  (let loop ([rtd rtd])
    (cond
      [(not rtd) #f]
      [(eq? (record-type-name rtd) name) rtd]
      [else (loop (record-type-parent rtd))])))

;;accessor-vector memo: an rtd is immutable, so the memoized vector is
;;deterministic content; the mutex is the bucket-corruption guard (threaded
;;init runs these walkers on worker threads -- cf. the b2fc5b41 weak-ht
;;incident in AGENTS.md).
(define private:accessor-mutex (make-mutex))
(define private:accessor-cache (make-eq-hashtable))

(define (private:accessors-of rtd)
  (with-mutex private:accessor-mutex
    (let ([cached (eq-hashtable-ref private:accessor-cache rtd #f)])
      (or cached
        (let* ([n (vector-length (record-type-field-names rtd))]
            [v (make-vector n)])
          (let loop ([i 0])
            (when (fx< i n)
              (vector-set! v i (record-accessor rtd i))
              (loop (fx+ i 1))))
          (eq-hashtable-set! private:accessor-cache rtd v)
          v)))))

(define (private:field-refs o rtd)
  ;; All field values of a record, guarded (some fields may be
  ;; unreadable in exotic cases; result is discarded anyway).
  (let ([accessors (private:accessors-of rtd)])
    (let loop ([i 0] [acc '()])
      (if (fx= i (vector-length accessors))
        (reverse acc)
        (loop (fx+ i 1)
          (cons (guard (e [#t #f]) ((vector-ref accessors i) o)) acc))))))

;; A source slot in the IR is either a plain source object (ref/set!
;; maybe-src) or an annotated identifier (prelex source), the latter
;; carrying its source one unwrap away.
(define (private:source-object-of src)
  (cond
    [(source-object? src) src]
    [(annotation? src)
      (let ([s (annotation-source src)])
        (and (source-object? s) s))]
    [else #f]))

(define (private:source-in-path? src path)
  (let ([s (private:source-object-of src)])
    (and s
      (string=? path (source-file-descriptor-path (source-object-sfd s))))))

(define (private:source-bias src)
  (let ([s (private:source-object-of src)])
    (and s (source-object-bfp s))))

(define (private:rtd-name-prefix? rtd prefix)
  (let* ([name (symbol->string (record-type-name rtd))]
      [len (string-length name)]
      [plen (string-length prefix)])
    (and (>= len plen) (string=? (substring name 0 plen) prefix))))

(define (private:record-walk ir proc)
  ;;proc: (record rtd) -> boolean; #t stops the walk with #t. Returns #f when
  ;;the whole tree was visited without a #t. When chez-ir-rebuild-budget is
  ;;set (identifier side, 2026-10-29: the broad macro arm exposes walkers to
  ;;arbitrary user macros), the counter fires DURING the walk -- aborting a
  ;;pathological expansion's traversal, not just its reconstruction.
  (let ([budget (chez-ir-rebuild-budget)])
    (define (burn!)
      (when budget
        (set-car! budget (+ (car budget) 1))
        (when (> (car budget) (cdr budget))
          (raise 'expansion-too-large))))
    (let ([seen (make-eq-hashtable)])
      (define (visit o)
        (burn!)
        (cond
          [(pair? o)
            (or (visit (car o)) (visit (cdr o)))]
          [(annotation? o)
            ;annotations wrap IR subtrees (and carry the source positions) --
            ;descend into the wrapped expression
            (visit (annotation-expression o))]
          [(record? o)
            (if (eq-hashtable-ref seen o #f)
              #f
              (begin
                (eq-hashtable-set! seen o #t)
                (let ([rtd (guard (e [#t #f]) (#%$record-type-descriptor o))])
                  (if (not rtd)
                    #f
                    (or (proc o rtd)
                      (let loop ([vals (private:field-refs o rtd)])
                        (cond
                          [(null? vals) #f]
                          [(visit (car vals)) #t]
                          [else (loop (cdr vals))])))))))]
          [else #f]))
      (visit ir))))

(define (private:global-lookup-record? o rtd identifier-string)
  ;;#t when record o is (<expr> ... ($top-level-value '<name>) ...) -- i.e. o
  ;;CALLS the result of a top-level lookup. call fields are (preinfo e0 e1 ...)
  ;;so the operator is the SECOND field.
  (and (private:rtd-name-prefix? rtd "Lsrc:call")
    (let ([vals (private:field-refs o rtd)])
      (and (pair? vals) (pair? (cdr vals))
        (let ([operator (cadr vals)])
          (and (record? operator)
            (let ([op-rtd (guard (e [#t #f]) (#%$record-type-descriptor operator))])
              (and op-rtd
                (private:rtd-name-prefix? op-rtd "Lsrc:call")
                (private:global-lookup-operator? operator op-rtd identifier-string)))))))))

(define (private:global-lookup-operator? operator op-rtd identifier-string)
  ;;inner call fields: (preinfo (primref $top-level-value) ((quote name)) ...)
  ;;the argument tail is stored as a single LIST field
  (let ([op-vals (private:field-refs operator op-rtd)])
    (and (pair? op-vals) (pair? (cdr op-vals)) (pair? (cddr op-vals))
      (pair? (caddr op-vals))
      (let ([head (cadr op-vals)]
          [arg (car (caddr op-vals))])
        (and (record? head) (record? arg)
          (let ([head-rtd (guard (e [#t #f]) (#%$record-type-descriptor head))]
              [arg-rtd (guard (e [#t #f]) (#%$record-type-descriptor arg))])
            (and head-rtd arg-rtd
              (eq? (record-type-name head-rtd) 'primref)
              (private:rtd-name-prefix? arg-rtd "Lsrc:quote")
              (private:global-lookup-parts? head-rtd head arg-rtd arg identifier-string))))))))

(define (private:global-lookup-parts? head-rtd head arg-rtd arg identifier-string)
  (let ([prim-name ((record-accessor head-rtd 0) head)]
      [datum ((record-accessor arg-rtd 0) arg)])
    (and (eq? prim-name '$top-level-value)
      (symbol? datum)
      (string=? (symbol->string datum) identifier-string))))

(define (chez-ir-global-lookup? ir identifier-string)
  ;;#t when ir contains ($top-level-value '<name>) -- i.e. the identifier was
  ;;treated as an UNBOUND GLOBAL (interaction envs are lenient) rather than
  ;;consumed by macro expansion. Chez marks such symbols specially
  ;;(#{match *top*:match}), hence the name-STRING comparison.
  (private:record-walk ir
    (lambda (o rtd)
      (private:global-lookup-record? o rtd identifier-string))))

(define (chez-ir-contains-quoted-symbol? ir symbol)
  ;;#t when ir contains (quote <the interned SYMBOL>). Positive confirmation
  ;;that a probe expansion really engaged the match macro (its failure thunk
  ;;carries (quote match)); complements chez-ir-global-lookup?.
  (private:record-walk ir
    (lambda (o rtd)
      (and (private:rtd-name-prefix? rtd "Lsrc:quote")
        (eq? ((record-accessor rtd 0) o) symbol)))))

(define (private:prelex-name prelex)
  (guard (e [#t '?])
    ((record-accessor (private:ancestor-rtd (#%$record-type-descriptor prelex) 'prelex) 0) prelex)))

(define (private:prelex-binding-bias prelex)
  (guard (e [#t #f])
    (let* ([rtd (#%$record-type-descriptor prelex)]
        [prelex-rtd (private:ancestor-rtd rtd 'prelex)])
      (and prelex-rtd
        (private:source-bias ((record-accessor prelex-rtd 2) prelex))))))

(define (chez-ir-call-site-facts ir path)
  ;; ONE walk collecting BOTH use-driven refs and binding-driven bindings;
  ;; chez-ir-call-site-refs / -bindings below are projections kept for
  ;; existing callers. The walk descends annotation wrappers (record-walk
  ;; semantics -- the old standalone bindings walker did; the old refs
  ;; walker skipped them, a blind spot subsumed here): the emit conditions
  ;; are unchanged, so no previously-collected fact moves, and anything
  ;; extra is still span-filtered downstream.
  ;; Returns (values refs bindings):
  ;;   refs     - ((name ref-bias binding-bias|#f) ...)  every Lsrc:ref
  ;;              whose own source is in `path` (code copied verbatim from
  ;;              the call site, as opposed to macro-introduced code whose
  ;;              source is the macro definition file or #f);
  ;;              binding-bias is #f when the binding is macro-introduced
  ;;   bindings - ((name binding-bias) ...), de-duplicated; covers
  ;;              never-used bindings (an unused match pattern variable
  ;;              produces no Lsrc:ref -- the prelex still survives in the
  ;;              IR with source at the pattern position; measured).
  ;;              Macro-generated temporaries carry no source and
  ;;              template-introduced bindings point at the macro
  ;;              definition file, so the path check excludes both.
  (let ([refs '()]
      [bindings '()]
      [seen-binding-keys (make-hashtable equal-hash equal?)])
    (private:record-walk ir
      (lambda (o rtd)
        (cond
          [(and (private:rtd-name-prefix? rtd "Lsrc:ref")
              (guard (e [#t #f])
                (equal? '#(maybe-src x) (record-type-field-names rtd))))
           ;; fields: (maybe-src x)
           (let* ([vals (private:field-refs o rtd)]
                  [src (car vals)]
                  [prelex (cadr vals)])
             (when (private:source-in-path? src path)
               (set! refs
                 (cons (list (private:prelex-name prelex)
                         (private:source-bias src)
                         (private:prelex-binding-bias prelex))
                   refs)))
             #f)]
          [else
            (let ([prelex-rtd (private:ancestor-rtd rtd 'prelex)])
              (when prelex-rtd
                (let ([src ((record-accessor prelex-rtd 2) o)])
                  (when (private:source-in-path? src path)
                    (let* ([name (guard (e [#t '?])
                                  ((record-accessor prelex-rtd 0) o))]
                        [bias (private:source-bias src)]
                        [key (cons name bias)])
                      (unless (hashtable-ref seen-binding-keys key #f)
                        (hashtable-set! seen-binding-keys key #t)
                        (set! bindings (cons `(,name ,bias) bindings)))))))
              #f)])))
    (values (reverse refs) (reverse bindings))))

(define (chez-ir-call-site-refs ir path)
  ;;use-driven projection of chez-ir-call-site-facts
  (let-values ([(refs _) (chez-ir-call-site-facts ir path)])
    refs))

(define (chez-ir-call-site-bindings ir path)
  ;;binding-driven projection of chez-ir-call-site-facts
  (let-values ([(_ bindings) (chez-ir-call-site-facts ir path)])
    bindings))

;;;Rebuild node budget for chez-ir->annotated-sexp: #f = unlimited
;;;(identifier side, oracle); otherwise a MUTABLE PAIR (count . limit)
;;;owned by the caller. rebuild() increments per IR node visited and
;;;raises 'expansion-too-large past the limit -- the type side sets this
;;;to abort PATHOLOGICAL expansions EARLY (measured 2026-10-28: one
;;;match-index-node call inside root-ancestor expands to 3.5 MILLION
;;;core-form nodes; counting after the build still grows the Chez heap
;;;by GBs per occurrence, so the cap must fire DURING the rebuild).
;;;A parameter (not a global counter) keeps concurrent rebuilds on
;;;worker threads independent.
(define chez-ir-rebuild-budget (make-parameter #f))

;rebuild dispatch-tag memo: an rtd is immutable, so the tag is
;deterministic content; the mutex is the bucket-corruption guard (see
;private:accessors-of).
(define private:rebuild-tag-mutex (make-mutex))
(define private:rebuild-tag-cache (make-eq-hashtable))

(define (private:rebuild-tag rtd)
  ;;tag precedence mirrors the rebuild cond chain this replaces; the Lsrc:
  ;;prefixes are mutually exclusive record families and primref / prelex are
  ;;disjoint from all of them, so the order only documents intent
  (with-mutex private:rebuild-tag-mutex
    (let ([cached (eq-hashtable-ref private:rebuild-tag-cache rtd #f)])
      (or cached
        (let ([tag
            (cond
              [(private:rtd-name-prefix? rtd "Lsrc:call") 'call]
              [(private:rtd-name-prefix? rtd "Lsrc:ref") 'ref]
              [(private:rtd-name-prefix? rtd "Lsrc:set!") 'set!]
              [(private:rtd-name-prefix? rtd "Lsrc:quote") 'quote]
              [(private:rtd-name-prefix? rtd "Lsrc:if") 'if]
              [(private:rtd-name-prefix? rtd "Lsrc:seq") 'seq]
              [(private:rtd-name-prefix? rtd "Lsrc:case-lambda") 'case-lambda]
              [(private:rtd-name-prefix? rtd "Lsrc:letrec") 'letrec]
              [(eq? (record-type-name rtd) 'primref) 'primref]
              [(private:ancestor-rtd rtd 'prelex) 'prelex]
              [else 'unknown])])
          (eq-hashtable-set! private:rebuild-tag-cache rtd tag)
          tag)))))

;;hoisted from chez-ir->annotated-sexp's internal defines (lightweight
;;refactor): stateless shape helpers shared by the rebuild cluster and
;;the walkers. private:prelex-name already lives at file level (facts
;;walker); its guard behavior matches the old internal copy.
(define (private:unwrap-ann o)
  ;;ONE annotation layer off (contrast the type-flows walker's multi-layer
  ;;unwrap, which is deliberately left internal to that function)
  (if (annotation? o) (annotation-expression o) o))

(define (private:rtd-of o)
  (and (record? o) (guard (e [#t #f]) (#%$record-type-descriptor o))))

(define (private:case-lambda-record? o)
  (let ([rtd (private:rtd-of (private:unwrap-ann o))])
    (and rtd (private:rtd-name-prefix? rtd "Lsrc:case-lambda"))))

(define (private:prelex-src px)
  ;;raw source slot of a prelex (private:prelex-binding-bias projects the
  ;;bias out of the same slot; both guard to #f)
  (guard (e [#t #f])
    ((record-accessor (private:ancestor-rtd (private:rtd-of px) 'prelex) 2) px)))

(define (chez-ir->annotated-sexp ir)
  ;;Reconstruct a readable ANNOTATED s-expression tree from the parsed
  ;;Lsrc IR (the undocumented records?=#t output). This is a source-
  ;;restoring counterpart of $uncprep: structure per record type, source
  ;;positions re-attached from preinfo.src / ref maybe-src / prelex.source,
  ;;primrefs reduced to their names, prelexes to theirs (user identifiers
  ;;keep their original names). Used to feed Chez's own macro EXPANSIONS
  ;;to the langserver's type generator: the default (records?=#f) expand
  ;;output carries NO annotations (measured: $uncprep strips every one),
  ;;so positions must come from the IR. Unknown shapes degrade to (void).
  ;;
  ;;Every reconstructed node is wrapped in an annotation whose source is
  ;;real when known and a shared dummy otherwise: the index-node builder
  ;;requires every node to carry a source object (measured) and drops non-
  ;;annotation children. Single-clause case-lambda applications are beta-
  ;;reduced into LETs -- including applications through let/letrec-bound
  ;;names (Chez lifts clause lambdas; measured) -- because the type
  ;;generator's let rule surfaces the binding-to-init value flow that
  ;;case-lambda application does not.
  (define dummy-sfd
    (make-source-file-descriptor "<macro-expansion>"
      (open-bytevector-input-port #vu8())))
  (define (mk-ann src sexpr)
    (make-annotation sexpr
      (or (and src (private:source-object-of src))
          (make-source-object dummy-sfd 0 0))
      sexpr))
  (define (rebuild-atom x)
    (let ([x0 (private:unwrap-ann x)])
      (cond
        [(not (record? x0)) (if (annotation? x) x (mk-ann #f x0))]
        [(private:ancestor-rtd (private:rtd-of x0) 'prelex)
          (mk-ann (private:prelex-src x0) (private:prelex-name x0))]
        [(eq? (record-type-name (private:rtd-of x0)) 'primref)
          (mk-ann #f (or (find symbol? (private:field-refs x0 (private:rtd-of x0))) 'void))]
        [else (mk-ann #f 'void)])))
  (define (rebuild o env)
    ;;budget check FIRST: the caller's limit aborts pathological
    ;;expansions while the tree is still small (see
    ;;chez-ir-rebuild-budget); the raise unwinds to the caller's guard
    (let ([b (chez-ir-rebuild-budget)])
      (when b
        (set-car! b (+ (car b) 1))
        (when (> (car b) (cdr b))
          (raise 'expansion-too-large))))
    (let ([o0 (private:unwrap-ann o)])
      (cond
        ;;bare values must stay annotated: the tree builder drops non-
        ;;annotation children; an already-annotated input passes through
        [(not (record? o0))
          (if (annotation? o) o (mk-ann #f o0))]
        [else
          (let ([rtd (private:rtd-of o0)])
            (if (not rtd)
              (mk-ann #f '(void))
              (case (private:rebuild-tag rtd)
                [(call)
                (rebuild-call o0 rtd env)]
                [(ref)
                (let* ([vals (private:field-refs o0 rtd)]
                    [atom (rebuild-atom (cadr vals))])
                  ;;rebuild-atom may already return an annotation (chi wraps
                  ;;prelexes in annotations); re-wrapping nests annotations
                  ;;and the builder then makes a childless shell whose inner
                  ;;symbol never becomes a node (measured: the w reference
                  ;;in (car w) -- the type chain died there)
                  (if (and (annotation? atom)
                          (not (equal? (annotation-source atom)
                                (and (car vals)
                                  (private:source-object-of (car vals))))))
                    atom
                    (mk-ann (car vals) atom)))]
                [(set!)
                (let ([vals (private:field-refs o0 rtd)])
                  (mk-ann (car vals)
                    (list (mk-ann #f 'set!)
                      (rebuild-atom (cadr vals))
                      (rebuild (caddr vals) env))))]
                [(quote)
                (mk-ann #f
                  (list 'quote (guard (e [#f]) ((record-accessor rtd 0) o0))))]
                [(if)
                (let ([vals (private:field-refs o0 rtd)])
                  (mk-ann (private:preinfo-src o0)
                    (list (mk-ann #f 'if)
                      (rebuild (car vals) env)
                      (rebuild (cadr vals) env)
                      (rebuild (caddr vals) env))))]
                [(seq)
                (let ([vals (private:field-refs o0 rtd)])
                  (mk-ann (private:preinfo-src o0)
                    (cons (mk-ann #f 'begin)
                      (map (lambda (v) (rebuild v env)) vals))))]
                [(case-lambda)
                (rebuild-case-lambda o0 rtd env)]
                [(letrec)
                (rebuild-letrec o0 rtd env)]
                [(primref)
                (mk-ann (private:preinfo-src o0)
                  (or (find symbol? (private:field-refs o0 rtd)) 'void))]
                [(prelex)
                (mk-ann (private:preinfo-src o0) (private:prelex-name o0))]
                [else (mk-ann #f '(void))])))]
            ;;closes rebuild's outer cond, let-o0 and let-b
            ;;closes let-o0 and the rebuild define (outer cond closed above)
            )))
  (define (rebuild-call o rtd env)
    (let* ([vals (private:field-refs o rtd)]
        [src (private:preinfo-src o)]
        [op (cadr vals)]
        [args (caddr vals)]
        [op0 (private:unwrap-ann op)]
        [direct
          (and (private:case-lambda-record? op0)
            (beta-reduce src op0 args env))]
        [via-name
          (and (not direct)
            (let ([op-rtd (private:rtd-of op0)])
              (and op-rtd
                ;;look THROUGH a ref record to the prelex it names
                (let* ([px
                        (if (private:rtd-name-prefix? op-rtd "Lsrc:ref")
                          (private:unwrap-ann
                            (cadr (private:field-refs op0 op-rtd)))
                          op0)]
                    [px-rtd (private:rtd-of px)])
                  (and px-rtd
                    (private:ancestor-rtd px-rtd 'prelex)
                    (let ([target (eq-hashtable-ref env px #f)])
                      (if target
                        (beta-reduce src target args (make-eq-hashtable))
                        #f)))))))])
      (or direct
        via-name
        (mk-ann src
          (cons (rebuild op env)
            (map (lambda (a) (rebuild a env)) args))))))
  (define (beta-reduce src op args inner-env)
    ;;an application of a case-lambda (inline or through a name) becomes
    ;;a LET whose bindings are the selected clause's formals paired with
    ;;the arguments -- arity-based clause selection, like a real
    ;;application. Clause lambdas may be MULTI-clause (measured: ufo-
    ;;match generates them), so the single-clause restriction is lifted.
    (let* ([op-rtd (private:rtd-of op)]
        [op-vals (and op-rtd (private:field-refs op op-rtd))]
        [clauses (and op-vals (cadr op-vals))]
        [matching
          (and (list? clauses)
            (filter
              (lambda (clause)
                (let ([c-rtd (private:rtd-of clause)])
                  (and c-rtd
                    (let* ([c-vals (private:field-refs clause c-rtd)]
                        [formals (map rebuild-atom (car c-vals))])
                      (and (list? formals)
                        (= (length formals) (length args))
                        (for-all
                          (lambda (f) (symbol? (private:unwrap-ann f)))
                          formals))))))
              clauses))])
      (if (= 1 (length matching))
        (let* ([clause (car matching)]
            [c-rtd (private:rtd-of clause)]
            [c-vals (private:field-refs clause c-rtd)]
            [formals (map rebuild-atom (car c-vals))]
            [formal-prelexes (car c-vals)]
            ;;the beta-let's own bindings feed the body's name
            ;;resolution: a binding whose init is a case-lambda makes
            ;;the name applicable (Chez lifts clause lambdas through
            ;;several such lets -- measured)
            [body-env (make-eq-hashtable)])
          (vector-for-each
            (lambda (k)
              (eq-hashtable-set! body-env k (eq-hashtable-ref inner-env k #f)))
            (hashtable-keys inner-env))
          (for-each
            (lambda (px a)
              (let ([a0 (private:unwrap-ann a)])
                (when (private:case-lambda-record? a0)
                  (eq-hashtable-set! body-env (private:unwrap-ann px) a0))))
            formal-prelexes args)
          (mk-ann src
            (list (mk-ann #f 'let)
              (mk-ann #f
                (map (lambda (f a)
                       (mk-ann #f (list f (rebuild a inner-env))))
                  formals args))
              (rebuild (caddr c-vals) body-env))))
        #f)))
  (define (rebuild-case-lambda o rtd env)
    (let ([vals (private:field-refs o rtd)])
      (mk-ann (private:preinfo-src o)
        (cons (mk-ann #f 'case-lambda)
          (map
            (lambda (clause)
              (let ([c-rtd (private:rtd-of clause)])
                (mk-ann #f
                  (if c-rtd
                    (let ([c-vals (private:field-refs clause c-rtd)])
                      (list (mk-ann #f (map rebuild-atom (car c-vals)))
                        (rebuild (caddr c-vals) env)))
                    '(args (void))))))
            (cadr vals))))))
  (define (rebuild-letrec o rtd env)
    (let* ([vals (private:field-refs o rtd)]
        [xs (car vals)]
        [es (cadr vals)]
        [body (caddr vals)]
        [child-env (make-eq-hashtable)])
      (vector-for-each
        (lambda (k) (eq-hashtable-set! child-env k (eq-hashtable-ref env k #f)))
        (hashtable-keys env))
      ;;lambda-valued bindings are visible to the body through their
      ;;names (clause lambdas are applied via refs -- see rebuild-call)
      (for-each
        (lambda (x e)
          (let ([e0 (private:unwrap-ann e)])
            (when (private:case-lambda-record? e0)
              (eq-hashtable-set! child-env (private:unwrap-ann x) e0))))
        xs es)
      (mk-ann (private:preinfo-src o)
        (list (mk-ann #f 'letrec)
          (mk-ann #f
            (map (lambda (x e)
                   (mk-ann #f (list (rebuild-atom x) (rebuild e env))))
              xs es))
          (rebuild body child-env)))))
  (rebuild ir (make-eq-hashtable)))

(define (private:cxr->steps op)
  ;;cadr -> (car cdr) etc, OUTERMOST first (c[a[d]r]: left-to-right
  ;;letters, a=car d=cdr). #f for non-cxr symbols.
  (let ([s (symbol->string op)])
    (if (and (>= (string-length s) 3)
          (char=? (string-ref s 0) #\c)
          (char=? (string-ref s (- (string-length s) 1)) #\r))
      (let ([middle (substring s 1 (- (string-length s) 1))])
        (if (and (positive? (string-length middle))
              (for-all (lambda (ch) (memv ch '(#\a #\d))) (string->list middle)))
          (map (lambda (ch) (if (char=? ch #\a) 'car 'cdr))
            (string->list middle))
          #f))
      #f)))

(define (private:preinfo-src preinfo)
  ;;preinfo records carry the expression source in field 0 (preinfo =
  ;;(src sexpr), parent of preinfo-lambda/preinfo-call)
  (let ([rtd (and (record? preinfo)
              (guard (e [#t #f]) (#%$record-type-descriptor preinfo)))])
    (and rtd
      (let ([p-rtd (private:ancestor-rtd rtd 'preinfo)])
        (and p-rtd
          (guard (e [#t #f])
            ((record-accessor p-rtd 0) preinfo)))))))

(define (chez-ir-call-site-type-flows ir path)
  ;; TYPE-FLOW ground truth for macro call sites. ufo-match-style macros
  ;; expand into cascades of case-lambda applications (failure-continuation
  ;; chaining): pattern variables are CLAUSE FORMALS, fed positionally by
  ;; accessor chains (car/cdr/...) over temporaries that bottom out at the
  ;; scrutinee. This walker recovers that flow WITHOUT any pattern-syntax
  ;; knowledge -- any syntax-case macro whose expansion binds user
  ;; identifiers through case-lambda/letrec applications gets typed.
  ;;
  ;; Returns ((binding-bias . flow) ...), one per user-source binding:
  ;;   flow = (ref . bias)       the argument is verbatim user code at
  ;;                             `bias` (typically the scrutinee)
  ;;   flow = (chain op . flow)  (op argument), op in {car, cdr}; cxr
  ;;                             accessors are decomposed (cadr ->
  ;;                             (chain car (chain cdr ...)))
  ;; Bindings whose arguments do not resolve to user code (macro-internal
  ;; computations) are omitted.
  (let ([bindings (make-eq-hashtable)]
      [binding-order '()]
      [resolved (make-eq-hashtable)]
      [resolving (make-eq-hashtable)]
      [result '()])
    (define (unwrap o) (if (annotation? o) (unwrap (annotation-expression o)) o))
    (define (prelex-rtd-of px)
      (private:ancestor-rtd (guard (e [#t #f]) (#%$record-type-descriptor px)) 'prelex))
    (define (add-binding! lhs rhs)
      (when (and (record? lhs)
              (prelex-rtd-of lhs)
              (not (eq-hashtable-ref bindings lhs #f)))
        (eq-hashtable-set! bindings lhs rhs)
        (set! binding-order (cons (cons lhs rhs) binding-order))))
    (define (resolve-prelex px)
      (let ([hit (eq-hashtable-ref resolved px 'missing)])
        (cond
          [(not (eq? hit 'missing)) hit]
          [(eq-hashtable-ref resolving px #f) #f]
          [else
            (eq-hashtable-set! resolving px #t)
            (let* ([prelex-rtd (prelex-rtd-of px)]
                [src ((record-accessor prelex-rtd 2) px)]
                [flow
                  (if (private:source-in-path? src path)
                    (let ([b (private:source-bias src)])
                      (and b `(ref . ,b)))
                    (let ([rhs (eq-hashtable-ref bindings px #f)])
                      (and rhs (resolve-expr rhs))))])
              (eq-hashtable-set! resolved px flow)
              flow)])))
    (define (resolve-expr o)
      (let ([o (unwrap o)])
        (cond
          [(record? o)
            (let ([rtd (guard (e [#t #f]) (#%$record-type-descriptor o))])
              (cond
                [(not rtd) #f]
                [(private:rtd-name-prefix? rtd "Lsrc:ref")
                  (let* ([vals (private:field-refs o rtd)]
                      [x (unwrap (cadr vals))])
                    (if (and (record? x) (prelex-rtd-of x))
                      (resolve-prelex x)
                      #f))]
                [(private:rtd-name-prefix? rtd "Lsrc:call")
                  (let* ([vals (private:field-refs o rtd)]
                      [op (unwrap (cadr vals))]
                      [args (caddr vals)]
                      [op-rtd (and (record? op)
                                (guard (e [#t #f]) (#%$record-type-descriptor op)))]
                      [op-name (and op-rtd
                                 (eq? (record-type-name op-rtd) 'primref)
                                 (find symbol? (private:field-refs op op-rtd)))]
                      ;;computed ONCE -- the old guard/body pair evaluated
                      ;;cxr->steps twice per accessor call (short-circuit:
                      ;;op-name may be #f or a non-symbol)
                      [steps (and (symbol? op-name)
                               (private:cxr->steps op-name))])
                    (cond
                      ;;(car x)/(cadr x)/... accessor chain over the base
                      [(and steps
                          (pair? args)
                          (null? (cdr args)))
                        (let ([base (resolve-expr (car args))])
                          (and base
                            (fold-left
                              (lambda (flow step) `(chain ,step . ,flow))
                              base
                              steps)))]
                      ;;driver/cascader application (case-lambda operator):
                      ;;the threaded value is among the arguments -- when
                      ;;exactly one resolves, that IS the flow (the outermost
                      ;;driver passes the scrutinee itself)
                      [(and op-rtd (private:rtd-name-prefix? op-rtd "Lsrc:case-lambda"))
                        (let ([flows (filter values (map resolve-expr args))])
                          (cond
                            [(= 1 (length flows)) (car flows)]
                            ;;the value the driver threads is verbatim user
                            ;;code whose expansion carried no source (e.g. a
                            ;;(list 1 2) scrutinee): anchor at the call
                            ;;site's scrutinee argument, filled in by the
                            ;;caller (the 'base marker)
                            [(null? flows) 'base]
                            [else #f]))]
                      ;;any other verbatim call (a (list 1 2) scrutinee, a user
                      ;;(f x) application, ...) is its own flow base
                      [(and (pair? vals)
                          (private:source-in-path? (private:preinfo-src (car vals)) path))
                        (let ([b (private:source-bias (private:preinfo-src (car vals)))])
                          (and b `(ref . ,b)))]
                      ;;no own source (template reconstruction of verbatim
                      ;;user code): anchor at the call-site scrutinee
                      [else 'base]))]
                [else
                  (cond
                    ;;function/definition forms are NOT value flows: their
                    ;;"source" is the enclosing binding form (measured: a
                    ;;define's case-lambda rhs resolves to (ref . 0), the
                    ;;define keyword itself -- noise substitutions)
                    [(private:rtd-name-prefix? rtd "Lsrc:case-lambda") #f]
                    [(private:rtd-name-prefix? rtd "Lsrc:lambda") #f]
                    ;;verbatim user code with its own source (typically the
                    ;;scrutinee): anchored at that source position
                    [(and (pair? (private:field-refs o rtd))
                        (let ([src0 (private:preinfo-src (car (private:field-refs o rtd)))])
                          (and src0 (private:source-in-path? src0 path))))
                      (let ([b (private:source-bias
                                (private:preinfo-src (car (private:field-refs o rtd))))])
                        (and b `(ref . ,b)))]
                    ;;a literal scrutinee ('sym, 5, ...): quotes carry no
                    ;;source even when user-written -- anchor at the
                    ;;call-site scrutinee argument (the 'base marker)
                    [(private:rtd-name-prefix? rtd "Lsrc:quote") 'base]
                    [else 'base])]))]
          [else #f])))
    ;; pass 1: collect raw bindings (prelex -> rhs expression)
    (private:record-walk ir
      (lambda (o rtd)
        (cond
          [(private:rtd-name-prefix? rtd "Lsrc:letrec")
            ;; fields (x* e* body): parallel lists, zip positionally
            (let ([vals (private:field-refs o rtd)])
              (when (and (pair? vals) (pair? (cdr vals)))
                (let ([xs (car vals)] [es (cadr vals)])
                  (when (and (list? xs) (list? es))
                    (for-each add-binding! xs es)))))
            #f]
          [(private:rtd-name-prefix? rtd "Lsrc:call")
            ;; operator case-lambda -> clause formals fed by the args
            (let ([vals (private:field-refs o rtd)])
              (when (and (pair? vals) (pair? (cdr vals)) (pair? (cddr vals)))
                (let ([op (unwrap (cadr vals))]
                    [args (caddr vals)])
                  (when (and (record? op)
                          (let ([op-rtd (guard (e [#t #f]) (#%$record-type-descriptor op))])
                            (and op-rtd (private:rtd-name-prefix? op-rtd "Lsrc:case-lambda"))))
                    (let ([op-rtd (#%$record-type-descriptor op)])
                      ;; case-lambda fields (preinfo cl): cl = clause list;
                      ;; clause fields (x* interface body)
                      (let ([clauses (let ([op-vals (private:field-refs op op-rtd)])
                                      (and (pair? op-vals) (pair? (cdr op-vals))
                                        (cadr op-vals)))])
                        (when (list? clauses)
                          (for-each
                            (lambda (clause)
                              (let ([c-rtd (and (record? clause)
                                             (guard (e [#t #f]) (#%$record-type-descriptor clause)))])
                                (when c-rtd
                                  (let ([c-vals (private:field-refs clause c-rtd)])
                                    (when (and (pair? c-vals) (list? (car c-vals)))
                                      (let ([formals (car c-vals)])
                                        ;; proper formals only: a dotted
                                        ;; rest-arg pair stops the zip
                                        (let loop ([fs formals] [as args])
                                          (when (and (pair? fs) (pair? as))
                                            (add-binding! (unwrap (car fs)) (car as))
                                            (loop (cdr fs) (cdr as))))))))))
                            clauses))))))))
            #f]
          [else #f])))
    ;; pass 2: resolve and emit user-source bindings
    (for-each
      (lambda (entry)
        (let* ([lhs (car entry)]
            [rhs (cdr entry)]
            [prelex-rtd (prelex-rtd-of lhs)])
          (when prelex-rtd
            (let ([src ((record-accessor prelex-rtd 2) lhs)])
              (when (private:source-in-path? src path)
                (let* ([bias (private:source-bias src)]
                    [flow (resolve-expr rhs)])
                  (when (and bias flow)
                    (set! result (cons (cons bias flow) result)))))))))
      binding-order)
    (reverse result)))
)
