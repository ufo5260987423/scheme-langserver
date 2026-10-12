(library (scheme-langserver analysis identifier reference)
  (export
    find-available-references-for
    find-references-in
    guard-for

    meta?
    meta-for?

    append-references-into-ordered-references-for

    identifier-reference?
    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-type-expressions
    identifier-reference-type
    identifier-reference-parents
    identifier-reference-parents-set!
    identifier-reference-type-expressions-set!
    identifier-reference-index-node
    identifier-reference-initialization-index-node
    identifier-reference-top-environment
    identifier-reference-syntax-expander
    identifier-reference-syntax-expander-set!
    identifier-reference-usage-count
    identifier-reference-usage-count-set!
    identifier-reference-speculative?
    identifier-reference-speculative?-set!

    identifier-compare?

    root-ancestor

    sort-identifier-references
    library-identifier?)
  (import
    (chezscheme)

    (ufo-match-steer)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver util contain)
    (scheme-langserver util merge-ordered-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (nongenerative scheme-langserver-identifier-reference)
  (fields
    (immutable identifier)
    (immutable document)
    (immutable index-node)
    (immutable initialization-index-node)

    (immutable library-identifier)
    (immutable type)
    ;parent can be used for two cases: (1) rename/prefix a identifier-reference in library importion/exportion (2) record-type inherent
    (mutable parents)
    ;;each type-expression is an alist consists of identifier-references and 'or 'something? 'inner:void? ...
    ;;NOTE: it must be index-node's type expression collection, because of case-lambda
    (mutable type-expressions)
    (mutable top-environment)
    (mutable syntax-expander)
    (mutable usage-count)
    ;#t when this reference was back-propagated from a macro expansion (tentative-level knowledge, cf. Culpepper 2010): consumers that need
    ;definite bindings should treat these with suspicion
    (mutable speculative?))
  (protocol
    (lambda (new)
      (case-lambda
        [(identifier document index-node initialization-index-node library-identifier type parents type-expressions)
          (new identifier document index-node initialization-index-node library-identifier type parents type-expressions '() #f 0 #f)]
        [(identifier document index-node initialization-index-node library-identifier type parents type-expressions top-environment)
          (new identifier document index-node initialization-index-node library-identifier type parents type-expressions top-environment #f 0 #f)]))))

(define (meta? identifier) (not (null? (identifier-reference-top-environment identifier))))
(define (meta-for? document index-node identifier)
  (let ([e (annotation-stripped (index-node-datum/annotations index-node))])
    (if (symbol? e)
      (let* ([as (find-available-references-for document index-node e)]
          [ras (fold-right append '() (map root-ancestor as))]
          [metas (filter meta? ras)])
        (find (lambda (i) (eq? identifier (identifier-reference-identifier i))) metas)))))

;symbol->string allocates a fresh string on every call, and identifier-compare? is invoked tens of millions of times during workspace
;init.  Cache each interned symbol's name string.  Per-thread: init-references runs under threaded-map, and a shared mutable hashtable risks
;the bucket corruption that once killed private:expander-doc-cache-ht (nonrecoverable invalid memory reference).  thread-parameter values are
;inherited by forked threads, so the cell is tagged with its owner thread and re-created on mismatch -- a #f-or-shared cell is never reused.
(define private:identifier-name-cache (make-thread-parameter #f)) ;plain eq-hashtable: symbols are interned (never collected)

(define (private:my-name-cache)
  (let ([cell (private:identifier-name-cache)])
    (if (and cell (eqv? (car cell) (get-thread-id)))
      (cdr cell)
      (let ([fresh (cons (get-thread-id) (make-eq-hashtable))])
        (private:identifier-name-cache fresh)
        (cdr fresh)))))

(define (private:identifier-name identifier)
  (let ([cache (private:my-name-cache)])
    (or (eq-hashtable-ref cache identifier #f)
      (let ([name (symbol->string identifier)])
        (eq-hashtable-set! cache identifier name)
        name))))

(define (identifier-compare? target1 target2)
  (let ([id1 (identifier-reference-identifier target1)]
      [id2 (identifier-reference-identifier target2)])
    ;eq? first: binary-search probes mostly land on the equal element, and interned symbols hit eq? far more often than the name compare.
    (or (eq? id1 id2)
      (and (symbol? id1) (symbol? id2)
        (string<=? (private:identifier-name id1) (private:identifier-name id2))))))

;Batch maintenance for reference lists.  The dominant call shape is a SINGLE fresh reference from a
;binding rule (define/let/lambda/...): sort+dedupe is skipped and private:insert-one-into-ordered walks
;the sorted spine once (one cons per preceding element, tail shared) -- merge-ordered-lists' per-run
;split+reverse+append triple-touches every run on the way, measured at 1.85s init-wide (2026-10-11).
;Multi-element batches keep the merge path; see merge-ordered-list.sls for the semantics contract.
(define (append-references-into-ordered-references-for document index-node list)
  (cond
    [(null? list) (void)]
    [(null? (cdr list))
      (let ([new-list
              (private:insert-one-into-ordered
                (car list)
                (if (null? index-node)
                  (document-ordered-reference-list document)
                  (index-node-references-import-in-this-node index-node))
                identifier-compare? equal?)])
        (if (null? index-node)
          (document-ordered-reference-list-set! document new-list)
          (index-node-references-import-in-this-node-set! index-node new-list)))]
    [else
      (let ([sorted-batch (dedupe-adjacent (sort-identifier-references (reverse list)) equal?)])
        (if (null? index-node)
          (document-ordered-reference-list-set! document
            (merge-ordered-lists (document-ordered-reference-list document) sorted-batch identifier-compare? equal?))
          (index-node-references-import-in-this-node-set! index-node
            (merge-ordered-lists (index-node-references-import-in-this-node index-node) sorted-batch identifier-compare? equal?))))]))

;Insert a single reference into a sorted+deduped list.  Walks past every element that sorts strictly
;before the newcomer (consing into a reversed prefix), then anchors before the first element of its
;compare-run: elements of that run which are eq?/equal? to the newcomer are dropped (batch-first anchor
;semantics, matching merge-same-run's drop-prefix-equal; the list is deduped, so at most one), the rest
;of the run and the untouched tail are shared.
(define (private:insert-one-into-ordered reference old compare? equal-procedure)
  (let loop ([rest old] [rev-prefix '()])
    (cond
      [(null? rest) (cons reference (reverse rev-prefix))]
      [(compare? reference (car rest))
        (fold-left (lambda (acc x) (cons x acc))
          (if (compare? (car rest) reference)
            (cons reference (private:drop-prefix-equal reference rest equal-procedure))
            (cons reference rest))
          rev-prefix)]
      [else (loop (cdr rest) (cons (car rest) rev-prefix))])))

;matching merge-ordered-lists' drop-prefix-equal: drop the leading run of elements eq?/equal? to target
(define (private:drop-prefix-equal target lst equal-procedure)
  (cond
    [(null? lst) '()]
    [(or (eq? target (car lst)) (equal-procedure target (car lst)))
      (private:drop-prefix-equal target (cdr lst) equal-procedure)]
    [else lst]))

(define (sort-identifier-references identifier-references)
  (sort identifier-compare? identifier-references))

(define (guard-for document current-index-node target-identifier . library-identifier-rest)
  (let ([candidates (find-available-references-for document current-index-node target-identifier)])
    (if (private-check-library-identifier? candidates library-identifier-rest)
      #t
      (let loop ([body
            (filter (lambda (identifier-reference) (not (null? identifier-reference)))
              (fold-left append '() (map identifier-reference-parents (find-available-references-for document current-index-node))))])
        (if (null? body)
          (raise "no such identifier for specific libraries")
          (if (private-check-library-identifier? body library-identifier-rest)
            #t
            (loop
              (filter (lambda (identifier-reference) (not (null? identifier-reference)))
                (fold-right append '() (map identifier-reference-parents body))))))))))

(define (private-check-library-identifier? candidates library-identifier-rest)
  (if (null? candidates)
    #f
    (let ([candidate (car candidates)])
      (if (null? library-identifier-rest)
        candidate
        (if (find (lambda (r) (equal? r (identifier-reference-library-identifier candidate))) library-identifier-rest)
        candidate
          #f)))))

(define (library-identifier? _document index-node)
  (let* ([parent (index-node-parent index-node)]
      [check?
        (lambda ()
          (let ([grandparent (index-node-parent parent)]
              [sibling (index-node-children parent)]
              [bigest-sibling (car (index-node-children parent))])
            (if (null? grandparent)
              #f
              (and
                (not (eq? bigest-sibling index-node))
                (contain? sibling index-node eq?)
                (match-index-node grandparent
                  [('library . rest) (not (eq? (cadr (index-node-children grandparent)) parent))]
                  [('define-library . rest) (not (eq? (cadr (index-node-children grandparent)) parent))]
                  [else #f])))))])
    (if (null? parent)
      #f
      (match-index-node parent
        [('library identifier-node . rest)
          (and
            (eq? identifier-node index-node)
            (not (check?)))]
        [('define-library identifier-node . rest)
          (and
            (eq? identifier-node index-node)
            (not (check?)))]
        [('import identifier-node . rest) (check?)]
        [('only identifier-node . rest) (check?)]
        [('rename identifier-node . rest) (check?)]
        [('prefix identifier-node . rest) (check?)]
        [('except identifier-node . rest) (check?)]
        [('alias identifier-node . rest) (check?)]
        [else #f]))))

(define (private:list->eq-set lst)
  (let ([ht (make-eq-hashtable)])
    (for-each (lambda (item) (eq-hashtable-set! ht item #t)) lst)
    ht))

;the root-level (filter (not-in local-ht) document-ordered-reference-list) sub-result of the 2-arity
;find-available-references-for variant is identical for every no-symbol lookup in the same document
;while the root's local list and the document ordered list keep their identity. Both are replaced
;functionally on every update (no set-car!/set-cdr!, same invariant as sorted-list-vector-cache in
;b2fc5b41), so an identity check is a sound validity test. Excluded-refs re-did this filter ~3k times
;per init (330 us each, ~1 s); with the cache it happens once per document. Mutex: MT workers on
;different documents share the cache table.
(define private:document-tail-cache (make-eq-hashtable))
(define private:document-tail-cache-mutex (make-mutex))
(define (private:document-tail document local)
  (let ([doc-list (document-ordered-reference-list document)])
    (with-mutex private:document-tail-cache-mutex
      (let ([entry (eq-hashtable-ref private:document-tail-cache document #f)])
        (if (and entry (eq? (car entry) local) (eq? (cadr entry) doc-list))
          (cddr entry)
          (let* ([local-ht (private:list->eq-set (map identifier-reference-identifier local))]
              [tail
                (filter
                  (lambda (reference)
                    (not (eq-hashtable-contains? local-ht (identifier-reference-identifier reference))))
                  doc-list)])
            (eq-hashtable-set! private:document-tail-cache document (cons local (cons doc-list tail)))
            tail))))))

(define find-available-references-for
  (case-lambda
    [(document current-index-node)
      (let* ([local (index-node-references-import-in-this-node current-index-node)]
          [excluded (index-node-excluded-references current-index-node)]
          [combined
            (append
              local
              (if (null? (index-node-parent current-index-node))
                (private:document-tail document local)
                (let ([local-ht (private:list->eq-set (map identifier-reference-identifier local))])
                  (filter
                    (lambda (reference)
                      (not (eq-hashtable-contains? local-ht (identifier-reference-identifier reference))))
                    (find-available-references-for document (index-node-parent current-index-node))))))])
        (if (null? excluded)
          combined
          (let ([exclude-ht (private:list->eq-set excluded)])
            (filter
              (lambda (reference) (not (eq-hashtable-contains? exclude-ht reference)))
              combined))))]
    [(document current-index-node identifier)
      (let ([expression (annotation-stripped (index-node-datum/annotations current-index-node))]
          [export-list (index-node-references-export-to-other-node current-index-node)])
        ;binding-position short-circuit: a node whose expression IS the identifier and which carries a same-named export reference is a
        ;declaration, not a use. Only a NON-speculative export counts: back-propagation (shallow-copy) stamps speculative export copies
        ;onto use-position nodes too (single-symbol macro clause bodies), and those must keep resolving -- see doc/analysis/
        ;match-auto-resolution.md TODO-steer investigation 2026-10-07.
        (if (and
            (find
              (lambda (i)
                (and (eq? identifier (identifier-reference-identifier i))
                  (not (identifier-reference-speculative? i))))
              export-list)
            (eq? expression identifier))
          '()
          (find-available-references-for document current-index-node identifier '())))]
    [(document current-index-node identifier exclude)
      (let* ([current-exclude (append exclude (index-node-excluded-references current-index-node))]
          [tmp-result
            (private-binary-search
              (index-node-references-import-in-this-node current-index-node)
              identifier
              current-exclude)])
        (if (null? tmp-result)
          (if (null? (index-node-parent current-index-node))
            (private-binary-search (document-ordered-reference-list document) identifier current-exclude)
            (find-available-references-for document (index-node-parent current-index-node) identifier current-exclude))
          tmp-result))]))

;name of a reference's identifier, cached; non-symbol identifiers are treated as the empty string (they sort first; the old comparator also
;treated them as incomparable, so their position was never well-defined).
(define (private:reference-name reference)
  (let ([id (identifier-reference-identifier reference)])
    (if (symbol? id) (private:identifier-name id) "")))

; first index whose name is not < target-name (i.e. target <= name)
(define (private:lower-bound vector-instance target-name)
  (let ([n (vector-length vector-instance)])
    (let loop ([lo 0] [hi n])
      (if (< lo hi)
        (let ([mid (fxarithmetic-shift-right (+ lo hi) 1)])
          (if (string<? (private:reference-name (vector-ref vector-instance mid)) target-name)
            (loop (+ mid 1) hi)
            (loop lo mid)))
        lo))))

;sorted-list -> vector memo (2026-10-11 perf): private-binary-search materialized the (possibly
;long, ~3000-entry) root reference list into a fresh vector on EVERY lookup -- a large per-lookup
;allocation driving GC. Lists are replaced functionally on every update, so keying the memo by list
;identity is sound: a stale vector can never be observed. Weak so replaced lists are collected.
;Per-thread (owner-tagged, cf. private:my-name-cache): threaded-map workers would corrupt a shared
;weak hashtable -- observed as nonrecoverable invalid memory reference on threaded init.
(define private:sorted-list-vector-cache (make-thread-parameter #f))

(define (private:my-vector-cache)
  (let ([cell (private:sorted-list-vector-cache)])
    (if (and cell (eqv? (car cell) (get-thread-id)))
      (cdr cell)
      (let ([fresh (cons (get-thread-id) (make-weak-eq-hashtable))])
        (private:sorted-list-vector-cache fresh)
        (cdr fresh)))))

(define (private:sorted-list->vector lst)
  (let ([cache (private:my-vector-cache)])
    (or (eq-hashtable-ref cache lst #f)
      (let ([v (list->vector lst)])
        (eq-hashtable-set! cache lst v)
        v))))

;Exclude-set representation: #f = no exclusion; a single reference = compare with eq?; otherwise an eq-hashtable.  Most lookups have an empty
;or single-element exclude list, so skip building a hashtable for them.
(define (private:exclude-set exclude)
  (cond
    [(null? exclude) #f]
    [(null? (cdr exclude)) (car exclude)]
    [else (private:list->eq-set exclude)]))

(define (private:excluded? exclude-set reference)
  (cond
    [(not exclude-set) #f]
    [(eq-hashtable? exclude-set) (eq-hashtable-contains? exclude-set reference)]
    [else (eq? exclude-set reference)]))

;Find all references in a list sorted by identifier-compare? whose identifier is equal to `identifier`, without allocating a dummy
;identifier-reference per lookup (the old code built one just to feed util/binary-search's order-compare).  A single lower-bound search
;plus a forward scan of the equal-name run: cheaper than the old converge-then-collect and than two bound searches.
(define (private-binary-search reference-list identifier exclude)
  (if (not (symbol? identifier))
    '()
    (let ([vector-instance (private:sorted-list->vector reference-list)]
        [target-name (private:identifier-name identifier)])
      (let ([n (vector-length vector-instance)]
          [lo (private:lower-bound vector-instance target-name)])
        (if (or (>= lo n)
                (not (string=? (private:reference-name (vector-ref vector-instance lo)) target-name)))
          '()
          (let ([exclude-set (private:exclude-set exclude)])
            (let loop ([i lo] [acc '()])
              (if (or (>= i n)
                      (not (string=? (private:reference-name (vector-ref vector-instance i)) target-name)))
                (reverse acc)
                (let ([reference (vector-ref vector-instance i)])
                  (loop (+ i 1)
                    (if (private:excluded? exclude-set reference)
                      acc
                      (cons reference acc))))))))))))

(define (root-ancestor identifier-reference)
  (if (null? (identifier-reference-parents identifier-reference))
    `(,identifier-reference)
    (fold-right append '() (map root-ancestor (identifier-reference-parents identifier-reference)))))

(define (find-references-in document index-node available-references predicate?)
  (match-index-node index-node
    [(:= index-node-expression (? predicate? maybe-symbol))
      (let ([result
            (find
              (lambda (candidate-reference)
                (if (find (lambda (cr) (eq? cr candidate-reference)) available-references)
                  #t
                  #f))
              (find-available-references-for document index-node maybe-symbol))])
        (if result
          `(,(make-location
            (document-uri document)
            (make-range
              (apply make-position (document+bias->position-list document (index-node-start index-node)))
              (apply make-position (document+bias->position-list document (index-node-end index-node))))))
          '()))]
    [else
      (let ([children (index-node-children index-node)])
        (if (null? children)
          '()
          (fold-left append '()
            (map (lambda (child-index-node) (find-references-in document child-index-node available-references predicate?)) children))))]))
)
