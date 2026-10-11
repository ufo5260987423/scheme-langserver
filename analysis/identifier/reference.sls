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
;init.  Cache each interned symbol's name string.  The cache is per-thread: init-references runs under threaded-map, and a global mutable hashtable
;would risk the same bucket corruption that once killed private:expander-doc-cache-ht.
(define private:identifier-name-cache (make-thread-parameter #f)) ;plain eq-hashtable: symbols are interned (never collected)

(define (private:identifier-name identifier)
  (let ([cache (private:identifier-name-cache)])
    (if cache
      (or (eq-hashtable-ref cache identifier #f)
        (let ([name (symbol->string identifier)])
          (eq-hashtable-set! cache identifier name)
          name))
      (let ([fresh (make-eq-hashtable)])
        (private:identifier-name-cache fresh)
        (let ([name (symbol->string identifier)])
          (eq-hashtable-set! fresh identifier name)
          name)))))

(define (identifier-compare? target1 target2)
  (let ([id1 (identifier-reference-identifier target1)]
      [id2 (identifier-reference-identifier target2)])
    ;eq? first: binary-search probes mostly land on the equal element, and interned symbols hit eq? far more often than the name compare.
    (or (eq? id1 id2)
      (and (symbol? id1) (symbol? id2)
        (string<=? (private:identifier-name id1) (private:identifier-name id2))))))

;Batch maintenance for reference lists: sort+dedupe the (small) new batch, then merge it into the (large) existing list in O(n) instead
;of re-sorting everything on every attach.  See merge-ordered-list.sls for the semantics contract.
(define (append-references-into-ordered-references-for document index-node list)
  (if (null? list)
    (void)
    (let ([sorted-batch (dedupe-adjacent (sort-identifier-references (reverse list)) equal?)])
      (if (null? index-node)
        (document-ordered-reference-list-set! document
          (merge-ordered-lists (document-ordered-reference-list document) sorted-batch identifier-compare? equal?))
        (index-node-references-import-in-this-node-set! index-node
          (merge-ordered-lists (index-node-references-import-in-this-node index-node) sorted-batch identifier-compare? equal?))))))

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

(define find-available-references-for
  (case-lambda
    [(document current-index-node)
      (let* ([local (index-node-references-import-in-this-node current-index-node)]
          [local-ht (private:list->eq-set (map identifier-reference-identifier local))]
          [exclude-ht (private:list->eq-set (index-node-excluded-references current-index-node))])
        (filter
          (lambda (reference) (not (eq-hashtable-contains? exclude-ht reference)))
          (append
            local
            (filter
              (lambda (reference)
                (not (eq-hashtable-contains? local-ht (identifier-reference-identifier reference))))
              (if (null? (index-node-parent current-index-node))
                (document-ordered-reference-list document)
                (find-available-references-for document (index-node-parent current-index-node)))))))]
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
(define private:sorted-list-vector-cache (make-weak-eq-hashtable))

(define (private:sorted-list->vector lst)
  (or (eq-hashtable-ref private:sorted-list-vector-cache lst #f)
    (let ([v (list->vector lst)])
      (eq-hashtable-set! private:sorted-list-vector-cache lst v)
      v)))

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
