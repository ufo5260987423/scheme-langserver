(library (scheme-langserver analysis identifier expanders expansion-wrap)
  (export
    expansion-generator->rule
    expansion-template-lookup)
  (import
    (chezscheme)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver util contain))


;Cross-call-site expansion templates (TODO-10 performance half). After a full expansion of (generator, call-expression), the created references
;are captured as paths into the compound tree and cached here; a later call site with an IDENTICAL datum replays them without stepping the
;skeleton. Keyed by generator procedure (eq) so re-defined macros get a fresh cache. Guarded by a mutex: threaded-map workers read AND
;write this during parallel step, and Chez hashtables corrupt under concurrent mutation (same failure class as the reference.sls
;sorted-list-vector-cache crash, 2026-10-11). Critical sections hold no callbacks, so no deadlock; accesses are ns-scale vs ms-scale calls.
(define private:expansion-template-cache (make-eq-hashtable))
(define private:expansion-template-cache-mutex (make-mutex))

(define (private:template-cache-ref proc expression)
  (with-mutex private:expansion-template-cache-mutex
    (let ([ht (hashtable-ref private:expansion-template-cache proc #f)])
      (and ht (hashtable-ref ht expression #f)))))

(define (private:template-cache-set! proc expression specs)
  (with-mutex private:expansion-template-cache-mutex
    (let ([ht (or (hashtable-ref private:expansion-template-cache proc #f)
                (let ([ht (make-hashtable equal-hash equal?)])
                  (hashtable-set! private:expansion-template-cache proc ht)
                  ht))])
      (hashtable-set! ht expression specs))))

(define (expansion-template-lookup proc expression)
  ;diagnostic accessor (tests): the stored template (list of specs) for (generator, call-site datum), or #f
  (private:template-cache-ref proc expression))

(define (private:compound-path compound target)
  ;path of 'a (car) / 'd (cdr) steps from compound to target (eq?), #f if unreachable (compound trees are acyclic by construction)
  (let search ([c compound] [rev '()])
    (cond
      [(eq? c target) (reverse rev)]
      [(or (index-node? c) (null? c) (not (pair? c))) #f]
      [else
        (or (search (car c) (cons 'a rev))
            (search (cdr c) (cons 'd rev)))])))

(define (private:compound-ref compound path)
  (fold-left
    (lambda (c step)
      (if (pair? c)
        (if (eq? step 'a) (car c) (cdr c))
        (raise 'path-error)))
    compound path))

(define (private:store-template! proc expression created compound-root call-node)
  ;template = list of (identifier library-identifier type kind path); #f path means the call node itself. Every target is either inside the
  ;compound tree (path-encodable) or the call node itself (#f), so storing is unconditional and always succeeds.
  (let ([specs
      (map
        (lambda (item)
          (let* ([ni (car item)]
              [target (cadr item)]
              [kind (caddr item)]
              [path (if (eq? target call-node)
                    #f
                    (private:compound-path compound-root target))])
            (list
              (identifier-reference-identifier ni)
              (identifier-reference-library-identifier ni)
              (identifier-reference-type ni)
              kind
              path)))
        created)])
    (private:template-cache-set! proc expression specs)
    #t))

(define (private:replay-template specs proc root-file-node root-library-node document index-node)
  ;returns #t on success; #f (fall back to a full run) on any path decode failure or if the generator declines the call
  (guard (e [else
      (when (getenv "REPLAY_TRACE")
        (printf "REPLAY-FAIL ~a~%"
          (if (condition? e) (condition-message e) e)))
      #f])
    (let* ([pairs+expansion (proc root-file-node root-library-node document index-node)])
      (if (not pairs+expansion)
        #f
        (let* ([pairs (car pairs+expansion)]
            [expansion-index-node (cdr pairs+expansion)]
            ;templates stored against the call-node fallback carry only #f paths (see private:full-run), so a
            ;missing root pair is fine as long as every path is #f; a non-#f path with no anchor raises below
            [root-pair (private:pair-find expansion-index-node pairs)]
            [compound-root (and root-pair (cdr root-pair))]
            [pending-inserts-ht (make-eq-hashtable)])
          (for-each
            (lambda (spec)
              (let* ([identifier (car spec)]
                  [library-identifier (cadr spec)]
                  [type (caddr spec)]
                  [kind (cadddr spec)]
                  [path (list-ref spec 4)]
                  [target (if (eq? path #f)
                            index-node
                            (let ([t (private:compound-ref compound-root path)])
                              (if (index-node? t) t (raise 'path-error))))]
                  [ni (make-identifier-reference
                      identifier document target index-node
                      library-identifier type '() '() '())])
                (identifier-reference-speculative?-set! ni #t)
                (if (eq? kind 'export)
                  (let ([current-exports (index-node-references-export-to-other-node target)])
                    (if (not (find (lambda (e2) (eq? identifier (identifier-reference-identifier e2))) current-exports))
                      (index-node-references-export-to-other-node-set!
                        target (append current-exports `(,ni)))
                      '()))
                  (hashtable-update! pending-inserts-ht target
                    (lambda (old) (cons ni old)) '()))))
            specs)
          (private:flush-inserts! pending-inserts-ht document)
          #t)))))

(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory . maybe-expander-ref)
  (let ([expander-ref (if (null? maybe-expander-ref) #f (car maybe-expander-ref))])
    (lambda (root-file-node root-library-node document index-node)
      (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
          [template (private:template-cache-ref proc expression)])
        (cond
          ;replay: identical datum seen before, recursion guards satisfied
          [(and template
              (not (contain? memory expression))
              (< (length memory) 15))
            (or (private:replay-template template proc root-file-node root-library-node document index-node)
              (private:full-run proc step file-linkage expanded+callee-list memory expander-ref root-file-node root-library-node document index-node expression))]
          [else
            (private:full-run proc step file-linkage expanded+callee-list memory expander-ref root-file-node root-library-node document index-node expression)])))))

(define (private:full-run proc step file-linkage expanded+callee-list memory expander-ref root-file-node root-library-node document index-node expression)
  (let ([pairs+expansion (proc root-file-node root-library-node document index-node)])
    (if pairs+expansion
      (let* ([pairs (car pairs+expansion)]
          [expansion-index-node (cdr pairs+expansion)]
          [possible-new-memory `(,expression . ,memory)]
          [expander-doc (if expander-ref (identifier-reference-document expander-ref) #f)]
          [previous-all-pairs (if (null? expanded+callee-list)
                                '()
                                (list-ref (car expanded+callee-list) 4))]
          [new-all-pairs (append pairs previous-all-pairs)]
          [new-expanded+callee-list
            (cons `(,expansion-index-node ,index-node ,expander-doc ,pairs ,new-all-pairs) expanded+callee-list)])
        ;Guard 1: prevent re-expansion of the exact same expression. Guard 2: cap memory chain length to avoid infinite cascades
        ;(e.g. match -> match-next -> match-one -> ...).
        (if (and (not (contain? memory expression)) (< (length memory) 15))
          (step root-file-node root-library-node file-linkage document expansion-index-node new-expanded+callee-list possible-new-memory))
        (let ([created (private:shallow-copy pairs (extract-all-pairs new-expanded+callee-list) expansion-index-node document index-node)]
            [root-pair (private:pair-find expansion-index-node pairs)])
          ;the list-shaped template instantiations of real macros (e.g. the match family) give the expansion root
          ;no pair in pairs (see syntax-rules.sls pairs-rev), so gating storage on root-pair meant templates were
          ;almost never stored and identical call-site datums re-expanded at full price. Store unconditionally:
          ;with a root pair the compound counterpart anchors path encoding as before; without one, anchor at the
          ;call node -- private:compound-path then yields #f for every target and replay attaches references at the
          ;call node instead of the precise counterpart (graceful degradation, still sound: no reference is lost).
          (private:store-template! proc expression created
            (if root-pair (cdr root-pair) index-node) index-node))
        #t)
      #f)))

(define (private:recursive-filter compound-list predicate?)
  (let loop ([stack (list compound-list)] [acc '()])
    (if (null? stack)
      (reverse acc)
      (let ([current (car stack)] [rest (cdr stack)])
        (cond
          [(predicate? current)
            (loop rest (cons current acc))]
          [(null? current)
            (loop rest acc)]
          [(pair? current)
            (loop (cons (car current) (cons (cdr current) rest)) acc)]
          [(vector? current)
            (loop (cons (vector->list current) rest) acc)]
          [else
            (loop rest acc)])))))

(define (private:recursive-collect expansion-index-node proc)
  (let loop ([stack (list expansion-index-node)] [acc '()])
    (if (null? stack)
      (reverse acc)
      (let* ([node (car stack)]
          [current (proc node)]
          [new-acc (if (null? current) acc (cons `(,node . ,current) acc))]
          [children (index-node-children node)]
          [new-stack
            (let child-loop ([cs children] [s (cdr stack)])
              (if (null? cs)
                s
                (child-loop (cdr cs) (cons (car cs) s))))])
        (loop new-stack new-acc)))))

(define (private:find-nodes-by-symbol node sym)
  (if (index-node? node)
    (let loop ([stack (list node)] [acc '()])
      (if (null? stack)
        (reverse acc)
        (let* ([current (car stack)]
            [new-acc (if (eq? sym (annotation-stripped (index-node-datum/annotations current)))
                      (cons current acc)
                      acc)]
            [children (index-node-children current)]
            [new-stack
              (let child-loop ([cs children] [s (cdr stack)])
                (if (null? cs)
                  s
                  (child-loop (cdr cs) (cons (car cs) s))))])
          (loop new-stack new-acc))))
    '()))

;Shallow-copy is a reference back-propagator: it copies identifier-references from the expanded AST back to the
;original macro-call nodes according to the pattern->expansion correspondence, so that IDE features (go-to-definition, completion) work on user-written code.
(define (extract-all-pairs expanded+callee-list)
  (if (null? expanded+callee-list)
    '()
    (list-ref (car expanded+callee-list) 4)))

;pairs entries are (skeleton-node . compound-counterpart). Lookup must be by IDENTITY of the skeleton node: the previous assoc-based lookup
;compared the node record with a dotted pair via equal?, which is always #f -- so pairs never matched and every consumer silently fell back to
;the symbol/call-node chains (TODO-15 P3 step 1: resurrect the pairs)
(define (private:pair-find node pairs)
  (find (lambda (p) (eq? node (car p))) pairs))

(define (build-reverse-map all-pairs)
  (let ([ht (make-eq-hashtable)])
    (for-each
      (lambda (pair)
        (let ([key (cdr pair)])
          (when (and (index-node? key) (not (hashtable-contains? ht key)))
            (hashtable-set! ht key (car pair)))))
      all-pairs)
    ht))

(define (private:sync-to-parent-expansion target-node export-list reverse-map document initialization-index-node)
  (let ([parent-node (hashtable-ref reverse-map target-node #f)])
    (when parent-node
      (for-each
        (lambda (current-identifier)
          (let ([ni
              (make-identifier-reference
                (identifier-reference-identifier current-identifier)
                document
                parent-node
                initialization-index-node
                (identifier-reference-library-identifier current-identifier)
                (identifier-reference-type current-identifier)
                '()
                '()
                (identifier-reference-top-environment current-identifier))])
            (identifier-reference-speculative?-set! ni #t)
            (index-node-references-export-to-other-node-set!
              parent-node
              (append
                (index-node-references-export-to-other-node parent-node)
                `(,ni)))))
        export-list))))

;counterpart must live in the ORIGINAL call-site tree: a pair-mapped counterpart that is itself an intermediate expansion node would strand
;the reference where no use-site lookup can reach it (per-hop papered over this with multi-hop accumulation; the single-back-prop cascade
;fusion experiment made the stranding visible -- TODO-13 root cause A)
(define (private:under-node? node ancestor)
  (let loop ((n node))
    (cond
      [(not (index-node? n)) #f]
      [(eq? n ancestor) #t]
      [else (loop (index-node-parent n))])))

;TODO-13 test against the DOCUMENT roots, not the hop-local call node: expansion trees are free-standing (their parent chains never reach a
;document root -- the under-node? checks elsewhere rely on exactly that), so call-site-tree membership identifies original source nodes at ANY
;cascade hop. The pvar substitution inserts call-site nodes (e.g. pattern variables) into template compounds; only document-root membership
;recognizes them across hops.
(define (private:call-site-node? node document)
  (let ([roots (document-index-node-list document)])
    (let loop ([n node])
      (cond
        [(not (index-node? n)) #f]
        [(memq n roots) #t]
        [else (loop (index-node-parent n))]))))

(define (private:call-site-subleaf node identifier document)
  ;First leaf in node's subtree whose symbol is identifier AND that lives in the original call-site tree. Template pvar substitution places
  ;call-site nodes inside compound subtrees, so a pair's compound (or a binding reference's index-node) can contain the true anchor as a
  ;descendant -- e.g. a pattern-variable binding reference registers the whole `(a w)` binding pair as its index-node, whose car is the
  ;call-site pattern node. Returns #f when no such leaf exists (genuinely macro-introduced identifier). Mirrors Chez's own expander
  ;attribution (see tests/analysis/identifier/test-sc-expand-differential.sps).
  (let loop ([stack (list node)])
    (if (null? stack)
      #f
      (let ([current (car stack)])
        (cond
          [(not (index-node? current)) (loop (cdr stack))]
          [(and (eq? identifier (annotation-stripped (index-node-datum/annotations current)))
              (private:call-site-node? current document))
            current]
          [else
            (loop (append (index-node-children current) (cdr stack)))])))))

(define (private:call-site-leaf-in-pairs identifier all-pairs document)
  ;First call-site-tree leaf named identifier found in any hop's pair compounds. See the
  ;fallback recovery in private:shallow-copy for why the scan spans all hops.
  (let loop ([ps all-pairs])
    (if (null? ps)
      #f
      (let ([compound (cdar ps)])
        (or (and (index-node? compound)
              (private:call-site-subleaf compound identifier document))
          (loop (cdr ps)))))))

(define (private:shallow-copy pairs all-pairs expansion-index-node document initialization-index-node)
  ;Back-propagates references from the expansion tree to the callee (call-site/compound) tree. Returns the list of (new-reference target-node kind)
  ;for every reference this call created (kind: export | import); the rule layer uses it to build expansion templates for cross-call-site
  ;replay. New references are marked speculative (tentative knowledge). all-pairs grows across cascade hops, and the export walk probes it
  ;per export-bearing node -- index it once per call (TODO-15 P3 perf)
  (let* ([created '()]
      [all-pairs-ht (make-eq-hashtable)]
      [reverse-map (build-reverse-map all-pairs)]
      [_ (for-each
          (lambda (entry) (eq-hashtable-set! all-pairs-ht (car entry) entry))
          all-pairs)]
      [local-identifiers+export-index-node (private:recursive-collect expansion-index-node index-node-references-export-to-other-node)]
      [local-identifiers+import-index-node (private:recursive-collect expansion-index-node index-node-references-import-in-this-node)]
      ;queue the ordered-list inserts per target and flush once per target afterwards (per-reference singleton inserts were quadratic)
      [pending-inserts-ht (make-eq-hashtable)])
    (define (queue-insert! import-index-node ni)
      (hashtable-update! pending-inserts-ht
        import-index-node
        (lambda (old) (cons ni old))
        '()))
    ;TODO-15 P2 anchor dedup: multiple import-bearing skeleton nodes can resolve to the SAME anchor (fallback call node or a shared compound
    ;counterpart), and each used to stamp its own copy of a reference -- the call node accumulated ~23 duplicates per binding. Skip a copy
    ;when an equal (identifier type) reference is already queued for or attached at the target. Arrows (P1) will replace this multiplicity
    ;model entirely; this is the bounded interim. intra-hop dedup is enough: the multiplicity case (many import-
    ;bearing skeleton nodes in ONE skeleton resolving to the anchor) arrives within a single shallow-copy batch, and the pending queue
    ;is flushed at the end of each batch
    (define (anchor-copy-exists? target ref)
      (let ([id (identifier-reference-identifier ref)]
          [ty (identifier-reference-type ref)])
        (find (lambda (queued)
            (and (eq? id (identifier-reference-identifier queued))
              (eq? ty (identifier-reference-type queued))))
          (hashtable-ref pending-inserts-ht target '()))))
    (for-each
      (lambda (p)
        (let* ([lis (cdr p)]
            [i (car p)]
            [corresponding-export-ip (private:pair-find i pairs)]
            ;If the expansion-sub-node has no counterpart in pairs (e.g. ellipsis truncation caused a mismatch), fall back to the
            ;original macro-call node so the reference is not lost.
            [compound-export-list
              (cond
                [corresponding-export-ip
                  (cdr corresponding-export-ip)]
                [(hashtable-ref all-pairs-ht i #f) => cdr]
                [else
                  (let ([sym (annotation-stripped (index-node-datum/annotations i))])
                    (if (symbol? sym)
                      (let ([matches (private:find-nodes-by-symbol initialization-index-node sym)])
                        (if (null? matches)
                          initialization-index-node
                          matches))
                      initialization-index-node))])])
          (for-each
            (lambda (single-compound-export-index-node)
              ; Primary attachment: write to callee tree (existing behavior)
              (for-each
                (lambda (current-identifier)
                  (let* ([ni
                      (make-identifier-reference
                        (identifier-reference-identifier current-identifier)
                        document
                        single-compound-export-index-node
                        initialization-index-node
                        (identifier-reference-library-identifier current-identifier)
                        (identifier-reference-type current-identifier)
                        '()
                        '()
                        (identifier-reference-top-environment current-identifier))])
                    (identifier-reference-speculative?-set! ni #t)
                    (let ([current-exports (index-node-references-export-to-other-node single-compound-export-index-node)])
                      (if (not (find (lambda (e) (eq? (identifier-reference-identifier current-identifier) (identifier-reference-identifier e))) current-exports))
                        (begin
                          (index-node-references-export-to-other-node-set!
                            single-compound-export-index-node
                            (append current-exports `(,ni)))
                          (set! created (cons `(,ni ,single-compound-export-index-node export) created)))
                        '()))))
                lis)
              ; Reverse-map sync: also write to parent expansion tree so outer layer can see it
              (private:sync-to-parent-expansion single-compound-export-index-node lis reverse-map document initialization-index-node))
            (cond
              [(index-node? compound-export-list) `(,compound-export-list)]
              [(list? compound-export-list) (filter index-node? compound-export-list)]
              [else '()]))))
      local-identifiers+export-index-node)
    ;Import side, independent of the export walk: for every import-bearing skeleton node, copy its references to the compound
    ;counterpart (call-site) node so use-site resolution works. Root and symbol shapes have no node-level pair (the pairs walk only covers
    ;index-node compounds and child zips), so fall back to the macro call node -- the import's scope is the whole expansion, which at
    ;the call site is exactly the call.
    (for-each
      (lambda (import-entry)
        (let ([counterpart
            (let ([p (private:pair-find (car import-entry) pairs)])
              (if p
                (let ([c (cdr p)])
                  (if (and (index-node? c) (private:call-site-node? c document))
                    c
                    initialization-index-node))
                initialization-index-node))])
          (when (index-node? counterpart)
            (for-each
              (lambda (import-reference)
                ;When still on the fallback anchor, recover the true anchor: the reference's own
                ;index-node may BE a call-site node (paired compounds embed pvar call-site nodes),
                ;and otherwise the pair's compound subtree is searched for a matching call-site leaf
                ;(e.g. pattern-variable bindings anchor at the pattern -- Chez expander attribution).
                (let* ([p (private:pair-find (car import-entry) pairs)]
                    [c (and p (cdr p))]
                    [identifier (identifier-reference-identifier import-reference)]
                    [ref-node (identifier-reference-index-node import-reference)]
                    [anchor
                      (if (eq? counterpart initialization-index-node)
                        (or (and (index-node? ref-node)
                                (not (eq? ref-node initialization-index-node))
                                (private:call-site-node? ref-node document)
                                ref-node)
                          (and (index-node? c)
                            (private:call-site-subleaf c identifier document))
                          (and (index-node? ref-node)
                            (private:call-site-subleaf ref-node identifier document))
                          ;Bindings attach to interior scope nodes (e.g. the generated let),
                          ;which have no pair -- the pvar link exists only at the leaf pairs.
                          ;Scan every hop's pair compounds for a call-site leaf with this
                          ;identifier: at the outermost hop the compounds embed the original
                          ;call-site nodes (pattern variables), so the binding anchors at its
                          ;true source position (Chez expander attribution; cf. the sc-expand
                          ;differential test). Inner hops have no document compounds and keep
                          ;the fallback. First match wins: template (left-to-right) order puts
                          ;the binding pattern before the body.
                          (private:call-site-leaf-in-pairs identifier all-pairs document)
                          initialization-index-node)
                        counterpart)])
                (let ([ni
                    (make-identifier-reference
                      identifier
                      document
                      anchor
                      initialization-index-node
                      (identifier-reference-library-identifier import-reference)
                      (identifier-reference-type import-reference)
                      '()
                      '()
                      (identifier-reference-top-environment import-reference))])
                  (identifier-reference-speculative?-set! ni #t)
                  ;Insertion (scope visibility) and anchor (definition position) are separated:
                  ;the reference resolves from the whole macro call (fallback counterpart, an
                  ;ancestor of every use), while goto-definition lands at the recovered
                  ;call-site position (e.g. the pattern). Inserting at the anchor instead would
                  ;hide the binding from sibling use sites (measured: resolution collapses to 0).
                  (when (not (and (eq? counterpart initialization-index-node)
                              (anchor-copy-exists? counterpart ni)))
                    (queue-insert! counterpart ni))
                  (set! created (cons `(,ni ,counterpart import) created)))))
              (cdr import-entry)))))
      local-identifiers+import-index-node)
    (private:flush-inserts! pending-inserts-ht document)
    (reverse created)))

(define (private:flush-inserts! pending-inserts-ht document)
  (when (positive? (hashtable-size pending-inserts-ht))
    (vector-for-each
      (lambda (import-index-node)
        (append-references-into-ordered-references-for document import-index-node
          (hashtable-ref pending-inserts-ht import-index-node '())))
      (hashtable-keys pending-inserts-ht))))
) ; end library
