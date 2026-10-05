(library (scheme-langserver analysis identifier expanders syntax-rules)
  (export 
    syntax-rules->generator:map+expansion
    make-generator-for-clauses
    make-fused-generator)
  (import
    (chezscheme)
    (ufo-match-steer)

    (scheme-langserver analysis identifier expanders pattern)
    (scheme-langserver analysis identifier expanders dispatch-matcher)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

;; ---- Signature extraction for fast clause filtering ----

(define (private:param-shape param)
  (cond
    [(null? param) 'null]
    [(symbol? param) 'symbol]
    [(pair? param) 'pair]
    [(vector? param) 'vector]
    [else 'other]))

(define (private:fixed-prefix params)
  (cond
    [(null? params) '()]
    [(and (not (null? (cdr params))) (eq? '... (cadr params)))
      (private:fixed-prefix (cddr params))]
    [else (cons (car params) (private:fixed-prefix (cdr params)))]))

(define (private:extract-signature pattern-expr)
  (let ([params (cdr pattern-expr)])
    (if (list? params)
      (let ([fixed (private:fixed-prefix params)])
        (cons (length fixed) (map private:param-shape fixed)))
      (let ([prefix 
              (let loop ([p params])
                (if (pair? p) (cons (car p) (loop (cdr p))) '()))])
        (cons (length prefix) (map private:param-shape prefix))))))

(define (private:shape-match? shape param)
  (case shape
    [(symbol) #t]
    [(null) (null? param)]
    [(pair) (or (pair? param) (null? param))]
    [(vector) (vector? param)]
    [else #t]))

(define (private:params-shapes-match? shapes params)
  (if (or (null? shapes) (null? params))
    (and (null? shapes) (null? params))
    (and (private:shape-match? (car shapes) (car params))
      (private:params-shapes-match? (cdr shapes) (cdr params)))))

(define (private:signature-match? signature input-expr)
  (let ([input-params (cdr input-expr)]
      [min-len (car signature)]
      [shapes (cdr signature)])
    (and 
      (list? input-params)
      (>= (length input-params) min-len)
      (private:params-shapes-match? shapes 
        (let loop ([lst input-params] [n (length shapes)])
          (if (or (zero? n) (null? lst)) 
            '() 
            (cons (car lst) (loop (cdr lst) (- n 1)))))))))

;; ---- Nested-macro guard ----

; If a syntax-rules template contains nested macro definitions
; (let-syntax, letrec-syntax, syntax-rules, define-syntax),
; the auto-resolver cannot handle them.  Detect this at definition
; time and install a no-op generator so that define-syntax:attach-generator
; does not crash and the macro falls back to hand-written rules.
; Templates may contain let-syntax/letrec-syntax with inner syntax-rules
; (e.g. match-two, match-check-identifier, match-check-ellipsis).
; The auto-resolver already has full let-syntax/letrec-syntax support
; (let-syntax-process + let-syntax:attach-generator), so nested macros
; inside templates are fine.  We only reject define-syntax in templates
; because runtime define-syntax is almost always a mistake and our
; scope tracking for it is not fully tested.

;; ---- Main generator factory ----

;input-index-node is supposed have the form of `(syntax-rules ...)`
(define (syntax-rules->generator:map+expansion root-file-node root-library-node document input-index-node)
  (match-index-node input-index-node
    ;clause means pattern and template
    ;:_ for the head: both call sites guarantee it -- the interpreter
    ;registers this as the second-pass rule of the 'syntax-rules special
    ;form, and the cache-reattach path case-matches (car expr) explicitly
    [(:_ (literals ...) clauses **1)
      (make-generator-for-clauses input-index-node
        (map index-node-expression literals)
        clauses
        (lambda (clause-expression) (car (reverse clause-expression))))]
    [else #f]))

; Shared logic for building a generator from clause index-nodes.
; extract-template is a function that takes a clause-expression datum
; and returns the bare template (for syntax-rules it is the last element;
; for syntax-case it is the unwrapped syntax form).
;NB: templates that themselves contain define-syntax (macro-generating
;macros) are no longer rejected here. Building the generator is pure
;definition-time work and stays inert until invoked; any expansion cascade
;through generated define-syntax forms is bounded by the recursion guard
;(memory cap) in expansion-wrap.sls. Telemetry (fallback/mismatch counters)
;now covers these macros too.
(define (make-generator-for-clauses input-index-node literals clause-index-nodes extract-template)
  (let ([clause-selector (private:compile-clause-selector literals clause-index-nodes)]
        ;definition-time per-clause compilation (Chez convert-pattern style):
        ;pattern tree, template tree and variable context are built once,
        ;not per call
        [clause-data-vector
          (list->vector
            (map
              (lambda (clause-index-node)
                (let* ([clause-expression (index-node-expression clause-index-node)]
                    [pattern-expression (car clause-expression)]
                    ;template tree per clause; the ellipsis context is derived
                    ;from the match pattern datum directly (dispatch-matcher's
                    ;gather-context -- the old pattern record existed only for
                    ;its cdr? flags and is gone); matching goes through the
                    ;Chez-transcribed dispatch-matcher (TODO-12)
                    [template-pattern (make-pattern (extract-template clause-expression))])
                  (let-values ([(dsl pvars) (convert-pattern pattern-expression literals)])
                    (vector dsl pvars template-pattern (gather-context pattern-expression literals)))))
              clause-index-nodes))]
        ;memoize the selector's transcription: keyed by the call-site datum,
        ;it maps to (clause-index . instantiated-template-datum). Only this
        ;node-independent stage is cached -- bindings/compound/skeleton/pairs
        ;embed call-site nodes and must be rebuilt per call site even for
        ;identical datums (89% of cascade expansions are exact duplicates).
        [selector-cache (make-hashtable equal-hash equal?)])
      (index-node-expansion-generator-set! input-index-node
        (lambda (local-root-file-node local-root-library-node local-document local-index-node)
          (let* ([local-expression (index-node-expression local-index-node)]
              [index+expansion
                (if clause-selector
                  (or (hashtable-ref selector-cache local-expression #f)
                    (let ([confirmed (private:selector-confirm clause-selector local-expression)])
                      (when confirmed
                        (hashtable-set! selector-cache local-expression confirmed))
                      confirmed))
                  (private:confirm-clause literals clause-index-nodes local-expression))])
          (if (not index+expansion)
            #f
            (let* ([index (car index+expansion)]
                [clause-data (vector-ref clause-data-vector index)]

                [dsl (vector-ref clause-data 0)]
                [pvars (vector-ref clause-data 1)]
                [template-pattern (vector-ref clause-data 2)]
                [pattern-context (vector-ref clause-data 3)]
                ;Chez-style dispatch matcher; anything but an alist means the
                ;match failed -- decline the expansion rather than mis-analyze
                [bindings (dsl+index-node->bindings dsl pvars local-index-node literals)])
            (if (not (list? bindings))
              #f
              (let* (
                  [callee-compound-index-node-list (expand->index-node-compound-list template-pattern bindings pattern-context)]

                [expansion-index-node
                  (init-index-node
                    (index-node-parent input-index-node)
                    (car (source-file->annotations
                      (with-output-to-string (lambda () (pretty-print (cdr index+expansion))))
                      (uri->path (document-uri local-document)))))]
                [matching-pairs (private:expansion+index-node->pairs callee-compound-index-node-list expansion-index-node)])
              ;three-element result (see expansion-wrap.sls): pairs, the
              ;reparsed skeleton, and the compound -- the compound keeps
              ;call-site node identity and is what cascade fusion splices
              `(,matching-pairs ,expansion-index-node ,callee-compound-index-node-list))))))))))

(define (private:take list n)
  (if (or (zero? n) (null? list))
    '()
    (cons (car list) (private:take (cdr list) (- n 1)))))


;these two parameter are supposed to be correct and this procedure won't do fault-tolerant things.
(define (private:expansion+index-node->pairs compound-list index-node)
  (reverse (private:expansion+index-node->pairs-rev compound-list index-node '())))

(define (private:expansion+index-node->pairs-rev compound-list index-node acc)
  (let* ([expression (index-node-expression index-node)]
      [children (index-node-children index-node)])
    (cond 
      [(index-node? compound-list)
        (let ([compound-children (index-node-children compound-list)])
          (let* ([len-cc (length compound-children)]
              [len-ch (length children)]
              [min-len (min len-cc len-ch)]
              [extra-children (if (> len-ch min-len) (list-tail children min-len) '())])
            (let ([acc0 (cons `(,index-node . ,compound-list) acc)])
              (let ([acc1
                  (let loop ([lefts (private:take compound-children min-len)]
                      [rights (private:take children min-len)]
                      [a acc0])
                    (if (or (null? lefts) (null? rights))
                      a
                      (loop (cdr lefts) (cdr rights)
                        (private:expansion+index-node->pairs-rev (car lefts) (car rights) a))))])
                (let ([acc2
                    (let loop ([ecs extra-children] [a acc1])
                      (if (null? ecs)
                        a
                        (let* ([exp-child (car ecs)]
                            [sym (index-node-expression exp-child)])
                          (if (symbol? sym)
                            (let ([compound-match (find (lambda (c) (eq? sym (index-node-expression c))) compound-children)])
                              (if compound-match
                                (loop (cdr ecs) (cons `(,exp-child . ,compound-match) a))
                                (loop (cdr ecs) a)))
                            (loop (cdr ecs) a)))))])
                  acc2)))))]
      [(list? compound-list) 
        (let ([len-c (length compound-list)]
            [len-ch (length children)])
          (let loop ([lefts (if (> len-c len-ch) (private:take compound-list len-ch) compound-list)]
              [rights (if (> len-ch len-c) (private:take children len-c) children)]
              [a acc])
            (if (or (null? lefts) (null? rights))
              a
              (loop (cdr lefts) (cdr rights)
                (private:expansion+index-node->pairs-rev (car lefts) (car rights) a)))))]
      [(vector? compound-list) 
        (private:expansion+index-node->pairs-rev (vector->list compound-list) index-node acc)]
      [(pair? compound-list) 
        (private:expansion+index-node->pairs-rev `(,(car compound-list) ,(cdr compound-list)) index-node acc)]
      [(and (symbol? compound-list) (symbol? expression))
        (cons `(,index-node . ,compound-list) acc)]
      ;symbol won't get pairs
      [else acc])))

;Compile the clause selector once per macro definition, so call sites pay a
;procedure call instead of recompiling the whole syntax-case form via eval
;(measured: ~72% of generator body cost was this per-call compilation).
;Returns #f when compilation fails, keeping private:confirm-clause as fallback.
(define (private:compile-clause-selector literals clause-index-nodes)
  (guard (e [else #f])
    (eval
      `(lambda (input-expression)
        (syntax-case input-expression ,literals
          ,@(map
              (lambda (index clause-index-node)
                (let ([clause-expression (index-node-expression clause-index-node)])
                  `(,(car clause-expression)
                    #',(cons index (car (reverse clause-expression))))))
              (iota (length clause-index-nodes))
              clause-index-nodes)
          (else #f))))))

(define (private:selector-confirm selector input-expression)
  (syntax->datum (selector input-expression)))

(define (private:confirm-clause literals clause-index-nodes input-expression)
  (let loop ([rest clause-index-nodes] [index 0])
    (if (null? rest) 
      #f
      (let* ([current-clause-index-node (car rest)]
          [current-clause-expression (index-node-expression current-clause-index-node)]
          [pattern-expression (car current-clause-expression)]
          [signature (private:extract-signature pattern-expression)])
        (if (private:signature-match? signature input-expression)
          (let* ([pre-target 
            `(syntax-case ',input-expression ,literals 
              (,(car current-clause-expression) 
                ;result
                #'(,index . ,(car (reverse current-clause-expression))))
              (else #f))]
              [target (syntax->datum (eval pre-target))])
            (if target target (loop (cdr rest) (+ 1 index))))
          (loop (cdr rest) (+ 1 index)))))))
; ---------------------------------------------------------------------------
; TODO-14 cascade fusion (post-library install, see workspace.sls
; private:fuse-exported-generators!): a fused generator runs its raw
; generator, then keeps expanding family-macro call sites found in the
; skeleton -- calling the partner macro's own generator directly, with no
; step/routing/reparse-per-hop -- until no family calls remain (or the
; budget/skip list says stop), then rebuilds pairs once from the final
; compound+skeleton. Call sites the loop declines or misses are left in
; place and handled by the ordinary per-hop path when the final skeleton
; is stepped, so fusion is strictly opportunistic.

(define private:fusion-hop-budget 2048)

(define (make-fused-generator raw-generator fusion-table)
  (lambda (root-file-node root-library-node document index-node)
    ;guards: today the raw call runs under step's try/except; a fused
    ;call may also come from another fusion loop, so carry our own -- a
    ;declined expansion must return #f, never raise (e.g. match-next's
    ;no-more-clauses clause template contains a string literal, which
    ;expand->index-node-compound-list declines as 'illegal-template)
    (guard (e (else #f))
      (let ((r (raw-generator root-file-node root-library-node document index-node)))
        (and r
          (private:fuse-cascade root-file-node root-library-node document
            (cadr r) (caddr r) fusion-table '() 0 '() (car r)))))))


(define (private:fuse-cascade root-file-node root-library-node document skeleton compound table skipped hops seen acc-pairs)
  (cond
    ((>= hops private:fusion-hop-budget)
      (private:fused-result skeleton compound acc-pairs))
    (else
      (let ((found (private:find-fusable-call skeleton table skipped)))
        (cond
          ((not found)
            (when (getenv "FUSE_TRACE")
              (printf "FUSE done hops=~a skipped=~a~%" hops (length skipped)))
            (private:fused-result skeleton compound acc-pairs))
          (else
            (let* ((call-node (car found))
                (path (cdr found))
                (sym (car (index-node-expression call-node)))
                ;a (symbol . call-datum) pair already expanded in this
                ;cascade is skipped: re-expansion is deterministic, so
                ;repeating it cannot make progress -- this breaks
                ;ping-pong cycles between dispatcher macros while leaving
                ;genuinely new calls to expand
                (tag (cons sym (index-node-expression call-node)))
                ;table values are RAW generators (workspace.sls collects
                ;them before wrapping), so one loop owns the whole
                ;cascade -- no nested wrapper recursion
                (partner (cdr (assq sym table))))
              (cond
                ((not (procedure? partner))
                  (when (getenv "FUSE_TRACE")
                    (printf "FUSE decline ~a: no partner~%" sym))
                  (private:fuse-cascade root-file-node root-library-node document skeleton compound table (cons sym skipped) hops seen acc-pairs))
                ((member tag seen)
                  (private:fuse-cascade root-file-node root-library-node document skeleton compound table (cons sym skipped) hops seen acc-pairs))
                (else
                  (let ((pr (guard (e (else
                              (when (getenv "FUSE_TRACE")
                                (printf "FUSE decline ~a: EXC ~a~%" sym
                                  (if (condition? e) (condition-message e) e)))
                              #f))
                              (partner root-file-node root-library-node document call-node))))
                    (cond
                      ((not pr)
                        (when (getenv "FUSE_TRACE")
                          (printf "FUSE decline ~a: ~a~%" sym
                            (let ((d (index-node-expression call-node)))
                              (if (and (list? d) (> (length d) 5)) (append (private:take d 5) '(...)) d))))
                        (private:fuse-cascade root-file-node root-library-node document skeleton compound table (cons sym skipped) hops seen acc-pairs))
                      (else
                        (let ((comp-sub (private:find-compound-subtree compound path)))
                          (cond
                            ((not comp-sub)
                              (private:fuse-cascade root-file-node root-library-node document skeleton compound table (cons sym skipped) hops seen acc-pairs))
                            ((null? path)
                              ;the fusable call IS the skeleton root (e.g. a
                              ;recursive macro's self-call): its parent is
                              ;the macro definition site, so children
                              ;mutation would splice the wrong tree --
                              ;continue with the new skeleton/compound
                              (private:fuse-cascade root-file-node root-library-node document
                                (cadr pr) (caddr pr) table skipped (+ hops 1) (cons tag seen)
                                (append acc-pairs (car pr))))
                            (else
                              (private:splice-skeleton! call-node (cadr pr))
                              (private:fuse-cascade root-file-node root-library-node document skeleton
                                (private:replace-subtree compound comp-sub (caddr pr))
                                table skipped (+ hops 1) (cons tag seen)
                                (append acc-pairs (car pr))))))))))))))))))

;pairs handed to full-run = ONLY the first hop's (raw) pairs: node-level
;mappings for deep generated nodes would target intermediate compounds
;(unreachable from the original tree) and suppress the import-pass
;fallback that per-hop relies on to anchor deep bindings at the call
;node. The hop pairs are still accumulated through full-run's all-pairs
;machinery, matching per-hop bookkeeping.
(define (private:fused-result skeleton compound _acc-pairs)
  (list (private:expansion+index-node->pairs compound skeleton) skeleton compound))

;preorder search for an index-node whose datum is a list headed by a
;fusion-table symbol (not on the skip list). A found call is replaced
;wholesale, so we do not descend into it. Returns (node . path) where
;path is the list of child indices from the root to the node.
(define (private:find-fusable-call node table skipped)
  (let ([expr (guard (e [else #f]) (index-node-expression node))])
    (if (and (pair? expr)
          (symbol? (car expr))
          (assq (car expr) table)
          (not (memq (car expr) skipped)))
      (cons node '())
      (let loop ([children (index-node-children node)] [i 0])
        (cond
          [(null? children) #f]
          [else
            (let ([sub (private:find-fusable-call (car children) table skipped)])
              (if sub
                (cons (car sub) (cons i (cdr sub)))
                (loop (cdr children) (+ i 1))))])))))

;the compound counterpart of a skeleton call node, located by the
;call's child-index path: compound list elements align with skeleton
;children positionally (that is the invariant pairs relies on). Improper
;compound tails count as one final element.
(define (private:find-compound-subtree compound path)
  (let walk ([tree compound] [remaining path])
    (cond
      [(null? remaining) tree]
      [(not (pair? tree)) #f]
      [else
        (let ([elements (private:compound-elements tree)])
          (and (< (car remaining) (length elements))
            (walk (list-ref elements (car remaining)) (cdr remaining))))])))

(define (private:compound-elements tree)
  (let loop ([rest tree])
    (cond
      [(null? rest) '()]
      [(pair? rest) (cons (car rest) (loop (cdr rest)))]
      [else (list rest)])))

;splice a partner skeleton in place of the call node inside its parent
;children (the partner built it with the call node's parent already)
(define (private:splice-skeleton! call-node new-node)
  (let ([parent (index-node-parent call-node)])
    (when (index-node? parent)
      (index-node-children-set! parent
        (map (lambda (c) (if (eq? c call-node) new-node c))
          (index-node-children parent))))))

;pure pair-tree replacement by identity
(define (private:replace-subtree tree old-sub new-sub)
  (cond
    [(eq? tree old-sub) new-sub]
    [(pair? tree)
      (cons (private:replace-subtree (car tree) old-sub new-sub)
        (private:replace-subtree (cdr tree) old-sub new-sub))]
    [else tree]))

) ; end library
