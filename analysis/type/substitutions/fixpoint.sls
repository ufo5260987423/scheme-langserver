(library (scheme-langserver analysis type substitutions fixpoint)
  (export fixpoint-substitutions-for!)
  (import
    (chezscheme)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver util dedupe)

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker))

;; P1: recursive-return fixpoint. See
;; doc/analysis/type/type-inference-improvement-plan.md §1 (two-phase
;; re-interpretation, after orly issue #128 Option B, with Dialyzer-style
;; caps). Self/mutual recursive calls currently come back unsolved through
;; the interpreter's cycle cut and degrade to something?. For each
;; recursion group -- a (define (name ...)), a (define name (lambda ...))
;; or (define name (case-lambda ...)), or a named let -- we iterate:
;;   R0   sever the recursive calls (empty substitution) and interpret the
;;        body returns: pure base-case candidates.
;;   R_n  bind every recursive call to R_{n-1} and re-interpret.
;; Stop when R_{n+1} == R_n (candidate-set equality), a round/atom cap fires,
;; or an exception aborts (the graph is always left in a sound state: every
;; R_n is a sound over-approximation by monotonicity of interpretation in
;; the recursive-call binding). On success the final R REPLACES each
;; recursive call node's substitution list -- the children chain is exactly
;; the cyclic path that degraded to raw remainders; the converged estimate
;; R = F(R) is its sound solution (Dialyzer-PLT style) -- and hover/export
;; at both the call sites and the group signature see the concrete type.
;;
;; Groups whose call sites mention each other (mutual recursion, e.g.
;; binary-search <-> private-collect) are union-merged and solved jointly
;; with one shared running union.
;;
;; Scope: first-order direct/mutual recursion only (named-let, self-calling
;; and mutually calling defines), matching Pyright/TypeScript practice.
;; Higher-order recursion stays at status quo. Nested redefinition/
;; shadowing of a group name inside its own body is not distinguished (a
;; spurious R candidate may be added to a shadowed same-named call; it is a
;; candidate UNION, so the result stays a sound over-approximation).

(define PRIVATE-MAX-FIXPOINT-ROUNDS 6)
(define PRIVATE-MAX-FIXPOINT-ATOMS 32)

(define (fixpoint-substitutions-for! document)
  (guard (ex [else
              ;;never break the generator; a failed pass degrades to status quo
              (append-new-diagnoses
                document
                `(0 0 2
                  ,(string-append "Recursive fixpoint warning: "
                      (if (condition? ex) (condition-message ex) "unknown"))
                  "type" "type-fixpoint-warning"))
              (void)])
    (for-each
      private:solve-group!
      (private:merge-groups (private:collect-raw-groups document)))))

;; group = (kind names return-nodes call-nodes); call-node = (node . name)
(define (private:solve-group! group)
  (let* ([names (cadr group)]
      [return-nodes (caddr group)]
      [my-calls
        (filter
          (lambda (call) (find (lambda (n) (eq? n (cdr call))) names))
          (cadddr group))]
      [call-nodes (map car my-calls)])
    (unless (null? call-nodes)
      (let ([saved (map (lambda (n) (cons n (index-node-substitution-list n))) call-nodes)])
        (guard (ex [else (private:restore-substitutions! saved)])
          ;;R0: sever the cycle, interpret the pure base case, keep only
          ;;solved candidates (raw severed call nodes and unsolved parameter
          ;;references are placeholders, never estimates)
          (for-each
            (lambda (n) (index-node-substitution-list-set! n '()))
            call-nodes)
          (let ([r0 (private:interpret-union return-nodes)])
            (if (private:worthwhile? r0)
              (private:iterate! return-nodes call-nodes saved r0 0)
              (private:restore-substitutions! saved))))))))

(define (private:iterate! return-nodes call-nodes saved current i)
  (cond
    [(or (>= i PRIVATE-MAX-FIXPOINT-ROUNDS)
        (> (private:count-atoms current) PRIVATE-MAX-FIXPOINT-ATOMS))
      ;;cap fired: current is still a sound over-approx -- keep it
      (private:finalize! call-nodes saved current)]
    [else
      (for-each
        (lambda (n) (index-node-substitution-list-set! n current))
        call-nodes)
      ;;same filter: the estimate only ever contains solved atoms, so a
      ;;placeholder can never be cached into a "concrete" composite
      (let ([next (private:interpret-union return-nodes)])
        (if (equal? (private:sort-candidates next) (private:sort-candidates current))
          (private:finalize! call-nodes saved next)
          (private:iterate! return-nodes call-nodes saved next (+ i 1))))]))

(define (private:interpret-union return-nodes)
  (fold-left
    (lambda (acc r)
      (dedupe-deduped acc
        (filter
          ;;something? is the placeholder itself: as an estimate it only
          ;;poisons every consumer into the top type -- never keep it
          (lambda (c) (not (eq? 'something? c)))
          (filter type:solved? (type:interpret-result-list r)))))
    '()
    return-nodes))

(define (private:worthwhile? candidates)
  (not (null? candidates)))

(define (private:finalize! call-nodes saved final)
  ;;Replace (not append): the children-chain substitution is exactly the
  ;;cyclic path that used to degrade to raw remainders; the converged
  ;;estimate R = F(R) is its sound solution. The saved chain is dropped --
  ;;the call node now types directly to the fixpoint candidates.
  (for-each
    (lambda (n) (index-node-substitution-list-set! n final))
    call-nodes))

(define (private:restore-substitutions! saved)
  (for-each
    (lambda (pair) (index-node-substitution-list-set! (car pair) (cdr pair)))
    saved))

(define (private:count-atoms expression)
  (cond
    [(null? expression) 0]
    [(list? expression)
      (fold-left + 0 (map private:count-atoms expression))]
    [else 1]))

(define (private:sort-candidates candidates)
  (list-sort
    (lambda (a b)
      (string<? (private:candidate->string a) (private:candidate->string b)))
    candidates))

(define (private:candidate->string candidate)
  (guard (ex [else "#raw"])
    (inner:type->string candidate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; raw group = (kind names return-nodes call-nodes); call-node = (node . name)
;; kind 'define groups may merge (mutual recursion); 'named-let never does.
;; Call collection keeps EVERY call whose head resolves to any document-level
;; define name (not just self-calls): the merge pass uses these name sets to
;; discover mutual recursion, and solving binds only calls whose name is in
;; the merged group's own name set.
(define (private:collect-raw-groups document)
  (let* ([visited (make-eq-hashtable)]
      [all-names (private:collect-define-names document (make-eq-hashtable))])
    (apply append
      (map
        (lambda (root) (private:walk-node document all-names root visited))
        (document-index-node-list document)))))

;;first pass: every top-level-ish define name in the document (pair form and
;;variable form), for cross-call collection
(define (private:collect-define-names document visited)
  (let walk ([nodes (document-index-node-list document)] [acc '()])
    (cond
      [(null? nodes) acc]
      [else
        (let* ([node (dereference-index-node (car nodes))]
            [children (if (index-node? node) (index-node-children node) '())]
            [head-expr
              (and (index-node? node)
                (not (null? children))
                (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
                  (and (symbol? e) e)))]
            [name
              (and (index-node? node)
                (eq? 'define head-expr)
                (>= (length children) 3)
                (let ([name-expr
                        (annotation-stripped
                          (index-node-datum/annotations (cadr children)))])
                  (cond
                    [(and (list? name-expr) (not (null? name-expr)) (symbol? (car name-expr)))
                      (car name-expr)]
                    [(symbol? name-expr) name-expr]
                    [else #f])))])
          (walk
            (append children (cdr nodes))
            (if (and name (not (memq name acc)))
              (cons name acc)
              acc)))])))

(define (private:walk-node document all-names node visited)
  (let ([node (dereference-index-node node)])
    (cond
      [(or (not (index-node? node)) (hashtable-ref visited node #f))
        '()]
      [else
        (hashtable-set! visited node #t)
        (let ([group (private:group-for-node document all-names node)]
            [child-groups
              (apply append
                (map
                  (lambda (child) (private:walk-node document all-names child visited))
                  (index-node-children node)))])
          (if group (cons group child-groups) child-groups))])))

(define (private:group-for-node document all-names node)
  (let* ([children (index-node-children node)]
      [head-expr
        (and (not (null? children))
          (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
            (and (symbol? e) e)))])
    (cond
      [(and (eq? 'define head-expr) (>= (length children) 3))
        (private:define-group document all-names children)]
      [(and (eq? 'let head-expr) (>= (length children) 4))
        (private:named-let-group document all-names children)]
      [else #f])))

(define (private:define-group document all-names children)
  ;;(define (name . params) body ...) or (define name value)
  (let* ([name-form (cadr children)]
      [name-expr (annotation-stripped (index-node-datum/annotations name-form))])
    (cond
      ;;(define (name . params) body ...)
      [(and (list? name-expr) (not (null? name-expr)) (symbol? (car name-expr)))
        (let ([name (car name-expr)]
            [body (cddr children)])
          (private:make-group 'define (list name) all-names name body))]
      ;;(define name (lambda ...)) / (define name (case-lambda ...))
      [(symbol? name-expr)
        (let ([value-node (and (>= (length children) 3) (caddr children))])
          (if (index-node? value-node)
            (let* ([value-kids (index-node-children value-node)]
                [value-head-expr
                  (and (not (null? value-kids))
                    (let ([e (annotation-stripped (index-node-datum/annotations (car value-kids)))])
                      (and (symbol? e) e)))])
              (cond
                [(eq? 'lambda value-head-expr)
                  (let ([body (cddr value-kids)])
                    (private:make-group 'define (list name-expr) all-names name-expr body))]
                [(eq? 'case-lambda value-head-expr)
                  ;;each clause is an independent return chain; solve jointly
                  (let ([clauses (cdr value-kids)])
                    (private:make-case-lambda-group document all-names name-expr clauses))]
                [else #f]))
            #f))]
      [else #f])))

(define (private:make-case-lambda-group document all-names name clauses)
  (let ([return-nodes
          (filter index-node?
            (map
              (lambda (clause)
                ;;clause children: (params body ...) -- body is cdr
                (let ([body (cdr (index-node-children clause))])
                  (if (null? body) #f (list-ref body (- (length body) 1)))))
              clauses))]
      [calls
        (fold-left
          (lambda (acc clause)
            (append acc
              (private:find-calls #f all-names (cdr (index-node-children clause))
                (make-eq-hashtable))))
          '()
          clauses)])
    (if (null? calls)
      #f
      `(define (,name) ,return-nodes ,calls))))

(define (private:named-let-group document all-names children)
  ;;children: (let loop ((binding ...) ...) body ...)
  ;;named-let groups stand alone (never merge) and use their own name as
  ;;the call filter
  (let ([name-node (cadr children)])
    (let ([name
            (let ([e (annotation-stripped (index-node-datum/annotations name-node))])
              (and (symbol? e) e))])
      (private:make-group 'named-let
        (if name (list name) '())
        (if name (list name) '())
        name
        (cdddr children)))))

(define (private:make-group kind names call-names name body)
  (if (and name (not (null? body)))
    (let ([calls (private:find-calls #f call-names body (make-eq-hashtable))])
      (if (null? calls)
        #f
        `(,kind ,names ,(list (list-ref body (- (length body) 1))) ,calls)))
    #f))

;;find application nodes in body whose head symbol is in call-names.
;;Returns ((node . name) ...). document = #f skips the resolution check; a
;;false positive only adds a sound candidate union at a same-named call.
(define (private:find-calls document call-names body visited)
  (let loop ([nodes body])
    (if (null? nodes)
      '()
      (let* ([node (dereference-index-node (car nodes))]
          [children (if (index-node? node) (index-node-children node) '())]
          [here
            (and (index-node? node)
              (not (hashtable-ref visited node #f))
              (private:group-call? document node call-names))])
        (if (index-node? node) (hashtable-set! visited node #t) (void))
        (append
          (if here (list (cons node (car here))) '())
          (loop children)
          (loop (cdr nodes)))))))

;;returns (name) when node's head is a symbol in call-names and (when
;;document is given) resolves to a reference with a matching root ancestor
(define (private:group-call? document node call-names)
  (let* ([children (index-node-children node)]
      [head (and (pair? children) (car children))])
    (and head
      (let ([expr (annotation-stripped (index-node-datum/annotations head))])
        (and (symbol? expr)
          (find (lambda (n) (eq? n expr)) call-names)
          (or (not document)
            (find
              (lambda (ref)
                (find
                  (lambda (top) (eq? expr (identifier-reference-identifier top)))
                  (root-ancestor ref)))
              (find-available-references-for document node expr)))
          (cons expr '()))))))

;;Union-merge raw groups connected by cross-calls (mutual recursion).
;;Only 'define groups participate; 'named-let groups are always standalone.
(define (private:merge-groups raw-groups)
  (let* ([define-groups (filter (lambda (g) (eq? 'define (car g))) raw-groups)]
      [other-groups (filter (lambda (g) (not (eq? 'define (car g)))) raw-groups)]
      [merged (private:merge-pass define-groups)])
    (append other-groups (map private:normalize-group merged))))

(define (private:merge-pass groups)
  ;;repeat until fixpoint (mutual recursion can be transitive)
  (let ([merged (private:merge-once groups)])
    (if (= (length merged) (length groups))
      merged
      (private:merge-pass merged))))

(define (private:merge-once groups)
  (cond
    [(null? groups) '()]
    [else
      (let* ([first (car groups)]
          [first-names (cadr first)]
          [first-call-names (map cdr (cadddr first))]
          [related?
            (lambda (g)
              (let ([g-names (cadr g)]
                  [g-call-names (map cdr (cadddr g))])
                (or
                  (find
                    (lambda (n) (find (lambda (fn) (eq? fn n)) first-names))
                    g-call-names)
                  (find
                    (lambda (n) (find (lambda (gn) (eq? gn n)) g-names))
                    first-call-names))))])
        (cons
          (private:join-groups (cons first (filter related? (cdr groups))))
          (private:merge-once
            (filter (lambda (g) (not (related? g))) (cdr groups)))))]))

(define (private:join-groups groups)
  (if (null? (cdr groups))
    (car groups)
    `(define
      ,(apply append (map cadr groups))
      ,(apply append (map caddr groups))
      ,(apply append (map cadddr groups)))))

;;dedupe call nodes shared between merged clauses
(define (private:normalize-group group)
  (let ([seen (make-eq-hashtable)])
    `(,(car group)
      ,(cadr group)
      ,(caddr group)
      ,(filter
        (lambda (call)
          (if (hashtable-ref seen (car call) #f)
            #f
            (begin (hashtable-set! seen (car call) #t) #t)))
        (cadddr group)))))

)
