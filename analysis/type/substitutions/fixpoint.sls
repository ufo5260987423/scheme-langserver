(library (scheme-langserver analysis type substitutions fixpoint)
  (export 
    fixpoint-substitutions-for!
    fixpoint-note-group!
    fixpoint-find-calls
    fixpoint-clear-document!)
  (import
    (chezscheme)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier util)

    (scheme-langserver util dedupe)

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker))

;; P1: recursive-return fixpoint (设计来源： type.md §15 改进路线图，
;; P0-P3 已落地，见 §15.2 注记)。Two-phase
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

(define (private:now-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

;;hard budget: the pass must never dominate analysis time. The cost of a
;;group is dominated by its interpretation rounds, which on huge import/
;;match rule bodies (define-library-import.sls measured 69 s for ONE
;;document) can blow up to the interpreter's internal cartesian caps. A
;;document-level wall-clock budget caps the whole pass; a group that would
;;exceed it is restored to status quo and the remaining groups are skipped.
;;Two guards keep the pass from ever dominating analysis time: (1) a
;;static size cap -- measured blow-up groups (define-library-import.sls:
;;69 s for ONE document) have return-chain subtrees of 1500+ nodes while
;;ordinary recursive functions stay under ~200 (binary-search 121,
;;contain 150, define.sls 80), so groups whose return chains exceed 500
;;nodes are skipped outright; (2) a document-level wall-clock budget --
;;a group that would exceed it is restored to status quo and the
;;remaining groups are skipped.
(define PRIVATE-MAX-FIXPOINT-MS-PER-DOCUMENT 50)
(define PRIVATE-MAX-GROUP-RETURN-NODES 500)

;;Groups are NOT collected by this pass: the define/let type rules note
;;them (via fixpoint-note-group!) as they fire during the generator
;;walk -- see possible-fix-point-process:define in rules/define.sls and
;;possible-fix-point-process:named-let in rules/let.sls, mounted as the
;;cddr half of the rule chain in generator.sls's
;;establish-available-rules-from (the same two-phase chain idiom as the
;;identifier side's (do-nothing . begin-process): first proc before the
;;children walk, cddr after it). This pass only merges the noted groups
;;and solves them; solving stays behind the type-inference-advanced?
;;gate in construct-substitutions-for, while collection is always on
;;(it is a cheap local read of the just-stepped subtree).

(define private:budget-tag 'private:fixpoint-budget)

;;raw-group accumulator, keyed by document and consumed (then dropped)
;;by fixpoint-substitutions-for!. A side table rather than a document
;;record field keeps the record layout -- and the workspace fasl cache
;;format -- untouched.
(define private:raw-groups (make-eq-hashtable))

(define (fixpoint-note-group! document group)
  (hashtable-update! private:raw-groups document
    (lambda (groups) (cons group groups)) '()))

(define (fixpoint-clear-document! document)
  (hashtable-delete! private:raw-groups document))

;;every application-shaped node in body whose head is a leaf symbol,
;;as ((node . name) ...). NOT filtered by a name set: solve-group!
;;filters by the group's own names, and merge-groups only links groups
;;that call each other's DEFINED names, so unfiltered collection is
;;safe and drops the old all-define-names pre-pass entirely.
(define (fixpoint-find-calls body)
  ;;visited cuts RECURSION (not just re-reporting): the dereferenced
  ;;walk must terminate on shared/cyclic literals (#0=(a . #0#) in a
  ;;lambda body measured to hang forever otherwise) and must not
  ;;re-walk shared subtrees exponentially ((#1=(x) #1#)).
  (let loop ([nodes body] [visited (make-eq-hashtable)])
    (if (null? nodes)
      '()
      (let ([node (dereference-index-node (car nodes))])
        (if (hashtable-ref visited node #f)
          (loop (cdr nodes) visited)
          (begin
            (hashtable-set! visited node #t)
            (append
              (cond
                [(private-application-head node)
                 => (lambda (head) (list (cons node head)))]
                [else '()])
              (loop (index-node-children node) visited)
              (loop (cdr nodes) visited))))))))

;;the head symbol of node when node is application-shaped
(define (private-application-head node)
  (let* ([children (index-node-children node)]
      [head (and (pair? children) (car children))])
    (and head
      (let ([expr (annotation-stripped (index-node-datum/annotations head))])
        (and (symbol? expr) expr)))))

(define (private:budget-exceeded? ex)
  (eq? ex private:budget-tag))

(define (fixpoint-substitutions-for! document)
  (let ([raw (hashtable-ref private:raw-groups document '())])
    ;;consume once: a re-analyzed document must not accumulate duplicates
    (hashtable-delete! private:raw-groups document)
    (let ([deadline (+ (private:now-ms) PRIVATE-MAX-FIXPOINT-MS-PER-DOCUMENT)])
      (guard (ex [else
                  (unless (private:budget-exceeded? ex)
                    ;;never break the generator; a failed pass degrades to
                    ;;status quo with a diagnosis (budget exhaustion is silent)
                    (append-new-diagnoses
                      document
                      `(0 0 2
                        ,(string-append "Recursive fixpoint warning: "
                            (if (condition? ex) (condition-message ex) "unknown"))
                        "type" "type-fixpoint-warning")))
                  (void)])
        (for-each
          (lambda (group) (private:solve-group! group deadline))
          (private:merge-groups raw))))))
;; group = (kind names return-nodes call-nodes); call-node = (node . name)
(define (private:solve-group! group deadline)
  (when (< (private:now-ms) deadline)
    (let* ([names (cadr group)]
        [return-nodes (caddr group)]
        [my-calls
          (filter
            (lambda (call) (find (lambda (n) (eq? n (cdr call))) names))
            (cadddr group))]
        [call-nodes (map car my-calls)])
      (unless (or (null? call-nodes)
              ;;static guard: huge return chains blow interpretation up to
              ;;the interpreter's internal caps (measured: a 1500-node body
              ;;takes 69 s); ordinary recursive groups stay well under it
              (> (private:group-return-size return-nodes)
                PRIVATE-MAX-GROUP-RETURN-NODES))
        (let ([saved (map (lambda (n) (cons n (index-node-substitution-list n))) call-nodes)])
          (guard (ex [else
                      (private:restore-substitutions! saved)
                      (when (private:budget-exceeded? ex) (raise ex))])
            ;;R0: sever the cycle, interpret the pure base case, keep only
            ;;solved candidates (raw severed call nodes and unsolved parameter
            ;;references are placeholders, never estimates)
            (for-each
              (lambda (n) (index-node-substitution-list-set! n '()))
              call-nodes)
            (private:check-budget! deadline)
            (let ([r0 (private:interpret-union return-nodes)])
              (if (private:worthwhile? r0)
                (private:iterate! return-nodes call-nodes r0 0 deadline)
                (private:restore-substitutions! saved)))))))))

;;total number of index-nodes in the return chains (shared nodes counted
;;once); stops counting past limit+1
(define (private:group-return-size return-nodes)
  (let ([visited (make-eq-hashtable)])
    (fold-left
      (lambda (acc node) (private:count-subtree node visited acc (+ PRIVATE-MAX-GROUP-RETURN-NODES 1)))
      0
      return-nodes)))

(define (private:count-subtree node visited acc limit)
  (cond
    [(> acc limit) acc]
    [(not (index-node? node)) acc]
    [(hashtable-ref visited node #f) acc]
    [else
      (hashtable-set! visited node #t)
      (fold-left
        (lambda (a child) (private:count-subtree child visited a limit))
        (+ acc 1)
        (index-node-children node))]))

(define (private:check-budget! deadline)
  (when (>= (private:now-ms) deadline)
    (raise private:budget-tag)))

(define (private:iterate! return-nodes call-nodes current i deadline)
  (cond
    [(or (>= i PRIVATE-MAX-FIXPOINT-ROUNDS)
        (> (private:count-atoms current) PRIVATE-MAX-FIXPOINT-ATOMS))
      ;;cap fired: current is still a sound over-approx -- keep it
      (private:finalize! call-nodes current)]
    [else
      (for-each
        (lambda (n) (index-node-substitution-list-set! n current))
        call-nodes)
      ;;same filter: the estimate only ever contains solved atoms, so a
      ;;placeholder can never be cached into a "concrete" composite
      (let ([next (private:interpret-union return-nodes)])
        (private:check-budget! deadline)
        (if (equal? (private:sort-candidates next) (private:sort-candidates current))
          (private:finalize! call-nodes next)
          (private:iterate! return-nodes call-nodes next (+ i 1) deadline)))]))

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

(define (private:finalize! call-nodes final)
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
;; Group collection lives in the rules (see the header note)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
