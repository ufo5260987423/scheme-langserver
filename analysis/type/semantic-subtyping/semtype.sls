(library (scheme-langserver analysis type semantic-subtyping semtype)
  (export
    make-sem:type sem:type? sem:type-base sem:type-pair sem:type-arrow sem:type-vector
    sem:convert sem:base-symbols
    sem:clear-memos!
    sem:subtype? sem:equivalent?
    sem:union sem:intersect sem:diff sem:complement sem:is-empty?
    sem:universe sem:empty
    sem:restrict sem:remove)

  (import
    (chezscheme)

    (scheme-langserver analysis identifier reference))

;route-4 (doc/analysis/type/type.md §16, 原 precedents 调研 §4.6): a
;set-theoretic internal representation for the type-equivalence layer.
;
;Phase 2 adds the pair/vector sorts and the inner:list? sugar:
;  pair    = primitive product sort (car x cdr atoms)
;  list(T) =def null ∪ pair(T, list(T))   -- built as a box cycle
;  vector  = uniform-element sort
;
;Representation choices (documented deviations from the §4.6 sketch):
;- Shards use simplified-DNF clause lists, not BDDs: a shard is a list of
;  clauses (P . N) where P/N are lists of atoms; disjunction of clauses,
;  conjunction within a clause. LSP-scale types stay small; the BDD upgrade
;  path (semtype's bdd.bal / Frisch §11.3) remains open.
;- Recursive types (the list sugar) are box cycles, and is-empty? carries a
;  depth budget: exhaustion returns "inhabited" (optimistic over-
;  approximation, Dialyzer depth-k style -- may miss an emptiness, never
;  asserts a false positive).
;- The arrow sort carries real clauses (lambda conversion lands there);
;  emptiness uses the full Thm 2.17 subset-splitting rule, memoized per
;  thread. A two-point #f/'ALL sentinel remains only for complement
;  totality at the sort level.
;
;Bit semantics: each PRIVATE-BASE-LEAVES predicate owns one bit; compound
;predicates expand to the union of their leaf bits (numeric tower).
;`inner:void?` is a positive-only marker and is excluded from the base
;complement universe.

;; base leaf predicates -- each owns one bit, in declaration order
(define PRIVATE-BASE-LEAVES
  '(null? boolean? char? string? symbol?
    fixnum? bignum? ratnum? flonum? cflonum?
    eof-object? void? procedure?))

(define PRIVATE-BIT-ALIST
  (let loop ([leaves PRIVATE-BASE-LEAVES] [i 0] [acc '()])
    (if (null? leaves)
      (reverse acc)
      (loop (cdr leaves) (+ i 1) (cons (cons (car leaves) i) acc)))))

(define (private-leaf-bit symbol)
  (cdr (assq symbol PRIVATE-BIT-ALIST)))

(define (private-leaf? symbol)
  (and (assq symbol PRIVATE-BIT-ALIST) #t))

(define PRIVATE-FULL-MASK
  (- (expt 2 (length PRIVATE-BASE-LEAVES)) 1))

(define PRIVATE-BASE-UNIVERSE-MASK
  (logand PRIVATE-FULL-MASK
    (lognot (expt 2 (private-leaf-bit 'void?)))))

;compound base predicates expand to a union of leaf bits
(define PRIVATE-BASE-EXPANSIONS
  (let ([mask-of (lambda (leaves)
                   (fold-left
                     (lambda (acc leaf)
                       (logior acc (expt 2 (private-leaf-bit leaf))))
                     0
                     leaves))])
    `((inner:void? . ,(expt 2 (private-leaf-bit 'void?)))
      (integer? . ,(mask-of '(fixnum? bignum?)))
      (rational? . ,(mask-of '(fixnum? bignum? ratnum?)))
      (real? . ,(mask-of '(fixnum? bignum? ratnum? flonum?)))
      (complex? . ,(mask-of '(fixnum? bignum? ratnum? flonum? cflonum?)))
      (number? . ,(mask-of '(fixnum? bignum? ratnum? flonum? cflonum?))))))

;per-thread memos (AGENTS.md hazard: parameters are inherited across
;fork-thread, so the cell is tagged with the thread id and rebuilt on
;mismatch -- the reference.sls:94 pattern). Two layers:
;- convert-cache: predicate tree (equal?-keyed) -> sem:type, so repeated
;  judgments over the same DSL trees share sem:type identities;
;- empty-memo: sem:type identity (eq?-keyed) -> ((depth . result)...),
;  so the is-empty? recursion never recomputes a shard query.
(define private:convert-cache-cell (make-thread-parameter #f))
(define private:empty-memo-cell (make-thread-parameter #f))

(define (private:cell-table cell-name make)
  (let ([cell (cell-name)])
    (if (and (pair? cell) (eqv? (car cell) (get-thread-id)))
      (cdr cell)
      (let ([fresh (cons (get-thread-id) (make))])
        (cell-name fresh)
        (cdr fresh)))))

(define (private:convert-cache)
  (private:cell-table private:convert-cache-cell
    (lambda () (make-hashtable equal-hash equal?))))

(define (private:empty-memo)
  (private:cell-table private:empty-memo-cell
    (lambda () (make-eq-hashtable))))

(define (sem:clear-memos!)
  (private:convert-cache-cell #f)
  (private:empty-memo-cell #f)
  (void))

(define-record-type sem:type
  (fields
    (immutable base)     ; fixnum bitset over base sorts (union of the sort)
    (immutable pair)     ; shard: clause list | #f (empty sort)
    (immutable arrow)    ; shard: clause list | #f (empty sort)
    (immutable vector))) ; shard: clause list | #f (empty sort)

;; ---------------------------------------------------------------------
;; sort universes
;; ---------------------------------------------------------------------

;a finite inhabited component for "any value" inside general atoms:
;base-universe only (no pair/vector/arrow shards). For EMPTINESS checks
;an inhabited component is all that matters, and the depth budget covers
;the lost nesting.
(define (private-top-component)
  (make-sem:type PRIVATE-BASE-UNIVERSE-MASK #f #f #f))

(define (private-all-pairs)
  ;one clause: the fully-general pair atom, no negatives
  (list (cons (list (vector (private-top-component) (private-top-component))) '())))

(define (private-all-vectors)
  (list (cons (list (private-top-component)) '())))

(define (private-all-arrows)
  (list (cons (list (vector (private-top-component) (private-top-component))) '())))

(define (sem:universe)
  (make-sem:type PRIVATE-BASE-UNIVERSE-MASK (private-all-pairs) (private-all-arrows) (private-all-vectors)))

(define (sem:empty)
  (make-sem:type 0 #f #f #f))

;; ---------------------------------------------------------------------
;; conversion: DSL predicate trees -> sem:type
;; ---------------------------------------------------------------------

;returns a sem:type, or #f. Three distinct meanings are collapsed into
;#f ON PURPOSE, and the API contract pins them apart:
;  (a) unknown / something?   -- no information; judgments short-circuit
;      permissively (never a false positive);
;  (b) unsupported shapes     -- same treatment as (a) by design;
;  (c) the universal type TOP -- NOT #f: it is a first-class value,
;      (sem:universe), and the five operations are total on it.
;Memoized per thread on the input tree so repeated judgments share
;sem:type identities (which the eq?-keyed empty-memo relies on).
(define sem:convert
  (case-lambda
    [(expression)
      (let ([cached (hashtable-ref (private:convert-cache) expression 'PRIVATE-MISS)])
        (if (eq? cached 'PRIVATE-MISS)
          (let ([result (private:convert expression)])
            (hashtable-set! (private:convert-cache) expression result)
            result)
          cached))]))

(define private:convert
  (case-lambda
    [(expression)
      (cond
        [(identifier-reference? expression)
          (sem:convert (identifier-reference-identifier expression))]
        [(symbol? expression)
          (cond
            [(eq? expression 'something?) #f]
            [(assq expression PRIVATE-BASE-EXPANSIONS)
              => (lambda (entry) (make-sem:type (cdr entry) #f #f #f))]
            [(private-leaf? expression)
              (make-sem:type
                (expt 2 (private-leaf-bit expression))
                #f #f #f)]
            [else #f])]
        [(list? expression)
          (cond
            [(null? expression) #f]
            [(eq? (car expression) 'inner:pair?)
              (private-convert-pair (cadr expression) (caddr expression))]
            [(eq? (car expression) 'inner:vector?)
              (private-convert-vector (cdr expression))]
            [(eq? (car expression) 'inner:list?)
              (private-convert-list (cdr expression))]
            [(and (= (length expression) 3) (eq? (cadr expression) '<-))
              (private-convert-lambda (car expression) (caddr expression))]
            [else #f])]
        [else #f])]))

(define (private-filter-modifiers items)
  (filter
    (lambda (x) (and (not (eq? x '...)) (not (eq? x '**1))))
    items))

(define (private-convert-pair car-expr cdr-expr)
  (let ([car-sem (sem:convert car-expr)]
      [cdr-sem (sem:convert cdr-expr)])
    (and car-sem cdr-sem
      (make-sem:type
        0
        (list (cons (list (vector car-sem cdr-sem)) '()))
        #f #f))))

(define (private-convert-vector items)
  (let ([elem
          (fold-left
            (lambda (acc item)
              (and acc
                (let ([c (sem:convert item)])
                  (and c (sem:intersect acc c)))))
            (sem:universe)
            (private-filter-modifiers items))])
    (and elem
      (make-sem:type 0 #f #f (list (cons (list elem) '()))))))

;list(T) = null ∪ pair(T, box-self); the box closes the recursion and
;is-empty?'s depth budget keeps queries terminating.
(define (private-list-of elem-sem)
  (let* ([box (box #f)]
        [self
          (make-sem:type
            (expt 2 (private-leaf-bit 'null?))
            (list (cons (list (vector elem-sem box)) '()))
            #f #f)])
    (set-box! box self)
    self))

(define (private-convert-list items)
  (let ([elem
          (fold-left
            (lambda (acc item)
              (and acc
                (let ([c (sem:convert item)])
                  (and c (sem:intersect acc c)))))
            (sem:universe)
            (private-filter-modifiers items))])
    (and elem (private-list-of elem))))

;(ret <- params): multi-arg domains are approximated by the intersection
;of the parameter types (the semtype tuple-desugaring approximation);
;any undetermined component makes the whole conversion permissive.
(define (private-convert-lambda ret-expr params-expr)
  (let ([ret-sem (sem:convert ret-expr)])
    (and ret-sem
      (let ([dom
              (fold-left
                (lambda (acc param)
                  (and acc
                    (let ([c (sem:convert param)])
                      (and c (sem:intersect acc c)))))
                (sem:universe)
                params-expr)])
        (and dom
          (make-sem:type 0 #f
            (list (cons (list (vector dom ret-sem)) '()))
            #f))))))

;; ---------------------------------------------------------------------
;; shard algebra (simplified DNF; #f = empty shard)
;; ---------------------------------------------------------------------

(define (private-shard-union a b)
  (cond
    [(not a) b]
    [(not b) a]
    [else (append a b)]))

(define (private-shard-intersect a b)
  (cond
    [(not a) #f]
    [(not b) #f]
    [else
      (apply append
        (map
          (lambda (c1)
            (map
              (lambda (c2)
                (cons (append (car c1) (car c2))
                  (append (cdr c1) (cdr c2))))
              b))
          a))]))

;complement within one sort; sort-universe is a one-clause shard holding
;the fully-general atom.
(define (private-shard-complement s sort-universe)
  (cond
    [(not s) sort-universe]
    [else
      (fold-left
        (lambda (acc clause)
          (private-shard-intersect acc (private-clause-complement clause sort-universe)))
        sort-universe
        s)]))

;¬(∧P ∧ ∧¬N) = ∨_{{},{a}} (a∈P) ++ ∨_{{n},{}} (n∈N); clause ({},{})
;is "the whole sort" and complements to the empty shard.
(define (private-clause-complement clause sort-universe)
  (let ([P (car clause)]
      [N (cdr clause)])
    (if (and (null? P) (null? N))
      #f
      (append
        (map (lambda (a) (cons '() (list a))) P)
        (map (lambda (n) (cons (list n) '())) N)))))

(define (private-shard-diff a b sort-universe)
  (private-shard-intersect a (private-shard-complement b sort-universe)))

;; ---------------------------------------------------------------------
;; top-level five operations (componentwise across sorts)
;; ---------------------------------------------------------------------

(define (sem:union s t)
  (make-sem:type
    (logior (sem:type-base s) (sem:type-base t))
    (private-shard-union (sem:type-pair s) (sem:type-pair t))
    (private-shard-union (sem:type-arrow s) (sem:type-arrow t))
    (private-shard-union (sem:type-vector s) (sem:type-vector t))))

(define (sem:intersect s t)
  (make-sem:type
    (logand (sem:type-base s) (sem:type-base t))
    (private-shard-intersect (sem:type-pair s) (sem:type-pair t))
    (private-shard-intersect (sem:type-arrow s) (sem:type-arrow t))
    (private-shard-intersect (sem:type-vector s) (sem:type-vector t))))

(define (sem:diff s t)
  (make-sem:type
    (logand (sem:type-base s) (lognot (sem:type-base t)))
    (private-shard-diff (sem:type-pair s) (sem:type-pair t) (private-all-pairs))
    (private-shard-diff (sem:type-arrow s) (sem:type-arrow t) (private-all-arrows))
    (private-shard-diff (sem:type-vector s) (sem:type-vector t) (private-all-vectors))))

(define (sem:complement s)
  (make-sem:type
    (logand PRIVATE-BASE-UNIVERSE-MASK (lognot (sem:type-base s)))
    (private-shard-complement (sem:type-pair s) (private-all-pairs))
    (private-shard-complement (sem:type-arrow s) (private-all-arrows))
    (private-shard-complement (sem:type-vector s) (private-all-vectors))))

;; ---------------------------------------------------------------------
;; emptiness (the entry point of every judgment)
;; ---------------------------------------------------------------------

(define sem:is-empty?
  (case-lambda
    [(t) (sem:is-empty? t 8)]
    [(t depth)
      (let* ([memo (private:empty-memo)]
          [entries (hashtable-ref memo t '())]
          [hit (assv depth entries)])
        (if hit
          (cdr hit)
          (let ([result
                  (and (zero? (sem:type-base t))
                    (private-pair-shard-empty? (sem:type-pair t) depth)
                    (private-arrow-shard-empty? (sem:type-arrow t) depth)
                    (private-vector-shard-empty? (sem:type-vector t) depth))])
            (hashtable-set! memo t (cons (cons depth result) entries))
            result)))]))

(define (private-powerset xs)
  (if (null? xs)
    '(())
    (let ([rest (private-powerset (cdr xs))])
      (append rest (map (lambda (s) (cons (car xs) s)) rest)))))

;a pair shard is empty iff every clause is empty; a clause (P,N) is empty
;iff for ALL N'⊆N the car side OR the cdr side is empty (primer Step 4).
;The negative atoms split: N' guards the car side, N∖N' guards the cdr
;side -- a negative pair (t',s') only excludes a value when BOTH the car
;is in t' AND the cdr is in s'.
(define (private-pair-shard-empty? shard depth)
  (or (not shard)
    (null? shard)
    (and (private-pair-clause-empty? (car shard) depth)
      (private-pair-shard-empty? (cdr shard) depth))))

(define (private-pair-clause-empty? clause depth)
  (let ([P (car clause)]
      [N (cdr clause)])
    (for-all
      (lambda (N-sub)
        (or (private-pair-side-empty? P N-sub 0 depth)
          (private-pair-side-empty? P (private-set-difference N N-sub) 1 depth)))
      (private-powerset N))))

(define (private-set-difference xs ys)
  (filter (lambda (x) (not (member x ys))) xs))

;pair atoms may hold boxes (the inner:list? recursion cycle) -- deref.
(define (private-atom-component atom slot)
  (let ([v (vector-ref atom slot)])
    (if (box? v) (unbox v) v)))

(define (private-pair-side-empty? P N-sub slot depth)
  (if (<= depth 0)
    #f ;depth budget exhausted: optimistic, over-approximation
    (sem:is-empty?
      (fold-left sem:intersect (sem:universe)
        (append
          (map (lambda (a) (private-atom-component a slot)) P)
          (map (lambda (n) (sem:complement (private-atom-component n slot))) N-sub)))
      (- depth 1))))

;a vector shard is empty iff every clause is empty; a clause (P,N) over
;element types is empty iff (∩P) ∖ (∪N) is empty -- one sort, so the
;negatives subtract directly (no N'/N∖N' split needed).
(define (private-vector-shard-empty? shard depth)
  (or (not shard)
    (null? shard)
    (and (private-vector-clause-empty? (car shard) depth)
      (private-vector-shard-empty? (cdr shard) depth))))

(define (private-vector-clause-empty? clause depth)
  (if (<= depth 0)
    #f
    (let ([pos (fold-left sem:intersect (sem:universe) (car clause))]
        [neg (fold-left sem:union (sem:empty) (cdr clause))])
      (sem:is-empty? (sem:diff pos neg) (- depth 1)))))

;arrow sort, FULL Thm 2.17 (Frisch LICS'02 / Castagna primer Step 4):
;a clause (P,N) is empty iff for SOME negative arrow n = t'→s' in N:
;  dom(n) <: ∨dom(P)   -- n's domain covers every positive domain, and
;  for EVERY P'⊆P:  dom(n) <: ∨dom(P')  ∨  ∩cod(P∖P') <: cod(n)
;The subset split is the completeness the old conservative single rule
;gave up: a combination of positive arrows may subtype n even when no
;single one does. Recursive checks inherit the depth budget.
(define (private-arrow-shard-empty? shard depth)
  (or (not shard)
    (null? shard)
    (and (private-arrow-clause-empty? (car shard) depth)
      (private-arrow-shard-empty? (cdr shard) depth))))

(define (private-arrow-clause-empty? clause depth)
  (if (<= depth 0)
    #f
    (let ([P (car clause)]
        [N (cdr clause)])
      (and (not (null? N))
        (exists
          (lambda (n)
            (let ([n-dom (vector-ref n 0)]
                [n-cod (vector-ref n 1)])
              (and (private-subtype-at? n-dom (private-dom-union P) depth)
                (for-all
                  (lambda (P-sub)
                    (or (private-subtype-at? n-dom (private-dom-union P-sub) depth)
                      (private-subtype-at?
                        (private-cod-intersection (private-set-difference P P-sub))
                        n-cod
                        depth)))
                  (private-powerset P)))))
          N)))))

(define (private-subtype-at? s t depth)
  (or (not s) (not t)
    (sem:is-empty? (sem:diff s t) (- depth 1))))

(define (private-dom-union arrows)
  (fold-left
    (lambda (acc a) (sem:union acc (vector-ref a 0)))
    (sem:empty)
    arrows))

(define (private-cod-intersection arrows)
  (fold-left
    (lambda (acc a) (sem:intersect acc (vector-ref a 1)))
    (sem:universe)
    arrows))

;; ---------------------------------------------------------------------
;; judgments
;; ---------------------------------------------------------------------

;#f on either side means "undetermined" -- stay permissive (never a
;false positive), mirroring how something? flows through type:->? today.
(define (sem:subtype? s t)
  (or (not s) (not t)
    (sem:is-empty? (sem:diff s t))))

(define (sem:equivalent? s t)
  (and (sem:subtype? s t) (sem:subtype? t s)))

;the predicate symbols set in a type's base bits, for display.
(define (sem:base-symbols t)
  (let ([base (sem:type-base t)])
    (fold-left
      (lambda (acc p)
        (if (zero? (logand base (expt 2 (cdr p))))
          acc
          (cons (car p) acc)))
      '()
      PRIVATE-BIT-ALIST)))

;occurrence-typing hooks (type.md §16, 原 precedents §2.5).
(define (sem:restrict s t)
  (sem:intersect s t))

(define (sem:remove s t)
  (sem:diff s t))
)
