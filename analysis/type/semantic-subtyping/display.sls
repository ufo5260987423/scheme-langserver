(library (scheme-langserver analysis type semantic-subtyping display)
  (export sem:compact-display)

  (import
    (chezscheme)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type semantic-subtyping semtype))

;display compaction for hover (phase 1 of the display-quality roadmap,
;see doc/analysis/type/type.md §15): candidate type trees are merged into a
;compact, deterministic view. The display guarantee is TWO-ORDERED:
;
;  (a) VALUE order (soundness): display(C) >= union(C) -- allowed to be
;      wider, never narrower. Checked per position with the system's
;      own subtype semantics (sem:subtype? / sem:equivalent? on
;      convertible trees).
;  (b) KNOWLEDGE order (informativeness): the something? sentinel is
;      "no information", NOT honest top -- sem:convert fails on any
;      tree containing it, so such trees only ever merge with an
;      EQUAL tree, never absorb or get absorbed by a known type.
;      (Value semantics alone would allow something? -- it IS the top
;      of type:->? -- and hover would collapse every signature into
;      "unknown", destroying exactly the information the candidate
;      list exists to show.)
;
;So the "definitional union" product contract is preserved: anti-chain
;absorption is a semantic equality on known types (the absorbed
;candidate's values are a subset of the covering candidate's), dedup is
;equality, and truncation
;only hides representatives behind a total count, never deletes values.
;Deletion under (a)+(b) never loses information: what remains is at
;least as wide AND at least as known.
;
;Phase 1 covers: semantic dedup (stage 1), anti-chain reduction (stage
;2), specificity ordering + truncation (stage 4). Factorized slot-union
;rendering (stage 3) is deliberately left to phase 2.
;
;Definitions are ordered strictly bottom-up: this Chez configuration
;does not allow forward references between library-body definitions.
;
;Input: a list of candidate type trees (as produced by
;type:interpret-result-list). Output: (values items total truncated?),
;where an item is
;  (raw tree)          -- not sem-convertible; render as-is
;  (rep tree count)    -- representative covering `count` candidates
;`total` counts ALL input candidates; truncated? is #t when items were
;cut at max-lines.

(define (private-bit-count n)
  (if (zero? n)
    0
    (+ (logand n 1) (private-bit-count (ash n -1)))))

(define (private-tree-size tree)
  (cond
    [(pair? tree) (+ 1 (private-tree-size (car tree)) (private-tree-size (cdr tree)))]
    [else 1]))

;stage 4 helper: specificity ordering over classes (shape
;(tree sem . count)). Deterministic heuristic, most specific first:
;  1. generality of the base sort (bit count ascending);
;  2. types with concrete sorts (pair/arrow/vector) before base-only;
;  3. smaller trees first.
;list-sort is stable, so equal scores keep input order.
(define (private-score class)
  (let ([sem (cadr class)])
    (list
      (private-bit-count (sem:type-base sem))
      (+
        (if (sem:type-pair sem) 0 1)
        (if (sem:type-arrow sem) 0 1)
        (if (sem:type-vector sem) 0 1))
      (private-tree-size (car class)))))

(define (private-specificity<? a b)
  (let ([sa (private-score a)]
      [sb (private-score b)])
    (cond
      [(< (car sa) (car sb)) #t]
      [(> (car sa) (car sb)) #f]
      [(< (cadr sa) (cadr sb)) #t]
      [(> (cadr sa) (cadr sb)) #f]
      [(< (caddr sa) (caddr sb)) #t]
      [else #f])))

;stage 1 inner loop: fold one syntax-hash bucket into equivalence
;classes; class shape (tree sem . count).
(define (private-merge-bucket entries)
  (let loop ([rest entries] [classes '()])
    (if (null? rest)
      classes
      (let* ([tree (caar rest)]
          [sem (cdar rest)]
          [hit
            (find
              (lambda (c) (sem:equivalent? sem (cadr c)))
              classes)])
        (if hit
          (loop (cdr rest)
            (map
              (lambda (c)
                (if (eq? c hit)
                  (cons (car c) (cons (cadr c) (+ 1 (cddr c))))
                  c))
              classes))
          (loop (cdr rest)
            (cons (cons tree (cons sem 1)) classes)))))))

;stage 2: anti-chain reduction. A class is absorbed when its type is a
;strict subtype of another class's type -- its values are then a subset
;of the covering union member, so dropping it preserves the union
;exactly. Counts of absorbed classes fold into the covering class. Two
;passes: absorbees are dropped outright, coverers collect the counts (a
;coverer may appear after its absorbee in the input).
(define (private-antichain classes)
  (filter ;chezscheme here does not bind filter-map
    (lambda (x) x)
    (map
      (lambda (current)
        (let ([sem (cadr current)])
          (if (find
                (lambda (other)
                  (and (not (eq? other current))
                    (sem:subtype? sem (cadr other))
                    (not (sem:equivalent? sem (cadr other)))))
                classes)
            #f
            (cons (car current)
              (cons sem
                (+ (cddr current)
                  (fold-left
                    (lambda (acc other)
                      (if (and (not (eq? other current))
                          (sem:subtype? (cadr other) sem)
                          (not (sem:equivalent? (cadr other) sem)))
                        (+ acc (cddr other))
                        acc))
                    0
                    classes)))))))
      classes)))

;stage 1.5 (P2 / 1A): pairwise dominance elimination over raw ARROW
;trees -- the generator's arrow shapes (ret <- (inner:list? params ...)),
;which sem:convert deliberately leaves raw (something? is the "no
;information" sentinel and stripping the inner:list? shell for the engine
;would route arrow-vs-arrow judgments through Thm 2.17, whose powerset
;implementation is exponential). Dominance is checked PER POSITION with
;plain atom judgments (never arrow-vs-arrow): wide dominates narrow when
;every wide parameter covers the narrow one (set inclusion) and the
;returns are equivalent. Deleting dominated signatures loses no
;information -- same return type, wider or equal domain.
;
;Variadic markers (stage 1.5b): in this DSL, ... and **1 attach to the
;PRECEDING parameter item (syntax-candy.sls private-segment), which is
;both a required position and the element type of the elastic tail, and
;each parameter list allows at most ONE top-level marker. Arrow params
;are compared COVARIANTLY here -- matching the DSL's own arrow comparison
;(type:->? pairs parameter positions covariantly), not classical
;contravariant function subtyping. A trailing marker therefore
;participates in dominance only when: both sides are stripped to equal
;fixed lengths, the marker shapes are compatible (wide must accept every
;arity narrow accepts; the only unsafe pair is narrow=... with
;wide=**1), and positions cover pairwise (the last fixed item doubles
;as the tail element type on both sides). Anything else -- marker on
;one side only, mid-list markers (apply's shape), marker without a
;preceding item -- disqualifies the pair from dominance, which is the
;conservative status quo.
(define (private-arrow-absorb raw-trees)
  (if (> (length raw-trees) 200)
    ;;pathological input: keep status quo rather than quadratic work
    raw-trees
    (let loop ([rest raw-trees] [kept '()])
      (if (null? rest)
        kept
        (let ([current (car rest)])
          (if (and (private-arrow-tree? current)
                (find
                  (lambda (other)
                    (and (not (eq? other current))
                      (private-arrow-dominates? other current)))
                  (append kept (cdr rest))))
            (loop (cdr rest) kept)
            (loop (cdr rest) (cons current kept))))))))

(define (private-arrow-tree? tree)
  (and (list? tree)
    (= (length tree) 3)
    (eq? (cadr tree) '<-)
    (list? (caddr tree))
    (not (null? (caddr tree)))
    (or (eq? (caaddr tree) 'inner:list?)
      (eq? (caaddr tree) 'inner:pair?))))

(define (private-arrow-dominates? wide narrow)
  ;raw candidates may be arbitrary non-convertible trees (e.g. bare
  ;index-nodes); only compare when both sides have the arrow shape
  (and (private-arrow-tree? wide)
    (private-arrow-tree? narrow)
    (private-return-equivalent? (car wide) (car narrow))
    (call-with-values
      (lambda () (private-split-marker (cdaddr wide)))
      (lambda (wfixed wmarker)
        (call-with-values
          (lambda () (private-split-marker (cdaddr narrow)))
          (lambda (nfixed nmarker)
            (and wfixed nfixed
              (= (length wfixed) (length nfixed))
              (private-marker-covers? wmarker nmarker)
              (for-all private-atom-covers? wfixed nfixed))))))))

;strip a single TRAILING variadic marker off a raw arrow's parameter
;items (the inner:list?/inner:pair? head stays a fixed item). Returns
;(values fixed-items marker) with marker in {'..., '**1, #f}; returns
;(values #f #f) when the marker is mid-list, missing its preceding
;item, or duplicated -- the pair must then stay out of dominance.
(define (private-split-marker items)
  (let loop ([rest items] [fixed '()])
    (cond
      [(null? rest) (values (reverse fixed) #f)]
      [(or (eq? (car rest) '...) (eq? (car rest) '**1))
        (if (or (null? fixed) (not (null? (cdr rest))))
          (values #f #f)
          (values (reverse fixed) (car rest)))]
      [else (loop (cdr rest) (cons (car rest) fixed))])))

;soundness table for equal fixed lengths (arity sets: no marker =
;exactly m, ... = >= m-1 unbounded, **1 = >= m unbounded; an elastic
;narrow is never covered by a fixed wide, and **1 does not cover ...).
;everything else is covariantly sound: elastic wide covers fixed
;narrow, and ... covers **1.
(define (private-marker-covers? wmarker nmarker)
  (cond
    [(not nmarker) #t]
    [(not wmarker) #f]
    [else (not (and (eq? nmarker '...) (eq? wmarker '**1)))]))

;;set inclusion wide ⊇ narrow for one parameter position; atom-level
;;sem judgments only (containers convert fine; arrows never reach here).
;;KNOWLEDGE ORDER: a tree containing something? failed sem:convert
;;above, so it falls through to equal? -- it merges only with an
;;identically-unknown tree, never absorbs nor is absorbed by known
;;types (see the header invariant (b)).
(define (private-atom-covers? wide narrow)
  (let ([sw (sem:convert wide)]
      [sn (sem:convert narrow)])
    (cond
      [(and sw sn) (sem:subtype? sn sw)]
      [(identifier-reference? wide)
        (and (identifier-reference? narrow)
          (eq? (identifier-reference-identifier wide)
            (identifier-reference-identifier narrow)))]
      [else (equal? wide narrow)])))

(define (private-return-equivalent? a b)
  ;return positions require KNOWLEDGE equality: a known return and an
  ;unknown (something?) return are never merged -- which one is
  ;"correct" is unknown, and showing both is the informative answer.
  ;sem:equivalent? covers the both-known case; something?-bearing
  ;returns failed sem:convert and fall through to equal?.
  (let ([sa (sem:convert a)]
      [sb (sem:convert b)])
    (cond
      [(and sa sb) (sem:equivalent? sa sb)]
      [(identifier-reference? a)
        (and (identifier-reference? b)
          (eq? (identifier-reference-identifier a)
            (identifier-reference-identifier b)))]
      [else (equal? a b)])))

;stage 1: semantic dedup. Syntax-hash bucketing first, semantic
;equivalence only within buckets (sem:equivalent? is memoized, but
;bucketing keeps the pairwise checks sparse).
(define (private-dedup-classes convertible)
  (let ([buckets (make-hashtable equal-hash equal?)])
    (for-each
      (lambda (entry)
        (hashtable-update! buckets (car entry) (lambda (l) (cons entry l)) '()))
      convertible)
    (call-with-values
      (lambda () (hashtable-entries buckets))
      (lambda (keys _vals)
        (apply append
          (vector->list
            (vector-map
              (lambda (k) (private-merge-bucket (hashtable-ref buckets k '())))
              keys)))))))

;stage 1 for raw candidates: syntactic dedup (semantic equivalence is
;unavailable for non-convertible trees). Two wrinkles: (1) shared
;candidates arrive as the SAME object twice, which the dominance check's
;(not (eq? other current)) self-guard cannot fold; (2) candidates that
;differ only in identifier-reference identity print identically but are
;not equal? (the records embed distinct documents/index-nodes). Both
;are handled by deduping on a canonical key where each reference folds
;to its bare identifier.
(define (private-canonical tree)
  (cond
    [(identifier-reference? tree) (identifier-reference-identifier tree)]
    [(pair? tree)
      (cons (private-canonical (car tree)) (private-canonical (cdr tree)))]
    [else tree]))

(define (private-syntax-dedup trees)
  (let ([seen (make-hashtable equal-hash equal?)])
    (filter
      (lambda (tree)
        (let ([key (private-canonical tree)])
          (if (hashtable-ref seen key #f)
            #f
            (begin (hashtable-set! seen key #t) #t))))
      trees)))

(define (sem:compact-display candidates max-lines)
  (let* ([entries
          (map
            (lambda (tree) (cons tree (sem:convert tree)))
            candidates)]
      [convertible (filter cdr entries)]
      [raw (private-arrow-absorb (private-syntax-dedup (map car (filter (lambda (e) (not (cdr e))) entries))))]
      [classes (private-antichain (private-dedup-classes convertible))]
      [items
        (append
          (map
            (lambda (c) `(rep ,(car c) ,(cddr c)))
            (list-sort private-specificity<? classes))
          (map (lambda (tree) `(raw ,tree)) raw))])
    (if (> (length items) max-lines)
      (values (list-head items max-lines) (length candidates) #t)
      (values items (length candidates) #f))))
)
