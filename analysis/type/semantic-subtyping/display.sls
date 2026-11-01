(library (scheme-langserver analysis type semantic-subtyping display)
  (export sem:compact-display)

  (import
    (chezscheme)

    (scheme-langserver analysis type semantic-subtyping semtype))

;display compaction for hover (phase 1 of the display-quality roadmap,
;see the improvement survey): candidate type trees are merged into a
;compact, deterministic view. The display guarantee is
;
;    display(C)  >=  union(C)        (allowed to be wider, never narrower)
;
;so the "definitional union" product contract is preserved: anti-chain
;absorption is a semantic equality (the absorbed candidate's values are a
;subset of the covering candidate's), dedup is equality, and truncation
;only hides representatives behind a total count, never deletes values.
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

(define (sem:compact-display candidates max-lines)
  (let* ([entries
          (map
            (lambda (tree) (cons tree (sem:convert tree)))
            candidates)]
      [convertible (filter cdr entries)]
      [raw (map car (filter (lambda (e) (not (cdr e))) entries))]
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
