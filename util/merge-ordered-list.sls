(library (scheme-langserver util merge-ordered-list)
  (export
    merge-ordered-lists
    dedupe-adjacent)
  (import
    (chezscheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordered-list maintenance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Remove adjacent equal elements from a sorted list, keeping the first of
; each run (ordered-dedupe semantics from util/dedupe.sls, generalized to
; an arbitrary equality procedure).
(define (dedupe-adjacent sorted equal-procedure)
  (cond
    [(null? sorted) sorted]
    [(null? (cdr sorted)) sorted]
    [(equal-procedure (car sorted) (cadr sorted))
      (dedupe-adjacent (cdr sorted) equal-procedure)]
    [else (cons (car sorted) (dedupe-adjacent (cdr sorted) equal-procedure))]))

; Merge two sorted, internally-deduped lists into one sorted, deduped
; list in O(n + m) — a drop-in incremental replacement for
;   (dedupe (sort (append (reverse batch) old)))
; on every attach.
;
; compare? must be a total preorder (a "less-or-equal" predicate); two
; elements belong to the same run when (compare? a b) and (compare? b a)
; both hold.  equal-procedure decides duplication; a preceding element
; stays the anchor for every following element, so the result keeps the
; first of each equal run.  Within a run, batch elements come before old
; elements, replicating a stable sort of (reverse batch) ++ old (note
; Chez's sort is NOT stable, so the old full-sort behaviour had no
; well-defined intra-run order anyway; the batch-first order given here
; is the well-defined replacement).
;
; Both inputs are taken to be sorted by compare? and deduped by
; equal-procedure; prepare a fresh batch with
;   (dedupe-adjacent (sort batch compare?) equal-procedure)
(define (merge-ordered-lists old batch compare? equal-procedure)
  (define (split-compare-run lst)
    (let ([first (car lst)])
      (let loop ([rest (cdr lst)] [rev-run (list first)])
        (if (or (null? rest)
                (not (and (compare? first (car rest))
                       (compare? (car rest) first))))
          (values (reverse rev-run) rest)
          (loop (cdr rest) (cons (car rest) rev-run))))))

  (define (drop-prefix-equal target run)
    (cond
      [(null? run) '()]
      [(or (eq? target (car run)) (equal-procedure target (car run)))
        (drop-prefix-equal target (cdr run))]
      [else run]))

  (define (last-car lst)
    (if (null? (cdr lst))
      (car lst)
      (last-car (cdr lst))))

  (define (merge-same-run b-run o-run)
    (append b-run (drop-prefix-equal (last-car b-run) o-run)))

  (cond
    [(null? old) batch]
    [(null? batch) old]
    [else
      (let ([o0 (car old)]
          [b0 (car batch)])
        (cond
          [(and (compare? b0 o0) (compare? o0 b0))
            (let-values ([(b-run b-rest) (split-compare-run batch)]
                        [(o-run o-rest) (split-compare-run old)])
              (append (merge-same-run b-run o-run)
                (merge-ordered-lists o-rest b-rest compare? equal-procedure)))]
          [(compare? b0 o0)
            (let-values ([(b-run b-rest) (split-compare-run batch)])
              (append b-run
                (merge-ordered-lists old b-rest compare? equal-procedure)))]
          [else
            (let-values ([(o-run o-rest) (split-compare-run old)])
              (append o-run
                (merge-ordered-lists o-rest batch compare? equal-procedure)))]))]))
)
