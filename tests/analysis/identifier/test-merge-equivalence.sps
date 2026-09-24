#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Equivalence fuzz for the incremental merge behind
;; append-references-into-ordered-references-for (util/merge-ordered-list.sls):
;; over random attach histories (including eq?-duplicate re-attaches), the
;; new merge-based transition must agree with the old
;;   (ordered-dedupe (sort ((reverse batch) ++ old)))
;; transition on the name sequence (total order of sort keys) and on the
;; set of references.  Intra-run order is intentionally not compared:
;; Chez's sort is not stable, so the old full-sort behaviour had no
;; well-defined intra-run order in the first place.
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util dedupe)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference))

; --- old transition, verbatim logic from the pre-merge implementation ---
(define (old-step old batch)
  (ordered-dedupe
    (sort-identifier-references
      (fold-left (lambda (acc x) (cons x acc)) old batch))))

; --- fuzz plumbing ---
(define symbol-pool
  '(alpha beta gamma delta epsilon zeta eta theta iota kappa
    lambda mu nu xi omicron pi rho sigma tau upsilon))
(define pool-size (length symbol-pool))

(define (random-symbol)
  (list-ref symbol-pool (random pool-size)))

(define (make-random-reference)
  (make-identifier-reference
    (random-symbol) '() '() '() '() '() '() '()))

(define (random-batch)
  (let loop ([n (random 8)] [acc '()])
    (if (zero? n)
      acc
      (loop (- n 1) (cons (make-random-reference) acc)))))

(define (permutation-of? xs ys)
  ; both lists contain no eq?-duplicates, so equal length + membership is
  ; enough to establish set equality; small lists, O(n^2) is fine
  (and (= (length xs) (length ys))
    (andmap
      (lambda (x) (find (lambda (y) (eq? x y)) ys))
      xs)))

(test-begin "merge-ordered-lists equivalence fuzz")

(define doc (make-document "file:///fuzz" "" '()))
(random-seed 20260924)

(define failures 0)
(define steps 3000)

(let loop ([i steps])
  (unless (zero? i)
    (let* ([batch (random-batch)]
        [old (document-ordered-reference-list doc)]
        [expected (old-step old batch)])
      ; occasionally attach a duplicate of an existing element (eq? path)
      (if (and (not (null? old)) (zero? (random 3)))
        (let ([dup (list-ref old (random (length old)))])
          (append-references-into-ordered-references-for doc '() (list dup))
          (set! expected (old-step (old-step old (list dup)) batch))))
      (append-references-into-ordered-references-for doc '() batch)
      (let ([actual (document-ordered-reference-list doc)])
        (unless
          (and
            ; sort keys (names) must match exactly, in order
            (equal? (map identifier-reference-identifier expected)
              (map identifier-reference-identifier actual))
            ; same set of references (both sides are deduped)
            (permutation-of? expected actual))
          (set! failures (+ failures 1))))
      (loop (- i 1)))))

(test-equal "3000 random attach histories equivalent to old sort+dedupe"
  0 failures)

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
