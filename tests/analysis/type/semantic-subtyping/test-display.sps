#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis type semantic-subtyping semtype)
  (scheme-langserver analysis type semantic-subtyping display))

(test-begin "display compaction: semantic dedup")
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '(fixnum? fixnum? fixnum? string?)
        10)])
    (test-equal 4 total)
    (test-assert (not truncated?))
    ;three fixnums merged into one representative; deterministic order
    (test-equal 2 (length items))
    (test-equal '(rep string? 1) (car items))
    (test-equal '(rep fixnum? 3) (cadr items)))
(test-end)

(test-begin "display compaction: anti-chain absorption")
  (let-values
    ([(items total truncated?)
      ;fixnum <: bignum's sibling; both <: integer <: rational (the
      ;compound): the whole numeric chain absorbs into rational
      (sem:compact-display
        '(fixnum? bignum? integer? rational? flonum? string?)
        10)])
    (test-equal 6 total)
    (test-equal 3 (length items))
    (test-equal
      '((rep string? 1) (rep flonum? 1) (rep rational? 4))
      items))
(test-end)

(test-begin "display compaction: lambdas merge by equivalence")
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((number? <- (number?))
          (number? <- (number?))
          (fixnum? <- (number?)))
        10)])
    (test-equal 3 total)
    ;two identical arrows dedup; (fixnum <- ...) is NOT a subtype of
    ;(number <- ...) (domains equal, cod fixnum <: number means the
    ;arrow IS <: ... check: arrow sub: dom contra (number<:number ok),
    ;cod co (fixnum<:number ok) => it IS a subtype and gets absorbed!
    (test-equal 1 (length items))
    (test-equal 3 (caddr (car items))))
(test-end)

(test-begin "display compaction: truncation is declared, values kept")
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '(null? boolean? char? string? symbol? fixnum? bignum? ratnum?)
        3)])
    (test-equal 8 total)
    (test-assert truncated?)
    (test-equal 3 (length items)))
(test-end)

(test-begin "display compaction: non-convertible candidates pass through raw")
  (let* ([node (make-index-node #f '() '() '() #f '() '() '() '())]
      [candidates (list 'number? node 'string?)])
    (let-values
      ([(items total truncated?)
        (sem:compact-display candidates 10)])
      (test-equal 3 total)
      (test-equal 3 (length items))
      ;raw entry preserves the original index-node object
      (test-assert
        (find
          (lambda (item)
            (and (eq? 'raw (car item)) (eq? node (cadr item))))
          items))))
(test-end)

(test-begin "display compaction: raw arrows with mixed non-arrow candidates")
  ;regression (2026-11-02): private-arrow-absorb crashed with car of an
  ;index-node when a raw arrow candidate coexisted with non-arrow raw
  ;candidates (dominance find ran over every other candidate unchecked)
  (let* ([node (make-index-node #f '() '() '() #f '() '() '() '())]
      [candidates
        (list
          '(something? <- (inner:list? number?))
          '(something? <- (inner:list? fixnum?))
          node)])
    (let-values
      ([(items total truncated?) (sem:compact-display candidates 10)])
      (test-equal 3 total)
      ;the fixnum-parameter arrow is dominated by the number-parameter
      ;one (same return, narrower domain) and folds away; the bare
      ;index-node survives as raw
      (test-equal 2 (length items))
      (test-assert
        (find
          (lambda (item)
            (and (eq? 'raw (car item)) (eq? node (cadr item))))
          items))
      (test-assert
        (find
          (lambda (item)
            (and (eq? 'raw (car item))
              (equal? '(something? <- (inner:list? number?)) (cadr item))))
          items))))
(test-end)

(test-begin "display compaction: variadic arrow dominance")
  ;.../**1 attach to the preceding item (required position AND tail
  ;element type). Soundness for equal fixed lengths, covariant per
  ;position: an elastic wide covers a fixed narrow (... and **1 both
  ;do), ... covers **1, but a fixed wide never covers an elastic
  ;narrow, and **1 does not cover ... . The survivor of every merge
  ;below is the wider arrow, in either input order.
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number? ...))
          (something? <- (inner:list? fixnum? ...)))
        10)])
    (test-equal 1 (length items))
    (test-equal
      '(raw (something? <- (inner:list? number? ...)))
      (car items)))
  ;... covers **1 (the only dominance in this pair, both orders)
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number? **1))
          (something? <- (inner:list? number? ...)))
        10)])
    (test-equal 1 (length items))
    (test-equal
      '(raw (something? <- (inner:list? number? ...)))
      (car items)))
  ;elastic wide covers fixed narrow, either order, wide survives
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number?))
          (something? <- (inner:list? number? ...)))
        10)])
    (test-equal 1 (length items))
    (test-equal
      '(raw (something? <- (inner:list? number? ...)))
      (car items)))
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number? ...))
          (something? <- (inner:list? number?)))
        10)])
    (test-equal 1 (length items))
    (test-equal
      '(raw (something? <- (inner:list? number? ...)))
      (car items)))
  ;different fixed lengths under markers: no dominance
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number? string? ...))
          (something? <- (inner:list? number? ...)))
        10)])
    (test-equal 2 (length items)))
  ;mid-list markers (apply's shape) disqualify the pair
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number? ... string?))
          (something? <- (inner:list? fixnum? ... string?)))
        10)])
    (test-equal 2 (length items)))
(test-end)

(test-begin "display compaction: knowledge order of the something? sentinel")
  ;something? is "no information", not honest top: it must never
  ;absorb -- nor be absorbed by -- a known type, or hover would
  ;collapse every signature into "unknown". (Value semantics alone
  ;would allow it: something? IS the top of type:->?.)
  ;poisoned param vs known param, both orders
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((boolean? <- (inner:list? something?))
          (boolean? <- (inner:list? number?)))
        10)])
    (test-equal 2 (length items)))
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((boolean? <- (inner:list? number?))
          (boolean? <- (inner:list? something?)))
        10)])
    (test-equal 2 (length items)))
  ;poison inside a nested container param still blocks absorption
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((boolean? <- (inner:list? (inner:list? something?)))
          (boolean? <- (inner:list? (inner:list? number?))))
        10)])
    (test-equal 2 (length items)))
  ;return position: known and unknown never merge
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? number?))
          (boolean? <- (inner:list? number?)))
        10)])
    (test-equal 2 (length items)))
  ;unknown return with coverable known params DOES merge (union of
  ;knowledge; the wider known position survives)
  (let-values
    ([(items total truncated?)
      (sem:compact-display
        '((something? <- (inner:list? something? number?))
          (something? <- (inner:list? something? fixnum?)))
        10)])
    (test-equal 1 (length items))
    (test-equal
      '(raw (something? <- (inner:list? something? number?)))
      (car items)))
(test-end)

(test-begin "display compaction: identical raw arrows dedup")
  ;shared candidates arrive as the SAME object twice; the dominance
  ;self-guard (not (eq? other current)) cannot fold those, so syntactic
  ;dedup must run before absorb
  (let* ([arrow '(something? <- (inner:list? number?))]
      [candidates (list arrow arrow)])
    (let-values
      ([(items total truncated?) (sem:compact-display candidates 10)])
      (test-equal 2 total)
      (test-equal 1 (length items))
      (test-equal '(raw (something? <- (inner:list? number?))) (car items)))
    ;same story when the duplicates differ only in identifier-reference
    ;identity (distinct records, same printed rendering)
    (let* ([ref-a (make-identifier-reference 'number? '() '() '() '() '() '() '())]
        [ref-b (make-identifier-reference 'number? '() '() '() '() '() '() '())]
        [candidates
          (list
            `(something? <- (inner:list? ,ref-a))
            `(something? <- (inner:list? ,ref-b)))])
      (let-values
        ([(items total truncated?) (sem:compact-display candidates 10)])
        (test-equal 2 total)
        (test-equal 1 (length items)))))
(test-end)

(test-begin "display compaction: deterministic ordering")
  (let-values
    ([(a _ __) (sem:compact-display
        '((fixnum? <- (number?)) (string? <- (string?)) number?) 10)]
      [(b _2 __2) (sem:compact-display
        '((fixnum? <- (number?)) (string? <- (string?)) number?) 10)])
    (test-equal a b))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
