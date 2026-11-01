#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system index-node)
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

(test-begin "display compaction: deterministic ordering")
  (let-values
    ([(a _ __) (sem:compact-display
        '((fixnum? <- (number?)) (string? <- (string?)) number?) 10)]
      [(b _2 __2) (sem:compact-display
        '((fixnum? <- (number?)) (string? <- (string?)) number?) 10)])
    (test-equal a b))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
