#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (scheme-langserver analysis type domain-specific-language syntax-candy))

;; ---------------------------------------------------------------------------
;; P0 — Core expander
;; ---------------------------------------------------------------------------

(test-begin "P0 A1 flat denotions basic expansion")
  (test-equal
    '(list something? something? something?)
    (execute-macro '((with (a b c) (list a b c)) something? something? something?)))
(test-end)

(test-begin "P0 A2 wrapped denotions car scenario")
  (test-equal
    'something?
    (execute-macro '((with ((a b c **1)) (with-equal? inner:list? a b)) (inner:list? something? something?))))
(test-end)

(test-begin "P0 A3 with-equal? guard passes")
  (test-equal
    'something?
    (execute-macro '((with (a b) (with-equal? something? a b)) something? something?)))
(test-end)

(test-begin "P0 A3 with-equal? guard fails")
  (test-equal
    '(with-equal? something? inner:void? something?)
    (execute-macro '((with (a b) (with-equal? something? a b)) inner:void? something?)))
(test-end)

(test-begin "P0 A4 with-append")
  (test-equal
    '(inner:list? number? string?)
    (execute-macro '(with-append (inner:list?) (number? string?))))
(test-end)

(test-begin "P0 A5 nested with")
  (test-equal
    '(list something? something?)
    (execute-macro '((with (x) ((with (a b) (list a b)) x something?)) something?)))
(test-end)

;; ---------------------------------------------------------------------------
;; P1 — Failure paths
;; ---------------------------------------------------------------------------

(test-begin "P1 B1 bare symbol fails inner:trivial? guard")
  (let ([expr '((with (a) a) number?)])
    (test-equal
      expr
      (execute-macro expr)))
(test-end)

(test-begin "P1 B2 wrapping-level mismatch")
  (let ([expr '((with (a b) (list a b)) (something? something?))])
    (test-equal
      expr
      (execute-macro expr)))
(test-end)

(test-begin "P1 B3 template length insufficient")
  (test-equal
    'macro-not-match:private-with-list?
    (guard (e [else e])
      (execute-macro '((with ((a b c **1)) (with-equal? inner:list? a b)) (inner:list? something?)))))
(test-end)

;; ---------------------------------------------------------------------------
;; P2 — private-with edge behaviour
;; ---------------------------------------------------------------------------

(test-begin "P2 C1 identifier-reference as wildcard")
  (let ([ref (construct-type-expression-with-meta 'number?)])
    (test-equal #t (identifier-reference? ref))
    (test-equal
      '(list a b)
      (private-with '(list a b) `((,ref . something?)))))
(test-end)

(test-begin "P2 C2 candy:match-right repeated segments")
  ;; candy:match-right expands a repeated segment into multiple flat pairs.
  ;; private-with's fold-left + private-substitute only catches the FIRST
  ;; replacement because the symbol is gone after the first step.
  (let ([mr-pairs (candy:match-right '(a b **1) '(x y z))]
        [ml-pairs (candy:match-left '(a b **1) '(x y z))])
    (test-equal '((a . x) (b . y) (b . z)) mr-pairs)
    (test-equal '((a . x) (b y z)) ml-pairs)
    ;; match-right path loses z (KNOWN LIMITATION)
    (test-equal '(list x y)
      (private-with '(list a b) mr-pairs))
    ;; match-left path correctly preserves the list
    (test-equal '(list x (y z))
      (private-with '(list a b) ml-pairs)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
