#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker))

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

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
