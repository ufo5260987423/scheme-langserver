#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis identifier meta))

(test-begin "inner:checkers")
    (test-equal #t (inner:list? '(list?)))
    (test-equal #f (inner:trivial? '(list? list?)))
    (test-equal #f (inner:trivial? '(pair? )))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta 'number?)))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta '(pair? number? number?))))
    (test-equal #f (inner:trivial? (construct-type-expression-with-meta '(pair? number?))))
    (test-equal #t (inner:lambda? (construct-type-expression-with-meta '(number? <- (list? number? number?)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
