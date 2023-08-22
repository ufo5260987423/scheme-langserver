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
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(list? number? **1))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(list? number? ...))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(list? number? ... string? ...))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(list? number? **1 string? ...))))
    (test-equal #f (inner:trivial? (construct-type-expression-with-meta '(pair? number?))))
    (test-equal #t (inner:lambda? (construct-type-expression-with-meta '(number? <- (list? number? number?)))))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta '((number? <- (list? number? number?)) number? number?))))
    (test-equal #t (inner:executable? (construct-type-expression-with-meta '((number? <- (list? number? number?)) number? number?))))
    (test-equal 
        (construct-type-expression-with-meta '(list? number? number? number?)) 
        (inner:with-macro (construct-type-expression-with-meta '((with (a b c) (list? a b c)) number? number? number?))))
    ;car list?
    (test-equal 
        (construct-type-expression-with-meta 'fixnum?) 
        (inner:with-macro (construct-type-expression-with-meta '((with ((a b c **1)) b) (list? fixnum? number?)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
