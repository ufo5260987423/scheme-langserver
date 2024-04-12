#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver util contain))

(test-begin "inner:checkers")
    (test-equal #t (inner:list? '(inner:list?)))
    (test-equal #f (inner:trivial? '(inner:list? list?)))
    (test-equal #f (inner:trivial? '(inner:pair? )))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta 'number?)))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta '(inner:pair? number? number?))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(inner:list? number? **1))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(inner:list? number? ...))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(inner:list? number? ... string? ...))))
    (test-equal #t (inner:list? (construct-type-expression-with-meta '(inner:list? number? **1 string? ...))))
    (test-equal #f (inner:trivial? (construct-type-expression-with-meta '(inner:pair? number?))))
    (test-equal #t (inner:lambda? (construct-type-expression-with-meta '(number? <- (inner:list? number? number?)))))
    (test-equal #t (inner:lambda? `(,(make-variable) <- (inner:list? ))))
    (test-equal #t (inner:trivial? (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number?))))
    (test-equal #t (inner:executable? (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number?))))
    (test-equal #t (inner:executable? `((,(make-variable) <- (inner:list? )) something?)))
    (test-equal #t
        (contain?
            (type:interpret-result-list (construct-type-expression-with-meta '((with-type (a b c) (inner:list? a b c)) number? number? number?)))
            (construct-type-expression-with-meta '(inner:list? number? number? number?))))
    ; car list?
    (test-equal #t
        (contain? 
            (type:interpret-result-list 
                (construct-type-expression-with-meta 
                    '((with-type 
                        ((a b c **1)) 
                        (with-equal? inner:list? a b))
                        (inner:list? fixnum? number?))))
            (construct-type-expression-with-meta 'fixnum?)))
    ; cdr list?
    (test-equal #t
        (contain?
            (type:interpret-result-list
                (construct-type-expression-with-meta 
                    '((with-type 
                        ((a b c **1 )) 
                        (with-equal? inner:list? a (with-append (inner:list?) c)))
                        (inner:list? fixnum? number? fixnum?))))
            (construct-type-expression-with-meta '(inner:list? number? fixnum?))))
    (test-equal 
        (inner:?->pair (construct-type-expression-with-meta '(inner:list? number? number?)))
        (construct-type-expression-with-meta '(inner:pair? number? (inner:list? number?))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
