#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 

    (scheme-langserver util contain)

    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver analysis identifier meta))

(test-begin "literal list")
    (let* ([expression '(1)]
            [document '()]
            [index-node '()]
            [substitutions '()]
            [variable (make-variable)]
            [allow-unquote? #f]
            [unquoted? #t]
            [check-base (construct-type-expression-with-meta '(inner:list? fixnum?))]
            [substitutions (trivial-process document index-node variable expression substitutions allow-unquote? unquoted?)])
        (test-equal #t (contain? (type:interpret-result-list variable (make-type:environment substitutions)) check-base)))
(test-end)

(test-begin "literal vector")
    (let* ([expression '#(1)]
            [document '()]
            [index-node '()]
            [substitutions '()]
            [variable (make-variable)]
            [allow-unquote? #f]
            [unquoted? #t]
            [check-base (construct-type-expression-with-meta '(inner:vector? fixnum?))]
            [substitutions (trivial-process document index-node variable expression substitutions allow-unquote? unquoted?)])
        (test-equal #t (contain? (type:interpret-result-list variable (make-type:environment substitutions)) check-base)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
