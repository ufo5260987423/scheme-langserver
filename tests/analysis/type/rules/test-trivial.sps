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

    (scheme-langserver analysis type rules trivial)
    (scheme-langserver analysis type walk-engine)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type util)

    (scheme-langserver analysis identifier meta))

(test-begin "literal list")
    (let* ([expression '(1)]
            [document '()]
            [index-node '()]
            [substitutions '()]
            [variable (make-variable)]
            [allow-unquote? #f]
            [unquoted? #t]
            [check-base (construct-type-expression-with-meta '(fixnum?))]
            [substitutions (trivial-process document index-node variable expression substitutions allow-unquote? unquoted?)])
        (test-equal #t (contain? (reify substitutions variable) check-base)))
(test-end)

(test-begin "literal vector")
    (let* ([expression '#(1)]
            [document '()]
            [index-node '()]
            [substitutions '()]
            [variable (make-variable)]
            [allow-unquote? #f]
            [unquoted? #t]
            [check-base (construct-type-expression-with-meta '#(fixnum?))]
            [substitutions (trivial-process document index-node variable expression substitutions allow-unquote? unquoted?)])
        (test-equal #t (contain? (reify substitutions variable) check-base)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
