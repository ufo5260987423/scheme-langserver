#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 

    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver util contain)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions generator)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver protocol alist-access-object))

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
