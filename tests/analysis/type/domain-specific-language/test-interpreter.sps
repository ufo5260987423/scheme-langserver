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
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis identifier meta))

(test-begin "type:intepret")
    (test-equal 
        (type:interpret (construct-type-expression-with-meta '((number? <- (list? number? number?)) number? number?)))
        (construct-type-expression-with-meta 'number?))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
