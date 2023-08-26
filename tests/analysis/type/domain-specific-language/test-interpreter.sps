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
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis identifier meta))

(test-begin "type:intepret")
    (test-equal 
        (type:interpret-result-list (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number?)))
        (list (construct-type-expression-with-meta 'number?)))
    (test-equal 
        (type:interpret-result-list 
            (construct-type-expression-with-meta 
                `((something? <-record-ref annotation? annotation-expression) 
                    (inner:record? annotation? ,(make-variable) (inner:pair? annotation-expression symbol?)))))
        (list (construct-type-expression-with-meta 'symbol?)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
