#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver analysis type util))

(test-begin "test-construct-lambda")
    (test-equal #t
        ((construct-lambda '(number? x)) 1))
(test-end)

(test-begin "test-type-equal?")
    (test-equal #t
        (type-equal? '(integer? x) '(or (flonum? x) (integer? x)) equal?))
(test-end)

(test-begin "test-type-satisfy>=")
    (test-equal #t
        (type-satisfy? '(integer? x) '(integer? x)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
