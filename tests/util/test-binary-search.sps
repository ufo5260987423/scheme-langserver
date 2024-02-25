#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver util binary-search))

(test-begin "simple binary-search ")
    (test-equal 
        (binary-search '#(request server-instance)
            (lambda (reference0 reference1)
                (string<=?
                    (symbol->string reference0)
                    (symbol->string reference1)))
            'server-instance)
        '(server-instance))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
