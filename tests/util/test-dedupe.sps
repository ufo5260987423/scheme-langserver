#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-Now WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) (scheme-langserver util dedupe))

(test-begin "dedupe")
    (test-equal '(1) (dedupe '(1)))
    (test-equal '(1) (dedupe '(1 1)))
    (test-equal '(1 2) (dedupe '(1 2 1)))
(test-end)

(test-begin "ordered-dedupe")
    (test-equal '(1) (ordered-dedupe '(1)))
    (test-equal '(1) (ordered-dedupe '(1 1)))
    (test-equal '(1 2) (dedupe '(1 1 2)))
(test-end)

(test-begin "dedupe-deduped")
    (test-equal '(1) (dedupe-deduped '(1) '()))
    (test-equal '(2 1) (dedupe-deduped '() '(2 1)))
    (test-equal '(1) (dedupe-deduped '(1) '(1)))
    (test-equal '(2 1) (dedupe-deduped '(2 1) '(1)))
    (test-equal '(2 1) (dedupe-deduped '(1) '(2 1)))
    (test-equal '(2 1 4 3) (dedupe-deduped '(1 3) '(2 1 4)))
    ; (test-equal '(1 2) (dedupe '(1 1 2)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
