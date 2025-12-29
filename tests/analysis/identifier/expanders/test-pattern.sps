#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme) 
  (srfi :64 testing) 
  (scheme-langserver analysis identifier expanders pattern))

(test-begin "context:ellipsed?")
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'body1))
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'clause1))
  (test-equal #f (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'condition))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
