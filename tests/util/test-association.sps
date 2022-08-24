#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver util association))

(let ([a-list '((b . 2) (a . 1))])
    (test-begin "make-alist")
        (test-equal a-list (make-alist 'b 2 'a 1))
    (test-end)
    (test-begin "assq-ref")
        (test-equal 1 (assq-ref 'a a-list))
    (test-end))


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
