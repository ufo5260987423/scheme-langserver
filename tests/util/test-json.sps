#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver util json))

(let ([a-list '((b . 2) (a . 1))])
    (test-begin "read-json")
        (test-equal a-list (read-json "{\"a\":1,\"b\":2}"))
    (test-end)
    (test-begin "generate-json")
        (test-equal "{\"b\":2,\"a\":1}" (generate-json a-list))
    (test-end))


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
