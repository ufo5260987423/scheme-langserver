#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis tokenizer))

(test-begin "read ss")
    (test-equal 14 (length (source-file->annotations "./run.ss")))
(test-end)

(test-begin "read sps")
    (test-equal 6 (length (source-file->annotations "./tests/log-debug.sps")))
(test-end)

(test-begin "read scm")
    (test-equal 1 (length (source-file->annotations ".akku/lib/srfi/%3a37/srfi-37-reference.scm")))
(test-end)

(test-begin "tolerant parse")
    (test-equal 8 (length (source-file->annotations "tests/resources/incomplete.ss.test")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
