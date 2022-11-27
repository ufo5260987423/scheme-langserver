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

(test-begin "read-ss-test")
    (test-equal 2 (length (source-file->annotations "./run.ss")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
