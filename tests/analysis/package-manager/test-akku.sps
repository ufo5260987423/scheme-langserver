#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis package-manager akku))

(test-begin "path")
    (test-equal #f (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/.akku/lib/scheme-langserver"))
    (test-equal #f (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/.akku/lib/scheme-langserver/"))
    (test-equal #f (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/.akku/lib/scheme-langserver.chezscheme.sls"))
    (test-equal #f (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/.akku/lib/scheme-langserver.sls"))
    (test-equal #f (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/.akku/"))
    (test-equal #t (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/scheme-langserver.sls"))
    (test-equal #t (akku-acceptable-file? "/home/ufo/Documents/workspace/scheme-langserver/util/path.sls"))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
