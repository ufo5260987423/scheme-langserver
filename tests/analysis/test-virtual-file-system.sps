#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver analyse virtual-file-system))

(test-begin "init-virtual-file-system")
    (test-equal #t (lambda(n) (equal? n "scheme-langserver.sls")) (map node-name (node-children (init-virtual-file-system "." '() folder-or-scheme-file?)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
