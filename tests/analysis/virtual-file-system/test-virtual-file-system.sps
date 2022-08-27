#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver analysis virtual-file-system file-node))

(test-begin "init-virtual-file-system")
    (test-equal "scheme-langserver.sls" 
        (find (lambda(n) (equal? n "scheme-langserver.sls")) 
        (map file-node-name (file-node-children (init-virtual-file-system (current-directory) '() folder-or-scheme-file?)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
