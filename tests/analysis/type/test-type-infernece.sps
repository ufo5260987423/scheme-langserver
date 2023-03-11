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
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis type type-inferencer))

(test-begin "test construct-substitution-list-for")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)])
        (construct-substitution-list-for target-document)
        (test-equal #f (null? (document-substitution-list target-document))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
