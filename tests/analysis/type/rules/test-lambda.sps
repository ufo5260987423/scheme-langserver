#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver util contain)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type type-inferencer)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)
    (scheme-langserver analysis type util)

    (scheme-langserver protocol alist-access-object))

(test-begin "parameter-index-node type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 8 44)))]
            [check-base (construct-type-expression-with-meta 'string?)])
        (construct-substitution-list-for target-document)
        (test-equal #t (contain? (type-inference-for target-index-node target-document) check-base)))
(test-end)

(test-begin "procedure type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 4 10)))]
            [check-base0 (construct-type-expression-with-meta '(boolean? (string? string? integer? integer?)))]
            [check-base1 (construct-type-expression-with-meta '(boolean? (string? string?)))])

        (test-equal #t (contain? (type-inference-for target-index-node target-document) check-base0))
        ; (test-equal #t (contain? (type-inference-for target-index-node target-document) check-base1))
        )
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))