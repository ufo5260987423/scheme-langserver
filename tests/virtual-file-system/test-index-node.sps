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
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object))

(test-begin "pick-index-node")
    (let* ([workspace (init-workspace "./protocol")]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node "./protocol/message.sls")]
            [document (file-node-document target-file-node)]
            [text (document-text document)]
            [index-node-list (document-index-node-list document)]
            [index-node (index-node-parent (pick-index-node-from index-node-list (text+position->int text (make-position 155 5))))])
        (test-equal #f (null? (map identifier-reference-identifier (index-node-references-import-in-this-node index-node)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
