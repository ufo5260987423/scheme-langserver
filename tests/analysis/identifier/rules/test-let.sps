#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "let-process")
    (let* ( [workspace (init-workspace "./util" '() #f #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [target-file-node (walk-file root-file-node "./util/natural-order-compare.sls")]
            [document (file-node-document target-file-node)]
            ;; a let node
            [position (make-position 8 12)]
            [root-index-node (car (document-index-node-list document))]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) position))])
            (import-process root-file-node (workspace-library-node workspace) document root-index-node)
            (let-process root-file-node document target-index-node)
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'length-a (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
