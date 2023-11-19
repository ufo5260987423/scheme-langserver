#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "syntax-process")
    (let* ( [workspace (init-workspace "./util" '() #f #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [target-file-node (walk-file root-file-node "./util/try.sls")]
            [document (file-node-document target-file-node)]
            [root-index-node (car (document-index-node-list document))]
            ;; a syntax-case node
            [ready-position (make-position 106 15)]
            [ready-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) ready-position))]
            [target-position (make-position 108 17)]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) target-position))])
            (import-process root-file-node (workspace-library-node workspace) document root-index-node)
            (syntax-process root-file-node document ready-index-node)
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'tst (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
