#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "include-resolve-process")
    (let* ( [workspace (init-workspace (string-append (current-directory) "/.akku/lib/srfi/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/srfi/:13/strings.chezscheme.sls"))]
            [document (file-node-document target-file-node)]
            [root-index-node (car (document-index-node-list document))])
        (test-equal #f
            (not 
                (find 
                    (lambda (reference) 
                        (equal? 'string-suffix? (identifier-reference-identifier reference)))
                    (index-node-references-import-in-this-node root-index-node)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
