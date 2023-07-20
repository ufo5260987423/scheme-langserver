#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver protocol alist-access-object)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis dependency rules library-import)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis dependency file-linkage))

(test-begin "library-import-process")
    (let* ([root-file-node (init-virtual-file-system "./util/io.sls" '() akku-acceptable-file?)]
            [root-index-node (car (document-index-node-list (file-node-document root-file-node)))])
        (test-equal '(rnrs) (car (library-import-process root-index-node))))
(test-end)

(test-begin "library-import-process for ss")
    (let* ([root-file-node (init-virtual-file-system "./run.ss" '() akku-acceptable-file?)]
            [root-index-nodes (document-index-node-list (file-node-document root-file-node))])
        (test-equal '((chezscheme) (scheme-langserver)) (car (map library-import-process root-index-nodes))))
(test-end)

(test-begin "test-is-library-identifiers?")
    (let* ( [workspace (init-workspace (string-append (current-directory) "/util"))]
            [root-file-node (workspace-file-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/path.sls"))]
            [document (file-node-document target-file-node)])
        (refresh-workspace-for workspace target-file-node (document-text document) 'previous+single)
        (let* ([root-index-node (car (document-index-node-list document))]

                [position (make-position 10 26)]
                [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) position))]

                [position-a (make-position 11 19)]
                [target-index-node-a (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) position-a))])
            (test-equal #t (is-library-identifiers? document target-index-node))
            (test-equal #t (is-library-identifiers? document target-index-node-a))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
