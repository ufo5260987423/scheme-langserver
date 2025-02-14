#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis workspace))

(test-begin "find-available-references")
    (let* ([workspace-instance (init-workspace (current-directory))]
            [root-file-node (workspace-file-node workspace-instance)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/io.sls"))]
            [document (file-node-document target-file-node)])
        (let ([index-node (car (document-index-node-list document))])
            (test-equal #f (null? (find-available-references-for document index-node)))))
(test-end)

(test-begin "import test")
    (let* ([workspace-instance (init-workspace (current-directory))]
            [root-file-node (workspace-file-node workspace-instance)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/workspace.sls"))]
            [document (file-node-document target-file-node)])
        (let ([index-node (car (document-index-node-list document))])
            ; (pretty-print (map identifier-reference-identifier (index-node-references-import-in-this-node index-node)))
            (test-equal #f (null? (filter (lambda (reference) (equal? 'get-init-reference-batches (identifier-reference-identifier reference))) 
                (find-available-references-for document index-node))))))
(test-end)

(test-begin "export-import test")
    (let* ([workspace-instance (init-workspace (current-directory))]
            [root-file-node (workspace-file-node workspace-instance)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/path.sls"))]
            [document (file-node-document target-file-node)])
        (let ([index-node (car (document-index-node-list document))])
            (test-equal #f 
                (null? 
                    (filter 
                        (lambda (reference) 
                            (equal? 'string-prefix? (identifier-reference-identifier reference))) 
                        (find-available-references-for document index-node))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
