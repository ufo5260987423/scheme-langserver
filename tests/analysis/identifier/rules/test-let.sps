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

    (scheme-langserver util text)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "let-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
            [root-library-node '()]
            [target-file-node (walk-file root-file-node "./util/matrix.sls")]
            [document (file-node-document target-file-node)]
            ;; a let node
            [root-index-node (car (document-index-node-list document))]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) 13 2))])
            (let-process root-file-node root-library-node document target-index-node)
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'rows-count (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
