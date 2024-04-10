#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules case-lambda)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))

(test-begin "case-lambda-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
            [target-file-node (walk-file root-file-node "./util/matrix.sls")]
            [document (file-node-document target-file-node)]
            [root-index-node (car (document-index-node-list document))]
            [ready-position (make-position 58 2)]
            [ready-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) ready-position))]
            [target-position (make-position 59 4)]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) target-position))])
            (case-lambda-process root-file-node document ready-index-node)
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'n (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))