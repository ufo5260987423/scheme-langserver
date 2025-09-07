#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025 WANG Zheng, HUANG zengqian
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver util text)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))

(test-begin "typed-lambda-process")
    (let* ( [root-file-node (init-virtual-file-system "./tests/resources/r7rs" '() (lambda (fuzzy) #t) 's7)]
            [root-library-node '()]
            [target-file-node (walk-file root-file-node "./tests/resources/r7rs/liii/base64.scm.txt")]
            [document (file-node-document target-file-node)]
            [root-index-node (car (document-index-node-list document))]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) 122 6))])
            
            (typed-lambda-process root-file-node root-library-node document target-index-node)
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'str (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))

