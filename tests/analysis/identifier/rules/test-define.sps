#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))

(test-begin "define-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
            [target-file-node (walk-file root-file-node "./util/io.sls")]
            [document (file-node-document target-file-node)]
            [index-node (car (document-index-node-list document))])
            (map (lambda (node) (define-process root-file-node document node)) (index-node-children index-node))
            (test-equal #t
                (not (null? 
                    (find 
                        (lambda (reference) 
                            (equal? 'read-line 
                                (annotation-stripped 
                                    (index-node-datum/annotations 
                                        (identifier-reference-index-node reference)))))
                        (index-node-references-import-in-this-node index-node))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
