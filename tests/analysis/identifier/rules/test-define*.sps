#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier rules s7 define*)
    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "define*-process")
    (let* ( [root-file-node (init-virtual-file-system "./tests/resources/r7rs" '() (lambda (fuzzy) #t) 's7)]
            [root-library-node '()]
            [target-file-node (walk-file root-file-node "./tests/resources/r7rs/scheme/base.scm.txt")]
            [document (file-node-document target-file-node)]
            [index-node (car (document-index-node-list document))]
            [begin-index-node (car (last-pair (index-node-children index-node)))])

            (map (lambda (node) (define*-process root-file-node root-library-node document node)) (index-node-children (car (last-pair (index-node-children index-node)))))
            
            (test-equal #f
                (not (find 
                        (lambda (reference) 
                            (equal? 'bytevector-advance-u8 
                                (annotation-stripped 
                                    (index-node-datum/annotations 
                                        (identifier-reference-index-node reference)))))
                        (index-node-references-import-in-this-node begin-index-node)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
