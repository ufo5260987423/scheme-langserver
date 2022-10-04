#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-define)
    (scheme-langserver analysis package-manager akku)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "library-define-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() akku-acceptable-file?)]
            [target-file-node (walk-file root-file-node "./util/io.sls")]
            [document (file-node-document target-file-node)]
            [index-node (document-index-node document)])
            (test-equal #t
                (not (null? 
                    (find 
                        (lambda (reference) 
                            (equal? 'read-line 
                                (annotation-stripped 
                                    (index-node-datum/annotations 
                                        (identifier-reference-index-node reference)))))
                        (index-node-references-import-in-this-node 
                            (library-define-process root-file-node document index-node)))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
