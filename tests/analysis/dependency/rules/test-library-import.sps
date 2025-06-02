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

    (scheme-langserver analysis dependency rules library-import)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis dependency file-linkage))

(test-begin "library-import-process")
    (let* ([root-file-node (init-virtual-file-system "./util/io.sls" '() (lambda (fuzzy) #t))]
            [root-index-node (car (document-index-node-list (file-node-document root-file-node)))])
        (test-equal '(rnrs) (car (library-import-process root-index-node))))
(test-end)

(test-begin "library-import-process for ss")
    (let* ([root-file-node (init-virtual-file-system "./run.ss" '() (lambda (fuzzy) #t))]
            [root-index-nodes (document-index-node-list (file-node-document root-file-node))])
        (test-equal '((chezscheme) (scheme-langserver)) (car (map library-import-process root-index-nodes))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
