#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis abstract-interpreter)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier rules library-import))

(test-begin "library-import-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
            [target-file-node (walk-file root-file-node "./util/io.sls")]
            [root-library-node (init-library-node root-file-node)]
            ; [target-file-node (walk-file root-file-node (string-append (current-directory) "/run.ss"))]
            [document (file-node-document target-file-node)])
        (document-reference-list-set! document (sort-identifier-references (find-meta '(chezscheme))))
        (step root-file-node root-library-node document)
        (test-equal 
            'write-lines
            (find 
                (lambda (identifier) (equal? identifier 'write-lines))
                (map identifier-reference-identifier 
                    (index-node-references-import-in-this-node (car (document-index-node-list document)))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
