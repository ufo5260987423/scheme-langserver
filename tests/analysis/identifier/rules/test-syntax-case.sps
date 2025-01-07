#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis abstract-interpreter)

    (scheme-langserver util text)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))

(test-begin "syntax-case-process")
    (let* ( [root-file-node (init-virtual-file-system (current-directory) '() (lambda (fuzzy) #t))]
            [root-library-node '()]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/src/ufo-try/ufo-try.sls"))]
            [document (file-node-document target-file-node)]
            [root-index-node (car (document-index-node-list document))]
            ; a syntax-case node
            [ready-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) 103 14))]
            [target-index-node (pick-index-node-from `(,root-index-node) (text+position->int (document-text document) 105 16))])
            (debug:print-expression ready-index-node)
            (debug:print-expression target-index-node)
            (syntax-case-process root-file-node root-library-node document ready-index-node)
            (pretty-print (map identifier-reference-identifier (index-node-references-import-in-this-node target-index-node)))
            (test-equal #f
                (not 
                    (find 
                        (lambda (reference) 
                            (equal? 'tst (identifier-reference-identifier reference)))
                        (index-node-references-import-in-this-node target-index-node)))))
(test-end)

(test-begin "syntax-case-process for quasisyntaxand unsyntax")
    (let* ( [workspace (init-workspace (string-append (current-directory) "/.akku/src/ufo-try/") #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/src/ufo-try/ufo-try.sls"))]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (workspace-file-linkage workspace)]
            [document (file-node-document target-file-node)]
            [loop-index-node (pick-index-node-from (document-index-node-list document) (text+position->int (document-text document) 112 75))])
        (document-ordered-reference-list-set! document (sort-identifier-references (find-meta '(chezscheme))))
        (step root-file-node root-library-node file-linkage document)
        (test-equal '(loop) (map identifier-reference-identifier (find-available-references-for document loop-index-node 'loop))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
