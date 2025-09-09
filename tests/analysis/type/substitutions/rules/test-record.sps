#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-2023 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver util contain)
    (scheme-langserver util text)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)

    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions generator)

    (scheme-langserver protocol alist-access-object))

(test-begin "type for define-record-type")
    (let* ([workspace (init-workspace (string-append (current-directory) "/protocol/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/protocol/alist-access-object.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 115 21))])
        (construct-substitutions-for target-document)
        (test-equal #t
            (contain? 
                (map inner:type->string (type:interpret-result-list target-index-node))
                "(something? <- (inner:list? [identifier-reference position?] ) ) ")))
(test-end)

(test-begin "type for setters")
    (let* ([workspace (init-workspace (string-append (current-directory) "/virtual-file-system/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/virtual-file-system/document.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 35 0))])
        (construct-substitutions-for target-document)
        (test-equal 
            (inner:type->string (car (identifier-reference-type-expressions (car (find-available-references-for target-document target-index-node 'document-text-set!)))))
            "(void? <- (inner:list? [identifier-reference document?] something? ) ) "))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
