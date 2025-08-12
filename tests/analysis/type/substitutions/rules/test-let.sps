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

(test-begin "variable declaration")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 40 17))]
            [check-base (construct-type-expression-with-meta 'fixnum?)])
        (construct-substitutions-for target-document)
        ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
        (test-equal #t 
            (contain? 
                (type:interpret-result-list target-index-node) 
                check-base)))
(test-end)

(test-begin "variable access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 41 14))]
            [check-base (construct-type-expression-with-meta 'fixnum?)])
        (construct-substitutions-for target-document)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list target-index-node) 
                check-base)))
    (let* ([workspace (init-workspace (string-append (current-directory) "/virtual-file-system/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/virtual-file-system/index-node.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 166 9))]
            [check-base (construct-type-expression-with-meta 'boolean?)])
        (construct-substitutions-for target-document)
        ; (debug:print-expression&uuid target-index-node)
        ; (pretty-print (map inner:type->string (type:interpret-result-list target-index-node)))
        (test-equal #t 
            (contain? 
                (map car (filter list? (type:interpret-result-list target-index-node)))
                check-base)))
(test-end)

; (test-begin "loop declaration")
;     (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
;             [target-document (file-node-document target-file-node)]
;             [target-text (document-text target-document)]
;             [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 15 10))]
;             [check-base (construct-type-expression-with-meta '((inner:list? something? ...) <- (inner:list? number? (inner:list? something? ...))))])
;         (construct-substitutions-for target-document)
;         (pretty-print (map  inner:type->string (type:recursive-interpret-result-list target-index-node)))
;         (test-equal #t 
;             (contain? 
;                 (type:recursive-interpret-result-list target-index-node)
;                 check-base)))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
