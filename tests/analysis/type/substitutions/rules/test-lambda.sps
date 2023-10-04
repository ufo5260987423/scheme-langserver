#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
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

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions generator)

    (scheme-langserver protocol alist-access-object))

(test-begin "parameter-index-node type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 8 44)))]
            [variable (index-node-variable target-index-node)]
            [check-base (construct-type-expression-with-meta 'string?)])
        (construct-substitution-list-for target-document)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
            check-base)))
(test-end)

; (test-begin "case-lambda procedure type access")
;     (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
;             [target-document (file-node-document target-file-node)]
;             [target-text (document-text target-document)]
;             [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 4 10)))]
;             [variable (index-node-variable target-index-node)]
;             [check-base0 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string? integer? integer?)))]
;             [check-base1 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string?)))])
;         (construct-substitution-list-for target-document)
;         (print-graph #t)
;         (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
;         (pretty-print 'sss)
;         (pretty-print (document-substitution-list target-document))
;         ; (test-equal #t 
;         ;     (contain? 
;         ;         (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
;         ;         check-base0))
;         (test-equal #t 
;             (contain? 
;                 (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
;                 check-base1))
;                 )
; (test-end)

(test-begin "cross clause type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 6 14)))]
            [variable (index-node-variable target-index-node)]
            [check-base (construct-type-expression-with-meta 'string? )])
        (construct-substitution-list-for target-document)
        ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
        ; (debug:print-expression target-index-node)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
                check-base)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))