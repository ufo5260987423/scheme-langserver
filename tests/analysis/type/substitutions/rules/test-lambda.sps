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
    (scheme-langserver util dedupe)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
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

(test-begin "case-lambda procedure type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 4 10)))]
            [variable (index-node-variable target-index-node)]
            [check-base0 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string? integer? integer?)))]
            [check-base1 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string?)))])
        (construct-substitution-list-for target-document)
        (print-graph #t)
        ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
                check-base0))
        (let* ([r0 (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document)))]
                [r1 (filter type:solved? r0)]
                [s0 (document-substitution-list target-document)]
                [s0-1 (map caddr s0)]
                [s1 (remove-from-substitutions s0 (lambda (i) (equal? variable (car i))))]
                [s2 (fold-left add-to-substitutions s1 (map (lambda(i) `(,variable = ,i)) r1))]
                [r2 (filter (lambda (i) (not (contain? r1 i))) r0)])
            (pretty-print (contain? (apply append (map
                (lambda (i) (type:interpret-result-list i (make-type:environment s2)))
                r2)) check-base1))
        )
        ; (test-equal #t 
        ;     (contain? 
        ;         (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
        ;         check-base1))
                )
(test-end)

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