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

    (scheme-langserver analysis workspace)

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

(test-begin "type:recursive-intepret-result-list")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f #f)]
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
        (let ([tmp (type:recursive-interpret-result-list variable (make-type:environment (document-substitution-list target-document)))])
            (test-equal #t (contain? tmp check-base0))
            (test-equal #t (contain? tmp check-base1))))
(test-end)

(test-begin "type:intepret")
    (test-equal 
        (type:interpret-result-list (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number?)))
        (list (construct-type-expression-with-meta 'number?)))
    (test-equal #t
        (contain? 
            (let ([v (make-variable)])
                (type:interpret-result-list `((,v <- (inner:list? ,v)) ,(construct-type-expression-with-meta 'number?))))
            (construct-type-expression-with-meta 'number?)))
(test-end)

(test-begin "type:->?/<-?/=? ")
    (test-equal #t (type:->? (construct-type-expression-with-meta 'integer?) (construct-type-expression-with-meta 'number?) (make-type:environment '())))
    (test-equal #t (type:->? (construct-type-expression-with-meta 'integer?) (construct-type-expression-with-meta 'real?) (make-type:environment '())))
    (test-equal #f (type:->? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
    (test-equal #t (type:<-? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
    (test-equal #f (type:=? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
    (test-equal #f (type:->? 
        (construct-type-expression-with-meta '(inner:list? number? number?)) 
        (construct-type-expression-with-meta '(inner:pair? number? (inner:list? number?))) 
        (make-type:environment '())))
(test-end)

; (test-begin "debug")
;     (let* ([workspace (init-workspace (string-append (current-directory) "/.akku/lib/industria/crypto/") '() #f #f #f)]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/industria/crypto/math.sls"))]
;             [target-document (file-node-document target-file-node)]
;             [target-text (document-text target-document)]
;             [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 57 12)))]
;             [variable (index-node-variable target-index-node)]
;             )
;         (construct-substitution-list-for target-document)
;         (let ([tmp (type:recursive-interpret-result-list variable (make-type:environment (document-substitution-list target-document)))])
;             (pretty-print tmp)))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
