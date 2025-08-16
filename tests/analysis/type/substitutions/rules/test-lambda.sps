#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
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

(test-begin "parameter-index-node type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 49 16))]
            [check-base (construct-type-expression-with-meta 'number?)])
        (construct-substitutions-for target-document)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list target-index-node) 
            check-base)))
(test-end)

; (test-begin "case-lambda procedure type access")
;     (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
;             [target-document (file-node-document target-file-node)]
;             [target-text (document-text target-document)]
;             [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 4 10))]
;             [check-base0 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string? integer? integer?)))]
;             [check-base1 (construct-type-expression-with-meta '(boolean? <- (inner:list? string? string?)))])
;         (construct-substitutions-for target-document)
;         ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
;         (let* ([r0 (type:interpret-result-list target-index-node)]
;                 [r1 (filter type:solved? r0)]
;                 ; [s0 (document-substitution-list target-document)]
;                 ; [s0-1 (map caddr s0)]
;                 ; [s1 (remove-from-substitutions s0 (lambda (i) (equal? variable (car i))))]
;                 ; [s2 (fold-left add-to-substitutions s1 (map (lambda(i) `(,variable = ,i)) r1))]
;                 [r2 (filter (lambda (i) (not (contain? r1 i))) r0)])
;             (pretty-print 'aaa)
;             (test-equal #t (contain? r0 check-base0))
;             ; (pretty-print 'aaa0)
;             ; (pretty-print (map inner:type->string r0))
;             (pretty-print 'aaa1)
;             (pretty-print (map inner:type->string r1))
;             ; (pretty-print 'aaa2)
;             ; (pretty-print (map inner:type->string r2))
;             ; (test-equal #t 
;             ;     (contain? 
;             ;         (apply append 
;             ;             (map
;             ;                 (lambda (i) (type:depature&interpret->result-list i))
;             ;                 r2)) 
;             ;         check-base1))
;                     )
;         ; (test-equal #t
;         ;     (contain? (type:recursive-interpret-result-list variable (make-type:environment (document-substitution-list target-document)))
;         ;         check-base1))
;                 )
; (test-end)

(test-begin "case-lambda:cross clause parameter type access")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 6 14))]
            [check-base (construct-type-expression-with-meta 'string? )])
        (construct-substitutions-for target-document)
        ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document)))
        ; (debug:print-expression target-index-node)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list target-index-node) 
                check-base)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))