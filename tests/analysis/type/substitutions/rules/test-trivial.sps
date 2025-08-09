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
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions generator)

    (scheme-langserver protocol alist-access-object))

(test-begin "for '()")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 15 37))]
            [variable (index-node-variable target-index-node)]
            [check-base (construct-type-expression-with-meta '(inner:list?))])
        (construct-substitution-list-for target-document)
        ; (debug:print-expression target-index-node)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
                check-base)))
(test-end)

(test-begin "for fixnum?")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 15 26))]
            [variable (index-node-variable target-index-node)]
            [check-base (construct-type-expression-with-meta 'fixnum?)])
        (construct-substitution-list-for target-document)
        ; (debug:print-expression target-index-node)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))) 
                check-base)))
(test-end)

(test-begin "outer-documents type reference")
    (let* ([workspace (init-workspace (current-directory) '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]

            [target-file-node0 (walk-file root-file-node (string-append (current-directory) "/util/dedupe.sls"))]
            [target-document0 (file-node-document target-file-node0)]

            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/util.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 37 25))]
            [variable (index-node-variable target-index-node)]
            [check-base 
                (construct-type-expression-with-meta 
                    '((inner:list?)<-(inner:list? something? something?)))])
        (construct-substitution-list-for target-document0)
        (construct-substitution-list-for target-document)
        ; (debug:print-expression target-index-node)
        ; (debug:recursive-print-expression&variable (car (document-index-node-list target-document0)))
        ; (pretty-print (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document))))
        (test-equal #t 
            (not (null?
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document)))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
