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
    (scheme-langserver util text)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions generator)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver protocol alist-access-object))

(test-begin "construct-substitution-list-for")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)])
        (construct-substitution-list-for target-document)
        (test-equal #f (null? (document-substitution-list target-document))))
(test-end)

(test-begin "type-inference-for fixnum literal")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 15 26))]
            [variable (index-node-variable target-index-node)])
        (construct-substitution-list-for target-document)
        (test-equal 
            #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document)))
                (construct-type-expression-with-meta 'fixnum?))))
(test-end)

(test-begin "walk for symbol")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 12 9))]
            [variable (index-node-variable target-index-node)])
        (construct-substitution-list-for target-document)
        (test-equal (car (car (substitution:walk (document-substitution-list target-document) variable))) variable))
(test-end)

(test-begin "type-inference-for symbol")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 16 11))]
            [variable (index-node-variable target-index-node)]
            [check-base (construct-type-expression-with-meta '(boolean? <- (inner:list? real? real? **1)))])
        (construct-substitution-list-for target-document)
        (test-equal #t 
            (contain? 
                (type:interpret-result-list variable (make-type:environment (document-substitution-list target-document)))
                check-base)))
(test-end)

; (test-begin "debug:tmp")
;     (let* ([workspace (init-workspace (current-directory) '() #f #f)]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/arew/json/body.scm"))]
;             [target-document (file-node-document target-file-node)])
;         (pretty-print 'aa0)
;         (construct-substitution-list-for target-document)
;         (pretty-print 'aa1)
;         )
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))