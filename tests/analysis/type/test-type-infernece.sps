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
    (scheme-langserver analysis type type-inferencer)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver protocol alist-access-object))

(test-begin "construct-substitution-list-for")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)])
        (construct-substitution-list-for target-document)
        (test-equal #f (null? (document-substitution-list target-document))))
(test-end)

(test-begin "type-inference-for fixnum literal")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 6 70)))])
        (construct-substitution-list-for target-document)
        (test-equal #t (contain? (type-inference-for target-index-node target-document) (construct-type-expression-with-meta 'fixnum?))))
(test-end)

(test-begin "walk for symbol")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 10 25)))])
        (construct-substitution-list-for target-document)
        (test-equal (car (car (walk (document-substitution-list target-document) (index-node-variable target-index-node)))) (index-node-variable target-index-node)))
(test-end)

(test-begin "type-inference-for symbol")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 10 25)))]
            [check-base (construct-type-expression-with-meta '(boolean? <- (real? real? **1)))])
        (construct-substitution-list-for target-document)
        (test-equal #t (contain? (type-inference-for target-index-node target-document) check-base)))
(test-end)

(test-begin "type-inference-for debug: index-a")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)]
            [target-text (document-text target-document)]
            [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 16 75)))]
            [check-base (construct-type-expression-with-meta 'integer?)])
        (construct-substitution-list-for target-document)
        (test-equal #t (contain? (type-inference-for target-index-node target-document) check-base)))
(test-end)

(test-begin "sorted substitution")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
            [target-document (file-node-document target-file-node)])
        (construct-substitution-list-for target-document)
        (test-equal #t (debug:substitution-sorted? (document-substitution-list target-document))))
(test-end)

; (test-begin "debug:tmp")
;     (let* ([workspace (init-workspace (current-directory))]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/industria/crypto/math.sls"))]
;             [target-document (file-node-document target-file-node)])
;         (construct-substitution-list-for target-document)
;         (test-equal #t (debug:substitution-sorted? (document-substitution-list target-document))))
; (test-end)

; (test-begin "find-type-conflicts")
;     (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
;             [target-document (file-node-document target-file-node)]
;             [target-text (document-text target-document)]
;             [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text (make-position 10 25)))])
;         (construct-substitution-list-for target-document)
;         (test-equal '() (find-type-conflicts target-index-node target-document)))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
