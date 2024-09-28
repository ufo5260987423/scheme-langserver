#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier macro-expander)
    (scheme-langserver analysis workspace))

(test-begin "expand:step-by-step & generate-pair:template+callee")
    (let* ([workspace-instance (init-workspace (current-directory))]
            [root-file-node (workspace-file-node workspace-instance)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/dependency/rules/library-import.sls"))]
            [document (file-node-document target-file-node)]
            [target-index-node (pick-index-node-from (document-index-node-list document) (text+position->int (document-text document) (make-position 37 6)))])
        (test-equal 
            '((let ([v expression])
                (match-next v (expression (set! expression))
                    (('only (identifier **1) _ ...) identifier)
                    (('except (identifier **1) _ ...) identifier)
                    (('prefix (identifier **1) _ ...) identifier)
                    (('rename (identifier **1) _ ...) identifier)
                    (('for (identifier **1) 'run ...) identifier)
                    (('for (identifier **1) '(meta 0) ...) identifier)
                    ((identifier **1) identifier) (else '()))))
            (expand:step-by-step (car (find-available-references-for document target-index-node 'match)) target-index-node document))
        (test-equal
            '((match . match)
                (atom . expression)
                (pat ('only (identifier **1) _ ...)
                    ('except (identifier **1) _ ...)
                    ('prefix (identifier **1) _ ...)
                    ('rename (identifier **1) _ ...)
                    ('for (identifier **1) 'run ...)
                    ('for (identifier **1) '(meta 0) ...) (identifier **1) else)
                (body identifier identifier identifier identifier identifier identifier identifier '()))
            (map 
                (lambda (p)
                    `(,(car p) .
                    ,(if (index-node? (cdr p))
                        (annotation-stripped (index-node-datum/annotations (cdr p)))
                        (map (lambda (px) (annotation-stripped (index-node-datum/annotations px))) (cdr p)))))
                (generate-pair:template+callee (car (find-available-references-for document target-index-node 'match)) target-index-node document))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))