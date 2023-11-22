#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs
;;to read log and reproduce similar action for debug
(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import))

(test-begin "output-identifier-types")
    (let* ([target-path (current-directory)] 
            [workspace (init-workspace target-path #t #f #t)]  
            [root-library-node (workspace-library-node workspace)]
            [target-library-identifier '(scheme-langserver virtual-file-system index-node)]
            [identifier-references (import-references root-library-node target-library-identifier)])
        (map 
            (lambda (identifier-reference)
                (cond 
                    [(null? (identifier-reference-index-node identifier-reference)) '()]
                    [(null? (identifier-reference-type-expressions identifier-reference))
                        (let* ([target-document (identifier-reference-document identifier-reference)]
                            [env (make-type:environment (document-substitution-list target-document))])
                            (identifier-reference-type-expressions-set! 
                                identifier-reference
                                (type:recursive-interpret-result-list (index-node-variable (identifier-reference-index-node identifier-reference)) env)))]
                    [else '()])
                (print-graph #t)
                (pretty-print (identifier-reference-identifier identifier-reference))
                (pretty-print (map inner:type->string (identifier-reference-type-expressions identifier-reference))))
            identifier-references))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
