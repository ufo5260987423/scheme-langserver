#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-2023 WANG Zheng
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

    (scheme-langserver util dedupe)

    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import))

; (test-begin "output-workspace")
;     (let* ([target-path (current-directory)] 
;             [workspace (init-workspace target-path #t #f #t)]
;             [port (open-file-output-port (string-append (current-directory) "/output/workspace"))]
;             [target (workspace-file-node workspace)])
;         (fasl-write target port))
; (test-end)

(test-begin "output-identifier-types")
    (let* ([target-path (current-directory)] 
            [input-port (open-file-input-port (string-append (current-directory) "/output/workspace"))]
            [file-node (fasl-read input-port)]
            [workspace (init-workspace target-path #t #f #t)]
            ; [workspace (load-with (init-workspace target-path #t #f #f) file-node)]
            [root-library-node (workspace-library-node workspace)]
            [target-library-identifier '(scheme-langserver util matrix)]
            ; [target-library-identifier '(scheme-langserver util contain)]
            ; [target-library-identifier '(industria crypto rsa)]
            [identifier-references (import-references root-library-node target-library-identifier)])
        (print-graph #t)
        (pretty-print 'output-identifier-types)
        (map 
            (lambda (identifier-reference)
                (pretty-print (identifier-reference-identifier identifier-reference))
                (cond 
                    [(null? (identifier-reference-index-node identifier-reference)) '()]
                    ;because the identifier-reference-type-expression may be the result of type:interpret-result-list
                    [else
                        (let* ([target-document (identifier-reference-document identifier-reference)]
                            [env (make-type:environment (document-substitution-list target-document))]
                            [result (type:recursive-interpret-result-list (index-node-variable (identifier-reference-index-node identifier-reference)) env)])
                            (identifier-reference-type-expressions-set! identifier-reference result))])
                (pretty-print 
                    (filter 
                        (lambda (i) (not (equal? i "something? ")))
                        (dedupe (apply append (map type:interpret->strings (identifier-reference-type-expressions identifier-reference)))))))
            identifier-references))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
