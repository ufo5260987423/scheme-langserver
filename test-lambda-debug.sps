#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type substitutions generator)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver util test))

(test-begin "lambda-debug")
(let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/matrix.sls"))]
    [target-document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list target-document))]
    [encode-node (find-define-with-params root-index-node 'encode)]
    [name-list-node (cadr (index-node-children encode-node))]
    [target-index-node (caddr (index-node-children name-list-node))]
    [encode-body-node (caddr (index-node-children encode-node))]
    [plus-node (car (index-node-children encode-body-node))]
    [star-node (car (index-node-children (cadr (index-node-children encode-body-node))))]
    [check-base (construct-type-expression-with-meta 'number?)])
  (construct-substitutions-for target-document)
  (let ([sub0 (list-ref (index-node-substitution-list star-node) 0)])
    (display "inner:lambda? sub0: ")
    (pretty-print (inner:lambda? sub0))
    (display "inner:executable? sub0: ")
    (pretty-print (inner:executable? sub0))
    (display "inner:trivial? (cadr sub0): ")
    (pretty-print (inner:trivial? (cadr sub0)))
    (display "inner:list? (caddr sub0): ")
    (pretty-print (inner:list? (caddr sub0)))))

(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
