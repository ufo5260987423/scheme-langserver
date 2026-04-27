#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver util contain)
  (scheme-langserver util test)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis type substitutions generator)
  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (scheme-langserver analysis type substitutions util))

(test-begin "case-lambda substitution generation")
  (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
      [target-document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list target-document))]
      [def-node (find-define-by-name root-index-node 'natural-order-compare)]
      [case-lambda-node (cadr (index-node-children def-node))])
    (construct-substitutions-for target-document)
    (test-assert "natural-order-compare case-lambda has substitutions"
      (> (length (index-node-substitution-list case-lambda-node)) 0)))
(test-end)

(test-begin "case-lambda parameter type access")
  (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))]
      [target-document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list target-document))]
      [target-index-node (find-index-node-recursive
                           (lambda (n) (eq? 'string-a (annotation-stripped-expression n)))
                           root-index-node)]
      [check-base (construct-type-expression-with-meta 'string?)])
    (construct-substitutions-for target-document)
    (test-equal #t
      (contain?
        (type:interpret-result-list target-index-node)
        check-base)))
(test-end)

(test-begin "case-lambda substitution generation for binary-search")
  (let* ([workspace (init-workspace (string-append (current-directory) "/util/") '() #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/binary-search.sls"))]
      [target-document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list target-document))]
      [def-node (find-define-by-name root-index-node 'binary-search)]
      [case-lambda-node (cadr (index-node-children def-node))])
    (construct-substitutions-for target-document)
    (test-assert "binary-search case-lambda has substitutions"
      (> (length (index-node-substitution-list case-lambda-node)) 0)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
