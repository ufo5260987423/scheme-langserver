#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis type substitutions generator)
  (scheme-langserver analysis type domain-specific-language interpreter)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system index-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-shared-pairs root)
  (let ([pairs '()])
    (let loop ([node root])
      (when (index-node-shared-reference node)
        (set! pairs (cons (cons (index-node-shared-reference node) node) pairs)))
      (for-each loop (index-node-children node)))
    pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "shared-reference type equals definition type")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/shared-type")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #t)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append fixture "/lib.scm.txt"))]
      [target-document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list target-document))]
      [pairs (collect-shared-pairs root-index-node)])

    (test-equal "found one shared-reference pair" 1 (length pairs))

    (construct-substitutions-for target-document)

    (let ([definition-node (caar pairs)]
          [reference-node (cdar pairs)])
      (test-assert "reference node has a type substitution"
        (not (null? (index-node-substitution-list reference-node))))
      (test-assert "reference node substitution points to definition node"
        (eq? definition-node (car (index-node-substitution-list reference-node))))
      (test-equal "reference type equals definition type"
        (type:interpret-result-list definition-node)
        (type:interpret-result-list reference-node))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
