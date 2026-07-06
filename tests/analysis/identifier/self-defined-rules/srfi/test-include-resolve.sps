#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "include-resolve-process handles shared-reference nodes without crash")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/include-resolve-shared")]
      [root-file-node (init-virtual-file-system fixture '() (lambda (fuzzy) #t) 'r6rs)]
      [root-library-node '()]
      [target-file-node (walk-file root-file-node (string-append fixture "/lib.scm.txt"))]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [include-node (list-ref (index-node-children root-index-node) 5)])

    ;; Should not raise even though the document contains shared references.
    (include-resolve-process root-file-node root-library-node document include-node (lambda (doc) '()))

    ;; References from the included file are appended to the library ancestor.
    ;; (The no-op step function means we only verify the merge mechanism runs,
    ;;  not that individual included identifiers are resolved.)
    (test-assert "references merged into library node"
      (> (length (index-node-references-import-in-this-node root-index-node)) 0)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
