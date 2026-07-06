#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules s7 lambda*)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "lambda*-process handles shared-reference parameter list")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/s7-lambda-star")]
      [root-file-node (init-virtual-file-system fixture '() (lambda (fuzzy) #t) 's7)]
      [root-library-node '()]
      [target-file-node (walk-file root-file-node (string-append fixture "/lib.scm.txt"))]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [lambda*-node (caddr (index-node-children root-index-node))])

    ;; Should not raise on the shared-reference parameter node.
    (lambda*-process root-file-node root-library-node document lambda*-node)

    ;; The non-shared parameter 'x' should be bound; the shared reference 'y'
    ;; is skipped by parameter*-process, so at least one binding exists.
    (test-assert "at least one parameter reference is recorded"
      (not (null? (index-node-references-import-in-this-node lambda*-node))))

    (test-assert "parameter 'x' is bound"
      (find 
        (lambda (reference) 
          (equal? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda*-node))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
