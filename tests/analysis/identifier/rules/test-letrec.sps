#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules letrec)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "letrec-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/letrec")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [letrec-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'letrec (car expr))
               (>= (length expr) 2)
               (list? (cadr expr)))))
      root-index-node)])
  (letrec-process root-file-node root-library-node document letrec-node)
  (test-equal "letrec binds even?"
    #t
    (not
      (null?
        (filter
          (lambda (reference)
            (eq? 'even? (identifier-reference-identifier reference)))
          (index-node-references-import-in-this-node letrec-node)))))
  (test-equal "letrec binds odd?"
    #t
    (not
      (null?
        (filter
          (lambda (reference)
            (eq? 'odd? (identifier-reference-identifier reference)))
          (index-node-references-import-in-this-node letrec-node))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
