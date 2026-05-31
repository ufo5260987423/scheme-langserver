#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules fluid-let)
  (scheme-langserver analysis identifier rules library-import)
  (scheme-langserver analysis package-manager txt-filter)

  (scheme-langserver util text)
  (scheme-langserver util test)
  (scheme-langserver protocol alist-access-object)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "fluid-let-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/fluid-let")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [fluid-let-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'fluid-let (car expr)))))
      root-index-node)])
  (fluid-let-process root-file-node root-library-node document fluid-let-node)
  (test-equal "fluid-let binds x"
    #t
    (not
      (null?
        (filter
          (lambda (reference)
            (eq? 'x (identifier-reference-identifier reference)))
          (index-node-references-import-in-this-node fluid-let-node)))))
  (test-equal "fluid-let binds y"
    #t
    (not
      (null?
        (filter
          (lambda (reference)
            (eq? 'y (identifier-reference-identifier reference)))
          (index-node-references-import-in-this-node fluid-let-node))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
