#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules do)
  (scheme-langserver analysis identifier rules library-import)
  (scheme-langserver analysis package-manager txt-filter)

  (scheme-langserver util text)
  (scheme-langserver util test)
  (scheme-langserver protocol alist-access-object)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "do-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/do")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [do-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'do (car expr)))))
      root-index-node)])
  (let* ([var-list-node (cadr (index-node-children do-node))]
      [var-children (index-node-children var-list-node)])
    (test-equal "var-list has 2 children" 2 (length var-children))
    (let ([result (do-process root-file-node root-library-node document do-node)])
      (test-equal "do-process returns list of 2" 2 (length result))
      (let* ([i-node (car var-children)]
          [j-node (cadr var-children)])
        (test-equal "do binds i"
          #t
          (not (null? (index-node-references-import-in-this-node i-node))))
        (test-equal "do binds j"
          #t
          (not (null? (index-node-references-import-in-this-node j-node))))))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
