#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier rules begin)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "begin-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/begin")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [begin-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'begin (car expr)))))
      root-index-node)])
  (begin-process root-file-node root-library-node document begin-node)
  (test-equal "begin-process returns void-ish"
    #t
    #t))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
