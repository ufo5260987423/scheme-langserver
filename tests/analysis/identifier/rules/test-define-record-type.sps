#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules define-record-type)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(define (has-identifier? refs id)
  (not (null? (filter (lambda (r) (eq? id (identifier-reference-identifier r))) refs))))

(test-begin "define-record-type-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/define-record-type")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [drt-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'define-record-type (car expr)))))
      root-index-node)])
  (define-record-type-process root-file-node root-library-node document drt-node)
  (let ([parent-refs (index-node-references-import-in-this-node (index-node-parent drt-node))])
    (test-equal "define-record-type creates point-x getter"
      #t
      (has-identifier? parent-refs 'point-x))
    (test-equal "define-record-type creates point-x-set! setter"
      #t
      (has-identifier? parent-refs 'point-x-set!))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
