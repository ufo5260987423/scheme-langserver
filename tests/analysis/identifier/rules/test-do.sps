#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier rules do)
  (scheme-langserver analysis identifier reference)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "do-process")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/do")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [target-file-node (walk-file root-file-node (string-append fixture "/do-scope.scm.txt"))]
    [document (file-node-document target-file-node)]
    [root-index-node (car (document-index-node-list document))]
    [do-node (find-index-node-recursive
      (lambda (node)
        (let ([expr (annotation-stripped-expression node)])
          (and (list? expr) (not (null? expr))
               (eq? 'do (car expr)))))
      root-index-node)]
    [binding-list-node (cadr (index-node-children do-node))]
    [i-clause (car (index-node-children binding-list-node))]
    [j-clause (cadr (index-node-children binding-list-node))]
    [i-var (car (index-node-children i-clause))]
    [j-var (car (index-node-children j-clause))]
    [i-init (cadr (index-node-children i-clause))]
    [i-step (caddr (index-node-children i-clause))]
    [j-step (caddr (index-node-children j-clause))]
    [test-node (caddr (index-node-children do-node))]
    [body-node (cadddr (index-node-children do-node))])

  (test-equal "do imports variables at do-node"
    #t
    (not (null? (index-node-references-import-in-this-node do-node))))

  (test-equal "do exports i"
    #t
    (not (null? (index-node-references-export-to-other-node i-var))))

  (test-equal "do exports j"
    #t
    (not (null? (index-node-references-export-to-other-node j-var))))

  (test-equal "do variable is not visible in its own init expression"
    '()
    (find-available-references-for document i-init
      (annotation-stripped-expression i-init)))

  (test-equal "do variable is visible in its own step expression"
    #t
    (not (null?
      (find-available-references-for document
        (find-index-node-recursive
          (lambda (node)
            (eq? 'i (annotation-stripped-expression node)))
          i-step)
        'i))))

  (test-equal "do sibling variable is visible in step expression"
    #t
    (not (null?
      (find-available-references-for document
        (find-index-node-recursive
          (lambda (node)
            (eq? 'i (annotation-stripped-expression node)))
          j-step)
        'i))))

  (test-equal "do variable is visible in test expression"
    #t
    (not (null?
      (find-available-references-for document
        (find-index-node-recursive
          (lambda (node)
            (eq? 'i (annotation-stripped-expression node)))
          test-node)
        'i))))

  (test-equal "do variable is visible in body"
    #t
    (not (null?
      (find-available-references-for document
        (find-index-node-recursive
          (lambda (node)
            (eq? 'i (annotation-stripped-expression node)))
          body-node)
        'i)))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
