#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

;; Q2 lazy expansion: a macro call with an expander but no hand-written
;; rule stays inert during init; a failed symbol resolution inside it
;; triggers on-demand expansion (lazy-expand-enclosing-call), after
;; which the reference network answers the query.

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier self-defined-rules router)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(test-begin "lazy-expand-enclosing-call")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-macro-auto-resolve")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [file-linkage (workspace-file-linkage workspace)]
    [document (file-node-document
        (walk-file root-file-node (string-append fixture "/consumer.scm.txt")))]
    [call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'simple-let (car expr)))))
        (car (document-index-node-list document)))]
    ;the body `x` -- last child of the call
    [body-x (car (reverse (index-node-children call-node)))]
    [macro-reference
      (car (find-available-references-for document call-node 'simple-let))]
    [before (find-available-references-for document body-x 'x)])

  (test-equal "body x is a leaf symbol" 'x
    (annotation-stripped-expression body-x))

  (test-assert "no hand-written rule covers simple-let"
    (not (hand-written-rule-covers? macro-reference)))

  (test-assert "x inside the macro call does not resolve before lazy expansion"
    (null? before))

  (test-assert "syntax-expander is available on the macro reference"
    (procedure? (identifier-reference-syntax-expander macro-reference)))

  (test-assert "lazy expansion triggers on the enclosing call"
    (lazy-expand-enclosing-call root-file-node root-library-node file-linkage
      document body-x step))

  ;back-propagation attaches the let-bound reference to the binding x
  ;(same semantics as test-auto-resolve-basic)
  (let* ([binding-list (cadr (index-node-children call-node))]
      [binding (car (index-node-children binding-list))]
      [binding-x (car (index-node-children binding))]
      [exports (index-node-references-export-to-other-node binding-x)])
    (test-assert "binding x gains references after lazy expansion"
      (not (null? exports)))
    (test-assert "the back-propagated references are speculative"
      (and (not (null? exports))
        (andmap identifier-reference-speculative? exports)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
