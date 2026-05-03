#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver util text)
  (scheme-langserver util path)
  (scheme-langserver util test)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)

  (scheme-langserver protocol alist-access-object)

  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(test-begin "auto-resolve simple-let macro without ellipses")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-macro-auto-resolve")]
     [workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
     [root-file-node (workspace-file-node workspace-instance)]
     [root-library-node (workspace-library-node workspace-instance)]
     [file-linkage (workspace-file-linkage workspace-instance)]
     [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
     [document (file-node-document target-file-node)]
     [root-index-node (car (document-index-node-list document))]
     [call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'simple-let (car expr)))))
        root-index-node)]
     [call-reference (car (find-available-references-for document call-node 'simple-let))]
     [syntax-expander (identifier-reference-syntax-expander call-reference)]
     ; locate the pattern variable 'x inside the simple-let call
     ; (simple-let ((x 1)) x)
     ; call-node children: [simple-let ((x 1)) x]
     [binding-list-node (cadr (index-node-children call-node))]
     [binding-node (car (index-node-children binding-list-node))]
     [var-node (car (index-node-children binding-node))])

    ; Verify auto-resolve for a simple macro without ellipses.
    ; Manually trigger the expansion rule since router.sls does not
    ; auto-create rules for custom macros yet (to avoid performance issues).
    (let ([rule (expansion-generator->rule syntax-expander step file-linkage '() '())])
      (rule root-file-node root-library-node document call-node))

    (let ([exports (index-node-references-export-to-other-node var-node)])
      (test-equal "auto-resolve simple-let attaches 'x reference after bug3 fix"
        '(x)
        (map identifier-reference-identifier exports))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
