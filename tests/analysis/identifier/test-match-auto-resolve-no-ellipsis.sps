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

(test-begin "match auto-resolve without ellipsis")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve")]
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
            (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
        root-index-node)])
    
    (display "\n[DEBUG] call-node expr: ")
    (pretty-print (annotation-stripped (index-node-datum/annotations call-node)))
    (display "[DEBUG] call-node children count: ")
    (display (length (index-node-children call-node)))
    (newline)
    
    (let* ([call-reference (car (find-available-references-for document call-node 'match))]
           [syntax-expander (identifier-reference-syntax-expander call-reference)]
           ; (match x [(? string? path) path])
           ; call-node children: [match x [(? string? path) path]]
           [clause-node (caddr (index-node-children call-node))]
           [pattern-node (car (index-node-children clause-node))]
           [path-node (caddr (index-node-children pattern-node))])

    ;; Call generator directly to inspect raw expansion
    (let ([gen-result (syntax-expander root-file-node root-library-node document call-node)])
      (when gen-result
        (let* ([pairs (car gen-result)]
               [expansion-node (cdr gen-result)]
               [expansion-expr (annotation-stripped (index-node-datum/annotations expansion-node))])
          (display "\n[DEBUG] expansion expression:\n  ")
          (pretty-print expansion-expr)
          (display "\n[DEBUG] pairs count: ")
          (display (length pairs))
          (newline))))

    ;; Run auto-resolve via expansion-generator->rule
    (let ([rule (expansion-generator->rule syntax-expander step file-linkage '() '())])
      (rule root-file-node root-library-node document call-node))

    (let ([exports (index-node-references-export-to-other-node path-node)])
      (display "\n[DEBUG] path-node exports: ")
      (display (map identifier-reference-identifier exports))
      (newline)
      (test-assert "auto-resolve match attaches 'path reference"
        (and (not (null? exports))
             (find (lambda (id) (eq? 'path id)) 
                   (map identifier-reference-identifier exports)))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
