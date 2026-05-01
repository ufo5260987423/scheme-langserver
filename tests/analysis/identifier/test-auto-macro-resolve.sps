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
  (scheme-langserver analysis identifier self-defined-rules ufo-match match)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(test-begin "compare auto-macro-resolve vs hand-written match-process")
  (let* ([workspace-instance (init-workspace (current-directory))]
     [root-file-node (workspace-file-node workspace-instance)]
     [root-library-node (workspace-library-node workspace-instance)]
     [file-linkage (workspace-file-linkage workspace-instance)]
     ; load.sls contains: (match expression [(_ (? string? path)) ...])
     [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/load.sls"))]
     [document (file-node-document target-file-node)]
     [root-index-node (car (document-index-node-list document))]
     [match-call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
        root-index-node)]
     [match-reference (car (find-available-references-for document match-call-node 'match))]
     [syntax-expander (identifier-reference-syntax-expander match-reference)]
     ; locate the pattern variable 'path inside the match clause
     [clause-node (caddr (index-node-children match-call-node))]
     [pattern-node (car (index-node-children clause-node))]
     [guard-node (cadr (index-node-children pattern-node))]
     [path-node (caddr (index-node-children guard-node))])

    ; Verify baseline: hand-written match-process attached 'path reference
    (let ([hand-written-exports (index-node-references-export-to-other-node path-node)])
      (test-equal "hand-written match-process attaches 'path reference"
        '(path)
        (map identifier-reference-identifier hand-written-exports))

      ; Save and clear so we can test auto-resolve from a clean state
      (let ([saved-exports hand-written-exports])
        (index-node-references-export-to-other-node-set! path-node '())
        (index-node-references-import-in-this-node-set! path-node '())

        ; Run auto-resolve via expansion-generator->rule.
        ; Known limitation: syntax-rules->generator:map+expansion has a
        ; fundamental bug in ellipse-pair-form handling (see pattern.sls) and
        ; private:expansion+index-node->pairs cannot cope with length mismatch
        ; between expanded compound-list and unexpanded expansion-index-node.
        ; Therefore auto-resolve for match currently crashes.
        (let ([auto-exports 
                (guard (c [else 'crash])
                  (let ([rule (expansion-generator->rule syntax-expander step file-linkage '() '())])
                    (rule root-file-node root-library-node document match-call-node))
                  (index-node-references-export-to-other-node path-node))])

          (test-equal "auto-resolve for match crashes due to ellipse-pair-form bug"
            'crash
            auto-exports)

          ; Restore hand-written state
          (index-node-references-export-to-other-node-set! path-node saved-exports)))))
(test-end)

; Verify that match calls containing '...' are rejected by tree-has?
(test-begin "auto macro resolution limitation: '...' in callee")
  (let* ([workspace-instance (init-workspace (current-directory))]
     [root-file-node (workspace-file-node workspace-instance)]
     [root-library-node (workspace-library-node workspace-instance)]
     [file-linkage (workspace-file-linkage workspace-instance)]
     [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/dependency/rules/library-import.sls"))]
     [document (file-node-document target-file-node)]
     [root-index-node (car (document-index-node-list document))]
     [match-clause-define (find-define-with-params root-index-node 'match-clause)]
     [match-call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
        match-clause-define)]
     [match-reference (car (find-available-references-for document match-call-node 'match))]
     [syntax-expander (identifier-reference-syntax-expander match-reference)])

    (let ([rule (expansion-generator->rule syntax-expander step file-linkage '() '())])
      (let ([result (rule root-file-node root-library-node document match-call-node)])
        (test-equal "match call with '...' returns empty list due to tree-has? guard"
          '()
          result))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
