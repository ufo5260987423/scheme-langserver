#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver protocol apis document-diagnostic)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis workspace)
  (scheme-langserver util association)
  (scheme-langserver util path))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/unused-local-variable-test"))

(define (diagnose-message diagnose)
  (if (>= (length diagnose) 4)
    (list-ref diagnose 3)
    ""))

(define (find-diagnose diagnoses message)
  (find 
    (lambda (d) (string=? (diagnose-message d) message))
    diagnoses))

(define (find-published-diagnostic diagnostics message)
  (find
    (lambda (d) (string=? message (assq-ref d 'message)))
    (vector->list diagnostics)))

(define (run-unused-local-test file-name expected-messages not-expected-messages)
  (let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append fixture "/" file-name))]
      [document (file-node-document target-file-node)]
      [diagnoses (document-diagnoses document)])
    ; (pretty-print diagnoses)
    (for-each
      (lambda (msg)
        (let ([diag (find-diagnose diagnoses msg)])
          (test-equal (string-append "contains " msg) #t (not (eq? diag #f)))
          (when (not (eq? diag #f))
            (test-equal "severity is Warning" 2 (list-ref diag 2))
            (test-equal "source is identifier" "identifier" (if (>= (length diag) 5) (list-ref diag 4) #f))
            (test-equal "code is unused-local-variable" "unused-local-variable" (if (>= (length diag) 6) (list-ref diag 5) #f)))))
      expected-messages)
    (for-each
      (lambda (msg)
        (test-equal (string-append "does not contain " msg) #f (find-diagnose diagnoses msg)))
      not-expected-messages)
    ; Verify LSP-published shape (source, code, tags).
    (workspace-undiagnosed-paths-set! workspace (list (uri->path (document-uri document))))
    (let* ([published (unpublish-diagnostics->list workspace)]
        [lsp-diagnostics (if (null? published) (vector) (assq-ref (car published) 'diagnostics))])
      (for-each
        (lambda (msg)
          (let ([lsp-diag (find-published-diagnostic lsp-diagnostics msg)])
            (test-equal (string-append "published contains " msg) #t (not (eq? lsp-diag #f)))
            (when (not (eq? lsp-diag #f))
              (test-equal "lsp source is identifier" "identifier" (assq-ref lsp-diag 'source))
              (test-equal "lsp code is unused-local-variable" "unused-local-variable" (assq-ref lsp-diag 'code))
              (test-equal "lsp tags is Unnecessary" (vector 1) (assq-ref lsp-diag 'tags)))))
        expected-messages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused local variable detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "unused local variables in library")
(run-unused-local-test "lib.scm.txt"
  (list
    "Unused local variable: unused-var"
    "Unused local variable: unused-proc"
    "Unused local variable: p"
    "Unused local variable: q"
    "Unused local variable: a"
    "Unused local variable: b"
    "Unused local variable: c"
    "Unused local variable: d"
    "Unused local variable: e"
    "Unused local variable: let-unused"
    "Unused local variable: let*-unused"
    "Unused local variable: letrec-unused"
    "Unused local variable: let-values-unused"
    "Unused local variable: do-unused"
    "Unused local variable: lambda-unused"
    "Unused local variable: case-lambda-unused"
    "Unused local variable: with-syntax-unused")
  (list
    "Unused local variable: used-proc"
    "Unused local variable: x"))
(test-end)

(test-begin "unused local variables in script")
(run-unused-local-test "script.scm.txt"
  (list "Unused local variable: unused-top-level-var")
  (list "Unused local variable: used-top-level-proc"))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
