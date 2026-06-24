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

(define (diagnose-message diagnose)
  (if (>= (length diagnose) 4)
    (list-ref diagnose 3)
    ""))

(define (diagnose-source diagnose)
  (if (>= (length diagnose) 5)
    (list-ref diagnose 4)
    #f))

(define (diagnose-code diagnose)
  (if (>= (length diagnose) 6)
    (list-ref diagnose 5)
    #f))

(define (find-diagnose diagnoses message)
  (find 
    (lambda (d) (string=? (diagnose-message d) message))
    diagnoses))

(define (find-published-diagnostic diagnostics message)
  (find
    (lambda (d) (string=? message (assq-ref d 'message)))
    (vector->list diagnostics)))

(define (run-unused-import-test fixture-name file-name expected-message)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/" fixture-name)]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append fixture "/" file-name))]
      [document (file-node-document target-file-node)]
      [diagnoses (document-diagnoses document)])
    ; (pretty-print diagnoses)
    (test-equal "at least one diagnostic" #t (not (null? diagnoses)))
    (let ([diag (find-diagnose diagnoses expected-message)])
      (test-equal (string-append "contains " expected-message)
        #t
        (not (eq? diag #f)))
      (when (not (eq? diag #f))
        (test-equal "severity is Warning" 2 (list-ref diag 2))
        (test-equal "source is import" "import" (diagnose-source diag))
        (test-equal "code is unused-import" "unused-import" (diagnose-code diag))))
    ; Verify the LSP-published shape too (source, code, tags).
    (workspace-undiagnosed-paths-set! workspace (list (uri->path (document-uri document))))
    (let* ([published (unpublish-diagnostics->list workspace)]
        [lsp-diagnostics (if (null? published) (vector) (assq-ref (car published) 'diagnostics))]
        [lsp-diag (find-published-diagnostic lsp-diagnostics expected-message)])
      (test-equal (string-append "published contains " expected-message)
        #t
        (not (eq? lsp-diag #f)))
      (when (not (eq? lsp-diag #f))
        (test-equal "lsp source is import" "import" (assq-ref lsp-diag 'source))
        (test-equal "lsp code is unused-import" "unused-import" (assq-ref lsp-diag 'code))
        (test-equal "lsp tags is Unnecessary" (vector 1) (assq-ref lsp-diag 'tags))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused import detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "unused import in only clause")
  (run-unused-import-test "unused-import-test" "test2.scm.txt" "Unused import: car")
  (run-unused-import-test "unused-import-test" "test2.scm.txt" "Unused import: cdr")
(test-end)

(test-begin "unused plain import of user library")
  (run-unused-import-test "unused-import-test" "test.scm.txt" "Unused import: (unused-import-test lib)")
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
