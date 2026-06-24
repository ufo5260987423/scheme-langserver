#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis workspace))

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
        (test-equal "code is unused-import" "unused-import" (diagnose-code diag))))))

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
