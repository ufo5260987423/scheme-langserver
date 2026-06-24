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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate identifier detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-duplicate-test fixture-name expected-message)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/" fixture-name)]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append fixture "/test.scm.txt"))]
      [document (file-node-document target-file-node)]
      [diagnoses (document-diagnoses document)])
    ; (pretty-print diagnoses)
    (test-equal "at least one diagnostic" #t (not (null? diagnoses)))
    (let ([dup (find-diagnose diagnoses expected-message)])
      (test-equal (string-append "contains " expected-message)
        #t
        (not (eq? dup #f)))
      (when (not (eq? dup #f))
        (test-equal "severity is Error" 1 (list-ref dup 2))
        (test-equal "source is identifier" "identifier" (diagnose-source dup))
        (test-equal "code is duplicate-identifier" "duplicate-identifier" (diagnose-code dup))))))

(test-begin "duplicate identifier in define")
  (run-duplicate-test "duplicate-test" "Duplicate identifier: x")
(test-end)

(test-begin "duplicate identifier in lambda")
  (run-duplicate-test "duplicate-lambda-test" "Duplicate identifier: x")
(test-end)

(test-begin "duplicate identifier in let")
  (run-duplicate-test "duplicate-let-test" "Duplicate identifier: x")
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
