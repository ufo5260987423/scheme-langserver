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
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis package-manager txt-filter))

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

(define (get-script-diagnoses fixture filename)
  (let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
     [root-file-node (workspace-file-node workspace)]
     [script-path (string-append fixture "/" filename)]
     [script-node (walk-file root-file-node script-path)]
     [doc (file-node-document script-node)])
    (document-diagnoses doc)))

(define (run-duplicate-import-test filename expected-message)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/duplicate-import-test")]
      [diagnoses (get-script-diagnoses fixture filename)]
      [diag (find-diagnose diagnoses expected-message)])
    (test-equal (string-append "contains " expected-message)
      #t
      (not (eq? diag #f)))
    (when (not (eq? diag #f))
      (test-equal "severity is Warning" 2 (list-ref diag 2))
      (test-equal "source is import" "import" (diagnose-source diag))
      (test-equal "code is duplicate-import" "duplicate-import" (diagnose-code diag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate import detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "duplicate plain imports")
  (run-duplicate-import-test "plain-duplicate.scm.txt" "Duplicate import: (rnrs)")
(test-end)

(test-begin "duplicate import after modifier")
  (run-duplicate-import-test "modifier-duplicate.scm.txt" "Duplicate import: (rnrs)")
(test-end)

(test-begin "duplicate import after prefix")
  (run-duplicate-import-test "prefix-duplicate.scm.txt" "Duplicate import: (rnrs)")
(test-end)

(test-begin "no false positive for different library identifiers")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/duplicate-import-test")]
      [diagnoses (get-script-diagnoses fixture "no-duplicate.scm.txt")])
    (test-equal "no duplicate-import diagnostic"
      #f
      (find-diagnose diagnoses "Duplicate import: (rnrs)"))
    (test-equal "no duplicate-import diagnostic for rnrs base"
      #f
      (find-diagnose diagnoses "Duplicate import: (rnrs base)")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
