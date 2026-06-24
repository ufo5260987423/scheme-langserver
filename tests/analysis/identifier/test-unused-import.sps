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
;; Unused import detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "unused import in only clause")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/unused-import-test")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node (string-append fixture "/test2.scm.txt"))]
      [document (file-node-document target-file-node)]
      [diagnoses (document-diagnoses document)])
    (test-equal "at least one diagnostic" #t (not (null? diagnoses)))
    (let ([car-diag (find-diagnose diagnoses "Unused import: car")]
          [cdr-diag (find-diagnose diagnoses "Unused import: cdr")])
      (test-equal "contains Unused import: car" #t (not (eq? car-diag #f)))
      (test-equal "contains Unused import: cdr" #t (not (eq? cdr-diag #f)))
      (when (not (eq? car-diag #f))
        (test-equal "severity is Warning" 2 (list-ref car-diag 2))
        (test-equal "source is import" "import" (diagnose-source car-diag))
        (test-equal "code is unused-import" "unused-import" (diagnose-code car-diag)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
