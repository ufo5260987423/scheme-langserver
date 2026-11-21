#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis identifier reference))

(define (has-identifier? refs id)
  (not (null? (filter (lambda (r) (eq? id (identifier-reference-identifier r))) refs))))

;; R7RS-small Appendix A names these libraries with hyphens.
(for-each
  (lambda (top-environment)
    (test-begin (string-append "R7RS hyphenated library names: " (symbol->string top-environment)))
    (test-assert "(scheme process-context) is a meta library"
      (meta-library? '(scheme process-context) top-environment))
    (test-assert "(scheme case-lambda) is a meta library"
      (meta-library? '(scheme case-lambda) top-environment))
    (test-assert "(scheme process-context) exports emergency-exit"
      (has-identifier? (find-meta '(scheme process-context) top-environment) 'emergency-exit))
    (test-assert "(scheme case-lambda) exports case-lambda"
      (has-identifier? (find-meta '(scheme case-lambda) top-environment) 'case-lambda))
    (test-assert "space-separated (scheme process context) is not a library"
      (not (meta-library? '(scheme process context) top-environment)))
    (test-assert "space-separated (scheme case lambda) is not a library"
      (not (meta-library? '(scheme case lambda) top-environment)))
    (test-end))
  '(r7rs s7))

(test-begin "R7RS script importing standard libraries has no library-not-found")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/r7rs-standard-libraries")]
      [workspace (init-workspace fixture 'txt 'r7rs #f #f)]
      [script-node (walk-file (workspace-file-node workspace) (string-append fixture "/script.scm.txt"))]
      [diagnoses (document-diagnoses (file-node-document script-node))]
      [not-found
        (filter
          (lambda (d) (and (>= (length d) 6) (equal? "library-not-found" (list-ref d 5))))
          diagnoses)])
    (test-equal "no library-not-found diagnoses" '() not-found))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
