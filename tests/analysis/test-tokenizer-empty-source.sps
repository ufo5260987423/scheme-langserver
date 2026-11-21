#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

;; An empty document (e.g. the user deleted all of a file's content) must
;; tokenize to no annotations instead of raising.

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis tokenizer))

(define path (string-append (current-directory) "/tests/resources/tmp-tokenizer-empty-source.scm"))

(test-begin "tokenizer: empty source")
  (call-with-output-file path (lambda (port) (display "(a)" port)) 'replace)
  (test-equal "consume-sps-auxiliary of empty source" 0 (consume-sps-auxiliary ""))
  (test-equal "empty source yields no annotations"
    '()
    (guard (e [else (list 'raised (condition-message e))])
      (source-file->annotations "" path (consume-sps-auxiliary "") #t #f 'r6rs)))
  (delete-file path)
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
