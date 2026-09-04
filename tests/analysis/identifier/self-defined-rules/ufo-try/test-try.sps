#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier self-defined-rules ufo-try try)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(define (all-nodes node)
  (cons node (apply append (map all-nodes (index-node-children node)))))

(define (try-form? node)
  (let ([e (index-node-expression node)])
    (and (pair? e) (eq? (car e) 'try))))

(define (exported-c-node try-node)
  (find
    (lambda (n)
      (not (null? (index-node-references-export-to-other-node n))))
    (all-nodes try-node)))

(test-begin "try-process registers except condition variable for all shapes")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/try-shapes")]
      [root-file-node (init-virtual-file-system fixture '() (lambda (fuzzy) #t) 'r6rs)]
      [root-library-node '()]
      [target-file-node (walk-file root-file-node (string-append fixture "/try.scm.txt"))]
      [document (file-node-document target-file-node)]
      [tries (filter try-form?
              (apply append
                (map all-nodes (document-index-node-list document))))])
    (test-assert "fixture contains 4 try forms" (= 4 (length tries)))
    (for-each
      (lambda (try-node)
        (try-process root-file-node root-library-node document try-node)
        (let* ([children (index-node-children try-node)]
            [except-node (car (reverse children))]
            [expected-c-node (cadr (index-node-children except-node))]
            [c-node (exported-c-node try-node)])
          (test-assert (string-append "exports c: " (format "~a" (index-node-expression try-node)))
            (and c-node (eq? expected-c-node c-node)))
          (test-assert (string-append "reference is c: " (format "~a" (index-node-expression try-node)))
            (and c-node
                 (eq? 'c
                      (identifier-reference-identifier
                        (car (index-node-references-export-to-other-node c-node))))))))
      tries))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
