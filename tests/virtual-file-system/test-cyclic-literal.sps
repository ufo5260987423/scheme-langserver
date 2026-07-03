#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver analysis tokenizer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-cyclic-definition/reference-pair node)
  (let ([children (index-node-children node)])
    (if (and (= (length children) 2)
             (index-node-shared-reference (cadr children))
             (null? (index-node-children (cadr children))))
      (cons node (cadr children))
      (let loop ([children children])
        (if (null? children)
          #f
          (or (find-cyclic-definition/reference-pair (car children))
              (loop (cdr children))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-index-node handles dotted cyclic pair")
  (let* ([fixture-path (string-append (current-directory) "/tests/resources/workspace-fixtures/cyclic-literal/lib.scm.txt")]
      [root (init-index-node '() (car (source-file->annotations fixture-path)))]
      [result (find-cyclic-definition/reference-pair root)])
    (test-assert "cyclic definition/reference pair found" result)
    (let ([definition-node (car result)]
          [reference-node (cdr result)])
      (test-equal "definition node has 2 children" 2 (length (index-node-children definition-node)))
      (test-equal "reference node has 0 children" 0 (length (index-node-children reference-node)))
      (test-assert "shared-reference points to definition node"
        (eq? (index-node-shared-reference reference-node) definition-node))
      (test-assert "reference node source is inside definition node source"
        (and (>= (index-node-start reference-node) (index-node-start definition-node))
             (<= (index-node-end reference-node) (index-node-end definition-node))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
