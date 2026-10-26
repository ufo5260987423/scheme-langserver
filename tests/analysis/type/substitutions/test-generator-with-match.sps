#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Type inference over a match call site (the previously untested
;; combination). The IR path synthesizes SPECULATIVE references for the
;; pattern variables; the type-side rule dispatch must not treat a
;; pattern sublist as a procedure application:
;;   [(a b) (+ a b)] -- the (a b) pattern gets NO substitution
;;   while the real application (+ a b) in the clause body keeps its own.
;; Also locks in: no type warnings/diagnoses anywhere in the document.
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis workspace))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/ir-clause-scope"))
(define consumer-path (string-append fixture "/consumer.scm.txt"))

(define (nth-bias text sub n)
  (let ([l (string-length sub)])
    (let loop ([i 0] [k 0])
      (cond
        [(> i (- (string-length text) l)) #f]
        [(and (string=? (substring text i (+ i l)) sub) (= k n)) i]
        [(string=? (substring text i (+ i l)) sub) (loop (+ i 1) (+ k 1))]
        [else (loop (+ i 1) k)]))))

(test-begin "type inference over match call sites")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #t)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [pattern-node (pick-index-node-from index-node-list (nth-bias text "(a b)" 0))]
      [body-node (pick-index-node-from index-node-list (nth-bias text "(+ a b)" 0))]
      [diagnoses (document-diagnoses document)])
    (test-assert "IR path took the call site (identifier pass ran)"
      (not (null? (index-node-references-export-to-other-node
                    (pick-index-node-from index-node-list (+ (nth-bias text "[(a" 0) 2))))))
    (test-assert "pattern sublist is NOT typed as an application"
      (null? (index-node-substitution-list pattern-node)))
    (test-assert "real application in the clause body keeps its substitution"
      (not (null? (index-node-substitution-list body-node))))
    (test-assert "no type warnings or any diagnoses"
      (null? diagnoses)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
