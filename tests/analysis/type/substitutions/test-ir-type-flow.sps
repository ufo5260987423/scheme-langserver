#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; IR auto-route type flows: the expansion's positional facts (pattern
;; variable <- accessor chain over the scrutinee) become type substitutions
;; on the pattern-variable nodes, lazily interpreted by the type DSL.
;;   fixture scrutinee: (string->list "ab") : (inner:list? char? ...)
;;   [(a b) ...]      a : char? (car), b : cadr (chain composed of car/cdr
;;                    templates; the variadic tail keeps it unresolved --
;;                    asserted at substitution level)
;;   [(? char? n) n]  n : the scrutinee itself (base flow)
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver util contain)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis type domain-specific-language interpreter))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/ir-type-flow"))
(define consumer-path (string-append fixture "/consumer.scm.txt"))

(define (nth-bias text sub n)
  (let ([l (string-length sub)])
    (let loop ([i 0] [k 0])
      (cond
        [(> i (- (string-length text) l)) #f]
        [(and (string=? (substring text i (+ i l)) sub) (= k n)) i]
        [(string=? (substring text i (+ i l)) sub) (loop (+ i 1) (+ k 1))]
        [else (loop (+ i 1) k)]))))

(test-begin "IR type flows into match pattern variables")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #t)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [pattern-open (nth-bias text "(a b)" 0)]
      [a-node (pick-index-node-from index-node-list (+ pattern-open 1))]
      [b-node (pick-index-node-from index-node-list (+ pattern-open 3))]
      [n-node (pick-index-node-from index-node-list
                (+ (nth-bias text " n) n]" 0) 1))])

    (test-assert "IR path ran (identifier pass synthesized the pattern bindings)"
      (not (null? (index-node-references-export-to-other-node a-node))))

    (test-assert "pattern variable a gets a type substitution"
      (not (null? (index-node-substitution-list a-node))))
    (test-assert "a carries a value-flow substitution (virtual-tree node pointer)"
      (find index-node? (index-node-substitution-list a-node)))

    (test-assert "pattern variable b gets a type substitution (cadr chain)"
      (not (null? (index-node-substitution-list b-node))))

    (test-assert "n carries a value-flow substitution (guard binding in the expansion)"
      (not (null? (index-node-substitution-list n-node))))

    ;;the last ring: recursive interpretation resolves the whole
    ;;value-flow chain (a <- (car w) <- w <- (cdr v) <- v <-
    ;;(string->list "ab") <- (inner:list? char? ...)) down to char?
    (test-assert "pattern variable a recursively interprets to char?"
      (contain?
        (type:recursive-interpret-result-list a-node)
        (construct-type-expression-with-meta 'char?))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
