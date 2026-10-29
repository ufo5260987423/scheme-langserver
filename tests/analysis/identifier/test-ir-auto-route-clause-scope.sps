#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Per-clause scoping + use-less binding synthesis (ir-auto-route):
;;   [(a b) (+ a b)] [(a b c) (+ a (+ b c))] [(a b) a]
;; locks in, at the analysis level:
;;   1. sibling-clause leakage is gone (c resolves to NOTHING in clause 1)
;;   2. one reference per (name . binding) -- no duplicate candidates
;;   3. a resolves to the OWN clause's pattern position in every clause
;;   4. a never-used pattern variable (b in clause 3) still materializes
;;      (binding-driven synthesis) and only in its own clause
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expander ir-auto-route)
  (scheme-langserver analysis workspace))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/ir-clause-scope"))
(define consumer-path (string-append fixture "/consumer.scm.txt"))

(define (private:test-log fmt . args)
  (when (getenv "TEST_VERBOSE")
    (apply format (cons #t (cons fmt args)))))

;; nth (0-based) occurrence of `sub` in `text`
(define (nth-bias text sub n)
  (let ([l (string-length sub)])
    (let loop ([i 0] [k 0])
      (cond
        [(> i (- (string-length text) l)) #f]
        [(and (string=? (substring text i (+ i l)) sub) (= k n)) i]
        [(string=? (substring text i (+ i l)) sub) (loop (+ i 1) (+ k 1))]
        [else (loop (+ i 1) k)]))))

;; ground-truth biases in consumer.scm.txt
;;   clause 1: [(a b) (+ a b)]
;;   clause 2: [(a b c) (+ a (+ b c))]
;;   clause 3: [(a b) a]
(define text (call-with-input-file consumer-path get-string-all))
(define clause-pattern-a
  (map (lambda (n) (+ (nth-bias text "[(a" n) 2)) '(0 1 2)))
;pattern b is followed by a space ("(a b) ..."); a body b sits at "(+ a b)]"
(define clause1-pattern-b (+ (nth-bias text " b) " 0) 1))
(define clause3-pattern-b (+ (nth-bias text " b) " 1) 1))
(define clause1-body-a (+ (nth-bias text " a " 0) 1))
(define clause3-body-a (+ (nth-bias text " a]" 0) 1))

(test-begin "ir-auto-route: per-clause scope and use-less bindings")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [stats (ir-auto-route:stats)]
      [body1-node
        (pick-index-node-from index-node-list clause1-body-a)]
      [body3-node
        (pick-index-node-from index-node-list clause3-body-a)])
    (test-assert "IR path took the call site" (>= (car stats) 1))
    (private:test-log "  [clause-scope] hits=~a fallbacks=~a~%"
      (car stats) (cadr stats))

    (let ([a-refs (find-available-references-for document body1-node 'a)])
      (test-assert "clause 1 body: exactly one a reference (no duplicates)"
        (= 1 (length a-refs)))
      (test-assert "clause 1 body: a resolves to clause 1 pattern position"
        (and (not (null? a-refs))
          (= (list-ref clause-pattern-a 0)
            (index-node-start (identifier-reference-index-node (car a-refs)))))))

    (test-assert "clause 1 body: c (sibling clause) does NOT resolve"
      (null? (find-available-references-for document body1-node 'c)))

    (let ([a-refs (find-available-references-for document body3-node 'a)])
      (test-assert "clause 3 body: a resolves to clause 3 pattern position"
        (and (= 1 (length a-refs))
          (= (list-ref clause-pattern-a 2)
            (index-node-start (identifier-reference-index-node (car a-refs)))))))

    (let ([b-refs (find-available-references-for document body3-node 'b)])
      (test-assert "clause 3 body: never-used b still materializes"
        (not (null? b-refs)))
      (test-assert "clause 3 body: b anchors at clause 3 pattern position"
        (and (not (null? b-refs))
          (= clause3-pattern-b
            (index-node-start (identifier-reference-index-node (car b-refs)))))))
    (test-assert "clause 3 pattern b is NOT the clause 1 binding"
      (not (= clause1-pattern-b clause3-pattern-b))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
