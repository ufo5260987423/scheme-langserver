#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Differential guard: the expansion-driven production type inference
;; (type:expand-and-infer via ir-auto-route:expand-call-site) vs the IR
;; flow extractor (ir-auto-route:call-site-flows), which is true by
;; construction. Every binding the IR says a pattern variable occupies
;; MUST receive at least one substitution from the production path --
;; this is the drift insurance against silently dropped bindings in the
;; rebuild/virtual-document glue (the production bug class that killed
;; the type chain on the w reference in (car w)). The original guarded
;; the hand-written match-family rule, retired 2026-10-27; the test now
;; guards its replacement architecture.
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier expander ir-auto-route))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

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

(test-begin "expansion-driven inference coverage vs IR flow ground truth")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #t)]
      [root-library-node (workspace-library-node workspace-instance)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [match-node (pick-index-node-from index-node-list (nth-bias text "(match " 0))]
      ;; the IR ground truth (oracle only -- no production caller)
      [flows (ir-auto-route:call-site-flows root-library-node document match-node)]
      ;; every IR flow binding must have been typed by the production pass
      [uncovered
        (filter
          (lambda (flow)
            (let* ([bias (car flow)]
                [node (pick-index-node-from index-node-list bias)])
              (or (not (index-node? node))
                (null? (index-node-substitution-list node)))))
          flows)])
    (test-assert "IR flow extractor produced ground truth" (pair? flows))
    (test-assert "every IR-reported pattern binding is typed by the production pass"
      (null? uncovered)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
