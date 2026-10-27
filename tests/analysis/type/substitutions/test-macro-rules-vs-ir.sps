#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Differential guard: the hand-written match-family type rule
;; (self-defined-rules/ufo-match/match.sls) vs the IR flow extractor
;; (ir-auto-route:call-site-flows), which is true by construction. Every
;; binding the IR says a pattern variable occupies MUST receive at least
;; one substitution from the family rule -- this is the drift insurance
;; for hand-written pattern semantics (the retired match-process died by
;; silently drifting from the macro's real behavior).
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier expander ir-auto-route))

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

(test-begin "match-family rule coverage vs IR flow ground truth")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #t)]
      [root-library-node (workspace-library-node workspace-instance)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [match-node (pick-index-node-from index-node-list (nth-bias text "(match " 0))]
      ;; the IR ground truth (oracle only -- no production caller)
      [flows (ir-auto-route:call-site-flows root-library-node document match-node)]
      ;; every IR flow binding must have been typed by the family rule
      [uncovered
        (filter
          (lambda (flow)
            (let* ([bias (car flow)]
                [node (pick-index-node-from index-node-list bias)])
              (or (not (index-node? node))
                (null? (index-node-substitution-list node)))))
          flows)])
    (test-assert "IR flow extractor produced ground truth" (pair? flows))
    (test-assert "every IR-reported pattern binding is typed by the family rule"
      (null? uncovered)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
