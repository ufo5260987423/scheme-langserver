#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Controlled parallel phase test: with SLSCHEME_IR_ROUTE on, the router's
;; ufo-match arm resolves the call site through Chez's own expander IR
;; (ir-auto-route) instead of route 2's generator pipeline, and the analysis
;; must still satisfy the IR ground truth. With the gate off (every other
;; test), ir-auto-route is inert and behavior is unchanged.
#!r6rs

(import
  (chezscheme)
  (ufo-match)
  (srfi :64 testing)

  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier chez-ir)
  (scheme-langserver analysis identifier ir-auto-route)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(putenv "SLSCHEME_IR_ROUTE" "1")

(test-begin "ir-auto-route: parallel Chez-IR path through the real workspace init")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/sc-expand-differential")]
      [consumer-path (string-append fixture "/consumer.scm.txt")]
      [workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace-instance)]
      [document (file-node-document (walk-file root-file-node consumer-path))]
      [index-node-list (document-index-node-list document)]
      ;ground truth, independent of the analysis under test
      [annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (s) (and (pair? s) (eq? 'define (car s))))
          annotated-toplevel)]
      [ground-truth (chez-ir-call-site-refs (chez-ir-expand define-form) consumer-path)]
      [stats (ir-auto-route:stats)])

    (test-assert "chez-ir feature probes pass" (chez-ir-available?))
    (test-assert "IR path took the call site (hit recorded)"
      (>= (car stats) 1))
    (format #t "  [ir-auto-route] stats hits=~a fallbacks=~a total-ms=~a~%"
      (car stats) (cadr stats) (caddr stats))

    ;Every ground-truth use resolves at the ground-truth binding position,
    ;with the analysis produced by the IR path inside a real workspace init.
    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
            [ref-bias (cadr entry)]
            [binding-bias (caddr entry)]
            [node (pick-index-node-from index-node-list ref-bias)])
          (test-assert (format "use site exists for ~a at bias ~a" name ref-bias)
            (and (not (null? node))
              (eq? name (index-node-expression node))))
          (let ([resolved
                  (if (null? node)
                    '()
                    (find-available-references-for document node name))])
            (test-assert (format "~a resolves to a binding" name)
              (not (null? resolved)))
            (test-assert (format "~a resolves at the ground-truth binding position" name)
              (find (lambda (r)
                      (= binding-bias (index-node-start (identifier-reference-index-node r))))
                resolved)))))
      ground-truth)

    ;Route-2-compatible attachment surface: pattern-variable bindings are also
    ;exported at their anchor nodes (what existing IDE features/tests probe).
    (let* ([pattern-a (pick-index-node-from index-node-list 152)]
        [a-exports
          (filter (lambda (r) (eq? 'a (identifier-reference-identifier r)))
            (index-node-references-export-to-other-node pattern-a))])
      (test-assert "pattern variable exported at its anchor node"
        (not (null? a-exports)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
