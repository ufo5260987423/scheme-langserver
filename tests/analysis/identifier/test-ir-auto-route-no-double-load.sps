#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Regression test for the double-load compilation-instance clash
;; (2026-10-29): the old bootstrap always (load source)-ed the macro
;; library. When that library was ALREADY imported by the analyzing
;; process -- (scheme-langserver virtual-file-system index-node) is
;; imported by every identifier rule, so any match-index-node call site
;; triggered it -- the second load re-registered a conflicting
;; compilation instance and every subsequent cross-instance call raised
;; "compiled program requires a different compilation instance". The
;; exception was swallowed by the try guard, the call site landed in the
;; negative cache, and NO expansion-derived references were ever
;; synthesized (user-visible: goto-definition dead on pattern variables
;; like `name-node` in analysis/identifier/rules/define.sls).
;;
;; The test inits the real project workspace and checks two files that
;; import (scheme-langserver virtual-file-system index-node) with different
;; import sets (define.sls, define-record-type.sls) -- the test process
;; itself imports the index-node library, standing in for the server's
;; instance. Old code: the bootstrap's (load source) re-registered a
;; conflicting compilation instance, after which every cross-instance call
;; raised; the try guard swallowed it, the call site was negatively cached,
;; and the pattern-variable references vanished. New code: build-env runs
;; first and reuses the registered library, so no load happens.
#!r6rs

(import
  (chezscheme)
  (ufo-match-steer) ; aux keywords (? :_ **1) resolve as aux in probes/expansions
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system index-node)

  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expander chez-ir)
  (scheme-langserver analysis identifier expander ir-auto-route)
  (scheme-langserver analysis workspace))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

(define target-a
  (string-append (current-directory) "/analysis/identifier/rules/define.sls"))
(define target-b
  (string-append (current-directory) "/analysis/identifier/rules/define-record-type.sls"))

;; resolve the FIRST use of `name-node` in the file to its pattern binding
;; (the exact goto-definition path the user reported).
(define (resolve-name-node workspace-instance path)
  (let* ([document (file-node-document
                    (walk-file (workspace-file-node workspace-instance) path))]
         [text (document-text document)]
         [symbol "name-node"]
         [occurrences
           (let loop ([i 0] [acc '()])
             (cond
               [(> i (- (string-length text) (string-length symbol))) (reverse acc)]
               [(string=? (substring text i (+ i (string-length symbol))) symbol)
                (loop (+ i 1) (cons i acc))]
               [else (loop (+ i 1) acc)]))]
         [binding-bias (car occurrences)]
         [use-bias (cadr occurrences)]
         [use-node (pick-index-node-from (document-index-node-list document) use-bias)]
         [refs (if (index-node? use-node)
                 (find-available-references-for document use-node 'name-node)
                 '())])
    (list binding-bias refs)))

(test-begin "ir-auto-route: no double-load compilation-instance clash")
  (let* ([workspace-instance (init-workspace (current-directory) 'akku 'r6rs #f #f)]
      [hits (car (ir-auto-route:stats))])
    (test-assert "chez-ir feature probes pass" (chez-ir-available?))
    (test-assert "macro call sites were attempted" (>= hits 1))
    (for-each
      (lambda (spec)
        (let* ([label (car spec)]
            [path (cdr spec)]
            [r (resolve-name-node workspace-instance path)]
            [binding-bias (car r)]
            [refs (cadr r)])
          (test-assert (format "~a pattern variable resolves" label)
            (not (null? refs)))
          (test-assert (format "~a resolves at its pattern binding position" label)
            (find (lambda (ref)
                    (= binding-bias
                      (index-node-start (identifier-reference-index-node ref))))
              refs))))
      `(("define.sls . " . ,target-a)
        ("define-record-type.sls" . ,target-b))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
