#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; IR auto-route GENERIC arm (2026-10-29): any define-syntax-headed call site
;; whose head resolved to a library -- here a user-defined syntax-rules macro
;; (simple-let) imported from another library -- goes through Chez's own
;; expander IR, and the analysis must satisfy the IR ground truth. The
;; retired ufo-match/match-index-node family arms were subsumed by this arm.
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier expander chez-ir)
  (scheme-langserver analysis identifier expander ir-auto-route)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

(test-begin "ir-auto-route generic arm: user-defined syntax-rules macro")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-macro-auto-resolve")]
      [consumer-path (string-append fixture "/consumer.scm.txt")]
      [workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace-instance)]
      [document (file-node-document (walk-file root-file-node consumer-path))]
      [index-node-list (document-index-node-list document)]
      ;ground truth, independent of the analysis under test. The env is
      ;built the way ir-auto-route bootstraps it (load macro source +
      ;fresh chezscheme+macro env); the define is let-wrapped because fresh
      ;envs are immutable (same wrap private:expand-fresh uses).
      [annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (s) (and (pair? s) (eq? 'define (car s))))
          annotated-toplevel)]
      ;ground truth, independent of the analysis under test. The env is
      ;built the way ir-auto-route bootstraps it (load macro source +
      ;fresh chezscheme+macro env); the define is let-wrapped because fresh
      ;envs are immutable (same wrap private:expand-fresh uses).. The env is
      ;built the same way ir-auto-route bootstraps it: load the macro
      ;library source, then a fresh (chezscheme + macro-library) env.
      [macro-path (string-append fixture "/macro.scm.txt")]
      [_ (load macro-path)]
      [ground-truth-env (environment '(chezscheme) '(simple-macro))]
      [ground-truth (chez-ir-call-site-refs (chez-ir-expand (list (quote let) (quote ()) define-form #f) ground-truth-env) consumer-path)]
      [stats (ir-auto-route:stats)])

    (test-assert "chez-ir feature probes pass" (chez-ir-available?))
    (test-assert "the call site was NOT handled by a family-specific arm: the head's library is (simple-macro)"
      (find (lambda (entry) (eq? 'x (car entry))) ground-truth))
    (test-assert "generic arm took the call site (hit recorded)"
      (>= (car stats) 1))

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

    ;The pattern variable is also exported at its anchor node (the surface
    ;existing IDE features and tests probe).
    (let* ([binding-entry (find (lambda (e) (eq? 'x (car e))) ground-truth)]
        [anchor (pick-index-node-from index-node-list (caddr binding-entry))]
        [x-exports
          (filter (lambda (r) (eq? 'x (identifier-reference-identifier r)))
            (index-node-references-export-to-other-node anchor))])
      (test-assert "pattern variable exported at its anchor node"
        (not (null? x-exports)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
