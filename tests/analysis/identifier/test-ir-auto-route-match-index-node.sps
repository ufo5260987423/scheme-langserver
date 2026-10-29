#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; IR path for match-index-node -- the generalization test. The mechanism is
;; macro-agnostic since 2026-10-21: the router arm matches the head's actual
;; library ((scheme-langserver virtual-file-system index-node)), ir-auto-route
;; derives head symbol + library from the call site, bootstraps the env from
;; the workspace's own copy of THAT library, and synthesizes the same way as
;; for the ufo-match family. Target: syntax-rules-process in rules/syntax-rules.sls
;; (a real match-index-node user in this repository).
#!r6rs

(import
  (chezscheme)
  (ufo-match-steer) ; GT expansion: aux keywords (? :_ **1) resolve as aux
  (scheme-langserver virtual-file-system index-node) ; GT expansion needs match-index-node
  (srfi :64 testing)

  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier expander chez-ir)
  (scheme-langserver analysis identifier expander ir-auto-route)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

(define (private:test-log fmt . args)
  (when (getenv "TEST_VERBOSE")
    (apply format (cons #t (cons fmt args)))))

(test-begin "ir-auto-route: match-index-node through the generalized IR path")
  (let* ([root (current-directory)]
      [target-path (string-append root "/analysis/identifier/rules/syntax-rules.sls")]
      [annotated-toplevel (car (chez-ir-read-annotated-file target-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (s) (and (pair? s) (eq? 'define (car s))))
          annotated-toplevel)]
      [ground-truth (chez-ir-call-site-refs (chez-ir-expand define-form) target-path)]
      [workspace-instance (init-workspace root 'akku 'r6rs #f #f)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) target-path))]
      [index-node-list (document-index-node-list document)]
      [stats (ir-auto-route:stats)]
      [env-source (ir-auto-route:env-source)])

    (test-assert "ground truth has entries" (not (null? ground-truth)))
    (test-assert "match-index-node call site took the IR path"
      (>= (car stats) 1))
    (private:test-log "  [steer] hits=~a fallbacks=~a env-source=~a gt=~a~%"
      (car stats) (cadr stats) env-source (length ground-truth))

    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
            [ref-bias (cadr entry)]
            [binding-bias (caddr entry)]
            [node (pick-index-node-from index-node-list ref-bias)])
          (test-assert (format "~a resolves (gt ref ~a -> binding ~a)" name ref-bias binding-bias)
            (and (not (null? node))
              (find (lambda (r)
                      (= binding-bias (index-node-start (identifier-reference-index-node r))))
                (find-available-references-for document node name))))))
      ground-truth))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
