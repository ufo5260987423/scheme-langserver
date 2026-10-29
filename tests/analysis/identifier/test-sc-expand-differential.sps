#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Differential test: Chez sc-expand IR ground truth vs route-2 resolution.
;; See doc/analysis/chez-sc-expand-records-switch.md.
#!r6rs

(import
  (chezscheme)
  (ufo-match)
  (srfi :64 testing)
  (scheme-langserver util test)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier expander chez-ir)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier expander ir-auto-route))

;; automatic macro resolution is opt-in since 2026-10-29 (default off);
;; this test exercises the mechanism, so turn it on.
(ir-auto-route:enabled? #t)

(test-begin "sc-expand differential: ground truth vs analysis resolution")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/sc-expand-differential")]
      [consumer-path (string-append fixture "/consumer.scm.txt")]
      [annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (stripped) (and (pair? stripped) (eq? 'define (car stripped))))
          annotated-toplevel)]
      [ir (chez-ir-expand define-form)]
      [ground-truth (chez-ir-call-site-refs ir consumer-path)]
      [workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace-instance)]
      [target-file-node (walk-file root-file-node consumer-path)]
      [document (file-node-document target-file-node)]
      [index-node-list (document-index-node-list document)])

    ;; Pattern-variable anchoring (fixed 2026-10-20, expansion-wrap.sls): Chez's
    ;; own expander attributes pattern-variable bindings to the pattern positions
    ;; (e.g. a -> bias 152). Route 2 now matches: shallow-copy's import fallback
    ;; recovers the true anchor by scanning all hops' pair compounds for a
    ;; call-site-tree leaf with the binding identifier (the pvar link exists only
    ;; at leaf pairs, while bindings attach to interior scope nodes). Scope
    ;; visibility and definition position are separated: the reference resolves
    ;; from the macro call node (ancestor of all uses) while its index-node is the
    ;; recovered pattern position. Investigation trail: protocol-wrapped
    ;; constructor trap (mkref) located the creators; SLSCOPY_TRACE showed the
    ;; pair-less interior entries. Known remaining gap: duplicate speculative
    ;; references (resolution count 2 vs Chez's 1) -- TODO-15 P1/P2 multiplicity.
    (test-assert "chez-ir feature probes pass" (chez-ir-available?))
    (test-assert "define form found in annotated source" (not (eq? #f define-form)))
    (test-assert "ground truth has call-site refs"
      (not (null? ground-truth)))

    ;; Ground truth for this fixture (verified by inspection):
    ;;   (input <scrutinee-pos> <param-pos>)  - scrutinee ref -> lambda binding
    ;;   (a     <body-pos>      #f)           - body ref, macro-introduced binding
    ;;   (b     <body-pos>      #f)
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
            (when binding-bias
              (test-assert (format "~a resolves at the ground-truth binding position" name)
                (find (lambda (r)
                        (= binding-bias (index-node-start (identifier-reference-index-node r))))
                  resolved))))))
      ground-truth)

    ;; Report the actual binding anchors of macro-introduced bindings
    ;; (ground truth binding-bias = #f) as plain output for inspection.
    (for-each
      (lambda (entry)
        (when (not (caddr entry))
          (let* ([name (car entry)]
              [node (pick-index-node-from index-node-list (cadr entry))]
              [resolved (if (null? node) '() (find-available-references-for document node name))])
            (for-each
              (lambda (r)
                (format #t "  [info] ~a @~a anchored at bias ~a~%"
                  name (cadr entry)
                  (index-node-start (identifier-reference-index-node r))))
              resolved))))
      ground-truth))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
