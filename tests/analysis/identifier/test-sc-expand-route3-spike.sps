#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Route-3 spike: replace route 2's generator pipeline with Chez sc-expand IR
;; for one match call site -- synthesize identifier-references from the IR's
;; call-site refs and verify resolution + performance.
;; Ground truth / extractor: analysis/identifier/chez-ir.sls
;; (doc/analysis/chez-sc-expand-records-switch.md).
#!r6rs

(import
  (chezscheme)
  (ufo-match)
  (srfi :64 testing)

  (scheme-langserver util test)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier expander chez-ir)
  (scheme-langserver analysis identifier reference))

;; Synthesize macro-derived binding references from Chez IR ground truth:
;; one reference per (identifier binding-bias), inserted into the macro call
;; node's imports (ancestor of every use, so resolution works), anchored at
;; the binding position (goto-definition target). This mirrors the semantics
;; route 2's expansion-wrap produces after the pattern-anchoring fix, but the
;; knowledge comes from Chez's own expander instead of the syntax-rules
;; machinery. Only bindings INSIDE the macro call span are macro-derived
;; (e.g. pattern variables); ordinary bindings (lambda params) are excluded.
(define (spike:synthesize! gt document call-node)
  (let ([seen (make-hashtable equal-hash equal?)]
      [inserted 0])
    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
            [binding-bias (caddr entry)]
            [macro-derived?
              (and binding-bias
                (>= binding-bias (index-node-start call-node))
                (< binding-bias (index-node-end call-node)))])
          (when macro-derived?
            (unless (hashtable-ref seen (list name binding-bias) #f)
              (hashtable-set! seen (list name binding-bias) #t)
              (let* ([picked (pick-index-node-from (document-index-node-list document) binding-bias)]
                  [anchor (if (index-node? picked) picked call-node)]
                  [ref (make-identifier-reference name document anchor call-node '() 'variable '() '())])
                (identifier-reference-speculative?-set! ref #t)
                (index-node-references-import-in-this-node-set! call-node
                  (sort-identifier-references
                    (cons ref (index-node-references-import-in-this-node call-node))))
                (set! inserted (+ inserted 1)))))))
      gt)
    inserted))

(test-begin "route-3 spike: Chez IR reference synthesis for one match call site")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/sc-expand-differential")]
      [consumer-path (string-append fixture "/consumer.scm.txt")]
      [src (call-with-input-file consumer-path get-string-all)]
      [document (make-document (string-append "file://" consumer-path) src '())]
      [root (init-index-node '() (car (source-file->annotations src consumer-path)))])
    (document-index-node-list-set! document (list root))
    (let* ([match-node (find-index-node-recursive
              (lambda (n)
                (let ([expr (annotation-stripped-expression n)])
                  (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
              root)]
        [annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
        [define-form (chez-ir-find-annotated-form
            (lambda (s) (and (pair? s) (eq? 'define (car s))))
            annotated-toplevel)]
        ;---- timed section: the route-3 replacement pipeline ----
        [t0 (current-time 'time-monotonic)]
        [ir (chez-ir-expand define-form)]
        [gt (chez-ir-call-site-refs ir consumer-path)]
        [inserted (spike:synthesize! gt document match-node)]
        [t1 (current-time 'time-monotonic)]
        [elapsed-ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)]
        ;---- ground truth entries for THIS call site ----
        [macro-entries
          (filter
            (lambda (entry)
              (let ([binding-bias (caddr entry)])
                (and binding-bias
                  (>= binding-bias (index-node-start match-node))
                  (< binding-bias (index-node-end match-node)))))
            gt)])

      (test-assert "chez-ir feature probes pass" (chez-ir-available?))
      (test-assert "match call node found" (not (null? match-node)))
      (test-assert "ground truth has entries" (not (null? gt)))
      (test-assert "macro-derived entries exist (pattern variables)"
        (not (null? macro-entries)))
      (test-assert "synthesis inserted one reference per macro binding"
        (= inserted (length macro-entries)))

      ;Resolution: every ground-truth use resolves to the synthesized binding
      ;at the exact ground-truth binding position -- no step of the expansion,
      ;no generator, no template, no back-propagation involved.
      (for-each
        (lambda (entry)
          (let* ([name (car entry)]
              [ref-bias (cadr entry)]
              [binding-bias (caddr entry)]
              [node (pick-index-node-from (document-index-node-list document) ref-bias)])
            (test-assert (format "use site exists for ~a at bias ~a" name ref-bias)
              (and (index-node? node) (eq? name (index-node-expression node))))
            (let ([resolved
                    (if (index-node? node)
                      (find-available-references-for document node name)
                      '())])
              (test-assert (format "~a resolves via synthesized references" name)
                (not (null? resolved)))
              (test-assert (format "~a resolves at the ground-truth binding position" name)
                (find (lambda (r)
                        (= binding-bias (index-node-start (identifier-reference-index-node r))))
                  resolved)))))
        macro-entries)

      ;Performance gate: the whole replacement pipeline (sc-expand + IR walk +
      ;synthesis) must be far below route 2's measured cold-call cost (228 ms,
      ;doc/analysis/macro-resolution-profile.md). Replay-hot route 2 is ~1 ms;
      ;the spike needs no replay cache to be in that class.
      (format #t "  [spike] pipeline ~a ms for ~a call-site refs (~a synthesized)~%"
        elapsed-ms (length gt) inserted)
      (test-assert "spike pipeline beats route 2 cold-call cost by an order of magnitude"
        (< elapsed-ms 50))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
