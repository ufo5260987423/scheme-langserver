#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; Multi-threaded soak for the route-3 IR path (ir-auto-route): repeated
;; THREADED workspace inits with the gate on. Exercises the one genuinely
;; untested interaction -- concurrent sc-expand from step worker threads
;; (caches are mutex-guarded; the expander itself is Chez-internal).
;; Gate for flipping the ir-auto-route default (single-thread dominance was
;; proven by bin/ir-route-canary.ss).
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
  (scheme-langserver analysis identifier expander ir-auto-route)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(putenv "SLSCHEME_IR_ROUTE" "1")

(define iterations 6)

(define fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/sc-expand-differential"))
(define consumer-path (string-append fixture "/consumer.scm.txt"))
;;ground truth for the resolution spot check (top level: sequential, no
;;binding-order surprises inside the test macro body)
(define ground-truth
  (let* ([annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (s) (and (pair? s) (eq? 'define (car s))))
          annotated-toplevel)])
    (chez-ir-call-site-refs (chez-ir-expand define-form) consumer-path)))

(test-begin "ir-auto-route threaded soak")
  (let ([fixture fixture]
      [consumer-path consumer-path]
      [ground-truth ground-truth])

    ;each iteration: fresh THREADED workspace init (threaded-map workers run
    ;the identifier analysis, invoking the wrapped rules concurrently)
    (do ([i 0 (+ i 1)]) ((= i iterations))
      (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #t #f)]
          [root-file-node (workspace-file-node workspace-instance)]
          [document (file-node-document (walk-file root-file-node consumer-path))]
          [index-node-list (document-index-node-list document)])
        (for-each
          (lambda (entry)
            (let* ([name (car entry)]
                [ref-bias (cadr entry)]
                [binding-bias (caddr entry)]
                [node (pick-index-node-from index-node-list ref-bias)])
              (test-assert (format "iter ~a: ~a resolves" i name)
                (and (not (null? node))
                  (find (lambda (r)
                          (= binding-bias (index-node-start (identifier-reference-index-node r))))
                    (find-available-references-for document node name))))))
          ground-truth)))

    (let ([stats (ir-auto-route:stats)])
      (format #t "  [soak] ~a threaded iterations, ir-stats hits=~a fallbacks=~a ms=~a~%"
        iterations (car stats) (cadr stats) (caddr stats))
      (test-assert "every iteration took the IR path exactly once for the call site"
        (>= (car stats) iterations))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
