#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
;; IR path via the LOAD fallback: this process does NOT import (ufo-match),
;; so the interaction probe must FAIL (the env is lenient -- an unbound match
;; expands silently, so the probe verifies REAL macro engagement via the IR)
;; and ir-auto-route locates the workspace's own ufo-match source through the
;; library-node tree, loads it, and expands in a fresh immutable env via the
;; let-wrapper + free-variable parameter growth. Verifies the v2 env strategy
;; that also makes compiled release binaries work (no host import needed) and
;; eliminates version skew.
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

;; 注意：故意不 import (ufo-match)
(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/sc-expand-differential"))
(define consumer-path (string-append fixture "/consumer.scm.txt"))

;; ground truth for the resolution checks
(define ground-truth
  (let* ([annotated-toplevel (car (chez-ir-read-annotated-file consumer-path))]
      [define-form (chez-ir-find-annotated-form
          (lambda (s) (and (pair? s) (eq? 'define (car s))))
          annotated-toplevel)])
    (chez-ir-call-site-refs (chez-ir-expand define-form) consumer-path)))

(define (private:test-log fmt . args)
  (when (getenv "TEST_VERBOSE")
    (apply format (cons #t (cons fmt args)))))

(test-begin "ir-auto-route load path: basic fixture")
  (let* ([workspace-instance (init-workspace fixture 'txt 'r6rs #f #f)]
      [document (file-node-document
                  (walk-file (workspace-file-node workspace-instance) consumer-path))]
      [index-node-list (document-index-node-list document)]
      [stats (ir-auto-route:stats)]
      [env-source (ir-auto-route:env-source)])
    (test-assert "interaction env probe fails without host import"
      (not (eq? env-source 'interaction)))
    (test-assert "env bootstrapped via load of the workspace library source"
      (and (pair? env-source) (eq? (car env-source) 'loaded)))
    (test-assert "IR path took the call site through the loaded env"
      (>= (car stats) 1))
    (private:test-log "  [load-path] env-source=~a hits=~a fallbacks=~a~%"
      env-source (car stats) (cadr stats))
    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
            [ref-bias (cadr entry)]
            [binding-bias (caddr entry)]
            [node (pick-index-node-from index-node-list ref-bias)])
          (test-assert (format "~a resolves through the load-path env" name)
            (and (not (null? node))
              (find (lambda (r)
                      (= binding-bias (index-node-start (identifier-reference-index-node r))))
                (find-available-references-for document node name))))))
      ground-truth))
(test-end)
(newline)

;; free-variable case: classify calls its sibling helper. The strict fresh
;; env rejects unbound identifiers; the wrapper grows one parameter per error.
(define freevar-dir "/tmp/sc-expand-load-freevar-fixture")
(define freevar-case-path (string-append freevar-dir "/case.scm.txt"))

(system "mkdir -p /tmp/sc-expand-load-freevar-fixture")
(system (string-append "cp " fixture "/ufo-match.scm.txt " freevar-dir "/"))
(call-with-output-file freevar-case-path
  (lambda (port)
    (display
      (string-append
        "(library (load-freevar-case)\n"
        "  (export classify)\n"
        "  (import (chezscheme) (ufo-match))\n"
        "  (define (helper x) (* x 2))\n"
        "  (define (classify input)\n"
        "    (match (helper input)\n"
        "      [(a b) (+ a b)]\n"
        "      [other 0])))\n")
      port))
  'replace)

(test-begin "ir-auto-route load path: free-variable retry")
  (let* ([ws2 (init-workspace freevar-dir 'txt 'r6rs #f #f)]
      [doc2 (file-node-document
              (walk-file (workspace-file-node ws2) freevar-case-path))]
      [text2 (document-text doc2)])
    (define (last-index-of ch)
      (let loop ([i 0] [last #f])
        (cond
          [(>= i (string-length text2)) last]
          [(char=? (string-ref text2 i) ch) (loop (+ i 1) i)]
          [else (loop (+ i 1) last)])))
    (define (pattern-a-bias)
      ;; the `a` inside the [(a b) ...] clause: "[(a" followed by space
      (let loop ([i 0])
        (cond
          [(>= i (- (string-length text2) 4)) #f]
          [(and (char=? (string-ref text2 i) #\[)
              (char=? (string-ref text2 (+ i 1)) #\()
              (char=? (string-ref text2 (+ i 3)) #\space))
            (+ i 2)]
          [else (loop (+ i 1))])))
    (let* ([body-a-bias (+ (last-index-of #\+) 2)]
        [node (pick-index-node-from (document-index-node-list doc2) body-a-bias)]
        [resolved (find-available-references-for doc2 node 'a)])
      (test-assert "free-var define expands via parameter-growth retry"
        (>= (car (ir-auto-route:stats)) 2))
      (test-assert "free-var body-a resolves to exactly one anchor at the pattern"
        (and (= 1 (length resolved))
          (= (pattern-a-bias)
            (index-node-start (identifier-reference-index-node (car resolved))))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
