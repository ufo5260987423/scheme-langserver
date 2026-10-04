#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme) 
  (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver util test)

  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis identifier expanders pattern))





(test-begin "expand:index-node-compound-list")
  (let* ([workspace (init-workspace (string-append (current-directory) "/tests/resources/fixtures/pattern-source") 'txt #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [root-library-node (workspace-library-node workspace)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/tests/resources/fixtures/pattern-source/let.scm.txt"))]
      [document (file-node-document target-file-node)]
      [index-node-list (document-index-node-list document)]
      [root-index-node (car index-node-list)]
      [index-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
        root-index-node)]
      [expression (annotation-stripped (index-node-datum/annotations index-node))]
      [pattern-expression '(match atom (pat . body) ...)]
      [pattern (make-pattern pattern-expression)]
      [context (gather-context pattern)]
      [bindings (pattern+index-node->bindings pattern index-node '())]
      [template (make-pattern '(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...)))]
      [expansion (expand->index-node-compound-list template bindings context)])
  ; (pretty-print (expansion->printable-object expansion))
      (test-equal 
      '(let ([v expression])
      (match-next
        v
        (expression (set! expression))
        ((_ (fuzzy0 **1) fuzzy1 ...)
          (let ([bindings-list-node (dereference-index-node
                                      (cadr
                                        (index-node-children index-node)))])
            (let ([binding-nodes (filter
                                   (lambda (i)
                                     (not (null? (index-node-children i))))
                                   (index-node-children bindings-list-node))])
              (check-duplicate-bindings document binding-nodes)
              (fold-left
                (lambda (exclude-list identifier-parent-index-node)
                  (let* ([identifier-parent-index-node (dereference-index-node
                                                         identifier-parent-index-node)]
                         [identifier-index-node (car (index-node-children
                                                       identifier-parent-index-node))]
                         [target-identifier-reference (let-parameter-process index-node
                                                        identifier-index-node
                                                        index-node document
                                                        type)]
                         [extended-exclude-list (append
                                                  exclude-list
                                                  target-identifier-reference)])
                    (index-node-excluded-references-set!
                      bindings-list-node
                      extended-exclude-list)
                    extended-exclude-list))
                '()
                binding-nodes)))
          '())
        (else
          (let ([bindings-list-node (dereference-index-node
                                      (cadr
                                        (index-node-children index-node)))])
            (let ([binding-nodes (filter
                                   (lambda (i)
                                     (not (null? (index-node-children i))))
                                   (index-node-children bindings-list-node))])
              (check-duplicate-bindings document binding-nodes)
              (fold-left
                (lambda (exclude-list identifier-parent-index-node)
                  (let* ([identifier-parent-index-node (dereference-index-node
                                                         identifier-parent-index-node)]
                         [identifier-index-node (car (index-node-children
                                                       identifier-parent-index-node))]
                         [target-identifier-reference (let-parameter-process index-node
                                                        identifier-index-node
                                                        index-node document
                                                        type)]
                         [extended-exclude-list (append
                                                  exclude-list
                                                  target-identifier-reference)])
                    (index-node-excluded-references-set!
                      bindings-list-node
                      extended-exclude-list)
                    extended-exclude-list))
                '()
                binding-nodes)))
          '())))
      (expansion->printable-object expansion)))
(test-end)

(test-begin "pattern+index-node->bindings")
  (define (make-call-node expr)
    (init-index-node '() (car (source-file->annotations (with-output-to-string (lambda () (pretty-print expr))) "/dev/null"))))
  (define (binding->datum b)
    (let ([v (cdr b)])
      (cond
        [(index-node? v) (annotation-stripped (index-node-datum/annotations v))]
        [(list? v) (map (lambda (n) (if (index-node? n) (annotation-stripped (index-node-datum/annotations n)) n)) v)]
        [else v])))
  (define (bindings-summary pattern expr literals)
    (let ([r (pattern+index-node->bindings (make-pattern pattern) (make-call-node expr) literals)])
      (cond
        [(list? r) (map (lambda (b) (cons (car b) (binding->datum b))) r)]
        [else r])))

  ;simple, no ellipsis
  (test-equal '((body . x) (val . 1) (var . x))
    (bindings-summary '(_ ((var val)) body) '(simple-let ((x 1)) x) '()))
  ;pair tail
  (test-equal '((t 2 3) (h . 1))
    (bindings-summary '(_ (h . t)) '(m (1 2 3)) '()))
  ;literal distinguished from pattern variable
  (test-equal '((x . 1))
    (bindings-summary '(_ (lit x)) '(m (lit 1)) '(lit)))
  (test-equal #f
    (bindings-summary '(_ (lit x)) '(m (k 1)) '(lit)))
  ;single ellipsis
  (test-equal '((atom . y) (match . match) (pat a b) (body 1 2))
    (bindings-summary '(match atom (pat . body) ...) '(match y (a 1) (b 2)) '()))
  ;nested ellipsis
  (test-equal '((a 1 3) (b 2 4))
    (bindings-summary '((a b) ...) '((1 2) (3 4)) '()))
  ;multiple separate ellipses, equal length (R6RS)
  (test-equal '((y 3 4) (x 1 2))
    (bindings-summary '(x ... y ...) '(1 2 3 4) '()))
  (test-equal '((z 5 6) (y 3 4) (x 1 2))
    (bindings-summary '(x ... y ... z ...) '(1 2 3 4 5 6) '()))
  ;multiple ellipses with fixed prefix and suffix
  (test-equal '((b . 5) (y 3 4) (x 1 2) (a . 0))
    (bindings-summary '(a x ... y ... b) '(0 1 2 3 4 5) '()))
  ;multiple ellipses with improper tail: the tail is a final fixed segment
  (test-equal '((a 1 3) (b 2 4) (c . 5) (d . 6))
    (bindings-summary '((a b) ... (c) . d) '((1 2) (3 4) (5) 6) '()))
  ;non-divisible middle defers to fallback
  (test-equal 'fallback
    (bindings-summary '(x ... y ...) '(1 2 3) '()))
  ;fallback and mismatch counters
  (pattern-matcher-reset-counters!)
  (test-equal 'fallback
    (bindings-summary '(x ... y ...) '(1 2 3) '()))
  (test-equal 1 (pattern-matcher-fallback-count))
  (test-equal 0 (pattern-matcher-mismatch-count))
  (pattern-matcher-reset-counters!)
  (test-equal 0 (pattern-matcher-fallback-count))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
