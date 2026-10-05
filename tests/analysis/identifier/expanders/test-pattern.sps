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
  (scheme-langserver analysis identifier expanders pattern)
  (scheme-langserver analysis identifier expanders dispatch-matcher))





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
      [expression (index-node-expression index-node)]
      [pattern-expression '(match atom (pat . body) ...)]
      [literals '()]
      [context (gather-context pattern-expression literals)]
      [bindings
        (let-values ([(dsl pvars) (convert-pattern pattern-expression literals)])
          (dsl+index-node->bindings dsl pvars index-node literals))]
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
                  binding-nodes))))
          (else '())))
      (expansion->printable-object expansion)))
(test-end)

(test-begin "dsl+index-node->bindings (Chez $syntax-dispatch transcription)")
  ;NB: the 2-arity source-file->annotations (string source) parses via a
  ;path-reopened source descriptor and produces structurally broken
  ;index-node trees with a dev-null/odd path; write a real temp file
  (define (make-call-node expr)
    (with-output-to-file "/tmp/t-pattern-tmp.scm" (lambda () (pretty-print expr)) 'replace)
    (init-index-node '() (car (source-file->annotations "/tmp/t-pattern-tmp.scm"))))
  (define (binding->datum b)
    (define (strip v)
      (cond
        [(index-node? v) (annotation-stripped (index-node-datum/annotations v))]
        [(list? v) (map strip v)]
        [else v]))
    (strip (cdr b)))
  (define (bindings-summary pattern expr literals)
    (let-values ([(dsl pvars) (convert-pattern pattern literals)])
      (let ([r (dsl+index-node->bindings dsl pvars (make-call-node expr) literals)])
        (cond
          [(list? r) (map (lambda (b) (cons (car b) (binding->datum b))) r)]
          [else r]))))

  ;simple, no ellipsis (order = convert-pattern pvars order)
  (test-equal '((var . x) (val . 1) (body . x))
    (bindings-summary '(_ ((var val)) body) '(simple-let ((x 1)) x) '()))
  ;pair tail
  (test-equal '((h . 1) (t 2 3))
    (bindings-summary '(_ (h . t)) '(m (1 2 3)) '()))
  ;literal distinguished from pattern variable
  (test-equal '((x . 1))
    (bindings-summary '(_ (lit x)) '(m (lit 1)) '(lit)))
  (test-equal #f
    (bindings-summary '(_ (lit x)) '(m (k 1)) '(lit)))
  ;single ellipsis; (pat . body) binds body to each clause's cdr; the
  ;binding ORDER follows convert-pattern's pvars (head symbol included
  ;-- literals is empty, so 'match is a pattern variable)
  (test-equal '((match . match) (atom . y) (pat a b) (body (1) (2)))
    (bindings-summary '(match atom (pat . body) ...) '(match y (a 1) (b 2)) '()))
  ;nested ellipsis
  (test-equal '((a 1 3) (b 2 4))
    (bindings-summary '((a b) ...) '((1 2) (3 4)) '()))
  ;one ellipsized head plus an improper tail -- the only multi-element
  ;each+ shape Chez's syntax-case validator accepts ((x ... . t) family;
  ;verified against Chez: x gets the whole list, t the null tail).
  ;Patterns with multiple ellipses at the same list level (e.g.
  ;(x ... y ...)) are rejected by the validator ("misplaced ellipsis")
  ;before $syntax-dispatch ever runs, so they are unreachable through
  ;real macros; the old matcher had invented semantics for them.
  (test-equal '((x 1 2 3) (t))
    (bindings-summary '(x ... . t) '(1 2 3) '()))
  (test-equal '((x 1 2 3) (t))
    (bindings-summary '(_ (x ... . t)) '(m (1 2 3)) '()))
  ;vector patterns
  (test-equal '((a . 1) (b . 2))
    (bindings-summary '#(a b) '#(1 2) '()))
  (test-equal '((a 1 2 3))
    (bindings-summary '#(a ...) '#(1 2 3) '()))
  (test-equal '((a 1 3) (b 2 4))
    (bindings-summary '#((a b) ...) '#((1 2) (3 4)) '()))
  (test-equal '((a . 1) (b . 2))
    (bindings-summary '(_ #(a b)) '(m #(1 2)) '()))
  (test-equal 'vector-form (pattern-type (make-pattern '#(a b))))
  (test-equal 'ellipse-vector-form (pattern-type (make-pattern '#(a ...))))
  (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
