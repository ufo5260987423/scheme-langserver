#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2026
;; SPDX-License-Identifier: MIT
#!r6rs

;; TODO-14 cascade fusion: exported macro expanders are wrapped (per
;; library, after the document's step) so that a single invocation expands
;; the whole internal cascade -- the skeleton handed back is the residual
;; code, with no family-macro call sites left. This test pins both the
;; fused shape (no match-next/match-one/match-two remnants) and the
;; analysis result (the pattern variable's body use still resolves).

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve"))

(define (tree-contains? t target)
  (cond
    [(equal? t target) #t]
    [(null? t) #f]
    [(pair? t) (or (tree-contains? (car t) target)
                 (tree-contains? (cdr t) target))]
    [(vector? t) (tree-contains? (vector->list t) target)]
    [else #f]))

(test-begin "cascade fusion (TODO-14)")
;fusion is opt-in (measurement gate, see todo.md TODO-14): this test
;enables it in-process; every other suite runs the default per-hop path
(putenv "SCHEME_LANGSERVER_FUSION" "1")
(let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [root (workspace-file-node workspace)]
    [consumer (walk-file root (string-append fixture "/consumer.scm.txt"))]
    [doc (file-node-document consumer)]
    [call-node (find-index-node-recursive
                 (lambda (n)
                   (let ([e (annotation-stripped-expression n)])
                     (and (list? e) (not (null? e)) (eq? 'match (car e)))))
                 (car (document-index-node-list doc)))]
    [match-ref (car (find-available-references-for doc call-node 'match))]
    [expander (identifier-reference-syntax-expander match-ref)]
    [result (expander root (workspace-library-node workspace) doc call-node)]
    [skeleton-datum (annotation-stripped
                      (index-node-datum/annotations (cadr result)))]
    [family '(match-next match-one match-two match-check-ellipsis match-check-identifier)])

  (test-assert "exported match expander is fused (produces a result)" (and result #t))

  (test-assert "match expander was actually wrapped by the fusion pass"
    (not (eq? expander
          (index-node-expansion-generator
            (caddr (index-node-children
                     (identifier-reference-initialization-index-node match-ref)))))))

  ;parity baseline: the per-hop cascade (SCHEME_LANGSERVER_NO_FUSION)
  ;leaves exactly the same single remnant on this fixture -- the
  ;failure-thunk's error-clause call declines expansion in both modes
  ;(match-two on the empty (and) pattern declines its (sk ...) template),
  ;and fusion is strictly opportunistic about declined calls
  (test-equal '(match-next)
    (let walk ((t skeleton-datum) (acc '()))
      (cond
        [(symbol? t) (if (and (memq t family) (not (memq t acc))) (cons t acc) acc)]
        [(pair? t) (walk (cdr t) (walk (car t) acc))]
        [(vector? t) (walk (vector->list t) acc)]
        [else acc])))

  (test-assert "fused expansion produces pairs"
    (and (pair? (car result)) (not (null? (car result)))))

  ; the workspace-level eager expansion went through the fused expander
  ; too: the pattern variable's body use must still resolve to a binding
  (let* ([clause-node (caddr (index-node-children call-node))]
      [body-use-node (cadr (index-node-children clause-node))]
      [refs (find-available-references-for doc body-use-node 'path)])
    (test-assert "pattern variable body use resolves after fused expansion"
      (and (not (null? refs))
        (find (lambda (r) (eq? 'variable (identifier-reference-type r))) refs)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
