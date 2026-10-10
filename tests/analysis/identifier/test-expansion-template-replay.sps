#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

;; Regression: list-shaped template instantiations (the common case, e.g. the
;; match family) give the expansion root no entry in pairs, so the template
;; store in expansion-wrap.sls was gated on root-pair and almost never stored
;; anything -- the cross-call-site replay path never engaged and identical
;; call-site datums re-expanded at full price. Storage is now unconditional:
;; with a root pair the compound counterpart anchors path encoding as before;
;; without one the call node anchors it and every path degrades to #f (replay
;; then attaches references at the call node, which is sound).

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(test-begin "expansion-template store/replay for list-root expansions")

(let* ([workspace (init-workspace (current-directory))]
    [root-file-node (workspace-file-node workspace)]
    [root-library-node (workspace-library-node workspace)]
    [file-linkage (workspace-file-linkage workspace)]
    [ufo-match-doc (file-node-document
        (walk-file root-file-node
          (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls")))]
    [match-gen
      (let loop ([nodes (document-index-node-list ufo-match-doc)])
        (if (null? nodes)
          #f
          (let ([r (find-index-node-recursive
                     (lambda (n)
                       (let ([e (annotation-stripped-expression n)])
                         (and (list? e) (not (null? e)) (eq? 'define-syntax (car e))
                              (list? (cdr e)) (not (null? (cdr e))) (eq? 'match (cadr e)))))
                     (car nodes))])
            (if r
              (index-node-expansion-generator (car (reverse (index-node-children r))))
              (loop (cdr nodes))))))]
    [make-call-node
      (lambda (expr)
        (init-index-node '()
          (car (source-file->annotations
                (with-output-to-string (lambda () (pretty-print expr)))
                "/dev/null"))))]
    [datum '(match x ((? string? p) p) (_ 'no))]
    [rule (expansion-generator->rule match-gen step file-linkage '() '())]
    [call-node1 (make-call-node datum)])

  ;;first call: full run; a list-root expansion has no root pair, but the
  ;;template must be stored anyway (anchored at the call node, all paths #f)
  (test-assert "rule runs on first call site"
    (guard (e [else #f])
      (rule root-file-node root-library-node ufo-match-doc call-node1)
      #t))

  (let ([specs (expansion-template-lookup match-gen datum)])
    (test-assert "template stored for list-root expansion" (and specs (not (null? specs))))
    (test-assert "degraded template carries only #f paths"
      (and specs
        (not (null? specs))
        (andmap
          (lambda (spec) (eq? (list-ref spec 4) #f))
          specs))))

  ;;second call with an IDENTICAL datum on a fresh call node: engages the
  ;;replay path; references must land on the new call node (no loss)
  (let ([call-node2 (make-call-node datum)])
    (test-assert "rule runs on identical second call site"
      (guard (e [else #f])
        (rule root-file-node root-library-node ufo-match-doc call-node2)
        #t))
    (test-assert "references back-propagated to second call node"
      (or (not (null? (index-node-references-import-in-this-node call-node2)))
        (not (null? (index-node-references-export-to-other-node call-node2)))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
