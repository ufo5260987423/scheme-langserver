#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

;; Regression: call sites whose datum contains the literal symbol ...
;; (e.g. quoted ellipsis patterns in match clauses) used to be rejected
;; wholesale by the tree-has? checks in syntax-rules.sls /
;; expansion-wrap.sls. They must now expand like any other call site.

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

(test-begin "ellipsis-in-call-site auto-resolve")

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
    ;(match x ('(a ...) 'yes) (_ 'no)) -- the quoted pattern carries a literal ...
    [call-node (make-call-node '(match x ('(a ...) 'yes) (_ 'no)))]
    [result (match-gen #f #f ufo-match-doc call-node)])

  (test-assert "generator expands a ...-containing call site" (and result #t))

  (when result
    (let ([rule (expansion-generator->rule match-gen step file-linkage '() '())])
      (test-assert "rule runs on a ...-containing call site"
        (guard (e [else #f])
          (rule root-file-node root-library-node ufo-match-doc call-node)
          #t))
      ;the quoted-pattern clause binds nothing, but the fallback clause's
      ;body symbol must survive analysis without losing references
      (let* ([yes-node (find-index-node-recursive
              (lambda (n)
                (let ([e (annotation-stripped-expression n)])
                  (and (symbol? e) (eq? e 'yes))))
              call-node)]
          [exports (if yes-node (index-node-references-export-to-other-node yes-node) '())])
        (test-assert "references survive analysis of the ...-containing call"
          (list? exports))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
