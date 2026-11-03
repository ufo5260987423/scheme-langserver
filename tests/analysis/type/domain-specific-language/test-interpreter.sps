#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  ; (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver virtual-file-system index-node)

  (scheme-langserver util contain)

  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis type domain-specific-language interpreter))

(test-begin "type:interpret")
  (test-equal
    (type:interpret-result-list (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number?)))
    (list (construct-type-expression-with-meta 'number?)))
  (test-equal #t
    (contain?
      (let ([v (make-virtual-index-node)])
        (type:interpret-result-list `((,v <- (inner:list? ,v)) ,(construct-type-expression-with-meta 'number?))))
      (construct-type-expression-with-meta 'number?)))
  ;meta-level arrows (rnrs signatures: no real parameter index-nodes
  ;inside) are arity-checked since 2026-11-02 -- a non-matchable call
  ;used to return the return type unconditionally, so (* "a" "b")
  ;against (number? <- (inner:list? number? ...)) still came back
  ;number?. The check is structural (candy shapes, not per-position
  ;types).
  (test-assert
    (null? (type:interpret-result-list
      (construct-type-expression-with-meta '((number? <- (inner:list? number? number?)) number? number? number?)))))
  ;zero-argument call of a fully variadic builtin is unmatched
  ;(candy:matchable?'s single-elastic-segment case) and gets no type
  (test-assert
    (null? (type:interpret-result-list
      (construct-type-expression-with-meta '((number? <- (inner:list? number? ...)))))))
(test-end)

(test-begin "type:->?/<-?/=? ")
  (test-equal #t (type:->? (construct-type-expression-with-meta 'integer?) (construct-type-expression-with-meta 'number?) (make-type:environment '())))
  (test-equal #t (type:->? (construct-type-expression-with-meta 'integer?) (construct-type-expression-with-meta 'real?) (make-type:environment '())))
  (test-equal #f (type:->? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
  (test-equal #t (type:<-? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
  (test-equal #f (type:=? (construct-type-expression-with-meta 'number?) (construct-type-expression-with-meta 'integer?) (make-type:environment '())))
  (test-equal #f (type:->?
    (construct-type-expression-with-meta '(inner:list? number? number?))
    (construct-type-expression-with-meta '(inner:pair? number? (inner:list? number?)))
    (make-type:environment '())))
  ;regression (2026-11-02): inner:?->pair used to rewrite arrows too,
  ;dropping the return position -- any structurally matchable arrow
  ;pair with parameter subtyping was judged subtype regardless of
  ;returns. Arrows are now exempt from the rewrite, so the return
  ;position is compared like any other segment.
  (test-equal #f (type:->?
    (construct-type-expression-with-meta '(string? <- (inner:list? integer? ...)))
    (construct-type-expression-with-meta '(number? <- (inner:list? number? ...)))
    (make-type:environment '())))
  (test-equal #t (type:->?
    (construct-type-expression-with-meta '(number? <- (inner:list? integer? ...)))
    (construct-type-expression-with-meta '(number? <- (inner:list? number? ...)))
    (make-type:environment '())))
(test-end)

; (test-begin "debug")
;   (let* ([workspace (init-workspace (string-append (current-directory) "/.akku/lib/industria/crypto/") '() #f #f #f)]
;       [root-file-node (workspace-file-node workspace)]
;       [root-library-node (workspace-library-node workspace)]
;       [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/industria/crypto/math.sls"))]
;       [target-document (file-node-document target-file-node)]
;       [target-text (document-text target-document)]
;       [target-index-node (pick-index-node-from (document-index-node-list target-document) (text+position->int target-text 57 12))]
;       [variable (index-node-variable target-index-node)]
;       )
;     (construct-substitution-list-for target-document)
;     (let ([tmp (type:recursive-interpret-result-list variable (make-type:environment (document-substitution-list target-document)))])
;       (pretty-print tmp)))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
