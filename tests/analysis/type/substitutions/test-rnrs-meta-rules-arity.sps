#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

;; 2026-11-04: meta-rule templates are arity-checked at every call site
;; (aed09617 gate), so a wrong segment count untypes the call. This file
;; pins the repaired shapes: for each rule, the real arity range matches
;; and one argument beyond the maximum does not.

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver analysis type domain-specific-language syntax-candy)
  (scheme-langserver analysis identifier meta))

(define (construct str)
  (construct-type-expression-with-meta (read (open-string-input-port str))))

;; (label template-string arg-count expected)
(define cases
  '(;; optional trailing arg: the short common form must match now
    ("make-vector 1"      "(inner:list? integer?)"                  1 #t)
    ("make-vector 2"      "(inner:list? integer? something?)"       2 #t)
    ("make-vector 3"      "(inner:list? integer? something?)"       3 #f)
    ("bytevector-s16-ref 2"  "(inner:list? bytevector? integer?)" 2 #t)
    ("bytevector-s16-ref 3"  "(inner:list? bytevector? integer? something?)" 3 #t)
    ("bytevector-s16-ref 4"  "(inner:list? bytevector? integer? something?)" 4 #f)
    ("display 1"          "(inner:list? something?)"                1 #t)
    ("display 2"          "(inner:list? something? output-port?)"   2 #t)
    ("display 3"          "(inner:list? something? output-port?)"   3 #f)
    ("write 2"            "(inner:list? something? output-port?)"   2 #t)
    ("fasl-write 1"       "(inner:list? something? output-port?)"   1 #f)
    ("fasl-write 2"       "(inner:list? something? output-port?)"   2 #t)
    ("fasl-write 3"       "(inner:list? something? output-port? something?)" 3 #t)
    ("fasl-write 4"       "(inner:list? something? output-port? something?)" 4 #f)
    ;; wrong-arity repairs
    ("record-predicate 1" "(inner:list? record-type-descriptor?)"   1 #t)
    ("record-predicate 2" "(inner:list? record-type-descriptor?)"   2 #f)
    ("record-mutator 2"   "(inner:list? record-type-descriptor? integer?)" 2 #t)
    ("record-mutator 3"   "(inner:list? record-type-descriptor? integer?)" 3 #f)
    ("make-rtd 6"         "(inner:list? symbol? something? something? boolean? boolean? something?)" 6 #t)
    ("make-rtd 5"         "(inner:list? symbol? something? something? boolean? boolean? something?)" 5 #f)
    ;; append: 0, 1, 2, ... arguments all typeable
    ("append 0"           "(inner:list? (inner:list? something? ...) ...)" 0 #t)
    ("append 1"           "(inner:list? (inner:list? something? ...) ...)" 1 #t)
    ("append 3"           "(inner:list? (inner:list? something? ...) ...)" 3 #t)))

(define (arg-list-shape n)
  (let loop ([i 0] [acc '()])
    (if (= i n)
      `(inner:list? ,@(reverse acc))
      (loop (+ i 1) (cons 'something? acc)))))

(test-begin "rnrs-meta-rules arity")
(for-each
  (lambda (c)
    (let ([label (car c)]
          [template (construct (cadr c))]
          [arg-count (caddr c)]
          [expected (cadddr c)]
          [args (construct-type-expression-with-meta
                  (arg-list-shape (caddr c)))])
      (test-equal label expected (candy:matchable? template args))))
  cases)
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
