#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test to reproduce the candy:matchable? misuse in execute-macro.
;; BUG: execute-macro passes denotions as '((a b c **1)) (a list-of-lists)
;;      but candy:matchable? expects a flat parameter template.

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis type domain-specific-language syntax-candy))

(test-begin "candy-matchable-bug")

;; The flat parameter template that SHOULD be passed to candy:matchable?
(define flat-denotions '(a b c **1))

;; The actual argument shape produced by the interpreter.
(define sample-inputs '(inner:list? fixnum? string?))

;; Correct usage: flat parameter template against flat argument list.
;; This SHOULD and DOES return #t.
(test-equal "flat denotions matches correctly"
  #t
  (candy:matchable? flat-denotions sample-inputs))

;; BUG: execute-macro passes the wrapped denotions '((a b c **1))
;; because (with ((a b c **1)) body) stores denotions as a list-of-lists.
;; candy:matchable? sees this as a single segment and fails.
(test-equal "wrapped denotions incorrectly fails — this is the bug"
  #f
  (candy:matchable? '((a b c **1)) sample-inputs))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
