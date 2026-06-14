#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
#!r6rs

(import
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver analysis bad-brackets-scanner))

(define (sort-positions positions)
  (list-sort < positions))

(test-begin "bad-brackets-scanner")

(test-equal "balanced parens"
  '()
  (sort-positions (compute-bad-brackets "(a b)")))

(test-equal "balanced brackets"
  '()
  (sort-positions (compute-bad-brackets "[a b]")))

(test-equal "nested balanced"
  '()
  (sort-positions (compute-bad-brackets "(a [b (c)])")))

(test-equal "unmatched closer"
  '(3)
  (sort-positions (compute-bad-brackets "foo)")))

(test-equal "unmatched bracket closer"
  '(3)
  (sort-positions (compute-bad-brackets "foo]")))

(test-equal "cross-mismatch paren-by-bracket"
  '(0 4)
  (sort-positions (compute-bad-brackets "(a b]")))

(test-equal "cross-mismatch bracket-by-paren"
  '(0 4)
  (sort-positions (compute-bad-brackets "[a b)")))

(test-equal "E1"
  '(0 1 3)
  (sort-positions (compute-bad-brackets "(]([)")))

(test-equal "E2"
  '(0 1 3 5)
  (sort-positions (compute-bad-brackets "(]([)]")))

(test-equal "string-literal"
  '()
  (sort-positions (compute-bad-brackets "\"(a b)\"")))

(test-equal "line-comment"
  '()
  (sort-positions (compute-bad-brackets "; (a b)")))

(test-equal "block-comment"
  '()
  (sort-positions (compute-bad-brackets "#| (a b) |#")))

(test-equal "char-literal-paren"
  '()
  (sort-positions (compute-bad-brackets "#\\(")))

(test-equal "char-literal-space"
  '()
  (sort-positions (compute-bad-brackets "#\\space")))

(test-equal "vector-literal"
  '()
  (sort-positions (compute-bad-brackets "#(1 2 3)")))

(test-equal "C2"
  '(6)
  (sort-positions (compute-bad-brackets "(a [b (c x] y)")))

(test-equal "D1"
  '(1)
  (sort-positions (compute-bad-brackets "([)")))

(test-equal "D4"
  '(1)
  (sort-positions (compute-bad-brackets "[(]")))

(test-equal "C1"
  '(0 18)
  (sort-positions (compute-bad-brackets "(define (foo [bar (baz])")))

(test-equal "E4"
  '(3 12 23)
  (sort-positions (compute-bad-brackets "(a [b (c [d (e] f) g) h)")))

(test-equal "A2"
  '(5 6)
  (sort-positions (compute-bad-brackets "(a b)))")))

(test-equal "A5"
  '(5 6)
  (sort-positions (compute-bad-brackets "(a b))]")))

(test-equal "E3"
  '(0 4 6 10)
  (sort-positions (compute-bad-brackets "(a b]\n(c d]")))

(test-equal "datum-comment-does-not-consume-closing-paren"
  '()
  (sort-positions (compute-bad-brackets "(fxior #;flag-checking-disabled)")))

(test-equal "datum-comment-does-not-consume-closing-bracket"
  '()
  (sort-positions (compute-bad-brackets "[a #;b]")))

(test-equal "datum-comment-around-standalone-symbol"
  '()
  (sort-positions (compute-bad-brackets "(#;x)")))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
