#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver virtual-file-system document))

(define (string-contains? substr str)
  (let ([sub-len (string-length substr)]
        [str-len (string-length str)])
    (let loop ([i 0])
      (cond
        [(> i (- str-len sub-len)) #f]
        [(string=? substr (substring str i (+ i sub-len))) #t]
        [else (loop (+ i 1))]))))

(test-begin "tokenizer-diagnoses")

(test-begin "nested-unclosed-parens")
(let ([doc (make-document "file:///test.ss" "(a (b (c" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a (b (c"))))
  (source-file->annotations "(a (b (c" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    (test-equal 3 (length diagnoses))
    (test-equal 3
      (length (filter (lambda (d) (string-contains? "unclosed parenthesis" (list-ref d 3))) diagnoses)))))
(test-end)

(test-begin "extra-close-paren")
(let ([doc (make-document "file:///test.ss" "(foo (bar)))" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(foo (bar)))"))))
  (source-file->annotations "(foo (bar)))" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    (test-equal 1 (length diagnoses))
    (test-assert
      (find (lambda (d) (string-contains? "unexpected close parenthesis" (list-ref d 3))) diagnoses))))
(test-end)

(test-begin "multiple-independent-errors")
(let ([doc (make-document "file:///test.ss" "(a b]\n(c d]" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a b]\n(c d]"))))
  (source-file->annotations "(a b]\n(c d]" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; 1 original cross-bracket + 2 unclosed parens
    (test-equal 3 (length diagnoses))
    (test-equal 1
      (length (filter (lambda (d) (string-contains? "parenthesized list terminated by bracket" (list-ref d 3))) diagnoses)))
    (test-equal 2
      (length (filter (lambda (d) (string-contains? "unclosed parenthesis" (list-ref d 3))) diagnoses)))))
(test-end)

(test-begin "cross-bracket-mismatch")
(let ([doc (make-document "file:///test.ss" "(a [b (c x] y)" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a [b (c x] y)"))))
  (source-file->annotations "(a [b (c x] y)" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; 1 original cross-bracket + 1 unclosed paren + 1 unclosed bracket
    (test-equal 3 (length diagnoses))
    (test-assert
      (find (lambda (d) (string-contains? "parenthesized list terminated by bracket" (list-ref d 3))) diagnoses))
    (test-assert
      (find (lambda (d) (string-contains? "unclosed bracket" (list-ref d 3))) diagnoses))
    (test-assert
      (find (lambda (d) (string-contains? "unclosed parenthesis" (list-ref d 3))) diagnoses))))
(test-end)

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
