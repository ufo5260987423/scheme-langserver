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
    ; Both ] should be reported as unexpected close bracket, plus 2 unclosed parens
    (test-equal 4 (length diagnoses))
    (test-equal 2
      (length (filter (lambda (d) (string-contains? "unexpected close bracket" (list-ref d 3))) diagnoses)))
    (test-equal 2
      (length (filter (lambda (d) (string-contains? "unclosed parenthesis" (list-ref d 3))) diagnoses)))))
(test-end)

(test-begin "cross-bracket-mismatch")
(let ([doc (make-document "file:///test.ss" "(a [b (c x] y)" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a [b (c x] y)"))))
  (source-file->annotations "(a [b (c x] y)" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; Task 2: ] at 10 matches [ at 3 after cleaning; only ( at 6 is orphaned.
    (test-equal 1 (length diagnoses))
    ; ( at 6 is orphaned (human-view: ] was meant for [ at 3, leaving ( at 6 unmatched)
    (test-assert
      (find (lambda (d) (and (= 6 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses))
    ; [ at 3 is NOT reported -- it was consumed by ] at 10 in human-view pairing
    (test-equal 0
      (length (filter (lambda (d) (and (= 3 (car d)) (string-contains? "unclosed bracket" (list-ref d 3)))) diagnoses)))
    ; ( at 0 is NOT reported -- closed by ) at 13
    (test-equal 0
      (length (filter (lambda (d) (and (= 0 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses)))))
(test-end)

;; P1: multiple consecutive/independent close delimiters should all be diagnosed
;; (A2, A5, E3 from task1.md)

(test-begin "consecutive-extra-close-parens")
(let ([doc (make-document "file:///test.ss" "(a b)))" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a b)))"))))
  (source-file->annotations "(a b)))" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; Both extra ) should be reported
    (test-equal 2 (length diagnoses))
    (test-equal 2
      (length (filter (lambda (d) (string-contains? "unexpected close parenthesis" (list-ref d 3))) diagnoses)))
    (test-assert
      (find (lambda (d) (and (= 5 (car d)) (string-contains? "unexpected close parenthesis" (list-ref d 3)))) diagnoses))
    (test-assert
      (find (lambda (d) (and (= 6 (car d)) (string-contains? "unexpected close parenthesis" (list-ref d 3)))) diagnoses))))
(test-end)

(test-begin "consecutive-extra-close-paren-and-bracket")
(let ([doc (make-document "file:///test.ss" "(a b))]" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(a b))]"))))
  (source-file->annotations "(a b))]" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; Both extra ) and ] should be reported
    (test-equal 2 (length diagnoses))
    (test-assert
      (find (lambda (d) (and (= 5 (car d)) (string-contains? "unexpected close parenthesis" (list-ref d 3)))) diagnoses))
    (test-assert
      (find (lambda (d) (and (= 6 (car d)) (string-contains? "unexpected close bracket" (list-ref d 3)))) diagnoses))))
(test-end)

;; E1/E2: cross-mismatch position and unclosed paren filtering

(test-begin "E1-cross-mismatch-with-extra-close")
(let ([doc (make-document "file:///test.ss" "(]([)" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(]([)"))))
  (source-file->annotations "(]([)" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; Task 2: ) at 4 pairs with ( at 2 after cleaning; 3 diagnoses total.
    ; ( at 2 should NOT be reported because in human view it pairs with ) at 4
    (test-equal 3 (length diagnoses))
    ; ] at 1 -- unmatched close bracket
    (test-assert
      (find (lambda (d) (and (= 1 (car d)) (string-contains? "unexpected close bracket" (list-ref d 3)))) diagnoses))
    ; ( at 0 unclosed
    (test-assert
      (find (lambda (d) (and (= 0 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses))
    ; [ at 3 unclosed
    (test-assert
      (find (lambda (d) (and (= 3 (car d)) (string-contains? "unclosed bracket" (list-ref d 3)))) diagnoses))
    ; ) at 4 MUST NOT appear -- it pairs with ( at 2 in cleaned source
    (test-equal 0
      (length (filter (lambda (d) (and (= 4 (car d)) (string-contains? "bracketed list terminated by parenthesis" (list-ref d 3)))) diagnoses)))
    ; ( at 2 MUST NOT appear -- human view pairs it with ) at 4
    (test-equal 0
      (length (filter (lambda (d) (and (= 2 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses)))))
(test-end)

(test-begin "E2-cross-mismatch-with-nested-bracket")
(let ([doc (make-document "file:///test.ss" "(]([)]" '())])
  (call-with-port (open-file-output-port "/tmp/test-tokenizer/test.ss" (file-options replace))
    (lambda (p) (put-bytevector p (string->utf8 "(]([)]"))))
  (source-file->annotations "(]([)]" "/tmp/test-tokenizer/test.ss" 0 #t doc)
  (let ([diagnoses (document-diagnoses doc)])
    ; Task 2: ) at 4 pairs with ( at 2; ] at 5 is unmatched. 4 diagnoses total.
    (test-equal 4 (length diagnoses))
    ; ] at 1 -- unmatched close bracket
    (test-assert
      (find (lambda (d) (and (= 1 (car d)) (string-contains? "unexpected close bracket" (list-ref d 3)))) diagnoses))
    ; ( at 0 unclosed
    (test-assert
      (find (lambda (d) (and (= 0 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses))
    ; [ at 3 unclosed -- orphaned because ) at 4 pairs with ( at 2
    (test-assert
      (find (lambda (d) (and (= 3 (car d)) (string-contains? "unclosed bracket" (list-ref d 3)))) diagnoses))
    ; ] at 5 is unmatched close bracket
    (test-assert
      (find (lambda (d) (and (= 5 (car d)) (string-contains? "unexpected close bracket" (list-ref d 3)))) diagnoses))
    ; ) at 4 MUST NOT appear -- it pairs with ( at 2 in cleaned source
    (test-equal 0
      (length (filter (lambda (d) (and (= 4 (car d)) (string-contains? "bracketed list terminated by parenthesis" (list-ref d 3)))) diagnoses)))
    ; ( at 2 MUST NOT appear -- human view pairs it with ) at 4
    (test-equal 0
      (length (filter (lambda (d) (and (= 2 (car d)) (string-contains? "unclosed parenthesis" (list-ref d 3)))) diagnoses)))))
(test-end)

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
