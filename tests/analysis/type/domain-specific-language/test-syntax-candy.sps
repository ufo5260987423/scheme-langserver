#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis type domain-specific-language syntax-candy))

;; ---------------------------------------------------------------------------
;; A. candy:segmentable?
;; ---------------------------------------------------------------------------
(test-begin "candy:segmentable?")
  (test-equal #t (candy:segmentable? '(a b c)))
  (test-equal #t (candy:segmentable? '(a b ... c)))
  (test-equal #t (candy:segmentable? '(a b **1 c)))
  (test-equal #f (candy:segmentable? '(... a)))
  (test-equal #f (candy:segmentable? '(**1 a)))
  (test-equal #t (candy:segmentable? '(a **1 b **1)))
  (test-equal #t (candy:segmentable? '()))
(test-end)

;; ---------------------------------------------------------------------------
;; B. candy:matchable?
;; ---------------------------------------------------------------------------
(test-begin "candy:matchable?")
  (test-equal #t (candy:matchable? '(a b c) '(1 2 3)))
  (test-equal #t (candy:matchable? '(a b ... c) '(1 2 3 4 5)))
  (test-equal #t (candy:matchable? '(a ... b) '(1 2)))
  (test-equal #t (candy:matchable? '(a b **1 c) '(1 2 3 4 5)))
  (test-equal #f (candy:matchable? '(a b **1 c) '(1 2)))
  (test-equal #f (candy:matchable? '(a b) '(1)))
  (test-equal #t (candy:matchable? '(a ... b ...) '(1 2 3)))
  (test-equal #t (candy:matchable? '(a **1 b **1) '(1 2 3)))
(test-end)

;; ---------------------------------------------------------------------------
;; C. candy:match + candy:match-left
;; ---------------------------------------------------------------------------
(test-begin "candy:match-left")
  ;; simple destructuring
  (test-equal
    '((a . 1) (b . 2) (c . 3))
    (candy:match-left '(a b c) '(1 2 3)))

  ;; ... consumes multiple
  (test-equal
    '((a . 1) (b . (2 3 4)) (c . 5))
    (candy:match-left '(a b ... c) '(1 2 3 4 5)))

  ;; ... consumes zero
  (test-equal
    '((a 1) (b . 2))
    (candy:match-left '(a ... b) '(1 2)))

  ;; **1 consumes multiple
  (test-equal
    '((a . 1) (b . (2 3 4)) (c . 5))
    (candy:match-left '(a b **1 c) '(1 2 3 4 5)))

  ;; only ...
  (test-equal
    '((a . (1 2 3)))
    (candy:match-left '(a ...) '(1 2 3)))

  ;; only **1
  (test-equal
    '((a . (1)))
    (candy:match-left '(a **1) '(1)))
(test-end)

;; ---------------------------------------------------------------------------
;; D. candy:match-right
;; ---------------------------------------------------------------------------
(test-begin "candy:match-right")
  ;; simple case: right side is plain, so each right segment maps to one left type
  (let ([result (candy:match-right '(a b c) '(1 2 3))])
    (test-equal #t (list? result))
    (test-equal 3 (length result))
    (test-equal 'a (caar result))
    (test-equal '1 (cdar result)))

  ;; with repeat: right side stays plain, left side is aggregated in the pair
  (let ([result (candy:match-right '(a b ... c) '(1 2 3 4 5))])
    (test-equal #t (list? result))
    (test-equal 5 (length result)))
(test-end)

;; ---------------------------------------------------------------------------
;; E. segment record type
;; ---------------------------------------------------------------------------
(test-begin "segment record")
  ;; segment accessors through candy:match results
  (let* ([pairs (candy:match '(a b c) '(1 2 3))]
         [seg (car (car pairs))])
    (test-equal 'a (segment-type seg))
    (test-equal '() (segment-tail seg))
    (test-equal #t (segment? seg)))

  (let* ([pairs (candy:match '(a **1 b) '(1 2 3))]
         [seg-a (car (car pairs))]
         [seg-b (car (caddr pairs))])
    (test-equal 'a (segment-type seg-a))
    (test-equal '**1 (segment-tail seg-a))
    (test-equal 'b (segment-type seg-b))
    (test-equal '() (segment-tail seg-b)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
