#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver util cartesian-product))

(test-begin "cartesian-product")
    (test-equal '((())) (cartesian-product '(())))
    (test-equal '() (cartesian-product '()))
    (test-equal '() (cartesian-product '(1) '()))
    (test-equal '((1)) (cartesian-product '(1)))
    (test-equal '((1 2)) (cartesian-product '(1) '(2)))
    (test-equal '((1 2) (1 3)) (cartesian-product '(1) '(2 3)))
    (test-equal '((1 2) (1 3) (4 2) (4 3)) (cartesian-product '(1 4) '(2 3)))
    ; (test-equal '(((1 2) 0) ((1 3) 0) ((4 2) 0) ((4 3) 0)) (cartesian-product '(1 4) '(2 3) '(0)))
    (test-equal '((1 2 0) (1 3 0) (4 2 0) (4 3 0)) (cartesian-product '(1 4) '(2 3) '(0)))
(test-end)


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
