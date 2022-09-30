#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver util natural-order-compare))

(test-begin "natural-order-compare")
    (test-equal '("a" "ac" "b" "bac" "c") (sort natural-order-compare '("b" "ac" "a" "bac" "c")))
(test-end)


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
