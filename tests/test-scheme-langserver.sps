#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver))

(test-begin "hello")
(test-equal "Hello World!" (hello "World"))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
