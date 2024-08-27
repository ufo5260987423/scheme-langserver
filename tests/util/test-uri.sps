#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver util path))


(test-begin "path->uri & uri->path")
  (let ([path "/.akku/lib/srfi/%3a13/srfi-13.scm"]
      [uri "file:///.akku/lib/srfi/%253a13/srfi-13.scm"])
    (test-equal uri (path->uri path))
    (test-equal path (uri->path uri)))
(test-end)


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))