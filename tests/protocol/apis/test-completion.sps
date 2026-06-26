#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver protocol apis completion)
  (scheme-langserver util association))

(test-begin "completion on unresolved uri returns empty vector")
(let* ([workspace (init-workspace (current-directory) 'akku 'r6rs #f #f)]
       [params (make-alist
                 'textDocument (make-alist 'uri "file:///nonexistent/ignored.sls")
                 'position (make-alist 'line 0 'character 0))]
       [result (completion workspace params)])
  (test-equal "unresolved uri returns empty vector" '#() result))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
