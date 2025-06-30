#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver analysis workspace))

(test-begin "init-workspace-basic-test")
  (pretty-print `(DEBUG: test workspace init-workspace))
  ;; (pretty-print `(DEBUG: var: current-directory ,(current-directory)))
(let* ([workspace (init-workspace (current-directory) 'akku 'r7rs #f #f)]
       [root-file-node (workspace-file-node workspace)]
       [root-library-node (workspace-library-node workspace)])
  ;; (pretty-print `(DEBUG: workspace ,workspace))
  (test-equal #f (null? root-file-node))
  (test-equal #f (null? root-library-node)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))