#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis package-manager paths-filter)
  (scheme-langserver analysis workspace))

(test-begin "test router works for self-defined rules")
  (let* ([paths 
      (map 
        (lambda (p) (string-append (current-directory) "/" p)) 
        '(
          ".akku"
          ".akku/lib"
          ".akku/lib/ufo-match.chezscheme.sls"

          "analysis"
          "analysis/identifier"
          "analysis/identifier/rules"
          "analysis/identifier/rules/let.sls"
    ))]
    [workspace-instance (init-workspace (current-directory) 'akku 'r6rs #f #f (generate-paths-file-filter (current-directory) paths))]
    [root-file-node (workspace-file-node workspace-instance)]
    [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/let.sls"))]
    [document (file-node-document target-file-node)])
   (let ([index-node (car (document-index-node-list document))])
     (test-equal #f (null? (find-available-references-for document index-node)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
