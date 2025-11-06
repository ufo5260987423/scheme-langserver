#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    ; (rnrs (6)) 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace))

(test-begin "walk-file for .scm and find document")
    (let* ( [target-path (string-append (current-directory) "/.akku/lib/srfi/%3a13")]
            [root-file-node (init-virtual-file-system target-path '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
            [target-file-node (walk-file root-file-node (string-append target-path "/srfi-13.scm"))])
        (test-equal #f (null? (document-index-node-list (file-node-document target-file-node)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
