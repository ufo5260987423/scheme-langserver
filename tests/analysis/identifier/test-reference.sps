#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis workspace))

(test-begin "find-available-references")
    (let* ([workspace-instance (init-workspace (current-directory))]
            [root-file-node (workspace-file-node workspace-instance)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/io.sls"))]
            [index-node (document-index-node (file-node-document target-file-node))])
        (test-equal #f (null? 
            (find-available-references-for index-node)
        ))
    )
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
