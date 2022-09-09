#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis dependency file-linkage))

(test-begin "init-linkage-matrix")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() folder-or-scheme-file?)]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-library-node)]
            [from-path (string-append (current-directory) "/analysis/workspace.sls")]
            [to-path (string-append (current-directory) "/util/io.sls")])
        (test-equal 1 (file-linkage-take file-linkage from-path to-path))
        ; (test-equal '(1 0) (file-linkage-head file-linkage))
        )
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
