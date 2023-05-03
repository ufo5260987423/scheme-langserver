#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis dependency file-linkage))

(test-begin "init-linkage-matrix")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() akku-acceptable-file?)]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-library-node)]
            [from-path (string-append (current-directory) "/analysis/workspace.sls")]
            [to-path (string-append (current-directory) "/util/io.sls")])
        (test-equal 1 (file-linkage-take file-linkage from-path to-path)))
(test-end)

(test-begin "get-init-inference-path")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() akku-acceptable-file?)]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-library-node)]
            [paths (get-init-reference-path file-linkage)]
            [target-path (string-append (current-directory) "/protocol/error-code.sls")])
        (test-equal target-path (find (lambda (p) (equal? target-path p)) paths)))
(test-end)

(test-begin "file-linkage-to")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() akku-acceptable-file?)]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-library-node)]
            [to-path (string-append (current-directory) "/protocol/error-code.sls")]
            [paths (file-linkage-to file-linkage to-path)])
        (test-equal 
            (string-append (current-directory) "/scheme-langserver.sls")
            (car paths)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
