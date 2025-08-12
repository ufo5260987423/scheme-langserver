#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter)
    (scheme-langserver analysis dependency file-linkage))

(test-begin "init-linkage-matrix")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-file-node root-library-node)]
            [from-path (string-append (current-directory) "/analysis/workspace.sls")]
            [to-path (string-append (current-directory) "/util/io.sls")])
        (test-equal 1 (file-linkage-take file-linkage from-path to-path)))
(test-end)

(test-begin "get-init-inference-path")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-file-node root-library-node)]
            [paths (apply append (get-init-reference-batches file-linkage))]
            [target-path (string-append (current-directory) "/protocol/error-code.sls")])
        (test-equal target-path (find (lambda (p) (equal? target-path p)) paths)))
(test-end)

(test-begin "file-linkage-to")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)]
            [file-linkage (init-file-linkage root-file-node root-library-node)]
            [to-path (string-append (current-directory) "/protocol/error-code.sls")]
            [paths (file-linkage-to file-linkage to-path)])
        (test-equal 
            (string-append (current-directory) "/scheme-langserver.sls")
            (car paths)))
(test-end)

(test-begin "init-linkage-matrix-r7rs")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
            [root-library-node (init-library-node root-file-node 'r7rs)]
            [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
            [from-path (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")]
            [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")])
        (test-equal 1 (file-linkage-take file-linkage from-path to-path)))
(test-end)

(test-begin "get-init-inference-path-r7rs")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
            [root-library-node (init-library-node root-file-node 'r7rs)]
            [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
            [paths (apply append (get-init-reference-batches file-linkage))]
            [target-path (string-append (current-directory) "/tests/resources/r7rs/scheme/base.scm.txt")])
        (test-equal target-path (find (lambda (p) (equal? target-path p)) paths)))
(test-end)

(test-begin "file-linkage-to-r7rs")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
            [root-library-node (init-library-node root-file-node 'r7rs)]
            [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
            [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")]
            [paths (file-linkage-to file-linkage to-path)])
        (test-equal 
            (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")
            (car paths)))
(test-end)

(test-begin "file-linkage-to-s7")
    (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 's7)]
            [root-library-node (init-library-node root-file-node 's7)]
            [file-linkage (init-file-linkage root-file-node root-library-node 's7)]
            [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")]
            [paths (file-linkage-to file-linkage to-path)])
        (test-equal 
            (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")
            (car paths)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
