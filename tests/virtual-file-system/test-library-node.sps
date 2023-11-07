#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace))

(test-begin "walk-library")
    (let* ( [root-file-node (init-virtual-file-system "./util/" '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)])
        (test-equal "io.sls"
            (file-node-name (car (library-node-file-nodes 
                (walk-library 
                    '(scheme-langserver util io) 
                    root-library-node))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
