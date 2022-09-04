#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver analysis workspace))

(test-begin "walk-file")
    (let* ( [root-file-node (init-virtual-file-system "./util/" '() folder-or-scheme-file?)])
        (test-equal "io.sls" (file-node-name (car (walk-file root-file-node "./util/io.sls")))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
