#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
(scheme-langserver virtual-file-system file-node)
(scheme-langserver analysis virtual-source-file-system))

(test-begin "init-virtual-file-system")
    (let* ([file-node-instance  (init-virtual-file-system (current-directory) '() folder-or-scheme-file?)]
            [source-file-node (init-virtual-source-file-system file-node-instance)])
    )
(test-end)

; (test-begin "pick-test")
;     (test-equal 'library 
;         (annotation-stripped 
;             (index-node-datum/annotations 
;             (car (pick (init-index-node '() (source-file->annotation "./util/path.sls")) 0 8)))))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
