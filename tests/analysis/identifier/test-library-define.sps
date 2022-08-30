#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier rules library-define)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document))


(test-begin "library-define-process")
    (let* ( [root-file-node (init-virtual-file-system "./util" '() folder-or-scheme-file?)]
            [target-file-node (car (walk-file root-file-node "./util/io.sls"))]
            [document (file-node-document target-file-node)]
            [index-node (document-index-node document)])
        (library-define-process root-file-node document index-node)
        ; (test-equal 'scheme-langserver (library-node-name (car (library-node-children root-library-node))))
        )
(test-end)

(test-begin "pick-test")
    (test-equal 'library 
        (annotation-stripped 
            (index-node-datum/annotations 
            (car (pick (init-index-node '() (source-file->annotation "./util/path.sls")) 0 8)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
