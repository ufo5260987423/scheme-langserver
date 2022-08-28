#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver analysis workspace))

(test-begin "init-virtual-file-system")
    (test-equal "scheme-langserver.sls" 
        (find (lambda(n) (equal? n "scheme-langserver.sls")) 
        (map file-node-name (file-node-children (init-virtual-file-system (current-directory) '() folder-or-scheme-file?)))))
(test-end)

(test-begin "index-test")
    (test-equal 'library 
        (annotation-stripped 
            (car 
                (annotation-expression 
                (index-node-datum/annotations
                    (init-index-node '() (source-file->annotation "./util/io.sls")))))))
(test-end)

(test-begin "pick-test")
    (test-equal 'library 
        (annotation-stripped 
            (index-node-datum/annotations 
            (car (pick (init-index-node '() (source-file->annotation "./util/path.sls")) 0 8)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
