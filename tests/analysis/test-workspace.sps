#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import))

(test-begin "init-virtual-file-system")
    (test-equal "scheme-langserver.sls" 
        (find (lambda(n) (equal? n "scheme-langserver.sls")) 
        (map file-node-name 
            (file-node-children (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))))))
(test-end)

(test-begin "init-index-node")
    (test-equal 'library 
        (annotation-stripped 
            (car 
                (annotation-expression 
                (index-node-datum/annotations
                    (init-index-node '() (car (source-file->annotations "./util/io.sls"))))))))
(test-end)

(test-begin "init-library-node")
    (let* ( [root-file-node (init-virtual-file-system "./util/" '() (generate-akku-acceptable-file-filter (string-append "./util" "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)])
        (test-equal 'scheme-langserver (library-node-name (car (library-node-children root-library-node)))))
(test-end)

(test-begin "pick-test")
    (test-equal 'library 
        (annotation-stripped 
            (index-node-datum/annotations 
            (car (pick (init-index-node '() (car (source-file->annotations "./util/path.sls"))) 0 8)))))
(test-end)

(test-begin "refresh-workspace-for+update-file-node-with-tail test")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/cartesian-product.sls"))])
        (update-file-node-with-tail 
            workspace
            target-file-node 
            "(library (scheme-langserver util cartesian-product1)\n  (export cartesian-product)\n  (import (rnrs))\n(define (cartesian-product . lists)\n  (fold-right \n    (lambda (xs ys)\n      (apply append \n        (map (lambda (x)\n          (map (lambda (y)\n            (cons x y))\n            ys))\n        xs)))\n    '(())\n    lists))\n)"
            )
        (refresh-workspace-for workspace target-file-node)
        (test-equal #f (null? (walk-library '(scheme-langserver util cartesian-product1) root-library-node))))
(test-end)

(test-begin "library-import-process")
    (let* ( [workspace (init-workspace (current-directory) #f #f)]  
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/run.ss"))]
            [document (file-node-document target-file-node)])
        (test-equal 
            'init-server
            (find 
                (lambda (identifier) (equal? identifier 'init-server))
                (map identifier-reference-identifier (document-ordered-reference-list document)))))
(test-end)

(test-begin "init-workspace-basic-test")
(let* ([workspace (init-workspace (string-append (current-directory) "/tests/resources/r7rs") 'txt 'r7rs #f #f)]
        [root-file-node (workspace-file-node workspace)]
        [root-library-node (workspace-library-node workspace)])
    (test-equal #f (null? root-file-node))
    (test-equal #f (null? root-library-node)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
