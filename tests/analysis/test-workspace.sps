#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    ; (chezscheme) 
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
        (map file-node-name (file-node-children (init-virtual-file-system (current-directory) '() akku-acceptable-file?)))))
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
    (let* ( [root-file-node (init-virtual-file-system "./util/" '() akku-acceptable-file?)]
            [root-library-node (init-library-node root-file-node)])
        (test-equal 'scheme-langserver (library-node-name (car (library-node-children root-library-node)))))
(test-end)

(test-begin "pick-test")
    (test-equal 'library 
        (annotation-stripped 
            (index-node-datum/annotations 
            (car (pick (init-index-node '() (car (source-file->annotations "./util/path.sls"))) 0 8)))))
(test-end)

(test-begin "refresh-workspace-for-test")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/natural-order-compare.sls"))])
        (refresh-workspace-for 
            workspace 
            target-file-node 
            "(library (scheme-langserver util natural-order-compare1)\n    (export natural-order-compare)\n    (import (rnrs) )\n\n(define natural-order-compare \n    (case-lambda \n        [(string-a string-b) (natural-order-compare string-a string-b 0 0)] \n        [(string-a string-b index-a index-b) \n            (let ([length-a (string-length string-a)] \n                    [length-b (string-length string-b)]) \n                (if (or (>= index-a length-a) \n                        (>= index-b length-b)) \n                    (< length-a length-b) \n                    (let ([char-a (string-ref string-a index-a)] \n                            [char-b (string-ref string-b index-b)]) \n                        (if (char=? char-a char-b) \n                            (natural-order-compare string-a string-b (+ 1 index-a) (+ 1 index-b)) \n                            (char<? char-a char-b)))))])) \n)"
            'single)
        (test-equal #f (null? (walk-library '(scheme-langserver util natural-order-compare1) root-library-node))))
(test-end)

; (test-begin "library-import-process")
;     (let* ( [workspace (init-workspace (current-directory) #f #t)]  
;             [root-file-node (workspace-file-node workspace)]
;             [root-library-node (workspace-library-node workspace)]
;             [target-file-node (walk-file root-file-node (string-append (current-directory) "/run.ss"))]
;             [document (file-node-document target-file-node)])
;         (pretty-print (map identifier-reference-identifier (document-reference-list document)))
;         (test-equal 
;             'init-server
;             (find 
;                 (lambda (identifier) (equal? identifier 'init-server))
;                 (map identifier-reference-identifier (document-reference-list document)))))
; (test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
