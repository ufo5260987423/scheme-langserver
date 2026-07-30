#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier rules define)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis tokenizer)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "define-process")
  (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
      [root-library-node '()]
      [target-file-node (walk-file root-file-node "./util/io.sls")]
      [document (file-node-document target-file-node)]
      [index-node (car (document-index-node-list document))])
    (map (lambda (node) (define-process root-file-node root-library-node document node)) (index-node-children index-node))
    (test-equal #t
    (not (null? 
        (find 
          (lambda (reference) 
            (equal? 'read-string 
              (annotation-stripped 
                (index-node-datum/annotations 
                  (identifier-reference-index-node reference)))))
          (index-node-references-import-in-this-node index-node))))))
(test-end)

(test-begin "define-process handles cyclic improper rest formals")
  (let* ([src "(define (f . #1=(x . #1#)) 1)\n"]
      [path "/tmp/test-define-cyclic-formals.ss"]
      [_ (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
           (display src p)
           (close-port p))]
      [document (make-document (string-append "file://" path) src '())]
      [root (init-index-node '() (car (source-file->annotations path)))])
    (define-process '() '() document root)
    (test-assert "parameter x is bound"
      (find 
        (lambda (reference) 
          (equal? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node root)))
    (test-equal "no diagnoses" '() (document-diagnoses document)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
