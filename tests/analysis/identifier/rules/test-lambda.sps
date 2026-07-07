#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier rules lambda)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis tokenizer)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document))

(test-begin "lambda-process handles shared formals")
  (let* ([src "(begin '#1=(x y) (lambda #1# (+ x y)))\n"]
      [path "/tmp/test-lambda-shared-formals.ss"]
      [_ (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
           (display src p)
           (close-port p))]
      [document (make-document (string-append "file://" path) src '())]
      [root-index-node (init-index-node '() (car (source-file->annotations path)))]
      [lambda-node (caddr (index-node-children root-index-node))])
    (lambda-process '() '() document lambda-node)
    (test-assert "parameter x is bound"
      (find 
        (lambda (reference) 
          (equal? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda-node)))
    (test-assert "parameter y is bound"
      (find 
        (lambda (reference) 
          (equal? 'y (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda-node)))
    (test-equal "no diagnoses" '() (document-diagnoses document)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
