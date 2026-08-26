#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver analysis identifier rules lambda)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis tokenizer)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document))

(define (write-temp-file path src)
  (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
    (display src p)
    (close-port p)))

(test-begin "lambda-process handles list formals")
  (let* ([src "(lambda (x y) (+ x y))\n"]
      [path "/tmp/test-lambda-list.ss"]
      [_ (write-temp-file path src)]
      [document (make-document (string-append "file://" path) src '())]
      [lambda-node (init-index-node '() (car (source-file->annotations src path)))]
      [params-node (cadr (index-node-children lambda-node))]
      [body-node (caddr (index-node-children lambda-node))]
      [x-param (car (index-node-children params-node))]
      [y-param (cadr (index-node-children params-node))])
    (lambda-process '() '() document lambda-node)
    (test-assert "parameter x is imported at lambda node"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda-node)))
    (test-assert "parameter y is imported at lambda node"
      (find 
        (lambda (reference) 
          (eq? 'y (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda-node)))
    (test-assert "parameter x is exported from its parameter node"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-export-to-other-node x-param)))
    (test-assert "parameter y is exported from its parameter node"
      (find 
        (lambda (reference) 
          (eq? 'y (identifier-reference-identifier reference)))
        (index-node-references-export-to-other-node y-param)))
    (test-assert "x usage in body resolves"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'x (annotation-stripped-expression node)))
            body-node)
          'x)))
    (test-assert "y usage in body resolves"
      (find 
        (lambda (reference) 
          (eq? 'y (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'y (annotation-stripped-expression node)))
            body-node)
          'y))))
(test-end)

(test-begin "lambda-process handles single rest formal")
  (let* ([src "(lambda x x)\n"]
      [path "/tmp/test-lambda-rest.ss"]
      [_ (write-temp-file path src)]
      [document (make-document (string-append "file://" path) src '())]
      [lambda-node (init-index-node '() (car (source-file->annotations src path)))]
      [x-param (cadr (index-node-children lambda-node))]
      [body-node (caddr (index-node-children lambda-node))])
    (lambda-process '() '() document lambda-node)
    (test-assert "rest parameter x is imported at lambda node"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node lambda-node)))
    (test-assert "rest parameter x is exported from its parameter node"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-export-to-other-node x-param)))
    (test-assert "x usage in body resolves"
      (find 
        (lambda (reference) 
          (eq? 'x (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'x (annotation-stripped-expression node)))
            body-node)
          'x))))
(test-end)

(test-begin "lambda-process reports duplicate formals")
  (let* ([src "(lambda (x x) x)\n"]
      [path "/tmp/test-lambda-duplicate.ss"]
      [_ (write-temp-file path src)]
      [document (make-document (string-append "file://" path) src '())]
      [lambda-node (init-index-node '() (car (source-file->annotations src path)))])
    (lambda-process '() '() document lambda-node)
    (test-assert "duplicate formal produces a diagnosis"
      (not (null? (document-diagnoses document)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
