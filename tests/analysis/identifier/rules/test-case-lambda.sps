#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (rnrs (6)) 
  (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules case-lambda)
  (scheme-langserver analysis tokenizer)

  (scheme-langserver util test)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document))

(test-begin "case-lambda-process")
  (let* ( [root-file-node (init-virtual-file-system "./util" '() (lambda (fuzzy) #t))]
      [root-library-node '()]
      [target-file-node (walk-file root-file-node "./util/matrix.sls")]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [matrix-take-node (find-define-by-name root-index-node 'matrix-take)]
      [ready-index-node (caddr (index-node-children matrix-take-node))]
      [target-index-node (cadr (index-node-children ready-index-node))])
    (case-lambda-process root-file-node root-library-node document ready-index-node)
    (test-equal #f
    (not 
      (find 
        (lambda (reference) 
          (equal? 'n (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node target-index-node)))))
(test-end)

(test-begin "case-lambda-process handles shared formals")
  (let* ([src "(begin '#1=(x y) (case-lambda (#1# (+ x y))))\n"]
      [path "/tmp/test-case-lambda-shared-formals.ss"]
      [_ (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
           (display src p)
           (close-port p))]
      [document (make-document (string-append "file://" path) src '())]
      [root-index-node (init-index-node '() (car (source-file->annotations path)))]
      [case-lambda-node (caddr (index-node-children root-index-node))]
      [clause-node (cadr (index-node-children case-lambda-node))])
    (case-lambda-process '() '() document case-lambda-node)
    (test-assert "parameter x is bound"
      (find 
        (lambda (reference) 
          (equal? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node clause-node)))
    (test-assert "parameter y is bound"
      (find 
        (lambda (reference) 
          (equal? 'y (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node clause-node)))
    (test-equal "no diagnoses" '() (document-diagnoses document)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
