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

(define (write-temp-file path src)
  (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
    (display src p)
    (close-port p)))

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

(test-begin "case-lambda-process binds per clause")
  (let* ([src "(case-lambda ((x) x) ((y z) (+ y z)))\n"]
      [path "/tmp/test-case-lambda-clauses.ss"]
      [_ (write-temp-file path src)]
      [document (make-document (string-append "file://" path) src '())]
      [case-lambda-node (init-index-node '() (car (source-file->annotations src path)))]
      [clause1 (cadr (index-node-children case-lambda-node))]
      [clause2 (caddr (index-node-children case-lambda-node))]
      [params1 (car (index-node-children clause1))]
      [body1 (cadr (index-node-children clause1))]
      [params2 (car (index-node-children clause2))]
      [body2 (cadr (index-node-children clause2))]
      [x-param (car (index-node-children params1))]
      [y-param (car (index-node-children params2))]
      [z-param (cadr (index-node-children params2))])
    (case-lambda-process '() '() document case-lambda-node)
    (test-assert "x is imported in clause1"
      (find 
        (lambda (reference) (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node clause1)))
    (test-assert "y is imported in clause2"
      (find 
        (lambda (reference) (eq? 'y (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node clause2)))
    (test-assert "z is imported in clause2"
      (find 
        (lambda (reference) (eq? 'z (identifier-reference-identifier reference)))
        (index-node-references-import-in-this-node clause2)))
    (test-assert "x is exported from its parameter node"
      (find 
        (lambda (reference) (eq? 'x (identifier-reference-identifier reference)))
        (index-node-references-export-to-other-node x-param)))
    (test-assert "x usage in clause1 body resolves"
      (find 
        (lambda (reference) (eq? 'x (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'x (annotation-stripped-expression node)))
            body1)
          'x)))
    (test-assert "y usage in clause2 body resolves"
      (find 
        (lambda (reference) (eq? 'y (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'y (annotation-stripped-expression node)))
            body2)
          'y)))
    (test-assert "z usage in clause2 body resolves"
      (find 
        (lambda (reference) (eq? 'z (identifier-reference-identifier reference)))
        (find-available-references-for document
          (find-index-node-recursive
            (lambda (node) (eq? 'z (annotation-stripped-expression node)))
            body2)
          'z))))
(test-end)

(test-begin "case-lambda-process reports duplicate formals in a clause")
  (let* ([src "(case-lambda ((x x) x))\n"]
      [path "/tmp/test-case-lambda-duplicate.ss"]
      [_ (write-temp-file path src)]
      [document (make-document (string-append "file://" path) src '())]
      [case-lambda-node (init-index-node '() (car (source-file->annotations src path)))])
    (case-lambda-process '() '() document case-lambda-node)
    (test-assert "duplicate formal produces a diagnosis"
      (not (null? (document-diagnoses document)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
