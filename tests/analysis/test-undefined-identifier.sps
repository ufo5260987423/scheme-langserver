#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (srfi :13 strings))

(test-begin "undefined-identifier diagnostic")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/undefined-identifier")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       [root-file-node (workspace-file-node workspace)]
       [main-path (string-append fixture "/main.scm.txt")]
       [main-file-node (walk-file root-file-node main-path)]
       [main-document (file-node-document main-file-node)])

  ;; There should be diagnoses for undefined-function and undefined-in-let
  (test-assert "main document has diagnoses"
    (not (null? (document-diagnoses main-document))))

  ;; Check for undefined-function diagnosis
  (test-assert "has undefined-function diagnosis"
    (find
      (lambda (d)
        (and (string-contains (cadddr d) "Undefined identifier: undefined-function")
             (equal? (list-ref d 4) "identifier")
             (equal? (list-ref d 5) "undefined-identifier")))
      (document-diagnoses main-document)))

  ;; Check for undefined-in-let diagnosis
  (test-assert "has undefined-in-let diagnosis"
    (find
      (lambda (d)
        (and (string-contains (cadddr d) "Undefined identifier: undefined-in-let")
             (equal? (list-ref d 4) "identifier")
             (equal? (list-ref d 5) "undefined-identifier")))
      (document-diagnoses main-document)))

  ;; undefined-in-quote should NOT produce a diagnosis (inside quote)
  (test-assert "no diagnosis for undefined-in-quote"
    (not (find
           (lambda (d)
             (string-contains (cadddr d) "undefined-in-quote"))
           (document-diagnoses main-document)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
