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
  (scheme-langserver util association))

(test-begin "exception-capture in threaded-map")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/exception-macro")]
       [workspace (init-workspace fixture 'akku 'r6rs #t #f)]
       [root-file-node (workspace-file-node workspace)]
       [consumer-path (string-append fixture "/consumer.sls")]
       [consumer-file-node (walk-file root-file-node consumer-path)]
       [consumer-document (file-node-document consumer-file-node)])

  ;; After init-workspace, init-references has run (possibly with threaded-map)
  ;; The exception from bad-macro expansion should have been caught
  (test-assert "consumer document has diagnoses"
    (not (null? (document-diagnoses consumer-document))))

  (test-assert "consumer document has analysis-error diagnosis"
    (find 
      (lambda (d)
        (string-contains? (cadddr d) "Analysis error"))
      (document-diagnoses consumer-document)))

  ;; workspace should not be hung: refresh-workspace-for on another file should work
  (let* ([macro-path (string-append fixture "/bad-macro.sls")]
         [macro-file-node (walk-file root-file-node macro-path)]
         [macro-document (file-node-document macro-file-node)])
    (test-assert "macro document also has diagnoses"
      (not (null? (document-diagnoses macro-document))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
