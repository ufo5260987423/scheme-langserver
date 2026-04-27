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
  (scheme-langserver protocol apis document-symbol)
  (scheme-langserver util path)
  (scheme-langserver util association))

(test-begin "document-symbol on util/binary-search.sls")

(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [target-path (string-append root "/util/binary-search.sls")]
       [uri (path->uri target-path)]
       [params (make-alist 'textDocument (make-alist 'uri uri))]
       [result (document-symbol workspace params)])

  (test-assert "returns a vector" (vector? result))
  (test-assert "non-empty" (> (vector-length result) 0))

  (let ([names (map (lambda (s) (assq-ref s 'name)) (vector->list result))])
    (test-assert "contains exported symbol 'binary-search'"
      (find (lambda (n) (string=? "binary-search" n)) names))
    (test-assert "contains internal symbol 'private-collect'"
      (find (lambda (n) (string=? "private-collect" n)) names))

    (for-each
      (lambda (sym)
        (let ([name (assq-ref sym 'name)])
          (test-assert (string-append name " has kind") (assq-ref sym 'kind))
          (test-assert (string-append name " has range") (assq-ref sym 'range))
          (test-assert (string-append name " has selectionRange") (assq-ref sym 'selectionRange))))
      (vector->list result))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
