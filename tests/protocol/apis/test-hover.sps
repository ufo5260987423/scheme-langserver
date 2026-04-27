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
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver protocol apis hover)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (scheme-langserver util test))

(define (string-contains? s substr)
  (let ([len (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i len) (string-length s)) #f]
        [(string=? (substring s i (+ i len)) substr) #t]
        [else (loop (+ i 1))]))))

(test-begin "hover without type inference")
(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [target-path (string-append root "/util/binary-search.sls")]
       [uri (path->uri target-path)]
       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))]
       [def-node (find-define-by-name root-node 'binary-search)]
       [name-node (define-node->name-node def-node)]
       [cursor-pos (document+bias->position-list document (index-node-start name-node))]
       [cursor-line (car cursor-pos)]
       [cursor-char (cadr cursor-pos)]
       [params (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line cursor-line 'character cursor-char))]
       [result (hover workspace params)]
       [contents (assq-ref result 'contents)])
  (test-assert "result is alist" (list? result))
  (test-assert "contents is a vector" (vector? contents))
  (test-assert "contents non-empty" (> (vector-length contents) 0))
  (test-assert "contents mention binary-search"
    (find (lambda (s) (and (string? s) (string-contains? s "binary-search"))) (vector->list contents))))
(test-end)

(test-begin "hover with type inference")
(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #t)]
       [target-path (string-append root "/util/association.sls")]
       [uri (path->uri target-path)]
       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))]
       [def-node (find-define-by-name root-node 'assq-ref)]
       [name-node (define-node->name-node def-node)]
       [cursor-pos (document+bias->position-list document (index-node-start name-node))]
       [cursor-line (car cursor-pos)]
       [cursor-char (cadr cursor-pos)]
       [params (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line cursor-line 'character cursor-char))]
       [result (hover workspace params)]
       [contents (assq-ref result 'contents)])
  (test-assert "result is alist" (list? result))
  (test-assert "contents is a vector" (vector? contents))
  (test-assert "contents non-empty" (> (vector-length contents) 0))
  (test-assert "contents mention assq-ref"
    (find (lambda (s) (and (string? s) (string-contains? s "assq-ref"))) (vector->list contents)))
  ;; when type inference is enabled, expect a type-inference section
  (test-assert "contents include type inference header"
    (find (lambda (s) (and (string? s) (string-contains? s "Type Inference"))) (vector->list contents))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
