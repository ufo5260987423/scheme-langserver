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
  (scheme-langserver protocol apis document-symbol)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (scheme-langserver util test))

(test-begin "document-symbol on util/binary-search.sls")

(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [target-path (string-append root "/util/binary-search.sls")]
       [uri (path->uri target-path)]

       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))]

       ;; derive expected names and positions from AST
       [bs-def (find-define-by-name root-node 'binary-search)]
       [pc-def (find-define-by-name root-node 'private-collect)]
       [bs-name (define-node->name-node bs-def)]
       [pc-name (define-node->name-node pc-def)]
       [bs-name-start (document+bias->position-list document (index-node-start bs-name))]
       [bs-name-end (document+bias->position-list document (index-node-end bs-name))]
       [pc-name-start (document+bias->position-list document (index-node-start pc-name))]
       [pc-name-end (document+bias->position-list document (index-node-end pc-name))]

       [params (make-alist 'textDocument (make-alist 'uri uri))]
       [result (document-symbol workspace params)])

  (test-assert "returns a vector" (vector? result))
  (test-equal "has 2 symbols" 2 (vector-length result))

  ;; correctness: each returned symbol's range must match the AST name-node position
  (let ([names (map (lambda (s) (assq-ref s 'name)) (vector->list result))])
    (test-assert "contains binary-search"
      (find (lambda (n) (string=? "binary-search" n)) names))
    (test-assert "contains private-collect"
      (find (lambda (n) (string=? "private-collect" n)) names))

    (for-each
      (lambda (sym)
        (let ([name (assq-ref sym 'name)]
              [range (assq-ref sym 'range)])
          (test-assert (string-append name " has kind") (assq-ref sym 'kind))
          (test-assert (string-append name " has range") range)
          (test-assert (string-append name " has selectionRange") (assq-ref sym 'selectionRange))

          ;; verify range correctness against AST-derived positions
          (cond
            [(string=? name "binary-search")
             (test-equal "binary-search start line" (car bs-name-start) (assq-ref (assq-ref range 'start) 'line))
             (test-equal "binary-search start char" (cadr bs-name-start) (assq-ref (assq-ref range 'start) 'character))
             (test-equal "binary-search end line" (car bs-name-end) (assq-ref (assq-ref range 'end) 'line))
             (test-equal "binary-search end char" (cadr bs-name-end) (assq-ref (assq-ref range 'end) 'character))]
            [(string=? name "private-collect")
             (test-equal "private-collect start line" (car pc-name-start) (assq-ref (assq-ref range 'start) 'line))
             (test-equal "private-collect start char" (cadr pc-name-start) (assq-ref (assq-ref range 'start) 'character))
             (test-equal "private-collect end line" (car pc-name-end) (assq-ref (assq-ref range 'end) 'line))
             (test-equal "private-collect end char" (cadr pc-name-end) (assq-ref (assq-ref range 'end) 'character))])))
      (vector->list result))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
