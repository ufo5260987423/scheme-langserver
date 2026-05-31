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
  (scheme-langserver protocol apis definition)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (scheme-langserver util test))

(test-begin "definition on util/binary-search.sls")

(let* ([root (current-directory)]
    [workspace (init-workspace root 'akku 'r6rs #f #f)]
    [target-path (string-append root "/util/binary-search.sls")]
    [uri (path->uri target-path)]

    [file-node (walk-file (workspace-file-node workspace) target-path)]
    [document (file-node-document file-node)]
    [root-node (car (document-index-node-list document))]

    ;; locate private-collect definition in AST
    [def-node (find-define-by-name root-node 'private-collect)]
    [name-node (define-node->name-node def-node)]

    ;; derive cursor position from the name node (no hard-coding)
    [cursor-pos (document+bias->position-list document (index-node-start name-node))]
    [cursor-line (car cursor-pos)]
    [cursor-char (cadr cursor-pos)]

    ;; definition returns the identifier's precise range, not the whole define form
    [expected-start (document+bias->position-list document (index-node-start name-node))]
    [expected-end (document+bias->position-list document (index-node-end name-node))]

    [params (make-alist
              'textDocument (make-alist 'uri uri)
              'position (make-alist 'line cursor-line 'character cursor-char))]
    [result (definition workspace params)]
    [loc (vector-ref result 0)]
    [range (assq-ref loc 'range)]
    [start-pos (assq-ref range 'start)]
    [end-pos (assq-ref range 'end)])

  (test-assert "returns a vector" (vector? result))
  (test-assert "non-empty" (> (vector-length result) 0))

  ;; correctness: the returned location must match the AST position of the identifier name
  (test-equal "definition uri points to same file" uri (assq-ref loc 'uri))
  (test-equal "definition start line" (car expected-start) (assq-ref start-pos 'line))
  (test-equal "definition start character" (cadr expected-start) (assq-ref start-pos 'character))
  (test-equal "definition end line" (car expected-end) (assq-ref end-pos 'line))
  (test-equal "definition end character" (cadr expected-end) (assq-ref end-pos 'character)))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
