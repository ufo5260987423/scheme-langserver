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
(let* ([root (string-append (current-directory) "/util/")]
       [workspace (init-workspace root 'scheme 'r6rs #f #f)]
       [target-path (string-append root "binary-search.sls")]
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
(let* ([root (string-append (current-directory) "/util/")]
       [workspace (init-workspace root 'scheme 'r6rs #f #t)]
       [target-path (string-append root "association.sls")]
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

(test-begin "hover on unresolved uri returns null")
(let* ([workspace (init-workspace (string-append (current-directory) "/util/") 'scheme 'r6rs #f #f)]
       [params (make-alist
                 'textDocument (make-alist 'uri "file:///nonexistent/ignored.sls")
                 'position (make-alist 'line 0 'character 0))]
       [result (hover workspace params)])
  (test-equal "unresolved uri returns null" 'null result))
(test-end)

(test-begin "hover occurrence narrowing (route-4 engine)")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/narrow-demo")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #t)]
       [target-path (string-append fixture "/lib.scm.txt")]
       [uri (path->uri target-path)]
       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))])
  (define (collect-sym node sym acc)
    (let ([e (annotation-stripped (index-node-datum/annotations node))])
      (fold-left (lambda (a c) (collect-sym c sym a))
        (if (eq? e sym) (cons node acc) acc)
        (index-node-children node))))
  (define (hover-strings node)
    (let* ([pos (document+bias->position-list document (index-node-start node))]
        [params (make-alist 'textDocument (make-alist 'uri uri)
                  'position (make-alist 'line (car pos) 'character (cadr pos)))]
        [result (hover workspace params)])
      (filter string? (vector->list (assq-ref result 'contents)))))
  ;v occurrences in source order: binding, test, then-branch, else-branch
  (let* ([vs (reverse (collect-sym root-node 'v '()))]
      [then-v (list-ref vs 2)]
      [else-v (list-ref vs 3)]
      [then-strings (hover-strings then-v)]
      [else-strings (hover-strings else-v)])
    (test-assert "then-branch narrows the union to fixnum?"
      (find (lambda (s) (string-contains? s "Narrowed")) then-strings))
    (test-assert "then-branch shows fixnum?"
      (find (lambda (s) (string-contains? s "fixnum?")) then-strings))
    (test-assert "else-branch narrows the union to string?"
      (find (lambda (s) (string-contains? s "Narrowed")) else-strings))
    (test-assert "else-branch shows string?"
      (find (lambda (s) (string-contains? s "string?")) else-strings))
    (test-assert "narrowing mentions the predicate"
      (find (lambda (s) (string-contains? s "number?")) then-strings))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
