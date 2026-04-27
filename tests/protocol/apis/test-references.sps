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
  (scheme-langserver protocol apis references)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (scheme-langserver util test))

(test-begin "references on util/binary-search.sls")

(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [target-path (string-append root "/util/binary-search.sls")]
       [uri (path->uri target-path)]

       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))]

       ;; cursor on private-collect definition name
       [def-node (find-define-by-name root-node 'private-collect)]
       [name-node (define-node->name-node def-node)]
       [cursor-pos (document+bias->position-list document (index-node-start name-node))]
       [cursor-line (car cursor-pos)]
       [cursor-char (cadr cursor-pos)]

       ;; collect all (private-collect ...) call nodes from AST
       [call-nodes
         (let collect ([node root-node])
           (let ([expr (annotation-stripped-expression node)])
             (append
               (if (and (list? expr) (not (null? expr)) (eq? 'private-collect (car expr)))
                   (list node)
                   '())
               (apply append (map collect (index-node-children node))))))]

       ;; the first child of each call node is the function name node
       [name-children (map (lambda (n) (car (index-node-children n))) call-nodes)]

       ;; derive expected positions from AST
       [expected-positions
         (map (lambda (child)
                (list
                  (document+bias->position-list document (index-node-start child))
                  (document+bias->position-list document (index-node-end child))))
              name-children)]

       [params (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line cursor-line 'character cursor-char))]
       [result (find-references workspace params)])

  (test-assert "returns a vector" (vector? result))
  (test-equal "returns expected count" (length expected-positions) (vector-length result))

  ;; correctness: every returned location must match an expected call-site position
  (for-each
    (lambda (loc)
      (let* ([range (assq-ref loc 'range)]
             [start (list (assq-ref (assq-ref range 'start) 'line)
                          (assq-ref (assq-ref range 'start) 'character))]
             [end (list (assq-ref (assq-ref range 'end) 'line)
                        (assq-ref (assq-ref range 'end) 'character))]
             [match (find
                      (lambda (exp)
                        (and (equal? start (car exp))
                             (equal? end (cadr exp))))
                      expected-positions)])
        (test-equal "uri points to binary-search.sls" uri (assq-ref loc 'uri))
        (test-assert
          (string-append "location at " (number->string (car start)) ":" (number->string (cadr start)) " matches expected call site")
          match)))
    (vector->list result)))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
