#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
(import 
  (chezscheme)
  (scheme-langserver util text)
  (scheme-langserver util path)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver protocol alist-access-object)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis identifier self-defined-rules ufo-match match)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(let* ([workspace-instance (init-workspace (current-directory))]
   [root-file-node (workspace-file-node workspace-instance)]
   [root-library-node (workspace-library-node workspace-instance)]
   [file-linkage (workspace-file-linkage workspace-instance)]
   [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/load.sls"))]
   [document (file-node-document target-file-node)]
   [root-index-node (car (document-index-node-list document))]
   [match-call-node (find-index-node-recursive
      (lambda (n)
        (let ([expr (annotation-stripped-expression n)])
          (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
      root-index-node)]
   [match-reference (car (find-available-references-for document match-call-node 'match))]
   [syntax-expander (identifier-reference-syntax-expander match-reference)]
   [clause-node (caddr (index-node-children match-call-node))]
   [pattern-node (car (index-node-children clause-node))]
   [guard-node (cadr (index-node-children pattern-node))]
   [path-node (caddr (index-node-children guard-node))])

  (display "match-call-node expr: ")
  (write (annotation-stripped-expression match-call-node))
  (newline)
  (display "path-node expr: ")
  (write (annotation-stripped-expression path-node))
  (newline)
  (display "hand-written-exports: ")
  (write (map identifier-reference-identifier (index-node-references-export-to-other-node path-node)))
  (newline)
  (display "match references count: ")
  (write (length (find-available-references-for document match-call-node 'match)))
  (newline)
  (display "match references: ")
  (write (map (lambda (r) (list (identifier-reference-identifier r) (identifier-reference-library-identifier r))) (find-available-references-for document match-call-node 'match)))
  (newline))

(exit 0)
