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
   [syntax-expander (identifier-reference-syntax-expander match-reference)])

  (display "==== TRIGGERING AUTO-RESOLVE FOR MATCH ====\n\n")
  (let ([rule (expansion-generator->rule syntax-expander step file-linkage '() '())])
    (rule root-file-node root-library-node document match-call-node))
  (display "\n==== DONE ====\n"))

(exit 0)
