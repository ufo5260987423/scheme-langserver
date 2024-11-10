#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver util text)
  (scheme-langserver util path)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)

  (scheme-langserver protocol alist-access-object)

  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier macro-expander)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis workspace))

(test-begin "expand:step-by-step & generate-pair:template+callee & generate-pair:template+expanded")
  (let* ([workspace-instance (init-workspace (current-directory))]
      [root-file-node (workspace-file-node workspace-instance)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/dependency/rules/library-import.sls"))]
      [document (file-node-document target-file-node)]
      [target-index-node (pick-index-node-from (document-index-node-list document) (text+position->int (document-text document) 37 6))]
      [identifier-reference (car (find-available-references-for document target-index-node 'match))]
      [template+callees (generate-pair:template+callee identifier-reference target-index-node document)]
      [expanded-expression (car (expand:step-by-step identifier-reference target-index-node document))]
      [expanded-index-node 
        (init-index-node 
          (identifier-reference-initialization-index-node identifier-reference) 
          (car 
            (source-file->annotations 
              (with-output-to-string (lambda () (pretty-print expanded-expression)))
              (uri->path (document-uri (identifier-reference-document identifier-reference))))))]
      [template+expanded (generate-pair:template+expanded identifier-reference expanded-index-node target-index-node document template+callees)])
    (test-equal 
      '(let ([v expression])
        (match-next v (expression (set! expression))
          (('only (identifier **1) _ ...) identifier)
          (('except (identifier **1) _ ...) identifier)
          (('prefix (identifier **1) _ ...) identifier)
          (('rename (identifier **1) _ ...) identifier)
          (('for (identifier **1) 'run ...) identifier)
          (('for (identifier **1) '(meta 0) ...) identifier)
          ((identifier **1) identifier) (else '())))
      expanded-expression)
    (test-equal
      '((match . match)
        (atom . expression)
        ((pat ...) ('only (identifier **1) _ ...)
            ('except (identifier **1) _ ...)
            ('prefix (identifier **1) _ ...)
            ('rename (identifier **1) _ ...)
            ('for (identifier **1) 'run ...)
            ('for (identifier **1) '(meta 0) ...) (identifier **1) else)
        ((body ...) identifier identifier identifier identifier identifier identifier identifier '()))
      (map 
        (lambda (p)
          `(,(car p) .
          ,(if (index-node? (cdr p))
            (annotation-stripped (index-node-datum/annotations (cdr p)))
            (map (lambda (px) (annotation-stripped (index-node-datum/annotations px))) (cdr p)))))
        template+callees))

    (test-equal '((atom ...) (pat ...) (body ...)) (map car template+expanded))
  )
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))