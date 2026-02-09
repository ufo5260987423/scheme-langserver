#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme) 
  (srfi :64 testing) 
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver protocol alist-access-object)

  (scheme-langserver util text)

  (scheme-langserver analysis identifier expanders pattern)
  (scheme-langserver analysis workspace))

(test-begin "context:ellipsed?")
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'body1))
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'clause1))
  (test-equal #f (context:ellipsed? (gather-context (make-pattern '(try body0 body1 ... (except condition clause0 clause1 ...)))) 'condition))
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(match atom (pat . body) ...))) 'pat))
  (test-equal #t (context:ellipsed? (gather-context (make-pattern '(match atom (pat . body) ...))) 'body))
  (test-equal #f (context:ellipsed? (gather-context (make-pattern '(match atom (pat . body) ...))) 'atom))
(test-end)

(test-begin "pattern+index-node->pair-list")
    (let* ([workspace (init-workspace (string-append (current-directory) "/analysis/identifier/rules/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/let.sls"))]
            [document (file-node-document target-file-node)]
            [text (document-text document)]
            [index-node-list (document-index-node-list document)]
            [index-node (index-node-parent (pick-index-node-from index-node-list (text+position->int text 22 6)))]
            [expression (annotation-stripped (index-node-datum/annotations index-node))]
            [pattern-expression '(match atom (pat . body) ...)]
            [pattern (make-pattern pattern-expression)]
            [context (gather-context pattern)])
      ; (pretty-print 
      ;   (map 
      ;     (lambda (p) `(,(pattern-content (car p)) . ,(annotation-stripped (index-node-datum/annotations (cdr p)))))
      ;     (pattern+index-node->pair-list pattern index-node)))
      (test-equal 
        (map (lambda (p) `(,(pattern-content (car p)) . ,(annotation-stripped (index-node-datum/annotations (cdr p))))) (pattern+index-node->pair-list pattern index-node))
        '(((match atom [pat . body] ...)
    match
    expression
    ((_ (fuzzy0 **1) fuzzy1 ...)
      (fold-left
        (lambda (exclude-list identifier-parent-index-node)
          (let* ([identifier-index-node (car (index-node-children
                                                identifier-parent-index-node))]
                  [target-identifier-reference (let-parameter-process index-node
                                                identifier-index-node
                                                index-node document type)]
                  [extended-exclude-list (append
                                          exclude-list
                                          target-identifier-reference)])
            (index-node-excluded-references-set!
              (cadr (index-node-children index-node))
              extended-exclude-list)
            extended-exclude-list))
        '()
        (filter
          (lambda (i) (not (null? (index-node-children i))))
          (index-node-children
            (cadr (index-node-children index-node))))))
    (else '())) (match . match) (atom . expression)
    ((pat . body)
      (_ (fuzzy0 **1) fuzzy1 ...)
      (fold-left
        (lambda (exclude-list identifier-parent-index-node)
          (let* ([identifier-index-node (car (index-node-children
                                              identifier-parent-index-node))]
                [target-identifier-reference (let-parameter-process index-node
                                                identifier-index-node
                                                index-node document type)]
                [extended-exclude-list (append
                                          exclude-list
                                          target-identifier-reference)])
            (index-node-excluded-references-set!
              (cadr (index-node-children index-node))
              extended-exclude-list)
            extended-exclude-list))
        '()
        (filter
          (lambda (i) (not (null? (index-node-children i))))
          (index-node-children
            (cadr (index-node-children index-node))))))
    (pat _ (fuzzy0 **1) fuzzy1 ...)
    (body
      fold-left
      (lambda (exclude-list identifier-parent-index-node)
        (let* ([identifier-index-node (car (index-node-children
                                            identifier-parent-index-node))]
              [target-identifier-reference (let-parameter-process index-node
                                              identifier-index-node
                                              index-node document type)]
              [extended-exclude-list (append
                                        exclude-list
                                        target-identifier-reference)])
          (index-node-excluded-references-set!
            (cadr (index-node-children index-node))
            extended-exclude-list)
          extended-exclude-list))
      '()
      (filter
        (lambda (i) (not (null? (index-node-children i))))
        (index-node-children
          (cadr (index-node-children index-node)))))
    ((pat . body) else '()) 
    (pat . else) 
    (body quote ()))))
(test-end)

(test-begin "pattern+context->pairs->iterator")
    (let* ([workspace (init-workspace (string-append (current-directory) "/analysis/identifier/rules/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/let.sls"))]
            [document (file-node-document target-file-node)]
            [text (document-text document)]
            [index-node-list (document-index-node-list document)]
            [index-node (index-node-parent (pick-index-node-from index-node-list (text+position->int text 22 6)))]
            [expression (annotation-stripped (index-node-datum/annotations index-node))]
            [pattern-expression '(match atom (pat . body) ...)]
            [pattern (make-pattern pattern-expression)]
            [context (gather-context pattern)]
            [pairs (pattern+index-node->pair-list pattern index-node)]
            [iterator ((pattern+context->pairs->iterator 'pat context) pairs)])
      (test-equal '(dive-into-an-ellipsed-form . 1) (iterator))
      ;it can repeat
      (test-equal '(dive-into-an-ellipsed-form . 1) (iterator 'repeat))
      (test-equal '(_ (fuzzy0 **1) fuzzy1 ...) (annotation-stripped (index-node-datum/annotations (iterator))))
      (test-equal 'escape-from-target-form (iterator))
      (test-equal '(dive-into-an-ellipsed-form . 1) (iterator))
      (test-equal 'else (annotation-stripped (index-node-datum/annotations (iterator))))
      (test-equal 'escape-from-target-form (iterator))
      (test-equal 'stop-iteration (iterator)))
(test-end)

(test-begin "generate-binding")
    (let* ([workspace (init-workspace (string-append (current-directory) "/analysis/identifier/rules/") '() #f #f)]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/let.sls"))]
            [document (file-node-document target-file-node)]
            [text (document-text document)]
            [index-node-list (document-index-node-list document)]
            [index-node (index-node-parent (pick-index-node-from index-node-list (text+position->int text 22 6)))]
            [expression (annotation-stripped (index-node-datum/annotations index-node))]
            [pattern-expression '(match atom (pat . body) ...)]
            [pattern (make-pattern pattern-expression)]
            [context (gather-context pattern)]
            [pairs (pattern+index-node->pair-list pattern index-node)]
            [iterator ((pattern+context->pairs->iterator 'pat context) pairs)]
            [binding (generate-binding 'pat iterator)])
      (test-equal 
        '((_ (fuzzy0 **1) fuzzy1 ...) else)
        (map (lambda (i) (annotation-stripped (index-node-datum/annotations i))) (cdr binding))))
(test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
