#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta))

(define fixture 
  (string-append (current-directory) "/tests/resources/workspace-fixtures/fluent-scheme"))

(define (find-reference document index-node identifier)
  (find 
    (lambda (reference) 
      (eq? (identifier-reference-identifier reference) identifier))
    (find-available-references-for document index-node)))

; Find the index-node of a leaf occurrence of `identifier` under any of
; the given root index-nodes.
(define (find-leaf roots identifier)
  (let loop ([node (car roots)]
             [rest (cdr roots)])
    (cond
      [(and (index-node? node)
          (symbol? (annotation-stripped (index-node-datum/annotations node)))
          (eq? (annotation-stripped (index-node-datum/annotations node)) identifier))
        node]
      [(index-node? node)
        (or (let inner ([children (index-node-children node)])
              (cond
                [(null? children) #f]
                [(loop (car children) '()) => (lambda (hit) hit)]
                [else (inner (cdr children))]))
            (if (null? rest) #f (loop (car rest) (cdr rest))))]
      [else (if (null? rest) #f (loop (car rest) (cdr rest)))])))

(test-begin "init-workspace-basic-test-fluent")
  (let* ([workspace (init-workspace fixture 'txt 'fluent #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [root-library-node (workspace-library-node workspace)])
    (test-equal #f (null? root-file-node))
    (test-equal #f (null? root-library-node)))
(test-end)

(test-begin "fluent-meta-resolution-test")
  (let* ([workspace (init-workspace fixture 'txt 'fluent #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-path (string-append fixture "/gui.scm.txt")]
      [file-node (walk-file root-file-node target-path)]
      [document (file-node-document file-node)]
      [roots (document-index-node-list document)]
      ; chezscheme identifier resolves through the merged fluent meta
      [define-node (find-leaf roots 'define)]
      ; fluent-specific identifiers resolve to the (fluent) library
      [create-node (find-leaf roots 'cx-create-panel)]
      [getvar-node (find-leaf roots '%rpgetvar)]
      [setvar-node (find-leaf roots 'rpsetvar)])
    (test-assert (find-reference document define-node 'define))
    (let ([ref (find-reference document create-node 'cx-create-panel)])
      (test-assert ref)
      (test-equal '(fluent) (identifier-reference-library-identifier ref)))
    (let ([ref (find-reference document getvar-node '%rpgetvar)])
      (test-assert ref)
      (test-equal '(fluent) (identifier-reference-library-identifier ref)))
    (test-assert (find-reference document setvar-node 'rpsetvar)))
(test-end)

(test-begin "fluent-user-definition-test")
  (let* ([workspace (init-workspace fixture 'txt 'fluent #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [target-path (string-append fixture "/gui.scm.txt")]
      [file-node (walk-file root-file-node target-path)]
      [document (file-node-document file-node)]
      [roots (document-index-node-list document)]
      [callback-node (find-leaf roots 'apply-cb)])
    ; apply-cb is defined in-file; its reference must not come from meta
    (let ([ref (find-reference document callback-node 'apply-cb)])
      (test-assert ref)
      (test-equal '() (identifier-reference-library-identifier ref))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
