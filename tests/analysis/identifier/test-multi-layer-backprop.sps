#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

;; TODO-10 phase-0 characterization: multi-layer back-propagation.
;;
;; A three-level relay macro family (my-let3 -> my-let2 -> my-let1 -> let)
;; drives the full cascade through the eager dispatch arm. Assertions:
;;
;;   exports:  the let-bound declaration must surface on the ORIGINAL
;;             call-site binding node through all three relay layers
;;             (currently PASSES -- the export side of back-propagation
;;             is multi-layer capable);
;;   imports:  the body use site must resolve to that binding
;;             (PASSES since 4afd834 + the call-node fallback: the
;;             import-bearing skeleton root has no node-level pair --
;;             the pairs walk only covers index-node compounds and
;;             child zips -- so the import scope falls back to the
;;             macro call node, exactly the expansion's scope).

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(test-begin "multi-layer back-propagation characterization")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/cascade-relay")]
    [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [document (file-node-document
        (walk-file (workspace-file-node workspace) (string-append fixture "/consumer.scm.txt")))]
    [call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'my-let3 (car expr)))))
        (car (document-index-node-list document)))]
    [kids (index-node-children call-node)]
    [binding-a (car (index-node-children (car (index-node-children (cadr kids)))))]
    [body-a (car (reverse kids))]
    [exports (index-node-references-export-to-other-node binding-a)])

  (test-assert "export arrives on the original binding node through 3 relay layers"
    (and (not (null? exports))
      (eq? 'a (identifier-reference-identifier (car exports)))
      (identifier-reference-speculative? (car exports))))

  (test-assert "export anchors at the macro call (the expansion origin)"
    (and (not (null? exports))
      (= (index-node-start call-node)
        (index-node-start (identifier-reference-initialization-index-node (car exports))))))

  (test-assert "use-site resolution through the cascade"
    (not (null? (find-available-references-for document body-a 'a)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
