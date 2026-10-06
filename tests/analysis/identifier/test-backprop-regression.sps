#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
#!r6rs

;; Regression pins for the TODO-15 back-propagation fixes:
;; 1. anchor dedup (401215a): the pattern variable's use site must see
;;    ONE variable reference, not the ~23 duplicate copies that used to
;;    accumulate at the macro call node.
;; 2. named let: the loop name is declared by let-process and the
;;    recursive call's use site resolves to the binding node (go-to-def
;;    target). No test covered named-let before 2026-10-06.

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(test-begin "back-propagation anchor dedup (TODO-15)")
(let* ((fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve"))
    (ws (init-workspace fixture 'txt 'r6rs #f #f))
    (root (workspace-file-node ws))
    (doc (file-node-document (walk-file root (string-append fixture "/consumer.scm.txt"))))
    (call-node (find-index-node-recursive
                 (lambda (n) (let ((e (annotation-stripped-expression n)))
                               (and (pair? e) (not (null? e)) (eq? 'match (car e)))))
                 (car (document-index-node-list doc))))
    (clause-node (caddr (index-node-children call-node)))
    (body-use-node (cadr (index-node-children clause-node)))
    (refs (find-available-references-for doc body-use-node 'path)))
  (test-assert "pattern-var use resolves to at least one variable ref"
    (find (lambda (r) (eq? 'variable (identifier-reference-type r))) refs))
  (test-assert "no duplicate-copy pileup (<= 2 candidates)"
    (<= (length refs) 2)))
(test-end)

(test-begin "named let: loop identifier resolves to its binding")
(let* ((fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/named-let"))
    (ws (init-workspace fixture 'txt 'r6rs #f #f))
    (root (workspace-file-node ws))
    (doc (file-node-document (walk-file root (string-append fixture "/lib.scm.txt"))))
    (loops (filter (lambda (n) (eq? 'loop (annotation-stripped-expression n)))
             (let walk ((n (car (document-index-node-list doc))))
               (cons n (apply append (map walk (index-node-children n)))))))
    (binding-node (car loops))
    (use-node (cadr loops))
    (refs (find-available-references-for doc use-node 'loop)))
  (test-assert "use site resolves to exactly the loop binding"
    (and (= (length refs) 1)
      (eq? (identifier-reference-index-node (car refs)) binding-node))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
