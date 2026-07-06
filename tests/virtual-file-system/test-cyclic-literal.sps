#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver analysis tokenizer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-shared-pairs root)
  (let ([pairs '()])
    (let loop ([node root])
      (when (index-node-shared-reference node)
        (set! pairs (cons (cons (index-node-shared-reference node) node) pairs)))
      (for-each loop (index-node-children node)))
    pairs))

(define (load-root-index-node filename)
  (let ([path (string-append (current-directory) "/tests/resources/workspace-fixtures/cyclic-literal/" filename)])
    (init-index-node '() (car (source-file->annotations path)))))

(define (private:count-unique defs)
  (let loop ([defs defs] [count 0])
    (if (null? defs)
      count
      (loop (cdr defs)
        (+ count (if (memq (car defs) (cdr defs)) 0 1))))))

(define (run-shared-structure-tests label root expected-definition-count expected-reference-count)
  (let ([pairs (collect-shared-pairs root)])
    (test-equal (string-append label " — definition count")
      expected-definition-count
      (private:count-unique (map car pairs)))
    (test-equal (string-append label " — reference count")
      expected-reference-count
      (length pairs))
    (for-each
      (lambda (pair)
        (let ([definition-node (car pair)]
              [reference-node (cdr pair)])
          (test-assert (string-append label " — reference is leaf")
            (null? (index-node-children reference-node)))
          (test-assert (string-append label " — shared-reference points to definition")
            (eq? (index-node-shared-reference reference-node) definition-node))
          (test-assert (string-append label " — reference is distinct from definition")
            (not (eq? reference-node definition-node)))))
      pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-index-node handles dotted cyclic pair")
  (let ([root (load-root-index-node "lib.scm.txt")])
    (run-shared-structure-tests "dotted cyclic pair" root 1 1)
    (let ([pair (car (collect-shared-pairs root))])
      (test-equal "definition node has 2 children" 2 (length (index-node-children (car pair))))))
(test-end)

(test-begin "init-index-node handles vector with shared element")
  (let ([root (load-root-index-node "vector-shared.scm.txt")])
    (run-shared-structure-tests "vector shared element" root 1 1))
(test-end)

(test-begin "init-index-node handles proper cyclic list")
  (let ([root (load-root-index-node "proper-cyclic.scm.txt")])
    (run-shared-structure-tests "proper cyclic list" root 1 1))
(test-end)

(test-begin "init-index-node handles nested shared references")
  (let ([root (load-root-index-node "nested-shared.scm.txt")])
    (run-shared-structure-tests "nested shared references" root 2 2))
(test-end)

(test-begin "init-index-node handles multiple references to one definition")
  (let ([root (load-root-index-node "multiple-refs.scm.txt")])
    (run-shared-structure-tests "multiple references" root 1 2))
(test-end)

(test-begin "init-index-node handles multiple distinct shared definitions")
  (let ([root (load-root-index-node "multiple-defs.scm.txt")])
    (run-shared-structure-tests "multiple definitions" root 2 2))
(test-end)

(test-begin "shared-reference predicates are safe")
  (let ([root (load-root-index-node "lib.scm.txt")])
    (let ([reference-node (cdar (collect-shared-pairs root))])
      (test-assert "quote? returns #f on reference node"
        (not (quote? reference-node)))
      (test-assert "syntax? returns #f on reference node"
        (not (syntax? reference-node)))
      (test-assert "quasiquote? returns #f on reference node"
        (not (quasiquote? reference-node)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
