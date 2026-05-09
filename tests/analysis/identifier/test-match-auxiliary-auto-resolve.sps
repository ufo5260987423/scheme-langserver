#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test whether match pattern variables get references
;; when workspace initializes with ufo-match source in fixture.
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util path)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace))

(define (find-match-call root-index-node)
  (let search ([nodes (index-node-children root-index-node)])
    (if (null? nodes)
      #f
      (let* ([node (car nodes)]
             [expr (annotation-stripped (index-node-datum/annotations node))])
        (if (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))
          node
          (let inner ([children (index-node-children node)])
            (if (null? children)
              (search (cdr nodes))
              (let* ([c (car children)]
                     [cexpr (annotation-stripped (index-node-datum/annotations c))])
                (if (and (list? cexpr) (not (null? cexpr)) (eq? 'match (car cexpr)))
                  c
                  (inner (cdr children)))))))))))

;; Navigate to the 'path' node inside (? string? path)
;; match-call -> clause [(? string? path) path]
;;           -> pattern (? string? path)
;;           -> path
(define (find-path-node match-call-node)
  (let* ([clauses (cddr (index-node-children match-call-node))]
         [first-clause (if (null? clauses) #f (car clauses))])
    (if first-clause
      (let* ([pattern-node (car (index-node-children first-clause))]
             [pattern-children (index-node-children pattern-node)])
        (if (>= (length pattern-children) 3)
          (caddr pattern-children)
          #f))
      #f)))

(test-begin "match pattern variable references with ufo-match source in fixture")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       [root-file-node (workspace-file-node workspace)]
       [document (file-node-document (walk-file root-file-node (string-append fixture "/consumer.scm.txt")))]
       [root-index-node (car (document-index-node-list document))]
       [match-call-node (find-match-call root-index-node)])

  (test-assert "match call node found" match-call-node)

  (when match-call-node
    (let ([path-node (find-path-node match-call-node)])
      (test-assert "path pattern node found" path-node)

      (when path-node
        (let ([refs (index-node-references-export-to-other-node path-node)])
          
          (test-assert "path pattern node has exported references" 
            (not (null? refs)))
          
          (test-assert "deep pattern variable 'path appears in path-node refs"
            (find (lambda (r) (eq? 'path (identifier-reference-identifier r))) refs)))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
