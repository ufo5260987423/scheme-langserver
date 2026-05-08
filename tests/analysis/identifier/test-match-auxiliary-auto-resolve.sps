#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test whether match-next/match-one/match-two are auto-resolved
;; when generic auto-resolve is enabled in router.sls
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util path)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
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

(test-begin "match auxiliary macros auto-resolve via generic path")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       [root-file-node (workspace-file-node workspace)]
       [document (file-node-document (walk-file root-file-node (string-append fixture "/consumer.scm.txt")))]
       [root-index-node (car (document-index-node-list document))]
       [match-call-node (find-match-call root-index-node)])

  (test-assert "match call node found" match-call-node)

  (when match-call-node
    (let ([refs (index-node-references-export-to-other-node match-call-node)])
      (test-assert "match call node has exported references" 
        (not (null? refs)))
      
      (display "\n[DEBUG] match call node references:\n")
      (for-each 
        (lambda (r)
          (display "  ") 
          (display (identifier-reference-identifier r))
          (newline))
        refs)
      
      (test-assert "deep pattern variable 'path appears in match call refs"
        (find (lambda (r) (eq? 'path (identifier-reference-identifier r))) refs)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
