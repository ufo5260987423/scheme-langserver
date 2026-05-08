#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util path)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(define (find-in-nodes predicate nodes)
  (let loop ([rest nodes])
    (if (null? rest)
      #f
      (let ([result (find-index-node-recursive predicate (car rest))])
        (if result result (loop (cdr rest)))))))

(test-begin "shallow-copy cascade propagation")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/cascade-macro")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       [root-file-node (workspace-file-node workspace)]
       [root-library-node (workspace-library-node workspace)]
       [file-linkage (workspace-file-linkage workspace)]
       [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))])
  (test-assert "file found" (not (eq? #f target-file-node)))
  (when target-file-node
    (let* ([document (file-node-document target-file-node)]
           [nodes (document-index-node-list document)]
           [call-node (find-in-nodes
                        (lambda (n)
                          (let ([expr (annotation-stripped-expression n)])
                            (and (list? expr) (not (null? expr)) (eq? 'macro-a (car expr)))))
                        nodes)])
      (test-assert "call-node found" (not (eq? #f call-node)))
      (when call-node
        ; Manually trigger macro-a expansion
        (let* ([macro-a-ref (car (find-available-references-for document call-node 'macro-a))]
               [macro-a-expander (identifier-reference-syntax-expander macro-a-ref)]
               [macro-a-rule (expansion-generator->rule macro-a-expander step file-linkage '() '())])
          (macro-a-rule root-file-node root-library-node document call-node))
        ; Now find z in the expanded tree
        (let ([z-node (find-index-node-recursive
                        (lambda (n) (eq? 'z (annotation-stripped-expression n)))
                        call-node)])
          (test-assert "z-node found after macro-a expansion" (not (eq? #f z-node)))
          (when z-node
            (let ([z-exports (index-node-references-export-to-other-node z-node)])
              (test-equal "z node has let-binding reference after cascade"
                '(z)
                (map identifier-reference-identifier z-exports)))))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
