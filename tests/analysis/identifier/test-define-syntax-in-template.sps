#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test: auto-resolve can handle define-syntax inside a template.
;; The macro expands to a let body that defines a local macro via define-syntax
;; and then calls it.
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
  (scheme-langserver analysis identifier expanders pattern)
  (scheme-langserver analysis identifier expanders syntax-rules)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(define (make-call-index-node parent expression)
  (let* ([str (with-output-to-string (lambda () (pretty-print expression)))]
         [annotations (source-file->annotations str "/dev/null")]
         [node (init-index-node parent (car annotations))])
    node))

(define (node-expr node)
  (if (index-node? node)
    (annotation-stripped (index-node-datum/annotations node))
    node))

(test-begin "define-syntax in template auto-resolve")

; Create a simple macro whose template contains define-syntax.
; (define-with-local-macro x)
; => (let () (define-syntax double-it (syntax-rules () ((_ y) (+ y y)))) (double-it x))
(let* ([workspace (init-workspace (current-directory))]
       [root-file-node (workspace-file-node workspace)]
       [root-library-node (workspace-library-node workspace)]
       ; Build a fake document containing the macro definition
       [macro-source 
         "(library (test-define-syntax-in-template) \
            (export define-with-local-macro) \
            (import (chezscheme)) \
            (define-syntax define-with-local-macro \
              (syntax-rules () \
                ((_ x) \
                 (let () \
                   (define-syntax double-it \
                     (syntax-rules () \
                       ((_ y) (+ y y)))) \
                   (double-it x))))))"]
       [tmp-path (begin
                   (with-output-to-file "/tmp/test-define-syntax.sls"
                     (lambda () (display macro-source))
                     'replace)
                   "/tmp/test-define-syntax.sls")]
       [annotations (source-file->annotations macro-source tmp-path)]
       [doc (make-document "file:///tmp/test-define-syntax.sls" macro-source annotations)]
       [_ (document-index-node-list-set! doc (map (lambda (item) (init-index-node '() item)) annotations))]
       ; Run step to init the document
       [_ (step root-file-node root-library-node (workspace-file-linkage workspace) doc)]
       ; Find the macro generator
       [def-syntax-node (find-index-node-recursive
                          (lambda (n)
                            (let ([expr (annotation-stripped-expression n)])
                              (and (list? expr) (eq? 'define-syntax (car expr))
                                   (list? (cdr expr)) (eq? 'define-with-local-macro (cadr expr)))))
                          (car (document-index-node-list doc)))]
       [syntax-rules-node (car (reverse (index-node-children def-syntax-node)))]
       [generator (index-node-expansion-generator syntax-rules-node)])

  (test-assert "generator exists (nested-macro guard did not reject)"
    (procedure? generator))

  ; Now try to expand a call
  (let* ([call-node (make-call-index-node #f '(define-with-local-macro 5))]
         [result (generator root-file-node root-library-node doc call-node)]
         [expansion (if result (cdr result) #f)])
    (test-assert "expansion produced a result" (not (equal? result #f)))
    (when expansion
      (test-assert "expansion contains double-it call"
        (let tree-contains? ([t (node-expr expansion)] [target 'double-it])
          (cond [(equal? t target) #t]
                [(null? t) #f]
                [(pair? t) (or (tree-contains? (car t) target) (tree-contains? (cdr t) target))]
                [else #f]))))
    ; Now run step on the expansion to see if double-it gets resolved
    (when expansion
      (let ([_ (step root-file-node root-library-node (workspace-file-linkage workspace) doc expansion '() '())])
        ; Look for double-it reference in the expansion
        (test-assert "double-it identifier was resolved in expansion"
          (let ([found (find-index-node-recursive
                         (lambda (n) (eq? 'double-it (node-expr n)))
                         expansion)])
            (and found
                 (not (null? (index-node-references-import-in-this-node found))))))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
