#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Trace pairs mapping for match expansion to understand shallow-copy layer limit
(import 
  (chezscheme)
  (scheme-langserver util path)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders pattern)
  (scheme-langserver analysis identifier expanders syntax-rules)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

(define (get-macro-generator document macro-name)
  (let loop ([nodes (document-index-node-list document)])
    (if (null? nodes)
        #f
        (let ([result (find-index-node-recursive
                        (lambda (n)
                          (let ([expr (annotation-stripped-expression n)])
                            (and (list? expr) (not (null? expr)) (eq? 'define-syntax (car expr))
                                 (list? (cdr expr)) (not (null? (cdr expr))) (eq? macro-name (cadr expr)))))
                        (car nodes))])
          (if result
              (let ([syntax-rules-node (car (reverse (index-node-children result)))])
                (index-node-expansion-generator syntax-rules-node))
              (loop (cdr nodes)))))))

(define (make-call-index-node parent expression)
  (let* ([str (with-output-to-string (lambda () (pretty-print expression)))]
         [annotations (source-file->annotations str "/dev/null")]
         [node (init-index-node parent (car annotations))])
    node))

(define (node-expression node)
  (if (index-node? node)
    (annotation-stripped (index-node-datum/annotations node))
    node))

(let* ([workspace (init-workspace (current-directory))]
       [root-file-node (workspace-file-node workspace)]
       [ufo-match-file (walk-file root-file-node (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls"))]
       [ufo-match-doc (file-node-document ufo-match-file)]
       [match-gen (get-macro-generator ufo-match-doc 'match)]
       [match-two-gen (get-macro-generator ufo-match-doc 'match-two)])

  (display "=== Layer 1: match ===")
  (newline)
  (let* ([call-node (make-call-index-node #f '(match x ((? string? path) path)))]
         [result (match-gen #f #f ufo-match-doc call-node)]
         [pairs (car result)]
         [exp-node (cdr result)])
    (display "Call expr: ") (write (node-expression call-node)) (newline)
    (display "Expansion expr: ") (write (node-expression exp-node)) (newline)
    (display "Pairs count: ") (display (length pairs)) (newline)
    (display "Pairs (pattern-node -> expansion-node):")
    (newline)
    (for-each
      (lambda (p)
        (display "  ")
        (write (node-expression (car p)))
        (display " -> ")
        (write (node-expression (cdr p)))
        (newline))
      pairs))

  (display "")
  (newline)
  (display "=== Layer 6: match-two (the one with let-syntax) ===")
  (newline)
  (let* ([call-node (make-call-index-node #f '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))]
         [result (match-two-gen #f #f ufo-match-doc call-node)]
         [pairs (car result)]
         [exp-node (cdr result)])
    (display "Call expr: ") (write (node-expression call-node)) (newline)
    (display "Expansion expr: ") (write (node-expression exp-node)) (newline)
    (display "Pairs count: ") (display (length pairs)) (newline)
    (display "Pairs (pattern-node -> expansion-node):")
    (newline)
    (for-each
      (lambda (p)
        (display "  ")
        (write (node-expression (car p)))
        (display " -> ")
        (write (node-expression (cdr p)))
        (newline))
      (if (> (length pairs) 20) (take pairs 20) pairs))
    (if (> (length pairs) 20) (begin (display "  ... (truncated)") (newline))))

  (exit 0))
