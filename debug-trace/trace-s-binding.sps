#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Trace how 's' maps through match cascade layers
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

(define (node-expr node)
  (if (index-node? node)
    (annotation-stripped (index-node-datum/annotations node))
    node))

(define (find-pair-for-expr pairs target-expr)
  (find (lambda (p) (equal? target-expr (node-expr (car p)))) pairs))

(define (print-pair p)
  (display "  pattern: ") (write (node-expr (car p)))
  (display " -> expansion: ") (write (node-expr (cdr p)))
  (newline))

(let* ([workspace (init-workspace (current-directory))]
       [root-file-node (workspace-file-node workspace)]
       [ufo-match-file (walk-file root-file-node (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls"))]
       [ufo-match-doc (file-node-document ufo-match-file)]
       [match-gen (get-macro-generator ufo-match-doc 'match)]
       [match-next-gen (get-macro-generator ufo-match-doc 'match-next)]
       [match-one-gen (get-macro-generator ufo-match-doc 'match-one)]
       [match-two-gen (get-macro-generator ufo-match-doc 'match-two)])

  ; Simulate the full cascade for (match '(1) [(s) s])
  
  ; Layer 1: match
  (display "=== Layer 1: (match '(1) [(s) s]) ===") (newline)
  (let* ([n1 (make-call-index-node #f '(match (quote (1)) ((s) s)))]
         [r1 (match-gen #f #f ufo-match-doc n1)]
         [p1 (car r1)]
         [e1 (cdr r1)])
    (display "Expansion: ") (write (node-expr e1)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p1)))

  ; Layer 2: match-next (from expansion above)
  (display "") (newline)
  (display "=== Layer 2: (match-next v ...) ===") (newline)
  (let* ([n2 (make-call-index-node #f '(match-next v (x (set! x)) ((s) s)))]
         [r2 (match-next-gen #f #f ufo-match-doc n2)]
         [p2 (car r2)]
         [e2 (cdr r2)])
    (display "Expansion: ") (write (node-expr e2)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p2)))

  ; Layer 3: match-one (for (s))
  (display "") (newline)
  (display "=== Layer 3: (match-one v (s) ...) ===") (newline)
  (let* ([n3 (make-call-index-node #f '(match-one v (s) (x (set! x)) (match-drop-ids (begin s)) (failure) ()))]
         [r3 (match-one-gen #f #f ufo-match-doc n3)]
         [p3 (car r3)]
         [e3 (cdr r3)])
    (display "Expansion: ") (write (node-expr e3)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p3)))

  ; Layer 4: match-two (for (s))
  (display "") (newline)
  (display "=== Layer 4: (match-two v (s) ...) ===") (newline)
  (let* ([n4 (make-call-index-node #f '(match-two v (s) (x (set! x)) (match-drop-ids (begin s)) (failure) ()))]
         [r4 (match-two-gen #f #f ufo-match-doc n4)]
         [p4 (car r4)]
         [e4 (cdr r4)])
    (display "Expansion: ") (write (node-expr e4)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p4)))

  ; Layer 5: match-one (for s, atom pattern)
  (display "") (newline)
  (display "=== Layer 5: (match-one w s ...) ===") (newline)
  (let* ([n5 (make-call-index-node #f '(match-one w s ((car v) (set-car! v)) (match-drop-ids (begin s)) (failure) ()))]
         [r5 (match-one-gen #f #f ufo-match-doc n5)]
         [p5 (car r5)]
         [e5 (cdr r5)])
    (display "Expansion: ") (write (node-expr e5)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p5)))

  ; Layer 6: match-two (for s, atom pattern -> let binding)
  (display "") (newline)
  (display "=== Layer 6: (match-two w s ...) -> THE LET BINDING ===") (newline)
  (let* ([n6 (make-call-index-node #f '(match-two w s ((car v) (set-car! v)) (match-drop-ids (begin s)) (failure) ()))]
         [r6 (match-two-gen #f #f ufo-match-doc n6)]
         [p6 (car r6)]
         [e6 (cdr r6)])
    (display "Expansion: ") (write (node-expr e6)) (newline)
    (display "Pairs involving 's':") (newline)
    (for-each print-pair (filter (lambda (p) (eq? 's (node-expr (car p)))) p6)))

  (exit 0))
