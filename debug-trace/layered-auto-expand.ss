#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
(import 
  (chezscheme)
  (ufo-match)
  (scheme-langserver util path)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
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

(define (expand-layer expander local-document parent expression depth)
  (display "==== Layer ")
  (display depth)
  (display ": ")
  (write expression)
  (display " ====\n")
  (let* ([call-node (make-call-index-node parent expression)]
         [result (expander #f #f local-document call-node)])
    (if result
        (let* ([pairs (car result)]
               [expansion (cdr result)]
               [expansion-expr (annotation-stripped (index-node-datum/annotations expansion))])
          (display "Expanded to: ")
          (write expansion-expr)
          (newline)
          expansion)
        (begin
          (display "Returned #f\n")
          #f))))

(define match-expr '(match x ((? string? path) path)))
(define match-next-expr-1 '(match-next v (x (set! x)) ((? string? path) path)))
(define match-next-expr-2 '(match-next v (x (set! x)) ((? string? path) (=> failure) path)))
(define match-one-expr '(match-one v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))
(define match-check-ellipsis-expr
  '(match-check-ellipsis
     string?
     (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ())
     (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))
(define match-two-expr '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))
(define match-one-expr-2 '(match-one v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))
(define match-check-ellipsis-expr-2
  '(match-check-ellipsis
     path
     (match-extract-vars and (match-gen-ellipsis v and () (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ())
     (match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))
(define match-two-expr-2 '(match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))
(define match-one-expr-3
  '(match-one v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ()))
(define match-two-expr-3
  '(match-two v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ()))
(define match-check-identifier-expr
  '(match-check-identifier
     path
     (let-syntax
         ((new-sym?
           (syntax-rules ()
             ((new-sym? path sk2 fk2) sk2)
             ((new-sym? y sk2 fk2) fk2))))
       (new-sym?
         random-sym-to-match
         (let ((path v)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) (path)))
         (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))))
     (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))))

(let* ([start-time (current-time)]
       [workspace (init-workspace (current-directory))]
       [root-file-node (workspace-file-node workspace)]
       [ufo-match-file (walk-file root-file-node (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls"))]
       [ufo-match-doc (file-node-document ufo-match-file)]
       [match-gen (get-macro-generator ufo-match-doc 'match)]
       [match-next-gen (get-macro-generator ufo-match-doc 'match-next)]
       [match-one-gen (get-macro-generator ufo-match-doc 'match-one)]
       [match-check-ellipsis-gen (get-macro-generator ufo-match-doc 'match-check-ellipsis)]
       [match-two-gen (get-macro-generator ufo-match-doc 'match-two)]
       [match-check-identifier-gen (get-macro-generator ufo-match-doc 'match-check-identifier)]
       [match-drop-ids-gen (get-macro-generator ufo-match-doc 'match-drop-ids)])

  (display "Generators loaded: match=")
  (write (procedure? match-gen))
  (display " match-next=")
  (write (procedure? match-next-gen))
  (display " match-one=")
  (write (procedure? match-one-gen))
  (display " match-check-ellipsis=")
  (write (procedure? match-check-ellipsis-gen))
  (display " match-two=")
  (write (procedure? match-two-gen))
  (display " match-check-identifier=")
  (write (procedure? match-check-identifier-gen))
  (display " match-drop-ids=")
  (write (procedure? match-drop-ids-gen))
  (newline)

  ; Layer 1: match
  (let ([layer1 (expand-layer match-gen ufo-match-doc #f match-expr 1)])
    (if layer1
        (let ([layer2 (expand-layer match-next-gen ufo-match-doc layer1 match-next-expr-1 2)])
          (if layer2
              (let ([layer3 (expand-layer match-next-gen ufo-match-doc layer2 match-next-expr-2 3)])
                (if layer3
                    (let ([layer4 (expand-layer match-one-gen ufo-match-doc layer3 match-one-expr 4)])
                      (if layer4
                          (let ([layer5 (expand-layer match-check-ellipsis-gen ufo-match-doc layer4 match-check-ellipsis-expr 5)])
                            (if layer5
                                (let ([layer6 (expand-layer match-two-gen ufo-match-doc layer5 match-two-expr 6)])
                                  (if layer6
                                      (let ([layer7 (expand-layer match-one-gen ufo-match-doc layer6 match-one-expr-2 7)])
                                        (if layer7
                                            (let ([layer8 (expand-layer match-check-ellipsis-gen ufo-match-doc layer7 match-check-ellipsis-expr-2 8)])
                                              (if layer8
                                                  (let ([layer9 (expand-layer match-two-gen ufo-match-doc layer8 match-two-expr-2 9)])
                                                    (if layer9
                                                        (let ([layer10 (expand-layer match-one-gen ufo-match-doc layer9 match-one-expr-3 10)])
                                                          (if layer10
                                                              (let ([layer11 (expand-layer match-two-gen ufo-match-doc layer10 match-two-expr-3 11)])
                                                                (if layer11
                                                                    (let ([layer12 (expand-layer match-check-identifier-gen ufo-match-doc layer11 match-check-identifier-expr 12)])
                                                                      (if layer12
                                                                          (display "Layer 12 done\n")
                                                                          (display "Layer 12 returned #f (expected due to let-syntax limitation)\n")))
                                                                    (display "Layer 11 failed\n")))
                                                              (display "Layer 10 failed\n")))
                                                        (display "Layer 9 failed\n")))
                                                  (display "Layer 8 failed\n")))
                                            (display "Layer 7 failed\n")))
                                      (display "Layer 6 failed\n")))
                                (display "Layer 5 failed\n")))
                          (display "Layer 4 failed\n")))
                    (display "Layer 3 failed\n")))
              (display "Layer 2 failed\n")))
        (display "Layer 1 failed\n")))

  (let* ([end-time (current-time)]
         [elapsed (time-difference end-time start-time)]
         [seconds (+ (time-second elapsed) (/ (time-nanosecond elapsed) 1e9))])
    (display "\n==== Total elapsed time: ")
    (display seconds)
    (display " seconds ====\n")))

(exit 0)
