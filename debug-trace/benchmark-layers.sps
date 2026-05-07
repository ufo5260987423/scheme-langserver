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

(define (benchmark-layer expander local-document parent expression name iterations)
  (let ([call-node (make-call-index-node parent expression)])
    ; Warmup
    (expander #f #f local-document call-node)
    ; Benchmark
    (let ([start (current-time)])
      (let loop ([i 0])
        (if (< i iterations)
          (begin
            (expander #f #f local-document call-node)
            (loop (+ i 1)))
          (let* ([end (current-time)]
                 [elapsed (time-difference end start)]
                 [seconds (+ (time-second elapsed) (/ (time-nanosecond elapsed) 1e9))]
                 [per-call (/ seconds iterations)])
            (display name)
            (display ": ")
            (display iterations)
            (display " iterations, total ")
            (display (* 1000 seconds))
            (display " ms, per-call ")
            (display (* 1000 per-call))
            (display " ms")
            (newline)))))))

(let* ([workspace (init-workspace (current-directory))]
       [root-file-node (workspace-file-node workspace)]
       [ufo-match-file (walk-file root-file-node (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls"))]
       [ufo-match-doc (file-node-document ufo-match-file)]
       [match-gen (get-macro-generator ufo-match-doc 'match)]
       [match-next-gen (get-macro-generator ufo-match-doc 'match-next)]
       [match-one-gen (get-macro-generator ufo-match-doc 'match-one)]
       [match-check-ellipsis-gen (get-macro-generator ufo-match-doc 'match-check-ellipsis)]
       [match-two-gen (get-macro-generator ufo-match-doc 'match-two)]
       [match-check-identifier-gen (get-macro-generator ufo-match-doc 'match-check-identifier)])

  (display "==== Benchmarking 12-layer cascade ====")
  (newline)
  
  (benchmark-layer match-gen ufo-match-doc #f '(match x ((? string? path) path)) "Layer 1: match" 50)
  (benchmark-layer match-next-gen ufo-match-doc #f '(match-next v (x (set! x)) ((? string? path) path)) "Layer 2: match-next" 50)
  (benchmark-layer match-next-gen ufo-match-doc #f '(match-next v (x (set! x)) ((? string? path) (=> failure) path)) "Layer 3: match-next" 50)
  (benchmark-layer match-one-gen ufo-match-doc #f '(match-one v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) "Layer 4: match-one" 50)
  (benchmark-layer match-check-ellipsis-gen ufo-match-doc #f '(match-check-ellipsis string? (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())) "Layer 5: match-check-ellipsis" 50)
  (benchmark-layer match-two-gen ufo-match-doc #f '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) "Layer 6: match-two" 50)
  (benchmark-layer match-one-gen ufo-match-doc #f '(match-one v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) "Layer 7: match-one" 50)
  (benchmark-layer match-check-ellipsis-gen ufo-match-doc #f '(match-check-ellipsis path (match-extract-vars and (match-gen-ellipsis v and () (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())) "Layer 8: match-check-ellipsis" 50)
  (benchmark-layer match-two-gen ufo-match-doc #f '(match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) "Layer 9: match-two" 50)
  (benchmark-layer match-one-gen ufo-match-doc #f '(match-one v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ()) "Layer 10: match-one" 50)
  (benchmark-layer match-two-gen ufo-match-doc #f '(match-two v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ()) "Layer 11: match-two" 50)
  (benchmark-layer match-check-identifier-gen ufo-match-doc #f '(match-check-identifier path (let-syntax ((new-sym? (syntax-rules () ((new-sym? path sk2 fk2) sk2) ((new-sym? y sk2 fk2) fk2)))) (new-sym? random-sym-to-match (let ((path v)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) (path))) (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure)))) (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))) "Layer 12: match-check-identifier" 50)
  
  (display "==== Done ====")
  (newline))

(exit 0)
