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

(define (benchmark-expander expander local-document parent expression name iterations)
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
       [match-two-gen (get-macro-generator ufo-match-doc 'match-two)])

  (display "==== Benchmarking macro expansion ====")
  (newline)
  
  ; Benchmark match (5 clauses)
  (benchmark-expander match-gen ufo-match-doc #f '(match x ((? string? path) path)) "match" 100)
  
  ; Benchmark match-two (28 clauses)
  (benchmark-expander match-two-gen ufo-match-doc #f '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) "match-two" 100)
  
  (display "==== Done ====")
  (newline))

(exit 0)
