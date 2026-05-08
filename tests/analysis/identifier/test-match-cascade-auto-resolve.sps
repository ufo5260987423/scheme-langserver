#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test: auto-resolve can fully cascade-expand match macro family
;; After removing nested-macro guard on let-syntax and raising memory cap to 10.
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

(define (expression-from-node node)
  (annotation-stripped (index-node-datum/annotations node)))

(test-begin "match cascade auto-resolve")

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

  ; All generators should exist (none rejected by nested-macro guard)
  (test-assert "match generator exists" (procedure? match-gen))
  (test-assert "match-next generator exists" (procedure? match-next-gen))
  (test-assert "match-one generator exists" (procedure? match-one-gen))
  (test-assert "match-check-ellipsis generator exists" (procedure? match-check-ellipsis-gen))
  (test-assert "match-two generator exists" (procedure? match-two-gen))
  (test-assert "match-check-identifier generator exists" (procedure? match-check-identifier-gen))

  ; Layer 1: match expands to something containing match-next
  (let* ([node1 (make-call-index-node '() '(match x ((? string? path) path)))]
         [result1 (match-gen #f #f ufo-match-doc node1)]
         [exp1 (expression-from-node (cdr result1))])
    (test-assert "L1: match expands" (not (equal? exp1 '(match x ((? string? path) path)))))
    (test-assert "L1: expansion contains match-next"
      (let tree-contains? ([t exp1] [target 'match-next])
        (cond [(equal? t target) #t]
              [(null? t) #f]
              [(pair? t) (or (tree-contains? (car t) target) (tree-contains? (cdr t) target))]
              [else #f]))))

  ; Layer 5: match-check-ellipsis (was #f before removing guard)
  (let* ([node5 (make-call-index-node '() 
                  '(match-check-ellipsis 
                     string? 
                     (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) 
                     (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))]
         [result5 (match-check-ellipsis-gen #f #f ufo-match-doc node5)]
         [exp5 (expression-from-node (cdr result5))])
    (test-assert "L5: match-check-ellipsis expands (was #f before guard removal)" 
      (not (equal? exp5 
             '(match-check-ellipsis 
                string? 
                (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) 
                (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))))))

  ; Layer 6: match-two expands (was #f before removing guard)
  (let* ([node6 (make-call-index-node '() '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))]
         [result6 (match-two-gen #f #f ufo-match-doc node6)]
         [exp6 (expression-from-node (cdr result6))])
    (test-assert "L6: match-two expands (was #f before guard removal)"
      (not (equal? exp6 '(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))))

  ; Layer 6b: match-two with symbol t - verify pairs contains t-node mapping
  (let* ([node6b (make-call-index-node '() '(match-two v t (x (set! x)) (match-drop-ids (begin t)) (failure) ()))]
         [result6b (match-two-gen #f #f ufo-match-doc node6b)]
         [pairs6b (car result6b)]
         [exp6b (cdr result6b)]
         [t-node (caddr (index-node-children node6b))])
    (test-assert "L6b: match-two symbol t expands"
      (not (equal? (expression-from-node exp6b)
             '(match-two v t (x (set! x)) (match-drop-ids (begin t)) (failure) ()))))
    ;; Verify t-node appears in pairs (as cdr, meaning expansion maps back to it)
    (test-assert "L6b: t-node is in pairs"
      (let loop ([ps pairs6b])
        (and (not (null? ps))
             (or (eq? (cdar ps) t-node)
                 (loop (cdr ps))))))
    
    ;; Verify shallow-copy propagates let-binding ref to t-node
    (let ([rule (expansion-generator->rule match-two-gen step (workspace-file-linkage workspace) '() '())])
      (rule root-file-node (workspace-library-node workspace) ufo-match-doc node6b))
    (let ([exports (index-node-references-export-to-other-node t-node)])
      (display "[DEBUG] t-node exports after shallow-copy: ")
      (display (length exports))
      (newline)
      (test-assert "L6b: t-node has exports after shallow-copy"
        (not (null? exports)))))

  ; Layer 12: match-check-identifier expands (contains let-syntax with inner syntax-rules)
  (let* ([node12 (make-call-index-node '() 
                   '(match-check-identifier 
                      path 
                      (let-syntax 
                          ((new-sym? (syntax-rules () ((new-sym? path sk2 fk2) sk2) ((new-sym? y sk2 fk2) fk2))))
                        (new-sym? random-sym-to-match 
                                  (let ((path v)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) (path))) 
                                  (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure)))) 
                      (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))))]
         [result12 (match-check-identifier-gen #f #f ufo-match-doc node12)]
         [exp12 (expression-from-node (cdr result12))])
    (test-assert "L12: match-check-identifier expands (was #f before guard removal)"
      (not (equal? exp12 
             '(match-check-identifier 
                path 
                (let-syntax 
                    ((new-sym? (syntax-rules () ((new-sym? path sk2 fk2) sk2) ((new-sym? y sk2 fk2) fk2))))
                  (new-sym? random-sym-to-match 
                            (let ((path v)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) (path))) 
                            (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure)))) 
                (if (equal? v path) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
