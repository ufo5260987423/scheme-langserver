#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Test expansion correctness of ufo-match auxiliary macros
;; using scheme-langserver's confirm-clause vs manual expected output.
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util path)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis tokenizer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; confirm-clause helper (same as test-match-expansion-compare)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (confirm-clause literals clause-index-nodes input-expression)
  (let loop ([rest clause-index-nodes] [index 0])
    (if (null? rest) 
      #f
      (let* ([current-clause-index-node (car rest)]
          [current-clause-expression (annotation-stripped (index-node-datum/annotations current-clause-index-node))]
          [pre-target 
            `(syntax-case ',input-expression ,literals 
              (,(car current-clause-expression) 
                ;result
                #'(,index . ,(car (reverse current-clause-expression))))
              (else #f))]
          [target (syntax->datum (eval pre-target))])
        (if target target (loop (cdr rest) (+ 1 index)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build document from file without full workspace init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file->document path)
  (let* ([text (call-with-input-file path get-string-all)]
         [annotations (source-file->annotations text path)]
         [root-index-node (init-index-node #f (car annotations))]
         [doc (make-document (path->uri path) text '())])
    (document-index-node-list-set! doc `(,root-index-node))
    doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find macro definition in document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-macro-definition document macro-name)
  (let ([root-index-node (car (document-index-node-list document))])
    (let search ([nodes (index-node-children root-index-node)])
      (if (null? nodes)
        #f
        (let* ([node (car nodes)]
               [expr (annotation-stripped (index-node-datum/annotations node))])
          (if (and (list? expr)
                   (not (null? expr))
                   (eq? 'define-syntax (car expr))
                   (not (null? (cdr expr)))
                   (eq? macro-name (cadr expr)))
            node
            (search (cdr nodes))))))))

(define (macro-definition->clauses define-syntax-node)
  (let* ([syntax-rules-node (caddr (index-node-children define-syntax-node))]
         [syntax-rules-expression (annotation-stripped (index-node-datum/annotations syntax-rules-node))]
         [literals (cadr syntax-rules-expression)]
         [clause-index-nodes (cddr (index-node-children syntax-rules-node))])
    (list literals clause-index-nodes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test runner for a single macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-macro-expansions macro-name test-cases document)
  (let* ([def-node (find-macro-definition document macro-name)]
         [clauses-info (macro-definition->clauses def-node)]
         [literals (car clauses-info)]
         [clause-index-nodes (cadr clauses-info)])
    (for-each
      (lambda (case)
        (let* ([input (car case)]
               [expected-index (cadr case)]
               [expected-expansion (caddr case)]
               [test-name (cadddr case)]
               [result (confirm-clause literals clause-index-nodes input)])
          (when (not (and result
                          (= (car result) expected-index)
                          (equal? (cdr result) expected-expansion)))
            (display (string-append "\n[DEBUG] " test-name ":\n"))
            (display "  input: ") (pretty-print input)
            (display "  expected: ") (pretty-print (list expected-index expected-expansion))
            (display "  actual: ") (pretty-print (if result (list (car result) (cdr result)) '(#f #f)))
            (newline))
          (test-equal test-name
            (list expected-index expected-expansion)
            (if result
              (list (car result) (cdr result))
              '(#f #f)))))
      test-cases)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "ufo-match auxiliary macro expansion correctness")

(let ([ufo-match-document 
        (file->document 
          (string-append (current-directory) "/.akku/src/ufo-match/ufo-match.sls"))])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; match-next
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-macro-expansions 'match-next
    `(
      ;; Case 1: anonymous failure continuation -> rewrite to named
      ((match-next v (g s) (pat body1 body2) rest)
       2
       (match-next v (g s) (pat (=> failure) body1 body2) rest)
       "match-next: anonymous failure -> named")

      ;; Case 2: named failure continuation
      ((match-next v (g s) (pat (=> fail) body1 body2) rest)
       1
       (let ((fail (lambda () (match-next v (g s) rest))))
         (match-one v pat (g s) (match-drop-ids (begin body1 body2)) (fail) ()))
       "match-next: named failure -> let + match-one")

      ;; Case 3: no more clauses
      ((match-next v (g s))
       0
       (error (quote match) "no matching pattern")
       "match-next: no clauses -> error")
      )
    ufo-match-document)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; match-one
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-macro-expansions 'match-one
    `(
      ;; Case 1: ellipsis check path
      ((match-one v (p q . r) g+s sk fk i)
       0
       (match-check-ellipsis
        q
        (match-extract-vars p (match-gen-ellipsis v p r g+s sk fk i) i ())
        (match-two v (p q . r) g+s sk fk i))
       "match-one: ellipsis check path")

      ;; Case 2: catch-all -> match-two
      ((match-one v pat g+s sk fk i)
       1
       (match-two v pat g+s sk fk i)
       "match-one: catch-all -> match-two")
      )
    ufo-match-document)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; match-two  (a subset of clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-macro-expansions 'match-two
    `(
      ;; Case 1: empty list
      ((match-two v () g+s (sk ...) fk i)
       0
       (if (null? v) (sk ... i) fk)
       "match-two: empty list")

      ;; Case 2: quote
      ((match-two v (quote p) g+s (sk ...) fk i)
       1
       (if (equal? v (quote p)) (sk ... i) fk)
       "match-two: quote")

      ;; Case 3: and (empty)
      ((match-two v (and) g+s (sk ...) fk i)
       3
       (sk ... i)
       "match-two: and empty")

      ;; Case 4: and (non-empty)
      ((match-two v (and p q r) g+s sk fk i)
       4
       (match-one v p g+s (match-one v (and q r) g+s sk fk) fk i)
       "match-two: and non-empty")

      ;; Case 5: or (empty)
      ((match-two v (or) g+s sk fk i)
       5
       fk
       "match-two: or empty")

      ;; Case 6: or (single)
      ((match-two v (or p) g+s sk fk i)
       6
       (match-one v p g+s sk fk i)
       "match-two: or single")

      ;; Case 7: not
      ((match-two v (not p) g+s (sk ...) fk i)
       8
       (let ((fk2 (lambda () (sk ... i))))
         (match-one v p g+s (match-drop-ids fk) (fk2) i))
       "match-two: not")

      ;; Case 8: pair pattern
      ((match-two v (p . q) g+s sk fk i)
       24
       (if (pair? v)
           (let ((w (car v)) (x (cdr v)))
             (match-one w p ((car v) (set-car! v))
                        (match-one x q ((cdr v) (set-cdr! v)) sk fk)
                        fk
                        i))
           fk)
       "match-two: pair pattern")
      )
    ufo-match-document)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; match-drop-ids
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-macro-expansions 'match-drop-ids
    `(
      ((match-drop-ids expr id1 id2)
       0
       expr
       "match-drop-ids: drop ids")
      )
    ufo-match-document)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; match-gen-or-step
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-macro-expansions 'match-gen-or-step
    `(
      ;; Case 1: empty list -> fk
      ((match-gen-or-step v () g+s sk fk x)
       0
       fk
       "match-gen-or-step: empty -> fk")

      ;; Case 2: single element
      ((match-gen-or-step v (p) g+s sk fk i)
       1
       (match-one v p g+s sk fk i)
       "match-gen-or-step: single -> match-one")

      ;; Case 3: multiple elements
      ((match-gen-or-step v (p q) g+s sk fk i)
       2
       (let ((fk2 (lambda () (match-gen-or-step v (q) g+s sk fk i))))
         (match-one v p g+s sk (fk2) i))
       "match-gen-or-step: multiple -> let + recurse")
      )
    ufo-match-document))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
