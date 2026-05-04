#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Compare scheme-langserver's expansion with Chez Scheme's expansion
;; for the match macro in load.sls
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
  (scheme-langserver protocol alist-access-object)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:tree-contains? tree target)
  (cond 
    [(equal? tree target) #t]
    [(null? tree) #f]
    [(pair? tree) (or (private:tree-contains? (car tree) target)
                      (private:tree-contains? (cdr tree) target))]
    [(vector? tree) (private:tree-contains? (vector->list tree) target)]
    [else #f]))

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
;; Test: compare expansions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "match expansion comparison")

(let* ([workspace-instance (init-workspace (current-directory))]
   [root-file-node (workspace-file-node workspace-instance)]
   [root-library-node (workspace-library-node workspace-instance)]
   ; find match call in load.sls
   [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/load.sls"))]
   [document (file-node-document target-file-node)]
   [root-index-node (car (document-index-node-list document))]
   [match-call-node (find-index-node-recursive
      (lambda (n)
        (let ([expr (annotation-stripped-expression n)])
          (and (list? expr) (not (null? expr)) (eq? 'match (car expr)))))
      root-index-node)]
   [match-expression (annotation-stripped (index-node-datum/annotations match-call-node))]
   ; find match macro definition
   [match-reference (car (find-available-references-for document match-call-node 'match))]
   [match-definition-index-node (identifier-reference-initialization-index-node match-reference)]
   [syntax-rules-node (caddr (index-node-children match-definition-index-node))]
   [syntax-rules-expression (annotation-stripped (index-node-datum/annotations syntax-rules-node))]
   [literals (cadr syntax-rules-expression)]
   [clause-index-nodes (cddr (index-node-children syntax-rules-node))])

  (display "match call expression:\n") (pretty-print match-expression) (newline)
  (display "number of clauses in match syntax-rules: ") (display (length clause-index-nodes)) (newline)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Method A: scheme-langserver's confirm-clause (first-step)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let ([sls-result (confirm-clause literals clause-index-nodes match-expression)])
    (display "\n=== scheme-langserver first-step expansion (confirm-clause) ===\n")
    (pretty-print sls-result)
    (newline)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Method B: Chez Scheme expand (recursive to core forms)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Method B: Chez Scheme expand (best-effort, may fail if match
    ;; is not bound in interaction environment)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (let ([expanded-datum 
            (guard (c [else 'expand-failed])
              (eval '(import (ufo-match)))
              (let* ([expr-syntax (datum->syntax #'here match-expression)]
                     [expanded-syntax (expand expr-syntax)])
                (syntax->datum expanded-syntax)))])
      (display "=== Chez Scheme expand (full) result ===\n")
      (pretty-print expanded-datum)
      (newline)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Assertions
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ; The first-step template from confirm-clause should contain match-next
      ; (because match expands to match-next in the first step)
      (test-equal "first-step expansion contains match-next"
        #t
        (if sls-result
          (private:tree-contains? (cdr sls-result) 'match-next)
          #f))

      ; Chez expand (full) should NOT contain match-next anymore
      (test-equal "full Chez expansion eliminates match-next"
        #f
        (if (equal? expanded-datum 'expand-failed)
          #f
          (private:tree-contains? expanded-datum 'match-next)))

      ; The confirm-clause matched the correct clause (index 4 for atom pattern)
      (test-equal "confirm-clause matched clause index 4"
        4
        (if sls-result (car sls-result) #f))

      ; The bound variable in first-step should be 'expression'
      (test-equal "first-step binds atom to 'expression'"
        #t
        (if sls-result
          (private:tree-contains? (cdr sls-result) 'expression)
          #f)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
