#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver util test)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)

  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker)

  (scheme-langserver analysis type substitutions generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define numeric-atoms '(number? fixnum? integer? real? bignum? flonum?))

(define (find-toplevel-define-return root name)
  ;;children of the library root: find (define (name . params) body ...) or
  ;;(define name (case-lambda ...)), return the LAST body form's index-node
  ;;(for case-lambda: the last clause's last body form)
  (let loop ([children (index-node-children root)])
    (cond
      [(null? children) #f]
      [else
        (let* ([child (car children)]
            [expr (annotation-stripped (index-node-datum/annotations child))]
            [kids (index-node-children child)])
          (if (and (list? expr)
                (not (null? expr))
                (eq? 'define (car expr))
                (>= (length expr) 3))
            (cond
              ;;(define (name . params) body ...)
              [(and (list? (cadr expr))
                  (not (null? (cadr expr)))
                  (eq? name (caadr expr)))
                (let ([body (cddr kids)])
                  (list-ref body (- (length body) 1)))]
              ;;(define name (case-lambda (...))) -- use last clause body
              [(and (symbol? (cadr expr))
                  (eq? name (cadr expr))
                  (list? (caddr expr))
                  (not (null? (caddr expr)))
                  (eq? 'case-lambda (car (caddr expr))))
                (let* ([clauses (cdr (caddr expr))]
                    [last-clause (list-ref clauses (- (length clauses) 1))]
                    [clause-kids (list-ref kids 2)]
                    [clause-nodes (index-node-children clause-kids)]
                    [last-clause-node (list-ref clause-nodes (- (length clause-nodes) 1))]
                    [body (cdr (index-node-children last-clause-node))])
                  (list-ref body (- (length body) 1)))]
              [else (loop (cdr children))])
            (loop (cdr children))))])))

(define (find-call-node root name)
  ;;an application (name arg ...) whose PARENT is not a define form (the
  ;;(define (name . params) ...) name-form also has car == name)
  (find-index-node-recursive
    (lambda (n)
      (let ([e (annotation-stripped (index-node-datum/annotations n))])
        (and (list? e)
          (not (null? e))
          (eq? name (car e))
          (let* ([parent (index-node-parent n)]
              [pe (and (index-node? parent)
                    (annotation-stripped (index-node-datum/annotations parent)))])
            (not (and (list? pe) (not (null? pe)) (eq? 'define (car pe))))))))
    root))

(define (atom-identifier candidate)
  (cond
    [(identifier-reference? candidate) (identifier-reference-identifier candidate)]
    [(symbol? candidate) candidate]
    [else #f]))

(define (has-numeric-atom? results)
  (find
    (lambda (c)
      (let ([id (atom-identifier c)])
        (and id (find (lambda (n) (eq? n id)) numeric-atoms))))
    results))

(define (has-list-candidate? results)
  (find
    (lambda (c)
      (and (list? c)
        (not (null? c))
        (or (eq? 'inner:list? (car c)) (eq? 'inner:pair? (car c)))))
    results))

(define (substitution-solved-candidates node)
  (filter
    (lambda (s) (or (identifier-reference? s) (and (list? s) (not (null? s)))))
    (index-node-substitution-list node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/recursive-return"))

(define workspace (init-workspace fixture 'txt 'r6rs #f #f))
(define root-file-node (workspace-file-node workspace))

(define (typed-root filename)
  (let* ([node (walk-file root-file-node (string-append fixture "/" filename))]
      [document (file-node-document node)])
    (construct-substitutions-for document)
    (car (document-index-node-list document))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case A: named-let arithmetic recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "named-let fixpoint")

(let ([root (typed-root "loop-sum.scm.txt")])
  (let ([return-node (find-toplevel-define-return root 'loop-sum)])
    (test-assert "return node found" (index-node? return-node))
    (test-assert "return includes a numeric candidate"
      (has-numeric-atom? (type:interpret-result-list return-node))))
  ;;the recursive call site itself must carry solved candidates
  (let ([call-node (find-call-node root 'loop)])
    (test-assert "loop call node found" (index-node? call-node))
    (test-assert "loop call gained solved candidates"
      (has-numeric-atom? (substitution-solved-candidates call-node)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case B: direct self-recursive define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "define self-recursion fixpoint")

(let ([root (typed-root "downby.scm.txt")])
  (let ([return-node (find-toplevel-define-return root 'downby)])
    (test-assert "return node found" (index-node? return-node))
    (test-assert "return includes a numeric candidate"
      (has-numeric-atom? (type:interpret-result-list return-node))))
  (let ([call-node (find-call-node root 'downby)])
    (test-assert "recursive call node found" (index-node? call-node))
    (test-assert "call node gained solved candidates"
      (has-numeric-atom? (substitution-solved-candidates call-node)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case C: unbounded container growth -- caps must stop gracefully
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "growth cap graceful degradation")

(let ([root (typed-root "grow.scm.txt")])
  (let ([return-node (find-toplevel-define-return root 'grow)])
    (test-assert "return node found" (index-node? return-node))
    (let ([results (type:interpret-result-list return-node)])
      (test-assert "result non-empty" (> (length results) 0))
      ;;the analysis must terminate and leave at least the base-case
      ;;container candidates solved (cap fires, nothing crashes)
      (test-assert "container candidates present"
        (has-list-candidate? results))))
  (let ([call-node (find-call-node root 'grow)])
    (test-assert "recursive call node found" (index-node? call-node))
    (test-assert "call node gained solved container candidates"
      (has-list-candidate? (substitution-solved-candidates call-node)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case D: non-recursive control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "non-recursive control unchanged")

(let ([root (typed-root "plain.scm.txt")])
  (let ([return-node (find-toplevel-define-return root 'plain)])
    (test-assert "return node found" (index-node? return-node))
    (test-assert "return includes a numeric candidate"
      (has-numeric-atom? (type:interpret-result-list return-node)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case E: variable define + case-lambda self-recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "case-lambda define fixpoint")

(let ([root (typed-root "case-lambda-sum.scm.txt")])
  (let ([return-node (find-toplevel-define-return root 'case-lambda-sum)])
    (test-assert "return node found" (index-node? return-node))
    (test-assert "return includes a numeric candidate"
      (has-numeric-atom? (type:interpret-result-list return-node))))
  (let ([call-node (find-call-node root 'case-lambda-sum)])
    (test-assert "recursive call node found" (index-node? call-node))
    (test-assert "call node gained solved candidates"
      (has-numeric-atom? (substitution-solved-candidates call-node)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
