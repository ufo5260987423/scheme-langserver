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

  (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (scheme-langserver analysis type substitutions generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;the name-symbol index-node of (define (name fixed... . rest) body...)
(define (find-define-name-node root name)
  (let ([define-node
          (find-index-node-recursive
            (lambda (n)
              (let ([e (annotation-stripped (index-node-datum/annotations n))])
                (and (list? e)
                  (not (null? e))
                  (eq? 'define (car e))
                  (>= (length e) 2)
                  (let ([formals (cadr e)])
                    (and (pair? formals) (eq? name (car formals)))))))
            root)])
    (and define-node
      (car (index-node-children (cadr (index-node-children define-node)))))))

;;every elastic-tail item of the arrows carried by the name node --
;;for the generator's rest encoding (inner:list? fixed... something?
;;...) that item IS the rest element type (inferred or the something?
;;sentinel)
(define (arrow-rest-elements name-node)
  (apply append
    (map
      (lambda (item)
        (if (inner:lambda? item)
          (let ([param (inner:lambda-param item)])
            (if (and (list? param) (eq? (car param) 'inner:list?))
              (let* ([content (inner:list-content param)]
                  [items
                    (filter
                      (lambda (x) (and (not (eq? x '...)) (not (eq? x '**1))))
                      content)])
                (if (and (< (length items) (length content))
                      (not (null? items)))
                  (list (car (reverse items)))
                  '()))
              '()))
          '()))
      (index-node-substitution-list name-node))))

(define (element-identifier e)
  (cond
    [(identifier-reference? e) (identifier-reference-identifier e)]
    [(symbol? e) e]
    [else #f]))

(define (has-rest-element? name-node elements)
  (find
    (lambda (e)
      (let ([id (element-identifier e)])
        (and id (memq id elements))))
    (arrow-rest-elements name-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/rest-elements"))

(define workspace (init-workspace fixture 'txt 'r6rs #f #f))
(define root-file-node (workspace-file-node workspace))

(define (typed-root filename)
  (let* ([node (walk-file root-file-node (string-append fixture "/" filename))]
      [document (file-node-document node)])
    (construct-substitutions-for document)
    (car (document-index-node-list document))))

(define root (typed-root "rest-elements.scm.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "rest element inference: call sites")
  ;;(f 1 "x" #\a) -- elements string? and char?
  (let ([name-node (find-define-name-node root 'f)])
    (test-assert "f name node found" name-node)
    (test-assert "string? element from call site"
      (has-rest-element? name-node '(string?)))
    (test-assert "char? element from call site"
      (has-rest-element? name-node '(char?))))
(test-end)

(test-begin "rest element inference: apply tail")
  ;;(g 1 2 3) contributes nothing numeric-specific beyond fixnum?;
  ;;the apply site contributes +'s elastic-tail element number?
  (let ([name-node (find-define-name-node root 'g)])
    (test-assert "g name node found" name-node)
    (test-assert "number? element from apply tail"
      (has-rest-element? name-node '(number?))))
(test-end)

(test-begin "rest element inference: symbol rest")
  ;;(h 1 "u") -- every argument is an element
  (let ([name-node (find-define-name-node root 'h)])
    (test-assert "h name node found" name-node)
    (test-assert "string? element from call site"
      (has-rest-element? name-node '(string?)))
    (test-assert "numeric element from call site"
      (has-rest-element? name-node '(fixnum? integer? number? bignum?))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
