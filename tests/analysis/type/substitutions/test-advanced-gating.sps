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

;;The two expensive type-inference passes (P1 recursive-return fixpoint,
;;P3 dotted-rest element inference) run only when the
;;type-inference-advanced? parameter is set: servers opt in via run.ss
;;-t advance (init-server's 11-argument form sets it before the thread
;;pool spawns); tests set the parameter directly. This suite pins the
;;gate: default off = status quo (rest sentinel stays something?), on =
;;the passes fire. The fixpoint pass shares the same gate (one when form
;;in construct-substitutions-for), so the rest-element sentinel flip is
;;sufficient proof for both.

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

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/rest-elements"))

(define (typed-root filename advanced?)
  (type-inference-advanced? advanced?)
  (let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [node (walk-file root-file-node (string-append fixture "/" filename))]
      [document (file-node-document node)])
    (construct-substitutions-for document)
    (car (document-index-node-list document))))

(test-begin "advanced gating: default off")
  (test-assert "parameter defaults to off" (not (type-inference-advanced?)))
  ;;(f 1 "x" #\a) calls (define (f a-fixed . rest)); with the gate off
  ;;the P3 pass must not fire and the elastic tail keeps the sentinel
  (let ([root (typed-root "rest-elements.scm.txt" #f)])
    (let ([name-node (find-define-name-node root 'f)])
      (test-assert "f name node found" name-node)
      (test-assert "rest element stays the something? sentinel"
        (has-rest-element? name-node '(something?)))
      (test-assert "no string? element inferred with the gate off"
        (not (has-rest-element? name-node '(string?))))))
(test-end)

(test-begin "advanced gating: advance on")
  (let ([root (typed-root "rest-elements.scm.txt" #t)])
    (test-assert "parameter on" (type-inference-advanced?))
    (let ([name-node (find-define-name-node root 'f)])
      (test-assert "string? element inferred with the gate on"
        (has-rest-element? name-node '(string?)))
      (test-assert "char? element inferred with the gate on"
        (has-rest-element? name-node '(char?)))))
  ;;leave the parameter off for any later suite in the same process
  (type-inference-advanced? #f)
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
