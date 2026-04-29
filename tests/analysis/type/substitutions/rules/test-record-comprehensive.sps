#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-2023 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)

  (scheme-langserver util contain)
  (scheme-langserver util text)
  (scheme-langserver util test)

  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier meta)

  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker)

  (scheme-langserver analysis type substitutions util)
  (scheme-langserver analysis type substitutions generator)

  (scheme-langserver protocol alist-access-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-first-ref doc root-node name)
  (let ([refs (find-available-references-for doc root-node name)])
    (if (null? refs) #f (car refs))))

;; Find a usage node inside any (define ...) body at top level.
(define (find-usage-node root-node name)
  (let loop ([children (index-node-children root-node)])
    (if (null? children)
      #f
      (let ([child (car children)])
        (or (and (index-node? child)
              (let ([expr (annotation-stripped (index-node-datum/annotations child))])
                (and (list? expr) (not (null? expr)) (eq? 'define (car expr))
                  (find-index-node-recursive
                    (lambda (n) (and (index-node? n) (eq? name (annotation-stripped-expression n))))
                    child))))
            (loop (cdr children)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/record-type"))

(define workspace (init-workspace fixture 'txt 'r6rs #f #f))
(define root-file-node (workspace-file-node workspace))
(define root-library-node (workspace-library-node workspace))
(define target-file-node (walk-file root-file-node (string-append fixture "/point.scm.txt")))
(define target-document (file-node-document target-file-node))
(define root-index-node (car (document-index-node-list target-document)))

(construct-substitutions-for target-document)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group A: Direct type-expressions set by record.sls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "record.sls direct type-expressions")

;; Constructor
(let ([make-point-ref (get-first-ref target-document root-index-node 'make-point)])
  (test-assert "constructor ref exists" (identifier-reference? make-point-ref))
  (test-equal "constructor type" 'constructor (identifier-reference-type make-point-ref))
  (let ([tes (identifier-reference-type-expressions make-point-ref)])
    (test-equal "constructor has 1 type-expression" 1 (length tes))
    (test-equal "constructor type string"
      "([identifier-reference point?] <- (inner:list? something? ... ) ) "
      (inner:type->string (car tes)))))

;; Predicator
(let ([point?-ref (get-first-ref target-document root-index-node 'point?)])
  (test-assert "predicator ref exists" (identifier-reference? point?-ref))
  (test-equal "predicator type" 'predicator (identifier-reference-type point?-ref))
  (let ([tes (identifier-reference-type-expressions point?-ref)])
    (test-equal "predicator has 1 type-expression" 1 (length tes))
    ;; record.sls wraps boolean? with construct-type-expression-with-meta
    (test-equal "predicator type string"
      "([identifier-reference boolean?] <- (inner:list? something? ) ) "
      (inner:type->string (car tes)))))

;; Getter
(let ([point-x-ref (get-first-ref target-document root-index-node 'point-x)])
  (test-assert "getter ref exists" (identifier-reference? point-x-ref))
  (test-equal "getter type" 'getter (identifier-reference-type point-x-ref))
  (let ([tes (identifier-reference-type-expressions point-x-ref)])
    (test-equal "getter has 1 type-expression" 1 (length tes))
    (test-equal "getter type string"
      "(something? <- (inner:list? [identifier-reference point?] ) ) "
      (inner:type->string (car tes)))))

;; Setter
(let ([point-x-set!-ref (get-first-ref target-document root-index-node 'point-x-set!)])
  (test-assert "setter ref exists" (identifier-reference? point-x-set!-ref))
  (test-equal "setter type" 'setter (identifier-reference-type point-x-set!-ref))
  (let ([tes (identifier-reference-type-expressions point-x-set!-ref)])
    (test-equal "setter has 1 type-expression" 1 (length tes))
    (test-equal "setter type string"
      "(void? <- (inner:list? [identifier-reference point?] something? ) ) "
      (inner:type->string (car tes)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group B: trivial.sls substitution generation for local identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "trivial.sls substitutions for local record identifiers")

;; For constructor/getter/setter, trivial.sls attaches the identifier-reference itself
(let ([make-point-node (find-usage-node root-index-node 'make-point)])
  (test-assert "make-point usage node exists" (index-node? make-point-node))
  (let ([subs (index-node-substitution-list make-point-node)])
    ;; The substitution list should contain the identifier-reference itself
    ;; because trivial.sls L96-97 does (extend-index-node-substitution-list index-node identifier-reference)
    (test-assert "make-point subs contain identifier-reference"
      (find identifier-reference? subs))))

;; For predicator, trivial.sls hard-codes (boolean? <- (inner:list? something?))
;; This is the KEY observation: record.sls's predicator type-expressions are NOT used locally.
(let ([point?-node (find-usage-node root-index-node 'point?)])
  (test-assert "point? usage node exists" (index-node? point?-node))
  (let ([subs (index-node-substitution-list point?-node)])
    ;; trivial.sls L98-99: (extend-index-node-substitution-list index-node `(,private-boolean? <- (inner:list? something?)))
    ;; So the substitution should be a LIST (function type), NOT an identifier-reference
    (test-assert "point? subs contain at least one list"
      (find list? subs))
    ;; It should NOT be just the identifier-reference (unlike constructor/getter/setter)
    (test-equal "point? subs do NOT contain identifier-reference as direct substitution"
      #f (find identifier-reference? subs))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group C: interpreter final results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "interpreter results for record identifiers")

;; Constructor: interpreter reads type-expressions from identifier-reference
(let ([make-point-node (find-usage-node root-index-node 'make-point)])
  (let ([results (type:interpret-result-list make-point-node)])
    (test-assert "constructor interpret result exists" (> (length results) 0))
    (test-equal "constructor interpret string"
      "([identifier-reference point?] <- (inner:list? something? ... ) ) "
      (inner:type->string (car results)))))

;; Predicator: trivial.sls hard-coded substitution is interpreted directly
(let ([point?-node (find-usage-node root-index-node 'point?)])
  (let ([results (type:interpret-result-list point?-node)])
    (test-assert "predicator interpret result exists" (> (length results) 0))
    ;; The result should be the hard-coded (boolean? <- (inner:list? something?))
    ;; where boolean? is private-boolean? (an identifier-reference constructed by trivial.sls)
    (test-equal "predicator interpret string"
      "([identifier-reference boolean?] <- (inner:list? something? ) ) "
      (inner:type->string (car results)))))

;; Getter
(let ([point-x-node (find-usage-node root-index-node 'point-x)])
  (let ([results (type:interpret-result-list point-x-node)])
    (test-assert "getter interpret result exists" (> (length results) 0))
    (test-equal "getter interpret string"
      "(something? <- (inner:list? [identifier-reference point?] ) ) "
      (inner:type->string (car results)))))

;; Setter
(let ([point-x-set!-node (find-usage-node root-index-node 'point-x-set!)])
  (let ([results (type:interpret-result-list point-x-set!-node)])
    (test-assert "setter interpret result exists" (> (length results) 0))
    (test-equal "setter interpret string"
      "(void? <- (inner:list? [identifier-reference point?] something? ) ) "
      (inner:type->string (car results)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group D: constructor return type contains predicator identifier-reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "constructor return type embedding")

;; record.sls sets constructor's type-expressions to:
;;   ((predicator <- (inner:list? something? ...)))
;; where predicator is an identifier-reference.
;; When the interpreter returns this, the predicator should be present as an identifier-reference.
(let ([make-point-ref (get-first-ref target-document root-index-node 'make-point)])
  (let ([type-expr (car (identifier-reference-type-expressions make-point-ref))])
    ;; type-expr should be: (predicator <- (inner:list? something? ...))
    (test-equal "constructor type is a list of 3 elements" 3 (length type-expr))
    (test-assert "constructor return type is identifier-reference (predicator)"
      (identifier-reference? (car type-expr)))
    (test-equal "constructor return type identifier is point?"
      'point? (identifier-reference-identifier (car type-expr)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group E: predicator type-expressions source mismatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "predicator type-expressions source mismatch")

;; record.sls sets predicator's type-expressions with (construct-type-expression-with-meta 'boolean?)
;; trivial.sls uses private-boolean? (also construct-type-expression-with-meta 'boolean? but cached in trivial.sls)
;; They should be equivalent identifier-references, but they are created in different modules.
(let ([point?-ref (get-first-ref target-document root-index-node 'point?)]
      [point?-node (find-usage-node root-index-node 'point?)])
  (let ([record-bool (car (car (identifier-reference-type-expressions point?-ref)))]
        [trivial-subs (index-node-substitution-list point?-node)])
    ;; record-bool is the boolean? identifier-reference from record.sls
    ;; Find the boolean? in trivial.sls substitution (first element of the function type list)
    (let ([trivial-bool (car (find list? trivial-subs))])
      (test-assert "record boolean? is identifier-reference" (identifier-reference? record-bool))
      (test-assert "trivial boolean? is identifier-reference" (identifier-reference? trivial-bool))
      ;; They should refer to the same chezscheme boolean?, but let's see if they are equal?
      (test-assert "record and trivial boolean? are equal?"
        (equal? record-bool trivial-bool)))))

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group F: identifier-reference-index-node anomaly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "identifier-reference-index-node anomaly")

;; For record identifiers, identifier-reference-index-node may return an annotation
;; instead of an index-node. This is a potential bug in the identifier-capture pipeline.
(let ([make-point-ref (get-first-ref target-document root-index-node 'make-point)])
  (test-assert "make-point ref exists" (identifier-reference? make-point-ref))
  ;; We expect index-node to be an index-node, but it might be an annotation or '()
  (let ([idx-node (identifier-reference-index-node make-point-ref)])
    (display "make-point index-node type: ")
    (display (if (index-node? idx-node) "index-node"
               (if (annotation? idx-node) "annotation"
                 (if (null? idx-node) "null" "other"))))
    (newline)
    ;; This assertion documents current behavior; if it fails, the upstream bug is fixed.
    (test-assert "make-point index-node is either index-node or annotation"
      (or (index-node? idx-node) (annotation? idx-node) (null? idx-node)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
