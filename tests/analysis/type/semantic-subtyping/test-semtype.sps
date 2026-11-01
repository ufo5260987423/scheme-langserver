#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)

  (scheme-langserver analysis type semantic-subtyping semtype))

;; ---------------------------------------------------------------------
;; phase 1 unit tests: base-type bitset layer
;; ---------------------------------------------------------------------

(test-begin "sem:convert base predicates")
  ;leaf predicates map to their own bit
  (test-assert (sem:type? (sem:convert 'fixnum?)))
  (test-assert (sem:is-empty?
    (sem:intersect (sem:convert 'fixnum?) (sem:convert 'string?))))
  ;compound predicates expand to leaf unions
  (test-assert
    (sem:subtype? (sem:convert 'fixnum?) (sem:convert 'integer?)))
  (test-assert
    (sem:subtype? (sem:convert 'integer?) (sem:convert 'rational?)))
  (test-assert
    (sem:subtype? (sem:convert 'rational?) (sem:convert 'real?)))
  (test-assert
    (sem:subtype? (sem:convert 'real?) (sem:convert 'complex?)))
  (test-assert
    (sem:subtype? (sem:convert 'complex?) (sem:convert 'number?)))
  ;symmetric expansion: integer? = fixnum? ∪ bignum?
  (test-assert
    (sem:equivalent? (sem:convert 'integer?)
      (sem:union (sem:convert 'fixnum?) (sem:convert 'bignum?))))
  ;undetermined inputs convert to #f and stay permissive
  (test-equal #f (sem:convert 'something?))
  (test-equal #f (sem:convert 'inner:list?))
  (test-assert (sem:subtype? (sem:convert 'fixnum?) (sem:convert 'something?)))
  (test-assert (sem:subtype? #f (sem:convert 'string?)))
(test-end)

(test-begin "sem:void is positive-only")
  ;void? is excluded from the complement universe
  (test-assert
    (sem:is-empty? (sem:intersect (sem:convert 'inner:void?) (sem:complement (sem:convert 'inner:void?)))))
  ;but a non-void type's complement is genuinely inhabited
  ;(fixnum ∩ ¬string = fixnum ≠ ∅)
  (test-assert
    (not (sem:is-empty? (sem:intersect (sem:convert 'fixnum?) (sem:complement (sem:convert 'string?))))))
  ;union with own complement = universe (void stays out)
  (test-assert
    (sem:equivalent? (sem:universe)
      (sem:union (sem:convert 'string?) (sem:complement (sem:convert 'string?)))))
(test-end)

(test-begin "sem:restrict/sem:remove hooks")
  ;restrict = intersect (occurrence-typing then-branch)
  (test-assert
    (sem:equivalent? (sem:convert 'fixnum?)
      (sem:restrict (sem:convert 'number?) (sem:convert 'fixnum?))))
  ;remove = diff (occurrence-typing else-branch); removing the whole
  ;type yields empty (unreachable branch marker)
  (test-assert
    (sem:is-empty? (sem:remove (sem:convert 'fixnum?) (sem:convert 'integer?))))
  (test-assert
    (sem:equivalent? (sem:convert 'flonum?)
      (sem:remove (sem:convert 'real?) (sem:convert 'rational?))))
(test-end)

;; ---------------------------------------------------------------------
;; property tests (Kent tutorial §6 strategy): random types built from
;; leaf unions, set axioms as invariants
;; ---------------------------------------------------------------------

(define TEST-LEAVES
  '(null? boolean? char? string? symbol?
    fixnum? bignum? ratnum? flonum? cflonum? eof-object? procedure?))

(define (random-semtype)
  (let loop ([leaves TEST-LEAVES] [acc (sem:empty)])
    (if (null? leaves)
      acc
      (loop (cdr leaves)
        (if (zero? (random 2))
          (sem:union acc (sem:convert (car leaves)))
          acc)))))

(define (sem-algebra-equal? s t)
  (= (sem:type-base s) (sem:type-base t)))

(test-begin "sem:set axioms over random unions")
  (let check ([i 0] [ok #t])
    (if (< i 500)
      (let ([s (random-semtype)]
            [t (random-semtype)]
            [u (random-semtype)])
        (check (+ i 1)
          (and ok
            ;S <: T  <=>  S ∖ T = ∅        (the algorithm's entry point)
            (eq? (sem:subtype? s t) (sem:is-empty? (sem:diff s t)))
            ;commutativity
            (sem-algebra-equal? (sem:union s t) (sem:union t s))
            (sem-algebra-equal? (sem:intersect s t) (sem:intersect t s))
            ;distributivity
            (sem-algebra-equal?
              (sem:intersect s (sem:union t u))
              (sem:union (sem:intersect s t) (sem:intersect s u)))
            ;De Morgan
            (sem-algebra-equal?
              (sem:complement (sem:union s t))
              (sem:intersect (sem:complement s) (sem:complement t)))
            ;S ∩ ¬S = ∅ ; S ∪ ¬S = universe
            (sem:is-empty? (sem:intersect s (sem:complement s)))
            (sem-algebra-equal?
              (sem:union s (sem:complement s))
              (sem:universe))
            ;diff = intersect with complement
            (sem-algebra-equal?
              (sem:diff s t)
              (sem:intersect s (sem:complement t))))))
      (test-assert ok)))
(test-end)

(test-begin "sem:pair sort")
  ;covariance in both components
  (test-assert
    (sem:subtype?
      (sem:convert '(inner:pair? fixnum? string?))
      (sem:convert '(inner:pair? number? string?))))
  (test-assert
    (sem:subtype?
      (sem:convert '(inner:pair? number? fixnum?))
      (sem:convert '(inner:pair? number? integer?))))
  ;disjoint cars kill the subtype relation
  (test-assert
    (not (sem:subtype?
      (sem:convert '(inner:pair? fixnum? string?))
      (sem:convert '(inner:pair? string? string?)))))
  ;reflexivity
  (test-assert
    (sem:equivalent?
      (sem:convert '(inner:pair? number? number?))
      (sem:convert '(inner:pair? number? number?))))
  ;a pair is not a base value, and vice versa
  (test-assert
    (not (sem:subtype? (sem:convert '(inner:pair? number? number?)) (sem:convert 'number?))))
  (test-assert
    (not (sem:subtype? (sem:convert 'number?) (sem:convert '(inner:pair? number? number?)))))
(test-end)

(test-begin "sem:list sugar")
  ;list(T) <: list(T') when T <: T'
  (test-assert
    (sem:subtype?
      (sem:convert '(inner:list? fixnum?))
      (sem:convert '(inner:list? number?))))
  ;element disjointness kills the relation
  (test-assert
    (not (sem:subtype?
      (sem:convert '(inner:list? fixnum?))
      (sem:convert '(inner:list? string?)))))
  ;null is a list of anything
  (test-assert
    (sem:subtype? (sem:convert 'null?) (sem:convert '(inner:list? number?))))
  ;a list is not a pair-free base type
  (test-assert
    (not (sem:subtype? (sem:convert '(inner:list? number?)) (sem:convert 'number?))))
  ;list <: list reflexivity through the box cycle terminates
  (test-assert
    (sem:equivalent?
      (sem:convert '(inner:list? number?))
      (sem:convert '(inner:list? number?))))
(test-end)

(test-begin "sem:vector sort")
  (test-assert
    (sem:subtype?
      (sem:convert '(inner:vector? fixnum?))
      (sem:convert '(inner:vector? number?))))
  (test-assert
    (not (sem:subtype?
      (sem:convert '(inner:vector? string?))
      (sem:convert '(inner:vector? number?)))))
  (test-assert
    (not (sem:subtype? (sem:convert '(inner:vector? number?)) (sem:convert 'number?))))
(test-end)

(test-begin "sem:arrow sort")
  ;ret covariant: (fixnum <- (number?)) <: (number <- (number?))
  (test-assert
    (sem:subtype?
      (sem:convert '(fixnum? <- (number?)))
      (sem:convert '(number? <- (number?)))))
  ;param contravariant: (number <- (fixnum?)) <: (number <- (integer?))
  ;i.e. a function accepting fixnums also accepts... target domain
  ;integer? must be <: source domain fixnum? -- no: use the sound side
  (test-assert
    (sem:subtype?
      (sem:convert '(number? <- (number?)))
      (sem:convert '(number? <- (fixnum?)))))
  ;disjoint domains kill the relation
  (test-assert
    (not (sem:subtype?
      (sem:convert '(number? <- (fixnum?)))
      (sem:convert '(number? <- (string?))))))
  ;disjoint codomains kill the relation
  (test-assert
    (not (sem:subtype?
      (sem:convert '(fixnum? <- (number?)))
      (sem:convert '(string? <- (number?))))))
  ;an arrow is not a base value
  (test-assert
    (not (sem:subtype? (sem:convert '(number? <- (number?))) (sem:convert 'number?))))
  ;base values are not arrows
  (test-assert
    (not (sem:subtype? (sem:convert 'number?) (sem:convert '(number? <- (number?))))))
  ;lambdas with undetermined parts stay permissive
  (test-equal #f (sem:convert '(something? <- (number?))))
(test-end)


(test-begin "sem:arrow Thm 2.17 combination cases")
  ;the completeness case the conservative single rule missed: a family
  ;of per-class arrows whose combination covers the whole numeric tower
  ;subtypes the generic arrow, though no single member does.
  (let ([per-class
          (sem:intersect
            (sem:convert '(integer? <- (integer?)))
            (sem:intersect
              (sem:convert '(ratnum? <- (ratnum?)))
              (sem:intersect
                (sem:convert '(flonum? <- (flonum?)))
                (sem:convert '(cflonum? <- (cflonum?))))))]
      [generic (sem:convert '(number? <- (number?)))])
    (test-assert "combination of per-class arrows <: generic arrow"
      (sem:subtype? per-class generic))
    (test-assert "generic arrow is NOT <: any single per-class arrow"
      (not (sem:subtype? generic (sem:convert '(integer? <- (integer?)))))))
  ;the old single-rule results still hold
  (test-assert
    (sem:subtype? (sem:convert '(fixnum? <- (number?))) (sem:convert '(number? <- (number?)))))
  (test-assert
    (not (sem:subtype? (sem:convert '(number? <- (fixnum?))) (sem:convert '(number? <- (string?))))))
(test-end)

(test-begin "sem:memos are per-thread and clearable")
  ;convert caching: same tree twice returns the same record
  (test-assert
    (eq? (sem:convert '(inner:pair? fixnum? string?))
      (sem:convert '(inner:pair? fixnum? string?))))
  ;empty-memo: repeated deep queries stay consistent after clearing
  (let ([t (sem:convert '(inner:list? fixnum?))]
      [u (sem:convert '(inner:list? number?))])
    (test-assert (sem:subtype? t u))
    (sem:clear-memos!)
    (test-assert (sem:subtype? t u)))
(test-end)

(test-begin "sem:unknown vs top")
  ;#f stays permissive in judgments (the existing contract)
  (test-assert (sem:subtype? #f (sem:convert 'number?)))
  (test-assert (sem:subtype? (sem:convert 'number?) #f))
  ;sem:universe is the real top, not #f
  (test-assert (sem:subtype? (sem:convert 'number?) (sem:universe)))
  (test-assert (not (sem:subtype? (sem:universe) (sem:convert 'number?))))
  (test-assert (sem:is-empty? (sem:complement (sem:universe))))
  ;its complement is genuinely inhabited
  (test-assert
    (not (sem:is-empty? (sem:diff (sem:universe) (sem:convert 'number?)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
