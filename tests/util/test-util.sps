#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (rnrs (6))
  (srfi :64 testing)
  (scheme-langserver util association)
  (scheme-langserver util binary-search)
  (scheme-langserver util cartesian-product)
  (scheme-langserver util contain)
  (scheme-langserver util dedupe)
  (scheme-langserver util json)
  (scheme-langserver util path)
  (scheme-langserver util sub-list))

;; from test-association.sps
(let ([a-list '((b . 2) (a . 1))])
  (test-begin "make-alist")
    (test-equal a-list (make-alist 'b 2 'a 1))
  (test-end)
  (test-begin "assq-ref")
    (test-equal 1 (assq-ref a-list 'a))
  (test-end))

;; from test-binary-search.sps
(test-begin "simple binary-search ")
  (test-equal 
    (binary-search '#(request server-instance)
      (lambda (reference0 reference1)
        (string<=?
          (symbol->string reference0)
          (symbol->string reference1)))
      'server-instance)
    '(server-instance))
(test-end)

;; from test-cartesian-product.sps
(test-begin "cartesian-product")
  (test-equal '((())) (cartesian-product '(())))
  (test-equal '() (cartesian-product '()))
  (test-equal '() (cartesian-product '(1) '()))
  (test-equal '((1)) (cartesian-product '(1)))
  (test-equal '((1 2)) (cartesian-product '(1) '(2)))
  (test-equal '((1 2) (1 3)) (cartesian-product '(1) '(2 3)))
  (test-equal '((1 2) (1 3) (4 2) (4 3)) (cartesian-product '(1 4) '(2 3)))
  (test-equal '((1 2 0) (1 3 0) (4 2 0) (4 3 0)) (cartesian-product '(1 4) '(2 3) '(0)))
(test-end)

;; from test-contain.sps
(test-begin "contain")
  (test-equal #t (contain? '(1) 1))
  (test-equal #t (contain? '(#f) #f))
  (test-equal #f (contain? '(#t) #f))
(test-end)

;; from test-dedupe.sps
(test-begin "dedupe")
  (test-equal '(1) (dedupe '(1)))
  (test-equal '(1) (dedupe '(1 1)))
  (test-equal '(1 2) (dedupe '(1 2 1)))
(test-end)

(test-begin "ordered-dedupe")
  (test-equal '(1) (ordered-dedupe '(1)))
  (test-equal '(1) (ordered-dedupe '(1 1)))
  (test-equal '(1 2) (dedupe '(1 1 2)))
(test-end)

(test-begin "dedupe-deduped")
  (test-equal '(1) (dedupe-deduped '(1) '()))
  (test-equal '(2 1) (dedupe-deduped '() '(2 1)))
  (test-equal '(1) (dedupe-deduped '(1) '(1)))
  (test-equal '(2 1) (dedupe-deduped '(2 1) '(1)))
  (test-equal '(2 1) (dedupe-deduped '(1) '(2 1)))
  (test-equal '(2 1 4 3) (dedupe-deduped '(1 3) '(2 1 4)))
(test-end)

;; from test-json.sps
(let ([a-list '((a . 1) (b . 2))])
  (test-begin "read-json")
    (test-equal a-list (read-json "{\"a\":1,\"b\":2}"))
  (test-end)
  (test-begin "generate-json")
    (test-equal "{\"a\":1,\"b\":2}" (generate-json a-list))
  (test-end))

;; from test-sub-list.sps
(test-begin "find-intersection test")
  (test-equal '(c) (find-intersection '(b a bc c) '(c d) equal?))
(test-end)

;; from test-uri.sps
(test-begin "path->uri & uri->path")
  (let ([path "/.akku/lib/srfi/%3a13/srfi-13.scm"]
        [uri "file:///.akku/lib/srfi/%253a13/srfi-13.scm"])
    (test-equal uri (path->uri path))
    (test-equal path (uri->path uri)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
