#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver protocol apis workspace-symbol)
  (scheme-langserver util association))

(define (private-string-contains? str sub)
  (let ([str-len (string-length str)]
      [sub-len (string-length sub)])
    (if (zero? sub-len)
      #t
      (let loop ([i 0])
        (if (> (+ i sub-len) str-len)
          #f
          (or (string=? sub (substring str i (+ i sub-len)))
            (loop (+ i 1))))))))

(test-begin "workspace-symbol on util/")

(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [result-empty (workspace-symbol workspace '((query . "")))]
       [result-vec (workspace-symbol workspace '((query . "binary")))]
       [result-list (vector->list result-vec)])

  ;; Empty query should return many symbols
  (test-assert "empty query returns some symbols" (> (vector-length result-empty) 0))

  ;; Query "binary" should find binary-search related symbols
  (test-assert "binary query returns some symbols" (> (vector-length result-vec) 0))

  ;; All results should have name, kind, location
  (test-assert "results have name" 
    (and (> (length result-list) 0)
         (string? (assq-ref (car result-list) 'name))))

  (test-assert "results have location"
    (and (> (length result-list) 0)
         (not (null? (assq-ref (car result-list) 'location)))))

  ;; At least one result should contain "binary" in its name (case-insensitive)
  (test-assert "at least one result contains binary"
    (not (null? 
      (filter 
        (lambda (sym)
          (let ([name (string-downcase (assq-ref sym 'name))])
            (private-string-contains? name "binary")))
        result-list)))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
