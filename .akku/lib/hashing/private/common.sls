;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashing private common)
  (export iota decode-coefficients symcat)
  (import (rnrs))

(define (iota n)
  (unless (>= n 0)
    (error 'iota "Argument must be non-negative" n))
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (- n 1) acc)))))

(define (decode-coefficients coeffs)
  (do ((i coeffs (cdr i))
       (p 0 (bitwise-ior p (bitwise-arithmetic-shift-left 1 (car i)))))
      ((null? i) p)))

(define (symcat name suffix)
  (datum->syntax name (string->symbol (string-append
                                       (symbol->string (syntax->datum name))
                                       suffix)))))
