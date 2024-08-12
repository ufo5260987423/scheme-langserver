;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2011, 2012 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(library (industria crypto math)
  (export invmod expt-mod div-mod sqrt-mod)
  (import (rnrs))

  (define (invmod a b)
    ;; Extended Euclidian algorithm. Used to find the inverse of a
    ;; modulo b.
    (let lp ((a a)
             (b b)
             (x0 0)
             (x1 1))
      (let-values (((q r) (div-and-mod b a)))
        (if (zero? r)
            x1
            (lp r a x1 (+ (* (- q) x1) x0))))))

  (define (expt-mod base exponent modulus)
    ;; Faster version of (mod (expt base exponent) modulus).
    (let lp ((base (if (negative? exponent)
                       (invmod (mod base modulus) modulus)
                       (mod base modulus)))
             (exponent (abs exponent))
             (result 1))
      (if (zero? exponent)
          result
          (lp (mod (* base base) modulus)
              (bitwise-arithmetic-shift-right exponent 1)
              (if (bitwise-bit-set? exponent 0)
                  (mod (* result base) modulus)
                  result)))))

  ;; Division modulo p
  (define (div-mod num den p)
    (mod (* num (expt-mod den -1 p)) p))

  (define (sqrt-mod n p)
    (let ((pow (/ (+ p 1) 4)))
      (unless (integer? pow)
        (error 'sqrt-mod "Composite p is not supported" p))
      (let ((ret (expt-mod n pow p)))
        (unless (= (mod (* ret ret) p) n)
          (error 'sqrt-mod "No square root found" n p))
        ret))))
