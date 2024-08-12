;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2012, 2018 Göran Weinholt <goran@weinholt.se>

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

;; Procedures for dealing with Mr. Costello's puny codes.

;; RFC3492 - Punycode: A Bootstring encoding of Unicode for
;; Internationalized Domain Names in Applications (IDNA)

(library (industria dns punycode)
  (export punycode->string string->punycode)
  (import (rnrs)
          (only (srfi :1 lists) split-at)
          (srfi :26 cut)
          (industria bytevectors))

  (define (print . x) (for-each display x) (newline))

  (define base 36)
  (define tmin 1)
  (define tmax 26)
  (define skew 38)
  (define damp 700)
  (define initial-bias 72)
  (define initial-n 128)
  (define delimiter (char->integer #\-))

;;; Decoding

  (define (adapt delta numpoints firsttime?)
    (let ((delta (div delta (if firsttime? damp 2))))
      (do ((delta (+ delta (div delta numpoints))
                  (div delta (- base tmin)))
           (k 0 (+ k base)))
          ((<= delta (div (* (- base tmin) tmax) 2))
           (+ k (div (* delta (+ (- base tmin) 1))
                     (+ delta skew)))))))

  (define (decode-basic bv)
    (cond ((bytevector-u8-index-right bv delimiter) =>
           (lambda (i)
             (let ((points (bytevector->u8-list (subbytevector bv 0 i))))
               (unless (for-all (cut < <> #x80) points)
                 (error 'punycode->string
                        "non-basic code points before last delimiter"))
               (values points (+ i 1) (+ i 1)))))
          (else (values '() 0 1))))

  (define (code-point->value code)
    (cond ((<= (char->integer #\a) code (char->integer #\z))
           (- code (char->integer #\a)))
          ((<= (char->integer #\A) code (char->integer #\Z))
           (- code (char->integer #\A)))
          ((<= (char->integer #\0) code (char->integer #\9))
           (+ (- code (char->integer #\0)) 26))
          (else
           (error 'punycode->string "invalid code-point" code))))

  (define (insert list i c)
    (let-values (((h t) (split-at list i)))
      (append h (cons c t))))

  (define (clamp k bias)
    (cond ((<= k (+ bias tmin)) tmin)
          ((>= k (+ bias tmax)) tmax)
          (else (- k bias))))

  (define (punycode->string bv)
    (let-values (((output index numpoints) (decode-basic bv)))
      (let lp ((index index)
               (oldi 0)
               (bias initial-bias)
               (n initial-n)
               (output output)
               (numpoints numpoints))
        (if (= index (bytevector-length bv))
            (list->string (map integer->char output))
            (let lpdelta ((index index)
                          (i oldi)
                          (k base)
                          (w 1))
              (let* ((digit (code-point->value (bytevector-u8-ref bv index)))
                     (i (+ i (* digit w)))
                     (t (clamp k bias)))
                (if (< digit t)
                    (let ((n* (+ n (div i numpoints)))
                          (i* (mod i numpoints)))
                      (lp (+ index 1)
                          (+ i* 1)
                          (adapt (- i oldi) numpoints (zero? oldi))
                          n*
                          (insert output i* n*)
                          (+ numpoints 1)))
                    (lpdelta (+ index 1)
                             i
                             (+ k base)
                             (* w (- base t))))))))))

  (define xlate (string->utf8 "abcdefghijklmnopqrstuvwxyz0123456789"))

  (define (string->punycode s)
    (call-with-bytevector-output-port
      (lambda (out)
        (define (putc c) (put-u8 out (bytevector-u8-ref xlate c)))
        (let*-values (((bas ext)
                       (partition (cut < <> #x80)
                                  (map char->integer (string->list s))))
                      ((b) (length bas)))
          (for-each (cut put-u8 out <>) bas)
          (unless (zero? b)
            (put-u8 out delimiter))
          (let lp ((n initial-n)
                   (bias initial-bias)
                   (delta 0)
                   (h b))
            (unless (= h (string-length s))
              (let* ((m (apply min (filter (cut >= <> n) ext)))
                     (delta (+ delta (* (- m n) (+ h 1))))
                     (n m))
                (let lp* ((index 0)
                          (bias bias)
                          (delta delta)
                          (h h))
                  (if (= index (string-length s))
                      (lp (+ n 1) bias (+ delta 1) h)
                      (let ((c (char->integer (string-ref s index))))
                        (cond ((< c n)
                               (lp* (+ index 1) bias (+ delta 1) h))
                              ((= c n)
                               (let lp** ((q delta)
                                          (k base))
                                 (let ((t (clamp k bias)))
                                   (cond ((< q t)
                                          (putc q)
                                          (lp* (+ index 1)
                                               (adapt delta (+ h 1) (= h b))
                                               0
                                               (+ h 1)))
                                         (else
                                          (putc (+ t (mod (- q t) (- base t))))
                                          (lp** (div (- q t) (- base t))
                                                (+ k base)))))))
                              (else
                               (lp* (+ index 1) bias delta h))))))))))))))
