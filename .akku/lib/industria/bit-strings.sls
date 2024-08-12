;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Bit strings

;; A data type that represents a sequence of bits.

(library (industria bit-strings)
  (export
    make-bit-string
    bytevector->bit-string
    integer->bit-string

    bit-string-length
    bit-string-unused

    bit-string->bytevector              ;bits are left-aligned
    bit-string->integer

    bit-string-bit-set?
    bit-string=?)
  (import
    (rnrs (6)))

(define-record-type bit-string
  (fields length >bytevector))

(define (bit-string-unused bs)
  (bitwise-and (- (bit-string-length bs)) 7))

(define (bit-string->integer bs)
  (bitwise-arithmetic-shift-right
   (bytevector-uint-ref (bit-string->bytevector bs) 0 (endianness big)
                        (bytevector-length (bit-string->bytevector bs)))
   (bit-string-unused bs)))

(define (bit-string-bit-set? bs i)
  (and (< i (bit-string-length bs))
       (let ((byte (fxarithmetic-shift-right i 3))
             (index (fxand i 7)))
         (fxbit-set? (bytevector-u8-ref (bit-string->bytevector bs) byte)
                     (fx- 7 index)))))

(define (bytevector->bit-string bv length)
  (make-bit-string length bv))

(define (integer->bit-string int length)
  (let* ((unused (bitwise-and (- length) 7))
         (bv (make-bytevector (fxarithmetic-shift-right (+ length unused) 3) 0)))
    (unless (zero? (bytevector-length bv))
      (bytevector-uint-set! bv 0 (bitwise-arithmetic-shift-left int unused)
                            (endianness big) (bytevector-length bv)))
    (bytevector->bit-string bv length)))

(define (bit-string=? b0 b1)
  (and (= (bit-string-length b0) (bit-string-length b1))
       (bytevector=? (bit-string->bytevector b0)
                     (bit-string->bytevector b1)))))
