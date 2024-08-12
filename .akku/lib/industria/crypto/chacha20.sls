;; -*- mode: scheme; coding: utf-8 -*-
;; ChaCha20
;; Copyright © 2020 Göran Weinholt <goran@weinholt.se>

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

;; This is a parameterized variant of ChaCha, originally described in:

;; "ChaCha, a variant of Salsa20" (2008) Daniel J. Bernstein

;; The ChaCha20 variant here is described in RFC 7539. The underlying
;; algorithm forms a source of pseudo-random bytes, which is used in
;; the ChaCha20 encryption algorithm.

(library (industria crypto chacha20)
  (export
    chacha20-block!
    chacha20-keystream
    chacha20-encrypt!
    chacha20-encrypt)
  (import
    (rnrs)
    (industria crypto private chacha20))

(define max-bytes (* 64 (- (expt 2 32) 1)))

;; Forms the string "expand 32-byte k"
(define c0 #x61707865)
(define c1 #x3320646e)
(define c2 #x79622d32)
(define c3 #x6b206574)

(define-syntax let*-quarter-round
  (syntax-rules ()
    ((_ ((a b c d) rest* ...)
        body* ...)
     (let* ((a (add32 a b)) (d (xor32 d a)) (d (rot32 d 16))
            (c (add32 c d)) (b (xor32 b c)) (b (rot32 b 12))
            (a (add32 a b)) (d (xor32 d a)) (d (rot32 d 8))
            (c (add32 c d)) (b (xor32 b c)) (b (rot32 b 7)))
       (let*-quarter-round (rest* ...) body* ...)))
    ((_ () body ...)
     (let ()
       body ...))))

(define (set32! bv i v)
  (bytevector-u32-set! bv (* i 4) v (endianness little)))

(define (get32 bv i)
  (bytevector-u32-ref bv (* i 4) (endianness little)))

(define (chacha20-block! out key block-count nonce)
  (let ((i0 c0)            (i1 c1)               (i2 c2)               (i3 c3)
        (i4 (get32 key 0)) (i5 (get32 key 1))    (i6 (get32 key 2))    (i7 (get32 key 3))
        (i8 (get32 key 4)) (i9 (get32 key 5))    (i10 (get32 key 6))   (i11 (get32 key 7))
        (i12 block-count)  (i13 (get32 nonce 0)) (i14 (get32 nonce 1)) (i15 (get32 nonce 2)))
    (let lp ((i 0)
             (s0  i0) (s1  i1) (s2  i2)  (s3  i3)  (s4  i4)  (s5  i5)  (s6  i6)  (s7  i7)
             (s8  i8) (s9  i9) (s10 i10) (s11 i11) (s12 i12) (s13 i13) (s14 i14) (s15 i15))
      (cond
        ((eqv? i 10)
         (set32! out 15 (add32 i15 s15))
         (set32! out 14 (add32 i14 s14))
         (set32! out 13 (add32 i13 s13))
         (set32! out 12 (add32 i12 s12))
         (set32! out 11 (add32 i11 s11))
         (set32! out 10 (add32 i10 s10))
         (set32! out 11 (add32 i11 s11))
         (set32! out 10 (add32 i10 s10))
         (set32! out 9 (add32 i9 s9))
         (set32! out 8 (add32 i8 s8))
         (set32! out 7 (add32 i7 s7))
         (set32! out 6 (add32 i6 s6))
         (set32! out 5 (add32 i5 s5))
         (set32! out 4 (add32 i4 s4))
         (set32! out 3 (add32 i3 s3))
         (set32! out 2 (add32 i2 s2))
         (set32! out 1 (add32 i1 s1))
         (set32! out 0 (add32 i0 s0)))
        (else
         (let*-quarter-round ((s0  s4 s8  s12)
                              (s1  s5 s9  s13)
                              (s2  s6 s10 s14)
                              (s3  s7 s11 s15)
                              (s0  s5 s10 s15)
                              (s1  s6 s11 s12)
                              (s2  s7 s8  s13)
                              (s3  s4 s9  s14))
           (lp (fx+ i 1)
               s0 s1 s2  s3  s4  s5  s6  s7
               s8 s9 s10 s11 s12 s13 s14 s15)))))))

(define (chacha20-keystream key block-count nonce)
  (define buf (make-bytevector (* 16 4)))
  (define offset 0)
  (define (read! bv start count)
    (cond
      ((> block-count #xffffffff)
       0)
      (else
       (when (eqv? offset (* 16 4))
         (set! block-count (+ block-count 1))
         (set! offset 0)
         (chacha20-block! buf key block-count nonce))
       (let ((k (fxmin count (fx- (* 16 4) offset))))
         (bytevector-copy! buf offset bv start k)
         (set! offset (fx+ offset k))
         k))))
  (define (get-position)
    (+ block-count offset))
  (define (set-position! pos)
    (let-values ([(block-count^ offset^) (div-and-mod pos (* 16 4))])
      (set! block-count block-count^)
      (set! offset offset^)
      (chacha20-block! buf key block-count nonce)))
  (define (close)
    (set! buf #f)
    (set! key #f)
    (set! block-count #f)
    (set! nonce #f))
  (chacha20-block! buf key block-count nonce)
  (make-custom-binary-input-port "chacha20 keystream"
                                 read! get-position set-position! close))

(define (chacha20-encrypt! source source-start target target-start len keystream)
  (do ((i 0 (fx+ i 1))
       (ss source-start (fx+ ss 1))
       (ts target-start (fx+ ts 1)))
      ((fx=? i len))
    (bytevector-u8-set! target ts (fxxor (get-u8 keystream) (bytevector-u8-ref source ss)))))

(define (chacha20-encrypt source source-start len keystream)
  (let ((target (make-bytevector len)))
    (chacha20-encrypt! source source-start target 0 len keystream)
    target)))
