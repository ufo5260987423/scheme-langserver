;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Elliptic Curve Diffie-Hellman (ECDH)

;; RFC7748 Elliptic Curves for Security

(library (industria crypto ecdh)
  (export
    make-ecdh-curve25519-secret
    make-ecdh-curve448-secret
    ecdh-curve25519
    ecdh-curve448
    X25519
    X448)
  (import
    (rnrs (6))
    (industria bytevectors)
    (industria crypto entropy)
    (industria crypto math))

(define X25519-p (- (expt 2 255) 19))
(define X25519-a24 (/ (- 486662 2) 4))

(define X448-p (- (expt 2 448) (expt 2 224) 1))
(define X448-a24 (/ (- 156326 2) 4))

(define (decode-little-endian bv bits)
  (bytevector-uint-ref bv 0 (endianness little) (fxdiv (fx+ bits 7) 8)))

(define (decode-u-coordinate bv bits)
  (let ((bv (bytevector-copy bv)))
    (when (> (mod bits 8) 0)
      (let* ((n-1 (fx- (bytevector-length bv) 1))
             (byte (bytevector-u8-ref bv n-1))
             (mask (- (bitwise-arithmetic-shift-left 1 (mod bits 8)) 1)))
        (bytevector-u8-set! bv n-1 (fxand byte mask))))
    (decode-little-endian bv bits)))

(define (encode-u-coordinate u bits p)
  (uint->bytevector (mod u p)
                    (endianness little)
                    (fxdiv (+ bits 7) 8)))

(define (decode-scalar/25519 k)
  (let ((bv (bytevector-copy k)))
    (bytevector-u8-set! bv 0 (fxand 248 (bytevector-u8-ref bv 0)))
    (bytevector-u8-set! bv 31 (fxior 64 (fxand 127 (bytevector-u8-ref bv 31))))
    (decode-u-coordinate bv 255)))

(define (decode-scalar/448 k)
  (let ((bv (bytevector-copy k)))
    (bytevector-u8-set! bv 0 (fxand 252 (bytevector-u8-ref bv 0)))
    (bytevector-u8-set! bv 55 (fxior 128 (bytevector-u8-ref bv 55)))
    (decode-u-coordinate bv 448)))

(define (cswap swap x2 x3)
  (let ((dummy (bitwise-and (fx- swap) (bitwise-xor x2 x3))))
    (values (bitwise-xor x2 dummy)
            (bitwise-xor x3 dummy))))

(define (X-n k u bits p a24)
  (let lp ((t (- bits 1)) (x1 u) (x2 1) (z2 0) (x3 u) (z3 1) (swap 0))
    (cond ((fx=? t -1)
           (let-values ([(x2 _x3) (cswap swap x2 x3)]
                        [(z2 _z3) (cswap swap z2 z3)])
             (mod (* x2 (expt-mod z2 (- p 2) p)) p)))
          (else
           (let* ((k_t (bitwise-and (bitwise-arithmetic-shift-right k t) 1))
                  (swap (bitwise-xor swap k_t)))
             (let-values ([(x2 x3) (cswap swap x2 x3)]
                          [(z2 z3) (cswap swap z2 z3)])
               (let* ((swap k_t)
                      (A (mod (+ x2 z2) p))
                      (AA (expt-mod A 2 p))
                      (B (mod (- x2 z2) p))
                      (BB (expt-mod B 2 p))
                      (E (mod (- AA BB) p))
                      (C (mod (+ x3 z3) p))
                      (D (mod (- x3 z3) p))
                      (DA (mod (* D A) p))
                      (CB (mod (* C B) p)))
                 (let ((x3 (expt-mod (+ DA CB) 2 p))
                       (z3 (mod (* x1 (expt-mod (- DA CB) 2 p)) p))
                       (x2 (mod (* AA BB) p))
                       (z2 (mod (* E (+ AA (* a24 E))) p)))
                   (lp (fx- t 1) x1 x2 z2 x3 z3 swap)))))))))

(define (X25519 k u)
  (define p X25519-p)
  (define a24 X25519-a24)
  (define bits 255)
  (let ((k (if (number? k) k (decode-scalar/25519 k)))
        (u (if (number? u) u (decode-u-coordinate u bits))))
    (encode-u-coordinate (X-n k u bits p a24)
                         bits X25519-p)))

(define (X448 k u)
  (define p X448-p)
  (define a24 X448-a24)
  (define bits 448)
  (let ((k (if (number? k) k (decode-scalar/448 k)))
        (u (if (number? u) u (decode-u-coordinate u bits))))
    (encode-u-coordinate (X-n k u bits p a24)
                         bits X448-p)))

;;; ECDH with X25519 and X448

;; https://git.gnupg.org/cgi-bin/gitweb.cgi?p=libgcrypt.git;a=commit;h=bf76acbf0da6b0f245e491bec12c0f0a1b5be7c9

;;   May the Fourth Be With You: A Microarchitectural Side Channel Attack
;;   on Real-World Applications of Curve25519
;;   by Daniel Genkin, Luke Valenta, and Yuval Yarom

;; This code performs output validation by checking for zero and input
;; validation by comparing with a list of bad points. A reference for
;; the bad Curve25519 points is https://cr.yp.to/ecdh.html, but
;; there's nothing about bad Curve448 points.

(define curve25519-bad-points
  '(#x0000000000000000000000000000000000000000000000000000000000000000
    #x0000000000000000000000000000000000000000000000000000000000000001
    #x00b8495f16056286fdb1329ceb8d09da6ac49ff1fae35616aeb8413b7c7aebe0
    #x57119fd0dd4e22d8868e1c58c45c44045bef839c55b1d0b1248c50a3bc959c5f
    #x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
    #x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
    #x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee))

(define (bad-point? K bytes bad-points)
  (exists
   (lambda (bad-point)
     (bytevector=?/constant-time K (uint->bytevector bad-point (endianness little) bytes)))
   bad-points))

(define (make-ecdh-curve25519-secret)
  (let* ((a (make-random-bytevector 32))
         (K_A (X25519 a 9)))
    (values a K_A)))

(define (ecdh-curve25519 a K_B)
  (and (not (bad-point? K_B 32 curve25519-bad-points))
       (let ((K (X25519 a K_B)))
         (and (not (bad-point? K 32 '(0))) K))))

(define curve448-bad-points
  '(0 1))                                 ;???

(define (make-ecdh-curve448-secret)
  (let* ((a (make-random-bytevector 56))
         (K_A (X448 a 5)))
    (values a K_A)))

(define (ecdh-curve448 a K_B)
  (and (not (bad-point? K_B 56 curve448-bad-points))
       (let ((K (X448 a K_B)))
         (and (not (bad-point? K 56 '(0))) K)))))
