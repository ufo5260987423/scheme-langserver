;; -*- mode: scheme; coding: utf-8 -*-
;; Advanced Encryption Standard (AES), FIPS-197.
;; Copyright © 2009, 2010, 2012, 2017, 2018, 2022 Göran Weinholt <goran@weinholt.se>

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

;; This is an implementation of the Rijndael cipher as parameterized
;; by AES (the block length is 16 bytes, keys are 128, 192 or 256 bits
;; long with 10, 12 and 14 rounds respectively).

;; For usage etc see the manual.

;;; Implementation details

;; The main operations in the encryption and decryption procedures are
;; bitwise-xor, bitwise-and and bitwise-arithmetic-shift-right. The
;; operands are 32-bit integers, mostly from vector-ref. If your
;; implementation does flow analysis then it might be beneficial to
;; switch to bytevectors and bytevector-u32-native-ref.

;; The encryption and decryption procedures are unrolled for the
;; 128-bit key case. I would have unrolled the 192-bit and 256-bit
;; cases too, but Ikarus takes forever to expand the code then.

;; The performance will depend on your Scheme implementation. I'm
;; getting around 1.87 MB/s with a 128-bit key, Ikarus 64-bit revision
;; 1856 on an AMD Athlon(tm) 64 X2 Dual Core Processor 5600+. For
;; comparison: OpenSSL gets around 120 MB/s.

;; There's nothing original here, just a straightforward
;; implementation of some of the ideas presented in these papers:

;; @MISC{Daemen98aesproposal:,
;;     author = {Joan Daemen and Vincent Rijmen},
;;     title = {AES Proposal: Rijndael},
;;     year = {1998}
;; }

;; @misc{AES-FIPS,
;;    title = "Specification for the Advanced Encryption Standard (AES)",
;;    howpublished = "Federal Information Processing Standards Publication 197",
;;    year = "2001",
;;    url = "http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf"
;; }

;; @inproceedings{ BS08,
;;     author      = {Daniel J. Bernstein and Peter Schwabe},
;;     title       = {New {AES} software speed records},
;;     year        = {2008},
;;     booktitle   = {Progress in Cryptology - {INDOCRYPT 2008}},
;;     series      = {Lecture Notes in Computer Science},
;;     volume      = {5365},
;;     pages       = {322--336},
;;     publisher   = {Springer},
;; }
;; http://www.cryptojedi.org/papers/aesspeed-20080926.pdf
;; http://cr.yp.to/aes-speed/aesspeed-20080926.pdf

;; @MISC{Trichina04secureand,
;;     author = {E. Trichina and L. Korkishko},
;;     title = {Secure and Efficient AES Software Implementation for Smart Cards},
;;     year = {2004}
;; }
;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.58.2363

(library (industria crypto aes)
  (export expand-aes-key aes-encrypt!
          reverse-aes-schedule aes-decrypt!
          clear-aes-schedule!
          aes-ctr!
          aes-cbc-encrypt! aes-cbc-decrypt!

          make-aes-gcm-state
          aes-gcm-encrypt! aes-gcm-decrypt!?)
  (import (for (industria crypto private aes) expand)
          (for (only (srfi :1 lists) iota) expand)
          (only (srfi :1 lists) split-at concatenate)
          (rename (rnrs)
                  (bitwise-xor r6rs:bitwise-xor))


          (industria bytevectors))

  (define-syntax byte
    (lambda (x)
      (syntax-case x ()
        ((_ b n)
         (with-syntax ((shift (* (syntax->datum #'n) 8)))
           (if (zero? (syntax->datum #'n))
               #'(bitwise-and #xff b)
               #'(bitwise-and #xff (bitwise-arithmetic-shift-right b shift))))))))

  (define-syntax copy-byte
    (lambda (x)
      (syntax-case x ()
        ((_ b n)
         (with-syntax ((mask (bitwise-arithmetic-shift-left #xff (* (syntax->datum #'n) 8))))
           #'(bitwise-and mask b))))))

  (define-syntax bitwise-xor
    ;; This is for Ikarus, which allocates memory for bitwise-xor with
    ;; more than four arguments [2009-09-11].
    (lambda (x)
      (with-syntax ((foo 'bar))
        (syntax-case x ()
          ((_ a b c d e f) #'(r6rs:bitwise-xor a b c (r6rs:bitwise-xor d e f)))
          ((_ a b c d e) #'(r6rs:bitwise-xor a b c (r6rs:bitwise-xor d e)))
          ((_ x ...) #'(r6rs:bitwise-xor x ...))))))

  (define (uncat l n)
    (if (null? l)
        l
        (let-values (((this next) (split-at l n)))
          (cons this (uncat next n)))))

;;; Lookup tables

  ;; The math for these tables is in the private library.
  (let-syntax ((rcon-table
                (lambda (x)
                  (syntax-case x ()
                    ((_ n)
                     (with-syntax ((tab (list->vector
                                         (map (lambda (i)
                                                (bitwise-arithmetic-shift-left
                                                 (GFexpt 2 i) 24))
                                              (iota (syntax->datum #'n))))))
                       #''tab)))))
               (table
                (lambda (x)
                  (define (sbox-table t e0 e1 e2 e3)
                    (define (table-entry i)
                      (bitwise-ior (bitwise-arithmetic-shift-left
                                    (GF* (syntax->datum e0) (t i)) 24)
                                   (bitwise-arithmetic-shift-left
                                    (GF* (syntax->datum e1) (t i)) 16)
                                   (bitwise-arithmetic-shift-left
                                    (GF* (syntax->datum e2) (t i)) 8)
                                   (GF* (syntax->datum e3) (t i))))
                    (list->vector (map table-entry (iota 256))))
                  (syntax-case x (S invS)
                    ((_ S e0 e1 e2 e3)
                     (with-syntax ((tab (sbox-table S-box #'e0 #'e1 #'e2 #'e3)))
                       #''tab))
                    ((_ invS e0 e1 e2 e3)
                     (with-syntax ((tab (sbox-table inv-S-box #'e0 #'e1 #'e2 #'e3)))
                       #''tab))))))
    ;; Various values for the round constant (expt 2 n) in GF(2⁸).
    (define rcon (rcon-table 20))
    ;; Tables for multiplication and SubBytes from [Daemen98aesproposal]
    (define te0 (table S #b00000010 #b00000001 #b00000001 #b00000011))
    (define te1 (table S #b00000011 #b00000010 #b00000001 #b00000001))
    (define te2 (table S #b00000001 #b00000011 #b00000010 #b00000001))
    (define te3 (table S #b00000001 #b00000001 #b00000011 #b00000010))
    (define te4 (table S 1 1 1 1))
    (define td0 (table invS #b00001110 #b00001001 #b00001101 #b00001011))
    (define td1 (table invS #b00001011 #b00001110 #b00001001 #b00001101))
    (define td2 (table invS #b00001101 #b00001011 #b00001110 #b00001001))
    (define td3 (table invS #b00001001 #b00001101 #b00001011 #b00001110))
    (define td4 (table invS 1 1 1 1)))

;;; Enciphering

  (define (expand-aes-key key)
    (let* ((rounds (case (bytevector-length key)
                     ((128/8) 10)
                     ((192/8) 12)
                     ((256/8) 14)
                     (else (assertion-violation 'expand-aes-key "bad key size"
                                                (* 8 (bytevector-length key))))))
           (ret (make-vector (* 4 (+ 1 rounds))))
           (len (div (bytevector-length key) 4)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vector-set! ret i (bytevector-u32-ref key (* i 4) (endianness big))))
      (do ((schedlen (vector-length ret))
           (i len (+ i 1)))
          ((= i schedlen) ret)
        (let ((Wi-Nk (vector-ref ret (- i len)))
              (Wi-1 (vector-ref ret (- i 1))))
          (cond ((zero? (mod i len))
                 (vector-set!
                  ret i
                  (bitwise-xor
                   Wi-Nk
                   (vector-ref rcon (div (- i len) len))
                   (copy-byte (vector-ref te4 (byte Wi-1 2)) 3)
                   (copy-byte (vector-ref te4 (byte Wi-1 1)) 2)
                   (copy-byte (vector-ref te4 (byte Wi-1 0)) 1)
                   (copy-byte (vector-ref te4 (byte Wi-1 3)) 0))))
                ((and (> len 6) (= (mod i len) 4))
                 (vector-set!
                  ret i
                  (bitwise-xor
                   Wi-Nk
                   (copy-byte (vector-ref te4 (byte Wi-1 3)) 3)
                   (copy-byte (vector-ref te4 (byte Wi-1 2)) 2)
                   (copy-byte (vector-ref te4 (byte Wi-1 1)) 1)
                   (copy-byte (vector-ref te4 (byte Wi-1 0)) 0))))
                (else
                 (vector-set! ret i (bitwise-xor Wi-Nk Wi-1))))))))

  (define (aes-encrypt! in in-start out out-start key-schedule)
    ;; First add the first round key. Then do n-1 rounds of
    ;; SubBytes, ShiftRows, MixColumns and AddRoundKey.
    (do ((len (vector-length key-schedule))
         (i 4 (+ i 4))
         (a0 (bitwise-xor (vector-ref key-schedule 0)
                          (bytevector-u32-ref in in-start (endianness big)))
             (bitwise-xor (vector-ref key-schedule i)
                          (vector-ref te0 (byte a0 3))
                          (vector-ref te1 (byte a1 2))
                          (vector-ref te2 (byte a2 1))
                          (vector-ref te3 (byte a3 0))))
         (a1 (bitwise-xor (vector-ref key-schedule 1)
                          (bytevector-u32-ref in (+ in-start 4) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 1))
                          (vector-ref te0 (byte a1 3))
                          (vector-ref te1 (byte a2 2))
                          (vector-ref te2 (byte a3 1))
                          (vector-ref te3 (byte a0 0))))
         (a2 (bitwise-xor (vector-ref key-schedule 2)
                          (bytevector-u32-ref in (+ in-start 8) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 2))
                          (vector-ref te0 (byte a2 3))
                          (vector-ref te1 (byte a3 2))
                          (vector-ref te2 (byte a0 1))
                          (vector-ref te3 (byte a1 0))))
         (a3 (bitwise-xor (vector-ref key-schedule 3)
                          (bytevector-u32-ref in (+ in-start 12) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 3))
                          (vector-ref te0 (byte a3 3))
                          (vector-ref te1 (byte a0 2))
                          (vector-ref te2 (byte a1 1))
                          (vector-ref te3 (byte a2 0)))))
        ((= i (- len 4))
         ;; Finally do a round of SubBytes, ShiftRows and AddRoundKey.
         (bytevector-u32-set! out out-start
                              (bitwise-xor
                               (vector-ref key-schedule i)
                               (copy-byte (vector-ref te4 (byte a0 3)) 3)
                               (copy-byte (vector-ref te4 (byte a1 2)) 2)
                               (copy-byte (vector-ref te4 (byte a2 1)) 1)
                               (copy-byte (vector-ref te4 (byte a3 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 4)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 1))
                               (copy-byte (vector-ref te4 (byte a1 3)) 3)
                               (copy-byte (vector-ref te4 (byte a2 2)) 2)
                               (copy-byte (vector-ref te4 (byte a3 1)) 1)
                               (copy-byte (vector-ref te4 (byte a0 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 8)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 2))
                               (copy-byte (vector-ref te4 (byte a2 3)) 3)
                               (copy-byte (vector-ref te4 (byte a3 2)) 2)
                               (copy-byte (vector-ref te4 (byte a0 1)) 1)
                               (copy-byte (vector-ref te4 (byte a1 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 12)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 3))
                               (copy-byte (vector-ref te4 (byte a3 3)) 3)
                               (copy-byte (vector-ref te4 (byte a0 2)) 2)
                               (copy-byte (vector-ref te4 (byte a1 1)) 1)
                               (copy-byte (vector-ref te4 (byte a2 0)) 0))
                              (endianness big)))))

;;; Deciphering

  (define (reverse-aes-schedule key)
    ;; Reverse the key schedule, then do InvMixColumns
    (do ((ret (list->vector             ;XXX: spills key material as garbage
               (concatenate (reverse (uncat (vector->list key) 4)))))
         (i 4 (+ i 1)))
        ((= i (- (vector-length ret) 4))
         ret)
      (let ((temp (vector-ref ret i)))
        (vector-set! ret i
                     (bitwise-xor
                      (vector-ref td0 (copy-byte (vector-ref te4 (byte temp 3)) 0))
                      (vector-ref td1 (copy-byte (vector-ref te4 (byte temp 2)) 0))
                      (vector-ref td2 (copy-byte (vector-ref te4 (byte temp 1)) 0))
                      (vector-ref td3 (copy-byte (vector-ref te4 (byte temp 0)) 0)))))))

  (define (aes-decrypt! in in-start out out-start key-schedule)
    ;; First add the first round key. Then do n-1 rounds of
    ;; InvSubBytes, InvShiftRows, InvMixColumns and AddRoundKey.
    (do ((len (vector-length key-schedule))
         (i 4 (+ i 4))
         (a0 (bitwise-xor (vector-ref key-schedule 0)
                          (bytevector-u32-ref in in-start (endianness big)))
             (bitwise-xor (vector-ref key-schedule i)
                          (vector-ref td0 (byte a0 3))
                          (vector-ref td1 (byte a3 2))
                          (vector-ref td2 (byte a2 1))
                          (vector-ref td3 (byte a1 0))))
         (a1 (bitwise-xor (vector-ref key-schedule 1)
                          (bytevector-u32-ref in (+ in-start 4) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 1))
                          (vector-ref td0 (byte a1 3))
                          (vector-ref td1 (byte a0 2))
                          (vector-ref td2 (byte a3 1))
                          (vector-ref td3 (byte a2 0))))
         (a2 (bitwise-xor (vector-ref key-schedule 2)
                          (bytevector-u32-ref in (+ in-start 8) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 2))
                          (vector-ref td0 (byte a2 3))
                          (vector-ref td1 (byte a1 2))
                          (vector-ref td2 (byte a0 1))
                          (vector-ref td3 (byte a3 0))))
         (a3 (bitwise-xor (vector-ref key-schedule 3)
                          (bytevector-u32-ref in (+ in-start 12) (endianness big)))
             (bitwise-xor (vector-ref key-schedule (+ i 3))
                          (vector-ref td0 (byte a3 3))
                          (vector-ref td1 (byte a2 2))
                          (vector-ref td2 (byte a1 1))
                          (vector-ref td3 (byte a0 0)))))
        ((= i (- len 4))
         ;; Finally do a round of InvSubBytes, InvShiftRows and AddRoundKey.
         (bytevector-u32-set! out out-start
                              (bitwise-xor
                               (vector-ref key-schedule i)
                               (copy-byte (vector-ref td4 (byte a0 3)) 3)
                               (copy-byte (vector-ref td4 (byte a3 2)) 2)
                               (copy-byte (vector-ref td4 (byte a2 1)) 1)
                               (copy-byte (vector-ref td4 (byte a1 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 4)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 1))
                               (copy-byte (vector-ref td4 (byte a1 3)) 3)
                               (copy-byte (vector-ref td4 (byte a0 2)) 2)
                               (copy-byte (vector-ref td4 (byte a3 1)) 1)
                               (copy-byte (vector-ref td4 (byte a2 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 8)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 2))
                               (copy-byte (vector-ref td4 (byte a2 3)) 3)
                               (copy-byte (vector-ref td4 (byte a1 2)) 2)
                               (copy-byte (vector-ref td4 (byte a0 1)) 1)
                               (copy-byte (vector-ref td4 (byte a3 0)) 0))
                              (endianness big))
         (bytevector-u32-set! out (+ out-start 12)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 3))
                               (copy-byte (vector-ref td4 (byte a3 3)) 3)
                               (copy-byte (vector-ref td4 (byte a2 2)) 2)
                               (copy-byte (vector-ref td4 (byte a1 1)) 1)
                               (copy-byte (vector-ref td4 (byte a0 0)) 0))
                              (endianness big)))))

;;;

  (define (clear-aes-schedule! sched)
    (vector-fill! sched 0))

;;; CTR mode

  (define (aes-ctr! source source-start target target-start len sched ctr)
    (do ((block (make-bytevector 16))
         ;; XXX: ctr should wrap at 2^128-1. Will it *ever* wrap? Stay
         ;; tuned to find out!
         (ctr ctr (+ ctr 1))
         (s source-start (+ s 16))
         (t target-start (+ t 16)))
        ((>= s (+ source-start len))
         ctr)
      (bytevector-uint-set! block 0 ctr (endianness big) 16)
      (aes-encrypt! block 0 block 0 sched)
      (do ((end (min (+ s 16) (+ source-start len)))
           (i 0 (+ i 1))
           (s s (+ s 1))
           (t t (+ t 1)))
          ((= s end))
        (bytevector-u8-set! target t (fxxor (bytevector-u8-ref block i)
                                            (bytevector-u8-ref source s))))))

;;; CBC mode

  (define (aes-cbc-encrypt! source source-start target target-start len sched iv)
    (unless (fxzero? (fxand len 15))
      (assertion-violation 'aes-cbc-encrypt!
                           "The length has to be an integer multiple of 16" len))
    (do ((ss source-start (fx+ ss 16))
         (ts target-start (fx+ ts 16))
         (len len (fx- len 16)))
        ((fx<? len 16))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i 16))
        (bytevector-u8-set! iv i
                            (fxxor (bytevector-u8-ref iv i)
                                   (bytevector-u8-ref source (fx+ ss i)))))
      (aes-encrypt! iv 0 target ts sched)
      (bytevector-copy! target ts iv 0 16)))

  (define (aes-cbc-decrypt! source source-start target target-start len sched iv)
    (unless (fxzero? (fxand len 15))
      (assertion-violation 'aes-cbc-decrypt!
                           "The length has to be an integer multiple of 16" len))
    (do ((buf (make-bytevector 16))
         (ss source-start (fx+ ss 16))
         (ts target-start (fx+ ts 16))
         (len len (fx- len 16)))
        ((fx<? len 16))
      (aes-decrypt! source ss buf 0 sched)
      (do ((i 0 (fx+ i 1)))
          ((fx=? i 16))
        (bytevector-u8-set! buf i
                            (fxxor (bytevector-u8-ref iv i)
                                   (bytevector-u8-ref buf i))))
      (bytevector-copy! source ss iv 0 16)
      (bytevector-copy! buf 0 target ts 16)))

;;; AES in GCM mode (NIST Special Publication 800-38D)

  (define-record-type aes-gcm-state
    (sealed #t)
    (opaque #t)
    (fields sched
            H                           ;hash subkey for GHASH
            iv-length
            tag-length)
    (protocol
     (lambda (p)
       (lambda (sched iv-length tag-length)
         (let ((H (make-bytevector 16 0)))
           (aes-encrypt! H 0 H 0 sched)
           (p sched H iv-length tag-length))))))

  (define (inc32! bv)
    (let* ((len (bytevector-length bv))
           ;; XXX: can wraparound be allowed to happen?
           (n (bitwise-and #xffffffff (+ (bytevector-u32-ref bv (fx- len 4) (endianness big)) 1))))
      (bytevector-u32-set! bv (fx- len 4) n (endianness big))))

  ;; FIXME: Optimize! But keep this algorithm around for verification use.
  (define (gcm-mult! x y target)
    (define R (bitwise-arithmetic-shift-left #b11100001 120)) ;x^128 + x^7 + x^2 + x + 1
    (let ((X (bytevector-uint-ref x 0 (endianness big) 16))
          (Y (bytevector-uint-ref y 0 (endianness big) 16)))
      (do ((i 0 (fx+ i 1))
           (Z 0 (if (bitwise-bit-set? Y (fx- 127 i))
                    (bitwise-xor Z V)
                    Z))
           (V X (if (eqv? 0 (bitwise-and V 1))
                    (bitwise-arithmetic-shift-right V 1)
                    (bitwise-xor R (bitwise-arithmetic-shift-right V 1)))))
          ((fx=? i 128)
           (bytevector-uint-set! target 0 Z (endianness big) 16)))))

  (define (GHASH! source target H)
    (let ((X source)
          (Y (make-bytevector 16 0)))
      (do ((i 0 (fx+ i 16)))
          ((fx=? i (bytevector-length X))
           (bytevector-copy! Y 0 target 0 16))
        (bytevector-u32-native-set! Y 0 (bitwise-xor (bytevector-u32-native-ref Y 0)
                                                     (bytevector-u32-native-ref X i)))
        (bytevector-u32-native-set! Y 4 (bitwise-xor (bytevector-u32-native-ref Y 4)
                                                     (bytevector-u32-native-ref X (fx+ i 4))))
        (bytevector-u32-native-set! Y 8 (bitwise-xor (bytevector-u32-native-ref Y 8)
                                                     (bytevector-u32-native-ref X (fx+ i 8))))
        (bytevector-u32-native-set! Y 12 (bitwise-xor (bytevector-u32-native-ref Y 12)
                                                      (bytevector-u32-native-ref X (fx+ i 12))))
        (gcm-mult! Y H Y))))

  (define (GCTR! source si target ti len initial-counter-block sched)
    (let ((CB (make-bytevector 16))
          (tmp (make-bytevector 16)))
      (bytevector-copy! initial-counter-block 0 CB 0 16)
      (do ((i 0 (fx+ i 16))
           (si si (fx+ si 16))
           (ti ti (fx+ ti 16))
           (plen (fxand len -16)))
          ((fx=? i plen)
           ;; Finish the last partial block
           (aes-encrypt! CB 0 CB 0 sched)
           (do ((i i (fx+ i 1))
                (j 0 (fx+ j 1)))
               ((fx=? i len))
             (bytevector-u8-set! target (fx+ ti j)
                                 (fxxor (bytevector-u8-ref source (fx+ si j))
                                        (bytevector-u8-ref CB j)))))
        ;; Handle a full block
        (aes-encrypt! CB 0 tmp 0 sched)
        (inc32! CB)
        (do ((j 0 (fx+ j 1)))
            ((fx=? j 16))
          (bytevector-u8-set! target (fx+ ti j)
                              (fxxor (bytevector-u8-ref source (fx+ si j))
                                     (bytevector-u8-ref tmp j)))))))

  (define (padlen n)
    (fx- (fxand (fx+ n 15) -16) n))

  (define (aes-gcm-J0 H iv)
    (let ((J0 (make-bytevector 16)))
      (cond ((eqv? (bytevector-length iv) 96/8)
             (bytevector-copy! iv 0 J0 0 96/8)
             (bytevector-u32-set! J0 96/8 1 (endianness big)))
            (else
             (let ((padded-iv (make-bytevector
                               (fx+ 16 (fxand (fx+ (bytevector-length iv) 15) -16))
                               0)))
               (bytevector-copy! iv 0 padded-iv 0 (bytevector-length iv))
               (bytevector-u64-set! padded-iv (fx- (bytevector-length padded-iv) 8)
                                    (* 8 (bytevector-length iv)) (endianness big))
               (GHASH! padded-iv J0 H))))
      J0))

  (define (aes-gcm! source source-start target target-start len state J0)
    (let ((J (bytevector-copy J0)))
      (inc32! J)
      (GCTR! source source-start target target-start len J (aes-gcm-state-sched state))
      J0))

  (define (aes-gcm-hash! cipher cipher-start len state aad tag tag-start J0)
    (let ((u (padlen len))
          (v (padlen (bytevector-length aad))))
      (let ((S (bytevector-append aad
                                  (make-bytevector v 0)
                                  ;; FIXME: don't copy like this
                                  (subbytevector cipher cipher-start (+ cipher-start len))
                                  (make-bytevector u 0)
                                  (make-bytevector 16))))
        (bytevector-u64-set! S (fx- (bytevector-length S) 16)
                             (* 8 (bytevector-length aad)) (endianness big))
        (bytevector-u64-set! S (fx- (bytevector-length S) 8)
                             (* 8 len) (endianness big))
        (GHASH! S S (aes-gcm-state-H state))
        (GCTR! S 0 tag tag-start 16 J0 (aes-gcm-state-sched state)))))

  (define (aes-gcm-encrypt! source source-start target target-start len state iv
                            aad tag tag-start)
    (let ((J0 (aes-gcm-J0 (aes-gcm-state-H state) iv)))
      (aes-gcm! source source-start target target-start len state J0)
      (aes-gcm-hash! target target-start len state aad tag tag-start J0)))

  ;; (A world first? Interrobang-based decryption procedure).
  ;; The user must check that this returns the symbol 'ok.
  (define (aes-gcm-decrypt!? source source-start target target-start len state iv
                             aad tag tag-start)
    (let* ((J0 (aes-gcm-J0 (aes-gcm-state-H state) iv))
           (tag-len (aes-gcm-state-tag-length state))
           (vtag (make-bytevector tag-len)))
      (aes-gcm-hash! source source-start len state aad vtag 0 J0)
      (aes-gcm! source source-start target target-start len state J0)
      ;; Verify the tag. The caller has to check the return value.
      (do ((i 0 (+ i 1))
           (diff 0 (fxior diff
                          (fxxor
                           (bytevector-u8-ref tag (fx+ tag-start i))
                           (bytevector-u8-ref vtag i)))))
          ((= i tag-len)
           (if (eqv? diff 0) 'ok #f))))))
