;; -*- mode: scheme; coding: utf-8 -*-
;; Slow implementation of the Data Encryption Standard (DES), FIPS 46.
;; Copyright © 2009, 2012 Göran Weinholt <goran@weinholt.se>

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

;; TODO: CBC, CFB, OFB, CTR

;; TODO: optimize, if there is any point in having this be faster.
;; Schneier's book should be useful for that. At the same time it
;; might be useful to sort out all the crazy bit orders. In some
;; places they used IBM's crazy reverse bit order, and in some they
;; use the One True order (lowest bit is bit zero). Or maybe I'm just
;; doing it wrong... :)

(library (industria crypto des)
  (export des-key-bad-parity? des! permute-key des-crypt
          tdea-permute-key tdea-encipher! tdea-decipher!
          tdea-cbc-encipher!
          tdea-cbc-decipher!)
  (import (except (rnrs) bitwise-rotate-bit-field))

  (define (bitwise-rotate-bit-field n start end count)
    ;; From R6RS, not yet implemented in Ikarus.
    (let ((width (- end start)))
      (if (positive? width)
          (let* ((count (mod count width))
                 (field0 (bitwise-bit-field n start end))
                 (field1 (bitwise-arithmetic-shift-left field0 count))
                 (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
                 (field (bitwise-ior field1 field2)))
            (bitwise-copy-bit-field n start end field))
          n)))

  (define (split-number x parts per)
    (do ((i 0 (fx+ i 1))
         (x x (bitwise-arithmetic-shift-right x per))
         (ret '() (cons (bitwise-and x (- (bitwise-arithmetic-shift-left 1 per) 1))
                        ret)))
        ((fx=? i parts) ret)))

  (define (join-number x per)
    (do ((x x (cdr x))
         (ret 0 (bitwise-ior (car x) (bitwise-arithmetic-shift-left ret per))))
        ((null? x) ret)))

  (define (permute input inlen table)
    ;; This is basically what makes this library so slow. Among other things.
    (do ((i 0 (fx+ i 1))
         (ret 0 (if (bitwise-bit-set? input (fx- inlen (bytevector-u8-ref table i)))
                    (bitwise-ior ret (bitwise-arithmetic-shift-left
                                      1 (- (bytevector-length table) i 1)))
                    ret)))
        ((fx=? i (bytevector-length table)) ret)))

  ;; Returns either #f or the index of the first bad byte.
  (define (des-key-bad-parity? key)
    (let lp ((i 0))
      (cond ((= i (bytevector-length key)) #f)
            ((even? (fxbit-count (bytevector-u8-ref key i))) i)
            (else (lp (+ i 1))))))

  ;; Takes an eight-byte bytevector and returns a list of 16 keys.
  (define (permute-key key)
    (define pc1
      '#vu8(57 49 41 33 25 17 9 1 58 50 42 34 26 18
               10 2 59 51 43 35 27 19 11 3 60 52 44 36
               63 55 47 39 31 23 15 7 62 54 46 38 30 22
               14 6 61 53 45 37 29 21 13 5 28 20 12 4))
    (define pc2
      '#vu8(14 17 11 24 1 5 3 28 15 6 21 10
               23 19 12 4 26 8 16 7 27 20 13 2
               41 52 31 37 47 55 30 40 51 45 33 48
               44 49 39 56 34 53 46 42 50 36 29 32))
    (let ((CD (permute (bytevector-u64-ref key 0 (endianness big)) 64 pc1)))
      (do ((rotates '(1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1 0)
                    (cdr rotates))
           (C (bitwise-bit-field CD 28 56)
              (bitwise-rotate-bit-field C 0 28 (car rotates)))
           (D (bitwise-bit-field CD 0 28)
              (bitwise-rotate-bit-field D 0 28 (car rotates)))
           (subkeys '()
                    (cons (permute (bitwise-ior (bitwise-arithmetic-shift-left C 28) D)
                                   56 pc2)
                          subkeys)))
          ((null? rotates)
           (cdr (reverse subkeys))))))

  ;; Initial Permutation
  (define (IP m)
    (define p
      '#vu8(58 50 42 34 26 18 10 2 60 52 44 36 28 20 12 4
               62 54 46 38 30 22 14 6 64 56 48 40 32 24 16 8
               57 49 41 33 25 17 9 1 59 51 43 35 27 19 11 3
               61 53 45 37 29 21 13 5 63 55 47 39 31 23 15 7))
    (do ((i 0 (fx+ i 1))
         (m0 0 (if (bitwise-bit-set? m (fx- (bytevector-u8-ref p i) 1))
                   (bitwise-ior m0 (bitwise-arithmetic-shift-left 1 i))
                   m0)))
        ((fx=? i 64) m0)))

  ;; Final Permutation, the inverse of IP above.
  (define (FP m)
    (define p
      '#vu8(40 8 48 16 56 24 64 32
               39 7 47 15 55 23 63 31
               38 6 46 14 54 22 62 30
               37 5 45 13 53 21 61 29
               36 4 44 12 52 20 60 28
               35 3 43 11 51 19 59 27
               34 2 42 10 50 18 58 26
               33 1 41  9 49 17 57 25))
    (do ((i 0 (fx+ i 1))
         (m0 0 (if (bitwise-bit-set? m (fx- (bytevector-u8-ref p i) 1))
                   (bitwise-ior m0 (bitwise-arithmetic-shift-left 1 i))
                   m0)))
        ((fx=? i 64) m0)))

  ;; (FP (IP 10000000000)) => 10000000000

  ;; The default expansion permutation
  (define E
    '#vu8(32 1 2 3 4 5 4 5 6 7 8 9
             8 9 10 11 12 13 12 13 14 15 16 17
             16 17 18 19 20 21 20 21 22 23 24 25
             24 25 26 27 28 29 28 29 30 31 32 1))

  ;; This is where the confusion and diffusion happens
  (define (feistel R KS E)
    (define s-boxes
      '(#vu8(14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7
                0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8
                4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0
                15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13)
        #vu8(15 1 8 14 6 11 3 4 9 7 2 13 12 0 5 10
                3 13 4 7 15 2 8 14 12 0 1 10 6 9 11 5
                0 14 7 11 10 4 13 1 5 8 12 6 9 3 2 15
                13 8 10 1 3 15 4 2 11 6 7 12 0 5 14 9)
        #vu8(10 0 9 14 6 3 15 5 1 13 12 7 11 4 2 8
                13 7 0 9 3 4 6 10 2 8 5 14 12 11 15 1
                13 6 4 9 8 15 3 0 11 1 2 12 5 10 14 7
                1 10 13 0 6 9 8 7 4 15 14 3 11 5 2 12)
        #vu8(7 13 14 3 0 6 9 10 1 2 8 5 11 12 4 15
               13 8 11 5 6 15 0 3 4 7 2 12 1 10 14 9
               10 6 9 0 12 11 7 13 15 1 3 14 5 2 8 4
               3 15 0 6 10 1 13 8 9 4 5 11 12 7 2 14)
        #vu8(2 12 4 1 7 10 11 6 8 5 3 15 13 0 14 9
               14 11 2 12 4 7 13 1 5 0 15 10 3 9 8 6
               4 2 1 11 10 13 7 8 15 9 12 5 6 3 0 14
               11 8 12 7 1 14 2 13 6 15 0 9 10 4 5 3)
        #vu8(12 1 10 15 9 2 6 8 0 13 3 4 14 7 5 11
                10 15 4 2 7 12 9 5 6 1 13 14 0 11 3 8
                9 14 15 5 2 8 12 3 7 0 4 10 1 13 11 6
                4 3 2 12 9 5 15 10 11 14 1 7 6 0 8 13)
        #vu8(4 11 2 14 15 0 8 13 3 12 9 7 5 10 6 1
               13 0 11 7 4 9 1 10 14 3 5 12 2 15 8 6
               1 4 11 13 12 3 7 14 10 15 6 8 0 5 9 2
               6 11 13 8 1 4 10 7 9 5 0 15 14 2 3 12)
        #vu8(13 2 8 4 6 15 11 1 10 9 3 14 5 0 12 7
                1 15 13 8 10 3 7 4 12 5 6 11 0 14 9 2
                7 11 4 1 9 12 14 2 0 6 10 13 15 3 5 8
                2 1 14 7 4 10 8 13 15 12 9 0 3 5 6 11)))
    (let ((C (map (lambda (s-box B)
                    (bytevector-u8-ref
                     s-box
                     (fx+ (fx* (fxior (fxand B 1)
                                      (fxarithmetic-shift-left (fxbit-field B 5 6) 1))
                               16)               ;s-box row
                          (fxbit-field B 1 5)))) ;s-box column
                  s-boxes
                  (split-number (bitwise-xor (permute R 32 E) KS) 8 6))))
      (permute (join-number C 4)
               32
               '#vu8(16 7 20 21 29 12 28 17 1 15 23 26 5 18 31 10
                        2 8 24 14 32 27 3 9 19 13 30 6 22 11 4 25))))

  ;; Runs the DES algorithm on the given plaintext bytevector. The key
  ;; list is the output from the permute-key function. Decryption is
  ;; performed if you reverse the key list. This is the ECB mode. Only
  ;; one eight-byte block is treated.
  (define des!
    (case-lambda
      ((bv keys offset E)
       (let ((m0 (IP (bytevector-u64-ref bv offset (endianness big)))))
         (do ((keys keys (cdr keys))
              (L (bitwise-bit-field m0 32 64) R)
              (R (bitwise-bit-field m0 0 32) (bitwise-xor L (feistel R (car keys) E))))
             ((null? keys)
              (bytevector-u64-set!
               bv offset (FP (bitwise-ior (bitwise-arithmetic-shift-left R 32) L))
               (endianness big))))))
      ((bv keys)
       (des! bv keys 0 E))))

;;; Triple DES, or Triple Data Encryption Algorithm.

  (define-record-type tdea-key
    (fields k1enc k1dec
            k2enc k2dec
            k3enc k3dec))

  (define (tdea-encipher! bv offset key)
    (des! bv (tdea-key-k1enc key) offset E)
    (des! bv (tdea-key-k2dec key) offset E)
    (des! bv (tdea-key-k3enc key) offset E))

  (define (tdea-decipher! bv offset key)
    (des! bv (tdea-key-k3dec key) offset E)
    (des! bv (tdea-key-k2enc key) offset E)
    (des! bv (tdea-key-k1dec key) offset E))

  (define tdea-permute-key
    (case-lambda
      ((k1 k2 k3)
       (let ((k1 (permute-key k1))
             (k2 (permute-key k2))
             (k3 (permute-key k3)))
         (make-tdea-key k1 (reverse k1)
                        k2 (reverse k2)
                        k3 (reverse k3))))
      ((key)
       (let ((k1 (make-bytevector 24))
             (k2 (make-bytevector 24))
             (k3 (make-bytevector 24)))
         (bytevector-copy! key 0 k1 0 8)
         (bytevector-copy! key 8 k2 0 8)
         (bytevector-copy! key 16 k3 0 8)
         (tdea-permute-key k1 k2 k3)))))

  ;; Encrypt the `data' bytevector with 3DES/TDEA in CBC mode. The
  ;; data must be a multiple of eight bytes (and currently aligned on
  ;; an eight byte boundary). The IV is updated.
  (define (tdea-cbc-encipher! data key iv offset count)
    (do ((i offset (fx+ i 8)))
        ((fx>=? i (fx+ offset count)))
      (bytevector-u64-native-set! data i
                                  (bitwise-xor (bytevector-u64-native-ref iv 0)
                                               (bytevector-u64-native-ref data i)))
      (tdea-encipher! data i key)
      (bytevector-copy! data i iv 0 8)))

  ;; Same as above, but decrypt instead.
  (define (tdea-cbc-decipher! data key iv offset count)
    (do ((tmp (make-bytevector 8))
         (i offset (fx+ i 8)))
        ((fx>=? i (fx+ offset count)))
      (bytevector-copy! data i tmp 0 8)
      (tdea-decipher! data i key)
      (bytevector-u64-native-set! data i
                                  (bitwise-xor (bytevector-u64-native-ref iv 0)
                                               (bytevector-u64-native-ref data i)))
      (bytevector-copy! tmp 0 iv 0 8)))

;;; UNIX crypt()

  (define (bytevector-u8-swap! bv i0 i1)
    (let ((tmp (bytevector-u8-ref bv i0)))
      (bytevector-u8-set! bv i0 (bytevector-u8-ref bv i1))
      (bytevector-u8-set! bv i1 tmp)))

  (define (bytevector->a64 bv)
    (define a64 "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    (list->string
     (map (lambda (x) (string-ref a64 x))
          (split-number (bitwise-arithmetic-shift-left
                         (bytevector-u64-ref bv 0 (endianness big))
                         2)
                        11 6))))

  (define (a64->integer str)
    (join-number
     (map (lambda (c)
            (cond ((char<=? #\a c #\z) (- (char->integer c) 7 6 (char->integer #\.)))
                  ((char<=? #\A c #\Z) (- (char->integer c) 7 (char->integer #\.)))
                  ((char<=? #\. c #\9) (- (char->integer c) (char->integer #\.)))
                  (else (raise
                          (condition
                           (make-who-condition 'crypt)
                           (make-assertion-violation)
                           (make-message-condition "Invalid character in salt")
                           (make-irritants-condition c))))))
          (reverse (string->list str)))
     6))

  (define (make-list len init)
    (vector->list (make-vector len init)))

  (define (des-crypt password salt)
    (let* ((password-shifted (map (lambda (x) (fxarithmetic-shift-left (fxand x #x7f) 1))
                                  (bytevector->u8-list (string->utf8 password))))
           (keys (permute-key
                  (u8-list->bytevector
                   (append password-shifted
                           (make-list (- 8 (length password-shifted)) 0))))))
      (let ((block (make-bytevector 8 0))
            (E* (bytevector-copy E)))
        ;; Swap round bits in E* according to the salt
        (do ((salt (a64->integer salt) (fxarithmetic-shift-right salt 1))
             (i 0 (fx+ i 1)))
            ((zero? salt))
          (when (= (fxand salt 1) 1) (bytevector-u8-swap! E* i (fx+ i 24))))
        ;; 25 rounds of DES
        (do ((i 0 (fx+ i 1)))
            ((fx=? i 25) (string-append salt (bytevector->a64 block)))
          (des! block keys 0 E*))))))
