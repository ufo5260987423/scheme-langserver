;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011, 2012, 2018, 2019, 2022 Göran Weinholt <goran@weinholt.se>

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

;;; Elliptic Curve Digital Signature Algorithm

(library (industria crypto ecdsa)
  (export make-ecdsa-public-key ecdsa-public-key? ecdsa-public-key=?
          ecdsa-public-key-curve
          ecdsa-public-key-Q
          ecdsa-public-key-length

          make-ecdsa-private-key ecdsa-private-key?
          ecdsa-private-key-curve
          ecdsa-private-key-d
          ecdsa-private-key-Q

          ecdsa-private->public

          ecdsa-private-key-from-bytevector

          ecdsa-verify-signature
          ecdsa-create-signature

          ecdsa-signature-from-bytevector
          ecdsa-signature-to-bytevector

          make-ecdsa-sha-2-public-key ecdsa-sha-2-public-key?
          make-ecdsa-sha-2-private-key ecdsa-sha-2-private-key?
          ecdsa-sha-2-verify-signature
          ecdsa-sha-2-create-signature
          ecdsa-sha-2-private-key-from-bytevector)
  (import (rnrs)
          (industria bytevectors)
          (industria crypto ec)
          (industria crypto entropy)
          (industria crypto math)
          (hashing sha-2)
          (prefix (industria der) der:)
          (industria base64))

  (define-record-type ecdsa-public-key
    (fields curve Q)
    (protocol
     (lambda (p)
       (lambda (curve Q)
         (p curve (->elliptic-point Q curve))))))

  (define (ecdsa-public-key=? a b)
    (and (equal? (ecdsa-public-key-curve a) (ecdsa-public-key-curve b))
         (equal? (ecdsa-public-key-Q a) (ecdsa-public-key-Q b))))

  (define-record-type ecdsa-private-key
    (opaque #t)
    (nongenerative ecdsa-private-key-2fb1085c-38ad-48ba-98de-463ab54036d3)
    (fields curve
            d                           ;private
            Q)                          ;public
    (protocol
     (lambda (p)
       (case-lambda
         ((curve)
          (let-values (((d Q) (make-random-key curve)))
            (assert (integer? d))
            (p curve d Q)))
         ((curve d)
          (assert (integer? d))
          (p curve d (ec* d (elliptic-curve-G curve) curve)))
         ((curve d Q)
          (assert (integer? d))
          (p curve d Q))))))

  (define (ecdsa-private->public key)
    (let ((curve (ecdsa-private-key-curve key))
          (Q (ecdsa-private-key-Q key)))
      (if (ecdsa-sha-2-private-key? key)
          (make-ecdsa-sha-2-public-key curve Q)
          (make-ecdsa-public-key curve Q))))

  (define (ecdsa-public-key-length key)
    (bitwise-length (elliptic-curve-n (ecdsa-public-key-curve key))))

  (define (ECPrivateKey)
    `(sequence (version (integer ((ecPrivKeyver1 . 1))))
               (privateKey octet-string)
               (parameters (explicit context 0 object-identifier) (default #f))
               (publicKey (explicit context 1 bit-string) (default #f))))

  (define (find-curve oid)
    ;; The list of OIDs is from RFC 5656
    (cond ((assoc oid `(((1 2 840 10045 3 1 7) . ,secp256r1)
                        ((1 3 132 0 34) . ,secp384r1)
                        ((1 3 132 0 35) . ,secp521r1)
                        ;; ((1 3 132 0 1) . ,sect163k1)
                        ;; ((1 2 840 10045 3 1 1) . ,secp192r1)
                        ;; ((1 3 132 0 33) . ,secp224r1)
                        ;; ((1 3 132 0 26) . ,sect233k1)
                        ;; ((1 3 132 0 27) . ,sect233r1)
                        ;; ((1 3 132 0 16) . ,sect283k1)
                        ;; ((1 3 132 0 36) . ,sect409k1)
                        ;; ((1 3 132 0 37) . ,sect409r1)
                        ;; ((1 3 132 0 38) . ,sect571k1)
                        ))
           => cdr)
          (else
           (error 'ecdsa-private-key-from-bytevector
                  "Unimplemented elliptic curve" oid))))

  (define (ecdsa-private-key-from-bytevector bv)
    ;; This is from RFC 5915, which is probably more general than
    ;; ECDSA. Should probably be called
    ;; ec-private-key-from-bytevector, and there'd be ec-private-key
    ;; and ec-public-key.
    (let-values (((ver private curve public . _)
                  (apply values (der:translate (der:decode bv) (ECPrivateKey)))))
      (unless (eq? ver 'ecPrivKeyver1)
        (error 'ecdsa-private-key-from-bytevector "Unknown version" ver))
      (make-ecdsa-private-key (find-curve curve)
                              (bytevector->uint private)
                              (der:bit-string->integer public))))

  (define (make-random n)
    ;; Generate a random number less than q
    (let ((c (bytevector->uint (make-random-bytevector
                                (fxdiv (fx+ (bitwise-length n) 7) 8)))))
      (if (< 0 c n)
          c
          (make-random n))))

  (define (make-random-key curve)
    (let* ((k (make-random (elliptic-curve-n curve)))
           (R (ec* k (elliptic-curve-G curve) curve)))
      (if (pair? R)
          (values k R)
          (make-random-key curve))))

  (define (hash->integer hash n)
    (let* ((H (bytevector->uint hash))
           (hlen (* 8 (bytevector-length hash)))
           (nlen (bitwise-length n)))
      (if (>= nlen hlen)
          H
          ;; This is weird, but the leftmost nlen bits are apparently
          ;; what should be returned in this case, so....
          (bitwise-arithmetic-shift-right H (- hlen nlen)))))

  ;; Returns #t if the signature is valid.
  (define (ecdsa-verify-signature hash key r s)
    (let* ((curve (ecdsa-public-key-curve key))
           (n (elliptic-curve-n curve))
           (G (elliptic-curve-G curve))
           (Q (ecdsa-public-key-Q key)))
      (and (< 0 r n)
           (< 0 s n)
           (let* ((w (expt-mod s -1 n))
                  (e (hash->integer hash n))
                  (u1 (mod (* e w) n))
                  (u2 (mod (* r w) n))
                  (X (ec+ (ec* u1 G curve)
                          (ec* u2 Q curve)
                          curve)))
             (and (pair? X)
                  (let ((v (mod (car X) n)))
                    (= v r)))))))

  (define (ecdsa-create-signature hash privkey)
    (let* ((curve (ecdsa-private-key-curve privkey))
           (n (elliptic-curve-n curve)))
      (let lp ()
        (let-values (((k R) (make-random-key curve)))
          (let ((r (mod (car R) n)))
            (if (zero? r)
                (lp)                  ;retry
                (let* ((e (hash->integer hash n))
                       (s (div-mod (+ e (* r (ecdsa-private-key-d privkey))) k n)))
                  (if (zero? s)
                      (lp)            ;retry
                      (values r s)))))))))

  ;; RFC 4492
  (define (Ecdsa-Sig-Value)
    `(sequence (r integer)
               (s integer)))

  (define (ecdsa-signature-from-bytevector signature)
    (der:translate (der:decode signature) (Ecdsa-Sig-Value)))

  (define (ecdsa-signature-to-bytevector r s)
    (der:encode #f `(sequence 0 0 ((integer 0 0 ,r)
                                   (integer 0 0 ,s)))))

  ;; These things are for working with ecdsa-sha2-* from RFC 5656. The
  ;; special record type lets a program indicate that SHA-2 should be
  ;; used, but it's up to the program to check that it is. Presumably
  ;; other hash algorithms could be defined in the future.

  (define-record-type ecdsa-sha-2-public-key
    (parent ecdsa-public-key)
    (protocol
     (lambda (p)
       (lambda x
         ((apply p x))))))

  (define-record-type ecdsa-sha-2-private-key
    (parent ecdsa-private-key)
    (protocol
     (lambda (p)
       (lambda x
         ((apply p x))))))

  (define (sha-2* message key)
    ;; RFC 5656, 6.2.1
    (let ((b (ecdsa-public-key-length key)))
      (cond ((<= b 256) (sha-256->bytevector (sha-256 message)))
            ((and (< 256 b) (<= b 384))
             (sha-384->bytevector (sha-384 message)))
            ((< 384 b)
             (sha-512->bytevector (sha-512 message))))))

  (define (ecdsa-sha-2-verify-signature message key r s)
    (ecdsa-verify-signature (sha-2* message key) key r s))

  (define (ecdsa-sha-2-create-signature message privkey)
    (ecdsa-create-signature (sha-2* message (ecdsa-private->public privkey)) privkey))

  (define (ecdsa-sha-2-private-key-from-bytevector bv)
    (let ((key (ecdsa-private-key-from-bytevector bv)))
      (make-ecdsa-sha-2-private-key (ecdsa-private-key-curve key)
                                    (ecdsa-private-key-d key)
                                    (ecdsa-private-key-Q key)))))
