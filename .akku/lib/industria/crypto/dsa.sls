;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2019 Göran Weinholt <goran@weinholt.se>

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

;; The Digital Signature Algorithm from FIPS Pub 186.

(library (industria crypto dsa)
  (export make-dsa-public-key dsa-public-key? dsa-public-key=?
          dsa-public-key-p
          dsa-public-key-q
          dsa-public-key-g
          dsa-public-key-y
          dsa-public-key-length

          make-dsa-private-key dsa-private-key?
          dsa-private-key-p
          dsa-private-key-q
          dsa-private-key-g
          dsa-private-key-y
          dsa-private-key-x

          dsa-private->public
          dsa-private-key-from-bytevector
          dsa-private-key-from-pem-file

          dsa-signature-from-bytevector
          dsa-verify-signature
          dsa-create-signature)
  (import (prefix (industria der) der:)
          (industria bytevectors)
          (industria crypto entropy)
          (industria crypto math)
          (industria base64)
          (rnrs))

  ;; y is (expt-mod g x p). x is secret.

  (define-record-type dsa-public-key
    (nongenerative dsa-public-key-c0f523ff-cc62-4cb7-83fc-34fe3dcafb6c)
    (fields p q g y))

  (define-record-type dsa-private-key
    (opaque #t)
    (nongenerative dsa-private-key-ec67dd75-be57-42ac-b8ed-95bc893a80db)
    (fields p q g y x))

  (define (dsa-public-key=? a b)
    (for-all (lambda (f)
               (equal? (f a) (f b)))
             (list dsa-public-key-p
                   dsa-public-key-q
                   dsa-public-key-g
                   dsa-public-key-y)))

  (define (dsa-public-key-length key)
    (bitwise-length (dsa-public-key-p key)))

  (define (Dss-Parms)
    '(sequence (p integer)
               (q integer)
               (g integer)))

  (define (Dss-Sig-Value)
    '(sequence (r integer)
               (s integer)))

  (define (DSAPrivateKey)
    ;; Not sure where this is specified. Copied from gnutls.asn.
    '(sequence (version integer)        ;should be zero
               (p integer)
               (q integer)
               (g integer)
               (y integer)
               (x integer)))

  (define (dsa-private->public priv)
    (make-dsa-public-key (dsa-private-key-p priv)
                         (dsa-private-key-q priv)
                         (dsa-private-key-g priv)
                         (dsa-private-key-y priv)))

  (define (dsa-private-key-from-bytevector bv)
    (let ((data (der:translate (der:decode bv) (DSAPrivateKey))))
      (unless (zero? (car data))
        (error 'dsa-private-key-from-bytevector
               "Bad version on private DSA key" (car data)))
      (apply make-dsa-private-key (cdr data))))

  (define (dsa-private-key-from-pem-file filename)
    (let-values (((type data) (get-delimited-base64 (open-input-file filename))))
      (unless (string=? type "DSA PRIVATE KEY")
        (assertion-violation 'dsa-private-key-from-pem-file
                             "The file is not a 'DSA PRIVATE KEY' PEM file" filename))
      (dsa-private-key-from-bytevector data)))

  ;; The int is normally from an X.509 certificate and this procedure
  ;; returns r and s in a list.
  (define (dsa-signature-from-bytevector bv)
    (der:translate (der:decode bv) (Dss-Sig-Value)))

  (define (dsa-verify-signature Hm pubkey r s)
    (and (< 0 r (dsa-public-key-q pubkey))
         (< 0 s (dsa-public-key-q pubkey))
         (let* ((w (expt-mod s -1 (dsa-public-key-q pubkey)))
                ;; FIXME: leftmost min(n,outlen) bits:
                (z (bytevector->uint Hm))
                (u1 (mod (* z w) (dsa-public-key-q pubkey)))
                (u2 (mod (* r w) (dsa-public-key-q pubkey)))
                (v1 (expt-mod (dsa-public-key-g pubkey) u1
                              (dsa-public-key-p pubkey)))
                (v2 (expt-mod (dsa-public-key-y pubkey) u2
                              (dsa-public-key-p pubkey)))
                (v (mod (mod (* v1 v2)
                             (dsa-public-key-p pubkey))
                        (dsa-public-key-q pubkey))))
           (= v r))))

  (define (make-random q)
    ;; Generate a random number less than q
    (let ((c (bytevector->uint
               (make-random-bytevector
                (+ 64 (div (+ (bitwise-length q) 7) 8))))))
      (+ (mod c (- q 1)) 1)))

  (define (dsa-create-signature Hm privkey)
    (let ((p (dsa-private-key-p privkey))
          (q (dsa-private-key-q privkey)))
      (let* ((k (make-random q))
             (r (mod (expt-mod (dsa-private-key-g privkey) k p) q))
             (s (mod (* (expt-mod k -1 q)
                        (+ (bytevector->uint Hm)
                           (* (dsa-private-key-x privkey) r)))
                     q)))
        (values r s))))

  )
