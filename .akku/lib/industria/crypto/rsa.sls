;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2017, 2018, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; PKCS #1: RSA Encryption.

;; RFC 3447. Public-Key Cryptography Standards (PKCS) #1: RSA
;; Cryptography Specifications Version 2.1

;; But look at RFC 2313, it's easier to read...

;; TODO: key generation. must be very very very careful to get a good
;; random number generator for that

(library (industria crypto rsa)
  (export
    make-rsa-public-key rsa-public-key? rsa-public-key=?
    rsa-public-key-modulus
    rsa-public-key-public-exponent
    rsa-public-key-n
    rsa-public-key-e

    rsa-public-key-from-bytevector
    rsa-public-key-length
    rsa-public-key-byte-length

    make-rsa-private-key rsa-private-key?
    rsa-private-key-version
    rsa-private-key-modulus
    rsa-private-key-public-exponent
    rsa-private-key-private-exponent
    rsa-private-key-prime1
    rsa-private-key-prime2
    rsa-private-key-exponent1
    rsa-private-key-exponent2
    rsa-private-key-coefficient
    rsa-private-key-n
    rsa-private-key-d

    rsa-private->public
    rsa-private-key-from-bytevector
    rsa-private-key-from-pem-file

    rsa-encrypt
    rsa-decrypt
    rsa-decrypt/blinding
    rsa-pkcs1-encrypt
    rsa-pkcs1-decrypt
    rsa-pkcs1-decrypt-signature
    rsa-pkcs1-encrypt-signature
    rsa-pkcs1-decrypt-digest
    rsa-pkcs1-encrypt-digest)
  (import
    (rnrs (6))
    (industria bytevectors)
    (industria crypto entropy)
    (industria crypto math)
    (prefix (industria der) der:)
    (industria base64))

(define (RSAPublicKey)
  `(sequence (modulus integer)
             (publicExponent integer)))

(define (DigestInfo)
  `(sequence (digestAlgorithm ,(DigestAlgorithmIdentifier))
             (digest ,(Digest))))

(define (DigestAlgorithmIdentifier)
  (AlgorithmIdentifier))

(define (Digest)
  'octet-string)

(define (AlgorithmIdentifier)
  `(sequence (algorithm object-identifier)
             (parameters ANY (default #f))))

(define (RSAPrivateKey)
  `(sequence (version ,(Version))
             (modulus integer)
             (publicExponent integer)
             (privateExponent integer)
             (prime1 integer)
             (prime2 integer)
             (exponent1 integer)
             (exponent2 integer)
             (coefficient integer)
             (otherPrimeInfos ,(OtherPrimeInfos) (default '()))))

(define (Version)
  'integer)

(define (OtherPrimeInfos)
  `(sequence-of 0 +inf.0 ,(OtherPrimeInfo)))

(define (OtherPrimeInfo)
  `(sequence (prime integer)
             (exponent integer)
             (coefficient integer)))

(define-record-type rsa-public-key
  (fields modulus                     ;n
          public-exponent))           ;e

(define rsa-public-key-n rsa-public-key-modulus)
(define rsa-public-key-e rsa-public-key-public-exponent)

(define (rsa-public-key=? a b)
  (for-all (lambda (f)
             (equal? (f a) (f b)))
           (list rsa-public-key-n
                 rsa-public-key-e)))

(define (rsa-public-key-length key)
  (bitwise-length (rsa-public-key-modulus key)))

(define (byte-length n)
  (fxdiv (fxand (fx+ (bitwise-length n) 7) -8) 8))

(define (rsa-public-key-byte-length key)
  (byte-length (rsa-public-key-modulus key)))

(define (rsa-public-key-from-bytevector bv)
  (apply make-rsa-public-key (der:translate (der:decode bv)
                                            (RSAPublicKey))))

(define-record-type rsa-private-key
  (opaque #t)
  (nongenerative rsa-private-key-bb9cab0e-a7c3-439f-9ab0-7be7f4c7c198)
  (fields version
          modulus                     ;n
          public-exponent             ;e
          private-exponent            ;d
          prime1                      ;p
          prime2                      ;q
          exponent1                   ;(mod d (- p 1))
          exponent2                   ;(mod d (- q 1))
          coefficient                 ;(expt-mod q -1 p)
          other-primes)
  (protocol
   (lambda (construct)
     (define who 'make-rsa-private-key)
     (define proto
       (case-lambda
         ((n e d)
          (unless (= (expt-mod (expt-mod 2 e n) d n) 2)
            (error who "Invalid key"))
          ;; Factoring n is overkill. So why not?
          (let lp ((s 0)
                   (t (- (* e d) 1)))
            (if (even? t)
                (lp (+ s 1) (div t 2))
                (let lp ()
                  (let ((a (random-integer n)))
                    (let lp* ((b (expt-mod a t n))
                              (it 0))
                      (cond ((> it 1000)
                             ;; Just in case the key is bad and the
                             ;; first check didn't catch it. Only 2
                             ;; iterations are expected here anyway.
                             (error who "Invalid key"))
                            ((not (= 1 (expt-mod b 2 n)))
                             (lp* (expt-mod b 2 n)
                                  (+ it 1)))
                            ((or (= 1 (mod b n))
                                 (= (- n 1) (mod b n)))
                             (lp))
                            (else
                             ;; Found a non-trivial square root of 1
                             (let* ((p (gcd (- b 1) n))
                                    (q (/ n p)))
                               (proto n e d p q))))))))))
         ((n e d p)
          (proto n e d (/ n p)))
         ((n e d p q)
          (let* ((n (or n (* p q)))
                 (p (or p (/ n q)))
                 (q (or q (/ n p)))
                 (d (or d (expt-mod e -1 (* (- p 1) (- q 1))))))
            (proto n e d p q
                   (mod d (- p 1))
                   (mod d (- q 1))
                   (expt-mod q -1 p)
                   '())))
         ((n e d p q e1) (proto n e d p q))
         ((n e d p q e1 e2) (proto n e d p q))
         ((n e d p q e1 e2 coeff)
          (proto n e d p q e1 e2 coeff '()))
         ((n e d p q e1 e2 coeff other)
          ;; XXX: should probably check that p and q are in the
          ;; right order. If not then reorder and possibly recompute
          ;; the coefficient?
          (construct 0 n e d p q e1 e2 coeff other))
         ((version n e d p q e1 e2 coeff other)
          (construct version n e d p q e1 e2 coeff other))))
     proto)))

(define rsa-private-key-n rsa-private-key-modulus)
(define rsa-private-key-d rsa-private-key-private-exponent)

(define (rsa-private->public key)
  (make-rsa-public-key (rsa-private-key-modulus key)
                       (rsa-private-key-public-exponent key)))

(define (rsa-private-key-byte-length key)
  (byte-length (rsa-private-key-modulus key)))

(define (rsa-private-key-from-pem-file filename)
  (let-values (((type data) (get-delimited-base64 (open-input-file filename))))
    (unless (string=? type "RSA PRIVATE KEY")
      (assertion-violation 'rsa-private-key-from-pem-file
                           "The file is not a 'RSA PRIVATE KEY' PEM file" filename))
    (rsa-private-key-from-bytevector data)))

(define (rsa-private-key-from-bytevector bv)
  (apply make-rsa-private-key (der:translate (der:decode bv)
                                             (RSAPrivateKey))))

(define (rsa-encrypt plaintext key)
  (if (rsa-public-key? key)
      (expt-mod plaintext
                (rsa-public-key-public-exponent key)
                (rsa-public-key-modulus key))
      (expt-mod plaintext
                (rsa-private-key-public-exponent key)
                (rsa-private-key-modulus key))))

(define (rsa-decrypt ciphertext key)
  (let ((c ciphertext)
        (d (rsa-private-key-private-exponent key))
        (n (rsa-private-key-modulus key))
        (p (rsa-private-key-prime1 key))
        (q (rsa-private-key-prime2 key))
        (dp (rsa-private-key-exponent1 key))
        (dq (rsa-private-key-exponent2 key))
        (u (rsa-private-key-coefficient key)))
    (if (and d n p q dp dq u)         ;faster
        (let ((m1 (expt-mod c dp p))
              (m2 (expt-mod c dq q)))
          (let ((h (mod (* u (if (< m1 m2)
                                 (- (+ m1 p) m2)
                                 (- m1 m2)))
                        p)))
            (+ m2 (* h q))))
        (expt-mod c d n))))           ;slower

;; Private key operation with RSA blinding
(define (rsa-decrypt/blinding ciphertext key)
  (let ((e (rsa-private-key-public-exponent key))
        (n (rsa-private-key-modulus key)))
    (let* ((r (bytevector->uint (make-random-bytevector
                                 (fxdiv (fx+ (bitwise-length n) 7) 16))))
           (c* (* ciphertext (expt-mod r e n))))
      (div-mod (rsa-decrypt c* key) r n))))

;; Block types
(define EMSA #x01)                 ;EMSA-PKCS1-v1_5 (for signatures)
(define EME #x02)                  ;EME-PKCS1-v1_5 (for encryption)

(define (pkcs1-wrap block-type key-length bv)
  (bytevector->uint
   (call-with-bytevector-output-port
     (lambda (p)
       ;; Wrap the plaintext as per PKCS #1:
       ;; EB = 00 || BT || PS || 00 || M.
       (put-u8 p #x00)
       (put-u8 p block-type)
       (let ((padding (- key-length (bytevector-length bv) 3)))
         (when (< padding 8)
           (error 'pkcs1-wrap "The message is too long"))
         (if (= block-type EMSA)
             (put-bytevector p (make-bytevector padding #xFF))
             (do ((i 0 (+ i 1)))
                 ((= i padding))
               (put-u8 p (random-positive-byte)))))
       (put-u8 p #x00)
       (put-bytevector p bv)))))

;; EME-PKCS1-v1_5 decoding. Returns the unwrapped M
(define (pkcs1-unwrap block-type key-length int fault)
  (let ((bv (make-bytevector key-length)))
    ;; Bytevector contains: 00 || BT || PS || 00 || M.
    (bytevector-uint-set! bv 0 int (endianness big) key-length)
    (pkcs1-check block-type bv fault)
    (subbytevector bv (+ (bytevector-u8-index bv 0 2) 1)
                   (bytevector-length bv))))

;; Verify that bv is a properly formed PKCS #1 block. Done in
;; constant time (see the note in section 7.2.2 of RFC 3447).
(define (pkcs1-check block-type bv fault)
  (define (combine x y)
    ;; Return zero if one of the arguments is zero. Otherwise return
    ;; a positive byte.
    (let ((z (fx* x y)))
      (fxior (fxarithmetic-shift-right z 8) (fxand z #xff))))
  (define (=? x y)
    (fx- (fxarithmetic-shift-right (fxnot (fxior (fx- y x) (fx- x y)))
                                   (- (fixnum-width) 1))))
  (define (<? x y)
    (fx- (fxarithmetic-shift-right (fxxor (fx- x y)
                                          (fxand (fxxor x y)
                                                 (fxxor (fx- x y) x)))
                                   (- (fixnum-width) 1))))
  ;; Check that bv starts with 00 || BT (done by diff0). Then
  ;; check that there is a zero that separates PS and M (done by
  ;; prod). Also check that PS is at least eight bytes long.
  (do ((diff0 (fxior (bytevector-u8-ref bv 0)
                     (fxxor block-type (bytevector-u8-ref bv 1))))
       (i 2 (+ i 1))
       (prod 1 (combine prod (bytevector-u8-ref bv i)))
       (<8 0 (fxior <8 (fxand (<? i (+ 8 3)) (=? prod 0)))))
      ((= i (bytevector-length bv))
       (unless (fxzero? (fx+ diff0 (fx+ prod <8)))
         (fault)))))

;; RSAES-PKCS1-V1_5-ENCRYPT. This take a short message, wraps it in
;; random padding, and encrypts it for the recipient key.
(define (rsa-pkcs1-encrypt bytevector key)
  (let ((k (rsa-public-key-byte-length key)))
    (when (> (bytevector-length bytevector) (- k 11))
      (error 'rsa-pkcs1-encrypt "The message is too long"))
    (rsa-encrypt (pkcs1-wrap EME k bytevector) key)))

;; RSAES-PKCS1-V1_5-DECRYPT. This is the inverse of the above.
(define (rsa-pkcs1-decrypt ciphertext key)
  (define (fault) (error 'rsa-pkcs1-decrypt "Decryption error"))
  (let ((clen (byte-length ciphertext))
        (klen (byte-length (rsa-private-key-n key))))
    (when (or (not (= clen klen))
              (< klen 11)
              (> ciphertext (rsa-private-key-n key)))
      (fault))
    (pkcs1-unwrap EME klen (rsa-decrypt/blinding ciphertext key)
                  fault)))

;; TODO: RSASSA-PKCS1-V1_5-VERIFY. Verify can be done using the
;; stuff below, or alternatively it can be done by using SIGN's
;; encoder stuff (DER) and doing a comparison.

(define (rsa-pkcs1-decrypt-signature signature key)
  ;; Encrypt the signature with a public key. If it comes out
  ;; alright, the signature was signed with the corresponding
  ;; private key.
  (define (fault)
    (error 'rsa-pkcs1-decrypt-signature "bad signature"))
  (let ((bvsig (uint->bytevector (rsa-encrypt signature key))))
    (unless (= (bytevector-u8-ref bvsig 0) EMSA)
      (fault))
    (do ((i 1 (fx+ i 1)))
        ((fxzero? (bytevector-u8-ref bvsig i))
         (subbytevector bvsig (fx+ i 1)
                        (bytevector-length bvsig)))
      (unless (fx=? #xff (bytevector-u8-ref bvsig i))
        (fault)))))

(define (rsa-pkcs1-encrypt-signature digest key)
  (let ((klen (rsa-private-key-byte-length key)))
    (rsa-decrypt/blinding (pkcs1-wrap EMSA klen digest)
                          key)))

(define (rsa-pkcs1-decrypt-digest signature key)
  ;; Encrypt the signature with a public key. If it comes out
  ;; alright, the signature was signed with the corresponding
  ;; private key. For X.509-certificates this means the signature
  ;; came from the issuer, but anyone can copy a decryptable
  ;; signature, so the message digest also has to be checked.
  (let* ((bvsig (rsa-pkcs1-decrypt-signature signature key))
         (dersig (der:decode bvsig)))
    (unless (= (der:data-length dersig) (bytevector-length bvsig))
      ;; Recommendation from RFC 5246 D.4.
      (error 'rsa-pkcs1-decrypt-digest
             "Bad signature: additional data after hash value"))
    (der:translate dersig (DigestInfo))))

(define id-md5        '(1 2 840 113549 2 5))
(define id-sha-1      '(1 3 14 3 2 26))
;; https://csrc.nist.gov/projects/computer-security-objects-register/algorithm-registration#Hash
(define id-sha256     '(2 16 840 1 101 3 4 2 1))
(define id-sha384     '(2 16 840 1 101 3 4 2 2))
(define id-sha512     '(2 16 840 1 101 3 4 2 3))
(define id-sha224     '(2 16 840 1 101 3 4 2 4))
(define id-sha512-224 '(2 16 840 1 101 3 4 2 5))
(define id-sha512-256 '(2 16 840 1 101 3 4 2 6))
(define id-sha3-224   '(2 16 840 1 101 3 4 2 7))
(define id-sha3-256   '(2 16 840 1 101 3 4 2 8))
(define id-sha3-384   '(2 16 840 1 101 3 4 2 9))
(define id-sha3-512   '(2 16 840 1 101 3 4 2 10))
(define id-shake128   '(2 16 840 1 101 3 4 2 11))
(define id-shake256   '(2 16 840 1 101 3 4 2 12))

(define (rsa-pkcs1-encrypt-digest algorithm digest key)
  ;;  This works for algorithms without arguments.
  (let ((object-id
         (case algorithm
           ((md5) id-md5)
           ((sha-1) id-sha-1)
           ((sha-224) id-sha224)
           ((sha-256) id-sha256)
           ((sha-384) id-sha384)
           ((sha-512) id-sha512)
           ((sha-512-224) id-sha512-224)
           ((sha-512-256) id-sha512-256)
           ((sha3-224) id-sha3-224)
           ((sha3-256) id-sha3-256)
           ((sha3-384) id-sha3-384)
           ((sha3-512) id-sha3-512)
           ((shake-128) id-shake128)
           ((shake-256) id-shake256)
           (else (if (and (list? algorithm) (for-all integer? algorithm))
                     algorithm
                     (error 'rsa-pkcs1-encrypt-digest
                            "Unknown algorithm" algorithm))))))
    (let ((der-coded
           (der:encode #f
                       `(sequence 0 0
                                  ((sequence 0 0
                                             ((object-identifier 0 0
                                                                 ,object-id)
                                              (null 0 0 #f)))
                                   (octet-string 0 0 ,digest))))))
      (let ((k (rsa-private-key-byte-length key)))
        (when (> (bytevector-length der-coded) (- k 11))
          (error 'rsa-pkcs1-encrypt-digest "The message is too long"))
        (rsa-decrypt/blinding (pkcs1-wrap EMSA k der-coded) key))))))
