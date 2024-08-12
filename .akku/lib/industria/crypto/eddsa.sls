;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Edwards-curve Digital Signature Algorithm (EdDSA)

;; RFC8032 Edwards-Curve Digital Signature Algorithm (EdDSA)
;; RFC8410 Ed25519, Ed448, X25519, and X448 for X.509

;; TODO: Ed448

(library (industria crypto eddsa)
  (export
    eddsa-public-key?
    eddsa-public-key-length
    ed25519-public-key-value

    make-ed25519-public-key ed25519-public-key? ed25519-public-key=?
    make-eddsa-private-key eddsa-private-key?
    make-ed25519-private-key ed25519-private-key?
    ed25519-private-key-secret
    ed25519-private->public

    eddsa-private-key-from-bytevector

    ed25519-sign
    ed25519-verify
    )
  (import
    (rnrs (6))
    (hashing sha-2)
    (industria bytevectors)
    (industria crypto entropy)
    (industria crypto math)
    (prefix (industria der) der:))

(define-record-type eddsa-public-key
  (fields))

(define-record-type ed25519-public-key
  (parent eddsa-public-key)
  (fields value)
  (protocol
   (lambda (p)
     (lambda (value)
       (assert (fx=? (bytevector-length value) 32))
       ((p) value)))))

(define (ed25519-public-key=? a b)
  (equal? (ed25519-public-key-value a) (ed25519-public-key-value b)))

(define-record-type eddsa-private-key
  (opaque #t))

(define-record-type ed25519-private-key
  (opaque #t)
  (fields secret)
  (protocol
   (lambda (p)
     (lambda (secret)
       (unless (and (bytevector? secret)
                    (fx=? (bytevector-length secret) 32))
         (assertion-violation 'ed25519-private-key
                              "The secret should be a bytevector of length 32"))
       (p secret)))))

(define (eddsa-public-key-length key)
  (cond ((ed25519-public-key? key)
         (* 8 32))
        (else
         (assertion-violation 'eddsa-public-key-length
                              "Not an EdDSA key" key))))

(define (OneAsymmetricKey)
  ;; RFC 8410 and RFC 5958. Missing some fields.
  (define (PrivateKeyAlgorithmIdentifier)
    `(sequence (algorithm object-identifier)))
  (define (PrivateKey)
    'octet-string)
  `(sequence (version (integer ((v1 . 0) (v2 . 1))))
             (privateKeyAlgorithm ,(PrivateKeyAlgorithmIdentifier))
             (privateKey ,(PrivateKey))))

(define (eddsa-private-key-from-bytevector bv)
  (let-values ([(_ver alg private . _)
                (apply values (der:translate (der:decode bv) (OneAsymmetricKey)))])
    (cond ((equal? (car alg) '(1 3 101 112))
           (let ((secret (der:translate (der:decode private) 'octet-string)))
             (make-ed25519-private-key secret)))
          (else
           (error 'eddsa-private-key-from-bytevector
                  "Unimplemented private key algorithm" alg)))))

(define-record-type ed-curve
  (fields p                             ;base field ℤₚ
          d                             ;curve constant
          q                             ;group order
          G                             ;base point
          ))

;; Given y and sign on an Edwards curve, recover the x value.
(define (recover-x y sign p d)
  (define modp_sqrt_-1 (expt-mod 2 (div (- p 1) 4) p))
  (if (>= y p)
      0
      (let ([x² (* (- (* y y) 1)
                   (invmod (+ (* d y y) 1) p))])
        (cond
          ((zero? x²)
           (if (eqv? sign 1) #f 0))
          (else
           (let* ([x (expt-mod x² (div (+ p 3) 8) p)]
                  [x (if (zero? (mod (- (* x x) x²) p))
                         x
                         (mod (* x modp_sqrt_-1) p))])
             (and (zero? (mod (- (* x x) x²) p))
                  (if (= (bitwise-and x 1) sign)
                      x
                      (- p x)))))))))

#;
(define Ed25519
  (let* ([p (- (expt 2 255) 19)]
         [d (mod (* -121665 (invmod 121666 p)) p)]
         [q (+ (expt 2 252)
               27742317777372353535851937790883648493)]
         [g_y (* 4 (invmod 5 p))]
         [g_x (recover-x g_y 0 p d)]
         [G (vector g_x g_y 1 (mod (* g_x g_y) p))])
    (make-ed-curve p d q G)))

(define Ed25519
  (make-ed-curve 57896044618658097711785492504343953926634992332820282019728792003956564819949
                 37095705934669439343138083508754565189542113879843219016388785533085940283555
                 7237005577332262213973186563042994240857116359379907606001950938285454250989
                 '#(15112221349535400772501151409588531511454012693041857206046113283949847762202
                    46316835694926478169428394003475163141307993866256225615783033603165251855960
                    1
                    46827403850823179245072216630277197565144205554125654976674165829533817101731)))

(define (ed25519-recover-x y sign)
  (let ([p (ed-curve-p Ed25519)]
        [d (ed-curve-d Ed25519)])
    (recover-x y sign p d)))

(define (point-values P)
  ;; A point P = (x,y) is represented as (X, Y, Z, T) where x = X/Z, y
  ;; = Y/Z, x * y = T/Z
  (values (vector-ref P 0)
          (vector-ref P 1)
          (vector-ref P 2)
          (vector-ref P 3)))

;; Add two points.
(define (ed25519+ P1 P2)
  (let ([p (ed-curve-p Ed25519)]
        [d (ed-curve-d Ed25519)])
    (if (equal? P1 P2)
        (let-values ([(X1 Y1 Z1 _T1) (point-values P1)])
          (let ([A (expt-mod X1 2 p)]
                [B (expt-mod Y1 2 p)]
                [C (mod (* 2 (expt-mod Z1 2 p)) p)])
            (let* ([H (+ A B)]
                   [E (- H (expt-mod (+ X1 Y1) 2 p))]
                   [G (- A B)]
                   [F (+ C G)])
              (let ([X3 (* E F)]
                    [Y3 (* G H)]
                    [Z3 (* F G)]
                    [T3 (* E H)])
                (vector X3 Y3 Z3 T3)))))
        (let-values ([(X1 Y1 Z1 T1) (point-values P1)]
                     [(X2 Y2 Z2 T2) (point-values P2)])
          (let ([A (mod (* (- Y1 X1) (- Y2 X2)) p)]
                [B (mod (* (+ Y1 X1) (+ Y2 X2)) p)]
                [C (mod (* T1 2 d T2) p)]
                [D (mod (* Z1 2 Z2) p)])
            (let ([E (- B A)]
                  [F (- D C)]
                  [G (+ D C)]
                  [H (+ B A)])
              (let ([X3 (* E F)]
                    [Y3 (* G H)]
                    [Z3 (* F G)]
                    [T3 (* E H)])
                (vector X3 Y3 Z3 T3))))))))

;; Multiply a point by a scalar.
(define (ed25519* s P)
    (let lp ((s s)
             (P P)
             (Q '#(0 1 1 0)))
      (if (eqv? s 0)
          Q
          (let ([Q (if (bitwise-bit-set? s 0)
                       (ed25519+ Q P)
                       Q)])
            (lp (bitwise-arithmetic-shift-right s 1)
                (ed25519+ P P)
                Q)))))

(define (ed25519-point=? P1 P2)
  (let ([p (ed-curve-p Ed25519)])
    (let-values ([(X1 Y1 Z1 _T1) (point-values P1)]
                 [(X2 Y2 Z2 _T2) (point-values P2)])
      (cond ((not (zero? (mod (- (* X1 Z2) (* X2 Z1)) p))) #f)
            ((not (zero? (mod (- (* Y1 Z2) (* Y2 Z1)) p))) #f)
            (else #t)))))

(define (ed25519-point-compress P)
  (let ([p (ed-curve-p Ed25519)])
    (let-values ([(X Y Z _T) (point-values P)])
      (let ([zinv (invmod Z p)])
        (let ([x (mod (* X zinv) p)]
              [y (mod (* Y zinv) p)])
          (uint->bytevector (bitwise-ior y (bitwise-arithmetic-shift-left
                                            (bitwise-and x 1) 255))
                            (endianness little) 32))))))

(define (ed25519-point-decompress bv)
  (let ([p (ed-curve-p Ed25519)])
    (let ([y (bytevector->uint bv (endianness little))])
      (let ([sign (bitwise-arithmetic-shift-right y 255)]
            [y (bitwise-bit-field y 0 255)])
        (let ([x (ed25519-recover-x y sign)])
          (and x (vector x y 1 (mod (* x y) p))))))))

(define (ed25519-secret-expand secret)
  (assert (fx=? (bytevector-length secret) 32))
  (let* ([h (sha-512->bytevector (sha-512 secret))]
         [a (bytevector->uint (subbytevector h 0 32) (endianness little))]
         [a (bitwise-ior (bitwise-arithmetic-shift-left 1 254)
                         (bitwise-and a (- (bitwise-arithmetic-shift-left 1 254) 8)))])
    (values a (subbytevector h 32 64))))

(define (ed25519-private->public key)
  (let-values ([(a _) (ed25519-secret-expand (ed25519-private-key-secret key))])
    (make-ed25519-public-key (ed25519-point-compress
                              (ed25519* a (ed-curve-G Ed25519))))))

(define (ed25519-sha-512 . bv*)
  (mod (bytevector->uint (sha-512->bytevector (apply sha-512 bv*))
                         (endianness little))
       (ed-curve-q Ed25519)))

(define (ed25519-sign private-key msg)
  (let ([q (ed-curve-q Ed25519)]
        [G (ed-curve-G Ed25519)])
    (let-values ([(a prefix) (ed25519-secret-expand
                              (ed25519-private-key-secret private-key))])
      (let* ([A (ed25519-point-compress (ed25519* a G))]
             [r (ed25519-sha-512 prefix msg)]
             [R (ed25519* r G)]
             [Rs (ed25519-point-compress R)]
             [h (ed25519-sha-512 Rs A msg)]
             [s (mod (+ r (* h a)) q)])
        (bytevector-append Rs (uint->bytevector s (endianness little) 32))))))

(define (ed25519-verify public-key msg signature)
  (assert (fx=? (bytevector-length signature) 64))
  (let* ([public (ed25519-public-key-value public-key)]
         [A (ed25519-point-decompress public)])
    (and A
         (let* ([Rs (subbytevector signature 0 32)]
                [R (ed25519-point-decompress Rs)])
           (and R
                (let ([s (bytevector->uint (subbytevector signature 32 64)
                                           (endianness little))])
                  (and (< s (ed-curve-q Ed25519))
                       (let* ([h (ed25519-sha-512 Rs public msg)]
                              [s*G (ed25519* s (ed-curve-G Ed25519))]
                              [h*A (ed25519* h A)])
                         (ed25519-point=? s*G (ed25519+ R h*A)))))))))))
