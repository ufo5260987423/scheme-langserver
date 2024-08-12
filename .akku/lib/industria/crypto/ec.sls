;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Elliptic curve cryptography: algorithms and curve definitions.

;; Points are either +inf.0 or a pair with x and y coordinates.
;; a and b are from the equation:
;; y² = x³ + ax + b

;; Prime  :  (p,a,b,G,n,h)
;; Binary: (m,f,a,b,G,n,h)

;; +inf.0 refers to the "point at infinity". In literature it is
;; often written as infinity (#\x221E) or a stylized O (#\x1D4DE).

;; No, 521 is not a typo.

(library (industria crypto ec)
  (export make-elliptic-prime-curve elliptic-prime-curve?
          elliptic-curve-a
          elliptic-curve-b
          elliptic-curve-G
          elliptic-curve-n
          elliptic-curve-h
          elliptic-prime-curve-p
          elliptic-curve=?
          ;; Standard curves
          secp256r1 nistp256
          secp384r1 nistp384
          secp521r1 nistp521
          ;; Mathematical operations for points on curves
          ec+ ec- ec*
          ;; Conversion to elliptic points
          bytevector->elliptic-point
          integer->elliptic-point
          ->elliptic-point
          ;; Conversion to interchange format
          elliptic-point->bytevector)
  (import (rnrs)
          (industria bytevectors)
          (industria crypto math))

  (define-record-type elliptic-curve
    (fields a b
            G                           ;base point
            n                           ;order of G
            h))                         ;cofactor

  (define-record-type elliptic-prime-curve
    (parent elliptic-curve)
    (fields p)                          ;prime field
    (protocol
     (lambda (parent)
       (lambda (p a b G n h)
         (let ((G-point (if (pair? G)
                            G
                            (->elliptic-point G ((parent a b #f #f #f) p)))))
           ((parent a b G-point n h) p))))))

  (define elliptic-prime-curve-G elliptic-curve-G)
  (define elliptic-prime-curve-n elliptic-curve-n)

  ;; TODO, perhaps some day
  ;; (define-record-type elliptic-binary-curve
  ;;   (parent elliptic-curve)
  ;;   (fields m f))

  (define (elliptic-curve=? x y)
    (or (eq? x y)
        (and (= (elliptic-curve-a x) (elliptic-curve-a y))
             (= (elliptic-curve-b x) (elliptic-curve-b y))
             (equal? (elliptic-curve-G x) (elliptic-curve-G y))
             (= (elliptic-curve-n x) (elliptic-curve-n y))
             (= (elliptic-curve-h x) (elliptic-curve-h y))
             (= (elliptic-prime-curve-p x) (elliptic-prime-curve-p y)))))

;;; Mathematical operations

  ;; Not the most efficient code, but short and sweet. The NIST curves
  ;; were chosen especially for efficient computation, but none of
  ;; that is done here.

  (define (ec+ P Q curve)
    (define who 'elliptic+)
    (cond ((and (pair? P) (pair? Q))
           (let ((x1 (car P)) (y1 (cdr P))
                 (x2 (car Q)) (y2 (cdr Q))
                 (a (elliptic-curve-a curve))
                 (b (elliptic-curve-b curve))
                 (p (elliptic-prime-curve-p curve)))
             (if (and (= x1 x2)
                      (or (not (= y1 y2))
                          (= y1 y2 0)))
                 +inf.0
                 (let ((return
                        (lambda (num den)
                          (if (zero? den)
                              +inf.0
                              (let* ((m (div-mod num den p))
                                     (x3 (mod (- (* m m) x1 x2) p))
                                     (y3 (mod (- (* m (- x1 x3)) y1) p)))
                                (cons x3 y3))))))
                   (if (equal? P Q)
                       (return (+ (* 3 (* x1 x1)) a) (* 2 y1))
                       (return (- y2 y1) (- x2 x1)))))))
          ((and (number? P) (infinite? P)) Q)
          ((and (number? Q) (infinite? Q)) P)
          (else (assertion-violation who "Bad point"))))

  (define ec-
    (case-lambda
      ((P curve) (cons (car P) (- (cdr P))))
      ((P Q curve) (ec+ P (ec- Q curve) curve))))

  (define (ec* multiplier point curve)
    (assert (and (integer? multiplier) (exact? multiplier)))
    (let lp ((m multiplier)
             (point point)
             (ret +inf.0))
      (if (zero? m)
          ret
          (let lp* ((m m)
                    (point point))
            (if (even? m)
                (lp* (bitwise-arithmetic-shift-right m 1)
                     (ec+ point point curve))
                (lp (- m 1) point (ec+ ret point curve)))))))

;;; Interchange format

  (define (bytevector->elliptic-point bv curve)
    (define who 'bytevector->elliptic-point)
    (let* ((a (elliptic-curve-a curve))
           (b (elliptic-curve-b curve))
           (p (elliptic-prime-curve-p curve))
           (p-len (div (+ (bitwise-length p) 7) 8)))
      (cond ((equal? bv #vu8(0))
             +inf.0)
            ((= (bytevector-length bv) (+ p-len 1))
             (let ((xp (bytevector-uint-ref bv 1 (endianness big) p-len))
                   (yp* (case (bytevector-u8-ref bv 0)
                          ((#x02) 0)
                          ((#x03) 1)
                          (else
                           (error who "Invalid elliptic curve point" bv curve)))))
               (if (= (bitwise-bit-count p) 1)
                   (error who "Binary elliptic curves are not yet supported" bv curve)
                   (let* ((alpha (mod (+ (expt-mod xp 3 p) (* a xp) b) p))
                          (beta (sqrt-mod alpha p))
                          (yp (if (boolean=? (even? yp*) (even? beta))
                                  beta
                                  (- p beta))))
                     (cons xp yp)))))
            ((= (bytevector-length bv) (+ (* p-len 2) 1))
             (let ((W (bytevector-u8-ref bv 0))
                   (xp (bytevector-uint-ref bv 1 (endianness big) p-len))
                   (yp (bytevector-uint-ref bv (+ p-len 1) (endianness big) p-len)))
               (unless (and (= W #x04)
                            (= (mod (* yp yp) p)
                               (mod (+ (expt-mod xp 3 p) (* a xp) b) p)))
                 (error who "Invalid elliptic curve point" bv curve))
               (cons xp yp)))
            (else
             (error who "Invalid elliptic curve point (bad length)" bv curve)))))

  (define (integer->elliptic-point int curve)
    (bytevector->elliptic-point (uint->bytevector int) curve))

  (define (->elliptic-point x curve)
    (cond ((or (pair? x) (eqv? x +inf.0))
           x)
          ((bytevector? x)
           (bytevector->elliptic-point x curve))
          ((integer? x)
           (integer->elliptic-point x curve))
          (else
           (error '->elliptic-point
                  "Can not convert this to an elliptic curve point"
                  x curve))))

  (define (elliptic-point->bytevector point curve)
    (define who 'elliptic-point->bytevector)
    (call-with-bytevector-output-port
      (lambda (out)
        (cond ((eqv? point +inf.0)
               (put-u8 out 0))
              ((pair? point)
               (put-u8 out #x04)
               (let* ((p (elliptic-prime-curve-p curve))
                      (p-len (div (+ (bitwise-length p) 7) 8))
                      (bv (make-bytevector (* 2 p-len))))
                 (bytevector-uint-set! bv 0 (car point) (endianness big) p-len)
                 (bytevector-uint-set! bv p-len (cdr point) (endianness big) p-len)
                 (put-bytevector out bv)))
              (else
               (error who "Invalid elliptic point" point))))))

;;; Some standardized curves

  (define secp256r1
    (make-elliptic-prime-curve
     (+ (* (expt 2 224) (- (expt 2 32) 1))
        (expt 2 192) (expt 2 96) -1)
     -3
     (string->number
      "#x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63B\
         CE3C3E27D2604B")
     (string->number
      "#x046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0\
         F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162B\
         CE33576B315ECECBB6406837BF51F5")
     (string->number
      "#xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3\
         B9CAC2FC632551")
     #x01))

  (define secp384r1
    (make-elliptic-prime-curve
     (- (expt 2 384) (expt 2 128) (expt 2 96) (- (expt 2 32)) 1)
     -3
     (string->number
      "#xB3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F\
         5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF")
     (string->number
      "#x04AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741\
         E082542A385502F25DBF55296C3A545E3872760AB73617DE4A96262C\
         6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1\
         CE1D7E819D7A431D7C90EA0E5F")
     (string->number
      "#xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81\
         F4372DDF581A0DB248B0A77AECEC196ACCC52973")
     #x01))

  (define secp521r1
    (make-elliptic-prime-curve
     (- (expt 2 521) 1)
     -3
     (string->number
      "#x0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B4\
         89918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C\
         34F1EF451FD46B503F00")
     (string->number
      "#x0400C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F8\
         28AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C185\
         6A429BF97E7E31C2E5BD66011839296A789A3BC0045C8A5FB42C7D1B\
         D998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9\
         013FAD0761353C7086A272C24088BE94769FD16650")
     (string->number
      "#x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF\
         FFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C\
         47AEBB6FB71E91386409")
     #x01))

  ;; NIST also defines these
  (define nistp256 secp256r1)           ;Curve P-256

  (define nistp384 secp384r1)           ;Curve P-384

  (define nistp521 secp521r1)           ;Curve P-521

  )
