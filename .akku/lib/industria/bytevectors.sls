;; -*- mode: scheme; coding: utf-8 -*-
;; Bytevector utilities
;; Copyright © 2009, 2010, 2012, 2018, 2019 Göran Weinholt <goran@weinholt.se>

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

(library (industria bytevectors)
  (export
    bytevector-for-each
    bytevector-append
    bytevectors-length
    bytevector-concatenate
    subbytevector
    bytevector-u8-index
    bytevector-u8-index-right
    bytevector->uint bytevector->sint
    uint->bytevector sint->bytevector
    bytevector=?/constant-time)
  (import
    (rnrs (6)))

(define bytevector-for-each
  (case-lambda
    ((proc bv)
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (bytevector-length bv)))
       (proc (bytevector-u8-ref bv i))))))

(define (bytevector-append . bvs)
  ;; TODO: is the other version faster?
  (call-with-bytevector-output-port
    (lambda (p)
      (for-each (lambda (bv)
                  (put-bytevector p bv))
                bvs))))

(define (bytevectors-length bvs)
  (do ((l bvs (cdr l))
       (sum 0 (+ sum (bytevector-length (car l)))))
      ((null? l) sum)))

(define (bytevector-concatenate x)
  (let ((length (bytevectors-length x)))
    (do ((bv (make-bytevector length))
         (x x (cdr x))
         (n 0 (+ n (bytevector-length (car x)))))
        ((null? x) bv)
      (bytevector-copy! (car x) 0 bv n (bytevector-length (car x))))))

(define subbytevector
  (case-lambda
    ((bv start end)
     (unless (<= 0 start end (bytevector-length bv))
       (error 'subbytevector "Invalid indices" bv start end))
     (if (and (zero? start)
              (= end (bytevector-length bv)))
         bv
         (let ((ret (make-bytevector (- end start))))
           (bytevector-copy! bv start
                             ret 0 (- end start))
           ret)))
    ((bv start)
     (subbytevector bv start (bytevector-length bv)))))

(define bytevector-u8-index
  (case-lambda
    ((bv c start end)
     (let lp ((i start))
       (cond ((= end i)
              #f)
             ((= (bytevector-u8-ref bv i) c)
              i)
             (else
              (lp (+ i 1))))))
    ((bv c start)
     (bytevector-u8-index bv c start (bytevector-length bv)))
    ((bv c)
     (bytevector-u8-index bv c 0 (bytevector-length bv)))))

(define bytevector-u8-index-right
  (case-lambda
    ((bv c start end)
     (assert (<= 0 c 255))
     (assert (<= 0 start end (bytevector-length bv)))
     (let lp ((i (- end 1)))
       (cond ((< i start) #f)
             ((= (bytevector-u8-ref bv i) c) i)
             (else (lp (- i 1))))))
    ((bv c start)
     (bytevector-u8-index-right bv c start (bytevector-length bv)))
    ((bv c)
     (bytevector-u8-index-right bv c 0 (bytevector-length bv)))))

(define bytevector->uint
  (case-lambda
    ((bv)
     (bytevector->uint bv (endianness big)))
    ((bv endian)
     (if (zero? (bytevector-length bv))
         0
         (bytevector-uint-ref bv 0 endian (bytevector-length bv))))))

(define bytevector->sint
  (case-lambda
    ((bv)
     (bytevector->sint bv (endianness big)))
    ((bv endian)
     (if (zero? (bytevector-length bv))
         0
         (bytevector-sint-ref bv 0 endian (bytevector-length bv))))))

(define uint->bytevector
  (case-lambda
    ((int)
     (uint->bytevector int (endianness big)))
    ((int endian)
     (uint->bytevector int endian #f))
    ((int endian len)
     (if (and (zero? int) (not len))
         #vu8()
         (let* ((len (or len (fxdiv (fxand -8 (fx+ 7 (bitwise-length int))) 8)))
                (ret (make-bytevector len)))
           (bytevector-uint-set! ret 0 int endian (bytevector-length ret))
           ret)))))

(define sint->bytevector
  (case-lambda
    ((int)
     (sint->bytevector int (endianness big)))
    ((int endian)
     (sint->bytevector int endian #f))
    ((int endian len)
     (if (and (zero? int) (not len))
         #vu8()
         (let* ((len (or len (fxdiv (fxand -8 (fx+ 8 (bitwise-length int))) 8)))
                (ret (make-bytevector len)))
           (bytevector-sint-set! ret 0 int endian (bytevector-length ret))
           ret)))))

;; Drop-in replacement for bytevector=? that does not leak
;; information about the outcome of the comparison via the time it
;; takes to perform it. Because this sort of operation will most
;; often be used on digests of various kinds there is no danger in
;; exiting early whenever the lengths are different. It works by
;; accumulating the differences between the bytevectors.
(define (bytevector=?/constant-time bv1 bv2)
  (let ((len (bytevector-length bv1)))
    (and (= len (bytevector-length bv2))
         (if (even? len)
             (do ((i 0 (+ i 2))
                  (diff 0 (fxior diff
                                 (fxxor
                                  (bytevector-u16-native-ref bv1 i)
                                  (bytevector-u16-native-ref bv2 i)))))
                 ((= i len) (fxzero? diff)))
             (do ((i 0 (+ i 1))
                  (diff 0 (fxior diff
                                 (fxxor
                                  (bytevector-u8-ref bv1 i)
                                  (bytevector-u8-ref bv2 i)))))
                 ((= i len) (fxzero? diff))))))))
