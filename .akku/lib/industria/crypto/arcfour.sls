;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; ARCFOUR encryption

(library (industria crypto arcfour)
  (export expand-arcfour-key arcfour!
          arcfour-discard!
          clear-arcfour-keystream!)
  (import (rnrs)
          (only (srfi :1 lists) iota))

  (define (bytevector-u8-swap! bv i j)
    (let ((tmp (bytevector-u8-ref bv i)))
      (bytevector-u8-set! bv i (bytevector-u8-ref bv j))
      (bytevector-u8-set! bv j tmp)))

  ;; Expand the key into a value suitable for arcfour!.
  (define (expand-arcfour-key key)
    (unless (<= 1 (bytevector-length key) 255)
      (error 'expand-arcfour-key
             "The key must be more than zero and less than 256 bytes long"
             (bytevector-length key)))
    (let ((S (u8-list->bytevector (iota 256)))
          (len (bytevector-length key)))
      (let lp ((i 0) (j 0))
        (if (fx=? i 256)
            (vector S 0 0)
            (let ((j (fxand (fx+ (fx+ (bytevector-u8-ref S i)
                                      (bytevector-u8-ref key (fxmod i len)))
                                 j)
                            #xff)))
              (bytevector-u8-swap! S i j)
              (lp (fx+ i 1) j))))))

  ;; Encipher or decipher the bytes in source and write them to the
  ;; target. The key is updated. There's no check for if source and
  ;; target overlap, but it's ok as long as target-start <=
  ;; source-start.
  (define (arcfour! source source-start target target-start len key)
    (let ((S (vector-ref key 0))
          (se (+ source-start len)))
      (assert (bytevector? S))
      (let lp ((i (vector-ref key 1))
               (j (vector-ref key 2))
               (ss source-start)
               (ts target-start))
        (cond ((fx=? ss se)
               (vector-set! key 1 i)
               (vector-set! key 2 j))
              (else
               (let* ((i (fxand #xff (fx+ i 1)))
                      (j (fxand #xff (fx+ j (bytevector-u8-ref S i)))))
                 (bytevector-u8-swap! S i j)
                 (let ((kb (bytevector-u8-ref S (fxand (fx+ (bytevector-u8-ref S i)
                                                            (bytevector-u8-ref S j))
                                                       #xff)))
                       (pb (bytevector-u8-ref source ss)))
                   (bytevector-u8-set! target ts (fxxor pb kb))
                   (lp i j (fx+ ss 1) (fx+ ts 1)))))))))

  ;; Discards n bytes from the keystream. Useful for arcfour128 which
  ;; discards 1536 bytes (RFC4345).
  (define (arcfour-discard! key n)
    (let ((S (vector-ref key 0)))
      (assert (bytevector? S))
      (let lp ((i (vector-ref key 1))
               (j (vector-ref key 2))
               (n n))
        (cond ((fxzero? n)
               (vector-set! key 1 i)
               (vector-set! key 2 j))
              (else
               (let* ((i (fxand #xff (fx+ i 1)))
                      (j (fxand #xff (fx+ j (bytevector-u8-ref S i)))))
                 (bytevector-u8-swap! S i j)
                 (lp i j (fx- n 1))))))))

  (define (clear-arcfour-keystream! key)
    (bytevector-fill! (vector-ref key 0) 0)
    (vector-set! key 0 #f)
    (vector-set! key 1 #f)
    (vector-set! key 2 #f)))
