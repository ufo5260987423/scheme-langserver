;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2020 Göran Weinholt <goran@weinholt.se>

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

(library (industria crypto private chacha20)
  (export add32 xor32 rot32)
  (import (rnrs))

(define-syntax rot32
  (syntax-rules ()
    ((_ n^ a^)
     (let ((n n^) (a a^))
       (if (> (fixnum-width) 32)
           (fxior (fxand #xFFFFFFFF (fxarithmetic-shift-left (fxand n (fx- (fxarithmetic-shift-left 1 (fx- 32 a)) 1))
                                                             a))
                  (fxarithmetic-shift-right n (fx- 32 a)))
           (bitwise-ior (bitwise-and #xFFFFFFFF (bitwise-arithmetic-shift-left n a))
                        (bitwise-arithmetic-shift-right n (- 32 a))))))))

(define-syntax xor32
  (syntax-rules ()
    ((_ a b)
     (if (> (fixnum-width) 32)
         (fxxor a b)
         (bitwise-xor a b)))))

(define-syntax add32
  (syntax-rules ()
    ((_ a b)
     (if (> (fixnum-width) 32)
         (fxand #xFFFFFFFF (fx+ a b))
         (bitwise-and #xFFFFFFFF (+ a b)))))))
