;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2020 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Easy definition of special-cased fixnum procedures.

(library (hashing fixnums)
  (export
    define-fixnum-procedures)
  (import
    (rnrs (6)))

  (define-syntax define-fx
    (lambda (x)
      (syntax-case x ()
        ((k prefix bit-width op-name fxname bitwise-name)
         (with-syntax ((name (datum->syntax #'prefix
                                            (string->symbol
                                             (string-append
                                              (symbol->string (syntax->datum #'prefix))
                                              (symbol->string (syntax->datum #'op-name)))))))
           #'(define name
               (if (> (fixnum-width) bit-width)
                   fxname bitwise-name)))))))

  (define-syntax define-fixnum-procedures
    (lambda (x)
      (syntax-case x ()
        ((_ prefix bit-width)
         #'(begin
             (define-fx prefix bit-width and fxand bitwise-and)
             (define-fx prefix bit-width xor fxxor bitwise-xor)
             (define-fx prefix bit-width ior fxior bitwise-ior)
             (define-fx prefix bit-width not fxnot bitwise-not)
             (define-fx prefix bit-width + fx+ +)
             (define-fx prefix bit-width - fx- -)
             (define-fx prefix bit-width * fx* *)
             (define-fx prefix bit-width =? fx=? =)
             (define-fx prefix bit-width bit-set? fxbit-set? bitwise-bit-set?)
             (define-fx prefix bit-width arithmetic-shift-right
                        fxarithmetic-shift-right bitwise-arithmetic-shift-right)
             (define-fx prefix bit-width arithmetic-shift-left
                        fxarithmetic-shift-left bitwise-arithmetic-shift-left)
             (define-fx prefix bit-width zero? fxzero? zero?)
             (define-fx prefix bit-width bit-field fxbit-field bitwise-bit-field)
             (define-fx prefix bit-width rotate-bit-field
                        fxrotate-bit-field bitwise-rotate-bit-field)
             (define-fx prefix bit-width length
                        fxlength bitwise-length)))))))
