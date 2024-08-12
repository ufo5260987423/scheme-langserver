;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2018, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Private parsing and formatting

(library (industria ssh private serialize)
  (export
    get-record read-byte read-uint32
    read-bytevector read-string read-name-list read-mpint
    put-record put-bvstring put-name-list put-mpint
    integer->mpint

    get-uint32 get-mpint get-bytevector get-string put-string)
  (import
    (except (rnrs (6)) put-string)
    (only (srfi :13 strings) string-join)
    (srfi :26 cut)
    (industria bytevectors)
    (industria buffer)
    (industria strings)
    (struct pack))

(define (get-record b make field-types)
  (define (read b type)
    (case type
      ((string) (read-string b))
      ((bytevector) (read-bytevector b))
      ((uint32) (read-uint32 b))
      ((mpint) (read-mpint b))
      ((name-list) (read-name-list b))
      ((boolean) (positive? (read-byte b)))
      ((byte) (read-byte b))
      ((cookie)
       (when (< (buffer-length b) 16)
         (error 'get-record "short record" (buffer-length b)))
       (let ((bv (subbytevector (buffer-data b)
                                (buffer-top b)
                                (+ (buffer-top b) 16))))
         (buffer-seek! b 16)
         bv))
      (else
       (error 'get-record "bug: unknown type" type))))
  (do ((field 0 (+ field 1))
       (types field-types (cdr types))
       (ret '() (cons (read b (car types)) ret)))
      ((null? types) (apply make (reverse ret)))))

(define (read-byte b)
  (let ((x (read-u8 b 0)))
    (buffer-seek! b 1)
    x))

(define (read-uint32 b)
  (let ((x (read-u32 b 0)))
    (buffer-seek! b 4)
    x))

(define (read-bytevector b)
  (let ((len (read-u32 b 0)))
    (when (> len (buffer-length b))
      (error 'read-bytevector "overlong string" len))
    (buffer-seek! b 4)
    (let ((bv (subbytevector (buffer-data b)
                             (buffer-top b)
                             (+ (buffer-top b) len))))
      (buffer-seek! b len)
      bv)))

(define (read-string b)
  (utf8->string (read-bytevector b)))

(define (read-name-list b)
  (let ((str (read-string b)))
    (if (string=? str "")
        '()
        (string-split str #\,))))

(define (read-mpint b)
  (let ((bv (read-bytevector b)))
    (bytevector-sint-ref bv 0 (endianness big) (bytevector-length bv))))

;;; Formatting

(define (put-record p msg rtd field-types)
  (do ((rtd (or rtd (record-rtd msg)))
       (field 0 (+ field 1))
       (types field-types (cdr types)))
      ((null? types))
    (let ((v ((record-accessor rtd field) msg)))
      (case (car types)
        ((string bytevector) (put-bvstring p v))
        ((uint32) (put-bytevector p (pack "!L" v)))
        ((mpint) (put-mpint p v))
        ((name-list) (put-name-list p v))
        ((boolean) (put-u8 p (if v 1 0)))
        ((byte) (put-u8 p v))
        ((cookie) (put-bytevector p v 0 16))
        (else
         (error 'put-record "bug: unknown type"
                (car types)))))))

(define (put-bvstring p s)
  (let ((bv (if (string? s) (string->utf8 s) s)))
    (put-bytevector p (pack "!L" (bytevector-length bv)))
    (put-bytevector p bv)))

(define (put-name-list p l)
  (put-bvstring p (string-join l ",")))

(define (mpnegative? bv)
  (and (fx>=? (bytevector-length bv) 1)
       (fxbit-set? (bytevector-u8-ref bv 0) 7)))

(define (put-mpint p i)
  (let ((bv (uint->bytevector i)))
    (cond ((mpnegative? bv)
           ;; Prevent this from being considered a negative number
           (put-bytevector p (pack "!L" (+ 1 (bytevector-length bv))))
           (put-u8 p 0)
           (put-bytevector p bv))
          (else
           (put-bytevector p (pack "!L" (bytevector-length bv)))
           (put-bytevector p bv)))))

(define (integer->mpint int)
  (call-with-bytevector-output-port
    (cut put-mpint <> int)))

;;; Port-based parsing/formatting, mostly for keys

(define (get-uint32 p)
  (get-unpack p "!L"))

(define (get-mpint p)
  (let ((bv (get-bytevector-n p (get-unpack p "!L"))))
    (when (mpnegative? bv)
      (assertion-violation 'get-mpint
                           "Refusing to read a negative mpint"))
    (bytevector->uint bv)))

(define (get-bytevector p)
  (let ((len (get-unpack p "!L")))
    (if (eqv? len 0)
        #vu8()
        (get-bytevector-n p len))))

(define (get-string p)
  (let ((len (get-unpack p "!L")))
    (if (eqv? len 0)
        ""
        (let ((bv (get-bytevector-n p len)))
          (if (eof-object? bv)
              bv
              (utf8->string bv))))))

(define (put-string p bv)
  (put-bytevector p (pack "!L" (bytevector-length bv)))
  (put-bytevector p bv)))
