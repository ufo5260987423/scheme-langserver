;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2017, 2021 Göran Weinholt <goran@weinholt.se>

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

;; Internet address parsing and formatting

;; RFC 4291 IP Version 6 Addressing Architecture.
;; RFC 5952 A Recommendation for IPv6 Address Text Representation.

(library (ip-address)
  (export ipv4->string string->ipv4
          ipv6->string string->ipv6)
  (import (rnrs (6)))

(define (put-ipv4-string p addr i)
  (let ((v0 (bytevector-u16-ref addr i (endianness big)))
        (v1 (bytevector-u16-ref addr (fx+ i 2) (endianness big))))
    (let ((a (fxarithmetic-shift-right v0 8))
          (b (fxand v0 #xff))
          (c (fxarithmetic-shift-right v1 8))
          (d (fxand v1 #xff)))
      (display a p) (put-char p #\.)
      (display b p) (put-char p #\.)
      (display c p) (put-char p #\.)
      (display d p))))

(define (ipv4->string addr)
  (call-with-string-output-port
    (lambda (p)
      (put-ipv4-string p addr 0))))

;; Accepts leading zeros, like in: 192.000.002.000
(define (string->ipv4 str)
  (let ((ret (make-bytevector 4)))
    (let ((in (open-string-input-port str)))
      (define (parse-octet idx)
        (let lp ((octet 0) (n 3))
          (let ((c (lookahead-char in)))
            (if (and (fxpositive? n) (char? c) (char<=? #\0 c #\9))
                (lp (fx+ (fx* octet 10)
                         (fx- (char->integer (get-char in))
                              (char->integer #\0)))
                    (fx- n 1))
                (cond ((and (fx<=? 0 octet 255) (fx<? n 3))
                       (bytevector-u8-set! ret idx octet)
                       #t)
                      (else #f))))))
      (define (parse-dot)
        (eqv? (get-char in) #\.))
      (and (parse-octet 0) (parse-dot)
           (parse-octet 1) (parse-dot)
           (parse-octet 2) (parse-dot)
           (parse-octet 3) (port-eof? in)
           ret))))

(define (word i addr)
  (bytevector-u16-ref addr (fx+ i i) (endianness big)))

(define (compression-index addr)
  ;; Finds the largest span of zero words. Chooses the first span if
  ;; two spans are of equal length.
  (let lp ((i 0) (start -1) (len 0) (start* -1) (len* 0))
    (cond ((fx=? i 8)
           (if (fx>? len len*)
               (values start len)
               (values start* len*)))
          ((fxzero? (word i addr))
           (lp (fx+ i 1) (if (fx=? start -1) i start)
               (fx+ len 1) start* len*))
          ((fx>? len len*)
           (lp (fx+ i 1) -1 0 start len))
          (else
           (lp (fx+ i 1) -1 0 start* len*)))))

;; Check for ::ffff:0:0/96
(define (ipv4-mapped-address? addr)
  (let lp ((i 0))
    (cond ((fx=? i 5)
           (eqv? #xFFFF (word 5 addr)))
          ((not (eqv? 0 (word i addr)))
           #f)
          (else
           (lp (fx+ i 1))))))

(define (ipv6->string addr)
  (define digits "0123456789abcdef")
  (call-with-string-output-port
    (lambda (p)
      (cond
        ((ipv4-mapped-address? addr)
         (put-string p "::ffff:")
         (put-ipv4-string p addr 12))
        (else
         (let-values (((cidx* clen) (compression-index addr)))
           (let ((cidx (if (fx=? clen 1) -1 cidx*)))
             (do ((i 0 (if (fx=? i cidx) (fx+ i clen) (fx+ i 1))))
                 ((fx=? i 8)
                  (when (fx=? i (fx+ cidx clen))
                    (put-char p #\:)))
               (cond ((fx=? i cidx)
                      (put-char p #\:))
                     (else
                      (unless (fxzero? i)
                        (put-char p #\:))
                      (let ((w (word i addr)))
                        (let ((a (fxbit-field w 12 16))
                              (b (fxbit-field w 8 12))
                              (c (fxbit-field w 4 8))
                              (d (fxbit-field w 0 4)))
                          (when (fx>? w #xfff)
                            (put-char p (string-ref digits a)))
                          (when (fx>? w #xff)
                            (put-char p (string-ref digits b)))
                          (when (fx>? w #xf)
                            (put-char p (string-ref digits c)))
                          (put-char p (string-ref digits d))))))))))))))

;; Returns a bytevector or #f.
(define (string->ipv6 str)
  (define (hexdigit ch)
    (cond ((char<=? #\0 ch #\9)
           (fx- (char->integer ch) (char->integer #\0)))
          ((char<=? #\a ch #\f)
           (fx+ 10 (fx- (char->integer ch) (char->integer #\a))))
          ((char<=? #\A ch #\F)
           (fx+ 10 (fx- (char->integer ch) (char->integer #\A))))
          (else #f)))
  (define (parse str start)
    (let ((addr (make-bytevector 16 0))
          (se (string-length str)))
      (let lp ((di 0) (si start) (ai 0) (nibbles 0) (cidx #f) (word 0))
        (cond
          ((fx=? si se)
           (cond ((fxpositive? nibbles)
                  ;; Trailing word
                  (cond ((< ai 16)
                         (bytevector-u16-set! addr ai word (endianness big))
                         (lp di si (fx+ ai 2) 0 cidx 0))
                        (else #f)))
                 (cidx
                  ;; The string used compression, move the words to
                  ;; the right.
                  (let ((didx (fx- 16 (fx- ai cidx))))
                    (bytevector-copy! addr cidx addr didx (fx- ai cidx))
                    (do ((i cidx (fx+ i 2)))
                        ((fx=? i didx) addr)
                      (bytevector-u16-native-set! addr i 0))))
                 ((fx=? ai 16) addr)
                 (else #f)))      ;too many/few words
          (else
           (let ((c (string-ref str si)))
             (cond
               ((eqv? #\: c)
                (cond ((fxzero? nibbles)
                       ;; Compression
                       (and (not cidx)
                            (lp (fx+ si 1) (fx+ si 1) ai nibbles ai word)))
                      ((fx<? ai 14)
                       (bytevector-u16-set! addr ai word (endianness big))
                       (lp (fx+ si 1) (fx+ si 1) (fx+ ai 2) 0 cidx 0))
                      (else #f)))      ;bad place for a colon
               ((hexdigit c)
                => (lambda (n)
                     (and (fx<? nibbles 4)
                          (lp di (fx+ si 1) ai (fx+ nibbles 1) cidx
                              (fxior n (fxarithmetic-shift-left word 4))))))
               ((eqv? c #\.)
                ;; IPv4-mapped address. This treats an IPv4-address as
                ;; a 32-bit word that always appears at the end of an
                ;; address.
                (cond ((string->ipv4 (substring str di (string-length str))) =>
                       (lambda (ipv4)
                         (bytevector-copy! ipv4 0 addr ai 4)
                         (lp di se (fx+ ai 4) 0 cidx 0)))
                      (else #f)))
               (else #f))))))))
  (if (and (fx>? (string-length str) 0) (char=? (string-ref str 0) #\:))
      (and (fx>? (string-length str) 1) (char=? (string-ref str 1) #\:)
           (parse str 1))
      (parse str 0))))
