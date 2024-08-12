;; -*- mode: scheme; coding: utf-8 -*-
;; Password utilities
;; Copyright © 2009, 2012, 2018 Göran Weinholt <goran@weinholt.se>

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

;; Basic usage: (string=? hash (crypt password hash))

;; Supports the Modular Crypt Format.

;; http://docs.sun.com/app/docs/doc/816-5175/6mbba7evg
;; http://www.freebsd.org/cgi/man.cgi?query=crypt&apropos=0&sektion=3&manpath=FreeBSD+7.2-RELEASE&format=html
;; http://www.kernel.org/doc/man-pages/online/pages/man3/crypt.3.html
;; http://www.usenix.org/events/usenix99/provos.html
;; http://people.redhat.com/drepper/SHA-crypt.txt

(library (industria password)
  (export crypt)
  (import (rnrs)
          (only (srfi :1 lists) make-list circular-list take drop)
          (only (srfi :13 strings) string-prefix? string-suffix?
                string-index string-index-right)
          (industria bytevectors)
          (industria crypto des)
          (hashing md5)
          (industria base64))

  (define (print . x) (for-each display x) (newline))

  ;; Create a bytevector of length `n' containing the repeated
  ;; sequence of bv.
  (define (bytevector-repeat bv n)
    (u8-list->bytevector
     (take (apply circular-list
                  (bytevector->u8-list bv))
           n)))

  (define (salt-type salt)
    (define (in-des? x)
      (or (char<=? #\a x #\z) (char<=? #\A x #\Z)
          (char=? x #\.) (char=? x #\/)
          (char<=? #\0 x #\9)))
    (cond ((and (not (string-suffix? "$" salt))
                (= (string-length salt) 2)
                (for-all in-des? (string->list salt)))
           'des)
          ;; $<ID>$<SALT>$<PWD>
          ((string-prefix? "$1$" salt) 'md5)
          ((string-prefix? "$2$" salt) 'blowfish)
          ((string-prefix? "$2a$" salt) 'eksblowfish)
          ((string-prefix? "$3$" salt) 'nt-hash)
          ((string-prefix? "$5$" salt) 'sha-256)
          ((string-prefix? "$6$" salt) 'sha-512)
          ((string-prefix? "$md5$" salt) 'sun-md5)
          (else #f)))

  ;; Take a salt with or without magic, or a hash and return only the
  ;; salt.
  (define (extract-salt salt)
    (cond ((string-prefix? "$" salt)
           (let ((last$ (string-index-right salt #\$)))
             (substring salt
                        (+ 1 (string-index-right salt #\$ 0 last$))
                        last$)))
          ((= (string-length salt) 13)
           (substring salt 0 2))        ;DES
          (else
           salt)))
  
  ;; This is the same password scrambler available in the standard C
  ;; library. The salt should include the magic. The salt can also be
  ;; a hash.
  (define (crypt key salt)
    (assert (and (string? key) (string? salt)))
    (case (salt-type salt)
      ((des) (des-crypt key (extract-salt salt)))
      ((md5) (md5-crypt key salt))
      (else
       (error 'crypt "unimplemented crypto" (salt-type salt) salt))))

;;; Poul-Henning Kamp's crypt with md5

  (define md5-alphabet
    "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

  (define (md5-base64-encode salt bv)
    ;; It's somewhat disturbing that nobody seems able to use the
    ;; normal base64 encoding.
    (define (get24 a b c)
      (bitwise-ior (bitwise-arithmetic-shift-left
                    (bytevector-u8-ref bv a) 16)
                   (bitwise-arithmetic-shift-left
                    (bytevector-u8-ref bv b) 8)
                   (bytevector-u8-ref bv c)))
    (call-with-string-output-port
      (lambda (p)
        (define (put b bits)
          (do ((b b (bitwise-arithmetic-shift-right b 6))
               (n 0 (+ n 6)))
              ((= n bits))
            (put-char p (string-ref md5-alphabet
                                    (bitwise-bit-field b 0 6)))))
        (display "$1$" p)
        (display (utf8->string salt) p)
        (display "$" p)
        (do ((i 0 (+ i 1)))
            ((= i 5))
          (put (get24 i (+ i 6) (if (= i 4) 5 (+ i 12))) 24))
        (put (bytevector-u8-ref bv 11) 12))))

  (define (md5-crypt key salt)
    (let ((key (string->utf8 key))
          (salt (string->utf8 (extract-salt salt))))
      ;; XXX: salt without magic must be at most 8 characters, they
      ;; say
      (do ((i 0 (fx+ i 1))
           (hash (md5->bytevector
                  (md5 key (string->utf8 "$1$") salt
                       (bytevector-repeat
                        (md5->bytevector (md5 key salt key))
                        (bytevector-length key))
                       (call-with-bytevector-output-port
                         (lambda (p)
                           (do ((i (bytevector-length key) (div i 2)))
                               ((zero? i))
                             (put-u8 p (if (zero? (mod i 2))
                                           (bytevector-u8-ref key 0)
                                           0)))))))
                 (md5->bytevector
                  (md5 (if (zero? (mod i 2)) hash key)
                       (if (zero? (mod i 3)) #vu8() salt)
                       (if (zero? (mod i 7)) #vu8() key)
                       (if (zero? (mod i 2)) key hash)))))
          ((fx=? i 1000) (md5-base64-encode salt hash)))))

  )
