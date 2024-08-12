;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Stuff that is needed by more than one of the DNS libraries

(library (industria dns private)
  (export put-labels parse-labels       ;wire format
          display-dns-label display-dns-string) ;master file format
  (import (rnrs)
          (srfi :26 cut)
          (srfi :39 parameters)
          (industria bytevectors)
          (struct pack))

  (define (displayer lower permitted prefix suffix)
    (lambda (bv p)
      (display prefix p)
      (do ((i 0 (+ i 1)))
          ((= i (bytevector-length bv)))
        (let* ((b (bytevector-u8-ref bv i))
               (c (integer->char b)))
          (cond ((memv c permitted)
                 (display #\\ p)
                 (display c p))
                ((< lower b #x7f)
                 (display c p))
                (else
                 (display #\\ p)
                 (if (< b 100) (display #\0 p))
                 (if (< b 10) (display #\0 p))
                 (display b p)))))
      (display suffix p)))

  (define display-dns-label
    (displayer #x20 '(#\@ #\. #\\ #\( #\) #\; #\$ #\") "" "."))

  (define display-dns-string
    (displayer #x1f '(#\\ #\") "\"" "\""))

  (define put-labels
    (case-lambda
      ((port labels table offset)
       (let lp ((labels labels))
         (cond ((null? labels)
                (put-u8 port 0))
               ((and table (hashtable-ref table (car labels) #f))
                => (lambda (position)
                     (put-bytevector port
                                     (pack "!S"
                                           (fxior (fxarithmetic-shift-left #b11 (+ 8 6))
                                                  position)))))
               (else
                (let ((label (car labels)))
                  (unless (< 0 (bytevector-length label) 64)
                    (error 'format-labels "Invalid DNS label" labels label))
                  (when table           ;compression table
                    (let ((pos (+ (port-position port) offset)))
                      (when (< pos #b0011111111111111)
                        (hashtable-set! table label pos))))
                  (put-u8 port (bytevector-length label))
                  (put-bytevector port label)
                  (lp (cdr labels)))))))
      ((port labels)
       (put-labels port labels #f 0))))

  ;; Parses labels in the wire format. This needs access to the whole
  ;; query if there are any compressed labels.
  (define (parse-labels bv start)
    (define who 'parse-labels)
    (let lp ((start start)
             (ret '())
             (acclen 0)
             (used-offsets '())
             (end #f)) ;the end offset of the label (does not follow pointers)
      (when (> start (bytevector-length bv))
        (error who "invalid pointer in a name" start))
      ;; Detect pointer loops. If I was much too clever for anyone's
      ;; good I might have translated these into circular lists
      ;; instead, and even supported them in put-dns-message. Who
      ;; wants an infinite domain name?
      (when (memv start used-offsets)
        (error who "looping name"))
      (let* ((len (bytevector-u8-ref bv start))
             (tag (fxbit-field len 6 8)))
        (cond ((zero? len)
               (values (reverse ret) (or end (+ start 1))))
              ((zero? tag)              ;normal label
               (when (> (+ acclen len 1) 255)
                 (error who "overlong name" acclen))
               (lp (+ start 1 len)
                   (cons (subbytevector bv (+ start 1) (+ start 1 len))
                         ret)
                   (+ acclen len 1) (cons start used-offsets) end))
              ((= #b11 tag)             ;pointer
               (lp (fxior (bitwise-arithmetic-shift (fxand len #b111111) 8)
                          (bytevector-u8-ref bv (+ start 1)))
                   ret acclen (cons start used-offsets)
                   (or end (+ start 2))))
              ;; TODO: #b01 EDNS0 rfc2671
              (else (error who "reserved bits in a length field" len)))))))
