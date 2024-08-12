;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2013, 2017 Göran Weinholt <goran@weinholt.se>

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

;; Universally Unique IDentifiers (RFC 4122).

(library (uuid)
  (export nil-uuid
          uuid-namespace-dns uuid-namespace-url
          uuid-namespace-oid uuid-namespace-x.500
          time-uuid random-uuid md5-uuid sha-1-uuid
          string->uuid uuid->string
          uuid-info)
  (import (rnrs)
          (srfi :19 time)
          (industria crypto entropy)
          (hashing md5)
          (hashing sha-1)
          (struct pack))

  (define (nil-uuid)
    #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

  (define (uuid-namespace-dns)
    #vu8(#x6b #xa7 #xb8 #x10  #x9d #xad  #x11 #xd1
              #x80 #xb4 #x00 #xc0 #x4f #xd4 #x30 #xc8))

  (define (uuid-namespace-url)
    #vu8(#x6b #xa7 #xb8 #x11  #x9d #xad  #x11 #xd1
              #x80 #xb4 #x00 #xc0 #x4f #xd4 #x30 #xc8))

  (define (uuid-namespace-oid)
    #vu8(#x6b #xa7 #xb8 #x12  #x9d #xad  #x11 #xd1
              #x80 #xb4 #x00 #xc0 #x4f #xd4 #x30 #xc8))

  (define (uuid-namespace-x.500)
    #vu8(#x6b #xa7 #xb8 #x14  #x9d #xad  #x11 #xd1
              #x80 #xb4 #x00 #xc0 #x4f #xd4 #x30 #xc8))

  (define (%random-node-id)
    ;; Generate a random node address and set the multicast bit to
    ;; show that it is random.
    (let ((r (make-random-bytevector 48/8)))
      (pack! "C" r 0 (fxior (unpack "C" r) #b1))
      r))

  ;; Time-based UUID.
  (define time-uuid
    (let ((*ieee-address* #f)
          (*last-time* #f)
          (*count* 0)
          (*clock-seq* #f))
      (define (get-address node-id)
        (cond ((and (bytevector? node-id)
                    (eqv? (bytevector-length node-id) 48/8))
               ;; A node-id was provided
               node-id)
              ((memv node-id '(#f generate))
               ;; Generate a random node address that persists across
               ;; calls. TODO: for #f, try to use one of the addresses
               ;; from the system.
               (unless *ieee-address*
                 (set! *ieee-address* (%random-node-id)))
               *ieee-address*)
              (else
               (assertion-violation
                'time-uuid "This is not a valid node-id argument"
                node-id))))
      (define (init-clock-seq)
        (set! *clock-seq* (random-integer (expt 2 14)))
        (set! *count* 0))
      (define (increment-clock-seq)
        (set! *clock-seq* (mod (+ *clock-seq* 1)
                               (expt 2 14)))
        (set! *count* 0))
      (define (get-time)
        (let retry ((attempts 0))
          (let ((t (current-time)))
            (cond ((not *clock-seq*)
                   ;; Initialize.
                   (init-clock-seq)
                   t)
                  ((and *last-time* (time<? t *last-time*))
                   ;; Time goes backwards. This generates duplicate
                   ;; UUIDs if the clock is stuck in a repeating
                   ;; decrementing loop.
                   (increment-clock-seq)
                   t)
                  ((time=? t *last-time*)
                   ;; Same time as last time.
                   (cond ((< *count* (- (/ (time-resolution) 100) 1))
                          (set! *count* (+ *count* 1))
                          *last-time*)
                         ((< attempts 100)
                          ;; More UUIDs than the resolution of the
                          ;; clock can handle.
                          (retry (+ attempts 1)))
                         (else
                          ;; Got stuck when waiting for the next clock
                          ;; tick.
                          (increment-clock-seq)
                          (retry 0))))
                  (else t)))))
      (case-lambda
        (()
         (time-uuid #f))
        ((node-id)
         (let ((addr (get-address node-id))
               (time (get-time)))
           (set! *last-time* time)
           (let* ((td (time-difference time
                                       (date->time-utc
                                        (make-date 0 0 0 0 15 10 1582 0))))
                  ;; 100ns ticks since Oct 15, 1582.
                  (t (+ (div (+ (time-nanosecond td)
                                (* (time-second td) (expt 10 9)))
                             100)
                        *count*))
                  (version 1))
             (call-with-bytevector-output-port
               (lambda (p)
                 (put-pack p "!LSSS"
                           (bitwise-bit-field t 0 32)
                           (bitwise-bit-field t 32 48)
                           (fxior (bitwise-bit-field t 48 60)
                                  (fxarithmetic-shift-left version 12))
                           (fxior (fxarithmetic-shift-left #b10 14)
                                  (fxand *clock-seq* #x3fff)))
                 (put-bytevector p addr)))))))))

  ;; Name-based UUID with MD5.
  (define (md5-uuid namespace name)
    (assert (eqv? (bytevector-length namespace) 16))
    (let ((name (if (string? name) (string->utf8 name) name)))
      (let ((h (md5->bytevector (md5 namespace name))))
        (%fix-random h 3))))

  ;; Pseudo-randomly generated UUID.
  (define (random-uuid)
    (%fix-random (make-random-bytevector 16) 4))

  ;; Name-based UUID with SHA-1.
  (define (sha-1-uuid namespace name)
    (assert (eqv? (bytevector-length namespace) 16))
    (let ((name (if (string? name) (string->utf8 name) name)))
      (let ((h (sha-1->bytevector (sha-1 namespace name))))
        (%fix-random h 5))))

  (define (%fix-random u version)
    (let-values (((time-low time-mid time-hi/version clk-seq/res node1 node2)
                  (unpack "!LSSSSL" u)))
      (pack "!LSSSSL" time-low time-mid
            (fxior (fxarithmetic-shift-left version 12)
                   (fxand time-hi/version #xfff))
            (fxior (fxarithmetic-shift-left #b10 14)
                   (fxand clk-seq/res #x3fff))
            node1 node2)))

  ;; Convert a string with a UUID to a bytevector.
  (define (string->uuid s)
    (define (assvio s)
      (assertion-violation 'string->uuid "This is not a valid UUID" s))
    (define (dehex s i)
      (let ((i (char->integer (string-ref s i))))
        (cond ((fx<=? (char->integer #\0) i (char->integer #\9))
               (fx- i (char->integer #\0)))
              ((fx<=? (char->integer #\A) i (char->integer #\F))
               (fx- i (fx- (char->integer #\A) #xa)))
              ((fx<=? (char->integer #\a) i (char->integer #\f))
               (fx- i (fx- (char->integer #\a) #xa)))
              (else #f))))
    (let ((u (make-bytevector 16)))
      (let lp ((si 0) (ui 0) (upper-nibble #f))
        (cond ((= si (string-length s))
               (if (and (eqv? ui 16) (not upper-nibble))
                   u
                   (assvio s)))
              ((dehex s si) =>
               (lambda (n)
                 (cond ((= ui (bytevector-length u))
                        (assvio s))
                       (upper-nibble
                        (bytevector-u8-set! u ui (fxior upper-nibble n))
                        (lp (+ si 1) (+ ui 1) #f))
                       (else
                        (lp (+ si 1) ui (fxarithmetic-shift-left n 4))))))
              (else
               (lp (+ si 1) ui upper-nibble))))))

  ;; Convert a UUID to its string representation.
  (define (uuid->string u)
    (define hex "0123456789abcdef")
    (assert (eqv? (bytevector-length u) 16))
    (call-with-string-output-port
      (lambda (p)
        (let lp ((si 0) (ui 0))
          (case si
            ((36) #f)
            ((8 13 18 23)
             (put-char p #\-)
             (lp (+ si 1) ui))
            (else
             (let ((b (bytevector-u8-ref u ui)))
               (put-char p (string-ref hex (fxarithmetic-shift-right b 4)))
               (put-char p (string-ref hex (fxand b #b1111)))
               (lp (+ si 2) (+ ui 1)))))))))

  ;; Extract as much information from the UUID as possible.
  (define (uuid-info u)
    (define asl bitwise-arithmetic-shift-left)
    (define fxasr fxarithmetic-shift-right)
    (let ((v (bytevector-u8-ref u 8)))
      (cond ((eqv? (fxand v #b10000000) #b00000000)
             (if (equal? u (nil-uuid))
                 '((variant . nil))
                 '((variant . ncs))))
            ((eqv? (fxand v #b11000000) #b10000000)
             (let-values (((time-low time-mid time-hi/version clk-seq/res)
                           (unpack "!LSSS6x" u)))
               (let* ((vers (fxasr time-hi/version 12))
                      (info `((variant . rfc4122)
                              (version . ,vers)
                              (reserved-field . ,(fxasr clk-seq/res 14)))))
                 (case vers
                   ((1)
                    (let ((t (bitwise-ior (asl (fxand time-hi/version #xfff) 48)
                                           (asl time-mid 32)
                                           time-low))
                          (clock-sequence (fxand clk-seq/res #x3fff)))
                      (let-values (((s ns) (div-and-mod t (/ (expt 10 9) 100))))
                        (let ((time (add-duration
                                     (date->time-utc
                                      (make-date 0 0 0 0 15 10 1582 0))
                                     (make-time 'time-duration ns s))))
                          `(,@info
                            (version-name . time)
                            (time . ,time)
                            (clock-sequence . ,clock-sequence)
                            (node . ,(call-with-port (open-bytevector-input-port u)
                                       (lambda (p)
                                         (set-port-position! p 10)
                                         (get-bytevector-n p 6))))
                            ;; Check the multicast bit
                            (node-random? . ,(fxbit-set? (bytevector-u8-ref u 10) 0)))))))
                   ;; TODO: DCE contains a POSIX UID
                   ((2) `(,@info (version-name . dce-security)))
                   ((3) `(,@info (version-name . md5)))
                   ((4) `(,@info (version-name . random)))
                   ((5) `(,@info (version-name . sha-1)))
                   (else
                    info)))))
            ((eqv? (fxand v #b11100000) #b11000000)
             ;; TODO: these apparently have all sorts of subtypes
             '((variant . microsoft-com)))
            ((eqv? (fxand v #b11110000) #b11100000)
             '((variant . reserved)))
            (else
             '((variant . unknown)))))))
