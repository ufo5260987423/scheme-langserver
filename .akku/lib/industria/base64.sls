;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2013, 2018 Göran Weinholt <goran@weinholt.se>

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

;; RFC 4648 Base-N Encodings

;; TODO: The procedures in this API has too many arguments, it would
;; be better if most were parameters.

(library (industria base64)
  (export base64-encode
          base64-decode
          base64-alphabet
          base64url-alphabet
          get-delimited-base64
          put-delimited-base64)
  (import (rnrs)
          (only (srfi :13 strings)
                string-index
                string-prefix? string-suffix?
                string-concatenate string-trim-both))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define base64url-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

  (define base64-encode
    (case-lambda
      ;; Simple interface. Returns a string containing the canonical
      ;; base64 representation of the given bytevector.
      ((bv)
       (base64-encode bv 0 (bytevector-length bv) #f #f base64-alphabet #f))
      ((bv start)
       (base64-encode bv start (bytevector-length bv) #f #f base64-alphabet #f))
      ((bv start end)
       (base64-encode bv start end #f #f base64-alphabet #f))
      ((bv start end line-length)
       (base64-encode bv start end line-length #f base64-alphabet #f))
      ((bv start end line-length no-padding)
       (base64-encode bv start end line-length no-padding base64-alphabet #f))
      ((bv start end line-length no-padding alphabet)
       (base64-encode bv start end line-length no-padding alphabet #f))
      ;; Base64 encodes the bytes [start,end[ in the given bytevector.
      ;; Lines are limited to line-length characters (unless #f),
      ;; which must be a multiple of four. To omit the padding
      ;; characters (#\=) set no-padding to a true value. If port is
      ;; #f, returns a string.
      ((bv start end line-length no-padding alphabet port)
       (assert (or (not line-length) (zero? (mod line-length 4))))
       (let-values (((p extract) (if port
                                     (values port (lambda () (values)))
                                     (open-string-output-port))))
         (letrec ((put (if line-length
                           (let ((chars 0))
                             (lambda (p c)
                               (when (fx=? chars line-length)
                                 (set! chars 0)
                                 (put-char p #\linefeed))
                               (set! chars (fx+ chars 1))
                               (put-char p c)))
                           put-char)))
           (let lp ((i start))
             (cond ((= i end))
                   ((<= (+ i 3) end)
                    (let ((x (bytevector-uint-ref bv i (endianness big) 3)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (put p (string-ref alphabet (fxbit-field x 6 12)))
                      (put p (string-ref alphabet (fxbit-field x 0 6)))
                      (lp (+ i 3))))
                   ((<= (+ i 2) end)
                    (let ((x (fxarithmetic-shift-left (bytevector-u16-ref bv i (endianness big)) 8)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (put p (string-ref alphabet (fxbit-field x 6 12)))
                      (unless no-padding
                        (put p #\=))))
                   (else
                    (let ((x (fxarithmetic-shift-left (bytevector-u8-ref bv i) 16)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (unless no-padding
                        (put p #\=)
                        (put p #\=)))))))
         (extract)))))

  ;; Create a lookup table for the alphabet and remember the latest table.
  (define get-decode-table
    (let ((ascii-table #f)
          (extra-table '())     ;in the unlikely case of unicode chars
          (table-alphabet #f))
      (lambda (alphabet)
        (unless (eq? alphabet table-alphabet)
          ;; Rebuild the table.
          (do ((ascii (make-vector 128 #f))
               (extra '())
               (i 0 (+ i 1)))
              ((= i (string-length alphabet))
               (set! ascii-table ascii)
               (set! extra-table extra))
            (let ((c (char->integer (string-ref alphabet i))))
              (if (fx<=? c 127)
                  (vector-set! ascii c i)
                  (set! extra (cons (cons c i) extra)))))
          (set! table-alphabet alphabet))
        (values ascii-table extra-table))))

  ;; Decodes a base64 string, optionally ignoring non-alphabet
  ;; characters and lack of padding.
  (define base64-decode
    (case-lambda
      ((str)
       (base64-decode str base64-alphabet #f))
      ((str alphabet)
       (base64-decode str alphabet #f))
      ((str alphabet port)
       (base64-decode str alphabet port #t))
      ((str alphabet port strict?)
       (base64-decode str alphabet port strict? #t))
      ((str alphabet port strict? strict-padding?)
       (define (pad? c) (eqv? c (char->integer #\=)))
       (let-values (((p extract) (if port
                                     (values port (lambda () (values)))
                                     (open-bytevector-output-port)))
                    ((ascii extra) (get-decode-table alphabet)))
         (define-syntax lookup
           (syntax-rules ()
             ((_ c) (or (and (fx<=? c 127) (vector-ref ascii c))
                        (cond ((assv c extra) => cdr)
                              (else #f))))))
         (let lp-restart ((str str))
           (let* ((len (if strict?
                           (string-length str)
                           (let lp ((i (fx- (string-length str) 1)))
                             ;; Skip trailing invalid chars.
                             (cond ((fxzero? i) 0)
                                   ((let ((c (char->integer (string-ref str i))))
                                      (or (lookup c) (pad? c)))
                                    (fx+ i 1))
                                   (else (lp (fx- i 1))))))))
             (let lp ((i 0))
               (cond
                 ((fx=? i len)
                  (extract))
                 ((fx<=? i (fx- len 4))
                  (let lp* ((c1 (char->integer (string-ref str i)))
                            (c2 (char->integer (string-ref str (fx+ i 1))))
                            (c3 (char->integer (string-ref str (fx+ i 2))))
                            (c4 (char->integer (string-ref str (fx+ i 3))))
                            (i i))
                    (let ((i1 (lookup c1)) (i2 (lookup c2))
                          (i3 (lookup c3)) (i4 (lookup c4)))
                      (cond
                        ((and i1 i2 i3 i4)
                         ;; All characters present and accounted for.
                         ;; The most common case.
                         (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                         (fxarithmetic-shift-left i2 12)
                                         (fxarithmetic-shift-left i3 6)
                                         i4)))
                           (put-u8 p (fxbit-field x 16 24))
                           (put-u8 p (fxbit-field x 8 16))
                           (put-u8 p (fxbit-field x 0 8))
                           (lp (fx+ i 4))))
                        ((and i1 i2 i3 (pad? c4) (= i (- len 4)))
                         ;; One padding character at the end of the input.
                         (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                         (fxarithmetic-shift-left i2 12)
                                         (fxarithmetic-shift-left i3 6))))
                           (put-u8 p (fxbit-field x 16 24))
                           (put-u8 p (fxbit-field x 8 16))
                           (lp (fx+ i 4))))
                        ((and i1 i2 (pad? c3) (pad? c4) (= i (- len 4)))
                         ;; Two padding characters.
                         (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                         (fxarithmetic-shift-left i2 12))))
                           (put-u8 p (fxbit-field x 16 24))
                           (lp (fx+ i 4))))
                        ((not strict?)
                         ;; Non-alphabet characters.
                         (let lp ((i i) (c* '()) (n 4))
                           (cond ((fxzero? n)
                                  ;; Found four valid characters.
                                  (lp* (cadddr c*) (caddr c*) (cadr c*) (car c*)
                                       (fx- i 4)))
                                 ((fx=? i len)
                                  (error 'base64-decode
                                         "Invalid input in non-strict mode."
                                         i c*))
                                 (else
                                  ;; Gather alphabetic (or valid
                                  ;; padding) characters.
                                  (let ((c (char->integer (string-ref str i))))
                                    (cond ((or (lookup c)
                                               (and (pad? c)
                                                    (fx<=? n 2)
                                                    (fx=? i (fx- len n))))
                                           (lp (fx+ i 1) (cons c c*) (fx- n 1)))
                                          (else
                                           (lp (fx+ i 1) c* n))))))))
                        (else
                         (error 'base64-decode
                                "Invalid input in strict mode."
                                c1 c2 c3 c4))))))
                 ((not strict-padding?)
                  ;; Append an appropriate amount of padding after the
                  ;; remaining characters.
                  (if (<= 2 (- len i) 3)
                      (lp-restart (string-append (substring str i (string-length str))
                                                 (if (= (- len i) 2) "==" "=")))
                      (error 'base64-decode "The input is too short." i)))
                 (else
                  (error 'base64-decode
                         "The input is too short, it may be missing padding."
                         i))))))))))

  (define (get-line-comp f port)
    (if (port-eof? port)
        (eof-object)
        (f (get-line port))))

  ;; Reads the common -----BEGIN/END type----- delimited format from
  ;; the given port. Returns two values: a string with the type and a
  ;; bytevector containing the base64 decoded data. The second value
  ;; is the eof object if there is an eof before the BEGIN delimiter.
  (define get-delimited-base64
    (case-lambda
      ((port)
       (get-delimited-base64 port #t))
      ((port strict)
       (define (get-first-data-line port)
         ;; Some MIME data has header fields in the same format as mail
         ;; or http. These are ignored.
         (let ((line (get-line-comp string-trim-both port)))
           (cond ((eof-object? line) line)
                 ((string-index line #\:)
                  (let lp ()               ;read until empty line
                    (let ((line (get-line-comp string-trim-both port)))
                      (if (string=? line "")
                          (get-line-comp string-trim-both port)
                          (lp)))))
                 (else line))))
       (let ((line (get-line-comp string-trim-both port)))
         (cond ((eof-object? line)
                (values "" (eof-object)))
               ((string=? line "")
                (get-delimited-base64 port))
               ((and (string-prefix? "-----BEGIN " line)
                     (string-suffix? "-----" line))
                (let* ((type (substring line 11 (- (string-length line) 5)))
                       (endline (string-append "-----END " type "-----")))
                  (let-values ([(outp extract) (open-bytevector-output-port)])
                    (let lp ((previous "") (line (get-first-data-line port)))
                      (cond ((eof-object? line)
                             (error 'get-delimited-base64
                                    "unexpected end of file"))
                            ((string-prefix? "-" line)
                             (unless (string=? line endline)
                               (error 'get-delimited-base64
                                      "bad end delimiter" type line))
                             (values type (extract)))
                            ((and (= (string-length line) 5)
                                  (string-prefix? "=" line))
                             ;; Skip Radix-64 checksum
                             (lp previous (get-line-comp string-trim-both port)))
                            ((not (fxzero? (fxmod (fx+ (string-length previous)
                                                       (string-length line))
                                                  4)))
                             ;; OpenSSH outputs lines with a bad length
                             (lp (string-append previous line)
                                 (get-line-comp string-trim-both port)))
                            (else
                             (base64-decode (string-append previous line) base64-alphabet outp)
                             (lp "" (get-line-comp string-trim-both port))))))))
               (else ;skip garbage (like in openssl x509 -in foo -text output).
                (get-delimited-base64 port)))))))

  (define put-delimited-base64
    (case-lambda
      ((port type bv line-length)
       (display (string-append "-----BEGIN " type "-----\n") port)
       (base64-encode bv 0 (bytevector-length bv)
                      line-length #f base64-alphabet port)
       (display (string-append "\n-----END " type "-----\n") port))
      ((port type bv)
       (put-delimited-base64 port type bv 76)))))
