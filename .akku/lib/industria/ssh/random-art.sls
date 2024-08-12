;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012, 2018, 2019 Göran Weinholt <goran@weinholt.se>

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

;; Random art for cryptographic key hashes, compatible with OpenSSH

(library (industria ssh random-art)
  (export random-art random-art-box-style
          random-art-style-ascii random-art-style-ascii-2
          random-art-style-unicode)
  (import (rnrs)
          (only (srfi :13 strings) string-pad string-pad-right)
          ;; (srfi :25 multi-dimensional-arrays)
          (srfi :39 parameters)
          (industria crypto dsa)
          (industria crypto rsa))

  ;; The algorithm is the same as in OpenSSH's random art generator.
  ;; It creates a 2D array, puts a marker in the middle and then
  ;; iterates over the bits in the key, moving the marker in different
  ;; directions based on the bits of the digest.

  ;; OpenSSH's random art uses MD5 digests, and MD5's shorter digest
  ;; length also makes the art less dense, which seems to give nicer
  ;; art.

  (define ylen 9)
  (define xlen 17)
  (define chars " .o+=*BOX@%&#/^SE")

  (define random-art-style-ascii
    '#(#\+ #\- "[" "]" "+\n"
       #\|             "|\n"
       #\+ #\-         "+\n"))

  (define random-art-style-ascii-2
    '#(#\, #\- "[" "]" ".\n"
       #\|             "|\n"
       #\` #\-         "'\n"))

  (define random-art-style-unicode
    '#(#\x256D #\x2500 "\x2524;" "\x251C;" "\x256E;\n"
       #\x2502                             "\x2502;\n"
       #\x2570 #\x2500                     "\x256F;\n"))

  ;; '#(#\╭ #\─ "┤" "├" "╮\n"
  ;;    #\│             "│\n"
  ;;    #\╰ #\─         "╯\n")

  (define random-art-box-style (make-parameter random-art-style-ascii))

  (define random-art
    (case-lambda
      ((digest header footer)
       (random-art digest header footer xlen ylen chars))
      ((digest header footer xlen ylen chars)
       ;; Fill-in for SRFI-25
       (define (array-index x y) (fx+ (fx* y xlen) x))
       (define (array-set! f x y v) (vector-set! f (array-index x y) v))
       (define (array-ref f x y) (vector-ref f (array-index x y)))
       (define make-array make-vector)
       (define (shape x0 x1 y0 y1) (assert (and (zero? x0) (zero? y0))) (* x1 y1))
       (define (perambulate! field x y b)
         (let ((x (min (- xlen 1) (max 0 (+ x (if (fxbit-set? b 0) 1 -1)))))
               (y (min (- ylen 1) (max 0 (+ y (if (fxbit-set? b 1) 1 -1))))))
           (array-set! field x y (+ (array-ref field x y) 1))
           (values x y (fxarithmetic-shift-right b 2))))
       (let ((field (make-array (shape 0 xlen 0 ylen) 0)))
         ;; Fill in the field
         (let lp ((i 0)
                  (x0 (div xlen 2))
                  (y0 (div ylen 2)))
           (cond ((= i (bytevector-length digest))
                  (array-set! field (div xlen 2) (div ylen 2)
                              (- (string-length chars) 2))
                  (array-set! field x0 y0 (- (string-length chars) 1)))
                 (else
                  (let ((b0 (bytevector-u8-ref digest i)))
                    (let*-values (((x1 y1 b1) (perambulate! field x0 y0 b0))
                                  ((x2 y2 b2) (perambulate! field x1 y1 b1))
                                  ((x3 y3 b3) (perambulate! field x2 y2 b2))
                                  ((x4 y4 b4) (perambulate! field x3 y3 b3)))
                      (lp (+ i 1) x4 y4))))))
         ;; Draw the field
         (call-with-string-output-port
           (lambda (p)
             (let ((box (random-art-box-style)))
               (define (centered-line text padding-char)
                 (let* ((header (string-append (vector-ref box 2)
                                               text
                                               (vector-ref box 3)))
                        (line (make-string (div (- xlen (string-length header)) 2)
                                           padding-char)))
                   (string-pad-right (string-append line header line) xlen padding-char)))
               (display (vector-ref box 0) p)
               (display (centered-line header (vector-ref box 1)) p)
               (display (vector-ref box 4) p)
               (do ((y 0 (+ y 1)))
                   ((= y ylen))
                 (display (vector-ref box 5) p)
                 (do ((x 0 (+ x 1)))
                     ((= x xlen))
                   (display (string-ref chars (min (array-ref field x y)
                                                   (- (string-length chars) 1)))
                            p))
                 (display (vector-ref box 6) p))
               (display (vector-ref box 7) p)
               (display (centered-line footer (vector-ref box 8)) p)
               (display (vector-ref box 9) p)))))))))
