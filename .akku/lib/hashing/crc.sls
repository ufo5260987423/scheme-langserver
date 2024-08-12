;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2011, 2012, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

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

;; Syntax for defining procedures that calculate Cyclic Redundancy Codes.

;; Ross N. Williams, "A painless guide to CRC error detection
;; algorithms". http://www.ross.net/crc/crcpaper.html

;;; Simple usage with pre-defined CRCs

;; If you want to use one of the pre-defined CRCs

;; (define-crc crc-32)
;;     calculates the CRC table at expand-time and defines the
;;     procedures below

;; (crc-32 bytevector)
;;     returns the final CRC of the entire bytevector
;; (crc-32-init)
;;     returns an initial CRC state
;; (crc-32-update state bv)
;; (crc-32-update state bv start)
;; (crc-32-update state bv start end)
;;     returns a new state which includes the CRC on the given bytes
;; (crc-32-finish state)
;;     returns the final CRC
;; (crc-32-width)
;;     returns the bit-width of the CRC, e.g. 32 for CRC-32
;; (crc-32-self-test)
;;     returns 'sucess, 'failure, or 'no-self-test

;;; Advanced usage

;; Quick and possibly confusing guide to using define-crc with new
;; CRCs, for those who are too busy to read the above paper:

;; Syntax: (define-crc name width polynomial init ref-in ref-out
;;                     xor-out check)

;; Syntax:
;; (define-crc <name>
;;   (width <width>)
;;   (polynomial <polynomial>)
;;   (init <init>)
;;   (ref-in <ref-in>)
;;   (ref-out <ref-out>)
;;   (xor-out <xor-out>)
;;   (check <check>))

;; The width is the bitwise length of the polynomial. You might be
;; lead to believe that it should sometimes be 33, but if so you've
;; been counting the highest bit, which doesn't count.

;; The polynomial for CRC-16 is given sometimes given as x^16 + x^15 +
;; x^2 + 1. This translates to #b1000000000000101 (#x8005). Note that
;; x^16 is absent. CRCs use polynomial division with modulo two
;; arithmetic (better known as XOR). Don't use the reversed polynomial
;; if you have one of those, instead set ref-in and ref-out properly.

;; After a CRC has been calculated it is sometimes XOR'd with a final
;; value, this is xor-out.

;; check is either the CRC of the ASCII string "123456789", or #f.

;; Syntax: (define-crc name (coefficients ...) init ref-in ref-out
;;                     xor-out check)

;; Syntax:
;; (define-crc <name>
;;   (polynomial (coefficients ...))
;;   (init <init>)
;;   (ref-in <ref-in>)
;;   (ref-out <ref-out>)
;;   (xor-out <xor-out>)
;;   (check <check>))

;; This is the slightly easier interface where you can simply specify
;; the powers of the coefficients. CRC-16 in this syntax becomes:

;; (define-crc crc-16 (16 15 2 0) #x0000 #t #t #x0000 #xBB3D)

;; Another example: the polynomial x^8 + x^2 + x + 1 in this syntax
;; becomes: (8 2 1 0). Note how the last "1" is 0.

;;; Bit-oriented CRCs

;; Use define-bitwise-crc when working with bytevectors where each
;; byte represents a bit, e.g. #vu8(0 1 1 1).

(library (hashing crc)
  (export
    define-crc
    define-bit-oriented-crc
    width polynomial init ref-in ref-out xor-out check)
  (import
    (except (rnrs (6)) bitwise-rotate-bit-field bitwise-reverse-bit-field)
    (hashing fixnums)
    (for (hashing private common) expand)
    (hashing private compat))

;; This makes the auxiliary keywords work in the Chez repl.
(define-syntax define-auxiliary-keyword*
  (lambda (x)
    (syntax-case x ()
      ((_ keyword)
       #'(define-syntax keyword
           (lambda (x)
             (syntax-violation #f "incorrect usage of auxiliary keyword" x))))
      ((_ keyword0 keyword* ...)
       #'(begin
           (define-auxiliary-keyword* keyword0)
           (define-auxiliary-keyword* keyword* ...))))))

(define-auxiliary-keyword* width polynomial init ref-in ref-out xor-out check)

(define (string->bits str reverse-bytes)
  ;; Horrible code that's only used for the bit-oriented test vectors.
  (u8-list->bytevector
   (apply append
          (map (lambda (x)
                 (let* ((s (number->string x 2))
                        (s (string-append (make-string (- 8 (string-length s)) #\0)
                                          s))
                        (l (map (lambda (x) (if (char=? x #\1) 1 0))
                                (string->list s))))
                   (if reverse-bytes (reverse l) l)))
               (bytevector->u8-list (string->utf8 str))))))

;; Bit-oriented.

(define-syntax define-bit-oriented-crc
  (lambda (x)
    (syntax-case x (width polynomial init ref-in ref-out xor-out check)
      [(_ name
          (polynomial (width^ coeff* ...))
          (init init^)
          (ref-in ref-in^)
          (ref-out ref-out^)
          (xor-out xor-out^)
          (check check^))
       #'(define-bit-oriented-crc name
           (width^ coeff* ...) init^ ref-in^ ref-out^ xor-out^ check^)]

      [(_ name
          (width width^)
          (polynomial poly^)
          (init init^)
          (ref-in ref-in^)
          (ref-out ref-out^)
          (xor-out xor-out^)
          (check check^))
       #'(define-bit-oriented-crc name
           width^ poly^ init^ ref-in^ ref-out^ xor-out^ check^)]

      ((_ name (width-value coeff* ...) . rest)
       (with-syntax ((polynomial (decode-coefficients (syntax->datum #'(coeff* ...)))))
         #'(define-bit-oriented-crc name
             width-value polynomial . rest)))

      [(_ name width^ poly^ init^ ref-in^ ref-out^ xor-out^ check^)
       (with-syntax ((crc-init (symcat #'name "-init"))
                     (crc-finish (symcat #'name "-finish"))
                     (crc-update (symcat #'name "-update"))
                     (crc-self-test (symcat #'name "-self-test"))
                     (crc-width (symcat #'name "-width")))
         (unless (and (syntax->datum #'ref-in^) (syntax->datum #'ref-out^))
           ;; TODO: implement everything else
           (syntax-violation x "ref-in=#f is unimplemented" #'ref-in^))
         #`(begin
             (define (name bv)
               (crc-finish (crc-update (crc-init) bv)))
             (define (crc-init) init^)
             (define (crc-finish r) (bitwise-xor r xor-out^))
             (define (crc-self-test)
               (if check^
                   (let ((check-value
                          (if ref-out^ (bitwise-reverse-bit-field check^ 0 width^))))
                     (if (= (name (string->bits "123456789" ref-in^))
                            check-value)
                         'success 'failure))
                   'no-self-test))
             (define (crc-width) width^)
             (define crc-update
               (case-lambda
                 ((r* bv)
                  (crc-update r* bv 0 (bytevector-length bv)))
                 ((r* bv start)
                  (crc-update r* bv start (bytevector-length bv)))
                 ((r* bv start end)
                  (define-fixnum-procedures fw width^)
                  (define mask (- (bitwise-arithmetic-shift-left 1 (- width^ 1)) 1))
                  (do ((i start (fx+ i 1))
                       (r r*
                          (let ((inv (fwxor (bytevector-u8-ref bv i)
                                            (fwarithmetic-shift-right r (- width^ 1)))))
                            ;; XX: slowly works with one bit at a time.
                            (fwior (fwarithmetic-shift-left
                                    (fwxor (fwand r mask)
                                           (fwand (fwarithmetic-shift-right poly^ 1)
                                                  (fw- inv)))
                                    1)
                                   inv))))
                      ((fx=? i end) r)))))))])))

;; Byte-oriented.

(define-syntax define-crc
  (lambda (x)
    (define (calc-table index width ref-in poly)
      (if ref-in
          (bitwise-reverse-bit-field (calc-table (bitwise-reverse-bit-field index 0 8)
                                                 width #f poly)
                                     0 width)
          (do ((bit 0 (+ bit 1))
               (r (bitwise-arithmetic-shift-left index (- width 8))
                  (if (bitwise-bit-set? r (- width 1))
                      (bitwise-xor (bitwise-arithmetic-shift-left r 1) poly)
                      (bitwise-arithmetic-shift-left r 1))))
              ((= bit 8)
               (bitwise-bit-field r 0 width)))))

    (syntax-case x (width polynomial init ref-in ref-out xor-out check)
      [(_ name)
       ;; Contributions are welcome. There should also be more
       ;; references here. A lot of work went into finding these
       ;; polynomials, and they are reduced to one-liners.
       (case (syntax->datum #'name)
         ;; Used for .ZIP, AUTODIN II, Ethernet, FDDI, PNG, MPEG-2
         ;; and various other things.
         ((crc-32)
          #'(define-crc name 32 #x04C11DB7 #xFFFFFFFF #t #t #xFFFFFFFF #xCBF43926))
         ((crc-16)
          #'(define-crc name 16 #x8005 #x0000 #t #t #x0000 #xBB3D))
         ((crc-16/ccitt)
          ;; Used by XMODEM, PPP and much more
          #'(define-crc name 16 #x1021 #xffff #f #f 0 #x29B1))
         ((crc-32c)
          ;; CRC-32C specified in e.g. RFC4960 or RFC3385. Used by SCTP
          ;; and iSCSI. Finds more errors than CRC-32.
          #'(define-crc name 32 #x1EDC6F41 #xFFFFFFFF #t #t #xFFFFFFFF #xE3069283))
         ;; OpenPGP, see RFC2440.
         ((crc-24)
          #'(define-crc name (24 23 18 17 14 11 10 7 6 5 4 3 1 0)
                        #xB704CE #f #f 0 #x21CF02))
         ((crc-64)
          #'(define-crc name (64 4 3 1 0) 0 #t #t 0 #x46A5A9388A5BEFFE))
         ((crc-64/ecma-182)
          ;; Used by XZ
          #'(define-crc name
              (64 62 57 55 54 53 52 47 46 45 40 39 38 37 35 33 32 31
                  29 27 24 23 22 21 19 17 13 12 10 9 7 4 1 0)
              #xFFFFFFFFFFFFFFFF #t #t #xFFFFFFFFFFFFFFFF
              #x995DC9BBDF1939FA))
         (else
          (syntax-violation #f "this CRC is not pre-defined" #'name)))]

      [(_ name
          (polynomial (width^ coeff* ...))
          (init init^)
          (ref-in ref-in^)
          (ref-out ref-out^)
          (xor-out xor-out^)
          (check check^))
       #'(define-crc name
           (width^ coeff* ...) init^ ref-in^ ref-out^ xor-out^ check^)]

      [(_ name
          (width width^)
          (polynomial poly^)
          (init init^)
          (ref-in ref-in^)
          (ref-out ref-out^)
          (xor-out xor-out^)
          (check check^))
       #'(define-crc name
           width^ poly^ init^ ref-in^ ref-out^ xor-out^ check^)]

      [(_ name (width^ coeffs ...) . rest)
       (with-syntax ((polynomial (decode-coefficients (syntax->datum #'(coeffs ...)))))
         #'(define-crc name width^ polynomial . rest))]

      [(_ name width^ polynomial^ init^ ref-in^ ref-out^ xor-out^ check^)
       (and (identifier? #'name) (>= (syntax->datum #'width^) 8)
            (zero? (mod (syntax->datum #'width^) 8)))
       ;; TODO: test different widths.
       (let* ((width* (syntax->datum #'width^))
              (polynomial* (syntax->datum #'polynomial^))
              (init* (syntax->datum #'init^))
              (ref-in* (syntax->datum #'ref-in^))
              (ref-out* (syntax->datum #'ref-out^)))
         (unless (boolean=? ref-in* ref-out*)
           ;; TODO: implement the other ref-in ref-out combinations?
           (syntax-violation x "Mixed in/out reflection is unimplemented" ref-in* ref-out*))
         (with-syntax ((init (if ref-in*
                                 (bitwise-reverse-bit-field init* 0 width*)
                                 init*))
                       (table (list->vector
                               (map (lambda (i)
                                      (calc-table i width* ref-in* polynomial*))
                                    (iota 256))))
                       (crc-init (symcat #'name "-init"))
                       (crc-finish (symcat #'name "-finish"))
                       (crc-update (symcat #'name "-update"))
                       (crc-self-test (symcat #'name "-self-test"))
                       (crc-width (symcat #'name "-width")))
           #`(begin
               (define (name bv)
                 (crc-finish (crc-update (crc-init) bv)))
               (define (crc-init) init)
               (define (crc-finish r) (bitwise-xor r xor-out^))
               (define (crc-self-test)
                 (if check^
                     (if (= (name (string->utf8 "123456789")) check^)
                         'success 'failure)
                     'no-self-test))
               (define (crc-width) width^)
               (define crc-update
                 (case-lambda
                   ((r* bv)
                    (crc-update r* bv 0 (bytevector-length bv)))
                   ((r* bv start)
                    (crc-update r* bv start (bytevector-length bv)))
                   ((r* bv start end)
                    (define-fixnum-procedures fw width^)
                    (define mask (- (bitwise-arithmetic-shift-left 1 (- width^ 8)) 1))
                    (define t 'table)
                    (do ((i start (fx+ i 1))
                         (r r*
                            (if (and ref-in^ ref-out^)
                                (fwxor (fwarithmetic-shift-right r 8)
                                       (vector-ref t (fwxor (fwand #xff r)
                                                            (bytevector-u8-ref bv i))))
                                (fwxor (fwarithmetic-shift-left (fwand mask r) 8)
                                       (vector-ref t (fwxor
                                                      (bytevector-u8-ref bv i)
                                                      (fwand
                                                       (fwarithmetic-shift-right r (- width^ 8))
                                                       #xff)))))))
                        ((fx=? i end) r))))))))]))))
