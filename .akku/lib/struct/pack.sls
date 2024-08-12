;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008, 2009, 2010, 2011, 2012, 2017, 2019 Göran Weinholt <goran@weinholt.se>
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

;;; Syntax for packing and unpacking C structs using bytevectors

(library (struct pack)
  (export
    format-size pack pack! unpack get-unpack put-pack)
  (import
    (rnrs (6)))

  (define (roundb offset alignment)
    (bitwise-and (+ offset (- alignment 1)) (- alignment)))

  (define-syntax unpack*
    (lambda (x)
      (define (type c)
        (case c
          ((#\c) (values #'(lambda (bv idx _e) (bytevector-s8-ref bv idx))
                         #'bytevector-s8-ref 1))
          ((#\C) (values #'(lambda (bv idx _e) (bytevector-u8-ref bv idx))
                         #'bytevector-u8-ref 1))
          ((#\s) (values #'bytevector-s16-ref
                         #'bytevector-s16-native-ref 2))
          ((#\S) (values #'bytevector-u16-ref
                         #'bytevector-u16-native-ref 2))
          ((#\l) (values #'bytevector-s32-ref
                         #'bytevector-s32-native-ref 4))
          ((#\L) (values #'bytevector-u32-ref
                         #'bytevector-u32-native-ref 4))
          ((#\q) (values #'bytevector-s64-ref
                         #'bytevector-s64-native-ref 8))
          ((#\Q) (values #'bytevector-u64-ref
                         #'bytevector-u64-native-ref 8))
          ((#\f) (values #'bytevector-ieee-single-ref
                         #'bytevector-ieee-single-native-ref 4))
          ((#\d) (values #'bytevector-ieee-double-ref
                         #'bytevector-ieee-double-native-ref 8))
          (else (syntax-violation
                 'unpack "Bad character in format string" x c))))
      (define (roundb offset alignment)
        (bitwise-and (+ offset (- alignment 1)) (- alignment)))
      (syntax-case x ()
        ((_ fmt bytevector)
         #'(unpack* fmt bytevector 0))
        ((_ fmt* bytevector base-offset*)
         (with-syntax
             ([(refs ...)
               (let ((fmt (syntax->datum #'fmt*)))
                 (let lp ((i 0)
                          (o 0)
                          (rep #f)
                          (endian #f)
                          (align #t)
                          (refs '()))
                   (cond
                     ((= i (string-length fmt))
                      (reverse refs))
                     ((char-whitespace? (string-ref fmt i))
                      (lp (+ i 1) o rep endian align refs))
                     (else
                      (case (string-ref fmt i)
                        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                         (lp (+ i 1) o
                             (+ (- (char->integer (string-ref fmt i))
                                   (char->integer #\0))
                                (* (if rep rep 0) 10))
                             endian align refs))
                        ((#\=)
                         (lp (+ i 1) o #f #f align refs))
                        ((#\<)
                         (lp (+ i 1) o #f #'(endianness little) align refs))
                        ((#\> #\!)
                         (lp (+ i 1) o #f #'(endianness big) align refs))
                        ((#\x)
                         (lp (+ i 1) (+ o (or rep 1)) #f endian align refs))
                        ((#\a)
                         (lp (+ i 1) o #f endian #t refs))
                        ((#\u)
                         (lp (+ i 1) o #f endian #f refs))
                        (else
                         (let-values ([(ref nref n) (type (string-ref fmt i))])
                           (let ((o (if align (roundb o n) o))
                                 (rep (or rep 1)))
                             (lp (+ i 1) (+ o (* n rep)) #f
                                 endian align
                                 (let lp* ((o o) (rep rep) (refs refs))
                                   (if (eqv? rep 0) refs
                                       (lp* (+ o n) (- rep 1)
                                            (cons
                                             (cond
                                               [endian
                                                #`(#,ref bv (fx+ base-offset #,o) #,endian)]
                                               [(or (not align)
                                                    (not
                                                     (and (integer? (syntax->datum #'base-offset*))
                                                          (eqv? 0 (mod (syntax->datum #'base-offset*)
                                                                       n)))))
                                                #`(#,ref bv (fx+ base-offset #,o) (native-endianness))]
                                               [else
                                                #`(#,nref bv (fx+ base-offset #,o))])
                                             refs)))))))))))))])
           #'(let ([bv bytevector]
                   [base-offset base-offset*])
               (values refs ...)))))))

  (define unpack**
    (case-lambda
      ((fmt bv offset)
       (define (type c)
         (case c
           ((#\c) (values (lambda (bv idx _e) (bytevector-s8-ref bv idx)) 1))
           ((#\C) (values (lambda (bv idx _e) (bytevector-u8-ref bv idx)) 1))
           ((#\s) (values bytevector-s16-ref 2))
           ((#\S) (values bytevector-u16-ref 2))
           ((#\l) (values bytevector-s32-ref 4))
           ((#\L) (values bytevector-u32-ref 4))
           ((#\q) (values bytevector-s64-ref 8))
           ((#\Q) (values bytevector-u64-ref 8))
           ((#\f) (values bytevector-ieee-single-ref 4))
           ((#\d) (values bytevector-ieee-double-ref 8))
           (else (error 'unpack "Bad character in format string" fmt c))))
       (let lp ((i 0)
                (o offset)
                (rep #f)
                (endian #f)
                (align #t)
                (refs '()))
         (cond ((fx=? i (string-length fmt))
                (apply values (reverse refs)))
               ((char-whitespace? (string-ref fmt i))
                (lp (fx+ i 1) o rep endian align refs))
               (else
                (case (string-ref fmt i)
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (lp (fx+ i 1) o
                       (fx+ (fx- (char->integer (string-ref fmt i))
                                 (char->integer #\0))
                            (fx* (if rep rep 0) 10))
                       endian align refs))
                  ((#\=)
                   (lp (fx+ i 1) o #f #f align refs))
                  ((#\<)
                   (lp (fx+ i 1) o #f (endianness little) align refs))
                  ((#\> #\!)
                   (lp (fx+ i 1) o #f (endianness big) align refs))
                  ((#\x)
                   (lp (fx+ i 1) (fx+ o (or rep 1)) #f endian align refs))
                  ((#\a)
                   (lp (fx+ i 1) o #f endian #t refs))
                  ((#\u)
                   (lp (fx+ i 1) o #f endian #f refs))
                  (else
                   (let-values (((ref n) (type (string-ref fmt i))))
                     (let ((o (if align (roundb o n) o))
                           (rep (or rep 1)))
                       (lp (fx+ i 1) (fx+ o (fx* n rep)) #f
                           endian align
                           (let lp* ((o o) (rep rep) (refs refs))
                             (if (eqv? rep 0) refs
                                 (lp* (fx+ o n) (fx- rep 1)
                                      (cons (cond ((eq? ref 's8)
                                                   (bytevector-s8-ref bv (fx+ offset o)))
                                                  ((eq? ref 'u8)
                                                   (bytevector-u8-ref bv (fx+ offset o)))
                                                  (endian
                                                   (ref bv (fx+ offset o) endian))
                                                  (else
                                                   (ref bv (fx+ offset o) (native-endianness))))
                                            refs)))))))))))))
      ((fmt bv)
       (unpack** fmt bv 0))))

  ;; Use the unpack* expander if possible, otherwise use the unpack**
  ;; function.
  (define-syntax unpack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt bytevector) #'(unpack fmt bytevector 0))
         ((_ fmt bytevector offset)
          (string? (syntax->datum #'fmt))
          #'(unpack* fmt bytevector offset))
         ((_ . rest) #'(unpack** . rest))
         (_ #'unpack**)))))

  ;; Find the number of bytes the format requires.
  ;; (format-size "2SQ") => 16
  (define (format-size* fmt)
    (define (size c)
      (case c
        ((#\x #\c #\C) 1)
        ((#\s #\S) 2)
        ((#\l #\L #\f) 4)
        ((#\q #\Q #\d) 8)
        (else
         (error 'format-size "Bad character in format string" fmt c))))
    (let lp ((i 0) (s 0) (rep #f) (align #t))
      (cond ((= i (string-length fmt))
             s)
            ((char<=? #\0 (string-ref fmt i) #\9)
             (lp (+ i 1) s
                 (+ (- (char->integer (string-ref fmt i))
                       (char->integer #\0))
                    (* (if rep rep 0) 10))
                 align))
            ((char-whitespace? (string-ref fmt i))
             (lp (+ i 1) s rep align))
            ((eqv? (string-ref fmt i) #\a)
             (lp (+ i 1) s rep #t))
            ((eqv? (string-ref fmt i) #\u)
             (lp (+ i 1) s rep #f))
            ((memv (string-ref fmt i) '(#\@ #\= #\< #\> #\!))
             (lp (+ i 1) s #f align))
            (else
             (let ((n (size (string-ref fmt i))))
               (lp (+ i 1) (+ (if align (roundb s n) s)
                              (if rep (* n rep) n))
                   #f align))))))

  (define-syntax format-size
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt-stx)
          (string? (syntax->datum #'fmt-stx))
          (let ()
            (define (roundb offset alignment)
              (bitwise-and (+ offset (- alignment 1)) (- alignment)))
            (define (format-size fmt)
              (define (size c)
                (case c
                  ((#\x #\c #\C) 1)
                  ((#\s #\S) 2)
                  ((#\l #\L #\f) 4)
                  ((#\q #\Q #\d) 8)
                  (else
                   (syntax-violation 'format-size
                                     "Bad character in format string" #'fmt-stx c))))
              (let lp ((i 0) (s 0) (rep #f) (align #t))
                (cond ((= i (string-length fmt))
                       s)
                      ((char<=? #\0 (string-ref fmt i) #\9)
                       (lp (+ i 1) s
                           (+ (- (char->integer (string-ref fmt i))
                                 (char->integer #\0))
                              (* (if rep rep 0) 10))
                           align))
                      ((char-whitespace? (string-ref fmt i))
                       (lp (+ i 1) s rep align))
                      ((eqv? (string-ref fmt i) #\a)
                       (lp (+ i 1) s rep #t))
                      ((eqv? (string-ref fmt i) #\u)
                       (lp (+ i 1) s rep #f))
                      ((memv (string-ref fmt i) '(#\@ #\= #\< #\> #\!))
                       (lp (+ i 1) s #f align))
                      (else
                       (let ((n (size (string-ref fmt i))))
                         (lp (+ i 1) (+ (if align (roundb s n) s)
                                        (if rep (* n rep) n))
                             #f align))))))
            (format-size (syntax->datum #'fmt-stx))))
         ((_ fmt)
          #'(format-size* fmt))))))

  (define (get-unpack** port fmt)
    (unpack fmt (get-bytevector-n port (format-size* fmt))))

  (define-syntax get-unpack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ port fmt)
          #'(unpack fmt (get-bytevector-n port (format-size fmt))))
         (var
          (identifier? #'var)
          #'get-unpack**)))))

  (define-syntax pack!*
    (lambda (x)
      (define (type c)
        (case c
          ((#\c) (values #'(lambda (bv idx v _e) (bytevector-s8-set! bv idx v))
                         #'bytevector-s8-set! 1))
          ((#\C) (values #'(lambda (bv idx v _e) (bytevector-u8-set! bv idx v))
                         #'bytevector-u8-set! 1))
          ((#\s) (values #'bytevector-s16-set! #'bytevector-s16-native-set! 2))
          ((#\S) (values #'bytevector-u16-set! #'bytevector-u16-native-set! 2))
          ((#\l) (values #'bytevector-s32-set! #'bytevector-s32-native-set! 4))
          ((#\L) (values #'bytevector-u32-set! #'bytevector-u32-native-set! 4))
          ((#\q) (values #'bytevector-s64-set! #'bytevector-s64-native-set! 8))
          ((#\Q) (values #'bytevector-u64-set! #'bytevector-u64-native-set! 8))
          ((#\f) (values #'bytevector-ieee-single-set! #'bytevector-ieee-single-native-set! 4))
          ((#\d) (values #'bytevector-ieee-double-set! #'bytevector-ieee-double-native-set! 8))
          (else (syntax-violation
                 'unpack "Bad character in format string" x c))))
      (define (zero-fill start len)
        ;; Return code which sets the bytes between start and end to
        ;; zero. This is important in order to not inadvertently leak
        ;; data through uninitialized buffers.
        (do ((i 0 (+ i 1))
             (e* '() (cons #`(bytevector-u8-set! bv (fx+ base-offset (fx+ #,start #,i)) 0)
                           e*)))
            ((= i len) e*)))
      (define (roundb offset alignment)
        (bitwise-and (+ offset (- alignment 1)) (- alignment)))
      (syntax-case x ()
        ((_ fmt* bytevector base-offset* vals ...)
         (with-syntax
             (((setters ...)
               (let ((fmt (syntax->datum #'fmt*)))
                 (let lp ((i 0)
                          (o 0)
                          (rep #f)
                          (endian #f)
                          (align #t)
                          (setters '())
                          (vals #'(vals ...)))
                   (cond
                     ((= i (string-length fmt))
                      (unless (null? (syntax->datum vals))
                        (syntax-violation #f "Too many values for the format" #'fmt*))
                      setters)
                     ((char-whitespace? (string-ref fmt i))
                      (lp (+ i 1) o rep endian align setters vals))
                     (else
                      (case (string-ref fmt i)
                        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                         (lp (+ i 1) o
                             (+ (- (char->integer (string-ref fmt i))
                                   (char->integer #\0))
                                (* (if rep rep 0) 10))
                             endian align setters vals))
                        ((#\=) (lp (+ i 1) o #f #f align setters vals))
                        ((#\<) (lp (+ i 1) o #f #'(endianness little) align setters vals))
                        ((#\> #\!) (lp (+ i 1) o #f #'(endianness big) align setters vals))
                        ((#\x)
                         (lp (+ i 1) (+ o (or rep 1)) #f endian align
                             (append (zero-fill o (or rep 1)) setters) vals))
                        ((#\a)
                         (lp (+ i 1) o #f endian #t setters vals))
                        ((#\u)
                         (lp (+ i 1) o #f endian #f setters vals))
                        (else           ;use the type table
                         (let-values ([(set nset n) (type (string-ref fmt i))])
                           (let ([rep (or rep 1)]
                                 [startoff (if align (roundb o n) o)])
                             (let lp* ((o^ startoff)
                                       (j 0)
                                       (setters (append (zero-fill o (- startoff o))
                                                        setters))
                                       (vals vals))
                               (cond
                                 ((= j rep)
                                  (lp (+ i 1) (+ startoff (* n rep)) #f endian align setters
                                      vals))
                                 (else
                                  (when (null? (syntax->datum vals))
                                    (syntax-violation #f "Too few values for the format" #'fmt*))
                                  (with-syntax ([(val1 vals ...) vals])
                                    (let ([setter
                                           (cond
                                             (endian
                                              #`(#,set bv (fx+ base-offset #,o^) val1 #,endian))
                                             ((or (not align)
                                                  (not
                                                   (and (integer? (syntax->datum #'base-offset*))
                                                        (eqv? 0 (mod (syntax->datum #'base-offset*) n)))))
                                              #`(#,set bv (fx+ base-offset #,o^) val1 (native-endianness)))
                                             (else
                                              #`(#,nset bv (fx+ base-offset #,o^) val1)))])
                                      (lp* (+ o^ n) (+ j 1) (cons setter setters) #'(vals ...)))))))))))))))))
           #'(let ((bv bytevector)
                   (base-offset base-offset*))
               setters ...
               (values)))))))

  (define (pack!** fmt bv offset . vals)
    (define (type c)
      (case c
        ((#\c) (values (lambda (bv idx v _e) (bytevector-s8-set! bv idx v)) 1))
        ((#\C) (values (lambda (bv idx v _e) (bytevector-u8-set! bv idx v)) 1))
        ((#\s) (values bytevector-s16-set! 2))
        ((#\S) (values bytevector-u16-set! 2))
        ((#\l) (values bytevector-s32-set! 4))
        ((#\L) (values bytevector-u32-set! 4))
        ((#\q) (values bytevector-s64-set! 8))
        ((#\Q) (values bytevector-u64-set! 8))
        ((#\f) (values bytevector-ieee-single-set! 4))
        ((#\d) (values bytevector-ieee-double-set! 8))
        (else (error 'pack! "Bad character in format string" fmt c))))
    (define (zero! i n)
      (do ((i i (fx+ i 1))
           (m (fx+ i n)))
          ((fx=? i m))
        (bytevector-u8-set! bv i 0)))
    (let lp ((i 0)
             (o 0)
             (rep #f)
             (endian (native-endianness))
             (align #t)
             (vals vals))
      (cond ((fx=? i (string-length fmt))
             (unless (null? vals)
               (error 'pack! "Too many values for the format" fmt)))
            ((char-whitespace? (string-ref fmt i))
             (lp (fx+ i 1) o rep endian align vals))
            (else
             (case (string-ref fmt i)
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (lp (fx+ i 1) o
                    (fx+ (fx- (char->integer (string-ref fmt i))
                              (char->integer #\0))
                       (fx* (if rep rep 0) 10))
                    endian align vals))
               ((#\=) (lp (fx+ i 1) o #f (native-endianness) align vals))
               ((#\<) (lp (fx+ i 1) o #f (endianness little) align vals))
               ((#\> #\!) (lp (fx+ i 1) o #f (endianness big) align vals))
               ((#\x)
                (zero! o (or rep 1))
                (lp (fx+ i 1) (fx+ o (or rep 1)) #f endian align vals))
               ((#\a)
                (lp (fx+ i 1) o #f endian #t vals))
               ((#\u)
                (lp (fx+ i 1) o #f endian #f vals))
               (else
                (let*-values (((set n) (type (string-ref fmt i)))
                              ((o*) (if align (roundb o n) o)))
                  (zero! o (fx- o* o))
                  (do ((rep (or rep 1) (fx- rep 1))
                       (o o* (+ o n))
                       (vals vals (cdr vals)))
                      ((eqv? rep 0)
                       (lp (fx+ i 1) (fx+ o (fx* n rep)) #f endian align vals))
                    (when (null? vals)
                      (error 'pack! "Too few values for the format" fmt))
                    (set bv (fx+ offset o) (car vals) endian)))))))))

  (define-syntax pack!
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt bv offset vals ...)
          (string? (syntax->datum #'fmt))
          #'(pack!* fmt bv offset vals ...))
         ((_ . rest) #'(pack!** . rest))
         (var
          (identifier? #'var)
          #'pack!**)))))

  (define (pack** fmt . values)
    (let ((bv (make-bytevector (format-size fmt))))
      (apply pack! fmt bv 0 values)
      bv))

  (define-syntax pack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt vals ...)
          #'(let ((bv (make-bytevector (format-size fmt))))
              (pack! fmt bv 0 vals ...)
              bv))
         (var
          (identifier? #'var)
          #'pack**)))))

  (define (put-pack** port fmt . vals)
    (put-bytevector port (apply pack fmt vals)))

  (define-syntax put-pack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ port fmt vals ...)
          #'(put-bytevector port (pack fmt vals ...)))
         ((_ . rest)
          #'(put-pack** . rest))
         (var
          (identifier? #'var)
          #'put-pack**))))))
