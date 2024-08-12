;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2013 Göran Weinholt <goran@weinholt.se>

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

;; Entropic helpers.

;; Everything that uses entropy in Industria must get it here.

;; TODO: procedures for estimating entropy.

;; TODO: support for EGD?

;; TODO: Should probably not use srfi-27 but something that works the
;; same everywhere.

(library (industria crypto entropy)
  (export make-random-bytevector
          bytevector-randomize!
          random-positive-byte
          string-random-case
          (rename (rand random-integer)))
  (import (rnrs)
          (only (srfi :13 strings) string-map)
          (srfi :26 cut)
          (srfi :27 random-bits))

  (define make-random-bytevector
    (lambda (n)
      (let ((bv (make-bytevector n)))
        (bytevector-randomize! bv)
        bv)))

  (define /dev/urandom
    (and (file-exists? "/dev/urandom")
         (open-file-input-port "/dev/urandom"
                               (file-options)
                               (buffer-mode none))))

  ;; The same interface as bytevector-copy! except with no source
  ;; arguments.
  (define bytevector-randomize!
    (if /dev/urandom
        (case-lambda
          ((bv) (bytevector-randomize! bv 0 (bytevector-length bv)))
          ((bv start) (bytevector-randomize! bv start (bytevector-length bv)))
          ((bv start count)
           (let lp ((start start)
                    (count count))
             (unless (zero? count)
               (let ((n (get-bytevector-n! /dev/urandom bv start count)))
                 (lp (+ start n) (- count n)))))))
        (let* ((s (make-random-source))
               (make-int (random-source-make-integers s)))
          (case-lambda
            ((bv) (bytevector-randomize! bv 0 (bytevector-length bv)))
            ((bv start) (bytevector-randomize! bv start (bytevector-length bv)))
            ((bv start count)
             (random-source-randomize! s)
             (do ((start start (+ start 1))
                  (count count (- count 1)))
                 ((zero? count))
               (bytevector-u8-set! bv start (make-int 255))))))))

  (define random-positive-byte
    (if /dev/urandom
        (lambda ()
          (let lp ()
            (let ((v (get-u8 /dev/urandom)))
              (if (zero? v) (lp) v))))
        (let* ((s (make-random-source))
               (make-int (random-source-make-integers s)))
          (random-source-randomize! s)
          (lambda () (+ 1 (make-int 254))))))
  
  (define rand
    (let ((s (make-random-source)))
      (random-source-randomize! s)
      (random-source-make-integers s)))

  (define (random-boolean) (zero? (rand 2)))
  
  (define (string-random-case name)
    (string-map (cut (if (random-boolean) char-upcase char-downcase) <>)
                name))


  )
