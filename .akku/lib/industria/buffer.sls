;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2022 Göran Weinholt <goran@weinholt.se>

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

;; Input buffering suitable for some binary network protocols.

;; This is not a port abstraction. If you e.g. want to implement a
;; protocol that uses both UDP and TCP, and you can normally make do
;; with a binary input port, then use open-bytevector-input-port when
;; using UDP.

;; This library is useful for when you have a protocol that uses
;; framing, but perhaps in an inconsistent way, and you need random
;; access inside whole frames. With this library you would not need to
;; append bytevectors and there might be less consing.

;; This library only does big-endian, since that is the normal network
;; byte order.

;; TODO: proper conditions

(library (industria buffer)
  (export make-buffer
          buffer-read! buffer-copy!
          buffer-port buffer-port-set!
          buffer-data buffer-data-set!
          buffer-top buffer-top-set!
          buffer-bottom buffer-bottom-set!
          buffer-length

          buffer-reset! buffer-seek! buffer-shorten!
          read-u8 read-u16 read-u24 read-u32)
  (import (rnrs)
          (struct pack))

  (define-record-type buffer
    (sealed #t)
    (opaque #t)
    (fields (mutable port)
            (mutable data)
            (mutable top)
            (mutable bottom))
    (protocol (lambda (p)
                (lambda (port)
                  ;; It's an interesting question what an optimal
                  ;; initial buffer size is. The overhead (length
                  ;; field) should probably be taken into account.
                  ;; Depends on implementation details...
                  (p port (make-bytevector (- 256 2)) 0 0)))))

  (define (grow! buf morelen)
    (define (extend have need)
      (if (> have need) have (extend (* 2 have) need)))
    (when (> (+ morelen (buffer-bottom buf))
             (bytevector-length (buffer-data buf)))
      ;; Extend the buffer size
      (let* ((old (buffer-data buf))
             (new (make-bytevector (extend (bytevector-length old)
                                           (+ morelen (buffer-bottom buf))))))
        (buffer-data-set! buf new)
        (bytevector-copy! old 0
                          new 0 (buffer-bottom buf)))))
  
  (define (buffer-read! buf n)
    (when (> n 0)
      (grow! buf n)
      (let ((bytes-read (get-bytevector-n! (buffer-port buf)
                                           (buffer-data buf)
                                           (buffer-bottom buf) n)))
        (if (or (eof-object? bytes-read)
                (< bytes-read n))
            (error 'buffer-read! "unexpected end of data" (buffer-port buf))
            (buffer-bottom-set! buf (+ (buffer-bottom buf) n))))))

  ;; Copy `n' bytes from source+source-start to the top of the buffer.
  (define (buffer-copy! source source-start buf n)
    (when (> n 0)
      (grow! buf n)
      (bytevector-copy! source source-start
                        (buffer-data buf) (buffer-bottom buf)
                        n)
      (buffer-bottom-set! buf (+ (buffer-bottom buf) n))))

  (define (buffer-reset! buf)
    (buffer-top-set! buf 0)
    (buffer-bottom-set! buf 0))

  (define (buffer-seek! buf offset)
    (when (> (+ (buffer-top buf) offset) (buffer-bottom buf))
      (error 'buffer-seek! "attempt to seek past bottom of buffer"))
    (buffer-top-set! buf (+ (buffer-top buf) offset)))
  
  (define (buffer-shorten! buf n)
    (when (> (buffer-top buf) (- (buffer-bottom buf) n))
      (error 'buffer-shorten! "attempt to chop off past buffer top"))
    (buffer-bottom-set! buf (- (buffer-bottom buf) n)))

  (define (buffer-length b)
    (- (buffer-bottom b) (buffer-top b)))

  (define (read-generic buf ref size index)
    (when (> (+ index (buffer-top buf) size) (buffer-bottom buf))
      (error 'read-generic "attempt to read past bottom of buffer" ref))
    (ref (buffer-data buf) (+ (buffer-top buf) index) (endianness big)))

  (define (read-u8 buf index)
    (when (> (+ index (buffer-top buf) 1) (buffer-bottom buf))
      (error 'read-u8 "attempt to read past bottom of buffer" index))
    (bytevector-u8-ref (buffer-data buf) (+ (buffer-top buf) index)))

  (define (read-u16 buf i)
    (read-generic buf bytevector-u16-ref 2 i))

  (define (read-u24 buf index)
    (when (> (+ index (buffer-top buf) 3) (buffer-bottom buf))
      (error 'read-u24 "attempt to read past bottom of buffer" index))
    (let ((data (buffer-data buf))
          (offset (+ (buffer-top buf) index)))
      (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-u8-ref data (+ offset 0)) 16)
                   (fxarithmetic-shift-left (bytevector-u8-ref data (+ offset 1)) 8)
                   (bytevector-u8-ref data (+ offset 2)))))

  (define (read-u32 buf i)
    (read-generic buf bytevector-u32-ref 4 i))
  
  
  )
