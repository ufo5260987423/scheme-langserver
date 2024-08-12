;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2012, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; RFC 2104, FIPS-198-1.

(library (hashing hmac)
  (export make-hmac)
  (import (rnrs))

  ;; Returns a procedure that calculates the HMAC given a secret and
  ;; data (both of which are bytevectors).
  (define (make-hmac block-length hash ->bytevector make-hash update! finish! clear!)
    (lambda (secret . data)
      (let lp ((secret secret))
        (if (> (bytevector-length secret) block-length)
            (lp (->bytevector (hash secret)))
            (let ((k-ipad (make-bytevector block-length 0))
                  (k-opad (make-bytevector block-length 0)))
              (bytevector-copy! secret 0 k-ipad 0 (bytevector-length secret))
              (bytevector-copy! secret 0 k-opad 0 (bytevector-length secret))
              (do ((i 0 (fx+ i 1)))
                  ((fx=? i block-length))
                (bytevector-u8-set! k-ipad i (fxxor #x36 (bytevector-u8-ref k-ipad i)))
                (bytevector-u8-set! k-opad i (fxxor #x5c (bytevector-u8-ref k-opad i))))
              (let ((state (make-hash)))
                (update! state k-ipad)
                (for-each (lambda (d) (update! state d)) data)
                (finish! state)
                (let ((digest (->bytevector state)))
                  (clear! state)
                  (update! state k-opad)
                  (update! state digest)
                  (finish! state)
                  state))))))))
