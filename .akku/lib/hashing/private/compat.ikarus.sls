;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Compatibility for Ikarus 0.0.4-rc1+.

(library (hashing private compat)
  (export bitwise-rotate-bit-field bitwise-reverse-bit-field)
  (import (except (rnrs) bitwise-rotate-bit-field bitwise-reverse-bit-field))

(define (bitwise-rotate-bit-field n start end count)
  ;; Straight out of r6rs-lib.
  (assert (<= 0 start end))
  (assert (not (negative? count)))
  (let ((width (- end start)))
    (if (positive? width)
        (let* ((count (mod count width))
               (field0 (bitwise-bit-field n start end))
               (field1 (bitwise-arithmetic-shift-left field0 count))
               (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
               (field (bitwise-ior field1 field2)))
          (bitwise-copy-bit-field n start end field))
        n)))

(define (bitwise-reverse-bit-field v start end)
  (do ((i start (+ i 1))
       (ret 0 (if (bitwise-bit-set? v i)
                  (bitwise-ior ret (bitwise-arithmetic-shift-left 1 (- end i 1)))
                  ret)))
      ((= i end)
       (bitwise-ior (bitwise-arithmetic-shift-left ret start)
                    (bitwise-copy-bit-field v start end 0))))))
