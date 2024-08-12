;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2017, 2018, 2020 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; The MD5 Message-Digest Algorithm. RFC 1321

;; MD5 is not recommended for use in new designs!

(library (hashing md5)
  (export
    make-md5 md5-update! md5-finish! md5-clear!
    md5 md5-copy md5-finish
    md5-length
    md5-copy-hash! md5-96-copy-hash!
    md5->bytevector md5->string
    md5-hash=? md5-96-hash=?
    hmac-md5)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (hashing fixnums)
    (hashing private common))

  (define-fixnum-procedures f32 33)

  (define (md5-length) 16)

  (define (vector-copy x) (vector-map (lambda (i) i) x))

  (define (rol32 n count)
    (let ((inv-count (fx- 32 count)))
      (f32ior (f32arithmetic-shift-left (f32bit-field n 0 inv-count)
                                        count)
              (f32arithmetic-shift-right n inv-count))))

  (define-record-type md5state
    (nongenerative md5state-v1-bfcb430d-ffa5-4e57-85ad-c7763b3ce83d)
    (sealed #t)
    (fields (immutable H)               ;Hash
            (immutable W)               ;temporary data
            (immutable m)               ;unprocessed data
            (mutable pending)           ;length of unprocessed data
            (mutable processed)))       ;length of processed data

  (define (make-md5)
    (let ((H (list->vector initial-hash))
          (W (make-vector 16 #f))
          (m (make-bytevector (+ 8 (* 4 16)))))
      (make-md5state H W m 0 0)))

  (define (md5-copy state)
    (let ((H (vector-copy (md5state-H state)))
          (W (make-vector 16 #f))
          (m (bytevector-copy (md5state-m state))))
      (make-md5state H W m
                     (md5state-pending state)
                     (md5state-processed state))))

  (define (md5-clear! state)
    (for-each (lambda (i v)
                (vector-set! (md5state-H state) i v))
              '(0 1 2 3)
              initial-hash)
    (vector-fill! (md5state-W state) #f)
    (bytevector-fill! (md5state-m state) 0)
    (md5state-pending-set! state 0)
    (md5state-processed-set! state 0))

  (define initial-hash '(#x67452301 #xEFCDAB89 #x98BADCFE #x10325476))

  ;; (define g
  ;;   (list->vector
  ;;    (map (lambda (t)
  ;;           (cond ((<= 0 t 15)  t)
  ;;                 ((<= 16 t 31) (mod (+ (* 5 t) 1) 16))
  ;;                 ((<= 32 t 47) (mod (+ (* 3 t) 5) 16))
  ;;                 (else         (mod (* 7 t) 16))))
  ;;         (iota 64))))

  (define g '#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 1 6 11 0 5 10 15 4 9
               14 3 8 13 2 7 12 5 8 11 14 1 4 7 10 13 0 3 6 9 12 15 2 0 7
               14 5 12 3 10 1 8 15 6 13 4 11 2 9))

  (define r
    '#(7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
       5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
       4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
       6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21))

  (define k
    '#(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
       #xf57c0faf #x4787c62a #xa8304613 #xfd469501
       #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
       #x6b901122 #xfd987193 #xa679438e #x49b40821
       #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
       #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
       #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
       #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
       #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
       #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
       #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
       #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
       #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
       #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
       #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
       #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))

  ;; This function transforms a whole 512 bit block.
  (define (md5-transform! H W m offset)
    (define (f t x y z)
      (cond ((fx<=? 0 t 15)
             (f32ior (f32and x y)
                     (f32and (f32not x) z)))
            ((fx<=? 16 t 31)
             (f32ior (f32and x z)
                     (f32and (f32not z) y)))
            ((fx<=? 32 t 47)
             (f32xor x y z))
            (else
             (f32xor y (f32ior (f32not z) x)))))
    ;; Copy the message block
    (do ((t 0 (fx+ t 1)))
        ((eqv? t 16))
      (vector-set! W t (bytevector-u32-ref m (fx+ (fx* t 4) offset) (endianness little))))
    ;; Do the hokey pokey
    (let lp ((A (vector-ref H 0))
             (B (vector-ref H 1))
             (C (vector-ref H 2))
             (D (vector-ref H 3))
             (t 0))
      (cond ((eqv? t 64)
             (vector-set! H 0 (f32and #xffffffff (f32+ A (vector-ref H 0))))
             (vector-set! H 1 (f32and #xffffffff (f32+ B (vector-ref H 1))))
             (vector-set! H 2 (f32and #xffffffff (f32+ C (vector-ref H 2))))
             (vector-set! H 3 (f32and #xffffffff (f32+ D (vector-ref H 3)))))
            (else
             (lp D
                 (f32and #xffffffff
                         (f32+ B (rol32
                                  (f32and #xffffffff
                                          (f32+ (f32+ A (f t B C D))
                                                (f32+ (vector-ref W (vector-ref g t))
                                                      (vector-ref k t))))
                                  (vector-ref r t))))
                 B
                 C
                 (fx+ t 1))))))

  ;; Add a bytevector to the state. Align your data to whole blocks if
  ;; you want this to go a little faster.
  (define md5-update!
    (case-lambda
      ((state data start end)
       (let ((m (md5state-m state))    ;unprocessed data
             (H (md5state-H state))
             (W (md5state-W state)))
         (let lp ((offset start))
           (cond ((= (md5state-pending state) 64)
                  ;; A whole block is pending
                  (md5-transform! H W m 0)
                  (md5state-pending-set! state 0)
                  (md5state-processed-set! state (+ 64 (md5state-processed state)))
                  (lp offset))
                 ((= offset end)
                  (values))
                 ((or (> (md5state-pending state) 0)
                      (> (+ offset 64) end))
                  ;; Pending data exists or less than a block remains.
                  ;; Add more pending data.
                  (let ((added (min (- 64 (md5state-pending state))
                                    (- end offset))))
                    (bytevector-copy! data offset
                                      m (md5state-pending state)
                                      added)
                    (md5state-pending-set! state (+ added (md5state-pending state)))
                    (lp (+ offset added))))
                 (else
                  ;; Consume a whole block
                  (md5-transform! H W data offset)
                  (md5state-processed-set! state (+ 64 (md5state-processed state)))
                  (lp (+ offset 64)))))))
      ((state data)
       (md5-update! state data 0 (bytevector-length data)))))

  (define zero-block (make-bytevector 64 0))

  ;; Finish the state by adding a 1, zeros and the counter.
  (define (md5-finish! state)
    ;; TODO: the rfc has a prettier way to do this.
    (let ((m (md5state-m state))
          (pending (+ (md5state-pending state) 1)))
      (bytevector-u8-set! m (md5state-pending state) #x80)
      (cond ((> pending 56)
             (bytevector-copy! zero-block 0
                               m pending
                               (- 64 pending))
             (md5-transform! (md5state-H state)
                             (md5state-W state)
                             m
                             0)
             (bytevector-fill! m 0))
            (else
             (bytevector-copy! zero-block 0
                               m pending
                               (- 64 pending))))
      ;; Number of bits in the data
      (bytevector-u64-set! m 56
                           (* (+ (md5state-processed state)
                                 (- pending 1))
                              8)
                           (endianness little))
      (md5-transform! (md5state-H state)
                      (md5state-W state)
                      m
                      0)))

  (define (md5-finish state)
    (let ((copy (md5-copy state)))
      (md5-finish! copy)
      copy))

  ;; Find the MD5 of the concatenation of the given bytevectors.
  (define (md5 . data)
    (let ((state (make-md5)))
      (for-each (lambda (d) (md5-update! state d))
                data)
      (md5-finish! state)
      state))

  (define (copy-hash! state bv off len)
    (do ((i 0 (+ i 1)))
        ((= i len))
      (bytevector-u32-set! bv (+ off (* 4 i))
                           (vector-ref (md5state-H state) i)
                           (endianness little))))

  (define (md5-copy-hash! state bv off)
    (copy-hash! state bv off 4))

  (define (md5-96-copy-hash! state bv off)
    (copy-hash! state bv off 3))

  (define (md5->bytevector state)
    (let ((ret (make-bytevector (* 4 4))))
      (md5-copy-hash! state ret 0)
      ret))

  (define (md5->string state)
    (define hex "0123456789abcdef")
    (do ((ret (make-string 32))
         (H (md5state-H state))
         (i 0 (fx+ i 1)))
        ((eqv? i 32) ret)
      (let ((n (bitwise-and (bitwise-arithmetic-shift-right
                             (vector-ref H (fxarithmetic-shift-right i 3))
                             (fx* 4 (fxand (fxxor 1 i) #b111)))
                            #xf)))
        (string-set! ret i (string-ref hex n)))))

  ;; Compare an SHA-1 state with a bytevector. It is supposed to not
  ;; terminate early in order to not leak timing information. Assumes
  ;; that the bytevector's length is ok.
  (define (cmp state bv len)
    (do ((i 0 (fx+ i 1))
         (diff 0 (+ diff
                    (bitwise-xor
                     (bytevector-u32-ref bv (* 4 i) (endianness little))
                     (vector-ref (md5state-H state) i)))))
        ((fx=? i len)
         (zero? diff))))

  (define (md5-hash=? state bv) (cmp state bv 4))

  (define (md5-96-hash=? state bv) (cmp state bv 3))

  (define (hmac-md5 secret . data)
    ;; RFC 2104.
    (if (> (bytevector-length secret) 64)
        (apply hmac-md5 (md5->bytevector (md5 secret)) data)
        (let ((k-ipad (make-bytevector 64 0))
              (k-opad (make-bytevector 64 0)))
          (bytevector-copy! secret 0 k-ipad 0 (bytevector-length secret))
          (bytevector-copy! secret 0 k-opad 0 (bytevector-length secret))
          (do ((i 0 (fx+ i 1)))
              ((eqv? i 64))
            (bytevector-u8-set! k-ipad i (fxxor #x36 (bytevector-u8-ref k-ipad i)))
            (bytevector-u8-set! k-opad i (fxxor #x5c (bytevector-u8-ref k-opad i))))
          (let ((state (make-md5)))
            (md5-update! state k-ipad)
            (for-each (lambda (d) (md5-update! state d)) data)
            (md5-finish! state)
            (let ((digest (md5->bytevector state)))
              (md5-clear! state)
              (md5-update! state k-opad)
              (md5-update! state digest)
              (md5-finish! state)
              state))))))
