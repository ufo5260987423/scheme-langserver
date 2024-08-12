;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2020 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; xxHash - fast digests, not for cryptography

(library (hashing xxhash)
  (export
    make-xxh32 xxh32-update! xxh32-finish!
    xxh32 xxh32-copy xxh32-finish
    xxh32-length
    xxh32->string xxh32->integer)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (hashing fixnums)
    (hashing private common))

(define PRIME32-1 2654435761)
(define PRIME32-2 2246822519)
(define PRIME32-3 3266489917)
(define PRIME32-4 668265263)
(define PRIME32-5 374761393)

(define PRIME64_1 11400714785074694791)
(define PRIME64_2 14029467366897019727)
(define PRIME64_3 1609587929392839161)
(define PRIME64_4 9650029242287828579)
(define PRIME64_5 2870177450012600261)

(define STRIPE-32 16)
(define STRIPE-64 32)

(define-fixnum-procedures f32 33)

(define (xxh32-length) 4)

(define (xxh64-length) 8)

(define (rol32 n count)
  (let ((inv-count (fx- 32 count)))
    (f32ior (f32arithmetic-shift-left (f32bit-field n 0 inv-count)
                                      count)
            (f32arithmetic-shift-right n inv-count))))

(define (umul32c n const)
  ;; TODO: multiply without making bignums
  (let ((tmp (* n const)))
    (bitwise-and #xffffffff tmp)))

(define-record-type xxh32state
  (nongenerative xxh32state-v0-65a0260f-0e07-4dad-a8dc-5e5ad07fa92c)
  (sealed #t)
  (fields (mutable acc)                 ;accumulator/result
          (mutable acc1)                ;accumulators
          (mutable acc2)
          (mutable acc3)
          (mutable acc4)
          (immutable m)                 ;unprocessed data
          (mutable pending)             ;length of unprocessed data
          (mutable processed)))         ;length of processed data

(define make-xxh32
  (case-lambda
    (()
     (make-xxh32 0))
    ((seed)
     (let ((acc (f32and #xffffffff (f32+ seed PRIME32-5)))
           (acc1 (f32and #xffffffff (f32+ seed (+ PRIME32-1 PRIME32-2))))
           (acc2 (f32and #xffffffff (f32+ seed PRIME32-2)))
           (acc3 (f32and #xffffffff seed))
           (acc4 (f32and #xffffffff (f32- seed PRIME32-1)))
           (m (make-bytevector STRIPE-32 0)))
       (make-xxh32state acc acc1 acc2 acc3 acc4 m 0 0)))))

(define (xxh32-copy state)
  (let ((m (bytevector-copy (xxh32state-m state))))
    (make-xxh32state (xxh32state-acc state)
                     (xxh32state-acc1 state)
                     (xxh32state-acc2 state)
                     (xxh32state-acc3 state)
                     (xxh32state-acc4 state)
                     m
                     (xxh32state-pending state)
                     (xxh32state-processed state))))

(define bytevector-u32-le-ref
  (lambda (bv idx)
    (bytevector-u32-ref bv idx (endianness little))))

(define (xxh32-transform acc1 acc2 acc3 acc4 m offset)
  (let ((lane1 (bytevector-u32-le-ref m offset))
        (lane2 (bytevector-u32-le-ref m (fx+ offset 4)))
        (lane3 (bytevector-u32-le-ref m (fx+ offset 8)))
        (lane4 (bytevector-u32-le-ref m (fx+ offset 12))))
    (let ((acc1 (umul32c (rol32
                          (bitwise-and #xffffffff
                                      (f32+ acc1 (umul32c lane1 PRIME32-2)))
                         13)
                         PRIME32-1))
          (acc2 (umul32c (rol32 (bitwise-and #xffffffff
                                             (f32+ acc2 (umul32c lane2 PRIME32-2)))
                                13)
                         PRIME32-1))
          (acc3 (umul32c (rol32 (bitwise-and #xffffffff (f32+ acc3 (umul32c lane3 PRIME32-2)))
                                13)
                         PRIME32-1))
          (acc4 (umul32c (rol32 (bitwise-and #xffffffff (f32+ acc4 (umul32c lane4 PRIME32-2)))
                                13)
                        PRIME32-1)))
      (values acc1 acc2 acc3 acc4))))

;; Add a bytevector to the state. Align your data to whole stripes if
;; you want this to go a little faster.
(define xxh32-update!
  (case-lambda
    ((state data)
     (xxh32-update! state data 0 (bytevector-length data)))
    ((state data start end)
     (let ((m (xxh32state-m state)))    ;pending data
       (let lp ((offset start)
                (acc1 (xxh32state-acc1 state))
                (acc2 (xxh32state-acc2 state))
                (acc3 (xxh32state-acc3 state))
                (acc4 (xxh32state-acc4 state)))
         (cond ((eqv? (xxh32state-pending state) STRIPE-32)
                ;; A whole stripe is pending
                (let-values (((acc1 acc2 acc3 acc4)
                              (xxh32-transform acc1 acc2 acc3 acc4 m 0)))
                  (xxh32state-pending-set! state 0)
                  (xxh32state-processed-set! state
                                             (f32and #xffffffff
                                                     (f32+ STRIPE-32
                                                           (xxh32state-processed state))))
                  (lp offset acc1 acc2 acc3 acc4)))
               ((fx=? offset end)
                (xxh32state-acc1-set! state acc1)
                (xxh32state-acc2-set! state acc2)
                (xxh32state-acc3-set! state acc3)
                (xxh32state-acc4-set! state acc4))
               ((or (fx>? (xxh32state-pending state) 0)
                    (fx>? (fx+ offset STRIPE-32) end))
                ;; Pending data exists or less than a block remains.
                ;; Add more pending data.
                (let ((added (fxmin (fx- STRIPE-32 (xxh32state-pending state))
                                    (fx- end offset))))
                  (bytevector-copy! data offset
                                    m (xxh32state-pending state)
                                    added)
                  (xxh32state-pending-set! state (fx+ added (xxh32state-pending state)))
                  (lp (fx+ offset added) acc1 acc2 acc3 acc4)))
               (else
                ;; Consume a whole stripe
                (let-values (((acc1 acc2 acc3 acc4)
                              (xxh32-transform acc1 acc2 acc3 acc4 data offset)))
                  (xxh32state-processed-set! state
                                             (f32and #xffffffff
                                                     (f32+ STRIPE-32
                                                           (xxh32state-processed state))))
                  (lp (fx+ offset STRIPE-32) acc1 acc2 acc3 acc4)))))))))

;; Finish the state.
(define (xxh32-finish! state)
  (let ((m (xxh32state-m state))
        (acc1 (xxh32state-acc1 state))
        (acc2 (xxh32state-acc2 state))
        (acc3 (xxh32state-acc3 state))
        (acc4 (xxh32state-acc4 state))
        (processed (xxh32state-processed state))
        (pending (xxh32state-pending state)))
    ;; Convergence
    (let ((acc (if (< (+ processed pending) 16)
                   (xxh32state-acc state)
                   (bitwise-and #xffffffff
                                (+ (rol32 acc1 1)
                                   (rol32 acc2 7)
                                   (rol32 acc3 12)
                                   (rol32 acc4 18))))))
      ;; Input length
      (let ((acc (bitwise-and #xffffffff (+ acc (+ processed pending)))))
        ;; Remaining input
        (let lp ((acc acc) (pending pending) (offset 0))
          (cond
            ((fx>=? pending 4)
             (let ((lane (bytevector-u32-le-ref m offset)))
               (let* ((acc (bitwise-and #xffffffff (f32+ acc (umul32c lane PRIME32-3))))
                      (acc (umul32c (rol32 acc 17) PRIME32-4)))
                 (lp acc (fx- pending 4) (fx+ offset 4)))))
            ((fx>=? pending 1)
             (let ((lane (bytevector-u8-ref m offset)))
               (let* ((acc (bitwise-and #xffffffff (f32+ acc (umul32c lane PRIME32-5))))
                      (acc (umul32c (rol32 acc 11) PRIME32-1)))
                 (lp acc (fx- pending 1) (fx+ offset 1)))))
            (else
             (assert (fxzero? pending))
             ;; Avalanche
             (let* ((acc (f32xor acc (f32arithmetic-shift-right acc 15)))
                    (acc (umul32c acc PRIME32-2))
                    (acc (f32xor acc (f32arithmetic-shift-right acc 13)))
                    (acc (umul32c acc PRIME32-3))
                    (acc (f32xor acc (f32arithmetic-shift-right acc 16))))
               (xxh32state-acc-set! state acc)))))))))

(define (xxh32-finish state)
  (let ((copy (xxh32-copy state)))
    (xxh32-finish! copy)
    copy))

;; Find the XXH of the concatenation of the given bytevectors.
(define (xxh32 . data)
  (let ((state (make-xxh32)))
    (for-each (lambda (d) (xxh32-update! state d))
              data)
    (xxh32-finish! state)
    state))

(define (xxh32->string state)
  (define hex "0123456789abcdef")
  (do ((ret (make-string 8))
       (acc (xxh32state-acc state))
       (i 0 (fx+ i 1)))
      ((eqv? i 8) ret)
    (let ((n (f32and (f32arithmetic-shift-right acc (fx* 4 (fx- 7 i)))
                     #xf)))
      (string-set! ret i (string-ref hex n)))))

(define (xxh32->integer state)
  (xxh32state-acc state)))
