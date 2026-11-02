#!/usr/bin/env scheme-script
;; P1 performance probe: whole-workspace init timing with type inference ON.
;; Usage: scheme --script bin/profile-fixpoint-cost.ss
#!r6rs

(import
  (chezscheme)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis type substitutions generator))

(define (ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

(define t0 (ms))
(define workspace (init-workspace (current-directory) 'akku 'r6rs #f #t))
(printf "init-workspace (type inference ON): ~a ms~n" (- (ms) t0))
