;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ufo-threaded-function)
  (export pool-size-ref pool-size-add 
    threaded-map 
    threaded-vector-map )
  (import (chezscheme) (ufo-thread-pool))

(define default-pool (init-thread-pool))

(define (pool-size-ref) (thread-pool-size-ref default-pool))
(define (pool-size-add value) (thread-pool-size-add default-pool value))

(define-record-type optional
  (fields 
    (immutable mutex)
    (immutable condition)
    (mutable finished?)
    (mutable value)))

(define (optional-wrapper proc)
  (lambda args
    (let ( [optional (make-optional (make-mutex) (make-condition) #f #f) ])
      (thread-pool-add-job default-pool 
        (lambda() 
          (let((value (apply proc args)))
            (with-mutex (optional-mutex optional)
              (optional-value-set! optional value)
              (optional-finished?-set! optional #t)
              (condition-broadcast (optional-condition optional))))))
      optional)))

(define (de-optional optional)
  (let loop ()
    (with-mutex (optional-mutex optional)
      (if (optional-finished? optional) 
        (optional-value optional)
        (begin
          (condition-wait (optional-condition optional) (optional-mutex optional))
          (loop))))))

(define (filter-optional-wrapper proc)
  (optional-wrapper 
    (lambda args
      (cons (apply proc args) args))))

(define (threaded-map proc . list-args)
  (map de-optional
    (apply map (append (list (optional-wrapper proc)) list-args))))

(define (threaded-vector-map proc . vector-args)
  (vector-map de-optional
    (apply vector-map (append (list (optional-wrapper proc)) vector-args))))
)
