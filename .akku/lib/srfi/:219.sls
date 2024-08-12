#!r6rs
(library (srfi :219)
  (export define)
  (import (rename (rnrs base) (define native-define)))
  (define-syntax define
    (syntax-rules ()
      ((define ((head . outer-args) . args) . body)
       (define (head . outer-args) (lambda args . body)))
      ((define head . body)
       (native-define head . body)))))
