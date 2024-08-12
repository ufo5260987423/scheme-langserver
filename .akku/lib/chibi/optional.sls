#!r6rs
;; Akku.scm wrote this file based on "chibi-optional-0.9.1.3/chibi/optional.sld"

(library
  (chibi optional)
  (export
    let-optionals
    let-optionals*
    opt-lambda
    define-opt
    let-keywords
    let-keywords*
    keyword-ref
    keyword-ref*)
  (import (scheme base))
  (define-syntax let-optionals*
    (syntax-rules ()
      ((let-optionals* opt-ls () . body)
       (begin . body))
      ((let-optionals* (op . args) vars . body)
       (let ((tmp (op . args)))
         (let-optionals* tmp vars . body)))
      ((let-optionals*
         tmp
         ((var default) . rest)
         .
         body)
       (let ((var (if (pair? tmp) (car tmp) default))
             (tmp2 (if (pair? tmp) (cdr tmp) '())))
         (let-optionals* tmp2 rest . body)))
      ((let-optionals* tmp tail . body)
       (let ((tail tmp)) . body))))
  (define-syntax symbol->keyword*
    (syntax-rules ()
      ((symbol->keyword* sym)
       (string->symbol
         (string-append (symbol->string sym) ":")))))
  (include "optional.scm"))
