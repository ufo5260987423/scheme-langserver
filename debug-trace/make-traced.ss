#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
(import (chezscheme))

(define (wrap-define-syntax expr)
  (if (and (list? expr) (eq? (car expr) 'define-syntax) (>= (length expr) 3))
      (let ([name (cadr expr)]
            [transformer (caddr expr)])
        (if (and (list? transformer)
                 (or (eq? (car transformer) 'syntax-rules)
                     (eq? (car transformer) 'lambda)))
            `(define-syntax ,name
               (let ([raw ,transformer])
                 (lambda (x)
                   (let ([result (raw x)])
                     (display "===CHEZ ")
                     (display ',name)
                     (display " INPUT: ")
                     (write (syntax->datum x))
                     (newline)
                     (display "===CHEZ ")
                     (display ',name)
                     (display " OUTPUT: ")
                     (write (syntax->datum result))
                     (newline)
                     (newline)
                     result))))
            expr))
      expr))

(define (transform-library expr)
  (if (and (list? expr) (eq? (car expr) 'library) (>= (length expr) 4))
      (let ([imports (cadddr expr)])
        `(library ,(cadr expr)
           ,(caddr expr)
           ,(if (and (list? imports) (eq? (car imports) 'import))
                `(import (rnrs io simple) ,@(cdr imports))
                imports)
           ,@(map wrap-define-syntax (cddddr expr))))
      expr))

(call-with-input-file ".akku/lib/ufo-match.chezscheme.sls"
  (lambda (in)
    (call-with-output-file "debug-trace/ufo-match-traced.sls"
      (lambda (out)
        (let loop ()
          (let ([expr (read in)])
            (unless (eof-object? expr)
              (write (transform-library expr) out)
              (newline out)
              (loop))))))))

(display "Generated debug-trace/ufo-match-traced.sls\n")
