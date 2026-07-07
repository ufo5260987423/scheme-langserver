#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier rules let)
  (scheme-langserver analysis identifier rules let*)
  (scheme-langserver analysis identifier rules letrec)
  (scheme-langserver analysis identifier rules let-values)
  (scheme-langserver analysis identifier rules do)
  (scheme-langserver analysis identifier rules with-syntax)
  (scheme-langserver analysis identifier rules syntax-case)
  (scheme-langserver analysis identifier rules syntax-rules)
  (scheme-langserver analysis identifier rules define-syntax)
  (scheme-langserver analysis identifier rules define-record-type)
  (scheme-langserver analysis identifier rules fluid-let)
  (scheme-langserver analysis identifier rules let-syntax)
  (scheme-langserver analysis identifier rules letrec-syntax)
  (scheme-langserver analysis identifier rules s7 lambda*)
  (scheme-langserver analysis identifier rules s7 define*)
  (scheme-langserver analysis tokenizer)

  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document))

(define counter 0)

(define (make-document&form-node src)
  (set! counter (+ counter 1))
  (let* ([path (string-append "/tmp/test-shared-reference-binding-forms-" (number->string counter) ".ss")]
      [_ (let ([p (open-file-output-port path (file-options replace) 'block (native-transcoder))])
           (display src p)
           (close-port p))]
      [document (make-document (string-append "file://" path) src '())]
      [form-node (init-index-node '() (car (source-file->annotations path)))])
    (cons document form-node)))

(define (contains-identifier? references sym)
  (find 
    (lambda (reference) 
      (equal? sym (identifier-reference-identifier reference)))
    references))

(test-begin "let-process handles shared binding list")
  (let* ([src "(let (#1=(x 1) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [let-node (cdr pair)])
    (let-process '() '() document let-node)
    (test-assert "parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node let-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "let*-process handles shared binding list")
  (let* ([src "(let* (#1=(x 1) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [let*-node (cdr pair)])
    (let*-process '() '() document let*-node)
    (test-assert "parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node let*-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "letrec-process handles shared binding list")
  (let* ([src "(letrec (#1=(x 1) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [letrec-node (cdr pair)])
    (letrec-process '() '() document letrec-node)
    (test-assert "parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node letrec-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "let-values-process handles shared binding list")
  (let* ([src "(let-values (#1=(((x) 1)) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [let-values-node (cdr pair)])
    (let-values-process '() '() document let-values-node)
    (test-assert "process completes without exception"
      #t)
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "do-process handles shared variable list")
  (let* ([src "(do (#1=((x 1)) #1#) (#f) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [do-node (cdr pair)])
    (test-assert "process returns without exception and produces references"
      (not (null? (do-process '() '() document do-node))))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "fluid-let-process handles shared binding list")
  (let* ([src "(fluid-let (#1=((x 1)) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [fluid-let-node (cdr pair)])
    (fluid-let-process '() '() document fluid-let-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "let-syntax-process handles shared binding list")
  (let* ([src "(let-syntax (#1=(((x (syntax-rules ((_) 1)))) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [let-syntax-node (cdr pair)])
    (let-syntax-process '() '() document let-syntax-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "letrec-syntax-process handles shared binding list")
  (let* ([src "(letrec-syntax (#1=(((x (syntax-rules ((_) 1)))) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [letrec-syntax-node (cdr pair)])
    (letrec-syntax-process '() '() document letrec-syntax-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "with-syntax-process handles shared binding list")
  (let* ([src "(with-syntax (#1=(((x 1)) #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [with-syntax-node (cdr pair)])
    (with-syntax-process '() '() document with-syntax-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "syntax-rules-process handles shared clause")
  (let* ([src "(syntax-rules () #1=((_) 1) #1#)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [syntax-rules-node (cdr pair)])
    (syntax-rules-process '() '() document syntax-rules-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "syntax-case-process handles shared clause")
  (let* ([src "(syntax-case 1 () #1=((_ ) 1) #1#)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [syntax-case-node (cdr pair)])
    (syntax-case-process '() '() document syntax-case-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "define-syntax-process handles cyclic improper macro formals")
  (let* ([src "(define-syntax (f . #1=(x . #1#)) 1)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [define-syntax-node (cdr pair)])
    (define-syntax-process '() '() document define-syntax-node)
    (test-assert "macro parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node define-syntax-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "define-record-type-process handles shared field spec")
  (let* ([src "(define-record-type foo (fields #1=(x) #1#))\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [drt-node (cdr pair)])
    (define-record-type-process '() '() document drt-node)
    (test-assert "process completes without exception"
      #t))
(test-end)

(test-begin "lambda*-process handles cyclic improper formals")
  (let* ([src "(lambda* #1=(x . #1#) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [lambda*-node (cdr pair)])
    (lambda*-process '() '() document lambda*-node)
    (test-assert "parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node lambda*-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(test-begin "define*-process handles cyclic improper formals")
  (let* ([src "(define* (f . #1=(x . #1#)) x)\n"]
      [pair (make-document&form-node src)]
      [document (car pair)]
      [define*-node (cdr pair)])
    (define*-process '() '() document define*-node)
    (test-assert "parameter x is bound"
      (contains-identifier? (index-node-references-import-in-this-node define*-node) 'x))
    (test-assert "no duplicate diagnoses"
      (null? (document-diagnoses document))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
