#!r6rs
;; Akku.scm wrote this file based on "slib-alist-SLIB-3b5-r7rs/slib/alist.sld"
;;;"alist.scm", alist functions for Scheme.
;;;Copyright (C) 1992, 1993, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017

;;@code{(require 'alist)}
;;@ftindex alist
;;
;;Alist functions provide utilities for treating a list of key-value pairs
;;as an associative database.  These functions take an equality predicate,
;;@var{pred}, as an argument.  This predicate should be repeatable,
;;symmetric, and transitive.
;;
;;Alist functions can be used with a secondary index method such as hash
;;tables for improved performance.

(library
  (slib alist)
  (export
    predicate->asso
    alist-inquirer
    alist-associator
    alist-remover
    alist-map
    alist-for-each)
  (import (scheme base) (scheme cxr))
  (define (predicate->asso pred)
    (cond ((eq? eq? pred) assq)
          ((eq? = pred) assv)
          ((eq? eqv? pred) assv)
          ((eq? char=? pred) assv)
          ((eq? equal? pred) assoc)
          ((eq? string=? pred) assoc)
          (else
           (lambda (key alist)
             (let l ((al alist))
               (cond ((null? al) #f)
                     ((pred key (caar al)) (car al))
                     (else (l (cdr al)))))))))
  (define (alist-inquirer pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key)
        (let ((pair (assofun key alist)))
          (and pair (cdr pair))))))
  (define (alist-associator pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key val)
        (let* ((pair (assofun key alist)))
          (cond (pair (set-cdr! pair val) alist)
                (else (cons (cons key val) alist)))))))
  (define (alist-remover pred)
    (lambda (alist key)
      (cond ((null? alist) alist)
            ((pred key (caar alist)) (cdr alist))
            ((null? (cdr alist)) alist)
            ((pred key (caadr alist))
             (set-cdr! alist (cddr alist))
             alist)
            (else
             (let l ((al (cdr alist)))
               (cond ((null? (cdr al)) alist)
                     ((pred key (caadr al))
                      (set-cdr! al (cddr al))
                      alist)
                     (else (l (cdr al)))))))))
  (define (alist-map proc alist)
    (map (lambda (pair)
           (cons (car pair) (proc (car pair) (cdr pair))))
         alist))
  (define (alist-for-each proc alist)
    (for-each
      (lambda (pair) (proc (car pair) (cdr pair)))
      alist)))
