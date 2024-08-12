#!/usr/bin/env scheme-script
;; Copied by Akku from ".akku/src/industria/bin/checksig.sps" !#
#!r6rs
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Demo program to verify OpenPGP signatures
;; Copyright © 2010, 2012, 2017, 2018 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(import (rnrs (6))
        (only (srfi :13 strings) string-index-right string-pad)
        (srfi :19 time)
        (srfi :26 cut)
        (srfi :98 os-environment-variables)
        (industria crypto dsa)
        (industria crypto rsa)
        (industria openpgp))

(define (print . x) (for-each display x) (newline))

(define (default-keyring)
  (let ((fn (string-append (get-environment-variable "HOME") "/.gnupg/pubring.gpg")))
    (if (file-exists? fn) fn "to use an empty one")))

(define (open-signature-file fn)
  (let ((p (open-file-input-port fn)))
    (cond ((port-ascii-armored? p)
           (display "Detected ASCII armored signature file.\n")
           (transcoded-port p (native-transcoder)))
        (else p))))

(define (get-signature p)
  (if (textual-port? p)
      (get-openpgp-detached-signature/ascii p)
      (let ((v (get-openpgp-packet p)))
        (unless (or (eof-object? v)
                    (openpgp-signature? v))
          (error 'get-signature "Expected an OpenPGP signature" v p))
        v)))

(define (checksig sigfile datafile keyfile)
  (display (string-append
            "Signature file: " sigfile "\n"
            "Data file: " datafile "\n"
            "Keyring file: " keyfile "\n\n"))
  ;; Verify all signatures in the sigfile
  (let ((p (open-signature-file sigfile))
        (dp (open-file-input-port datafile)))
    (let lp ()
      (let ((sig (get-signature p)))
        (unless (eof-object? sig)
          (display "Reading keyring...")
          (let ((keyring
                 (if (file-exists? keyfile)
                     (call-with-port (open-file-input-port keyfile)
                       (lambda (p) (get-openpgp-keyring/keyid p (openpgp-signature-issuer sig))))
                     (make-eqv-hashtable))))
            (display "\nVerifying signature...\n")
            (set-port-position! dp 0)
            (let-values (((result key)
                          (verify-openpgp-signature sig keyring dp)))
              (print "Signature made with "
                     (symbol->string (openpgp-signature-hash-algorithm sig))
                     " at time "
                     (date->string (openpgp-signature-creation-time sig)
                                   "~4")
                     "\nusing the "
                     (symbol->string (openpgp-signature-public-key-algorithm sig))
                     " key with ID "
                     (number->string (openpgp-signature-issuer sig) 16) ".\n")
              (case result
                ((missing-key)
                 (display
                  (string-append
                   "The key that made the signature is not in the keyring. You could try this: \n"
                   "gpg --keyserver subkeys.pgp.net --recv-key "
                   (string-pad (number->string key 16) 16 #\0)
                   "\n")))
                (else
                 (display
                  (if (eq? result 'good-signature)
                      "\x1b;[1;32mThis is a good signature.\x1b;[0m\n"
                      "\x1b;[1;31m*************** BAD SIGNATURE ***************\x1b;[0m\n"))
                 (let* ((keyid (openpgp-public-key-id key))
                        (keydata (hashtable-ref keyring keyid #f)))
                   (print "Fingerprint of the primary key: "
                          (openpgp-format-fingerprint
                           (openpgp-public-key-fingerprint (car keydata))))
                   (for-each (lambda (userid)
                               (print "User ID: "
                                      (openpgp-user-id-value userid)))
                             (filter openpgp-user-id? keydata)))
                 (newline)))))
          (lp))))))

(apply
 (case-lambda
   ((name sigfile)
    (checksig sigfile
              (cond ((string-index-right sigfile #\.)
                     => (lambda (i) (substring sigfile 0 i)))
                    (else
                     (display (string-append
                               "Can't guess the data filename.\n"
                               "Usage: " name
                               " signature-file [data-file] [keyring-file]\n")
                              (current-error-port))
                     (exit 1)))
              (default-keyring)))
   ((_ sigfile datafile)
    (checksig sigfile datafile (default-keyring)))
   ((_ sigfile datafile keyring)
    (checksig sigfile datafile keyring))
   ((name)
    (display (string-append
              "Usage: " name " signature-file [data-file] [keyring-file]\n\
\n\
Checks the detached OpenPGP signatures in SIGNATURE-FILE against
the data in DATA-FILE. If DATA-FILE is not specified the default
is to drop the last part of the SIGNATURE-FILE filename.
If KEYRING-FILE file is not specified the default is
" (default-keyring) ".\n")
             (current-error-port))
    (exit 1)))
  (command-line))
