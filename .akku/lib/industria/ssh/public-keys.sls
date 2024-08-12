;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2017, 2018, 2019 Göran Weinholt <goran@weinholt.se>

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

;; Procedures that read and write SSH 2 public keys

;; 4253 The Secure Shell (SSH) Transport Layer Protocol. T. Ylonen, C.
;;      Lonvick, Ed.. January 2006. (Format: TXT=68263 bytes) (Status:
;;      PROPOSED STANDARD)

;; 4716 The Secure Shell (SSH) Public Key File Format. J. Galbraith, R.
;;      Thayer. November 2006. (Format: TXT=18395 bytes) (Status:
;;      INFORMATIONAL)

;; TODO: http://permalink.gmane.org/gmane.ietf.secsh/6520 ?

;; https://www.iana.org/assignments/ssh-parameters/ssh-parameters.xhtml#ssh-parameters-19

(library (industria ssh public-keys)
  (export get-ssh-public-key
          ssh-public-key->bytevector
          ssh-public-key-fingerprint
          ssh-public-key-random-art
          ssh-public-key-algorithm
          ssh-public-key-algorithm*)
  (import (only (srfi :13 strings) string-pad
                string-join string-prefix?)
          (except (rnrs) put-string)
          (hashing md5)
          (hashing sha-2)
          (struct pack)
          (industria base64)
          (industria bytevectors)
          (industria crypto dsa)
          (industria crypto ec)
          (industria crypto ecdsa)
          (industria crypto eddsa)
          (industria crypto rsa)
          (industria ssh random-art)
          (industria ssh private serialize))

  ;; ssh-dss           REQUIRED     sign   Raw DSS Key
  ;; ssh-rsa           RECOMMENDED  sign   Raw RSA Key
  ;; pgp-sign-rsa      OPTIONAL     sign   OpenPGP certificates (RSA key)
  ;; pgp-sign-dss      OPTIONAL     sign   OpenPGP certificates (DSS key)

  ;; Reads a binary SSH public key. They are normally Base64 encoded
  ;; when stored in files.
  (define (get-ssh-public-key p)
    (define who 'get-ssh-public-key)
    (let ((type (get-string p)))
      (cond ((string=? type "ssh-rsa")
             (let* ((e (get-mpint p))
                    (n (get-mpint p)))
               (make-rsa-public-key n e)))
            ((string=? type "ssh-dss")
             (let* ((p* (get-mpint p))
                    (q (get-mpint p))
                    (g (get-mpint p))
                    (y (get-mpint p)))
               (make-dsa-public-key p* q g y)))
            ((string-prefix? "ecdsa-sha2-" type)
             (let* ((id (get-string p)) ;curve ID
                    (Q (get-mpint p)))  ;public point
               (make-ecdsa-sha-2-public-key (id->curve id who) Q)))
            ;; https://datatracker.ietf.org/doc/draft-ietf-curdle-ssh-ed25519-ed448/
            ((string=? type "ssh-ed25519")
             (make-ed25519-public-key (get-bytevector p)))
            #;
            ((string=? type "ssh-ed448")
             (make-ed448-public-key (get-bytevector p)))
            (else
             (assertion-violation 'get-ssh-public-key
                                  "Unknown public key algorithm"
                                  type p)))))

  (define (id->curve x who)
    (cond ((string=? x "nistp256") nistp256)
          ((string=? x "nistp384") nistp384)
          ((string=? x "nistp521") nistp521)
          (else
           (assertion-violation who "Unknown elliptic curve" x))))

  (define (curve->id x who)
    (cond ((elliptic-curve=? x nistp256) "nistp256")
          ((elliptic-curve=? x nistp384) "nistp384")
          ((elliptic-curve=? x nistp521) "nistp521")
          ;; For every other curve, its ASN.1 OID in ASCII is used
          (else
           (assertion-violation who "Unknown elliptic curve" x))))

  (define (ssh-public-key-algorithm key)
    (car (ssh-public-key-algorithm* key)))

  (define (ssh-public-key-algorithm* key)
    (define who 'ssh-public-key-algorithm*)
    (cond ((rsa-public-key? key) '("ssh-rsa" "rsa-sha2-256" "rsa-sha2-512"))
          ((dsa-public-key? key) '("ssh-dss"))
          ((ecdsa-sha-2-public-key? key)
           (list (string-append "ecdsa-sha2-"
                                (curve->id (ecdsa-public-key-curve key) who))))
          ((ed25519-public-key? key) '("ssh-ed25519"))
          (else
           (assertion-violation who "Unknown public key algorithm" key))))

  (define (ssh-public-key->bytevector key)
    (define who 'ssh-public-key->bytevector)
    (call-with-bytevector-output-port
      (lambda (p)
        (cond ((rsa-public-key? key)
               (put-string p (string->utf8 "ssh-rsa"))
               (put-mpint p (rsa-public-key-e key))
               (put-mpint p (rsa-public-key-n key)))
              ((dsa-public-key? key)
               (put-string p (string->utf8 "ssh-dss"))
               (put-mpint p (dsa-public-key-p key))
               (put-mpint p (dsa-public-key-q key))
               (put-mpint p (dsa-public-key-g key))
               (put-mpint p (dsa-public-key-y key)))
              ((ecdsa-sha-2-public-key? key)
               ;; This does not use point compression. If point
               ;; compression could be used, then each ECDSA key would
               ;; have two different fingerprints.
               (let ((id (curve->id (ecdsa-public-key-curve key) who))
                     (Q (elliptic-point->bytevector (ecdsa-public-key-Q key)
                                                    (ecdsa-public-key-curve key))))
                 (put-string p (string->utf8 (string-append "ecdsa-sha2-" id)))
                 (put-string p (string->utf8 id))
                 (put-bytevector p (pack "!L" (bytevector-length Q)))
                 (put-bytevector p Q)))
              ((ed25519-public-key? key)
               (put-string p (string->utf8 "ssh-ed25519"))
               (put-string p (ed25519-public-key-value key)))
              (else
               (assertion-violation who "Unknown public key algorithm" key))))))

  (define ssh-public-key-fingerprint
    (case-lambda
      ((key)
       (ssh-public-key-fingerprint key 'sha256))
      ((key algorithm)
       (case algorithm
         ((sha256)
          (string-append
           "SHA256:"
           (base64-encode (sha-256->bytevector (sha-256 (ssh-public-key->bytevector key)))
                          0 (sha-256-length) #f 'no-padding)))
         ((md5)
          (string-append
           "MD5:"
           (string-join
            (map (lambda (b)
                   (string-pad (string-downcase (number->string b 16)) 2 #\0))
                 (bytevector->u8-list
                  (md5->bytevector (md5 (ssh-public-key->bytevector key)))))
            ":" 'infix)))
         (else
          (assertion-violation 'ssh-public-key-fingerprint
                               "Invalid algorithm" key algorithm))))))

  ;; TODO: bubblebabble

  (define ssh-public-key-random-art
    (case-lambda
      ((key)
       (ssh-public-key-random-art key 'sha256))
      ((key algorithm)
       (let-values (((prefix length)
                     (cond ((rsa-public-key? key)
                            (values "RSA" rsa-public-key-length))
                           ((dsa-public-key? key)
                            (values "DSA" dsa-public-key-length))
                           ((ecdsa-public-key? key)
                            (values "ECDSA" ecdsa-public-key-length))
                           ((ed25519-public-key? key)
                            (values "ED25519" eddsa-public-key-length))
                           (else
                            (values "UNKNOWN" (lambda (_) +nan.0))))))
         (let ((header (string-append prefix " " (number->string (length key)))))
           (case algorithm
             ((sha256)
              (random-art (sha-256->bytevector (sha-256 (ssh-public-key->bytevector key)))
                          header
                          "SHA256"))
             ((md5)
              (random-art (md5->bytevector (md5 (ssh-public-key->bytevector key)))
                          header
                          "MD5"))
             (else
              (assertion-violation 'ssh-public-key-random-art
                                   "Invalid algorithm" key algorithm)))))))))
