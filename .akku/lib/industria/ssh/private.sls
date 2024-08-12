;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2018, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

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

;; Private signature algorithms, stuff

(library (industria ssh private)
  (export ssh-packet? ssh-packet-type ssh-packet
          parse-signature make-signature
          verify-signature hash-kex-data
          algorithm-can-sign? algorithm-can-verify?
          private->public prf-sha-1 prf-sha-256)
  (import (except (rnrs (6)) put-string)
          (only (srfi :13 strings) string-prefix?)
          (industria bytevectors)
          (industria crypto dsa)
          (industria crypto ec)
          (industria crypto ecdsa)
          (industria crypto eddsa)
          (industria crypto rsa)
          (hashing sha-1)
          (hashing sha-2)
          (industria ssh public-keys)
          (industria buffer)
          (struct pack)
          (industria strings)
          (industria ssh private serialize))

  (define (private->public key)
    (cond ((rsa-private-key? key)
           (rsa-private->public key))
          ((dsa-private-key? key)
           (dsa-private->public key))
          ((ecdsa-private-key? key)
           (ecdsa-private->public key))
          ((ed25519-private-key? key)
           (ed25519-private->public key))
          (else
           (error 'private->public
                  "Unimplemented public key algorithm"
                  key))))

  (define (algorithm-can-sign? algorithm)
    (member algorithm
            '("rsa-sha2-512"
              "rsa-sha2-256"
              "ssh-rsa"
              "ecdsa-sha2-nistp256"
              "ecdsa-sha2-nistp384"
              "ecdsa-sha2-nistp521"
              "ssh-ed25519"
              "ssh-dss")))

  (define (algorithm-can-verify? algorithm)
    (member algorithm
            '("rsa-sha2-512"
              "rsa-sha2-256"
              "ssh-rsa"
              "ecdsa-sha2-nistp256"
              "ecdsa-sha2-nistp384"
              "ecdsa-sha2-nistp521"
              "ssh-ed25519"
              "ssh-dss")))

  (define (parse-signature sig)
    (define (get p)
      (get-bytevector-n p (get-unpack p "!L")))
    (let ((p (open-bytevector-input-port sig)))
      (let ((type (utf8->string (get p))))
        (cond ((member type '("ssh-rsa" "rsa-sha2-256" "rsa-sha2-512"))
               (list type (bytevector->uint (get p))))
              ((string=? type "ssh-dss")
               (let* ((bv (get p))
                      (r (subbytevector bv 0 160/8))
                      (s (subbytevector bv 160/8 (* 160/8 2))))
                 (list type (bytevector->uint r)
                       (bytevector->uint s))))
              ((string-prefix? "ecdsa-sha2-" type)
               (let* ((blob (open-bytevector-input-port (get p)))
                      (r (bytevector->uint (get blob)))
                      (s (bytevector->uint (get blob))))
                 (list type r s)))
              ((member type '("ssh-ed25519"))
               (list type (get p)))
              (else
               (error 'parse-signature "Unimplemented signature algorithm"
                      type))))))

  (define (make-signature msg keyalg key)
    (call-with-bytevector-output-port
      (lambda (p)
        (cond
          ((and (rsa-private-key? key)
                (member keyalg '("ssh-rsa" "rsa-sha2-256" "rsa-sha2-512")))
           (let-values ([(alg hash)
                         (cond ((string=? keyalg "ssh-rsa")
                                (values 'sha-1 (sha-1->bytevector (sha-1 msg))))
                               ((string=? keyalg "rsa-sha2-256")
                                (values 'sha-256 (sha-256->bytevector (sha-256 msg))))
                               ((string=? keyalg "rsa-sha2-512")
                                (values 'sha-512 (sha-512->bytevector (sha-512 msg))))
                               (else
                                (error 'make-signature
                                       "Unimplemented RSA algorithm" keyalg)))])
             (let ((sig (uint->bytevector
                         (rsa-pkcs1-encrypt-digest alg hash key))))
               (put-bvstring p keyalg)
               (put-bytevector p (pack "!L" (bytevector-length sig)))
               (put-bytevector p sig))))
          ((and (dsa-private-key? key)
                (member keyalg '("ssh-dss")))
           (let-values (((r s) (dsa-create-signature
                                (sha-1->bytevector (sha-1 msg)) key)))
             (let ((sig (make-bytevector (* 160/8 2) 0)))
               (bytevector-uint-set! sig 0 r (endianness big) 160/8)
               (bytevector-uint-set! sig 160/8 s (endianness big) 160/8)
               (put-bvstring p "ssh-dss")
               (put-bytevector p (pack "!L" (bytevector-length sig)))
               (put-bytevector p sig))))
          ((and (ecdsa-sha-2-private-key? key)
                (member keyalg '("ecdsa-sha2-nistp256"
                                 "ecdsa-sha2-nistp384"
                                 "ecdsa-sha2-nistp521")))
           (let-values (((r s) (ecdsa-sha-2-create-signature msg key))
                        ((blob extract) (open-bytevector-output-port)))
             (put-mpint blob r)
             (put-mpint blob s)
             (put-bvstring p (ssh-public-key-algorithm (private->public key)))
             (let ((sig (extract)))
               (put-bytevector p (pack "!L" (bytevector-length sig)))
               (put-bytevector p sig))))
          ((and (ed25519-private-key? key) (member keyalg '("ssh-ed25519")))
           (let ((sig (ed25519-sign key msg)))
             (put-bvstring p "ssh-ed25519")
             (put-bytevector p (pack "!L" (bytevector-length sig)))
             (put-bytevector p sig)))
          (else
           (error 'make-signature
                  "Unimplemented public key algorithm"
                  keyalg))))))

  (define (verify-signature H keyalg key sig-bv)
    (let ((signature (parse-signature sig-bv)))
      (cond
        ((not (string=? keyalg (car signature)))
         ;; (error 'verify-signature "The algorithms do not match"
         ;;        keyalg key signature)
         'bad)
        ((not (member keyalg (ssh-public-key-algorithm* key)))
         ;; (error 'verify-signature "The algorithm is not supported by the key"
         ;;        keyalg key signature)
         'bad)
        ((rsa-public-key? key)
         (let ((sig (cadr (rsa-pkcs1-decrypt-digest (cadr signature) key))))
           (cond
             ((not (string=? keyalg (car signature)))
              'bad)
             ((and (string=? keyalg "ssh-rsa")
                   (fx=? (bytevector-length sig) (sha-1-length)))
              (if (sha-1-hash=? (sha-1 H) sig) 'ok 'bad))
             ((and (string=? keyalg "rsa-sha2-512")
                   (fx=? (bytevector-length sig) (sha-512-length)))
              (if (sha-512-hash=? (sha-512 H) sig) 'ok 'bad))
             ((and (string=? keyalg "rsa-sha2-256")
                   (fx=? (bytevector-length sig) (sha-256-length)))
              (if (sha-256-hash=? (sha-256 H) sig) 'ok 'bad))
             (else 'bad))))
        ((dsa-public-key? key)
         (if (and (string=? keyalg "ssh-dss")
                  (string=? keyalg (car signature))
                  (dsa-verify-signature (sha-1->bytevector (sha-1 H))
                                        key (cadr signature)
                                        (caddr signature)))
             'ok 'bad))
        ((ecdsa-sha-2-public-key? key)
         (if (and (string=? keyalg (car signature))
                  (ecdsa-sha-2-verify-signature H key (cadr signature)
                                                (caddr signature)))
             'ok 'bad))
        ((ed25519-public-key? key)
         (if (and (string=? keyalg "ssh-ed25519")
                  (string=? keyalg (car signature))
                  (ed25519-verify key H (cadr signature)))
             'ok 'bad))
        (else
         (error 'verify-signature
                "Unimplemented public key algorithm"
                keyalg key signature)))))

  ;; Used by kexdh and kex-dh-gex. The server signs this digest to
  ;; prove it owns the key it sent.
  (define (hash-kex-data hash ->bytevector . data)
    ;; For kexdh:
    ;; H = hash(V_C || V_S || I_C || I_S || K_S || e || f || K)
    ;; For kex-dh-gex:
    ;; H = hash(V_C || V_S || I_C || I_S || K_S || min || n ||
    ;;          max || p || g || e || f || K)
    ;; For kex-ecdh:
    ;; H = hash(V_C || V_S || I_C || I_S || K_S || Q_C || Q_S || K)
    (->bytevector
     (hash
      (call-with-bytevector-output-port
        (lambda (p)
          (for-each (lambda (k)
                      (cond ((memq k data) =>
                             (lambda (v)
                               (put-bvstring p (cadr v))))))
                    '(V_C V_S I_C I_S K_S Q_C Q_S))
          (for-each (lambda (k)
                      (cond ((memq k data) =>
                             (lambda (v)
                               (put-bytevector p (pack "!L" (cadr v)))))))
                    '(min n max))
          (for-each (lambda (k)
                      (cond ((memq k data) =>
                             (lambda (v)
                               (put-mpint p (cadr v))))))
                    '(p g e f K)))))))

  (define (make-prf make length update! copy finish! finish ->bytevector)
    (lambda (X len session-id K H)
      ;; Generate LEN bytes of key material. Section 7.2 in RFC 4253.
      (call-with-bytevector-output-port
        (lambda (p)
          (let ((s (make)))
            (update! s K)
            (update! s H)
            (let ((s* (copy s)))
              (update! s* (pack "C" (char->integer X)))
              (update! s* session-id)
              (finish! s*)
              (do ((Kn (->bytevector s*) (->bytevector (finish s)))
                   (len len (- len (length))))
                  ((<= len 0))
                (update! s Kn)
                (put-bytevector p Kn 0 (min len (bytevector-length Kn))))))))))

  (define prf-sha-1 (make-prf make-sha-1 sha-1-length sha-1-update! sha-1-copy
                              sha-1-finish! sha-1-finish sha-1->bytevector))

  (define prf-sha-256 (make-prf make-sha-256 sha-256-length sha-256-update! sha-256-copy
                                sha-256-finish! sha-256-finish sha-256->bytevector))

  ;; The parent of all record abstractions of ssh packets
  (define-record-type ssh-packet
    (fields type)))
