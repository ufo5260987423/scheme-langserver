;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; RFC 5656 adapted to Curve25519
;; http://git.libssh.org/users/aris/libssh.git/plain/doc/curve25519-sha256@libssh.org.txt

;; This handles curve25519-sha256@libssh.org

(library (industria ssh kex-ecdh)
  (export register-kex-ecdh

          make-kex-ecdh-init kex-ecdh-init?
          kex-ecdh-init-Q_C

          make-kex-ecdh-reply kex-ecdh-reply?
          kex-ecdh-reply-K_S
          kex-ecdh-reply-Q_S
          kex-ecdh-reply-signature

          make-kex-ecdh-key-exchanger)
  (import (except (rnrs (6)) put-string)
          (hashing sha-2)
          (industria bytevectors)
          (industria crypto ecdh)
          (industria ssh public-keys)
          (industria ssh private)
          (industria ssh private serialize))

  (define SSH-MSG-KEX-ECDH-INIT 30)
  (define SSH-MSG-KEX-ECDH-REPLY 31)

  (define (register-kex-ecdh reg)
    (reg SSH-MSG-KEX-ECDH-INIT parse-kex-ecdh-init put-kex-ecdh-init)
    (reg SSH-MSG-KEX-ECDH-REPLY parse-kex-ecdh-reply put-kex-ecdh-reply))

  (define-record-type kex-ecdh-init
    (parent ssh-packet)
    (fields Q_C)                        ;client's ephemeral public key
    (protocol
     (lambda (p)
       (lambda (Q_C)
         ((p SSH-MSG-KEX-ECDH-INIT) Q_C)))))

  (define (parse-kex-ecdh-init b)
    (make-kex-ecdh-init (read-bytevector b)))

  (define (put-kex-ecdh-init p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(bytevector)))

  (define-record-type kex-ecdh-reply
    (parent ssh-packet)
    (fields K_S                         ;public host key
            Q_S                         ;server's ephemeral public key
            signature)                  ;exchange signature
    (protocol
     (lambda (p)
       (lambda (K_S Q_S signature)
         ((p SSH-MSG-KEX-ECDH-REPLY) K_S Q_S signature)))))

  (define (parse-kex-ecdh-reply b)
    (let* ((K_S (read-bytevector b))
           (Q_S (read-bytevector b))
           (signature (read-bytevector b)))
      (make-kex-ecdh-reply K_S Q_S signature)))

  (define (put-kex-ecdh-reply p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(bytevector bytevector bytevector)))

;;; Key exchange logic

  (define (make-kex-ecdh-key-exchanger kex client? send)
    (let ((kexer (if client? make-client-kexer make-server-kexer)))
      (cond ((string=? kex "curve25519-sha256@libssh.org")
             (kexer sha-256 sha-256->bytevector prf-sha-256 send))
            ((string=? kex "curve25519-sha256")
             (kexer sha-256 sha-256->bytevector prf-sha-256 send))
            (else
             (error 'make-kex-ecdh-key-exchanger
                    "Unknown key exchange algorithm" kex)))))

  (define (invalid-state method state)
    (error 'kexdh-ecdh "Invalid state" method state))

  (define (make-client-kexer hash hash->bytevector prf send)
    (let ((state 'send-init)
          (init-data #f)
          (Q_C #f)
          (secret #f))
      (lambda (method arg)
        (case method
          ((start)
           (case state
             ((send-init)
              (set! state (if init-data
                              'recv-reply
                              'wait-version/init))
              (let-values ([(a Q_C*) (make-ecdh-curve25519-secret)])
                (set! secret a)
                (set! Q_C Q_C*)
                (send (make-kex-ecdh-init Q_C)))
              #f)
             ((wait-version/init)
              (set! state (if init-data
                              'recv-reply
                              'wait-version/init))
              (send (make-kex-ecdh-init Q_C))
              #f)
             (else (invalid-state method state))))
          ((init)
           (case state
             ((wait-version/init)
              (set! state 'recv-reply)
              (set! init-data arg) ; host-key-algorithm V_C V_S I_C I_S
              #f)
             (else (invalid-state method state))))
          ((packet)
           (case state
             ((recv-reply)
              (set! state 'done)
              (let ((K_S (kex-ecdh-reply-K_S arg))
                    (Q_S (kex-ecdh-reply-Q_S arg))
                    (sig (kex-ecdh-reply-signature arg)))
                (unless (= (bytevector-length Q_S) 32)
                  (error 'kex-ecdh "Bad kex-ecdh-reply"))
                (let* ((keyalg (cadr (memq 'host-key-algorithm init-data)))
                       (hostkey (get-ssh-public-key
                                 (open-bytevector-input-port K_S)))
                       (K (ecdh-curve25519 secret Q_S)))
                  (unless K
                    (error 'kex-ecdh "Bad kex-ecdh-reply"))
                  (let* ((K (bytevector->uint K))
                         (H (apply hash-kex-data hash hash->bytevector
                                   'K_S (ssh-public-key->bytevector hostkey)
                                   'Q_C Q_C 'Q_S Q_S 'K K init-data)))
                    (unless (eq? 'ok (verify-signature H keyalg hostkey sig))
                      (error 'kex-ecdh "Bad kex-ecdh-reply signature"))
                    (list hostkey (integer->mpint K) H prf)))))
             (else (invalid-state method state))))
          (else (invalid-state method state))))))

  (define (make-server-kexer hash hash->bytevector prf send)
    (let ((state 'start)
          (init-data #f)
          (private-key #f))
      (lambda (method arg)
        (case method
          ((start)
           (case state
             ((start)
              (set! state 'wait-version/init)
              #f)
             (else (invalid-state method state))))
          ((init)
           (case state
             ((wait-version/init)
              (set! state 'get-key)
              (set! init-data arg)        ; V_C V_S I_C I_S
              #f)
             (else (invalid-state method state))))
          ((private-key)
           (case state
             ((get-key)
              (set! private-key arg)
              (set! state 'recv-init)
              #f)
             (else (invalid-state method state))))
          ((packet)
           (case state
             ((recv-init)
              (set! state 'done)
              (let ((Q_C (kex-ecdh-init-Q_C arg)))
                (unless (= (bytevector-length Q_C) 32)
                  (error 'kex-ecdh/server "Bad kex-ecdh-init"))
                (let-values ([(secret Q_S) (make-ecdh-curve25519-secret)])
                  (let ((hostkey (private->public private-key))
                        (K (ecdh-curve25519 secret Q_C)))
                    (unless K
                      (error 'kexdh-ecdh/server "Bad kex-ecdh-init"))
                    (let* ((K (bytevector->uint K))
                           (H (apply hash-kex-data hash hash->bytevector
                                     'K_S (ssh-public-key->bytevector hostkey)
                                     'Q_C Q_C 'Q_S Q_S 'K K init-data))
                           (keyalg (cadr (memq 'host-key-algorithm init-data)))
                           (sig (make-signature H keyalg private-key)))
                      (send (make-kex-ecdh-reply (ssh-public-key->bytevector hostkey)
                                                 Q_S sig))
                      (list hostkey (integer->mpint K) H prf))))))
             (else (invalid-state method state))))
          (else (invalid-state method state)))))))
