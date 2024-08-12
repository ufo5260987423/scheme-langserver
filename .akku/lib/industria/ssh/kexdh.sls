;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012, 2019 Göran Weinholt <goran@weinholt.se>

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

;; Diffie-Hellman Key Exchange from RFC4253

;; This handles diffie-hellman-group14-sha1 and
;; diffie-hellman-group1-sha1.

(library (industria ssh kexdh)
  (export register-kexdh
          make-kexdh-init kexdh-init? kexdh-init-e
          make-kexdh-reply kexdh-reply? kexdh-reply-f
          kexdh-reply-host-key kexdh-reply-signature
          make-kex-dh-key-exchanger)
  (import (except (rnrs (6)) put-string)
          (industria crypto dh)
          (hashing sha-1)
          (industria ssh public-keys)
          (industria ssh private)
          (industria ssh private serialize))

;;; Messages

  (define SSH-MSG-KEXDH-INIT 30)
  (define SSH-MSG-KEXDH-REPLY 31)

  (define (register-kexdh reg)
    (reg SSH-MSG-KEXDH-INIT parse-kexdh-init put-kexdh-init)
    (reg SSH-MSG-KEXDH-REPLY parse-kexdh-reply put-kexdh-reply))

  (define-record-type kexdh-init
    (parent ssh-packet)
    (fields e)
    (protocol
     (lambda (p)
       (lambda (e)
         ((p SSH-MSG-KEXDH-INIT) e)))))

  (define (parse-kexdh-init b)
    (make-kexdh-init (read-mpint b)))

  (define (put-kexdh-init p m)
    (put-u8 p SSH-MSG-KEXDH-INIT)
    (put-mpint p (kexdh-init-e m)))

  (define-record-type kexdh-reply
    (parent ssh-packet)
    (fields host-key f signature)
    (protocol
     (lambda (p)
       (lambda (host-key f signature)
         ((p SSH-MSG-KEXDH-REPLY) host-key f signature)))))

  (define (parse-kexdh-reply b)
    (let* ((key-bv (read-bytevector b))
           (f (read-mpint b))
           (sig (read-bytevector b)))
      (make-kexdh-reply key-bv f sig)))

  (define (put-kexdh-reply p m)
    (put-u8 p SSH-MSG-KEXDH-REPLY)
    (put-bvstring p (kexdh-reply-host-key m))
    (put-mpint p (kexdh-reply-f m))
    (put-bvstring p (kexdh-reply-signature m)))

;;; Kex exchange logic

  (define (make-kex-dh-key-exchanger kex client? send)
    (let ((kexer (if client? make-client-kexer make-server-kexer)))
      (cond ((string=? kex "diffie-hellman-group1-sha1")
             ;; SHA-1 and Oakley Group 2.
             (kexer modp-group2-p modp-group2-g send))
            ((string=? kex "diffie-hellman-group14-sha1")
             ;; SHA-1 and Oakley Group 14.
             (kexer modp-group14-p modp-group14-g send))
            (else
             (error 'make-kex-dh-key-exchanger
                    "Unknown D-H group" kex)))))

  (define (invalid-state method state)
    (error 'kexdh "Invalid state" method state))

  ;; Client part

  (define (make-client-kexer group-p group-g send)
    (let ((state 'send-kexdh-init)
          (init-data #f))
      (let-values (((x e) (make-dh-secret group-g group-p
                                          (bitwise-length group-p))))
        (lambda (method arg)
          (case method
            ((start)
             (case state
               ((send-kexdh-init wait-version/init)
                (set! state (if init-data
                                'recv-kexdh-reply
                                'wait-version/init))
                (send (make-kexdh-init e)) ; e = g^x mod p
                #f)
               (else (invalid-state method state))))
            ((init)
             (case state
               ((wait-version/init)
                (set! state 'recv-kexdh-reply)
                (set! init-data arg)    ; host-key-algorithm V_C V_S I_C I_S
                #f)
               (else (invalid-state method state))))
            ((packet)
             (case state
               ((recv-kexdh-reply)
                (set! state 'done)
                (let ((key-bv (kexdh-reply-host-key arg))
                      (f (kexdh-reply-f arg)) ; f = g^y mod p
                      (sig (kexdh-reply-signature arg)))
                  (unless (< 1 f (- group-p 1)) (error 'kexdh "Bad kexdh-reply"))
                  (let* ((keyalg (cadr (memq 'host-key-algorithm init-data)))
                         (hostkey (get-ssh-public-key (open-bytevector-input-port key-bv)))
                         (K (expt-mod f x group-p))
                         (H (apply hash-kex-data sha-1 sha-1->bytevector
                                   'K_S (ssh-public-key->bytevector hostkey)
                                   'e e 'f f 'K K init-data)))
                    (unless (eq? 'ok (verify-signature H keyalg hostkey sig))
                      (error 'kexdh "Bad kexdh-reply"))
                    (list hostkey (integer->mpint K) H prf-sha-1))))
               (else (invalid-state method state))))
            (else (invalid-state method state)))))))

  ;; Server part

  (define (make-server-kexer group-p group-g send)
    (let ((state 'recv-kexdh-init)
          (init-data #f)
          (private-key #f))
      (let-values (((y f) (make-dh-secret group-g group-p
                                          (bitwise-length group-p))))
        (lambda (method arg)
          (case method
            ((start)
             (case state
               ((recv-kexdh-init)
                (set! state 'wait-version/init)
                #f)
               (else (invalid-state method state))))
            ((init)
             (case state
               ((wait-version/init)
                (set! state 'recv-kexdh-reply)
                (set! init-data arg)    ; V_C V_S I_C I_S
                #f)
               (else (invalid-state method state))))
            ((private-key) (set! private-key arg)) ;TODO: checks
            ((packet)
             (case state
               ((recv-kexdh-reply)
                (set! state 'done)
                (let ((e (kexdh-init-e arg)))       ; e = g^x mod p
                  (unless (< 1 e (- group-p 1)) (error 'kexdh "Bad kexdh-init"))
                  (let* ((hostkey (private->public private-key))
                         (K (expt-mod e y group-p))
                         (H (apply hash-kex-data sha-1 sha-1->bytevector
                                   'K_S (ssh-public-key->bytevector hostkey)
                                   'e e 'f f 'K K init-data))
                         (keyalg (cadr (memq 'host-key-algorithm init-data)))
                         (sig (make-signature H keyalg private-key)))
                    (send (make-kexdh-reply (ssh-public-key->bytevector hostkey)
                                            f sig)) ; f = g^y mod p
                    (list hostkey (integer->mpint K) H prf-sha-1))))
               (else (invalid-state method state))))
            (else (invalid-state method state))))))))
