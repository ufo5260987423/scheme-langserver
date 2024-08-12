;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2019 Göran Weinholt <goran@weinholt.se>

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

;; RFC4419 Diffie-Hellman Group Exchange for the Secure Shell (SSH)
;; Transport Layer Protocol.

;; This handles diffie-hellman-group-exchange-sha256 and
;; diffie-hellman-group-exchange-sha1.

(library (industria ssh kex-dh-gex)
  (export register-kex-dh-gex
          select-kex-dh-gex-group       ;parameter

          make-kex-dh-gex-request-old kex-dh-gex-request-old?
          kex-dh-gex-request-old-n

          make-kex-dh-gex-request kex-dh-gex-request?
          kex-dh-gex-request-min kex-dh-gex-request-n
          kex-dh-gex-request-max

          make-kex-dh-gex-group kex-dh-gex-group?
          kex-dh-gex-group-p kex-dh-gex-group-g

          make-kex-dh-gex-init kex-dh-gex-init?
          kex-dh-gex-init-e

          make-kex-dh-gex-reply kex-dh-gex-reply?
          kex-dh-gex-reply-host-key kex-dh-gex-reply-f
          kex-dh-gex-reply-signature

          make-kex-dh-gex-key-exchanger)
  (import (except (rnrs (6)) put-string)
          (srfi :39 parameters)
          (industria bytevectors)

          (industria crypto dh)

          (industria crypto entropy)
          (industria crypto math)
          (hashing sha-1)
          (hashing sha-2)

          (industria ssh public-keys)

          (industria ssh private)
          (industria ssh private serialize))

  ;; Used by the server part to pick a D-H group. These should really
  ;; be custom generated groups. In OpenSSH they distribute a long
  ;; list of "moduli". In the RFC they suggest that groups could be
  ;; generated in the background.
  (define select-kex-dh-gex-group
    (make-parameter
     (lambda (min n max)
       (if (> n 2048)
           (values modp-group15-p modp-group15-g)
           (values modp-group14-p modp-group14-g)))))

  (define SSH-MSG-KEX-DH-GEX-REQUEST-OLD 30)
  (define SSH-MSG-KEX-DH-GEX-REQUEST 34)
  (define SSH-MSG-KEX-DH-GEX-GROUP 31)
  (define SSH-MSG-KEX-DH-GEX-INIT 32)
  (define SSH-MSG-KEX-DH-GEX-REPLY 33)

  (define (register-kex-dh-gex reg)
    (reg SSH-MSG-KEX-DH-GEX-REQUEST-OLD
         parse-kex-dh-gex-request-old
         put-kex-dh-gex-request-old)
    (reg SSH-MSG-KEX-DH-GEX-REQUEST parse-kex-dh-gex-request put-kex-dh-gex-request)
    (reg SSH-MSG-KEX-DH-GEX-GROUP parse-kex-dh-gex-group put-kex-dh-gex-group)
    (reg SSH-MSG-KEX-DH-GEX-INIT parse-kex-dh-gex-init put-kex-dh-gex-init)
    (reg SSH-MSG-KEX-DH-GEX-REPLY parse-kex-dh-gex-reply put-kex-dh-gex-reply))

  ;; Used by e.g. putty in Debian 5.0.
  (define-record-type kex-dh-gex-request-old
    (parent ssh-packet)
    (fields n)
    (protocol
     (lambda (p)
       (lambda (n)
         ((p SSH-MSG-KEX-DH-GEX-REQUEST-OLD) n)))))

  (define (parse-kex-dh-gex-request-old b)
    (make-kex-dh-gex-request-old (read-uint32 b)))

  (define (put-kex-dh-gex-request-old p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(uint32)))

  (define-record-type kex-dh-gex-request
    (parent ssh-packet)
    (fields min n max)
    (protocol
     (lambda (p)
       (lambda (min n max)
         ((p SSH-MSG-KEX-DH-GEX-REQUEST) min n max)))))

  (define (parse-kex-dh-gex-request b)
    (let* ((min (read-uint32 b))
           (n (read-uint32 b))
           (max (read-uint32 b)))
      (make-kex-dh-gex-request min n max)))

  (define (put-kex-dh-gex-request p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(uint32 uint32 uint32)))

  (define-record-type kex-dh-gex-group
    (parent ssh-packet)
    (fields p g)
    (protocol
     (lambda (p)
       (lambda (p* g)
         ((p SSH-MSG-KEX-DH-GEX-GROUP) p* g)))))

  (define (parse-kex-dh-gex-group b)
    (let* ((p (read-mpint b))
           (g (read-mpint b)))
      (make-kex-dh-gex-group p g)))

  (define (put-kex-dh-gex-group p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(mpint mpint)))

  (define-record-type kex-dh-gex-init
    (parent ssh-packet)
    (fields e)
    (protocol
     (lambda (p)
       (lambda (e)
         ((p SSH-MSG-KEX-DH-GEX-INIT) e)))))

  (define (parse-kex-dh-gex-init b)
    (make-kex-dh-gex-init (read-mpint b)))

  (define (put-kex-dh-gex-init p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(mpint)))

  (define-record-type kex-dh-gex-reply
    (parent ssh-packet)
    (fields host-key f signature)
    (protocol
     (lambda (p)
       (lambda (host-key f signature)
         ((p SSH-MSG-KEX-DH-GEX-REPLY) host-key f signature)))))

  (define (parse-kex-dh-gex-reply b)
    (let* ((host-key (read-bytevector b))
           (f (read-mpint b))
           (signature (read-bytevector b)))
      (make-kex-dh-gex-reply host-key f signature)))

  (define (put-kex-dh-gex-reply p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(string mpint string)))

;;; Key exchange logic

  (define (make-kex-dh-gex-key-exchanger kex client? send)
    (let ((kexer (if client? make-client-kexer make-server-kexer)))
      (cond ((string=? kex "diffie-hellman-group-exchange-sha256")
             (kexer sha-256 sha-256->bytevector prf-sha-256 send))
            ((string=? kex "diffie-hellman-group-exchange-sha1")
             (kexer sha-1 sha-1->bytevector prf-sha-1 send))
            (else
             (error 'make-kex-dh-gex-key-exchanger
                    "Unknown key exchange algorithm" kex)))))

  (define (invalid-state method state)
    (error 'kexdh "Invalid state" method state))

  (define (make-secret g p)
    (let ((q (/ (- p 1) 2)))
      (let lp ()
        ;; Maybe this can be too slow. The other way is to generate a
        ;; bunch of bits too many and do mod q.
        (let* ((x (bytevector->uint (make-random-bytevector
                                     (div (+ (bitwise-length q) 7) 8))))
               (e (expt-mod g x p)))
          (if (and (< 1 x q) (< 1 e (- p 1)))
              (values x e)
              (lp))))))

  (define (make-client-kexer hash hash->bytevector prf send)
    (let ((state 'send-request)
          (init-data #f)
          (prime #f)
          (secret #f))
      ;; Maybe the sizes should be configurable
      (let ((min 1024) (n 2048) (max 8192))
        (lambda (method arg)
          (case method
            ((start)
             (case state
               ((send-request wait-version/init)
                (set! state (if init-data
                                'recv-group
                                'wait-version/init))
                (send (make-kex-dh-gex-request min n max))
                #f)
               (else (invalid-state method state))))
            ((init)
             (case state
               ((wait-version/init)
                (set! state 'recv-group)
                (set! init-data arg) ; host-key-algorithm V_C V_S I_C I_S
                #f)
               (else (invalid-state method state))))
            ((packet)
             (case state
               ((recv-group)
                (set! state 'recv-reply)
                (let ((p (kex-dh-gex-group-p arg))
                      (g (kex-dh-gex-group-g arg)))
                  (let-values (((x e) (make-secret g p)))
                    (set! init-data `(e ,e p ,p g ,g ,@init-data))
                    (set! secret x)
                    (set! prime p)
                    (send (make-kex-dh-gex-init e))
                    #f)))
               ((recv-reply)
                (set! state 'done)
                (let ((key-bv (kex-dh-gex-reply-host-key arg))
                      (f (kex-dh-gex-reply-f arg)) ; f = g^y mod p
                      (sig (kex-dh-gex-reply-signature arg)))
                  (unless (< 1 f (- prime 1))
                    (error 'kex-dh-gex "Bad kex-dh-gex-reply"))
                  (let* ((keyalg (cadr (memq 'host-key-algorithm init-data)))
                         (hostkey (get-ssh-public-key
                                   (open-bytevector-input-port key-bv)))
                         (K (expt-mod f secret prime))
                         (H (apply hash-kex-data hash hash->bytevector
                                   'K_S (ssh-public-key->bytevector hostkey)
                                   'min min 'n n 'max max
                                   'f f 'K K init-data)))
                    (unless (and (eq? 'ok (verify-signature H keyalg hostkey sig))
                                 (< 1 K (- prime 1)))
                      (error 'kexdh "Bad kex-dh-gex-reply"))
                    (list hostkey (integer->mpint K) H prf))))
               (else (invalid-state method state))))
            (else (invalid-state method state)))))))

  (define (make-server-kexer hash hash->bytevector prf send)
    (let ((state 'start)
          (init-data #f)
          (private-key #f)
          (prime #f)
          (generator #f))
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
              (set! state 'recv-request)
              #f)
             (else (invalid-state method state))))
          ((packet)
           (case state
             ((recv-request)
              (set! state 'recv-init)
              (let-values
                  (((p g)
                    (if (kex-dh-gex-request? arg)
                        (let ((min (kex-dh-gex-request-min arg))
                              (n (kex-dh-gex-request-n arg))
                              (max (kex-dh-gex-request-max arg)))
                          (set! init-data `(min ,min n ,n max ,max ,@init-data))
                          ((select-kex-dh-gex-group) min n max))
                        (let ((n (kex-dh-gex-request-old-n arg)))
                          (set! init-data `(n ,n ,@init-data))
                          ((select-kex-dh-gex-group) 1024 n 8192)))))
                (set! prime p)
                (set! generator g)
                (send (make-kex-dh-gex-group prime generator))
                #f))
             ((recv-init)
              (set! state 'done)
              (let-values (((e) (kex-dh-gex-init-e arg)) ; e = g^x mod p
                           ((y f) (make-secret generator prime))) ; f = g^y mod p
                (unless (< 1 e (- prime 1))
                  (error 'kex-dh-gex "Bad kex-dh-gex-init"))
                (let* ((hostkey (private->public private-key))
                       (K (expt-mod e y prime))
                       (H (apply hash-kex-data hash hash->bytevector
                                 'K_S (ssh-public-key->bytevector hostkey)
                                 'p prime 'g generator
                                 'e e 'f f 'K K init-data))
                       (keyalg (cadr (memq 'host-key-algorithm init-data)))
                       (sig (make-signature H keyalg private-key)))
                  (send (make-kex-dh-gex-reply (ssh-public-key->bytevector hostkey)
                                               f sig))
                  (unless (< 1 K (- prime 1))
                    (error 'kex-dh-gex "Bad shared secret"))
                  (list hostkey (integer->mpint K) H prf))))
             (else (invalid-state method state))))
          (else (invalid-state method state)))))))


