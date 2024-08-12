;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Procedures that work with SSH private keys

(library (industria ssh private-keys)
  (export
    get-ssh-private-keys
    openssh-private-key?
    openssh-private-key-public
    openssh-private-key-private
    openssh-private-key-comment)
  (import
    (except (rnrs (6)) put-string)
    (industria base64)
    (industria bytevectors)
    (industria crypto dsa)
    (industria crypto ec)
    (industria crypto ecdsa)
    (industria crypto eddsa)
    (industria crypto rsa)
    (industria ssh public-keys)
    (industria ssh private serialize))

(define (get-ssh-private-keys p)
  (let-values (((type data) (get-delimited-base64 p)))
    (cond ((string=? type "DSA PRIVATE KEY")
           (list (dsa-private-key-from-bytevector data)))
          ((string=? type "RSA PRIVATE KEY")
           (list (rsa-private-key-from-bytevector data)))
          ((string=? type "EC PRIVATE KEY")
           (list (ecdsa-sha-2-private-key-from-bytevector data)))
          ((string=? type "OPENSSH PRIVATE KEY")
           (openssh-private-keys-from-bytevector data))
          (else
           (error 'get-ssh-private-keys "Unsupported key type" type p)))))

(define-record-type openssh-private-key
  (fields public private comment))

;; Read an OpenSSH private key file. Needed for Ed25519 keys. OpenSSH
;; private keys are documented in the OpenSSH source distribution, in
;; PROTOCOL.key, but with some handwaving.
(define (openssh-private-keys-from-bytevector data)
  (define p (open-bytevector-input-port data))
  (define auth-magic "openssh-key-v1\x0;")
  (define who 'openssh-private-key-from-bytevector)
  (let ((magic (get-bytevector-n p (string-length auth-magic))))
    (unless (equal? magic (string->utf8 auth-magic))
      (error who "Bad magic in key data" magic)))
  (let* ((ciphername (get-string p))
         (kdfname (get-string p))
         (kdfoptions (get-bytevector p))
         (num-keys (get-uint32 p)))
    (unless (and (equal? ciphername "none")
                 (equal? kdfname "none")
                 (equal? kdfoptions #vu8()))
      (error who "Unimplemented algorithm"
             ciphername kdfname kdfoptions))
    (let* ((public-keys
            (do ((i 0 (fx+ i 1))
                 (keys '()
                       (cons (get-ssh-public-key
                              (open-bytevector-input-port
                               (get-bytevector p)))
                             keys)))
                ((fx=? i num-keys) keys)))
           ;; TODO: Get a password and decrypt the data.
           (enc-private-keys (get-bytevector p))
           (ep (open-bytevector-input-port enc-private-keys)))
      (let* ((checkint0 (get-uint32 ep))
             (checkint1 (get-uint32 ep)))
        (unless (and (number? checkint0) (= checkint0 checkint1))
          (error who "Bad checkint values" checkint0 checkint1)))
      (let lp ((i 0) (keys '()) (comments '()))
        (cond
          ((fx=? i num-keys)
           (bytevector-fill! enc-private-keys 0)
           (reverse (map make-openssh-private-key
                         public-keys keys comments)))
          (else
           (let* ((keyalg (get-string ep))
                  (key
                   (case (string->symbol keyalg)
                     ((ssh-dss)
                      (let* ((p (get-mpint ep))
                             (q (get-mpint ep))
                             (g (get-mpint ep))
                             (pub (get-mpint ep))
                             (priv (get-mpint ep)))
                        (make-dsa-private-key p q g pub priv)))
                     ((ssh-rsa)
                      (let* ((n (get-mpint ep))
                             (e (get-mpint ep))
                             (d (get-mpint ep))
                             (_iqmp (get-mpint ep))
                             (p (get-mpint ep))
                             (q (get-mpint ep)))
                        (make-rsa-private-key n e d p q)))
                     ((ecdsa-sha2-nistp256
                       ecdsa-sha2-nistp384
                       ecdsa-sha2-nistp521)
                      (let* ((curve-name (get-string ep))
                             (Q (get-bytevector ep))
                             (d (get-mpint ep)))
                        (let-values ([(alg curve)
                                      (case (string->symbol keyalg)
                                        ((ecdsa-sha2-nistp256)
                                         (values "nistp256" nistp256))
                                        ((ecdsa-sha2-nistp384)
                                         (values "nistp384" nistp384))
                                        (else
                                         (values "nistp521" nistp521)))])
                          (unless (equal? curve-name alg)
                            (error who "Mismatching curve" keyalg curve-name))
                          (make-ecdsa-sha-2-private-key curve d Q))))
                     ((ssh-ed25519)
                      (let* ((_public (get-bytevector ep))
                             (private (get-bytevector ep)))
                        (make-ed25519-private-key (subbytevector private 0 32))))
                     (else
                      (error who "Unsupported algorithm" keyalg))))
                  (comment (get-string ep)))
             (lp (fx+ i 1)
                 (cons key keys)
                 (cons comment comments))))))))))
