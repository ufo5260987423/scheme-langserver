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

;; RFC4252 The Secure Shell (SSH) Authentication Protocol.

;; keyboard-interactive:
;; RFC4256 Generic Message Exchange Authentication for the Secure
;; Shell Protocol (SSH)

(library (industria ssh userauth)
  (export register-userauth
          register-userauth-public-key
          register-userauth-password
          deregister-userauth

          ;; generic

          make-userauth-request userauth-request?
          userauth-request-username userauth-request-service
          userauth-request-method

          make-userauth-failure userauth-failure?
          userauth-failure-can-continue userauth-failure-partial?

          make-userauth-success userauth-success?

          make-userauth-banner userauth-banner?
          userauth-banner-message userauth-banner-language

          ;; password

          make-userauth-request/password userauth-request/password?
          userauth-request/password-value

          make-userauth-password-changereq
          userauth-password-changereq?
          userauth-password-changereq-prompt
          userauth-password-changereq-language

          make-userauth-request/password-change userauth-request/password-change?
          userauth-request/password-change-old userauth-request/password-change-new

          ;; public key

          make-userauth-request/public-key-query userauth-request/public-key-query?
          userauth-request/public-key-query-algorithm
          userauth-request/public-key-query-key

          make-userauth-public-key-ok userauth-public-key-ok?
          userauth-public-key-ok-algorithm
          userauth-public-key-ok-key

          make-userauth-request/public-key userauth-request/public-key?
          userauth-request/public-key-algorithm
          userauth-request/public-key-key
          sign-userauth-request/public-key)
  (import (except (rnrs (6)) put-string)
          (industria crypto dsa)
          (industria crypto rsa)
          (industria ssh public-keys)
          (industria buffer)
          (industria ssh private)
          (industria ssh private serialize)
          (struct pack))

  ;; Message numbers
  (define SSH-MSG-USERAUTH-REQUEST 50)
  (define SSH-MSG-USERAUTH-FAILURE 51)
  (define SSH-MSG-USERAUTH-SUCCESS 52)
  (define SSH-MSG-USERAUTH-BANNER 53)

  (define SSH-MSG-USERAUTH-PK-OK 60)

  (define SSH-MSG-USERAUTH-PASSWD-CHANGEREQ 60)

  (define (register-userauth reg)
    (reg SSH-MSG-USERAUTH-REQUEST parse-userauth-request put-userauth-request)
    (reg SSH-MSG-USERAUTH-FAILURE parse-userauth-failure put-userauth-failure)
    (reg SSH-MSG-USERAUTH-SUCCESS parse-userauth-success put-userauth-success)
    (reg SSH-MSG-USERAUTH-BANNER parse-userauth-banner put-userauth-banner))

  (define (register-userauth-public-key reg)
    (reg SSH-MSG-USERAUTH-PK-OK
         parse-userauth-public-key-ok
         put-userauth-public-key-ok))

  (define (register-userauth-password reg)
    (reg SSH-MSG-USERAUTH-PASSWD-CHANGEREQ
         parse-userauth-password-changereq
         put-userauth-password-changereq))

  (define (deregister-userauth reg)
    (do ((type 50 (+ type 1)))
        ((= type 80))
      (reg type #f #f)))

;;; User auth requests

  ;; These are used to get access to a specific service, such as the
  ;; ssh-connection service.

  (define-record-type userauth-request
    (parent ssh-packet)
    (fields username service method)
    (protocol
     (lambda (p)
       (lambda (username service method)
         ((p SSH-MSG-USERAUTH-REQUEST) username service method)))))

  (define-record-type userauth-request/password
    (parent userauth-request)
    (fields value)                      ;the actual password
    (protocol
     (lambda (p)
       (lambda (username service value)
         ((p username service "password") value)))))

  (define-record-type userauth-request/password-change
    (parent userauth-request)
    (fields old new)
    (protocol
     (lambda (p)
       (lambda (username service old new)
         ((p username service "password") old new)))))

  ;; This is the one that proves you have the private key
  (define-record-type userauth-request/public-key
    (parent userauth-request)
    (fields algorithm key sig)
    (protocol
     (lambda (p)
       (case-lambda
         ((username service key)
          ((p username service "publickey")
           (ssh-public-key-algorithm 'make-userauth-request/public-key key)
           key #f))
         ((username service algorithm key sig)
          ((p username service "publickey") algorithm key sig))))))

  ;; This is used to ask the server if the given public key will work
  ;; in a userauth-request/public-key. The server responds with a
  ;; userauth-request/public-key-ok if it will. This might be useful
  ;; if the time for signing and verification is larger than n·RTT
  ;; to the server. Apparently some servers can be made to always say
  ;; yes: <http://permalink.gmane.org/gmane.ietf.secsh/6651>
  (define-record-type userauth-request/public-key-query
    (parent userauth-request)
    (fields algorithm key)
    (protocol
     (lambda (p)
       (case-lambda
         ((username service key)
          ((p username service "publickey")
           (ssh-public-key-algorithm 'make-userauth-request/public-key-query key)
           key))
         ((username service algorithm key)
          ((p username service "publickey") algorithm key))))))

  ;; This gets sent to the server to prove you have the private key.
  (define (sign-userauth-request/public-key msg session-id privkey)
    (make-userauth-request/public-key
     (userauth-request-username msg)
     (userauth-request-service msg)
     (userauth-request/public-key-algorithm msg)
     (userauth-request/public-key-key msg)
     (make-signature (pubkey-blob msg session-id)
                     (userauth-request/public-key-algorithm msg)
                     privkey)))

  ;; The returned bytevector is what you sign to prove your identity
  ;; to the server. XXX: this must be exported, because otherwise an
  ;; external agent can't sign it.
  (define (pubkey-blob msg session-id)
    (call-with-bytevector-output-port
      (lambda (p)
        (put-bvstring p session-id)
        (put-userauth-request p msg))))

  (define (put-userauth-request p m)
    (put-u8 p SSH-MSG-USERAUTH-REQUEST)
    (put-bvstring p (userauth-request-username m))
    (put-bvstring p (userauth-request-service m))
    (put-bvstring p (userauth-request-method m))
    (cond ((userauth-request/password? m)
           (put-u8 p 0)
           (put-bvstring p (userauth-request/password-value m)))
          ((userauth-request/password-change? m)
           (put-u8 p 1)
           (put-bvstring p (userauth-request/password-change-old m))
           (put-bvstring p (userauth-request/password-change-new m)))
          ((userauth-request/public-key? m)
           (put-u8 p 1)
           (put-bvstring p (userauth-request/public-key-algorithm m))
           (put-bvstring p (ssh-public-key->bytevector
                            (userauth-request/public-key-key m)))
           ;; This is conditional so that this code can also be used
           ;; to emit the data that the private key signs.
           (if (userauth-request/public-key-sig m)
               (put-bvstring p (userauth-request/public-key-sig m))))
          ((userauth-request/public-key-query? m)
           (put-u8 p 0)
           (put-bvstring p (userauth-request/public-key-query-algorithm m))
           (put-bvstring p (ssh-public-key->bytevector
                            (userauth-request/public-key-query-key m))))
          (else
           (assert (string=? (userauth-request-method m) "none")))))

  (define (parse-userauth-request b)
    (define (parse-userauth-request/password b username service)
      (let ((bool (read-byte b)))
        (let ((pwd (read-string b)))
          (if (zero? bool)
              (make-userauth-request/password username service pwd)
              (make-userauth-request/password-change username service
                                                     pwd (read-string b))))))
    (define (parse-userauth-request/public-key b username service)
      (let ((bool (read-byte b)))
        (let* ((alg (read-string b))
               (blob (read-bytevector b))
               (key blob))
          (if (zero? bool)
              (make-userauth-request/public-key-query username service
                                                      alg key)
              (make-userauth-request/public-key username service alg key
                                                (read-bytevector b))))))
    (let* ((username (read-string b))
           (service (read-string b))
           (method (read-string b)))
      (cond ((string=? method "password")
             (parse-userauth-request/password b username service))
            ((string=? method "publickey")
             (parse-userauth-request/public-key b username service))
            ;; "hostbased"
            (else
             ;; "None" or unknown method
             (make-userauth-request username service method)))))

;;; Server requests a password change

  (define-record-type userauth-password-changereq
    (parent ssh-packet)
    (fields prompt language)
    (protocol
     (lambda (p)
       (lambda (prompt language)
         ((p SSH-MSG-USERAUTH-PASSWD-CHANGEREQ) prompt language)))))

  (define (parse-userauth-password-changereq b)
    (let* ((prompt (read-string b))
           (language (read-string b)))
      (make-userauth-password-changereq prompt language)))

  (define (put-userauth-password-changereq p m)
    (put-u8 p SSH-MSG-USERAUTH-PASSWD-CHANGEREQ)
    (put-bvstring p (userauth-password-changereq-prompt m))
    (put-bvstring p (userauth-password-changereq-language m)))

;;; The server would accept a signature from the given key

  (define-record-type userauth-public-key-ok
    (parent ssh-packet)
    (fields algorithm key)
    (protocol
     (lambda (p)
       (lambda (algorithm key)
         ((p SSH-MSG-USERAUTH-PK-OK) algorithm key)))))

  (define (parse-userauth-public-key-ok b)
    (let* ((algorithm (read-string b))
           (key (read-bytevector b)))
      (guard (exn
              ((error? exn)
               (make-userauth-public-key-ok algorithm key)))
        (make-userauth-public-key-ok algorithm
                                     (get-ssh-public-key
                                      (open-bytevector-input-port key))))))

  (define (put-userauth-public-key-ok p m)
    (put-u8 p (ssh-packet-type m))
    (put-bvstring p (userauth-public-key-ok-algorithm m))
    (put-bvstring p (userauth-public-key-ok-key m)))

;;; User authentication failure

  (define-record-type userauth-failure
    (parent ssh-packet)
    (fields can-continue            ;authentications that can continue
            partial?)               ;partial success
    (protocol
     (lambda (p)
       (lambda (can-continue partial?)
         ((p SSH-MSG-USERAUTH-FAILURE) can-continue partial?)))))

  (define (parse-userauth-failure b)
    (let ((can-continue (read-name-list b)))
      (make-userauth-failure can-continue (positive? (read-byte b)))))

  (define (put-userauth-failure p m)
    (put-u8 p SSH-MSG-USERAUTH-FAILURE)
    (put-name-list p (userauth-failure-can-continue m))
    (put-u8 p (if (userauth-failure-partial? m) 1 0)))

;;; User authentication success

  (define-record-type userauth-success
    (parent ssh-packet)
    (protocol
     (lambda (p)
       (lambda ()
         ((p SSH-MSG-USERAUTH-SUCCESS))))))

  (define (parse-userauth-success b)
    (make-userauth-success))

  (define (put-userauth-success p m)
    (put-u8 p SSH-MSG-USERAUTH-SUCCESS))

;;; Show a banner to the user

  (define-record-type userauth-banner
    (parent ssh-packet)
    (fields message language)
    (protocol
     (lambda (p)
       (lambda (message language)
         ((p SSH-MSG-USERAUTH-BANNER) message language)))))

  (define (parse-userauth-banner b)
    (let ((msg (read-string b)))
      (make-userauth-banner msg (read-string b))))

  (define (put-userauth-banner p m)
    (put-u8 p SSH-MSG-USERAUTH-BANNER)
    (put-bvstring p (userauth-banner-message m))
    (put-bvstring p (userauth-banner-language m))))
