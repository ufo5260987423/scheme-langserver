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

;; RFC4253 The Secure Shell (SSH) Transport Layer Protocol.

;; This contains only the parsing and formatting of the transport
;; layer messages. All the meat is in (industria ssh).

(library (industria ssh transport)
  (export register-transport

          disconnect? make-disconnect disconnect-code
          disconnect-message disconnect-language

          ignore? make-ignore

          unimplemented? make-unimplemented
          unimplemented-sequence-number

          debug? make-debug debug-always-display?
          debug-message debug-language

          service-request? make-service-request
          service-request-name

          service-accept? make-service-accept
          service-accept-name

          kexinit? put-kexinit
          make-kexinit kexinit-cookie kexinit-kex-algorithms
          kexinit-server-host-key-algorithms
          kexinit-encryption-algorithms-client-to-server
          kexinit-encryption-algorithms-server-to-client
          kexinit-mac-algorithms-client-to-server
          kexinit-mac-algorithms-server-to-client
          kexinit-compression-algorithms-client-to-server
          kexinit-compression-algorithms-server-to-client
          kexinit-languages-client-to-server
          kexinit-languages-server-to-client
          kexinit-first-kex-packet-follows?
          kexinit-reserved

          newkeys? make-newkeys

          SSH-DISCONNECT-HOST-NOT-ALLOWED-TO-CONNECT
          SSH-DISCONNECT-PROTOCOL-ERROR
          SSH-DISCONNECT-KEY-EXCHANGE-FAILED
          SSH-DISCONNECT-RESERVED
          SSH-DISCONNECT-MAC-ERROR
          SSH-DISCONNECT-COMPRESSION-ERROR
          SSH-DISCONNECT-SERVICE-NOT-AVAILABLE
          SSH-DISCONNECT-PROTOCOL-VERSION-NOT-SUPPORTED
          SSH-DISCONNECT-HOST-KEY-NOT-VERIFIABLE
          SSH-DISCONNECT-CONNECTION-LOST
          SSH-DISCONNECT-BY-APPLICATION
          SSH-DISCONNECT-TOO-MANY-CONNECTIONS
          SSH-DISCONNECT-AUTH-CANCELLED-BY-USER
          SSH-DISCONNECT-NO-MORE-AUTH-METHODS-AVAILABLE
          SSH-DISCONNECT-ILLEGAL-USER-NAME)
  (import (except (rnrs (6)) put-string)
          (industria buffer)
          (industria ssh private)
          (industria ssh private serialize)
          (struct pack))

  (define SSH-MSG-DISCONNECT 1)
  (define SSH-MSG-IGNORE 2)
  (define SSH-MSG-UNIMPLEMENTED 3)
  (define SSH-MSG-DEBUG 4)
  (define SSH-MSG-SERVICE-REQUEST 5)
  (define SSH-MSG-SERVICE-ACCEPT 6)
  (define SSH-MSG-KEXINIT 20)
  (define SSH-MSG-NEWKEYS 21)

  (define (register-transport reg)
    (reg SSH-MSG-DISCONNECT parse-disconnect put-disconnect)
    (reg SSH-MSG-IGNORE parse-ignore put-ignore)
    (reg SSH-MSG-UNIMPLEMENTED parse-unimplemented put-unimplemented)
    (reg SSH-MSG-DEBUG parse-debug put-debug)
    (reg SSH-MSG-SERVICE-REQUEST parse-service-request put-service-request)
    (reg SSH-MSG-SERVICE-ACCEPT parse-service-accept put-service-accept)
    (reg SSH-MSG-KEXINIT parse-kexinit put-kexinit)
    (reg SSH-MSG-NEWKEYS parse-newkeys put-newkeys))

;;; Disconnection messages

  ;; Disconnect codes
  (define SSH-DISCONNECT-HOST-NOT-ALLOWED-TO-CONNECT 1)
  (define SSH-DISCONNECT-PROTOCOL-ERROR 2)
  (define SSH-DISCONNECT-KEY-EXCHANGE-FAILED 3)
  (define SSH-DISCONNECT-RESERVED 4)
  (define SSH-DISCONNECT-MAC-ERROR 5)
  (define SSH-DISCONNECT-COMPRESSION-ERROR 6)
  (define SSH-DISCONNECT-SERVICE-NOT-AVAILABLE 7)
  (define SSH-DISCONNECT-PROTOCOL-VERSION-NOT-SUPPORTED 8)
  (define SSH-DISCONNECT-HOST-KEY-NOT-VERIFIABLE 9)
  (define SSH-DISCONNECT-CONNECTION-LOST 10)
  (define SSH-DISCONNECT-BY-APPLICATION 11)
  (define SSH-DISCONNECT-TOO-MANY-CONNECTIONS 12)
  (define SSH-DISCONNECT-AUTH-CANCELLED-BY-USER 13)
  (define SSH-DISCONNECT-NO-MORE-AUTH-METHODS-AVAILABLE 14)
  (define SSH-DISCONNECT-ILLEGAL-USER-NAME 15)

  (define-record-type disconnect
    (parent ssh-packet)
    (fields code message language)
    (protocol
     (lambda (p)
       (lambda (code message language)
         ((p SSH-MSG-DISCONNECT) code message language)))))

  (define (parse-disconnect b)
    (let* ((code (read-uint32 b))
           (msg (read-string b))
           ;; Some implementations don't send the language field
           (lang (if (zero? (buffer-length b)) "" (read-string b))))
      (make-disconnect code msg lang)))

  (define (put-disconnect p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(uint32 string string)))

;;; Ignore

  ;; If these are going to be used a lot then it might be better to
  ;; just record the length and discard the data in them.

  (define-record-type ignore
    (parent ssh-packet)
    (fields data)
    (protocol
     (lambda (p)
       (lambda (data)
         ((p SSH-MSG-IGNORE) data)))))

  (define (parse-ignore b)
    (make-ignore (read-bytevector b)))

  (define (put-ignore p m)
    (put-u8 p (ssh-packet-type m))
    (put-bvstring p (ignore-data m)))

;;; Unimplemented

  (define-record-type unimplemented
    (parent ssh-packet)
    (fields sequence-number)
    (protocol
     (lambda (p)
       (lambda (seq-no)
         ((p SSH-MSG-UNIMPLEMENTED) seq-no)))))

  (define (parse-unimplemented b)
    (make-unimplemented (read-uint32 b)))

  (define (put-unimplemented p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(uint32)))

;;; Debug messages

  (define-record-type debug
    (parent ssh-packet)
    (fields always-display? message language)
    (protocol
     (lambda (p)
       (lambda x
         (apply (p SSH-MSG-DEBUG) x)))))

  (define (parse-debug b)
    (let* ((always-display? (positive? (read-byte b)))
           (message (read-string b))
           (language (read-string b)))
      (make-debug always-display? message language)))

  (define (put-debug p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f '(boolean string string)))

;;; Service requests

  ;; After the key exchange the client uses this message to request a
  ;; service, e.g. ssh-userauth.

  (define-record-type service-request
    (parent ssh-packet)
    (fields name)
    (protocol
     (lambda (p)
       (lambda (name)
         ((p SSH-MSG-SERVICE-REQUEST) name)))))

  (define (parse-service-request b)
    (make-service-request (read-string b)))

  (define (put-service-request p msg)
    (put-u8 p SSH-MSG-SERVICE-REQUEST)
    (put-bvstring p (service-request-name msg)))

  (define-record-type service-accept
    (parent ssh-packet)
    (fields name)
    (protocol
     (lambda (p)
       (lambda (name)
         ((p SSH-MSG-SERVICE-ACCEPT) name)))))

  (define (parse-service-accept b)
    (make-service-accept (read-string b)))

  (define (put-service-accept p msg)
    (put-u8 p SSH-MSG-SERVICE-ACCEPT)
    (put-bvstring p (service-accept-name msg)))

;;; Kex exchange initialization

  (define-record-type kexinit
    (parent ssh-packet)
    (fields cookie kex-algorithms
            server-host-key-algorithms
            encryption-algorithms-client-to-server
            encryption-algorithms-server-to-client
            mac-algorithms-client-to-server
            mac-algorithms-server-to-client
            compression-algorithms-client-to-server
            compression-algorithms-server-to-client
            languages-client-to-server
            languages-server-to-client
            first-kex-packet-follows?
            reserved)
    (protocol
     (lambda (p)
       (lambda x
         (apply (p SSH-MSG-KEXINIT) x)))))

  (define kexinit-fields
    '(cookie
      name-list name-list name-list
      name-list name-list name-list
      name-list name-list name-list
      name-list boolean uint32))

  (define (parse-kexinit b)
    (get-record b make-kexinit kexinit-fields))

  (define (put-kexinit p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m #f kexinit-fields))

;;; Tells the peer to use the new keys

  (define-record-type newkeys
    (parent ssh-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda ()
         ((p SSH-MSG-NEWKEYS))))))

  (define (parse-newkeys b)
    (make-newkeys))

  (define (put-newkeys p m) (put-u8 p (ssh-packet-type m))))
