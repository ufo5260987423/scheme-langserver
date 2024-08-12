#!/usr/bin/env scheme-script
;; Copied by Akku from ".akku/src/industria/bin/domain.sps" !#
#!r6rs
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2009, 2010, 2011, 2019 Göran Weinholt <goran@weinholt.se>

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

;; Generally useless program to test the dns library.

(import
  (rnrs)
  (industria dns)
  (industria dns numbers)
  (usocket))

(define (print . x) (for-each display x) (newline))

;; TODO: accept command line arguments

(define (query-and-print host service protocol qname qtype)
  (let ((s (case protocol
             ((udp) (make-udp-client-usocket host service))
             ((tcp) (make-tcp-client-usocket host service)))))
    (let ((i (client-usocket-input-port s))
          (o (client-usocket-output-port s)))
      (let ((q (make-normal-dns-query qname qtype #t)))
        ;; (print ";;; Sending query: ") (print-dns-message q)
        (case protocol
          ((tcp) (put-dns-message/delimited o q))
          ((udp) (put-dns-message o q)))
        (flush-output-port o)
        (let lp ()
          (let ((r (parse-dns-message
                    (case protocol
                      ((tcp) (get-bytevector-dns i))
                      ((udp) (get-bytevector-some i))))))
            (cond ((falsified-dns-reply? q r)
                   ;; Useful for UDP only, really
                   (print ";;; FALSIFIED MESSAGE:")
                   (print-dns-message r)
                   (print ";;; FALSIFIED MESSAGE DISCARDED!")
                   (lp))
                  ((= (dns-message-rcode r) (dns-rcode NOERROR))
                   ;; TODO: restore the case from qname
                   (print ";;; Reply on " protocol " socket " host ":" service)
                   (print-dns-message r)
                   (close-port i) (close-port o))
                  (else
                   (print ";; Reply with an ERROR:")
                   (print-dns-message r)
                   (print "\n\n;;; THERE IS AN ERROR!!!!")
                   (close-port i) (close-port o)))))))))

(query-and-print "1.1.1.1" "53" 'tcp
                 "." (dns-rrtype DNSKEY))
