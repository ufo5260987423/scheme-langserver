;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2011, 2012, 2018, 2019, 2020 Göran Weinholt <goran@weinholt.se>

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

;; Procedures and data structures for the domain name system (DNS)

;; This is a work in progress.

;; STD0013 Domain name system

;; Current errata: http://www.rfc-editor.org/errata_search.php?rfc=1035

;; labels          63 octets or less
;; names           255 octets or less

;; TODO: handle truncation (see RFC2181)

;; UDP messages can be at most 512 bytes. EDNS0 can extend this to
;; 65536, but this library uses 4096, which is pretty standard and
;; avoids the higher reassembly overhead of large datagrams.

;; RFC 5452 DNS Resilience against Forged Answers

;; This is of interest: http://www.nlnetlabs.nl/downloads/nsd/differences.pdf

;; http://www.ietf.org/dyn/wg/charter/dnsext-charter.html

(library (industria dns)
  (export print-dns-message print-dns-resource
          put-dns-message
          put-dns-message/delimited
          parse-dns-message
          get-bytevector-dns

          dns-labels->string
          dns-labels->ustring
          string->dns-labels

          make-dns-message dns-message?
          dns-message-id dns-message-opcode dns-message-rcode
          dns-message-flags dns-message-question
          dns-message-answer dns-message-authority
          dns-message-additional

          make-dns-question dns-question?
          dns-question-name dns-question-type dns-question-class
          dns-question=?

          make-edns-resource
          make-normal-dns-query
          falsified-dns-reply?)
  (import (rnrs)
          (except (srfi :13 strings) string-copy string->list string-titlecase
                  string-upcase string-downcase string-hash string-for-each)
          (srfi :14 char-sets)
          (srfi :19 time)
          (srfi :26 cut)
          (srfi :39 parameters)
          (industria bytevectors)
          (industria crypto entropy)
          (industria dns numbers)
          (industria dns types)
          (industria dns private)
          (industria dns punycode)
          (struct pack))

  (define (print . x) (for-each display x) (newline))

;;; Types

  (define-record-type dns-message
    (fields id opcode rcode flags
            question                      ;list of questions
            answer authority additional)) ;lists of resources

  (define-record-type dns-question
    (fields name type class))

  (define (dns-question=? x y)
    (and (= (dns-question-class x) (dns-question-class y))
         (= (dns-question-type x) (dns-question-type y))
         (equal? (dns-question-name x) (dns-question-name y))))

;;; DNS message encoding

  (define (put-dns-message* port msg)
    (define label-table
      ;; TODO: case insensitive comparison can be used sometimes
      (make-hashtable (lambda (x) (fold-left + 0 (bytevector->u8-list x)))
                      bytevector=?))
    (define (put-question x)
      (put-labels port (dns-question-name x) label-table 0)
      (put-bytevector port (pack "!SS" (dns-question-type x)
                                 (dns-question-class x))))
    (define (put-resource x)
      (put-labels port (dns-resource-name x) label-table 0)
      (let ((rdata
             (call-with-bytevector-output-port
               (cut dns-resource-wire-write <> label-table
                    (+ (format-size "!SSLS") (port-position port)) x))))
        (put-bytevector port (pack "!SSLS"
                                   (dns-resource-type x)
                                   (dns-resource-class x)
                                   (dns-resource-ttl x)
                                   (bytevector-length rdata)))
        (put-bytevector port rdata)))
    (let ((etc (fxior (fxand (dns-message-flags msg) flag-mask)
                      (fxarithmetic-shift-left
                       (fxand (dns-message-opcode msg) #xf) 11)
                      (fxand (dns-message-rcode msg) #xf))))
      (put-bytevector port (pack "!SSSSSS" (dns-message-id msg) etc
                                 (length (dns-message-question msg))
                                 (length (dns-message-answer msg))
                                 (length (dns-message-authority msg))
                                 (length (dns-message-additional msg))))
      (for-each put-question (dns-message-question msg))
      (for-each put-resource (dns-message-answer msg))
      (for-each put-resource (dns-message-authority msg))
      (for-each put-resource (dns-message-additional msg))))

  (define (put-dns-message port msg)
    (let-values ([(p extract) (open-bytevector-output-port)])
      (put-dns-message* p msg)
      (let ((bv (extract)))
        (put-bytevector port bv))))

  (define (put-dns-message/delimited port msg)
    (let-values ([(p extract) (open-bytevector-output-port)])
      (put-dns-message* p msg)
      (let ((bv (extract)))
        (put-bytevector port (pack "!S" (bytevector-length bv)))
        (put-bytevector port bv))))

;;; DNS message parsing

  ;; This procedure takes a complete DNS message, without any framing,
  ;; and returns a dns-message record.
  (define (parse-dns-message bv)
    (define (questions start)
      (let*-values (((qname end) (parse-labels bv start))
                    ((qtype qclass) (unpack "!uSS" bv end)))
        (values (make-dns-question qname qtype qclass)
                (+ end (format-size "!uSS")))))
    (define (resources start)
      (let*-values (((name end) (parse-labels bv start))
                    ((type class ttl rdlength) (unpack "!uSSLS" bv end)))
        (let* ((rdstart (+ end (format-size "!uSSLS")))
               (resource (dns-resource-wire-read type name
                                                 ttl class
                                                 bv rdstart
                                                 (+ rdstart rdlength))))
          (values resource (+ end (format-size "!uSSLS") rdlength)))))
    (define (get f i start)
      (let lp ((i i) (start start) (ret '()))
        (if (zero? i) (values (reverse ret) start)
            (let-values (((entry end) (f start)))
              (lp (- i 1) end (cons entry ret))))))
    (let-values (((id etc qdcount ancount nscount arcount) (unpack "!6S" bv)))
      (let ((opcode (fxbit-field etc 11 15))
            (rcode (fxbit-field etc 0 4)))
        (let*-values (((qd end-qd) (get questions qdcount
                                        (format-size "!6S")))
                      ((an end-an) (get resources ancount end-qd))
                      ((ns end-ns) (get resources nscount end-an))
                      ((ar end-ar) (get resources arcount end-ns)))
          (make-dns-message id opcode rcode (fxand etc flag-mask)
                            qd an ns ar)))))

  (define (get-bytevector-dns port)
    (get-bytevector-n port (get-unpack port "!S")))

;;; Working in the master zone format

  ;; Converts a list of labels (bytevectors) to the master file
  ;; format, i.e. the string representation of a domain name. There
  ;; will be a trailing dot. Does not handle IDNA. FIXME: should do
  ;; error checking.
  (define (dns-labels->string x)
    (call-with-string-output-port
      (lambda (p)
        (if (null? x)
            (display "." p)
            (for-each (cut display-dns-label <> p) x)))))

  ;; Like dns-label->string, except it converts to a unicode string
  ;; (IDNA). It might be preferable to use this in some applications,
  ;; e.g. programs that deal with the WWW.
  ;;; TODO: not complete, see RFC 5891
  (define (dns-labels->ustring x)
    (define (display-dns-label* l p)
      (cond ((and (> (bytevector-length l) (string-length "xn--"))
                  (equal? (subbytevector l 0 (string-length "xn--"))
                          (string->utf8 "xn--")))
             (display (punycode->string
                       (subbytevector l (string-length "xn--")
                                      (bytevector-length l)))
                      p)
             (display #\. p))
            (else
             (display-dns-label l p))))
    (call-with-string-output-port
      (lambda (p)
        (if (null? x)
            (display "." p)
            (for-each (cut display-dns-label* <> p) x)))))

  ;; Converts a DNS name from the master file format to the internal
  ;; DNS labels format (which is a list of bytevectors). If you're
  ;; parsing a file in the master file format you'll need to handle
  ;; the case where the last character isn't a dot separately (append
  ;; the $ORIGIN). This procedure should also be ok for converting
  ;; user input to DNS labels.
  (define (string->dns-labels str)
    (define who 'string->dns-labels)
    (define (get-label p)
      (define (return codepoints unicode?)
        (if unicode?
            ;;; RFC 5891. TODO: this is not complete, but it's better
            ;;; than inserting utf8 bytes directly in the label.
            ;;; nameprep is missing.
            (bytevector-append (string->utf8 "xn--")
                               (string->punycode
                                (string-normalize-nfc
                                 (string-downcase
                                  (list->string
                                   (map integer->char (reverse codepoints)))))))
            (u8-list->bytevector (reverse codepoints))))
      (let lp ((chars '()) (unicode? #f))
        (case (peek-char p)
          ((#\.)
           (get-char p)                 ;eat #\.
           (return chars unicode?))
          ((#\\)
           (get-char p)                 ;eat #\\
           (let ((c1 (get-char p)))
             (unless (char? c1)
               (error who "There is an invalid escape in a DNS label" str c1))
             (if (char<=? #\0 c1 #\9)
                 (let* ((c2 (get-char p))
                        (c3 (get-char p)))
                   (unless (and (char? c2) (char? c3)
                                (char<=? #\0 c2 #\9)
                                (char<=? #\0 c3 #\9))
                     (error who "There is an invalid numeric escape in a DNS label"
                            str c1 c2 c3))
                   (let ((n (string->number (string c1 c2 c3))))
                     (unless (<= 0 n 255)
                       (error who "There is an invalid numeric escape in a DNS label" str n))
                     (lp (cons n chars) unicode?)))
                 (lp (cons (char->integer c1) chars) unicode?))))
          (else
           (if (port-eof? p)
               (return chars unicode?)
               (let ((c (get-char p)))
                 (lp (cons (char->integer c) chars)
                     (or unicode? (char>? c #\delete)))))))))
    (let ((p (open-string-input-port str)))
      (let lp ((labels #f))
        (if (port-eof? p)
            (if labels
                (if (<= 0 (+ (length labels) (bytevectors-length labels) 1) 255)
                    (reverse labels)
                    (error who "This DNS name is too long" str))
                (error who "Empty DNS names are not allowed (use . to represent the root)" str))
            (let ((label (get-label p)))
              (cond ((equal? label #vu8())
                     (if (or (list? labels) (not (port-eof? p)))
                         (error who "There is an empty DNS label" str)
                         '()))
                    ((not (< 0 (bytevector-length label) 64))
                     (error who "There is an overlong DNS label" str (utf8->string label)))
                    (else
                     (lp (cons label (or labels '()))))))))))

  (define print-dns-resource
    (case-lambda
      ((r) (print-dns-resource r (current-output-port)))
      ((r port)
       (let ((name (dns-resource-name r))
             (class (integer->dns-class (dns-resource-class r)))
             (type (integer->dns-rrtype (dns-resource-type r))))
         (for-each (cut display <> port)
                   (list (dns-labels->string name)
                         "\t" (dns-resource-ttl r) ""
                         "\t" class "\t" type "\t"))
         (dns-resource-print port 40 r)
         (newline port)
         (let ((uname (dns-labels->ustring name)))
           (unless (string=? (dns-labels->string name)
                             uname)
             (display "; (" port)
             (display uname port)
             (display ")\n" port)))))))

  ;; Print a DNS message in the master zone format
  (define print-dns-message
    (case-lambda
      ((x) (print-dns-message x (current-output-port)))
      ((x port)
       (define (print . x) (for-each (cut display <> port) x) (newline port))
       (define (printq q)
         (let ((name (dns-question-name q)))
           (print ";" (dns-labels->string (dns-question-name q))
                  "\t\t" (integer->dns-class (dns-question-class q))
                  "\t" (integer->dns-rrtype (dns-question-type q)))
           (unless (string=? (dns-labels->string name)
                             (dns-labels->ustring name))
             (print "; (" (dns-labels->ustring name) ")"))))
       (define (printo r)
         (print "; " r))
       (define (rcode->msg rcode)
         (cond ((= rcode (dns-rcode NOERROR)) "No error")
               ((= rcode (dns-rcode FORMERR)) "Format error (our fault)")
               ((= rcode (dns-rcode SERVFAIL)) "Server failure (their fault)")
               ((= rcode (dns-rcode NXDOMAIN)) "Name error (no such domain)")
               ((= rcode (dns-rcode NOTIMP)) "Not implemented")
               ((= rcode (dns-rcode REFUSED)) "Refused by the server")
               ((assv rcode (dns-rcode)) => cdr)
               (else rcode)))
       (print ";; id: " (dns-message-id x) " opcode: "
              (assv (dns-message-opcode x) (dns-opcode))
              " rcode: " (rcode->msg (dns-message-rcode x)))
       (display ";; flags:" port)
       (for-each (lambda (flag name)
                   (when (= flag (fxand flag (dns-message-flags x)))
                     (display #\space port)
                     (display name port)))
                 (list flag-QR flag-AA flag-TC flag-RD flag-RA
                       flag-Z flag-AD flag-CD)
                 '("response" "authoritative-answer" "truncated"
                   "recursion-desired" "recursion-available"
                   "reserved-flag" "authentic-data" "checking-disabled"))
       (newline port)
       (print "\n;; Question section")
       (for-each printq (dns-message-question x))
       (print "\n;; Answer section")
       (for-each (cut print-dns-resource <> port) (dns-message-answer x))
       (print "\n;; Authority section")
       (for-each (cut print-dns-resource <> port) (dns-message-authority x))
       (print "\n;; Additional section")
       (let-values (((edns additional)
                     (partition (lambda (r)
                                  (= (dns-resource-type r) (dns-rrtype OPT)))
                                (dns-message-additional x))))
         (for-each (cut print-dns-resource <> port) additional)
         (unless (null? edns)
           (print "\n;; EDNS options")
           (for-each printo edns))))))

;;; Helpers for making messages

  (define (make-edns-resource udp-payload-size extended-rcode version
                              flags options)
    (let ((name '())
          (class udp-payload-size)
          (ttl (bitwise-ior (bitwise-arithmetic-shift-left extended-rcode 24)
                            (bitwise-arithmetic-shift-left version 16)
                            (bitwise-and #xffff flags)))
          (type (dns-rrtype OPT)))
      ;; FIXME: options
      (make-dns-resource/raw name ttl class type #vu8())))

  (define (make-normal-dns-query qname qtype edns?)
    ;; qname is a DNS name
    (make-dns-message (bytevector->uint (make-random-bytevector 2))
                      (dns-opcode QUERY) (dns-rcode NOERROR)
                      (fxior flag-recursion-desired
                             #;flag-checking-disabled) ;DNSSEC?
                      ;; draft-wijngaards-dnsext-resolver-side-mitigation-01
                      (list (make-dns-question (string->dns-labels
                                                (string-random-case qname))
                                               qtype (dns-class IN)))
                      '() '()
                      (if edns?
                          (list (make-edns-resource 4096 0 0
                                                    edns-flag-dnssec-answer-ok
                                                    '()))
                          '())))

  ;; Try to guess if the reply is false. False messages can appear if
  ;; an attacker is trying to poison your cache. He'll try to guess
  ;; which id and port number you used in your query and send a reply
  ;; before the real reply arrives.
  (define (falsified-dns-reply? q r)
    (or (not (= (dns-message-id r) (dns-message-id q)))
        (not (= (dns-message-opcode r) (dns-opcode QUERY)))
        (not (= (length (dns-message-question r)) 1))
        (not (dns-question=? (car (dns-message-question q))
                             (car (dns-message-question r))))
        (zero? (fxand (dns-message-flags r)
                      flag-response)))))
