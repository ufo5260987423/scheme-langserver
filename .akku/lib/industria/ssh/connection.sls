;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2017, 2019 Göran Weinholt <goran@weinholt.se>

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

;; RFC4254 The Secure Shell (SSH) Connection Protocol.
;; RFC4335 The Secure Shell (SSH) Session Channel Break Extension.

;; When creating a channel both parties specify a window size. When
;; data (or extended data) is sent over the channel you need to
;; subtract (bytevector-length (channel-data-value msg) from the
;; window size, and if the window size is zero then no data should be
;; sent (the peer can just ignore that data). The message
;; channel-window-adjust is used to increase the window size.

(library (industria ssh connection)
  (export register-connection

          make-global-request global-request?
          global-request-type
          global-request-want-reply?

          make-global-request/tcpip-forward
          global-request/tcpip-forward?
          global-request/tcpip-forward-address
          global-request/tcpip-forward-port
          make-global-request/cancel-tcpip-forward
          global-request/cancel-tcpip-forward?
          global-request/cancel-tcpip-forward-address
          global-request/cancel-tcpip-forward-port

          request-success? make-request-success request-success-data
          request-failure? make-request-failure

          channel-open?
          channel-open-type channel-open-sender
          channel-open-initial-window-size channel-open-maximum-packet-size

          channel-open/direct-tcpip? make-channel-open/direct-tcpip
          channel-open/direct-tcpip-connect-address
          channel-open/direct-tcpip-connect-port
          channel-open/direct-tcpip-originator-address
          channel-open/direct-tcpip-originator-port

          channel-open/forwarded-tcpip? make-channel-open/forwarded-tcpip
          channel-open/forwarded-tcpip-connected-address
          channel-open/forwarded-tcpip-connected-port
          channel-open/forwarded-tcpip-originator-address
          channel-open/forwarded-tcpip-originator-port

          channel-open/session? make-channel-open/session

          channel-open/x11? make-channel-open/x11
          channel-open/x11-originator-address
          channel-open/x11-originator-port

          channel-open-failure? make-channel-open-failure
          channel-open-failure-reason-code
          channel-open-failure-description
          channel-open-failure-language

          channel-packet?
          channel-packet-recipient

          channel-open-confirmation? make-channel-open-confirmation
          channel-open-confirmation-sender
          channel-open-confirmation-initial-window-size
          channel-open-confirmation-maximum-packet-size

          channel-data? make-channel-data channel-data-value

          channel-extended-data? make-channel-extended-data
          channel-extended-data-value

          channel-eof? make-channel-eof
          channel-close? make-channel-close
          channel-success? make-channel-success
          channel-failure? make-channel-failure

          channel-request? make-channel-request
          channel-request-type channel-request-want-reply?

          channel-request/break? make-channel-request/break
          channel-request/break-length

          channel-request/env? make-channel-request/env
          channel-request/env-name
          channel-request/env-value

          channel-request/exec? make-channel-request/exec
          channel-request/exec-command

          channel-request/exit-signal? make-channel-request/exit-signal
          channel-request/exit-signal-name
          channel-request/exit-signal-core-dumped?
          channel-request/exit-signal-message
          channel-request/exit-signal-language

          channel-request/exit-status? make-channel-request/exit-status
          channel-request/exit-status-value

          channel-request/pty-req? make-channel-request/pty-req
          channel-request/pty-req-term
          channel-request/pty-req-columns
          channel-request/pty-req-rows
          channel-request/pty-req-width
          channel-request/pty-req-height
          channel-request/pty-req-modes

          channel-request/shell? make-channel-request/shell

          channel-request/signal? make-channel-request/signal
          channel-request/signal-name

          channel-request/subsystem? make-channel-request/subsystem
          channel-request/subsystem-name

          channel-request/window-change? make-channel-request/window-change
          channel-request/window-change-columns
          channel-request/window-change-rows
          channel-request/window-change-width
          channel-request/window-change-height

          channel-request/x11-req? make-channel-request/x11-req
          channel-request/x11-req-single-connection?
          channel-request/x11-req-protocol
          channel-request/x11-req-cookie
          channel-request/x11-req-screen

          channel-request/xon-xoff? make-channel-request/xon-xoff
          channel-request/xon-xoff-client-can-do?

          channel-window-adjust? make-channel-window-adjust
          channel-window-adjust-amount

          terminal-modes->bytevector bytevector->terminal-modes

          ;; Reasons for channel-open-failure
          SSH-OPEN-ADMINISTRATIVELY-PROHIBITED
          SSH-OPEN-CONNECT-FAILED
          SSH-OPEN-UNKNOWN-CHANNEL-TYPE
          SSH-OPEN-RESOURCE-SHORTAGE
          ;; Types channel-extended-data
          SSH-EXTENDED-DATA-STDERR)
  (import (except (rnrs (6)) put-string)
          (srfi :26 cut)
          (industria buffer)
          (industria ssh private)
          (industria ssh private serialize)
          (struct pack))

  ;; Message numbers
  (define SSH-MSG-GLOBAL-REQUEST 80)
  (define SSH-MSG-REQUEST-SUCCESS 81)
  (define SSH-MSG-REQUEST-FAILURE 82)
  (define SSH-MSG-CHANNEL-OPEN 90)
  (define SSH-MSG-CHANNEL-OPEN-CONFIRMATION 91)
  (define SSH-MSG-CHANNEL-OPEN-FAILURE 92)
  (define SSH-MSG-CHANNEL-WINDOW-ADJUST 93)
  (define SSH-MSG-CHANNEL-DATA 94)
  (define SSH-MSG-CHANNEL-EXTENDED-DATA 95)
  (define SSH-MSG-CHANNEL-EOF 96)
  (define SSH-MSG-CHANNEL-CLOSE 97)
  (define SSH-MSG-CHANNEL-REQUEST 98)
  (define SSH-MSG-CHANNEL-SUCCESS 99)
  (define SSH-MSG-CHANNEL-FAILURE 100)

  (define (register-connection reg)
    (reg SSH-MSG-GLOBAL-REQUEST parse-global-request put-global-request)
    (reg SSH-MSG-REQUEST-SUCCESS parse-request-success put-request-success)
    (reg SSH-MSG-REQUEST-FAILURE parse-request-failure put-request-failure)
    (reg SSH-MSG-CHANNEL-OPEN parse-channel-open put-channel-open)
    (reg SSH-MSG-CHANNEL-OPEN-CONFIRMATION parse-channel-open-confirmation put-channel-open-confirmation)
    (reg SSH-MSG-CHANNEL-OPEN-FAILURE parse-channel-open-failure put-channel-open-failure)
    (reg SSH-MSG-CHANNEL-WINDOW-ADJUST parse-channel-window-adjust put-channel-window-adjust)
    (reg SSH-MSG-CHANNEL-DATA parse-channel-data put-channel-data)
    (reg SSH-MSG-CHANNEL-EXTENDED-DATA parse-channel-extended-data put-channel-extended-data)
    (reg SSH-MSG-CHANNEL-EOF parse-channel-eof put-channel-eof)
    (reg SSH-MSG-CHANNEL-CLOSE parse-channel-close put-channel-close)
    (reg SSH-MSG-CHANNEL-REQUEST parse-channel-request put-channel-request)
    (reg SSH-MSG-CHANNEL-SUCCESS parse-channel-success put-channel-success)
    (reg SSH-MSG-CHANNEL-FAILURE parse-channel-failure put-channel-failure))

;;; Global requests

  (define-record-type global-request
    (parent ssh-packet)
    (fields type want-reply?)
    (protocol
     (lambda (p)
       (lambda (type want-reply?)
         ((p SSH-MSG-GLOBAL-REQUEST) type want-reply?)))))

  (define-record-type global-request/tcpip-forward
    (parent global-request)
    (fields address port)
    (protocol
     (lambda (p)
       (lambda (want-reply? address port)
         ((p "tcpip-forward" want-reply?) address port)))))

  (define-record-type global-request/cancel-tcpip-forward
    (parent global-request)
    (fields address port)
    (protocol
     (lambda (p)
       (lambda (want-reply? address port)
         ((p "cancel-tcpip-forward" want-reply?) address port)))))

  (define (parse-global-request b)
    (let* ((type (read-string b))
           (want-reply? (positive? (read-byte b))))
      (cond
        ((string=? type "tcpip-forward")
         (let* ((address (read-string b))
                (port (read-uint32 b)))
           (make-global-request/tcpip-forward
            want-reply? address port)))
        ((string=? type "cancel-tcpip-forward")
         (let* ((address (read-string b))
                (port (read-uint32 b)))
           (make-global-request/cancel-tcpip-forward
            want-reply? address port)))
        (else
         (make-global-request type want-reply?)))))

  (define (put-global-request p m)
    (put-u8 p SSH-MSG-GLOBAL-REQUEST)
    (let ((type (global-request-type m)))
      (put-bvstring p type)
      (put-u8 p (if (global-request-want-reply? m) 1 0))
      (cond ((or (string=? type "tcpip-forward")
                 (string=? type "cancel-tcpip-forward"))
             (put-record p m #f '(string uint32))))))

  (define-record-type request-success
    (parent ssh-packet)
    (fields data)
    (protocol
     (lambda (p)
       (lambda (data)
         ((p SSH-MSG-REQUEST-SUCCESS) data)))))

  (define (parse-request-success b)
    (make-channel-success (read-bytevector b)))

  (define (put-request-success p m)
    (put-u8 p SSH-MSG-REQUEST-SUCCESS)
    (put-bytevector p (request-success-data m)))

  (define-record-type request-failure
    (parent ssh-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda ()
         ((p SSH-MSG-REQUEST-FAILURE))))))

  (define (parse-request-failure b)
    (make-request-failure))

  (define (put-request-failure p m)
    (put-u8 p SSH-MSG-REQUEST-FAILURE))

;;; Channel open

  (define-record-type channel-open
    (parent ssh-packet)
    (fields type sender initial-window-size maximum-packet-size)
    (protocol
     (lambda (p)
       (lambda (type sender initial-window-size maximum-packet-size)
         ((p SSH-MSG-CHANNEL-OPEN) type sender initial-window-size maximum-packet-size)))))

  (define (co-protocol type)
    (lambda (p)
      (lambda (sender initial-window-size maximum-packet-size . x)
        (apply (p type sender initial-window-size maximum-packet-size) x))))

  (define-record-type channel-open/direct-tcpip
    (parent channel-open)
    (fields connect-address connect-port
            originator-address originator-port)
    (protocol (co-protocol "direct-tcpip")))

  (define-record-type channel-open/forwarded-tcpip
    (parent channel-open)
    (fields connected-address connected-port
            originator-address originator-port)
    (protocol (co-protocol "forwarded-tcpip")))

  (define-record-type channel-open/session
    (parent channel-open)
    (fields)
    (protocol (co-protocol "session")))

  (define-record-type channel-open/x11
    (parent channel-open)
    (fields originator-address originator-port)
    (protocol (co-protocol "x11")))

  (define (parse-channel-open b)
    (let* ((type (read-string b))
           (sendchn (read-uint32 b))
           (initwnsz (read-uint32 b))
           (maxpktsz (read-uint32 b)))
      (define (parse-tcpip make)
        (let* ((addr1 (read-string b))
               (port1 (read-uint32 b))
               (addr2 (read-string b))
               (port2 (read-uint32 b)))
          (make sendchn initwnsz maxpktsz addr1 port1 addr2 port2)))
      (cond ((string=? type "direct-tcpip")
             (parse-tcpip make-channel-open/direct-tcpip))
            ((string=? type "forwarded-tcpip")
             (parse-tcpip make-channel-open/forwarded-tcpip))
            ((string=? type "session")
             (make-channel-open/session sendchn initwnsz maxpktsz))
            ((string=? type "x11")
             (let ((address (read-string b))
                   (port (read-uint32 b)))
               (make-channel-open/x11 sendchn initwnsz maxpktsz
                                      address port)))
            (else
             (make-channel-open type sendchn initwnsz maxpktsz)))))

  (define (put-channel-open p m)
    (put-u8 p (ssh-packet-type m))
    (put-record p m (record-type-descriptor channel-open)
                '(string uint32 uint32 uint32))
    (let ((type (channel-open-type m)))
      (cond ((string=? type "direct-tcpip")
             (put-record p m #f '(string uint32 string uint32)))
            ((string=? type "forwarded-tcpip")
             (put-record p m #f '(string uint32 string uint32)))
            ((string=? type "session"))
            ((string=? type "x11")
             (put-record p m #f '(string uint32))))))

;;; Packets intended for a specific channel

  (define-record-type channel-packet
    (parent ssh-packet)
    (fields recipient)
    (protocol
     (lambda (p)
       (lambda (type recipient)
         ((p type) recipient)))))

;;; Channel open confirmation

  (define-record-type channel-open-confirmation
    (parent channel-packet)
    (fields sender                      ;sender's channel id
            initial-window-size maximum-packet-size)
    (protocol
     (lambda (p)
       (lambda (recipient . x)
         (apply (p SSH-MSG-CHANNEL-OPEN-CONFIRMATION recipient) x)))))

  (define (parse-channel-open-confirmation b)
    (let* ((r (read-uint32 b))
           (s (read-uint32 b))
           (initwnsz (read-uint32 b))
           (maxpktsz (read-uint32 b)))
      (make-channel-open-confirmation r s initwnsz maxpktsz)))

  (define (put-channel-open-confirmation p m)
    (put-u8 p SSH-MSG-CHANNEL-OPEN-CONFIRMATION)
    (put-bytevector p (pack "!LLLL" (channel-packet-recipient m)
                            (channel-open-confirmation-sender m)
                            (channel-open-confirmation-initial-window-size m)
                            (channel-open-confirmation-maximum-packet-size m))))

;;; Channel open failure

  (define SSH-OPEN-ADMINISTRATIVELY-PROHIBITED 1)
  (define SSH-OPEN-CONNECT-FAILED 2)
  (define SSH-OPEN-UNKNOWN-CHANNEL-TYPE 3)
  (define SSH-OPEN-RESOURCE-SHORTAGE 4)

  (define-record-type channel-open-failure
    (parent channel-packet)
    (fields reason-code description language)
    (protocol
     (lambda (p)
       (lambda (recipient . x)
         (apply (p SSH-MSG-CHANNEL-OPEN-FAILURE recipient) x)))))

  (define (parse-channel-open-failure b)
    (let* ((r (read-uint32 b))
           (reason (read-uint32 b))
           (description (read-string b))
           (language (read-string b)))
      (make-channel-open-failure r reason description language)))

  (define (put-channel-open-failure p m)
    (put-u8 p SSH-MSG-CHANNEL-OPEN-FAILURE)
    (put-bytevector p (pack "!LL" (channel-packet-recipient m)
                            (channel-open-failure-reason-code m)))
    (put-bvstring p (channel-open-failure-description m))
    (put-bvstring p (channel-open-failure-language m)))

;;; Window adjust

  (define-record-type channel-window-adjust
    (parent channel-packet)
    (fields amount)
    (protocol
     (lambda (p)
       (lambda (recipient amount)
         ((p SSH-MSG-CHANNEL-WINDOW-ADJUST recipient) amount)))))

  (define (parse-channel-window-adjust b)
    (let* ((recipient (read-uint32 b))
           (amount (read-uint32 b)))
      (make-channel-window-adjust recipient amount)))

  (define (put-channel-window-adjust p m)
    (put-u8 p (ssh-packet-type m))
    (put-bytevector p (pack "!LL" (channel-packet-recipient m)
                            (channel-window-adjust-amount m))))

;;; Channel data

  ;; TODO: most packets will be channel-data, so it would be nice to
  ;; have a special case that makes handling these more efficient.

  (define-record-type channel-data
    (parent channel-packet)
    (fields value)
    (protocol
     (lambda (p)
       (lambda (recipient value)
         ((p SSH-MSG-CHANNEL-DATA recipient) value)))))

  (define (parse-channel-data b)
    (let* ((recipient (read-uint32 b))
           (data (read-bytevector b)))
      (make-channel-data recipient data)))

  (define (put-channel-data p m)
    (put-u8 p SSH-MSG-CHANNEL-DATA)
    (put-bytevector p (pack "!L" (channel-packet-recipient m)))
    (put-bvstring p (channel-data-value m)))

;;; Extended channel data

  (define SSH-EXTENDED-DATA-STDERR 1)

  (define-record-type channel-extended-data
    (parent channel-packet)
    (fields type value)
    (protocol
     (lambda (p)
       (lambda (recipient type value)
         ((p SSH-MSG-CHANNEL-EXTENDED-DATA recipient) type value)))))

  (define (parse-channel-extended-data b)
    (let* ((recipient (read-uint32 b))
           (type (read-uint32 b))
           (data (read-bytevector b)))
      (make-channel-extended-data recipient type data)))

  (define (put-channel-extended-data p m)
    (put-u8 p SSH-MSG-CHANNEL-EXTENDED-DATA)
    (put-bytevector p (pack "!LL" (channel-packet-recipient m)
                            (channel-extended-data-type m)))
    (put-bvstring p (channel-extended-data-value m)))

;;; Channel end of file

  (define-record-type channel-eof
    (parent channel-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda (recipient)
         ((p SSH-MSG-CHANNEL-EOF recipient))))))

  (define (parse-channel-eof b)
    (make-channel-eof (read-uint32 b)))

  (define (put-channel-eof p m)
    (put-u8 p SSH-MSG-CHANNEL-EOF)
    (put-bytevector p (pack "!L" (channel-packet-recipient m))))

;;; Channel close

  (define-record-type channel-close
    (parent channel-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda (recipient)
         ((p SSH-MSG-CHANNEL-CLOSE recipient))))))

  (define (parse-channel-close b)
    (make-channel-close (read-uint32 b)))

  (define (put-channel-close p m)
    (put-u8 p SSH-MSG-CHANNEL-CLOSE)
    (put-bytevector p (pack "!L" (channel-packet-recipient m))))

;;; Channel request

  (define-record-type channel-request
    (parent channel-packet)
    (fields type want-reply?)
    (protocol
     (lambda (p)
       (lambda (recipient type want-reply?)
         ((p SSH-MSG-CHANNEL-REQUEST recipient) type want-reply?)))))

  (define (cr-protocol type)
    (lambda (p)
      (lambda (recipient want-reply? . x)
        (apply (p recipient type want-reply?) x))))

  (define (cr-protocol/no-reply type)
    (lambda (p)
      (lambda (recipient . x)
        (apply (p recipient type #f) x))))

  (define cr-types '())

  (define-syntax define-request-type
    (lambda (x)
      (define (symappend prefix name)
        (datum->syntax name
                       (string->symbol
                        (string-append prefix (symbol->string (syntax->datum name))))))
      (define (make-parser name fields types wr)
        (with-syntax ((make (symappend "make-channel-request/" name))
                      ((field ...) fields)
                      ((type ...) types))
          (if wr
              #'(lambda (recipient want-reply? b)
                  (get-record b (lambda (field ...)
                                  (make recipient want-reply? field ...))
                              '(type ...)))
              #'(lambda (recipient _ b)
                  (get-record b (lambda (field ...)
                                  (make recipient field ...))
                              '(type ...))))))
      (syntax-case x (fields want-reply? has-want-reply)
        ((_ name
            (fields (boolean want-reply?) (type field) ...))
         #`(define-request-type name
            (fields (type field) ...)
            (has-want-reply #t)
            (protocol (cr-protocol #,(symbol->string (syntax->datum #'name))))))
        ((_ name
            (fields (type field) ...))
         #`(define-request-type name
            (fields (type field) ...)
            (has-want-reply #f)
            (protocol (cr-protocol/no-reply #,(symbol->string (syntax->datum #'name))))))
        ((_ name
            (fields (type field) ...)
            (has-want-reply wr?)
            (protocol prot))
         (let ((wr (syntax->datum #'wr?)))
           (with-syntax ((rec-name (symappend "channel-request/" #'name))
                         (parser (make-parser #'name #'(field ...) #'(type ...) wr))
                         (formatter #'(lambda (p m)
                                        (put-record p m #f '(type ...)))))
             #`(begin
                 (define-record-type rec-name
                   (parent channel-request)
                   (fields field ...)
                   (protocol prot))
                 (define dummy
                   (begin
                     (set! cr-types (cons (list #,(symbol->string (syntax->datum #'name))
                                                parser formatter)
                                          cr-types))
                     #f)))))))))

  (define-request-type break
    (fields (boolean want-reply?)
            (uint32 length)))

  (define-request-type env
    (fields (boolean want-reply?)
            (string name)
            (bytevector value)))

  (define-request-type exec
    (fields (boolean want-reply?)
            (bytevector command)))

  (define-request-type exit-signal
    (fields (string name)
            (boolean core-dumped?)
            (string message)
            (string language)))

  (define-request-type exit-status
    (fields (uint32 value)))

  (define-request-type pty-req
    (fields (boolean want-reply?)
            (string term)
            (uint32 columns)
            (uint32 rows)
            (uint32 width)
            (uint32 height)
            (bytevector modes)))

  (define-request-type shell
    (fields (boolean want-reply?)))

  (define-request-type signal
    (fields (string name)))

  (define-request-type subsystem
    (fields (boolean want-reply?)
            (string name)))

  (define-request-type window-change
    (fields (uint32 columns)
            (uint32 rows)
            (uint32 width)
            (uint32 height)))

  (define-request-type x11-req
    (fields (boolean want-reply?)
            (boolean single-connection?)
            (string protocol)
            (string cookie)
            (uint32 screen)))

  (define-request-type xon-xoff
    (fields (boolean client-can-do?)))

  (define (parse-channel-request b)
    (let* ((recipient (read-uint32 b))
           (type (read-string b))
           (want-reply? (positive? (read-byte b))))
      (cond ((assoc type cr-types) =>
             (lambda (encoding)
               ((cadr encoding) recipient want-reply? b)))
          (else
           (make-channel-request recipient type want-reply?)))))

  (define (put-channel-request p m)
    (put-u8 p SSH-MSG-CHANNEL-REQUEST)
    (put-bytevector p (pack "!L" (channel-packet-recipient m)))
    (put-bvstring p (channel-request-type m))
    (put-u8 p (if (channel-request-want-reply? m) 1 0))
    (cond ((assoc (channel-request-type m) cr-types) =>
           (lambda (encoding)
             ((caddr encoding) p m)))
          (else
           (error 'put-channel-request
                  "bug: can't encode this message"
                  m))))

;;; Channel success

  (define-record-type channel-success
    (parent channel-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda (recipient)
         ((p SSH-MSG-CHANNEL-SUCCESS recipient))))))

  (define (parse-channel-success b)
    (make-channel-success (read-uint32 b)))

  (define (put-channel-success p m)
    (put-u8 p SSH-MSG-CHANNEL-SUCCESS)
    (put-bytevector p (pack "!L" (channel-packet-recipient m))))

;;; Channel failure

  (define-record-type channel-failure
    (parent channel-packet)
    (fields)
    (protocol
     (lambda (p)
       (lambda (recipient)
         ((p SSH-MSG-CHANNEL-FAILURE recipient))))))

  (define (parse-channel-failure b)
    (make-channel-failure (read-uint32 b)))

  (define (put-channel-failure p m)
    (put-u8 p SSH-MSG-CHANNEL-FAILURE)
    (put-bytevector p (pack "!L" (channel-packet-recipient m))))

;;; Terminal modes

  ;; This encoding is used in the channel-request/pty-req that gets
  ;; sent to the server. RFC4254 has more on this. Modes are
  ;; represented as alists, (mnemonic . value).

  (define mnemonics
    '#(TTY_OP_END
       VINTR VQUIT VERASE VKILL VEOF VEOL VEOL2 VSTART
       VSTOP VSUSP VDSUSP VREPRINT VWERASE VLNEXT VFLUSH VSWTCH
       VSTATUS VDISCARD #f #f #f #f #f #f #f #f #f #f #f IGNPAR
       PARMRK INPCK ISTRIP INLCR IGNCR ICRNL IUCLC IXON IXANY
       IXOFF IMAXBEL #f #f #f #f #f #f #f #f ISIG ICANON XCASE
       ECHO ECHOE ECHOK ECHONL NOFLSH TOSTOP IEXTEN ECHOCTL
       ECHOKE PENDIN #f #f #f #f #f #f #f OPOST OLCUC ONLCR OCRNL
       ONOCR ONLRET #f #f #f #f #f #f #f #f #f #f #f #f #f #f CS7
       CS8 PARENB PARODD #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f TTY_OP_ISPEED TTY_OP_OSPEED #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f))

  (define (bytevector->terminal-modes bv)
    (call-with-port (open-bytevector-input-port bv)
      (lambda (p)
        (let lp ((ret '()))
          (if (or (port-eof? p)
                  (zero? (lookahead-u8 p))
                  (<= 160 (lookahead-u8 p) 255))
              (reverse ret)
              (let* ((opcode (get-u8 p))
                     (value (get-unpack p "!L")))
                (lp (cons (cons (or (vector-ref mnemonics opcode) opcode)
                                value)
                          ret))))))))

  (define (warning who msg . irritants)
    (raise (condition
            (make-warning)
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irritants))))

  (define terminal-modes->bytevector
    (let ((ht (make-eq-hashtable)))
      (do ((op 0 (+ op 1)))
          ((= op (vector-length mnemonics)))
        (cond ((vector-ref mnemonics op) =>
               (cut hashtable-set! ht <> op))))
      (lambda (modes)
        (call-with-bytevector-output-port
          (lambda (out)
            (do ((modes modes (cdr modes)))
                ((null? modes) (put-u8 out 0))
              (cond ((hashtable-ref ht (caar modes) #f) =>
                     (lambda (opcode)
                       (put-bytevector out (pack "!uCL" opcode
                                                 (cdar modes)))))
                    (else
                     (warning 'terminal-modes->bytevector
                              "Unknown terminal mode mnemonic (try an integer instead?)"
                              (caar modes)))))))))))
