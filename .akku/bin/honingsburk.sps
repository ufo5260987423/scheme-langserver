#!/usr/bin/env scheme-script
;; Copied by Akku from ".akku/src/industria/bin/honingsburk.sps" !#
#!r6rs
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Example SecSH server
;; Copyright © 2010, 2011, 2018, 2019 Göran Weinholt <goran@weinholt.se>

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

;; Take what you see here with a pinch of salt!

;; TODO: handle window adjustments properly

(import (rnrs (6))
        (srfi :19 time)
        (struct pack)
        (industria bytevectors)
        (industria crypto dsa)
        (industria crypto ecdsa)
        (industria crypto rsa)
        (industria ssh)
        (industria ssh connection)
        (industria ssh private-keys)
        (industria ssh public-keys)
        (industria ssh transport)
        (industria ssh userauth)
        (industria password)
        (industria strings)
        (industria base64)
        (only (ikarus) tcp-server-socket fork
              accept-connection waitpid))

(define (print . x) (for-each display x) (newline))

(define passwd
  '(("root" . "$1$P0j07sTf$aqyWUwdBcvgIYaIwVBzwS/"))) ;toor

(define hostname "darkstar")

(define (test-request x)
  (and (userauth-request/password? x)
       (string=? (userauth-request-service x) "ssh-connection")
       (cond ((assoc (userauth-request-username x) passwd)
              => (lambda (pwline)
                   (if (string=? (cdr pwline)
                                 (crypt (userauth-request/password-value x)
                                        (cdr pwline)))
                       'pwd-ok #f)))
             (else #f))))

;; Incomplete, and I'm not sure if these really are any sort of
;; defaults...
(define (default-terminal-modes)
  '((VINTR . 3) (VERASE . 127) (VKILL . 21) (VEOF . 4)
    (ICRNL . 1) (ONLCR . 1)))

#;((TTY_OP_OSPEED . 38400) (TTY_OP_ISPEED . 38400) (VINTR . 3) (VQUIT . 28)
   (VERASE . 127) (VKILL . 21) (VEOF . 4) (VEOL . 0) (VEOL2 . 0) (VSTART . 17)
   (VSTOP . 19) (VSUSP . 26) (VREPRINT . 18) (VWERASE . 23) (VLNEXT . 22)
   (VDISCARD . 15) (IGNPAR . 1) (PARMRK . 0) (INPCK . 0) (ISTRIP . 0) (INLCR . 0)
   (IGNCR . 0) (ICRNL . 1) (IUCLC . 0) (IXON . 1) (IXANY . 0) (IXOFF . 0)
   (IMAXBEL . 1) (ISIG . 1) (ICANON . 1) (XCASE . 0) (ECHO . 1) (ECHOE . 1)
   (ECHOK . 1) (ECHONL . 0) (NOFLSH . 0) (TOSTOP . 0) (IEXTEN . 1) (ECHOCTL . 1)
   (ECHOKE . 1) (PENDIN . 0) (OPOST . 1) (OLCUC . 0) (ONLCR . 1) (OCRNL . 0)
   (ONOCR . 0) (ONLRET . 0) (CS7 . 1) (CS8 . 1) (PARENB . 0) (PARODD . 0))

(define *pty* '())

(define *pty-line* #vu8())
(define *prompt* "\n# ")

(define (pty? b key)
  (cond ((assq key *pty*) => (lambda (x) (= (cdr x) b)))
        (else #f)))

(define (send conn ID msg)
  (define (fix bv)
    (call-with-bytevector-output-port
      (lambda (p)
        (do ((i 0 (+ i 1)))
            ((= i (bytevector-length bv)))
          (let ((b (bytevector-u8-ref bv i)))
            (cond ((and (= b (char->integer #\linefeed))
                        (pty? 1 'ONLCR))
                   (put-u8 p (char->integer #\return))
                   (put-u8 p b))
                  (else
                   (put-u8 p b))))))))
  (let ((bv (if (bytevector? msg) msg (string->utf8 msg))))
    (put-ssh conn (make-channel-data ID (fix bv)))))

(define (handle-command conn ID line)
  (let ((line (utf8->string line)))
    (print "Handling command:") (write line) (newline)
    (unless (string=? line "")
      ;; It's a realistic Linux simulation!
      (send conn ID (string-append
                     (car (string-split line #\space))
                     ": Stale NFS handle")))))

(define (fixup b)
  (cond ((and (= b (char->integer #\return))
              (pty? 1 'ICRNL))
         (char->integer #\linefeed))
        (else b)))

(define (handle-data conn ID x)
  (let ((bv (channel-data-value x)))
    (do ((i 0 (+ i 1)))
        ((= i (bytevector-length bv)))
      (let ((b (fixup (bytevector-u8-ref bv i))))
        (cond
          ((= b (char->integer #\linefeed))
           (send conn ID "\n")
           (handle-command conn ID *pty-line*)
           (set! *pty-line* #vu8())
           (send conn ID *prompt*))
          ((pty? b 'VINTR)
           (set! *pty-line* #vu8())
           (send conn ID "^C")
           (send conn ID *prompt*))
          ((pty? b 'VERASE)
           (let* ((end (max 0 (- (bytevector-length *pty-line*) 1)))
                  (new (subbytevector *pty-line* 0 end)))
             (if (equal? *pty-line* new)
                 (send conn ID "\a")
                 (send conn ID (pack "CCC" 8 32 8)))
             (set! *pty-line* new)))
          ((and (pty? b 'VEOF) (equal? *pty-line* #vu8()))
           (put-ssh conn (make-channel-data ID "\r\n"))
           (put-ssh conn (make-channel-eof ID))
           (put-ssh conn (make-channel-close ID)))
          ((< b 32))                    ;ignore control characters
          (else
           (set! *pty-line* (bytevector-append *pty-line*
                                               (pack "C" b)))
           (send conn ID (pack "C" b))))))))

(define (server conn)
  (let ((ID #f)
        (userid "root"))
    (ssh-key-exchange conn)
    (let ((service (get-ssh conn)))   ;probably service-request
      (put-ssh conn (make-service-accept "ssh-userauth"))
      (register-userauth (ssh-conn-registrar conn)))
    ;; Wait for a valid password
    (let lp ()
      (let ((x (get-ssh conn)))
        (cond ((not (userauth-request? x))
               (ssh-error conn 'server "Unexpected message" SSH-DISCONNECT-PROTOCOL-ERROR))
              ((eq? 'pwd-ok (test-request x))
               (print "Authorization succeeded.")
               (set! userid (userauth-request-username x))
               (put-ssh conn (make-userauth-success)))
              (else
               (print "Authorization failure.")
               (put-ssh conn (make-userauth-failure '("password") #f))
               (lp)))))
    (deregister-userauth (ssh-conn-registrar conn))
    (register-connection (ssh-conn-registrar conn))
    (let lp ()
      (let ((x (get-ssh conn)))
        (cond ((channel-data? x)
               (handle-data conn ID x)
               (lp))
              ((eof-object? x)
               (display "disconnected!\n"))
              ((key-exchange-packet? x)
               (process-key-exchange-packet conn x)
               (lp))
              ((channel-request/shell? x)
               (when (channel-request-want-reply? x)
                 (put-ssh conn (make-channel-success ID)))
               (set! *prompt* (string-append "\n" hostname ":~# "))
               (send conn ID (string-append
                              "Linux " hostname " 2.6.35.8 #1 Sat Oct 30 10:43:19 CEST 2010 i686\n"
                              "\nWelcome to your new account!\n"
                              "No mail."))
               (send conn ID *prompt*)
               (lp))
              ((channel-request/exec? x)
               (put-ssh conn (make-channel-failure ID))
               (lp))
              ((channel-request/pty-req? x)
               (set! *pty* (append
                            (bytevector->terminal-modes
                             (channel-request/pty-req-modes x))
                            (default-terminal-modes)))
               (when (channel-request-want-reply? x)
                 (put-ssh conn (make-channel-success ID)))
               (lp))
              ((channel-request? x)
               (when (channel-request-want-reply? x)
                 (put-ssh conn (make-channel-success ID)))
               (lp))
              ((channel-open/session? x)
               (cond ((not ID)
                      (set! ID (channel-open-sender x))
                      (put-ssh conn (make-channel-open-confirmation
                                     (channel-open-sender x) 0 32768 32768)))
                     (else
                      (put-ssh conn (make-channel-open-failure
                                     (channel-open-sender x)
                                     SSH-OPEN-ADMINISTRATIVELY-PROHIBITED
                                     "No more channels" ""))))
               (lp))
              ((channel-open? x)
               (put-ssh conn (make-channel-open-failure (channel-open-sender x)
                                                        SSH-OPEN-UNKNOWN-CHANNEL-TYPE
                                                        "Request denied" ""))
               (lp))
              ((global-request? x)
               (when (global-request-want-reply? x)
                 (put-ssh conn
                          (if (string=? (global-request-type x) "no-more-sessions@openssh.com")
                              (make-request-success)
                              (make-request-failure))))
               (lp))
              (else
               (display "ignored\n")
               (lp)))))
    (put-ssh conn (make-disconnect SSH-DISCONNECT-BY-APPLICATION
                                   "Good bye!" ""))
    (close-ssh conn)))

(define (make-new-name pid)
  (call-with-string-output-port
    (lambda (p)
      ;; Just something to make it unique enough
      (display "log." p)
      (display (time-second (current-time)) p)
      (display "-" p)
      (display pid p)
      (display ".txt" p))))

(define (main socket . keys)
  ;; (identification-software-version "OpenSSH_5.1p1")
  ;; (identification-comments "Debian-5")
  (ssh-debugging #b111)
  (print "Waiting for connections...")
  (let lp ((pid 0))
    (waitpid -1 #f #f)                  ;kill old zombies
    (let-values (((i o) (accept-connection socket))
                 ((logname) (make-new-name pid)))
      (print "New connection: " i " logging to " logname)
      (fork (lambda (pid)
              (close-port i)
              (close-port o)
              (lp pid))
            (lambda ()
              (with-output-to-file logname
                (lambda ()
                  (print "Server on " i " and " o)
                  (server (make-ssh-server i o keys)))))))))

(define (get-private-key filename)
  (let* ((keys (call-with-port (open-input-file filename) get-ssh-private-keys))
         (key (car keys)))
    (if (openssh-private-key? key)
        (openssh-private-key-private key)
        key)))

(apply
 (case-lambda
   ((who port key1 . keys)
    (apply main (tcp-server-socket (string->number port))
           (map get-private-key (cons key1 keys))))
   ((who . _)
    (print "Usage: " who " port hostkey.pem")
    (print)
    (print "Hint: generate the key with one of these commands:")
    (print " ssh-keygen -t rsa -f demo -m PEM    # empty password")
    (print " ssh-keygen -t ed25519 -f demo -m PEM    # empty password")
    (print " openssl dsaparam 1024 | openssl gendsa /dev/stdin > demo.pem")
    (print " certtool --dsa --bits 1024 -p > demo.pem")
    (exit 1)))
 (command-line))
