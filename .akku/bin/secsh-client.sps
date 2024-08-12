#!/usr/bin/env scheme-script
;; Copied by Akku from ".akku/src/industria/bin/secsh-client.sps" !#
#!r6rs
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Semi-interactive demonstration SSH client
;; Copyright © 2010, 2011, 2017 Göran Weinholt <goran@weinholt.se>

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

(import (rnrs (6))
        (only (srfi :13 strings) string-index)
        (srfi :39 parameters)
        (hashing sha-1)
        (industria crypto dsa)
        (industria crypto ecdsa)
        (industria crypto rsa)
        (industria ssh public-keys)
        ;; (industria dns numbers)
        ;; (industria dns types)
        (industria ssh)
        (industria ssh connection)
        (industria ssh transport)
        (industria ssh userauth)
        (industria tcp)
        (industria base64))

(define (print . x) (for-each display x) (newline))

(define (open-connection hostname portname)
  (let-values (((in out) (tcp-connect hostname portname)))
    (print "Connecting to " hostname " port " portname "...")
    (let ((conn (make-ssh-client in out)))
      (print "Running key exchange...")
      (ssh-key-exchange conn)
      conn)))

(define (reexchange conn algorithm)
  ;; Key re-exchange.
  (let ((local
         (parameterize ((preferred-server-host-key-algorithms (list algorithm)))
           (build-kexinit-packet conn))))
    (put-ssh conn local)
    (let lp ()
      (let ((pkt (get-ssh conn)))
        (cond ((kexinit? pkt)
               (ssh-key-re-exchange conn pkt local)
               (lp))
              ((key-exchange-packet? pkt)
               (unless (eq? (process-key-exchange-packet conn pkt) 'finished)
                 (lp)))
              (else
               ;; XXX: pkt was probably something important, and now it's lost
               (lp)))))))

(define (print-key hostname key)
  (print (ssh-public-key-fingerprint key))
  (print (ssh-public-key-random-art key))
  ;; (print key)
  (let ((key-bv (ssh-public-key->bytevector key)))
    (print hostname " "
           (ssh-public-key-algorithm key) " "
           (base64-encode key-bv))
    ;; RFC4255
    #;
    (let ((algo (cond ((rsa-public-key? key) (dns-sshfp-algorithm RSA))
                      ((dsa-public-key? key) (dns-sshfp-algorithm DSA))
                      (else #f))))
      (when algo
        (display "\tSSHFP\t")
        (dns-resource-print (current-output-port) 80
                            (make-dns-resource/SSHFP
                             '() 0 (dns-class IN)
                             algo
                             (dns-sshfp-type SHA-1)
                             (sha-1->bytevector (sha-1 key-bv))))
        (newline)))))

;; Prints all of the server's host keys. It uses one new connection
;; per host key.
(define (scan-keys hostname portname)
  (let ((conn (open-connection hostname portname)))
    (let lp ((conn conn)
             (algos (remove
                     (ssh-public-key-algorithm (ssh-conn-host-key conn))
                     (kexinit-server-host-key-algorithms
                      (ssh-conn-peer-kexinit conn)))))
      (print-key hostname (ssh-conn-host-key conn))
      (close-ssh conn)
      (unless (null? algos)
        (lp (parameterize ((preferred-server-host-key-algorithms (list (car algos))))
              (open-connection hostname portname))
            (cdr algos))))))

;; Prints all of the server's host keys. It uses one connection, but
;; requires userauth.
(define (getkeys hostname conn)
  (for-each (lambda (algo)
              (print "Switching to host key algorithm " algo)
              (reexchange conn algo)
              (print-key hostname (ssh-conn-host-key conn)))
            (kexinit-server-host-key-algorithms
             (ssh-conn-peer-kexinit conn))))



(define (main hostname portname)
  (define username #f)
  (define sessions '())
  (define authenticated #f)
  (define (get-send-id rec-id)
    (let ((s (assv rec-id sessions)))
      (and s (vector-ref (cdr s) 0))))
  (define (get-auth-result conn)
    (let ((x (get-ssh conn)))
      (cond ((userauth-failure? x)
             (print "You may try these authentication methods: "
                    (userauth-failure-can-continue x)))
            ((userauth-success? x)
             (print "You've succesfully authenticated.")
             (let ((r (ssh-conn-registrar conn)))
               (deregister-userauth r)
               (print "You now have access to the SSH connection protocol.")
               (set! authenticated #t)
               (register-connection r)))
            (else
             (error 'print-auth-result "Unexpected message" x)))))
  (print "Industria SSH demo client.\n")
  (let ((conn (open-connection hostname portname)))
    (print-key hostname (ssh-conn-host-key conn))
    (print "Please verify the above key.")
    (print "SSH session established.\nType help for a list of commands.\n")
    (let loop ()
      (display hostname)
      (display "=> ")
      (flush-output-port (current-output-port))
      (guard (exn
              (else
               (print "There was an error: " exn)))
        (case (read)
          ((help)
           (print "Command reference:")
           (print "d                  Toggle protocol debug messages")
           (print "help               This helpful help text")
           (print "q                  Exit the program")
           (print "g                  Get all host keys")
           (print "u \"username\"       Start user authentication")
           (print "p \"password\"       User authentication with a password")
           (print "s                  Open a new session channel")
           (print "t <channel id>     Request a pseudo-terminal and shell")
           (print "w <channel id>     Send a line to a channel")
           (print "r                  Read a packet (might block)"))

          ((d)
           (ssh-debugging (fxxor (ssh-debugging) #b111))
           (print "ssh-debugging set to #b" (number->string #b111 2)))

          ((g)
           (if authenticated
               (getkeys hostname conn)
               (scan-keys hostname portname)))

          ((u)
           (when username
             (error 'main "Once a username has been sent it can't be changed."))
           (set! username (read))
           ;; Ask nicely that we may use the userauth protocol
           (put-ssh conn (make-service-request "ssh-userauth"))
           (let ((x (get-ssh conn)))
             (if (and (service-accept? x)
                      (string=? (service-accept-name x)
                                "ssh-userauth"))
                 (print "Your request to use ssh-userauth was accepted.")
                 (error 'main "Couldn't request ssh-userauth" x)))
           (register-userauth (ssh-conn-registrar conn))
           ;; Ask what authentication methods are available. Also
           ;; indicates that we want to use the ssh-connection
           ;; protocol later.
           (put-ssh conn (make-userauth-request username "ssh-connection" "none"))
           (get-auth-result conn))

          ((p)
           ;; Try password authentication. Passwords are of course
           ;; visible on the terminal.
           (let ((password (read)))
             (put-ssh conn (make-userauth-request/password username "ssh-connection"
                                                           password))
             (get-auth-result conn)))

          ;; Open a new session
          ((s)
           (let ((rec-id (length sessions))
                 (rec-w 4096)
                 (rec-max 32768))
             (put-ssh conn (make-channel-open/session rec-id rec-w rec-max))
             (let ((x (let lp ()
                        (let ((x (get-ssh conn)))
                          (if (channel-open-confirmation? x)
                              x
                              (lp))))))
               (let ((send-id (channel-open-confirmation-sender x))
                     (send-w (channel-open-confirmation-initial-window-size x))
                     (send-max (channel-open-confirmation-maximum-packet-size x)))
                 (set! sessions (cons (cons rec-id
                                            (vector send-id rec-w send-w
                                                    rec-max send-max))
                                      sessions))
                 (print "New session opened.")
                 (print "Receive side parameters:")
                 (print "ID: " rec-id " window size: " rec-w
                        " maximum packet size: " rec-max)
                 (print "Send side parameters:")
                 (print "ID: " send-id " window size: " send-w
                        " maximum packet size: " send-max)))))

          ;; Request a pseudo-terminal and a shell
          ((t)
           ;; These parameters here are mostly useless, because this
           ;; program doesn't put the user's terminal into raw mode.
           (let ((modes
                  (terminal-modes->bytevector
                   '((TTY_OP_OSPEED . 38400) (TTY_OP_ISPEED . 38400) (VINTR . 3)
                     (VQUIT . 28) (VERASE . 127) (VKILL . 21) (VEOF . 4)
                     (VEOL . 0) (VEOL2 . 0) (VSTART . 17) (VSTOP . 19)
                     (VSUSP . 26) (VREPRINT . 18) (VWERASE . 23) (VLNEXT . 22)
                     (VDISCARD . 15) (IGNPAR . 1) (PARMRK . 0) (INPCK . 0)
                     (ISTRIP . 0) (INLCR . 0) (IGNCR . 0) (ICRNL . 1)
                     (IUCLC . 0) (IXON . 1) (IXANY . 0) (IXOFF . 0)
                     (IMAXBEL . 1) (ISIG . 1) (ICANON . 1) (XCASE . 0)
                     (ECHO . 1) (ECHOE . 1) (ECHOK . 1) (ECHONL . 0)
                     (NOFLSH . 0) (TOSTOP . 0) (IEXTEN . 1) (ECHOCTL . 1)
                     (ECHOKE . 1) (PENDIN . 0) (OPOST . 1) (OLCUC . 0)
                     (ONLCR . 1) (OCRNL . 0) (ONOCR . 0) (ONLRET . 0) (CS7 . 1)
                     (CS8 . 1) (PARENB . 0) (PARODD . 0)))))
             (cond ((assv (read) sessions) =>
                    (lambda (s)
                      (let ((send-id (vector-ref (cdr s) 0)))
                        (put-ssh conn (make-channel-request/pty-req
                                       send-id #f "vt100" 24 80 1032 325 modes))
                        (put-ssh conn (make-channel-request/shell send-id #f))
                        (flush-ssh-output conn))))
                   (else
                    (print "No such session. Use the receive ID.")))))

          ((w)
           (cond ((get-send-id (read)) =>
                  (lambda (send-id)
                    (print "Type a line:")
                    (get-line (current-input-port))
                    (let lp ((line (string->utf8
                                    (string-append (get-line (current-input-port))
                                                   "\n"))))
                      (put-ssh conn (make-channel-data send-id line))
                      (flush-ssh-output conn))))
                 (else
                  (print "No such session. Use the receive ID."))))

          ((r)
           (let ((x (get-ssh conn)))
             (cond ((channel-data? x)
                    (display (utf8->string (channel-data-value x)))
                    (newline)
                    ;; This is super-naive. These adjustments should
                    ;; be based on some real buffer size and should
                    ;; not be sent this often.
                    (let ((send-id (get-send-id (channel-packet-recipient x))))
                      (put-ssh conn (make-channel-window-adjust send-id
                                                                (bytevector-length
                                                                 (channel-data-value x))))))
                   (else
                    (print x)))))

          ((q) (exit 0))

          (else
           (print "Unknown command."))))

      (loop))))

(apply
 (case-lambda
   ((who hostname portname)
    (main hostname portname))
   ((who hostname)
    (main hostname "22"))
   ((who . _)
    (print "Usage: " who " hostname [port]")
    (exit 1)))
 (command-line))
