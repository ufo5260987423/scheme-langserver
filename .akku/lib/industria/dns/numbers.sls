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

;; Assigned numbers for the DNS. This is basically copied from the
;; IANA registries, with some few changes, e.g. mnemonics are in all
;; capital letters.

;; http://www.iana.org/assignments/dns-parameters

(library (industria dns numbers)
  (export dns-rrtype integer->dns-rrtype
          dns-class integer->dns-class
          dns-opcode
          dns-rcode integer->dns-rcode integer->dns-tsig-error
          dnssec-algorithm integer->dnssec-algorithm
          dnssec-digest
          dns-cert-type integer->dns-cert-type

          dns-sshfp-algorithm dns-sshfp-type

          flag-QR flag-AA flag-TC flag-RD flag-RA
          flag-Z flag-AD flag-CD
          flag-mask
          flag-response flag-authoritative-answer
          flag-truncated flag-recursion-desired
          flag-recursion-available flag-authentic-data
          flag-checking-disabled
          dnskey-flag-zone dnskey-flag-revoke dnskey-flag-sep
          edns-flag-DO edns-flag-dnssec-answer-ok)
  (import (rnrs))

  ;; This creates a syntax that can be used as (name mnemonic) to look
  ;; up the corresponding mnemonic at compile time, or as (name) to
  ;; return all mnemonics as an alist.
  (define-syntax define-mnemonics
    (lambda (x)
      (define (symappend name suffix)
        (datum->syntax name
                       (string->symbol
                        (string-append (symbol->string (syntax->datum name))
                                       suffix))))
      (syntax-case x ()
        ((_ name . rest)
         (identifier? #'name)
         (let lp ((rest #'rest) (acc '()))
           (with-syntax ((acc acc))
             (syntax-case rest ()
               ((mnemonic number . rest)
                (identifier? #'mnemonic)
                (lp #'rest #'((mnemonic number) . acc)))
               (()
                ;; XXX: naming the dummy manually to work around a bug
                ;; in Guile 2.0.1 (likely bug #31472)
                (with-syntax ((dummy (symappend #'name "-alist")))
                  (syntax-case #'acc ()
                    (((mnemonic number) ...)
                     #'(begin
                         (define dummy '((number . mnemonic) ...))
                         (define-syntax name
                           (lambda (x)
                             (syntax-case x (mnemonic ...)
                               ((_ mnemonic) #'number)
                               ...
                               ((_ something)
                                (syntax-violation
                                 #f "This mnemonic is not defined, \
                                     see (industria dns numbers)."
                                 x #'something))
                               ((_) #'dummy))))))))))))))))

  ;; Registry Name: Resource Record (RR) TYPEs
  (define-mnemonics dns-rrtype
  ;; TYPE         Value and meaning                              Reference
  ;; -----------  ---------------------------------------------  ---------
     A            1;a host address                               [RFC1035]
     NS           2;an authoritative name server                 [RFC1035]
     MD           3;a mail destination (Obsolete - use MX)       [RFC1035]
     MF           4;a mail forwarder (Obsolete - use MX)         [RFC1035]
     CNAME        5;the canonical name for an alias              [RFC1035]
     SOA          6;marks the start of a zone of authority       [RFC1035]
     MB           7;a mailbox domain name (EXPERIMENTAL)         [RFC1035]
     MG           8;a mail group member (EXPERIMENTAL)           [RFC1035]
     MR           9;a mail rename domain name (EXPERIMENTAL)     [RFC1035]
     NULL         10;a null RR (EXPERIMENTAL)                    [RFC1035]
     WKS          11;a well known service description            [RFC1035]
     PTR          12;a domain name pointer                       [RFC1035]
     HINFO        13;host information                            [RFC1035]
     MINFO        14;mailbox or mail list information            [RFC1035]
     MX           15;mail exchange                               [RFC1035]
     TXT          16;text strings                                [RFC1035]
     RP           17;for Responsible Person                      [RFC1183]
     AFSDB        18;for AFS Data Base location                  [RFC1183][RFC5864]
     X25          19;for X.25 PSDN address                       [RFC1183]
     ISDN         20;for ISDN address                            [RFC1183]
     RT           21;for Route Through                           [RFC1183]
     NSAP         22;for NSAP address, NSAP style A record       [RFC1706]
     NSAP-PTR     23;for domain name pointer, NSAP style         [RFC1348]
     SIG          24;for security signature                      [RFC4034][RFC3755][RFC2535]
     KEY          25;for security key                            [RFC4034][RFC3755][RFC2535]
     PX           26;X.400 mail mapping information              [RFC2163]
     GPOS         27;Geographical Position                       [RFC1712]
     AAAA         28;IP6 Address                                 [RFC3596]
     LOC          29;Location Information                        [RFC1876]
     NXT          30;Next Domain - OBSOLETE                      [RFC3755][RFC2535]
     EID          31;Endpoint Identifier                         [Patton]
     NIMLOC       32;Nimrod Locator                              [Patton]
     SRV          33;Server Selection                            [RFC2782]
     ATMA         34;ATM Address                                 [ATMDOC]
     NAPTR        35;Naming Authority Pointer                    [RFC2915][RFC2168][RFC3403]
     KX           36;Key Exchanger                               [RFC2230]
     CERT         37;CERT                                        [RFC4398]
     A6           38;A6 (Experimental)                           [RFC3226][RFC2874]
     DNAME        39;DNAME                                       [RFC2672]
     SINK         40;SINK                                        [Eastlake]
     OPT          41;OPT                                         [RFC2671]
     APL          42;APL                                         [RFC3123]
     DS           43;Delegation Signer                           [RFC4034][RFC3658]
     SSHFP        44;SSH Key Fingerprint                         [RFC4255]
     IPSECKEY     45;IPSECKEY                                    [RFC4025]
     RRSIG        46;RRSIG                                       [RFC4034][RFC3755]
     NSEC         47;NSEC                                        [RFC4034][RFC3755]
     DNSKEY       48;DNSKEY                                      [RFC4034][RFC3755]
     DHCID        49;DHCID                                       [RFC4701]
     NSEC3        50;NSEC3                                       [RFC5155]
     NSEC3PARAM   51;NSEC3PARAM                                  [RFC5155]
     TLSA         52;TLSA                                        [RFC6698]
     SMIMEA       53;S/MIME cert association                     [RFC8162]
  ;; Unassigned   54
     HIP          55;Host Identity Protocol                      [RFC5205]
     NINFO        56;NINFO                                       [Reid]
     RKEY         57;RKEY                                        [Reid]
     TALINK       58;Trust Anchor LINK                           [Wijngaards]
     CDS          59;Child DS                                    [RFC7344]
     CDNSKEY      60;DNSKEY(s) the Child wants reflected in DS   [RFC7344]
     OPENPGPKEY   61;OpenPGP Key                                 [RFC7929]
     CSYNC        62;Child-To-Parent Synchronization             [RFC7477]
     ZONEMD       63;message digest for DNS zone                 [draft-wessels-dns-zone-digest]
  ;; Unassigned   64-98
     SPF          99;                                            [RFC4408]
     UINFO        100;                                           [IANA-Reserved]
     UID          101;                                           [IANA-Reserved]
     GID          102;                                           [IANA-Reserved]
     UNSPEC       103;                                           [IANA-Reserved]
     NID          104;                                           [RFC6742]
     L32          105;                                           [RFC6742]
     L64          106;                                           [RFC6742]
     LP           107;                                           [RFC6742]
     EUI48        108;an EUI-48 address                          [RFC7043]
     EUI64        109;an EUI-64 address                          [RFC7043]
  ;; Unassigned   110-248
     TKEY         249;Transaction Key                            [RFC2930]
     TSIG         250;Transaction Signature                      [RFC2845]
     IXFR         251;incremental transfer                       [RFC1995]
     AXFR         252;transfer of an entire zone                 [RFC1035][RFC5936]
     MAILB        253;mailbox-related RRs (MB, MG or MR)         [RFC1035]
     MAILA        254;mail agent RRs (Obsolete - see MX)         [RFC1035]
     *            255;A request for all records                  [RFC1035]
     URI          256;URI                                        [RFC7553]
     CAA          257;Certification Authority Restriction        [RFC6844]
     AVC          258;Application Visibility and Control         [Wolfgang_Riedel]
     DOA          259;Digital Object Architecture                [draft-durand-doa-over-dns]
     AMTRELAY     260;Automatic Multicast Tunneling Relay        [draft-ietf-mboned-driad-amt-discovery]
  ;; Unassigned   261-32767
     TA           32768;  DNSSEC Trust Authorities               [Weiler]           2005-12-13
     DLV          32769;  DNSSEC Lookaside Validation            [RFC4431]
  ;; Unassigned   32770-65279
  ;; Private use  65280-65534
  ;; Reserved     65535
     )

  ;; Registry Name: DNS CLASSes
  (define-mnemonics dns-class
    ;; Name                            Decimal      Reference
    ;; ------------------------------  -----------  ---------
    ;; Reserved                        0           ;[RFC5395]
    #| Internet |# IN                  1           ;[RFC1035]
    ;; Unassigned (was CSNET)          2
    #| Chaos |#    CH                  3           ;[Moon1981]
    #| Hesiod |#   HS                  4           ;[Dyer1987]
    ;; Unassigned                      5-253
    #| QCLASS |#   NONE                254         ;[RFC2136]
    #| QCLASS |#   *                   255         ;[RFC1035]
                   ANY                 255         ;an alias
    ;; Unassigned                      256-65279
    ;; Reserved for Private Use        65280-65534 ;[RFC5395]
    ;; Reserved                        65535       ;[RFC5395]
             )

  ;; Registry Name: DNS Header Flags
  (define flag-QR #b1000000000000000)   ;Query=0/response=1
  (define flag-AA #b0000010000000000)   ;Authoritative answer
  (define flag-TC #b0000001000000000)   ;message was TrunCated
  (define flag-RD #b0000000100000000)   ;Recursion Desired
  (define flag-RA #b0000000010000000)   ;Recursion Available
  (define flag-Z  #b0000000001000000)   ;Reserved, set to zero
  ;; DNS Security Extensions, RFC2535, RFC3655
  (define flag-AD #b0000000000100000)   ;Authentic Data
  (define flag-CD #b0000000000010000)   ;Checking Disabled

  (define flag-mask (fxior flag-QR flag-AA flag-TC flag-RD
                           flag-RA flag-AD flag-CD))

  (define flag-response flag-QR)
  (define flag-authoritative-answer flag-AA)
  (define flag-truncated flag-TC)
  (define flag-recursion-desired flag-RD)
  (define flag-recursion-available flag-RA)
  (define flag-authentic-data flag-AD)
  (define flag-checking-disabled flag-CD)

  ;; Registry Name: EDNS Header Flags (16 bits)
  (define edns-flag-DO #b1000000000000000) ;DNSSEC answer OK [RFC4035][RFC3225]

  (define edns-flag-dnssec-answer-ok edns-flag-DO)

  ;; Registry Name: DNS OpCodes
  (define-mnemonics dns-opcode
;; Name                               OpCode  Reference
;; ---------------------------------  ------  ---------
   QUERY                              0      ;[RFC1035]
   IQUERY #|Inverse Query, Obsolete|# 1      ;[RFC3425]
   STATUS                             2      ;[RFC1035]
;; Unassigned                         3
   NOTIFY                             4      ;[RFC1996]
   UPDATE                             5      ;[RFC2136]
   DSO                                6      ;[RFC8490]
;; Unassigned                         7-15
   )

  ;;; XXX: note the duplicate rcode assignment of 16. One is for the
  ;;; TSIG resource's error field.

  ;; Registry Name: DNS RCODEs
  ;; Mnemonics from the IANA dns-parameters file
  (define-mnemonics dns-rcode
;; Name        Decimal      Description                          Reference
;; ----------  -----------  -----------------------------------  ---------
   NOERROR     0           ;No Error                             [RFC1035]
   FORMERR     1           ;Format Error                         [RFC1035]
   SERVFAIL    2           ;Server Failure                       [RFC1035]
   NXDOMAIN    3           ;Non-Existent Domain                  [RFC1035]
   NOTIMP      4           ;Not Implemented                      [RFC1035]
   REFUSED     5           ;Query Refused                        [RFC1035]
   YXDOMAIN    6           ;Name Exists when it should not       [RFC2136]
   YXRRSET     7           ;RR Set Exists when it should not     [RFC2136]
   NXRRSET     8           ;RR Set that should exist does not    [RFC2136]
   NOTAUTH     9           ;Server Not Authoritative for zone    [RFC2136]
   NOTZONE     10          ;Name not contained in zone           [RFC2136]
   DSOTYPENI   11          ;DSO-TYPE Not Implemented             [RFC8490]
;; Unassigned  12-15
   BADVERS     16          ;Bad OPT Version                      [RFC2671]
   BADSIG      16          ;TSIG Signature Failure               [RFC2845]
   BADKEY      17          ;Key not recognized                   [RFC2845]
   BADTIME     18          ;Signature out of time window         [RFC2845]
   BADMODE     19          ;Bad TKEY Mode                        [RFC2930]
   BADNAME     20          ;Duplicate key name                   [RFC2930]
   BADALG      21          ;Algorithm not supported              [RFC2930]
   BADTRUNC    22          ;Bad Truncation                       [RFC4635]
   BADCOOKIE   23          ;Bad/missing Server Cookie            [RFC7873]
;; Unassigned  23-3840
;; Private Use 3841-4095                                         [RFC5395]
;; Unassigned  4096-65534
;; Reserved    65535                                             [RFC5395]
   )

  ;; http://www.iana.org/assignments/dns-sec-alg-numbers/dns-sec-alg-numbers.txt
  ;; DNS Security Algorithm Numbers
  (define-mnemonics dnssec-algorithm

   ;; Mnemonic         Number           Description         Zone   Trans.                                Reference
   ;;                                                     Signing  Sec.
   DELETE               0   ; Delete DS                      N      N    [RFC4034][RFC4398][RFC8078]
   RSAMD5               1   ; RSA/MD5 (deprecated, see 5)    N      Y    [RFC3110][RFC4034]
   DH                   2   ; Diffie-Hellman                 N      Y    [RFC2539][proposed standard]
   ;;                                                                    [RFC3755][proposed standard][RFC2536][proposed standard]
   DSA                  3   ; DSA/SHA1                       Y      Y
   RSASHA1              5   ; RSA/SHA-1                      Y      Y    [RFC3110][RFC4034]
   DSA-NSEC3-SHA1       6   ; DSA-NSEC3-SHA1                 Y      Y    [RFC5155][proposed standard]
   RSASHA1-NSEC3-SHA1   7   ; RSASHA1-NSEC3-SHA1             Y      Y    [RFC5155][proposed standard]
   RSASHA256            8   ; RSA/SHA-256                    Y      *    [RFC5702][proposed standard]
   RSASHA512           10   ; RSA/SHA-512                    Y      *    [RFC5702][proposed standard]
   ECC-GOST            12   ; GOST R 34.10-2001              Y      *    [RFC5933][standards track]
   ECDSAP256SHA256     13   ; ECDSA Curve P-256 with SHA-256 Y      *    [RFC6605][standards track]
   ECDSAP384SHA384     14   ; ECDSA Curve P-384 with SHA-384 Y      *    [RFC6605][standards track]
   ED25519             15   ; Ed25519                        Y      *    [RFC8080][standards track]
   ED448               16   ; Ed448                          Y      *    [RFC8080][standards track]
   INDIRECT            252  ; Reserved for Indirect Keys     N      N    [RFC4034][proposed standard]
   PRIVATEDNS          253  ; private algorithm              Y      Y    [RFC4034]
   PRIVATEOID          254  ; private algorithm OID          Y      Y    [RFC4034]
   )

  ;; http://www.iana.org/assignments/ds-rr-types/ds-rr-types.txt
  ;; Digest Algorithms
  (define-mnemonics dnssec-digest
     SHA-1 1
     SHA-256 2
     GOST 3
     SHA-384 4)

  ;; http://www.iana.org/assignments/dnskey-flags/dnskey-flags.txt
  ;; DNSKEY RR Flags. Bits are counted from the wrong direction.
  (define dnskey-flag-zone (bitwise-arithmetic-shift-left 1 (- 15 7))) ;[RFC3755][RFC4034]
  (define dnskey-flag-revoke (bitwise-arithmetic-shift-left 1 (- 15 8))) ;[RFC5011]
  (define dnskey-flag-sep (bitwise-arithmetic-shift-left 1 (- 15 15))) ;[RFC3757][RFC4034]

  ;; http://www.iana.org/assignments/cert-rr-types/cert-rr-types.txt
  ;; CERT RR Certificate Types
  (define-mnemonics dns-cert-type
  ;; Type      Decimal                 Meaning               Reference
  ;;               0      Reserved                            [RFC4398]
     PKIX          1     ;X.509 as per PKIX                   [RFC4398]
     SPKI          2     ;SPKI certificate                    [RFC4398]
     PGP           3     ;OpenPGP packet                      [RFC4398]
     IPKIX         4     ;The URL of an X.509 data object     [RFC4398]
     ISPKI         5     ;The URL of an SPKI certificate      [RFC4398]
     IPGP          6     ;The URL of an OpenPGP packet        [RFC4398]
     ACPKIX        7     ;Attribute Certificate               [RFC4398]
     IACPKIX       8     ;The URL of an Attribute Certificate [RFC4398]
              ;; 9-252    Unassigned
     URI          253    ;URI private                         [RFC4398]
     OID          254    ;OID private                         [RFC4398]
           ;;     255     Reserved                            [RFC4398]
           ;;  256-65279  Unassigned
           ;; 65280-65534 Experimental                        [RFC4398]
           ;;    65535    Reserved                            [RFC4398]
     )

  ;; http://www.iana.org/assignments/dns-sshfp-rr-parameters/dns-sshfp-rr-parameters.txt
  ;; SSHFP RR Types for public key algorithms
  (define-mnemonics dns-sshfp-algorithm
   ;; Description Value Reference
   ;; Reserved      0   [RFC4255]
      RSA           1  ;[RFC4255]
      DSA           2  ;[RFC4255]
      ECDSA         3  ;[RFC6594]
      Ed25519       4  ;[RFC7479]
      )

  ;; SSHFP RR types for fingerprint types
  (define-mnemonics dns-sshfp-type
   ;; Description Value Reference
   ;; Reserved      0   [RFC4255]
      SHA-1         1  ;[RFC4255]
      SHA-256       2  ;[RFC6594]
      )

  ;; TODO: check if this needs to be faster
  (define (looker-upper alist prefix)
    (lambda (x)
      (cond ((assv x alist) => cdr)
            (else (string->symbol
                   (string-append prefix (number->string x)))))))

  (define integer->dns-rrtype
    (looker-upper (dns-rrtype) "TYPE"))

  (define integer->dns-class
    (looker-upper (dns-class) "CLASS"))

  (define integer->dnssec-algorithm
    (looker-upper (dnssec-algorithm) ""))

  (define integer->dns-cert-type
    (looker-upper (dns-cert-type) ""))

  (define integer->dns-rcode
    (looker-upper (dns-rcode) "RESERVED"))

  (define integer->dns-tsig-error
    (looker-upper (reverse (dns-rcode)) "RESERVED")))
