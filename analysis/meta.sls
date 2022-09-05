(library (scheme-langserver analysis meta)
  (export find-meta)
  (import (rnrs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-meta list-instance)
  (cond
    [(equal? list-instance '(rnrs)) '( &assertion &condition &error &i/o &i/o-decoding
 &i/o-encoding &i/o-file-already-exists
 &i/o-file-does-not-exist &i/o-file-is-read-only
 &i/o-file-protection &i/o-filename &i/o-invalid-position
 &i/o-port &i/o-read &i/o-write &implementation-restriction
 &irritants &lexical &message &no-infinities &no-nans
 &non-continuable &serious &syntax &undefined &violation
 &warning &who * + - ... / => _ abs acos and angle append
 apply asin assert assertion-violation assertion-violation?
 assoc assp assq assv atan begin binary-port? bitwise-and
 bitwise-arithmetic-shift bitwise-arithmetic-shift-left
 bitwise-arithmetic-shift-right bitwise-bit-count
 bitwise-bit-field bitwise-bit-set? bitwise-copy-bit
 bitwise-copy-bit-field bitwise-first-bit-set bitwise-if
 bitwise-ior bitwise-length bitwise-not
 bitwise-reverse-bit-field bitwise-rotate-bit-field
 bitwise-xor boolean=? boolean? bound-identifier=?
 buffer-mode buffer-mode? bytevector->sint-list
 bytevector->string bytevector->u8-list bytevector->uint-list
 bytevector-copy bytevector-copy! bytevector-fill!
 bytevector-ieee-double-native-ref
 bytevector-ieee-double-native-set!
 bytevector-ieee-double-ref bytevector-ieee-double-set!
 bytevector-ieee-single-native-ref
 bytevector-ieee-single-native-set!
 bytevector-ieee-single-ref bytevector-ieee-single-set!
 bytevector-length bytevector-s16-native-ref
 bytevector-s16-native-set! bytevector-s16-ref
 bytevector-s16-set! bytevector-s32-native-ref
 bytevector-s32-native-set! bytevector-s32-ref
 bytevector-s32-set! bytevector-s64-native-ref
 bytevector-s64-native-set! bytevector-s64-ref
 bytevector-s64-set! bytevector-s8-ref bytevector-s8-set!
 bytevector-sint-ref bytevector-sint-set!
 bytevector-u16-native-ref bytevector-u16-native-set!
 bytevector-u16-ref bytevector-u16-set!
 bytevector-u32-native-ref bytevector-u32-native-set!
 bytevector-u32-ref bytevector-u32-set!
 bytevector-u64-native-ref bytevector-u64-native-set!
 bytevector-u64-ref bytevector-u64-set! bytevector-u8-ref
 bytevector-u8-set! bytevector-uint-ref bytevector-uint-set!
 bytevector=? bytevector? caaaar caaadr caaar caadar caaddr
 caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
 call-with-bytevector-output-port
 call-with-current-continuation call-with-port
 call-with-string-output-port call-with-values call/cc car
 case-lambda cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar
 cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling
 char->integer char-alphabetic? char-downcase char-foldcase
 char-general-category char-lower-case? char-numeric?
 char-title-case? char-titlecase char-upcase char-upper-case?
 char-whitespace? char? close-input-port close-output-port
 close-port complex? cond condition condition-accessor
 condition-irritants condition-message condition-predicate
 condition-who condition? cons cons* cos datum->syntax define
 define-condition-type define-enumeration define-record-type
 define-syntax denominator display div div-and-mod div0
 div0-and-mod0 do else endianness enum-set->list
 enum-set-complement enum-set-constructor enum-set-difference
 enum-set-indexer enum-set-intersection enum-set-member?
 enum-set-projection enum-set-subset? enum-set-union
 enum-set-universe enum-set=? eof-object eof-object?
 eol-style eq? equal-hash equal? eqv? error
 error-handling-mode error? even? exact exact-integer-sqrt
 exact? exists exp expt fields file-options filter find
 finite? fixnum->flonum fixnum-width fixnum? fl* fl+ fl- fl/
 fl<=? fl<? fl=? fl>=? fl>? flabs flacos flasin flatan
 flceiling flcos fldenominator fldiv fldiv-and-mod fldiv0
 fldiv0-and-mod0 fleven? flexp flexpt flfinite? flfloor
 flinfinite? flinteger? fllog flmax flmin flmod flmod0 flnan?
 flnegative? flnumerator flodd? flonum? floor flpositive?
 flround flsin flsqrt fltan fltruncate flzero? fold-left
 fold-right for-all for-each free-identifier=? fx*/carry
 fx+/carry fx-/carry fx<=? fx<? fx=? fx>=? fx>? fxand
 fxarithmetic-shift fxarithmetic-shift-left
 fxarithmetic-shift-right fxbit-count fxbit-field fxbit-set?
 fxcopy-bit fxcopy-bit-field fxdiv fxdiv-and-mod fxdiv0
 fxdiv0-and-mod0 fxeven? fxfirst-bit-set fxif fxior fxlength
 fxmax fxmin fxmod fxmod0 fxnegative? fxnot fxodd?
 fxpositive? fxreverse-bit-field fxrotate-bit-field fxxor
 fxzero? gcd generate-temporaries get-bytevector-all
 get-bytevector-n get-bytevector-n! get-bytevector-some
 get-char get-datum get-line get-string-all get-string-n
 get-string-n! get-u8 greatest-fixnum guard hashtable-clear!
 hashtable-contains? hashtable-copy hashtable-delete!
 hashtable-equivalence-function hashtable-hash-function
 hashtable-mutable? hashtable-ref hashtable-set!
 hashtable-size hashtable-update! hashtable?
 i/o-decoding-error? i/o-encoding-error-char
 i/o-encoding-error? i/o-error-filename i/o-error-port
 i/o-error-position i/o-error? i/o-file-already-exists-error?
 i/o-file-does-not-exist-error? i/o-file-is-read-only-error?
 i/o-file-protection-error? i/o-filename-error?
 i/o-invalid-position-error? i/o-port-error? i/o-read-error?
 i/o-write-error? identifier-syntax identifier? if imag-part
 immutable implementation-restriction-violation? inexact
 inexact? infinite? input-port? integer->char integer-valued?
 integer? irritants-condition? lambda latin-1-codec lcm
 least-fixnum length let let* let*-values let-syntax
 let-values letrec letrec* letrec-syntax lexical-violation?
 list list->string list->vector list-ref list-sort list-tail
 list? log lookahead-char lookahead-u8 magnitude
 make-assertion-violation make-bytevector
 make-custom-binary-input-port
 make-custom-binary-input/output-port
 make-custom-binary-output-port
 make-custom-textual-input-port
 make-custom-textual-input/output-port
 make-custom-textual-output-port make-enumeration
 make-eq-hashtable make-eqv-hashtable make-error
 make-hashtable make-i/o-decoding-error
 make-i/o-encoding-error make-i/o-error
 make-i/o-file-already-exists-error
 make-i/o-file-does-not-exist-error
 make-i/o-file-is-read-only-error
 make-i/o-file-protection-error make-i/o-filename-error
 make-i/o-invalid-position-error make-i/o-port-error
 make-i/o-read-error make-i/o-write-error
 make-implementation-restriction-violation
 make-irritants-condition make-lexical-violation
 make-message-condition make-no-infinities-violation
 make-no-nans-violation make-non-continuable-violation
 make-polar make-record-constructor-descriptor
 make-record-type-descriptor make-rectangular
 make-serious-condition make-string make-syntax-violation
 make-transcoder make-undefined-violation
 make-variable-transformer make-vector make-violation
 make-warning make-who-condition map max member memp memq
 memv message-condition? min mod mod0 mutable nan?
 native-endianness native-eol-style native-transcoder
 negative? newline no-infinities-violation?
 no-nans-violation? non-continuable-violation? nongenerative
 not null? number? numerator odd? opaque
 open-bytevector-input-port open-bytevector-output-port
 open-file-input-port open-file-input/output-port
 open-file-output-port open-string-input-port
 open-string-output-port or output-port-buffer-mode
 output-port? pair? parent parent-rtd partition peek-char
 port-eof? port-has-port-position?
 port-has-set-port-position!? port-position port-transcoder
 port? positive? procedure? protocol put-bytevector put-char
 put-datum put-string put-u8 quasiquote quasisyntax quote <
 <= = > >= call-with-input-file call-with-output-file case
 char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char<=?
 char<? char=? char>=? char>? command-line current-error-port
 current-input-port current-output-port delete-file
 dynamic-wind exit file-exists? flush-output-port fx* fx+ fx-
 hashtable-entries hashtable-keys number->string
 open-input-file open-output-file record-constructor record?
 standard-error-port standard-input-port standard-output-port
 string->number string-ci<=? string-ci<? string-ci=?
 string-ci>=? string-ci>? string<=? string<? string=?
 string>=? string>? syntax-rules utf-16-codec
 with-input-from-file with-output-to-file raise
 raise-continuable rational-valued? rational? rationalize
 read read-char real->flonum real-part real-valued? real?
 record-accessor record-constructor-descriptor
 record-field-mutable? record-mutator record-predicate
 record-rtd record-type-descriptor record-type-descriptor?
 record-type-field-names record-type-generative?
 record-type-name record-type-opaque? record-type-parent
 record-type-sealed? record-type-uid remove remp remq remv
 reverse round sealed serious-condition? set!
 set-port-position! simple-conditions sin
 sint-list->bytevector sqrt string string->bytevector
 string->list string->symbol string->utf16 string->utf32
 string->utf8 string-append string-ci-hash string-copy
 string-downcase string-foldcase string-for-each string-hash
 string-length string-normalize-nfc string-normalize-nfd
 string-normalize-nfkc string-normalize-nfkd string-ref
 string-titlecase string-upcase string? substring
 symbol->string symbol-hash symbol=? symbol? syntax
 syntax->datum syntax-case syntax-violation
 syntax-violation-form syntax-violation-subform
 syntax-violation? tan textual-port? transcoded-port
 transcoder-codec transcoder-eol-style
 transcoder-error-handling-mode truncate u8-list->bytevector
 uint-list->bytevector undefined-violation? unless unquote
 unquote-splicing unsyntax unsyntax-splicing utf-8-codec
 utf16->string utf32->string utf8->string values vector
 vector->list vector-fill! vector-for-each vector-length
 vector-map vector-ref vector-set! vector-sort vector-sort!
 vector? violation? warning? when who-condition?
 with-exception-handler with-syntax write write-char zero?)]
    [(equal? list-instance '(scheme)) '(symbol-hashtable-update! r6rs:dynamic-wind trace-define
 \x2D;1+ 
 \x31;+ 
 \x31;- 
 mutable-string? assert textual-port-input-count
 property-list make-parameter compile-library logbit0 logbit1
 make-i/o-file-does-not-exist-error pretty-line-length
 bytevector-s56-ref make-custom-binary-output-port logbit?
 virtual-register ftype-set! fields make-ftype-pointer
 make-source-object port-output-full? abs break-handler fl*
 library-directories fl+ set-binary-port-output-buffer! fl-
 fl/ port-output-index car fl< fl= and fl> cdr
 profile-line-number-color eq? bytevector-s64-ref fx* fx+ ash
 make-assertion-violation fx- gcd fx/ div box
 record-type-field-names flacos fx< fx= cos fx>
 string->symbol compile-whole-program bytevector-u16-set!
 for-each lcm exp map let max disable-interrupts min
 make-i/o-port-error log bytevector-compress string<=? mod
 fldiv0 expt-mod string>=? rec not flatan tan
 critical-section vector-length close-port
 set-textual-port-output-size! flasin binary-port-output-size
 sin make-object-finder engine-return current-expand
 console-input-port textual-port-input-index port-bol?
 &i/o-write filter datum->syntax no-infinities-violation?
 vector-for-each gensym fxvector-ref flpositive?
 bytevector-ieee-double-ref getenv eq-hashtable-delete!
 set-port-input-buffer! standard-error-port port-eof?
 make-input/output-port r6rs:char-ci<? set-port-output-index!
 r6rs:char-ci=? r6rs:char-ci>? who-condition?
 i/o-decoding-error? make-i/o-invalid-position-error
 bytevector-u24-set! flnan? add1 mutable-fxvector? flexpt
 flodd? imag-part reverse! top-level-program &source flmod0
 source-table-delete! &i/o-file-does-not-exist
 date-zone-offset caar foreign-callable-entry-point cfl*
 print-record cfl+ acos bytevector-u16-ref cadr cfl- cfl/
 fold-left compile-program-handler unsyntax-splicing fl<=
 open-process-ports fl<? fl=? fl>= fl>? cfl= port-name cdar
 output-port-buffer-mode logtest condition-irritants cddr
 case display-condition r5rs foreign-entry? fx1+ fx1-
 csv7:record-field-accessor cfl-real-part bytevector-u24-ref
 &lexical set-port-nonblocking! div0 transcoder-eol-style
 bitwise-first-bit-set pretty-one-line-limit foreign-alloc
 make-condition atan remove-hash-table! inexact? box? fx<=
 fx<? asin fx=? fx>= fx>? bytevector-u32-set!
 clear-output-port newline cond assp assq cons assv
 bytevector-u32-ref cosh hashtable-clear!
 keyboard-interrupt-handler find enum-set? string->utf16
 string->utf32 string-append else fxvector-length
 put-hash-table! ieee inexact->exact define-syntax
 i/o-file-does-not-exist-error? eqv? fllp r6rs:syntax-rules
 get-datum/annotations u8-list->bytevector eval flsqrt equal?
 flceiling bytevector-u40-ref print-vector-length let*
 bytevector-s64-native-set! cp0-outer-unroll-limit
 expression-editor implicit-exports nan? error-handling-mode
 exit fxif record-type-equal-procedure r6rs:char-ci<=? expt
 odd? fxcopy-bit port-has-port-nonblocking?? exact?
 r6rs:char-ci>=? transcoder-codec timer-interrupt-handler
 real-valued? iota top-level-value mod0 memp memq memv meta
 hashtable-hash-function syntax-violation-subform
 get-thread-id load datum->syntax-object list format
 bytevector-u40-set! internal-defines-as-letrec* error?
 bytevector-u64-native-ref default-exception-handler
 endianness port-file-descriptor utf-16-codec read except
 errorf set! bitwise-arithmetic-shift-right case-sensitive
 lambda substring-fill! quotient enum-set-complement
 violation? annotation? remp remq remv only make-guardian
 tanh undefined-variable-warnings box-cas! &error
 pretty-print path-extension buffer-mode? procedure? sinh
 i/o-error? bytevector sub1 binary-port? time
 unregister-guardian nonnegative? flinteger?
 set-top-level-value! date-zone-name foreign-entry
 syntax->l    available-references-for make-identifier-reference
    identifier-reference-documentfxdiv0 source-object-column file-position
 open-output-string bytevector-ieee-double-set! unget-u8
 with-source-path expand exists define-top-level-value
 append! collect-rendezvous path-last current-date
 make-irritants-condition define-record-type char-numeric?
 display-statistics library-list open-bytevector-output-port
 string-normalize-nfc string-normalize-nfd export
 file-symbolic-link? fxodd? bytevector-u48-ref length
 make-custom-textual-input-port raise-continuable fxmod0
 annotation-stripped current-eval fasl-file
 set-sstats-gc-count! import path-rest set-virtual-register!
 put-string-some parameterize bytevector-u56-ref
 ftype-init-lock! cost-center-time immutable r6rs:char<=?
 r6rs:char>=? port-output-size expand-output
 collect-trip-bytes fxbit-field member path-root call/1cc
 unread-char bytevector-u64-ref char-ci<? char-ci=? char-ci>?
 collect-request-handler locked-object? null-environment
 cost-center-allocation-count i/o-encoding-error-char
 rationalize create-exception-state scheme-program letrec
 make-boot-file sstats-bytes fxbit-count
 open-file-input/output-port time-difference! merge!
 weak-cons string-ci<=? record-writer binary-port-input-size
 string-ci>=? hashtable-contains? trace-case-lambda
 vector-fill! trace-lambda fasl-read open-fd-input-port
 r6rs:call-with-output-file strip-fasl-file hashtable?
 write-char current-time record-case
 generate-inspector-information bytevector-s8-set!
 fluid-let-syntax apropos petite? interactive?
 source-file-descriptor? hashtable-equivalence-function
 r6rs:current-input-port ftype-spin-lock! let*-values
 open-input-output-file oblist bytevector-uint-set!
 open-fd-input/output-port subtract-duration
 hash-table-for-each char-title-case? set-sstats-gc-bytes!
 last-pair caaar caadr fxlength &i/o-read cadar logand caddr
 collections abort r6rs:call-with-input-file
 condition-predicate string-ci-hash fl<=? char-name
 enum-set-universe acosh fl>=? string->bytevector cdaar
 time-nanosecond cdadr symbol-hashtable-cell &non-continuable
 make-i/o-error compress-format begin cddar fxnegative?
 compile-file-message record? cdddr
 bytevector-ieee-double-native-set! logior &i/o-port
 symbol-hashtable-delete! foreign-address-name
 current-input-port condition-message lognot char-
 csv7:record-field-mutable? textual-port-input-buffer char?
 custom-port-buffer-size fxfirst-bit-set pariah
 make-variable-transformer alias date?
 set-binary-port-input-index! parent delete-file ftype-ref
 real-part hashtable-entries put-datum path-absolute? debug
 logxor bytevector-u32-native-set! angle open-source-file
 r6rs:file-exists? vector->immutable-vector define-structure
 delay flnonpositive? fxarithmetic-shift
 bytevector->uint-list datum fxvector->list enable-interrupts
 chmod console-output-port library-extensions
 call-with-current-continuation mutex-acquire
 bytevector->s8-list put-char real->flonum module
 source-condition? fx<=? modulo atanh immutable-bytevector?
 source-directories real-time profile-dump-data fx>=? collect
 ephemeron-cons call-with-port break display
 port-has-port-length? make-no-nans-violation apply asinh
 let-syntax csv7:record-type-symbol time-utc->date
 directory-separator atom? make-record-type
 symbol-hashtable-set! compile-whole-library cons*
 get-bytevector-some! date-nanosecond bitwise-bit-set?
 rational-valued? string-copy! r5rs-syntax string-fill! assoc
 with-implicit top-level-mutable? source-object-bfp
 quasisyntax flabs string-length bytevector-s32-native-ref
 sstats-gc-real ratnum? char-downcase iconv-codec
 char-upper-case? input-port-ready? source-object-efp 
 fxlogand interaction-environment sstats-print random
 gensym->unique-string fldiv flcos bytevector-length
 input-port? <= => >= r6rs:standard-input-port
 subtract-duration! object-counts compile flexp
 current-memory-bytes fxlogior mark-port-closed! flmax
 string-ref profile-dump-html pretty-format &continuation
 flmin fllog source-object-sfd flmod identifier?
 current-directory pretty-maximum-lines profile-dump-list
 gensym-count fxlognot make-serious-condition
 binary-port-output-buffer fltan floor
 enable-cross-library-optimization
 make-custom-binary-input/output-port syntax-object->datum
 flsin irritants-condition? optimize-level
 library-search-handler i/o-file-already-exists-error? even?
 call-with-input-file fxlogbit0 make-lexical-violation
 fxlogbit1 fxvector-copy annotation-options force cd
 fxlogbit? port-input-size do bitwise-bit-field fxlogxor if
 exact top-level-syntax compile-to-file bytevector-copy error
 substring or scheme generate-procedure-source-information
 sealed source-table-ref record-accessor copy-environment
 opaque list-head default-record-equal-procedure fxabs rename
 bitwise-bit-count open-file-input-port
 make-i/o-file-is-read-only-error list-copy fxand
 library-version guard bytevector-u48-set! fxmodulo
 cost-center? fxdiv remove! open-string-output-port
 implementation-restriction-violation?
 port-has-set-port-position!? debug-level flnumerator
 register-signal-handler fxior fxmax integer? fxmin
 initial-bytes-allocated remove fxmod compile-library-handler
 define-property make-i/o-filename-error record-type-name
 trace-output-port fxnot fxvector-set! remprop
 record-hash-procedure call-with-values fxsll fxsra
 vector-sort! fxsrl merge utf-8-codec command-line-arguments
 make-vector list* read-token bwp-object? fxxor list?
 visit-compiled-from-port div0-and-mod0 pair? mkdir
 set-time-second! collect-notify mutex? bytevector-u56-set!
 remove-foreign-entry library-exports ieee-environment
 eol-style compile-to-port block-read isqrt syntax-violation
 list-tail set-sstats-bytes! environment? transcoded-port
 logor undefined-violation? ftype-guardian prefix
 bitwise-rotate-bit-field binary-port-input-buffer
 make-thread-parameter mutable fxlogtest
 r6rs:with-output-to-file source-table? mutable-bytevector?
 eof-object finite? bytevector-u64-native-set! real?
 list-sort bytevector-u8-set! profile-query-weight
 foreign-callable bytevector-ieee-single-set! raise
 eq-hashtable-ref trace-let put-u8 subset-mode for-all
 date-year-day time-second bytevector-u64-set! gensym?
 make-custom-textual-input/output-port remq! new-cafe
 make-ephemeron-eq-hashtable enum-set-indexer remv!
 scheme-version printf char-foldcase load-library
 make-i/o-encoding-error i/o-error-filename reverse magnitude
 number? condition-name compile-imported-libraries
 procedure-arity-mask flfloor reset time<? enum-set-union
 time=? time>? null? fresh-line set-port-input-index! ormap
 fleven? guardian? current-exception-state revisit
 fxdiv0-and-mod0 eval-when &implementation-restriction port?
 with-input-from-string bytevector->immutable-bytevector
 threaded? r6rs:hashtable-entries rational? values
 hashtable-ephemeron? with-output-to-string
 make-i/o-write-error i/o-file-is-read-only-error? sleep
 time? bound-identifier=? utf-16le-codec sc-expand
 foreign-sizeof source-file-descriptor-path condition-wait
 vector time-difference char->integer integer-valued?
 engine-block bitwise-and infinite? ftype-pointer=?
 make-sstats cp0-effort-limit getprop char-titlecase
 compress-level waiter-prompt-and-read round utf32->string
 date-and-time sort! bytevector->u8-list drop-prefix trace
 record-constructor-descriptor? syntax-violation-form
 vector-cas! get-char quote cpu-time port-nonblocking?
 make-string bitwise-ior record-field-mutable? unbox
 eq-hashtable-ephemeron? commonization-level date-dst?
 eval-syntax-expanders-when putenv subst enum-set=?
 bitwise-not string-truncate! r6rs:delete-file
 verify-loadability textual-port-input-size condition
 textual-port-output-size make-syntax-violation visit
 type-descriptor hash-table? generate-instruction-counts
 constructor flnegative? make-ephemeron-eqv-hashtable
 get-string-some! string-for-each bitwise-xor fold-right
 date-hour make-input-port condition-accessor string-ci<?
 string-ci=? string-ci>? record-type-hash-procedure
 i/o-filename-error? vector-copy format-condition?
 i/o-write-error? fxquotient boolean? bytevector-s16-set!
 enum-set-projection default-prompt-and-read
 bytevector-s16-native-set! pretty-initial-indent
 immutable-vector? zero? get-line ftype-pointer?
 bitwise-copy-bit transcript-cafe standard-input-port
 list->vector fxarithmetic-shift-right source-object?
 get-mode case-lambda &i/o-decoding make-source-table
 print-precision write subst! sstats-real
 bytevector-ieee-single-native-ref
 csv7:record-field-accessible? command-line record-type-uid
 enum-set-subset? make-undefined-violation greatest-fixnum
 r6rs:record? fxvector set-binary-port-input-size! fixnum?
 flonum? bytevector-uncompress substq flush-output-port
 substv string-copy file-regular? string-hash latin-1-codec
 bytevector-s24-set! date-year vector-set!
 compile-time-value? unless source-table-cell flfinite?
 directory-list port-output-buffer cfl-conjugate fltruncate
 transcript-off make-who-condition r6rs:utf-16-codec
 concatenate-object-files print-extended-identifiers
 positive? file-change-time hashtable-update! process
 &violation sstats-gc-cpu make-weak-eq-hashtable random-seed
 string<? string=? vector-sort time<=? string>?
 fxvector->immutable-fxvector set-sstats-gc-real!
 compile-script time>=? make-continuation-condition string
 locate-source-object-source condition-continuation
 syntax->datum record-mutator cost-center-instruction-count
 source-table-dump revisit-compiled-from-port profile
 r6rs:case char-ready? put-bytevector make-i/o-decoding-error
 bytevector-s32-set! top-level-syntax?
 make-custom-binary-input-port string-set! fx*/carry
 syntax-error exit-handler fldenominator
 current-locate-source-object-source gensym-prefix flround
 environment-symbols enum-set-member? i/o-read-error?
 maximum-memory-bytes fluid-let make-boot-header
 letrec-syntax textual-port-output-count
 r6rs:standard-output-port generate-covin-files record-rtd
 scheme-start cfl-magnitude-squared source-object-line
 set-time-nanosecond! default-record-hash-procedure
 profile-clear binary-port-input-count
 set-binary-port-output-size! unquote-splicing
 bitwise-copy-bit-field set-sstats-cpu! &i/o-encoding
 r6rs:eval source-table-set! syntax-case
 make-non-continuable-violation
 port-has-set-port-nonblocking!? set-timer fxnonnegative?
 csv7:record-type-field-decls fasl-compressed r6rs:exit
 bytevector-u16-native-ref import-only bytevector-s40-set!
 with-output-to-file truncate-file open-input-string
 load-shared-object &i/o-filename base-exception-handler
 file-buffer-size ftype-lock! source-table-size
 console-error-port file-options dynamic-wind
 port-has-port-position? bitwise-reverse-bit-field
 symbol-hashtable-contains? hashtable-cell syntax remainder
 hashtable-values fixnum-width flzero? &i/o-invalid-position
 source-condition-form textual-port-output-index list->string
 open-output-file immutable-fxvector? condition-who
 record-type-parent binary-port-input-index meta-cond
 ftype-locked-decr! hashtable-copy make-eq-hashtable run-cp0
 open-bytevector-input-port protocol system
 bytevector-sint-ref i/o-file-protection-error?
 flonum->fixnum fixnum->flonum syntax->annotation
 number->string thread? with-syntax r6rs:string-ci<=?
 uint-list->bytevector record-type-descriptor
 r6rs:string-ci>=? enable-object-counts hashtable-keys
 transcript-on condition? bitwise-if simple-conditions
 collect-generation-radix transcoder? truncate-port
 symbol-hashtable? put-string vector? sstats-cpu
 set-port-bol! make-fxvector maybe-compile-program
 bytevector-sint-set! trace-define-syntax
 profile-clear-database date-minute foreign-free
 hashtable-set! vector-map string-upcase enumerate
 r6rs:number->string fx+/carry set-port-eof!
 make-record-type-descriptor make-custom-textual-output-port
 vector-ref waiter-prompt-string open-file-output-port
 interpret fprintf standard-output-port r6rs:hashtable-keys
 continuation-condition? i/o-encoding-error? set-car!
 generate-temporaries hashtable-size make-output-port
 set-cdr! fxvector-fill! set-port-name! path-first list-ref
 compile-time-value-value add-prefix library-requirements
 hashtable-ref suppress-greeting close-input-port
 profile-load-data environment-mutable? foreign-set!
 print-brackets &serious &format make-eqv-hashtable
 apropos-list set-box! set-port-length! message-condition?
 denominator magnitude-squared csv7:record-type-field-names
 date->time-utc thread-condition? make-source-condition
 enum-set-intersection symbol-hashtable-ref list->fxvector
 bitwise-arithmetic-shift-left bytevector->sint-list
 set-sstats-real! copy-time put-source-table syntax-rules
 non-continuable-violation? date-day port-input-buffer
 bytevector-s64-native-ref ftype-locked-incr!
 r6rs:flush-output-port set-port-position! annotation-source
 directory-separator? invoke-library syntax-violation?
 call-with-output-file extend-syntax r6rs:string-ci<?
 r6rs:string-ci=? r6rs:string-ci>? binary-port-output-count
 eq-hashtable? r6rs:open-input-file fasl-strip-options
 current-transcoder virtual-register-count port-closed?
 bitwise-length import-notify delete-directory date-month
 nonpositive? display-string assertion-violation
 ftype-unlock! print-graph &condition define-top-level-syntax
 with-cost-center bytes-deallocated make-compile-time-value
 textual-port-output-buffer eq-hashtable-cell flnonnegative?
 make-bytevector make-implementation-restriction-violation
 immutable-box? fxdiv-and-mod r6rs:fx* r6rs:fx+ r6rs:fx-
 bytevector=? lock-object source-file-descriptor decode-float
 fxreverse-bit-field output-port? conjugate include
 parent-rtd mutable-vector? native-endianness compile-profile
 port-input-count exact-integer-sqrt warning
 scheme-version-number file-length foreign-procedure
 expand/optimize set-port-output-size!
 binary-port-output-index print-level date-second
 textual-port? cp0-score-limit file-directory? putprop
 ftype-pointer-address compile-program clear-input-port
 &assertion set-textual-port-input-index!
 compile-interpret-simple hashtable-cells
 collect-maximum-generation string-downcase
 make-source-file-descriptor bytevector-u32-native-ref
 i/o-invalid-position-error? i/o-error-port port-input-empty?
 mutex-release make-format-condition r6rs:string<?
 r6rs:string=? r6rs:string>? generate-profile-forms
 condition-signal top-level-bound? bitwise-arithmetic-shift
 get-bytevector-n pretty-file mutable-box?
 r6rs:current-output-port source-file-descriptor-checksum
 define-condition-type truncate char-upcase locate-source
 transcoder-error-handling-mode
 make-i/o-file-already-exists-error make-annotation
 eq-hashtable-set! port-input-index ftype-sizeof inexact
 get-string-n! print-radix path-parent fldiv-and-mod
 csv7:record-field-mutator bytevector-truncate!
 profile-release-counters unsyntax indirect-export
 char-lower-case? * + - / port-transcoder < = >
 port-has-set-port-length!? complex? file-modification-time
 port-handler make-no-infinities-violation open-input-file _
 ftype-&ref r6rs:current-error-port fork-thread symbol=?
 expand/optimize-output eof-object? sint-list->bytevector
 utf-16be-codec immutable-string? char-alphabetic?
 r6rs:open-output-file fxeven? get-bytevector-n! file-port?
 print-gensym default-library-search-handler
 string-normalize-nfkc string-normalize-nfkd sstats-gc-count
 reset-handler char-ci<=? eq-hashtable-update! box-immutable
 char-ci>=? current-make-source-object
 bytevector-u16-native-set! hashtable-delete!
 eq-hashtable-contains? set-binary-port-input-buffer!
 ftype-pointer-ftype buffer-mode bytevector-ieee-single-ref
 set-binary-port-output-index! profile-dump
 put-bytevector-some open-fd-output-port
 make-record-constructor-descriptor predicate scheme-script
 native-eol-style unget-char free-identifier=?
 current-error-port bytevector-ieee-single-native-set!
 get-source-table! least-fixnum condition-broadcast
 inspect/object sstats-gc-bytes no-nans-violation? call/cc
 utf8->string get-datum call-with-string-output-port
 library-requirements-options make-error
 csv7:record-type-name caaaar set-port-input-size!
 bytevector-s48-set! record-constructor-descriptor caaadr
 fx-/carry fxpositive? syntax->vector with-profile-tracker
 fxcopy-bit-field caadar define-ftype caaddr
 debug-on-exception substq! record-type-generative? substv!
 set-textual-port-input-buffer! compute-size cadaar cadadr
 define-record set-textual-port-output-index! time-type
 caddar cadddr mutex-name string->number &no-nans library
 machine-type add-duration! symbol->string
 get-bytevector-some fldiv0-and-mod0 vector->list port-length
 string->immutable-string fxlogor lookahead-u8
 set-port-output-buffer! library-object-filename
 file-access-time record-type-sealed? define-enumeration
 numerator annotation-option-set block-write get-string-n
 cdaaar string-foldcase unlock-object bytevector-s56-set!
 make-date cdaadr record-type-opaque? cdadar cdaddr
 get-process-id record-predicate &irritants cddaar &i/o
 ftype-pointer-null? cddadr r6rs:string->number cdddar
 csv7:record-type-descriptor cddddr utf16->string
 file-exists? r6rs:standard-error-port
 set-textual-port-output-buffer! make-cost-center
 bytevector-copy! bytevector-fill! maybe-compile-file
 cfl-imag-part bytevector-s8-ref foreign-callable-code-object
 close-output-port add-duration compile-file div-and-mod
 enum-set-difference set-sstats-gc-cpu! quasiquote
 make-hash-table inspect enum-set->list bytevector-s64-set!
 literal-identifier=? get-string-some fxremainder environment
 boolean=? release-minimum-generation
 set-textual-port-input-size! letrec* make-i/o-read-error
 r6rs:string<=? rename-file make-list vector-set-fixnum!
 symbol-hash make-i/o-file-protection-error
 &i/o-file-already-exists char<? char=? char>? r6rs:string>=?
 sstats? record-reader record-equal-procedure abort-handler
 bytevector-s16-ref r6rs:< r6rs:= s8-list->bytevector r6rs:>
 lookahead-char require-nongenerative-clause
 i/o-error-position &who weak-pair? reset-cost-center!
 generate-wpo-files &undefined bytevector-s24-ref
 ephemeron-pair? fl-make-rectangular make-polar
 fxarithmetic-shift-left generate-interrupt-trap make-mutex
 &i/o-file-is-read-only &warning assertion-violation?
 fxbit-set? make-time integer-length
 r6rs:with-input-from-file andmap make-message-condition
 call-with-bytevector-output-port assertion-violationf
 maybe-compile-library bytevector-s32-ref string?
 hashtable-weak? define compile-port make-violation
 serious-condition? debug-condition trace-print print-length
 sstats-difference bytevector-s40-ref date-week-day
 $primitive print-char-name char-general-category
 make-transcoder &no-infinities fxrotate-bit-field
 r6rs:command-line eq-hashtable-weak? bytes-allocated
 set-time-type! string->list char<=? bytevector-u8-ref
 generate-allocation-counts char>=?  ...
 make-weak-eqv-hashtable current-output-port
 lexical-violation? record-type-descriptor?
 with-interrupts-disabled with-mutex define-values
 get-output-string char-whitespace? get-hash-table
 integer->char ftype-pointer->sexpr string-titlecase
 make-hashtable waiter-write r6rs:<= r6rs:>=
 source-table-contains? fxzero? most-positive-fixnum
 enum-set-constructor native-transcoder untrace cflonum?
 nongenerative get-string-all unquote fxvector? fasl-write
 compute-composition peek-char scheme-report-environment
 make-engine string->utf8 make-warning port-file-compressed!
 append statistics partition with-exception-handler negative?
 flinfinite? r6rs:record-constructor annotation-expression
 ceiling bytevector->string foreign-ref hash-table-map
 make-rectangular bytevector-s32-native-set!
 heap-reserve-ratio make-enumeration profile-palette warning?
 open-string-input-port $system get-u8 warningf
 exclusive-cond port-output-count load-compiled-from-port
 most-negative-fixnum hashtable-mutable? print-unicode
 get-bytevector-all identifier-syntax &i/o-file-protection
 bytevector? read-char symbol? reset-maximum-memory-bytes!
 bytevector-ieee-double-native-ref equal-hash
 with-input-from-file bignum? trace-do i/o-port-error?
 exact->inexact bytevector-s48-ref record-constructor)]
    [(equal? list-instance '(chezscheme)) '(symbol-hashtable-update! r6rs:dynamic-wind trace-define
 \x31;+
 \x31;- 
 \x2D;1+ 
 mutable-string? assert textual-port-input-count
 property-list make-parameter compile-library logbit0 logbit1
 make-i/o-file-does-not-exist-error pretty-line-length
 bytevector-s56-ref make-custom-binary-output-port logbit?
 virtual-register ftype-set! fields make-ftype-pointer
 make-source-object port-output-full? abs break-handler fl*
 library-directories fl+ set-binary-port-output-buffer! fl-
 fl/ port-output-index car fl< fl= and fl> cdr
 profile-line-number-color eq? bytevector-s64-ref fx* fx+ ash
 make-assertion-violation fx- gcd fx/ div box
 record-type-field-names flacos fx< fx= cos fx>
 string->symbol compile-whole-program bytevector-u16-set!
 for-each lcm exp map let max disable-interrupts min
 make-i/o-port-error log bytevector-compress string<=? mod
 fldiv0 expt-mod string>=? rec not flatan tan
 critical-section vector-length close-port
 set-textual-port-output-size! flasin binary-port-output-size
 sin make-object-finder engine-return current-expand
 console-input-port textual-port-input-index port-bol?
 &i/o-write filter datum->syntax no-infinities-violation?
 vector-for-each gensym fxvector-ref flpositive?
 bytevector-ieee-double-ref getenv eq-hashtable-delete!
 set-port-input-buffer! standard-error-port port-eof?
 make-input/output-port r6rs:char-ci<? set-port-output-index!
 r6rs:char-ci=? r6rs:char-ci>? who-condition?
 i/o-decoding-error? make-i/o-invalid-position-error
 bytevector-u24-set! flnan? add1 mutable-fxvector? flexpt
 flodd? imag-part reverse! top-level-program &source flmod0
 source-table-delete! &i/o-file-does-not-exist
 date-zone-offset caar foreign-callable-entry-point cfl*
 print-record cfl+ acos bytevector-u16-ref cadr cfl- cfl/
 fold-left compile-program-handler unsyntax-splicing fl<=
 open-process-ports fl<? fl=? fl>= fl>? cfl= port-name cdar
 output-port-buffer-mode logtest condition-irritants cddr
 case display-condition r5rs foreign-entry? fx1+ fx1-
 csv7:record-field-accessor cfl-real-part bytevector-u24-ref
 &lexical set-port-nonblocking! div0 transcoder-eol-style
 bitwise-first-bit-set pretty-one-line-limit foreign-alloc
 make-condition atan remove-hash-table! inexact? box? fx<=
 fx<? asin fx=? fx>= fx>? bytevector-u32-set!
 clear-output-port newline cond assp assq cons assv
 bytevector-u32-ref cosh hashtable-clear!
 keyboard-interrupt-handler find enum-set? string->utf16
 string->utf32 string-append else fxvector-length
 put-hash-table! ieee inexact->exact define-syntax
 i/o-file-does-not-exist-error? eqv? fllp r6rs:syntax-rules
 get-datum/annotations u8-list->bytevector eval flsqrt equal?
 flceiling bytevector-u40-ref print-vector-length let*
 bytevector-s64-native-set! cp0-outer-unroll-limit
 expression-editor implicit-exports nan? error-handling-mode
 exit fxif record-type-equal-procedure r6rs:char-ci<=? expt
 odd? fxcopy-bit port-has-port-nonblocking?? exact?
 r6rs:char-ci>=? transcoder-codec timer-interrupt-handler
 real-valued? iota top-level-value mod0 memp memq memv meta
 hashtable-hash-function syntax-violation-subform
 get-thread-id load datum->syntax-object list format
 bytevector-u40-set! internal-defines-as-letrec* error?
 bytevector-u64-native-ref default-exception-handler
 endianness port-file-descriptor utf-16-codec read except
 errorf set! bitwise-arithmetic-shift-right case-sensitive
 lambda substring-fill! quotient enum-set-complement
 violation? annotation? remp remq remv only make-guardian
 tanh undefined-variable-warnings box-cas! &error
 pretty-print path-extension buffer-mode? procedure? sinh
 i/o-error? bytevector sub1 binary-port? time
 unregister-guardian nonnegative? flinteger?
 set-top-level-value! date-zone-name foreign-entry
 syntax->list load-program &syntax let-values fxnonpositive?
 sort when port-position bytevector-uint-ref sqrt
 pretty-standard-indent void bytevector-s16-native-ref
 scheme-environment r6rs:char<? r6rs:char=? r6rs:char>?
 &message fxdiv0 source-object-column file-position
 open-output-string bytevector-ieee-double-set! unget-u8
 with-source-path expand exists define-top-level-value
 append! collect-rendezvous path-last current-date
 make-irritants-condition define-record-type char-numeric?
 display-statistics library-list open-bytevector-output-port
 string-normalize-nfc string-normalize-nfd export
 file-symbolic-link? fxodd? bytevector-u48-ref length
 make-custom-textual-input-port raise-continuable fxmod0
 annotation-stripped current-eval fasl-file
 set-sstats-gc-count! import path-rest set-virtual-register!
 put-string-some parameterize bytevector-u56-ref
 ftype-init-lock! cost-center-time immutable r6rs:char<=?
 r6rs:char>=? port-output-size expand-output
 collect-trip-bytes fxbit-field member path-root call/1cc
 unread-char bytevector-u64-ref char-ci<? char-ci=? char-ci>?
 collect-request-handler locked-object? null-environment
 cost-center-allocation-count i/o-encoding-error-char
 rationalize create-exception-state scheme-program letrec
 make-boot-file sstats-bytes fxbit-count
 open-file-input/output-port time-difference! merge!
 weak-cons string-ci<=? record-writer binary-port-input-size
 string-ci>=? hashtable-contains? trace-case-lambda
 vector-fill! trace-lambda fasl-read open-fd-input-port
 r6rs:call-with-output-file strip-fasl-file hashtable?
 write-char current-time record-case
 generate-inspector-information bytevector-s8-set!
 fluid-let-syntax apropos petite? interactive?
 source-file-descriptor? hashtable-equivalence-function
 r6rs:current-input-port ftype-spin-lock! let*-values
 open-input-output-file oblist bytevector-uint-set!
 open-fd-input/output-port subtract-duration
 hash-table-for-each char-title-case? set-sstats-gc-bytes!
 last-pair caaar caadr fxlength &i/o-read cadar logand caddr
 collections abort r6rs:call-with-input-file
 condition-predicate string-ci-hash fl<=? char-name
 enum-set-universe acosh fl>=? string->bytevector cdaar
 time-nanosecond cdadr symbol-hashtable-cell &non-continuable
 make-i/o-error compress-format begin cddar fxnegative?
 compile-file-message record? cdddr
 bytevector-ieee-double-native-set! logior &i/o-port
 symbol-hashtable-delete! foreign-address-name
 current-input-port condition-message lognot char-
 csv7:record-field-mutable? textual-port-input-buffer char?
 custom-port-buffer-size fxfirst-bit-set pariah
 make-variable-transformer alias date?
 set-binary-port-input-index! parent delete-file ftype-ref
 real-part hashtable-entries put-datum path-absolute? debug
 logxor bytevector-u32-native-set! angle open-source-file
 r6rs:file-exists? vector->immutable-vector define-structure
 delay flnonpositive? fxarithmetic-shift
 bytevector->uint-list datum fxvector->list enable-interrupts
 chmod console-output-port library-extensions
 call-with-current-continuation mutex-acquire
 bytevector->s8-list put-char real->flonum module
 source-condition? fx<=? modulo atanh immutable-bytevector?
 source-directories real-time profile-dump-data fx>=? collect
 ephemeron-cons call-with-port break display
 port-has-port-length? make-no-nans-violation apply asinh
 let-syntax csv7:record-type-symbol time-utc->date
 directory-separator atom? make-record-type
 symbol-hashtable-set! compile-whole-library cons*
 get-bytevector-some! date-nanosecond bitwise-bit-set?
 rational-valued? string-copy! r5rs-syntax string-fill! assoc
 with-implicit top-level-mutable? source-object-bfp
 quasisyntax flabs string-length bytevector-s32-native-ref
 sstats-gc-real ratnum? char-downcase iconv-codec
 char-upper-case? input-port-ready? source-object-efp fxlogand 
 interaction-environment sstats-print random
 gensym->unique-string fldiv flcos bytevector-length
 input-port? <= => >= r6rs:standard-input-port
 subtract-duration! object-counts compile flexp
 current-memory-bytes fxlogior mark-port-closed! flmax
 string-ref profile-dump-html pretty-format &continuation
 flmin fllog source-object-sfd flmod identifier?
 current-directory pretty-maximum-lines profile-dump-list
 gensym-count fxlognot make-serious-condition
 binary-port-output-buffer fltan floor
 enable-cross-library-optimization
 make-custom-binary-input/output-port syntax-object->datum
 flsin irritants-condition? optimize-level
 library-search-handler i/o-file-already-exists-error? even?
 call-with-input-file fxlogbit0 make-lexical-violation
 fxlogbit1 fxvector-copy annotation-options force cd
 fxlogbit? port-input-size do bitwise-bit-field fxlogxor if
 exact top-level-syntax compile-to-file bytevector-copy error
 substring or scheme generate-procedure-source-information
 sealed source-table-ref record-accessor copy-environment
 opaque list-head default-record-equal-procedure fxabs rename
 bitwise-bit-count open-file-input-port
 make-i/o-file-is-read-only-error list-copy fxand
 library-version guard bytevector-u48-set! fxmodulo
 cost-center? fxdiv remove! open-string-output-port
 implementation-restriction-violation?
 port-has-set-port-position!? debug-level flnumerator
 register-signal-handler fxior fxmax integer? fxmin
 initial-bytes-allocated remove fxmod compile-library-handler
 define-property make-i/o-filename-error record-type-name
 trace-output-port fxnot fxvector-set! remprop
 record-hash-procedure call-with-values fxsll fxsra
 vector-sort! fxsrl merge utf-8-codec command-line-arguments
 make-vector list* read-token bwp-object? fxxor list?
 visit-compiled-from-port div0-and-mod0 pair? mkdir
 set-time-second! collect-notify mutex? bytevector-u56-set!
 remove-foreign-entry library-exports ieee-environment
 eol-style compile-to-port block-read isqrt syntax-violation
 list-tail set-sstats-bytes! environment? transcoded-port
 logor undefined-violation? ftype-guardian prefix
 bitwise-rotate-bit-field binary-port-input-buffer
 make-thread-parameter mutable fxlogtest
 r6rs:with-output-to-file source-table? mutable-bytevector?
 eof-object finite? bytevector-u64-native-set! real?
 list-sort bytevector-u8-set! profile-query-weight
 foreign-callable bytevector-ieee-single-set! raise
 eq-hashtable-ref trace-let put-u8 subset-mode for-all
 date-year-day time-second bytevector-u64-set! gensym?
 make-custom-textual-input/output-port remq! new-cafe
 make-ephemeron-eq-hashtable enum-set-indexer remv!
 scheme-version printf char-foldcase load-library
 make-i/o-encoding-error i/o-error-filename reverse magnitude
 number? condition-name compile-imported-libraries
 procedure-arity-mask flfloor reset time<? enum-set-union
 time=? time>? null? fresh-line set-port-input-index! ormap
 fleven? guardian? current-exception-state revisit
 fxdiv0-and-mod0 eval-when &implementation-restriction port?
 with-input-from-string bytevector->immutable-bytevector
 threaded? r6rs:hashtable-entries rational? values
 hashtable-ephemeron? with-output-to-string
 make-i/o-write-error i/o-file-is-read-only-error? sleep
 time? bound-identifier=? utf-16le-codec sc-expand
 foreign-sizeof source-file-descriptor-path condition-wait
 vector time-difference char->integer integer-valued?
 engine-block bitwise-and infinite? ftype-pointer=?
 make-sstats cp0-effort-limit getprop char-titlecase
 compress-level waiter-prompt-and-read round utf32->string
 date-and-time sort! bytevector->u8-list drop-prefix trace
 record-constructor-descriptor? syntax-violation-form
 vector-cas! get-char quote cpu-time port-nonblocking?
 make-string bitwise-ior record-field-mutable? unbox
 eq-hashtable-ephemeron? commonization-level date-dst?
 eval-syntax-expanders-when putenv subst enum-set=?
 bitwise-not string-truncate! r6rs:delete-file
 verify-loadability textual-port-input-size condition
 textual-port-output-size make-syntax-violation visit
 type-descriptor hash-table? generate-instruction-counts
 constructor flnegative? make-ephemeron-eqv-hashtable
 get-string-some! string-for-each bitwise-xor fold-right
 date-hour make-input-port condition-accessor string-ci<?
 string-ci=? string-ci>? record-type-hash-procedure
 i/o-filename-error? vector-copy format-condition?
 i/o-write-error? fxquotient boolean? bytevector-s16-set!
 enum-set-projection default-prompt-and-read
 bytevector-s16-native-set! pretty-initial-indent
 immutable-vector? zero? get-line ftype-pointer?
 bitwise-copy-bit transcript-cafe standard-input-port
 list->vector fxarithmetic-shift-right source-object?
 get-mode case-lambda &i/o-decoding make-source-table
 print-precision write subst! sstats-real
 bytevector-ieee-single-native-ref
 csv7:record-field-accessible? command-line record-type-uid
 enum-set-subset? make-undefined-violation greatest-fixnum
 r6rs:record? fxvector set-binary-port-input-size! fixnum?
 flonum? bytevector-uncompress substq flush-output-port
 substv string-copy file-regular? string-hash latin-1-codec
 bytevector-s24-set! date-year vector-set!
 compile-time-value? unless source-table-cell flfinite?
 directory-list port-output-buffer cfl-conjugate fltruncate
 transcript-off make-who-condition r6rs:utf-16-codec
 concatenate-object-files print-extended-identifiers
 positive? file-change-time hashtable-update! process
 &violation sstats-gc-cpu make-weak-eq-hashtable random-seed
 string<? string=? vector-sort time<=? string>?
 fxvector->immutable-fxvector set-sstats-gc-real!
 compile-script time>=? make-continuation-condition string
 locate-source-object-source condition-continuation
 syntax->datum record-mutator cost-center-instruction-count
 source-table-dump revisit-compiled-from-port profile
 r6rs:case char-ready? put-bytevector make-i/o-decoding-error
 bytevector-s32-set! top-level-syntax?
 make-custom-binary-input-port string-set! fx*/carry
 syntax-error exit-handler fldenominator
 current-locate-source-object-source gensym-prefix flround
 environment-symbols enum-set-member? i/o-read-error?
 maximum-memory-bytes fluid-let make-boot-header
 letrec-syntax textual-port-output-count
 r6rs:standard-output-port generate-covin-files record-rtd
 scheme-start cfl-magnitude-squared source-object-line
 set-time-nanosecond! default-record-hash-procedure
 profile-clear binary-port-input-count
 set-binary-port-output-size! unquote-splicing
 bitwise-copy-bit-field set-sstats-cpu! &i/o-encoding
 r6rs:eval source-table-set! syntax-case
 make-non-continuable-violation
 port-has-set-port-nonblocking!? set-timer fxnonnegative?
 csv7:record-type-field-decls fasl-compressed r6rs:exit
 bytevector-u16-native-ref import-only bytevector-s40-set!
 with-output-to-file truncate-file open-input-string
 load-shared-object &i/o-filename base-exception-handler
 file-buffer-size ftype-lock! source-table-size
 console-error-port file-options dynamic-wind
 port-has-port-position? bitwise-reverse-bit-field
 symbol-hashtable-contains? hashtable-cell syntax remainder
 hashtable-values fixnum-width flzero? &i/o-invalid-position
 source-condition-form textual-port-output-index list->string
 open-output-file immutable-fxvector? condition-who
 record-type-parent binary-port-input-index meta-cond
 ftype-locked-decr! hashtable-copy make-eq-hashtable run-cp0
 open-bytevector-input-port protocol system
 bytevector-sint-ref i/o-file-protection-error?
 flonum->fixnum fixnum->flonum syntax->annotation
 number->string thread? with-syntax r6rs:string-ci<=?
 uint-list->bytevector record-type-descriptor
 r6rs:string-ci>=? enable-object-counts hashtable-keys
 transcript-on condition? bitwise-if simple-conditions
 collect-generation-radix transcoder? truncate-port
 symbol-hashtable? put-string vector? sstats-cpu
 set-port-bol! make-fxvector maybe-compile-program
 bytevector-sint-set! trace-define-syntax
 profile-clear-database date-minute foreign-free
 hashtable-set! vector-map string-upcase enumerate
 r6rs:number->string fx+/carry set-port-eof!
 make-record-type-descriptor make-custom-textual-output-port
 vector-ref waiter-prompt-string open-file-output-port
 interpret fprintf standard-output-port r6rs:hashtable-keys
 continuation-condition? i/o-encoding-error? set-car!
 generate-temporaries hashtable-size make-output-port
 set-cdr! fxvector-fill! set-port-name! path-first list-ref
 compile-time-value-value add-prefix library-requirements
 hashtable-ref suppress-greeting close-input-port
 profile-load-data environment-mutable? foreign-set!
 print-brackets &serious &format make-eqv-hashtable
 apropos-list set-box! set-port-length! message-condition?
 denominator magnitude-squared csv7:record-type-field-names
 date->time-utc thread-condition? make-source-condition
 enum-set-intersection symbol-hashtable-ref list->fxvector
 bitwise-arithmetic-shift-left bytevector->sint-list
 set-sstats-real! copy-time put-source-table syntax-rules
 non-continuable-violation? date-day port-input-buffer
 bytevector-s64-native-ref ftype-locked-incr!
 r6rs:flush-output-port set-port-position! annotation-source
 directory-separator? invoke-library syntax-violation?
 call-with-output-file extend-syntax r6rs:string-ci<?
 r6rs:string-ci=? r6rs:string-ci>? binary-port-output-count
 eq-hashtable? r6rs:open-input-file fasl-strip-options
 current-transcoder virtual-register-count port-closed?
 bitwise-length import-notify delete-directory date-month
 nonpositive? display-string assertion-violation
 ftype-unlock! print-graph &condition define-top-level-syntax
 with-cost-center bytes-deallocated make-compile-time-value
 textual-port-output-buffer eq-hashtable-cell flnonnegative?
 make-bytevector make-implementation-restriction-violation
 immutable-box? fxdiv-and-mod r6rs:fx* r6rs:fx+ r6rs:fx-
 bytevector=? lock-object source-file-descriptor decode-float
 fxreverse-bit-field output-port? conjugate include
 parent-rtd mutable-vector? native-endianness compile-profile
 port-input-count exact-integer-sqrt warning
 scheme-version-number file-length foreign-procedure
 expand/optimize set-port-output-size!
 binary-port-output-index print-level date-second
 textual-port? cp0-score-limit file-directory? putprop
 ftype-pointer-address compile-program clear-input-port
 &assertion set-textual-port-input-index!
 compile-interpret-simple hashtable-cells
 collect-maximum-generation string-downcase
 make-source-file-descriptor bytevector-u32-native-ref
 i/o-invalid-position-error? i/o-error-port port-input-empty?
 mutex-release make-format-condition r6rs:string<?
 r6rs:string=? r6rs:string>? generate-profile-forms
 condition-signal top-level-bound? bitwise-arithmetic-shift
 get-bytevector-n pretty-file mutable-box?
 r6rs:current-output-port source-file-descriptor-checksum
 define-condition-type truncate char-upcase locate-source
 transcoder-error-handling-mode
 make-i/o-file-already-exists-error make-annotation
 eq-hashtable-set! port-input-index ftype-sizeof inexact
 get-string-n! print-radix path-parent fldiv-and-mod
 csv7:record-field-mutator bytevector-truncate!
 profile-release-counters unsyntax indirect-export
 char-lower-case? * + - / port-transcoder < = >
 port-has-set-port-length!? complex? file-modification-time
 port-handler make-no-infinities-violation open-input-file _
 ftype-&ref r6rs:current-error-port fork-thread symbol=?
 expand/optimize-output eof-object? sint-list->bytevector
 utf-16be-codec immutable-string? char-alphabetic?
 r6rs:open-output-file fxeven? get-bytevector-n! file-port?
 print-gensym default-library-search-handler
 string-normalize-nfkc string-normalize-nfkd sstats-gc-count
 reset-handler char-ci<=? eq-hashtable-update! box-immutable
 char-ci>=? current-make-source-object
 bytevector-u16-native-set! hashtable-delete!
 eq-hashtable-contains? set-binary-port-input-buffer!
 ftype-pointer-ftype buffer-mode bytevector-ieee-single-ref
 set-binary-port-output-index! profile-dump
 put-bytevector-some open-fd-output-port
 make-record-constructor-descriptor predicate scheme-script
 native-eol-style unget-char free-identifier=?
 current-error-port bytevector-ieee-single-native-set!
 get-source-table! least-fixnum condition-broadcast
 inspect/object sstats-gc-bytes no-nans-violation? call/cc
 utf8->string get-datum call-with-string-output-port
 library-requirements-options make-error
 csv7:record-type-name caaaar set-port-input-size!
 bytevector-s48-set! record-constructor-descriptor caaadr
 fx-/carry fxpositive? syntax->vector with-profile-tracker
 fxcopy-bit-field caadar define-ftype caaddr
 debug-on-exception substq! record-type-generative? substv!
 set-textual-port-input-buffer! compute-size cadaar cadadr
 define-record set-textual-port-output-index! time-type
 caddar cadddr mutex-name string->number &no-nans library
 machine-type add-duration! symbol->string
 get-bytevector-some fldiv0-and-mod0 vector->list port-length
 string->immutable-string fxlogor lookahead-u8
 set-port-output-buffer! library-object-filename
 file-access-time record-type-sealed? define-enumeration
 numerator annotation-option-set block-write get-string-n
 cdaaar string-foldcase unlock-object bytevector-s56-set!
 make-date cdaadr record-type-opaque? cdadar cdaddr
 get-process-id record-predicate &irritants cddaar &i/o
 ftype-pointer-null? cddadr r6rs:string->number cdddar
 csv7:record-type-descriptor cddddr utf16->string
 file-exists? r6rs:standard-error-port
 set-textual-port-output-buffer! make-cost-center
 bytevector-copy! bytevector-fill! maybe-compile-file
 cfl-imag-part bytevector-s8-ref foreign-callable-code-object
 close-output-port add-duration compile-file div-and-mod
 enum-set-difference set-sstats-gc-cpu! quasiquote
 make-hash-table inspect enum-set->list bytevector-s64-set!
 literal-identifier=? get-string-some fxremainder environment
 boolean=? release-minimum-generation
 set-textual-port-input-size! letrec* make-i/o-read-error
 r6rs:string<=? rename-file make-list vector-set-fixnum!
 symbol-hash make-i/o-file-protection-error
 &i/o-file-already-exists char<? char=? char>? r6rs:string>=?
 sstats? record-reader record-equal-procedure abort-handler
 bytevector-s16-ref r6rs:< r6rs:= s8-list->bytevector r6rs:>
 lookahead-char require-nongenerative-clause
 i/o-error-position &who weak-pair? reset-cost-center!
 generate-wpo-files &undefined bytevector-s24-ref
 ephemeron-pair? fl-make-rectangular make-polar
 fxarithmetic-shift-left generate-interrupt-trap make-mutex
 &i/o-file-is-read-only &warning assertion-violation?
 fxbit-set? make-time integer-length
 r6rs:with-input-from-file andmap make-message-condition
 call-with-bytevector-output-port assertion-violationf
 maybe-compile-library bytevector-s32-ref string?
 hashtable-weak? define compile-port make-violation
 serious-condition? debug-condition trace-print print-length
 sstats-difference bytevector-s40-ref date-week-day
 $primitive print-char-name char-general-category
 make-transcoder &no-infinities fxrotate-bit-field
 r6rs:command-line eq-hashtable-weak? bytes-allocated
 set-time-type! string->list char<=? bytevector-u8-ref
 generate-allocation-counts char>=?  ...
 make-weak-eqv-hashtable current-output-port
 lexical-violation? record-type-descriptor?
 with-interrupts-disabled with-mutex define-values
 get-output-string char-whitespace? get-hash-table
 integer->char ftype-pointer->sexpr string-titlecase
 make-hashtable waiter-write r6rs:<= r6rs:>=
 source-table-contains? fxzero? most-positive-fixnum
 enum-set-constructor native-transcoder untrace cflonum?
 nongenerative get-string-all unquote fxvector? fasl-write
 compute-composition peek-char scheme-report-environment
 make-engine string->utf8 make-warning port-file-compressed!
 append statistics partition with-exception-handler negative?
 flinfinite? r6rs:record-constructor annotation-expression
 ceiling bytevector->string foreign-ref hash-table-map
 make-rectangular bytevector-s32-native-set!
 heap-reserve-ratio make-enumeration profile-palette warning?
 open-string-input-port $system get-u8 warningf
 exclusive-cond port-output-count load-compiled-from-port
 most-negative-fixnum hashtable-mutable? print-unicode
 get-bytevector-all identifier-syntax &i/o-file-protection
 bytevector? read-char symbol? reset-maximum-memory-bytes!
 bytevector-ieee-double-native-ref equal-hash
 with-input-from-file bignum? trace-do i/o-port-error?
 exact->inexact bytevector-s48-ref record-constructor)]
    [(equal? list-instance '(rnrs condition)) '(&assertion &condition &error &implementation-restriction
 &irritants &lexical &message &non-continuable &serious
 &syntax &undefined &violation &warning &who
 assertion-violation? condition condition-accessor
 condition-irritants condition-message condition-predicate
 condition-who condition? define-condition-type error?
 implementation-restriction-violation? irritants-condition?
 lexical-violation? make-assertion-violation make-error
 make-implementation-restriction-violation
 make-irritants-condition make-lexical-violation
 make-message-condition make-non-continuable-violation
 make-serious-condition make-syntax-violation
 make-undefined-violation make-violation make-warning
 make-who-condition message-condition?
 non-continuable-violation? serious-condition?
 simple-conditions syntax-violation-form
 syntax-violation-subform syntax-violation?
 undefined-violation? violation? warning? who-condition?)]
    [(equal? list-instance '(rnrs files)) '(&i/o &i/o-file-already-exists &i/o-file-does-not-exist
 &i/o-file-is-read-only &i/o-file-protection &i/o-filename
 &i/o-invalid-position &i/o-port &i/o-read &i/o-write
 i/o-error-filename i/o-error-port i/o-error-position
 i/o-error? i/o-file-already-exists-error?
 i/o-file-does-not-exist-error? i/o-file-is-read-only-error?
 i/o-file-protection-error? i/o-filename-error?
 i/o-invalid-position-error? i/o-port-error? i/o-read-error?
 i/o-write-error? make-i/o-error make-i/o-file-already-exists-error
 make-i/o-file-does-not-exist-error make-i/o-file-is-read-only-error
 make-i/o-file-protection-error make-i/o-filename-error
 make-i/o-invalid-position-error make-i/o-port-error
 make-i/o-read-error make-i/o-write-error delete-file
 file-exists?)]
    [(equal? list-instance '(rnrs base)) '(* + - ... / => _ abs acos and angle append apply asin assert
 assertion-violation atan begin boolean=? boolean? caaaar
 caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar
 caddar cadddr caddr cadr call-with-current-continuation
 call-with-values call/cc car cdaaar cdaadr cdaar cdadar
 cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
 cddr cdr ceiling char->integer char? complex? cond cons cos
 define define-syntax denominator div div-and-mod div0
 div0-and-mod0 else eq? equal? eqv? error even? exact
 exact-integer-sqrt exact? exp expt finite? floor for-each
 gcd identifier-syntax if imag-part inexact inexact?
 infinite? integer->char integer-valued? integer? lambda lcm
 length let let* let*-values let-syntax let-values letrec
 letrec* letrec-syntax list list->string list->vector
 list-ref list-tail list? log magnitude make-polar
 make-rectangular make-string make-vector map max min mod
 mod0 nan? negative? not null? number? numerator odd? or
 pair? positive? procedure? quasiquote quote < <= = > >= case
 char<=? char<? char=? char>=? char>? dynamic-wind
 number->string string->number string<=? string<? string=?
 string>=? string>? syntax-rules rational-valued? rational?
 rationalize real-part real-valued? real? reverse round set!
 sin sqrt string string->list string->symbol string-append
 string-copy string-for-each string-length string-ref string?
 substring symbol->string symbol=? symbol? tan truncate
 unquote unquote-splicing values vector vector->list
 vector-fill! vector-for-each vector-length vector-map
 vector-ref vector-set! vector? zero?)]
    [(equal? list-instance '(rnrs syntax-case)) '(... _ bound-identifier=? datum->syntax free-identifier=?
     generate-temporaries identifier? make-variable-transformer
     quasisyntax syntax syntax->datum syntax-case
     syntax-violation unsyntax unsyntax-splicing with-syntax)]
    [(equal? list-instance '(rnrs exception)) '(=> else guard raise raise-continuable with-exception-handler)]
    [(equal? list-instance '(rnrs lists)) '(assoc assp assq assv cons* exists filter find fold-left
  fold-right for-all member memp memq memv partition remove remp remq remv)]
    [(equal? list-instance '(rnrs bytevectors)) '(bytevector->sint-list bytevector->u8-list
 bytevector->uint-list bytevector-copy bytevector-copy!
 bytevector-fill! bytevector-ieee-double-native-ref
 bytevector-ieee-double-native-set!
 bytevector-ieee-double-ref bytevector-ieee-double-set!
 bytevector-ieee-single-native-ref
 bytevector-ieee-single-native-set!
 bytevector-ieee-single-ref bytevector-ieee-single-set!
 bytevector-length bytevector-s16-native-ref
 bytevector-s16-native-set! bytevector-s16-ref
 bytevector-s16-set! bytevector-s32-native-ref
 bytevector-s32-native-set! bytevector-s32-ref
 bytevector-s32-set! bytevector-s64-native-ref
 bytevector-s64-native-set! bytevector-s64-ref
 bytevector-s64-set! bytevector-s8-ref bytevector-s8-set!
 bytevector-sint-ref bytevector-sint-set!
 bytevector-u16-native-ref bytevector-u16-native-set!
 bytevector-u16-ref bytevector-u16-set!
 bytevector-u32-native-ref bytevector-u32-native-set!
 bytevector-u32-ref bytevector-u32-set!
 bytevector-u64-native-ref bytevector-u64-native-set!
 bytevector-u64-ref bytevector-u64-set! bytevector-u8-ref
 bytevector-u8-set! bytevector-uint-ref bytevector-uint-set!
 bytevector=? bytevector? endianness make-bytevector
 native-endianness sint-list->bytevector string->utf16
 string->utf32 string->utf8 u8-list->bytevector
 uint-list->bytevector utf16->string utf32->string
 utf8->string)]
    [(equal? list-instance '(rnrs control)) '(case-lambda do unless when)]
    [(equal? list-instance '(rnrs unicode)) '(char-alphabetic? char-downcase char-foldcase
 char-general-category char-lower-case? char-numeric?
 char-title-case? char-titlecase char-upcase char-upper-case?
 char-whitespace? char-ci<=? char-ci<? char-ci=? char-ci>=?
 char-ci>? string-ci<=? string-ci<? string-ci=? string-ci>=?
 string-ci>? string-downcase string-foldcase
 string-normalize-nfc string-normalize-nfd
 string-normalize-nfkc string-normalize-nfkd string-titlecase
 string-upcase)]
    [(equal? list-instance '(rnrs enums)) '(define-enumeration enum-set->list
  enum-set-complement enum-set-constructor enum-set-difference enum-set-indexer enum-set-intersection
  enum-set-member?  enum-set-projection enum-set-subset?  enum-set-union enum-set-universe
  enum-set=?  make-enumeration)]
    [(equal? list-instance '(rnrs r5rs)) '(delay exact->inexact force inexact->exact modulo
  null-environment quotient remainder scheme-report-environment)]
    [(equal? list-instance '(rnrs eval)) '(environment eval)]
    [(equal? list-instance '(rnrs hashtables)) '(equal-hash hashtable-clear! hashtable-contains?
 hashtable-copy hashtable-delete!  hashtable-equivalence-function hashtable-hash-function
 hashtable-mutable? hashtable-ref hashtable-set!  hashtable-size hashtable-update! hashtable?
 make-eq-hashtable make-eqv-hashtable make-hashtable hashtable-entries hashtable-keys 
 string-ci-hash string-hash symbol-hash)]
    [(equal? list-instance '(rnrs sorting)) '(list-sort vector-sort vector-sort!)]
    [(equal? list-instance '(rnrs programs)) '(command-line exit)]
    [(equal? list-instance '(rnrs mutable-pairs)) '(set-car! set-cdr!)]
    [(equal? list-instance '(rnrs mutable-strings)) '(string-fill! string-set!)]
    [(equal? list-instance '(rnrs io ports)) '(&i/o &i/o-decoding &i/o-encoding &i/o-file-already-exists
 &i/o-file-does-not-exist &i/o-file-is-read-only
 &i/o-file-protection &i/o-filename &i/o-invalid-position
 &i/o-port &i/o-read &i/o-write binary-port? buffer-mode
 buffer-mode? bytevector->string
 call-with-bytevector-output-port call-with-port
 call-with-string-output-port close-port eof-object
 eof-object? eol-style error-handling-mode file-options
 get-bytevector-all get-bytevector-n get-bytevector-n!
 get-bytevector-some get-char get-datum get-line
 get-string-all get-string-n get-string-n! get-u8
 i/o-decoding-error? i/o-encoding-error-char
 i/o-encoding-error? i/o-error-filename i/o-error-port
 i/o-error-position i/o-error? i/o-file-already-exists-error?
 i/o-file-does-not-exist-error? i/o-file-is-read-only-error?
 i/o-file-protection-error? i/o-filename-error?
 i/o-invalid-position-error? i/o-port-error? i/o-read-error?
 i/o-write-error? input-port? latin-1-codec lookahead-char
 lookahead-u8 make-custom-binary-input-port
 make-custom-binary-input/output-port
 make-custom-binary-output-port
 make-custom-textual-input-port
 make-custom-textual-input/output-port
 make-custom-textual-output-port make-i/o-decoding-error
 make-i/o-encoding-error make-i/o-error
 make-i/o-file-already-exists-error
 make-i/o-file-does-not-exist-error
 make-i/o-file-is-read-only-error
 make-i/o-file-protection-error make-i/o-filename-error
 make-i/o-invalid-position-error make-i/o-port-error
 make-i/o-read-error make-i/o-write-error make-transcoder
 native-eol-style native-transcoder
 open-bytevector-input-port open-bytevector-output-port
 open-file-input-port open-file-input/output-port
 open-file-output-port open-string-input-port
 open-string-output-port output-port-buffer-mode output-port?
 port-eof? port-has-port-position?
 port-has-set-port-position!? port-position port-transcoder
 port? put-bytevector put-char put-datum put-string put-u8
 current-error-port current-input-port current-output-port
 flush-output-port standard-error-port standard-input-port
 standard-output-port utf-16-codec set-port-position!
 string->bytevector textual-port? transcoded-port
 transcoder-codec transcoder-eol-style
 transcoder-error-handling-mode utf-8-codec)]
    [(equal? list-instance '(rnrs io simple)) '(&i/o &i/o-file-already-exists &i/o-file-does-not-exist
 &i/o-file-is-read-only &i/o-file-protection &i/o-filename
 &i/o-invalid-position &i/o-port &i/o-read &i/o-write
 close-input-port close-output-port display eof-object
 eof-object? i/o-error-filename i/o-error-port
 i/o-error-position i/o-error? i/o-file-already-exists-error?
 i/o-file-does-not-exist-error? i/o-file-is-read-only-error?
 i/o-file-protection-error? i/o-filename-error?
 i/o-invalid-position-error? i/o-port-error? i/o-read-error?
 i/o-write-error? input-port? make-i/o-error
 make-i/o-file-already-exists-error
 make-i/o-file-does-not-exist-error
 make-i/o-file-is-read-only-error
 make-i/o-file-protection-error make-i/o-filename-error
 make-i/o-invalid-position-error make-i/o-port-error
 make-i/o-read-error make-i/o-write-error newline
 output-port? peek-char call-with-input-file
 call-with-output-file current-error-port current-input-port
 current-output-port open-input-file open-output-file
 with-input-from-file with-output-to-file read read-char
 write write-char)]
    [(equal? list-instance '(rnrs arithmetic flonums)) '(&no-infinities &no-nans fixnum->flonum fl* fl+ fl- fl/ fl<=?
 fl<? fl=? fl>=? fl>? flabs flacos flasin flatan flceiling
 flcos fldenominator fldiv fldiv-and-mod fldiv0
 fldiv0-and-mod0 fleven? flexp flexpt flfinite? flfloor
 flinfinite? flinteger? fllog flmax flmin flmod flmod0 flnan?
 flnegative? flnumerator flodd? flonum? flpositive? flround
 flsin flsqrt fltan fltruncate flzero?
 make-no-infinities-violation make-no-nans-violation
 no-infinities-violation? no-nans-violation? real->flonum)]
    [(equal? list-instance '(rnrs arithmetic bitwise)) '(bitwise-and bitwise-arithmetic-shift bitwise-arithmetic-shift-left
  bitwise-arithmetic-shift-right bitwise-bit-count
  bitwise-bit-field bitwise-bit-set? bitwise-copy-bit
  bitwise-copy-bit-field bitwise-first-bit-set bitwise-if
  bitwise-ior bitwise-length bitwise-not
  bitwise-reverse-bit-field bitwise-rotate-bit-field bitwise-xor)]
    [(equal? list-instance '(rnrs arithmetic fixnums)) '(fixnum-width fixnum? fx*/carry fx+/carry fx-/carry fx<=?
 fx<? fx=? fx>=? fx>? fxand fxarithmetic-shift
 fxarithmetic-shift-left fxarithmetic-shift-right fxbit-count
 fxbit-field fxbit-set? fxcopy-bit fxcopy-bit-field fxdiv
 fxdiv-and-mod fxdiv0 fxdiv0-and-mod0 fxeven? fxfirst-bit-set
 fxif fxior fxlength fxmax fxmin fxmod fxmod0 fxnegative?
 fxnot fxodd? fxpositive? fxreverse-bit-field
 fxrotate-bit-field fxxor fxzero? greatest-fixnum
 least-fixnum fx* fx+ fx-)]
    [(equal? list-instance '(rnrs records syntactic)) '(define-record-type fields
  immutable mutable nongenerative opaque parent parent-rtd protocol record-constructor-descriptor
  record-type-descriptor sealed)]
    [(equal? list-instance '(rnrs records procedure)) '(make-record-constructor-descriptor make-record-type-descriptor 
    record-constructor record-accessor record-mutator record-predicate record-type-descriptor?)]
    [(equal? list-instance '(rnrs records inspection)) '(record? record-field-mutable? record-rtd record-type-field-names
  record-type-generative? record-type-name record-type-opaque?  record-type-parent record-type-sealed? record-type-uid)]
    [(equal? list-instance '(chezscheme csv7)) '(record-field-accessible? record-field-accessor record-field-mutable?
  record-field-mutator record-type-descriptor record-type-field-decls record-type-field-names 
  record-type-name record-type-symbol)]
    [(equal? list-instance '(scheme csv7)) '(record-field-accessible? record-field-accessor record-field-mutable?
  record-field-mutator record-type-descriptor record-type-field-decls record-type-field-names 
  record-type-name record-type-symbol)]))
)