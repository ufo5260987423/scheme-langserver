(library (scheme-langserver analysis type substitutions rnrs-meta-rules)
  (export rnrs-chez-rules)
  (import 
    (chezscheme)
    (scheme-langserver util natural-order-compare))

(define rnrs-chez-rules (sort 
  (lambda (target1 target2)
    (natural-order-compare 
      (symbol->string (car target1))
      (symbol->string (car target2))))
  '(
(- (number? <- (inner:list? number? **1)))
(* (number? <- (inner:list? number? ...)))
(/ (number? <- (inner:list? number? **1)))
(+ (number? <- (inner:list? number? ...)))
(< (boolean? <- (inner:list? real? real? **1)))
(<= (boolean? <- (inner:list? real? real? **1)))
(> (boolean? <- (inner:list? real? real? **1)))
(>= (boolean? <- (inner:list? real? real? **1)))
; (1- (number? <- (inner:list? number?)))
; (1+ (number? <- (inner:list? number?)))
; (-1+ (number? <- (inner:list? number?)))
(abort (void? <- (inner:list? something? ...)))
(abs (number? <- (inner:list? real?)))
(acos (number? <- (inner:list? number?)))
(acosh (number? <- (inner:list? number?)))
(add1 (number? <- (inner:list? number?)))
(add-duration (time? <- (inner:list? time? time?)))
(add-duration! (time? <- (inner:list? time? time?)))
(and (something? <- (list? something? ...)))
(angle (number? <- (inner:list? number?)))
(annotation? (boolean? <- (inner:list? something?)))
(annotation-expression (something? <- (inner:list? annotation?)))
(annotation-options (something? <- (inner:list? annotation?)))
(annotation-source (something? <- (inner:list? annotation?)))
(annotation-stripped (something? <- (inner:list? annotation?)))
(append ((inner:list? ) <- (inner:list? )))
(ash (integer? <- (inner:list? integer? integer?)))
(asin (number? <- (inner:list? number?)))
(asinh (number? <- (inner:list? number?)))
(assertion-violation? (boolean? <- (inner:list? something?)))
(atan (real? <- (inner:list? real? real?)))
(atan (number? <- (inner:list? number?)))
(atom? (boolean? <- (inner:list? something?)))
(bignum? (boolean? <- (inner:list? )))
(binary-port? (boolean? <- (inner:list? something?)))
(bitwise-bit-set? (boolean? <- (inner:list? )))
(boolean? (boolean? <- (inner:list? something?)))
(boolean=? (boolean? <- (inner:list? boolean? boolean?)))
(bound-identifier=? (boolean? <- (inner:list? )))
(box? (boolean? <- (inner:list? something?)))
(bwp-object? (boolean? <- (inner:list? something?)))
(bytevector? (boolean? <- (inner:list? something?)))
(caaaar (with ((((a fuzzy0 ...) fuzzy ...) b ...) c ...) a))
(caaaar (something? <- (inner:list? (inner:pair? something? something?))))
(caaadr (with ((a ((b c ...) d ...) e ...)) b))
(caaadr (something? <- (inner:list? (inner:pair? something? something?))))
(caaar (with (((a fuzzy ...) b ...) c ...) a))
(caaar (something? <- (inner:list? (inner:pair? something? something?))))
(caadr (with ((a (b c ...) d ...)) b))
(caadr (something? <- (inner:list? (inner:pair? something? something?))))
(caar (with ((a b ...) c ...) a))
(caar (something? <- (inner:list? (inner:pair? something? something?))))
(cadddr (with ((a b c d f e **1)) (with-equal? inner:list? a f)) )
(cadddr (something? <- (inner:list? (inner:pair? something? something?))))
(caddr (with ((a b c d f **1)) (with-equal? inner:list? a d)) )
(caddr (something? <- (inner:list? (inner:pair? something? something?))))
(cadr (with ((a b c d **1)) (with-equal? inner:list? a c)) )
(cadr (something? <- (inner:list? (inner:pair? something? something?))))
(car (with ((a b c **1)) (with-equal? inner:list? a b)) )
(car (something? <- (inner:list? (inner:pair? something? something?))))
(cdar (with (((a b **1) c ...)) b))
(cdar (something? <- (inner:list? (inner:pair? something? something?))))
(cddddr (with ((a b c d e f **1 )) (with-equal? inner:list? a (with-append (inner:list?) f))))
(cddddr (something? <- (inner:list? (inner:pair? something? something?))))
(cdddr (with ((a b c d e **1 )) (with-equal? inner:list? a (with-append (inner:list?) e))))
(cdddr (something? <- (inner:list? (inner:pair? something? something?))))
(cddr (with ((a b c d **1 )) (with-equal? inner:list? a (with-append (inner:list?) d))))
(cddr (something? <- (inner:list? (inner:pair? something? something?))))
(cdr (with ((a b c **1 )) (with-equal? inner:list? a (with-append (inner:list?) c))))
(cdr (something? <- (inner:list? (inner:pair? something? something?))))
(ceiling (integer? <- (inner:list? real?)))
(cfl- (cflonum? <- (inner:list? cflonum? **1)))
(cfl* (cflonum? <- (inner:list? cflonum? ...)))
(cfl/ (cflonum? <- (inner:list? cflonum? **1)))
(cfl+ (cflonum? <- (inner:list? cflonum? ...)))
(cfl= (cflonum? <- (inner:list? cflonum? ...)))
(cfl-conjugate (cflonum? <- (inner:list? cflonum?)))
(cfl-imag-part (cflonum? <- (inner:list? cflonum?)))
(cfl-magnitude-squared (cflonum? <- (inner:list? cflonum?)))
(cflonum? (boolean? <- (inner:list? something?)))
(cfl-real-part (cflonum? <- (inner:list? cflonum?)))
(char- (integer? <- (inner:list? char? char?)))
(char? (boolean? <- (inner:list? something?)))
(char<? (boolean? <- (inner:list? char? char? **1)))
(char<? (boolean? <- (inner:list? char? char? **1)))
(char<=? (boolean? <- (inner:list? char? char? **1)))
(char<=? (boolean? <- (inner:list? char? char? **1)))
(char=? (boolean? <- (inner:list? char? char? **1)))
(char>? (boolean? <- (inner:list? char? char? **1)))
(char>? (boolean? <- (inner:list? char? char? **1)))
(char>=? (boolean? <- (inner:list? char? char? **1)))
(char>=? (boolean? <- (inner:list? char? char? **1)))
(char->integer (integer? <- (inner:list? char?)))
(char-alphabetic? (boolean? <- (inner:list? char?)))
(char-ci<? (boolean? <- (inner:list? char? char? **1)))
(char-ci<? (boolean? <- (inner:list? char? char? **1)))
(char-ci<=? (boolean? <- (inner:list? char? char? **1)))
(char-ci=? (boolean? <- (inner:list? char? char? **1)))
(char-ci=? (boolean? <- (inner:list? char? char? **1)))
(char-ci>? (boolean? <- (inner:list? char? char? **1)))
(char-ci>? (boolean? <- (inner:list? char? char? **1)))
(char-ci>=? (boolean? <- (inner:list? char? char? **1)))
(char-ci>=? (boolean? <- (inner:list? char? char? **1)))
(char-downcase (char? <- (inner:list? char?)))
(char-foldcase (char? <- (inner:list? char?)))
(char-lower-case? (boolean? <- (inner:list? char?)))
(char-numeric? (boolean? <- (inner:list? char?)))
(char-ready? (boolean? <- (inner:list? )))
(char-title-case? (boolean? <- (inner:list? char?)))
(char-upcase (char? <- (inner:list? char?)))
(char-upper-case? (boolean? <- (inner:list? char?)))
(char-whitespace? (boolean? <- (inner:list? char?)))
(command-line ((inner:list? string? ...) <- (inner:list? )))
(compile-time-value? (boolean? <- (inner:list? something?)))
(complex? (boolean? <- (inner:list? something?)))
(condition? (boolean? <- (inner:list? something?)))
(continuation-condition? (boolean? <- (inner:list? something?)))
(cos (number? <- (inner:list? number?)))
(cosh (number? <- (inner:list? number?)))
(cost-center? (boolean? <- (inner:list? something?)))
(current-date (date? <- (inner:list? )))
(current-date (date? <- (inner:list? integer?)))
(current-time (time? <- (inner:list? )))
(current-time (time? <- (inner:list? symbol?)))
(date? (boolean? <- (inner:list? something?)))
(date->time-utc (time? <- (inner:list? symbol?)))
(date-and-time (string? <- (inner:list? )))
(date-and-time (string? <- (inner:list? date?)))
(date-day (integer? <- (inner:list? date?)))
(date-dst? (boolean? <- (inner:list? something?)))
(date-hour (integer? <- (inner:list? date?)))
(date-minute (integer? <- (inner:list? date?)))
(date-month (integer? <- (inner:list? date?)))
(date-nanosecond (integer? <- (inner:list? date?)))
(date-second (integer? <- (inner:list? date?)))
(date-week-day (integer? <- (inner:list? date?)))
(date-year (integer? <- (inner:list? date?)))
(date-year-day (integer? <- (inner:list? date?)))
(date-zone-name (string? <- (inner:list? date?)))
(date-zone-offset (integer? <- (inner:list? date?)))
(decode-float ((inner:vector? integer? integer? integer?) <- (inner:list? number?)))
(delete-directory (void? <- (inner:list? string?)))
(delete-file (void? <- (inner:list? string?)))
(denominator (rational? <- (inner:list? rational?)))
(directory-list ((inner:list? string? ...) <- (inner:list? string?)))
(directory-separator (char? <- (inner:list? void?)))
(directory-separator? (boolean? <- (inner:list? char?)))
(display-statistics (void? <- (inner:list? )))
(display-string (void? <- (inner:list? string?)))
(div (number? <- (inner:list? number? number?)))
(div0 (number? <- (inner:list? number? number?)))
(environment? (boolean? <- (inner:list? something?)))
(eof-object? (boolean? <- (inner:list? something?)))
(ephemeron-pair? (boolean? <- (inner:list? something?)))
(eq? (boolean? <- (inner:list? something? something?)))
(eq-hashtable? (boolean? <- (inner:list? something?)))
(equal? (boolean? <- (inner:list? something? something?)))
(eqv? (boolean? <- (inner:list? something? something?)))
(error? (boolean? <- (inner:list? something?)))
(even? (boolean? <- (inner:list? integer?)))
(exact? (boolean? <- (inner:list? number?)))
(file-directory? (boolean? <- (inner:list? string?)))
(file-directory? (boolean? <- (inner:list? string? boolean?)))
(file-exists? (boolean? <- (inner:list? string?)))
(file-exists? (boolean? <- (inner:list? string? boolean?)))
(file-regular? (boolean? <- (inner:list? string?)))
(file-regular? (boolean? <- (inner:list? string? boolean?)))
(file-symbolic-link? (boolean? <- (inner:list? string?)))
(finite? (boolean? <- (inner:list? real?)))
(fixnum? (boolean? <- (inner:list? something?)))
(fixnum->flonum (flonum? <- (inner:list? fixnum?)))
(fl- (flonum? <- (inner:list? flonum?)))
(fl- (flonum? <- (inner:list? flonum? flonum? **1)))
(fl* (flonum? <- (inner:list? flonum? ...)))
(fl/ (flonum? <- (inner:list? flonum?)))
(fl/ (flonum? <- (inner:list? flonum? flonum? **1)))
(fl+ (flonum? <- (inner:list? flonum? ...)))
(fl< (flonum? <- (inner:list? flonum? **1)))
(fl<? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fl<= (flonum? <- (inner:list? flonum? **1)))
(fl<=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fl= (flonum? <- (inner:list? flonum? **1)))
(fl=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fl> (flonum? <- (inner:list? flonum? **1)))
(fl>? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fl>= (flonum? <- (inner:list? flonum? **1)))
(fl>=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(flabs (flonum? <- (inner:list? flonum?)))
(flacos (flonum? <- (inner:list? flonum?)))
(flasin (flonum? <- (inner:list? flonum?)))
(flatan (flonum? <- (inner:list? flonum?)))
(flatan (flonum? <- (inner:list? flonum? flonum?)))
(flceiling (flonum? <- (inner:list? flonum?)))
(flcos (flonum? <- (inner:list? flonum?)))
(fldenominator (flonum? <- (inner:list? flonum?)))
(fldiv (flonum? <- (inner:list? flonum? flonum?)))
(fldiv0 (flonum? <- (inner:list? flonum? flonum?)))
(fleven? (boolean? <- (inner:list? fixnum?)))
(flexp (flonum? <- (inner:list? flonum?)))
(flexpt (flonum? <- (inner:list? flonum? flonum?)))
(flfinite? (boolean? <- (inner:list? fixnum?)))
(flfloor (flonum? <- (inner:list? flonum?)))
(flinfinite? (boolean? <- (inner:list? fixnum?)))
(flinteger? (boolean? <- (inner:list? fixnum?)))
(fllog (flonum? <- (inner:list? flonum?)))
(fllog (flonum? <- (inner:list? flonum? flonum?)))
(fllp (flonum? <- (inner:list? flonum?)))
(flmax (flonum? <- (inner:list? flonum? **1)))
(flmin (flonum? <- (inner:list? flonum? **1)))
(flmod (flonum? <- (inner:list? flonum? flonum?)))
(flmod0 (flonum? <- (inner:list? flonum? flonum?)))
(flnan? (boolean? <- (inner:list? fixnum?)))
(flnegative? (boolean? <- (inner:list? fixnum?)))
(flnonnegative? (boolean? <- (inner:list? fixnum?)))
(flnonpositive? (boolean? <- (inner:list? fixnum?)))
(flnumerator (flonum? <- (inner:list? flonum?)))
(flodd? (boolean? <- (inner:list? fixnum?)))
(flonum? (boolean? <- (inner:list? something?)))
(flonum->fixnum (fixnum? <- (inner:list? flonum?)))
(floor (flonum? <- (inner:list? real?)))
(flpositive? (boolean? <- (inner:list? fixnum?)))
(flround (flonum? <- (inner:list? flonum?)))
(flsin (flonum? <- (inner:list? flonum?)))
(flsqrt (flonum? <- (inner:list? flonum?)))
(fltan (flonum? <- (inner:list? flonum?)))
(fltruncate (flonum? <- (inner:list? flonum?)))
(flush-output-port (flonum? <- (inner:list? )))
(flush-output-port (flonum? <- (inner:list? )))
(flzero? (boolean? <- (inner:list? fixnum?)))
(format-condition? (boolean? <- (inner:list? something?)))
(fx- (fixnum? <- (inner:list? fixnum? **1)))
(fx- (fixnum? <- (inner:list? fixnum?)))
(fx- (fixnum? <- (inner:list? fixnum? fixnum?)))
(fx* (fixnum? <- (inner:list? fixnum? ...)))
(fx* (fixnum? <- (inner:list? fixnum? fixnum?)))
(fx*/carry (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fx/ (fixnum? <- (inner:list? fixnum? **1)))
(fx-/carry (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fx+ (fixnum? <- (inner:list? fixnum? ...)))
(fx+ (fixnum? <- (inner:list? fixnum? fixnum?)))
(fx+/carry (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fx< (boolean? <- (inner:list? fixnum? **1)))
(fx<? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fx<= (boolean? <- (inner:list? fixnum? **1)))
(fx<=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fx= (boolean? <- (inner:list? fixnum? **1)))
(fx=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fx> (boolean? <- (inner:list? fixnum? **1)))
(fx>? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fx>= (boolean? <- (inner:list? fixnum? **1)))
(fx>=? (boolean? <- (inner:list? fixnum? fixnum? **1)))
(fx1- (fixnum? <- (inner:list? fixnum?)))
(fx1+ (fixnum? <- (inner:list? fixnum?)))
(fxabs (fixnum? <- (inner:list? fixnum?)))
(fxand (fixnum? <- (inner:list? fixnum? ...)))
(fxarithmetic-shift (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxarithmetic-shift-left (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxarithmetic-shift-right (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxbit-count (fixnum? <- (inner:list? fixnum?)))
(fxbit-field (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fxbit-set? (boolean? <- (inner:list? fixnum? fixnum?)))
(fxcopy-bit (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fxcopy-bit-field (fixnum? <- (inner:list? fixnum? fixnum? fixnum? fixnum?)))
(fxdiv (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxdiv0 (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxdiv0-and-mod0 (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxdiv-and-mod (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxeven? (boolean? <- (inner:list? fixnum?)))
(fxfirst-bit-set (fixnum? <- (inner:list? fixnum?)))
(fxif (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fxior (fixnum? <- (inner:list? fixnum? ...)))
(fxlength (fixnum? <- (inner:list? fixnum?)))
(fxlogand (fixnum? <- (inner:list? fixnum? ...)))
(fxlogbit? (boolean? <- (inner:list? integer? fixnum?)))
(fxlogbit0 (fixnum? <- (inner:list? integer? fixnum?)))
(fxlogbit1 (fixnum? <- (inner:list? integer? fixnum?)))
(fxlogior (fixnum? <- (inner:list? fixnum? ...)))
(fxlognot (fixnum? <- (inner:list? fixnum?)))
(fxlogor (fixnum? <- (inner:list? fixnum? ...)))
(fxlogtest (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxlogxor (fixnum? <- (inner:list? fixnum? ...)))
(fxmax (fixnum? <- (inner:list? fixnum? **1)))
(fxmin (fixnum? <- (inner:list? fixnum?)))
(fxmod (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxmod0 (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxmodulo (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxnegative? (boolean? <- (inner:list? fixnum?)))
(fxnonnegative? (boolean? <- (inner:list? fixnum?)))
(fxnonpositive? (boolean? <- (inner:list? fixnum?)))
(fxnot (fixnum? <- (inner:list? fixnum?)))
(fxodd? (boolean? <- (inner:list? fixnum?)))
(fxpositive? (boolean? <- (inner:list? fixnum?)))
(fxquotient (fixnum? <- (inner:list? fixnum? **1)))
(fxremainder (fixnum? <- (inner:list? fixnum? fixnum?)))
(fxreverse-bit-field (fixnum? <- (inner:list? fixnum? fixnum? fixnum?)))
(fxrotate-bit-field (fixnum? <- (inner:list? fixnum? fixnum? fixnum? fixnum?)))
(fxsll (fixnum? <- (inner:list? fixnum? integer?)))
(fxsra (fixnum? <- (inner:list? fixnum? integer?)))
(fxsrl (fixnum? <- (inner:list? fixnum? integer?)))
(fxvector ((inner:vector? fixnum? ...) <- (inner:list? fixnum? ...)))
(fxvector? (boolean? <- (inner:list? something?)))
(fxvector->list ((inner:list? fixnum? ...) <- (inner:list? (inner:vector? fixnum? ...))))
(fxvector-copy ((inner:vector? fixnum? ...) <- (inner:list? fixnum? ...)))
(fxvector-fill! (void? <- (inner:list? (inner:vector? fixnum? ...) fixnum?)))
(fxvector-ref (fixnum? <- (inner:list? (inner:vector? fixnum? ...) integer?)))
(fxvector-set! (void? <- (inner:list? (inner:vector? fixnum? ...) integer? fixnum?)))
(fxxor (fixnum? <- (inner:list? fixnum? ...)))
(fxzero? (boolean? <- (inner:list? fixnum?)))
(gensym? (boolean? <- (inner:list? something?)))
(greatest-fixnum (fixnum? <- (inner:list? )))
(guardian? (boolean? <- (inner:list? something?)))
(hashtable? (boolean? <- (inner:list? something?)))
(hash-table? (boolean? <- (inner:list? something?)))
(hashtable-clear! (void? <- (inner:list? )))
(hashtable-clear! (void? <- (inner:list? )))
(hashtable-delete! (void? <- (inner:list? )))
(hashtable-ephemeron? (boolean? <- (inner:list? something?)))
(hashtable-set! (void? <- (inner:list? )))
(hashtable-update! (void? <- (inner:list? )))
(hashtable-weak? (boolean? <- (inner:list? something?)))
(i/o-decoding-error? (boolean? <- (inner:list? something?)))
(i/o-encoding-error? (boolean? <- (inner:list? something?)))
(i/o-error? (boolean? <- (inner:list? something?)))
(i/o-file-already-exists-error? (boolean? <- (inner:list? something?)))
(i/o-file-does-not-exist-error? (boolean? <- (inner:list? something?)))
(i/o-file-is-read-only-error? (boolean? <- (inner:list? something?)))
(i/o-filename-error? (boolean? <- (inner:list? something?)))
(i/o-file-protection-error? (boolean? <- (inner:list? something?)))
(i/o-invalid-position-error? (boolean? <- (inner:list? something?)))
(i/o-port-error? (boolean? <- (inner:list? something?)))
(i/o-read-error? (boolean? <- (inner:list? something?)))
(i/o-write-error? (boolean? <- (inner:list? something?)))
(identifier? (boolean? <- (inner:list? something?)))
(immutable-box? (boolean? <- (inner:list? something?)))
(immutable-bytevector? (boolean? <- (inner:list? something?)))
(immutable-fxvector? (boolean? <- (inner:list? something?)))
(immutable-string? (boolean? <- (inner:list? something?)))
(immutable-vector? (boolean? <- (inner:list? something?)))
(implementation-restriction-violation? (boolean? <- (inner:list? something?)))
(inexact? (boolean? <- (inner:list? number?)))
(infinite? (boolean? <- (inner:list? real?)))
(input-port? (boolean? <- (inner:list? something?)))
(integer? (boolean? <- (inner:list? something?)))
(integer->char (char? <- (inner:list? integer?)))
(integer-valued? (boolean? <- (inner:list? something?)))
(interactive? (boolean? <- (inner:list? )))
(irritants-condition? (boolean? <- (inner:list? something?)))
(least-fixnum (fixnum? <- (inner:list? )))
(lexical-violation? (boolean? <- (inner:list? something?)))
(list (with (a ...) (with-append (inner:list?) a)))
(list ((inner:list? something? ...) <- (inner:list? something? ...)))
(list? (boolean? <- (inner:list? something?)))
(list->fxvector ((inner:vector? fixnum? ...) <- (inner:list? (inner:list? something?))))
(load (void? <- (inner:list? string?)))
(load-library (void? <- (inner:list? string?)))
(load-program (void? <- (inner:list? string?)))
(load-shared-object (void? <- (inner:list? string?)))
(locked-object? (boolean? <- (inner:list? something?)))
(log (number? <- (inner:list? number?)))
(log (number? <- (inner:list? number? number?)))
(logbit? (boolean? <- (inner:list? integer? integer?)))
(machine-type (symbol? <- (inner:list? )))
(magnitude (number? <- (inner:list? number?)))
(magnitude-squared (number? <- (inner:list? number?)))
(make-compile-time-value (compile-time-value? <- (inner:list? something?)))
(make-date (date? <- (inner:list? integer? integer? integer? integer? integer? integer? integer?)))
(make-date (date? <- (inner:list? integer? integer? integer? integer? integer? integer? integer? integer?)))
(make-fxvector ((inner:vector? fixnum? ...) <- (inner:list? integer?)))
(make-fxvector ((inner:vector? fixnum? ...) <- (inner:list? integer? fixnum?)))
(max (real? <- (inner:list? real? real?)))
(message-condition? (boolean? <- (inner:list? something?)))
(min (real? <- (inner:list? real? real?)))
(mkdir (void? <- (inner:list? string?)))
(mkdir (void? <- (inner:list? string? integer?)))
(mod (number? <- (inner:list? number? number?)))
(mod0 (number? <- (inner:list? number? number?)))
(modulo (integer? <- (inner:list? integer? integer?)))
(most-negative-fixnum (fixnum? <- (inner:list? )))
(most-positive-fixnum (fixnum? <- (inner:list? )))
(mutable-box? (boolean? <- (inner:list? something?)))
(mutable-bytevector? (boolean? <- (inner:list? something?)))
(mutable-fxvector? (boolean? <- (inner:list? something?)))
(mutable-string? (boolean? <- (inner:list? something?)))
(mutable-vector? (boolean? <- (inner:list? something?)))
(mutex? (boolean? <- (inner:list? something?)))
(nan? (boolean? <- (inner:list? real?)))
(negative? (boolean? <- (inner:list? real?)))
(no-infinities-violation? (boolean? <- (inner:list? something?)))
(no-nans-violation? (boolean? <- (inner:list? something?)))
(non-continuable-violation? (boolean? <- (inner:list? something?)))
(nonnegative? (boolean? <- (inner:list? real?)))
(nonpositive? (boolean? <- (inner:list? real?)))
(not (boolean? <- (inner:list? something?)))
(null? (boolean? <- (inner:list? something?)))
(number? (boolean? <- (inner:list? something?)))
(numerator (number? <- (inner:list? rational?)))
(object-counts (integer? <- (inner:list? )))
(odd? (boolean? <- (inner:list? integer?)))
(or (something? <- (list? something? ...)))
(output-port? (boolean? <- (inner:list? something?)))
(pair? (boolean? <- (inner:list? something?)))
(path-absolute? (boolean? <- (inner:list? string?)))
(petite? (boolean? <- (inner:list? )))
(port? (boolean? <- (inner:list? something?)))
(positive? (boolean? <- (inner:list? real?)))
(procedure? (boolean? <- (inner:list? something?)))
(quotient (integer? <- (inner:list? integer? integer?)))
(random (real? <- (inner:list? real?)))
(rational? (boolean? <- (inner:list? something?)))
(rationalize (real? <- (inner:list? real? real?)))
(rational-valued? (boolean? <- (inner:list? something?)))
(ratnum? (boolean? <- (inner:list? something?)))
(read-char (char? <- (inner:list? )))
(read-char (char? <- (inner:list? textual-output-port?)))
(real? (boolean? <- (inner:list? something?)))
(real->flonum (flonum? <- (inner:list? real?)))
(real-part (real? <- (inner:list? number?)))
(real-time (integer? <- (inner:list? )))
(real-valued? (boolean? <- (inner:list? something?)))
(record? (boolean? <- (inner:list? something?)))
(record? (boolean? <- (inner:list? something? something?)))
(record-constructor-descriptor? (boolean? <- (inner:list? something?)))
(record-type-descriptor? (boolean? <- (inner:list? something?)))
(reset-cost-center! (void? <- (inner:list? )))
(reset-maximum-memory-bytes! (void? <- (inner:list? )))
(round (integer? <- (inner:list? real?)))
(serious-condition? (boolean? <- (inner:list? something?)))
(sin (number? <- (inner:list? number?)))
(sinh (number? <- (inner:list? number?)))
(source-condition? (boolean? <- (inner:list? something?)))
(source-file-descriptor? (boolean? <- (inner:list? something?)))
(source-object? (boolean? <- (inner:list? something?)))
(source-table? (boolean? <- (inner:list? something?)))
(sqrt (number? <- (inner:list? number?)))
(sstats? (boolean? <- (inner:list? something?)))
(string? (boolean? <- (inner:list? something?)))
(string<? (boolean? <- (inner:list? string? string? **1)))
(string<=? (boolean? <- (inner:list? string? string? **1)))
(string=? (boolean? <- (inner:list? string? string? **1)))
(string>? (boolean? <- (inner:list? string? string? **1)))
(string>=? (boolean? <- (inner:list? string? string? **1)))
(string->list ((inner:list? char? ...) <- (inner:list? string?)))
(string->number (number? <- (inner:list? string?)))
(string->number (number? <- (inner:list? string? integer?)))
(string->symbol (symbol? <- (inner:list? string?)))
(string-ci<? (boolean? <- (inner:list? string? string? **1)))
(string-ci<=? (boolean? <- (inner:list? string? string? **1)))
(string-ci=? (boolean? <- (inner:list? string? string? **1)))
(string-ci>? (boolean? <- (inner:list? string? string? **1)))
(string-ci>=? (boolean? <- (inner:list? string? string? **1)))
(string-length (integer? <- (inner:list? string?)))
(string-ref (char? <- (inner:list? string? integer?)))
(sub1 (number? <- (inner:list? number?)))
(symbol? (boolean? <- (inner:list? something?)))
(symbol=? (boolean? <- (inner:list? symbol? symbol?)))
(symbol-hash (integer? <- (inner:list? symbol?)))
(symbol-hashtable? (boolean? <- (inner:list? something?)))
(syntax-violation? (boolean? <- (inner:list? something?)))
(tan (number? <- (inner:list? number?)))
(tanh (number? <- (inner:list? number?)))
(textual-port? (boolean? <- (inner:list? something?)))
(thread? (boolean? <- (inner:list? something?)))
(thread-condition? (boolean? <- (inner:list? something?)))
(threaded? (boolean? <- (inner:list? )))
(time? (boolean? <- (inner:list? something?)))
(time<? (boolean? <- (inner:list? time? time?)))
(time<=? (boolean? <- (inner:list? time? time?)))
(time=? (boolean? <- (inner:list? time? time?)))
(time>? (boolean? <- (inner:list? time? time?)))
(time>=? (boolean? <- (inner:list? time? time?)))
(time-utc->date (date? <- (inner:list? time?)))
(time-utc->date (date? <- (inner:list? time? time?)))
(top-level-bound? (boolean? <- (inner:list? symbol?)))
(top-level-mutable? (boolean? <- (inner:list? symbol?)))
(top-level-syntax? (boolean? <- (inner:list? symbol?)))
(trace-case-lambda ((something? <- (something? ...)) <- (list? )))
(trace-lambda ((something? <- (something? ...)) <- (list? )))
(transcoder? (boolean? <- (inner:list? something?)))
(truncate (real? <- (inner:list? real?)))
(undefined-violation? (boolean? <- (inner:list? something?)))
(vector? (boolean? <- (inner:list? something?)))
(vector-length (integer? <- (inner:list? (inner:vector? something? ...))))
(vector-ref (something? <- (inner:list? (inner:vector? something?) integer?)))
(violation? (boolean? <- (inner:list? something?)))
(virtual-register-count (integer? <- (inner:list? )))
(warning? (boolean? <- (inner:list? something?)))
(weak-pair? (boolean? <- (inner:list? something?)))
(who-condition? (boolean? <- (inner:list? something?)))
(zero? (boolean? <- (inner:list? number?)))
))))