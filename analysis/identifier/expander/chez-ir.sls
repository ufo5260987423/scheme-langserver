#!chezscheme
(library (scheme-langserver analysis identifier expander chez-ir)
  ;; The #!chezscheme directive on the first line is required: this file
  ;; uses #%$ kernel primitives, which are rejected in #!r6rs mode, and
  ;; the directive must immediately precede the (library ...) form. No
  ;; other project library needs this, so it stays isolated here.
  (export
    chez-ir-available?
    chez-ir-read-annotated-file
    chez-ir-find-annotated-form
    chez-ir-find-form-containing
    chez-ir-find-define-containing
    chez-ir-expand
    chez-ir-call-site-refs
    chez-ir-global-lookup?
    chez-ir-contains-quoted-symbol?)
  (import (chezscheme))

;;; chez-ir: extract ground-truth identifier resolution from Chez's own
;;; macro expander via the undocumented (sc-expand x env #t) switch,
;;; which returns the parsed Lsrc IR with per-expression / per-reference
;;; / per-binding source locations.
;;;
;;; See doc/analysis/chez-sc-expand-records-switch.md for the full story.
;;; WARNING: everything below relies on undocumented Chez internals
;;; (sc-expand 3rd argument, #%$record-type-descriptor, nongenerative
;;; record uids, nanopass record type names). Isolate all access here;
;;; a Chez upgrade only breaks this file. The hard "unrecognized record"
;;; error raised by the public record-type-descriptor on kernel records
;;; is NOT catchable (it bypasses the exception system), so we must use
;;; #%$record-type-descriptor everywhere and never touch annotation
;;; records with the public procedure.

;; A user record for the feature self-check only.
(define-record-type private:probe
  (fields a))

(define (chez-ir-available?)
  ;; Two probes: the 3-arity sc-expand (argument-count errors are
  ;; catchable), and #%$record-type-descriptor on a user record.
  (and (guard (e [#t #f])
         (sc-expand '(quote x) (interaction-environment) #t)
         #t)
    (guard (e [#t #f])
      (eq? 'private:probe
        (record-type-name (#%$record-type-descriptor (make-private:probe 1))))
      #t)))

(define (chez-ir-read-annotated-file path)
  ;; Read all top-level forms with annotations. The bfp protocol of
  ;; get-datum/annotations is required for multi-byte characters; the
  ;; first call takes bfp 0, each subsequent call the previous efp.
  (let ([port (open-input-file path)]
      [sfd (make-source-file-descriptor path (open-file-input-port path))])
    (let loop ([bfp 0] [acc '()])
      (let-values ([(datum efp) (get-datum/annotations port sfd bfp)])
        (if (eof-object? datum)
          (begin (close-port port) (reverse acc))
          (loop efp (cons datum acc)))))))

(define (private:strip o)
  (if (annotation? o) (annotation-stripped o) o))

(define (private:deep-strip o)
  ;; get-datum/annotations wraps every node (including atoms) in an
  ;; annotation, so a single strip is not enough for shape comparison.
  (let ([s (private:strip o)])
    (if (pair? s)
      (cons (private:deep-strip (car s)) (private:deep-strip (cdr s)))
      s)))

(define (chez-ir-find-annotated-form predicate annotated)
  ;; Depth-first search over an annotated datum. predicate receives the
  ;; stripped form of each compound pair. Returns the annotated subform
  ;; whose stripped shape satisfies predicate, or #f.
  (let ([seen (make-eq-hashtable)])
    (let search ([o annotated])
      (cond
        [(eq-hashtable-ref seen o #f) #f]
        [(annotation? o) (search (annotation-expression o))]
        [(not (pair? o)) #f]
        [else
          (eq-hashtable-set! seen o #t)
          (let ([stripped (private:deep-strip o)])
            (if (and (pair? stripped) (predicate stripped))
              o
              (let ([here (search (car o))])
                (if here here (search (cdr o))))))]))))

(define (chez-ir-find-form-containing annotated-forms bias)
  ;; The annotated top-level form whose source span contains bias, choosing
  ;; the smallest span when forms nest (they should not at top level, but be
  ;; defensive). Returns #f when no form contains the position.
  (let ([hits
      (filter
        (lambda (form)
          (and (annotation? form)
            (let ([src (annotation-source form)])
              (and (source-object? src)
                (<= (source-object-bfp src) bias)
                (>= (source-object-efp src) bias)))))
        annotated-forms)])
    (if (null? hits)
      #f
      (car (sort (lambda (f1 f2)
            (< (- (source-object-efp (annotation-source f1))
                 (source-object-bfp (annotation-source f1)))
              (- (source-object-efp (annotation-source f2))
                (source-object-bfp (annotation-source f2)))))
          hits)))))

(define (chez-ir-find-define-containing annotated bias)
  ;; Smallest (define ...) form whose source span contains bias, descending by
  ;; span containment. Expanding the enclosing DEFINE -- not the library form --
  ;; matters: library top-level expansion defers definitions to compile-time
  ;; parts and yields an empty runtime IR (measured: sc-expand of a library form
  ;; returns a single Lsrc:quote). Spans are memoized per call (pair spans are
  ;; merged from children; only annotated nodes carry real source objects).
  (let ([memo (make-eq-hashtable)])
    (define (span o)
      (let ([cached (eq-hashtable-ref memo o 'unset)])
        (if (not (eq? cached 'unset))
          cached
          (let ([s
              (cond
                [(annotation? o)
                  (let ([src (annotation-source o)])
                    (and (source-object? src)
                      (cons (source-object-bfp src) (source-object-efp src))))]
                [(pair? o)
                  (let ([a (span (car o))] [d (span (cdr o))])
                    (cond
                      [(and a d) (cons (min (car a) (car d)) (max (cdr a) (cdr d)))]
                      [a]
                      [d]
                      [else #f]))]
                [else #f])])
            (eq-hashtable-set! memo o s)
            s))))
    (define (contains? s)
      (and s (<= (car s) bias) (>= (cdr s) bias)))
    (let loop ([o annotated] [best #f])
      (cond
        [(annotation? o) (loop (annotation-expression o) best)]
        [(not (pair? o)) best]
        [else
          (let* ([s (span o)]
              [here? (and (contains? s)
                      (eq? 'define (private:strip (car o))))]
              [best2 (if here? o best)])
            (let child-loop ([kids o] [b best2])
              (cond
                [(null? kids) b]
                ;improper tail (dotted template syntax like (pat . body)): the
                ;tail is itself a node and may still contain the bias
                [(not (pair? kids)) (loop kids b)]
                [else
                  (child-loop (cdr kids)
                    (if (contains? (span (car kids)))
                      (loop (car kids) b)
                      b))])))]))))

(define chez-ir-expand
  (case-lambda
    [(annotated-datum) (chez-ir-expand annotated-datum (interaction-environment))]
    [(annotated-datum env)
      ;; The switch: records? = #t skips the $uncprep unparsing and
      ;; returns the parsed Lsrc IR with source locations intact.
      (sc-expand annotated-datum env #t)]))

(define (private:ancestor-rtd rtd name)
  (let loop ([rtd rtd])
    (cond
      [(not rtd) #f]
      [(eq? (record-type-name rtd) name) rtd]
      [else (loop (record-type-parent rtd))])))

(define (private:name-prefix? rtd prefix)
  (let* ([name (symbol->string (record-type-name rtd))]
      [len (string-length name)]
      [plen (string-length prefix)])
    (and (>= len plen)
      (string=? (substring name 0 plen) prefix))))

(define (private:field-refs o rtd)
  ;; All field values of a record, guarded (some fields may be
  ;; unreadable in exotic cases; result is discarded anyway).
  (let ([n (vector-length (record-type-field-names rtd))])
    (let loop ([i 0] [acc '()])
      (if (fx= i n)
        (reverse acc)
        (loop (fx+ i 1)
          (cons (guard (e [#t #f]) ((record-accessor rtd i) o)) acc))))))

;; A source slot in the IR is either a plain source object (ref/set!
;; maybe-src) or an annotated identifier (prelex source), the latter
;; carrying its source one unwrap away.
(define (private:source-object-of src)
  (cond
    [(source-object? src) src]
    [(annotation? src)
      (let ([s (annotation-source src)])
        (and (source-object? s) s))]
    [else #f]))

(define (private:source-in-path? src path)
  (let ([s (private:source-object-of src)])
    (and s
      (string=? path (source-file-descriptor-path (source-object-sfd s))))))

(define (private:source-bias src)
  (let ([s (private:source-object-of src)])
    (and s (source-object-bfp s))))

(define (private:rtd-name-prefix? rtd prefix)
  (let* ([name (symbol->string (record-type-name rtd))]
      [len (string-length name)]
      [plen (string-length prefix)])
    (and (>= len plen) (string=? (substring name 0 plen) prefix))))

(define (private:record-walk ir proc)
  ;;proc: (record rtd) -> boolean; #t stops the walk with #t. Returns #f when
  ;;the whole tree was visited without a #t.
  (let ([seen (make-eq-hashtable)])
    (define (visit o)
      (cond
        [(pair? o)
          (or (visit (car o)) (visit (cdr o)))]
        [(annotation? o)
          ;annotations wrap IR subtrees (and carry the source positions) --
          ;descend into the wrapped expression
          (visit (annotation-expression o))]
        [(record? o)
          (if (eq-hashtable-ref seen o #f)
            #f
            (begin
              (eq-hashtable-set! seen o #t)
              (let ([rtd (guard (e [#t #f]) (#%$record-type-descriptor o))])
                (if (not rtd)
                  #f
                  (or (proc o rtd)
                    (let loop ([vals (private:field-refs o rtd)])
                      (cond
                        [(null? vals) #f]
                        [(visit (car vals)) #t]
                        [else (loop (cdr vals))])))))))]
        [else #f]))
    (visit ir)))

(define (private:global-lookup-record? o rtd identifier-string)
  ;;#t when record o is (<expr> ... ($top-level-value '<name>) ...) -- i.e. o
  ;;CALLS the result of a top-level lookup. call fields are (preinfo e0 e1 ...)
  ;;so the operator is the SECOND field.
  (and (private:rtd-name-prefix? rtd "Lsrc:call")
    (let ([vals (private:field-refs o rtd)])
      (and (pair? vals) (pair? (cdr vals))
        (let ([operator (cadr vals)])
          (and (record? operator)
            (let ([op-rtd (guard (e [#t #f]) (#%$record-type-descriptor operator))])
              (and op-rtd
                (private:rtd-name-prefix? op-rtd "Lsrc:call")
                (private:global-lookup-operator? operator op-rtd identifier-string)))))))))

(define (private:global-lookup-operator? operator op-rtd identifier-string)
  ;;inner call fields: (preinfo (primref $top-level-value) ((quote name)) ...)
  ;;the argument tail is stored as a single LIST field
  (let ([op-vals (private:field-refs operator op-rtd)])
    (and (pair? op-vals) (pair? (cdr op-vals)) (pair? (cddr op-vals))
      (pair? (caddr op-vals))
      (let ([head (cadr op-vals)]
          [arg (car (caddr op-vals))])
        (and (record? head) (record? arg)
          (let ([head-rtd (guard (e [#t #f]) (#%$record-type-descriptor head))]
              [arg-rtd (guard (e [#t #f]) (#%$record-type-descriptor arg))])
            (and head-rtd arg-rtd
              (eq? (record-type-name head-rtd) 'primref)
              (private:rtd-name-prefix? arg-rtd "Lsrc:quote")
              (private:global-lookup-parts? head-rtd head arg-rtd arg identifier-string))))))))

(define (private:global-lookup-parts? head-rtd head arg-rtd arg identifier-string)
  (let ([prim-name ((record-accessor head-rtd 0) head)]
      [datum ((record-accessor arg-rtd 0) arg)])
    (and (eq? prim-name '$top-level-value)
      (symbol? datum)
      (string=? (symbol->string datum) identifier-string))))

(define (chez-ir-global-lookup? ir identifier-string)
  ;;#t when ir contains ($top-level-value '<name>) -- i.e. the identifier was
  ;;treated as an UNBOUND GLOBAL (interaction envs are lenient) rather than
  ;;consumed by macro expansion. Chez marks such symbols specially
  ;;(#{match *top*:match}), hence the name-STRING comparison.
  (private:record-walk ir
    (lambda (o rtd)
      (private:global-lookup-record? o rtd identifier-string))))

(define (chez-ir-contains-quoted-symbol? ir symbol)
  ;;#t when ir contains (quote <the interned SYMBOL>). Positive confirmation
  ;;that a probe expansion really engaged the match macro (its failure thunk
  ;;carries (quote match)); complements chez-ir-global-lookup?.
  (private:record-walk ir
    (lambda (o rtd)
      (and (private:rtd-name-prefix? rtd "Lsrc:quote")
        (eq? ((record-accessor rtd 0) o) symbol)))))

(define (chez-ir-call-site-refs ir path)
  ;; Walk the IR and collect every Lsrc:ref whose own source is in
  ;; `path` (i.e. code copied verbatim from the call site, as opposed
  ;; to macro-introduced code whose source is the macro definition
  ;; file or #f).
  ;; Returns a list of (name ref-bias binding-bias|#f):
  ;;   name        - the referenced variable's display name
  ;;   ref-bias    - character offset of this reference
  ;;   binding-bias- offset where the referenced prelex is bound, or #f
  ;;                 when the binding is macro-introduced (no source)
  (let ([seen (make-eq-hashtable)]
      [result '()])
    (define (emit! name ref-bias binding-bias)
      (set! result (cons (list name ref-bias binding-bias) result)))
    (define (prelex-name prelex)
      (guard (e [#t '?])
        ((record-accessor (private:ancestor-rtd (#%$record-type-descriptor prelex) 'prelex) 0) prelex)))
    (define (prelex-binding-bias prelex)
      (guard (e [#t #f])
        (let* ([rtd (#%$record-type-descriptor prelex)]
            [prelex-rtd (private:ancestor-rtd rtd 'prelex)])
          (and prelex-rtd
            (private:source-bias ((record-accessor prelex-rtd 2) prelex))))))
    (define (walk o)
      (when (record? o)
        (unless (eq-hashtable-ref seen o #f)
          (eq-hashtable-set! seen o #t)
          (let ([rtd (#%$record-type-descriptor o)])
            (cond
              [(and (private:name-prefix? rtd "Lsrc:ref")
                  (guard (e [#t #f])
                    (equal? '#(maybe-src x) (record-type-field-names rtd))))
               ;; fields: (maybe-src x)
               (let* ([vals (private:field-refs o rtd)]
                      [src (car vals)]
                      [prelex (cadr vals)])
                 (when (private:source-in-path? src path)
                   (emit! (prelex-name prelex)
                     (private:source-bias src)
                     (prelex-binding-bias prelex)))
                 (for-each walk-any vals))]
              [else
                (for-each walk-any (private:field-refs o rtd))])))))
    (define (walk-any o)
      (cond
        [(pair? o)
          (walk-any (car o))
          (walk-any (cdr o))]
        [(and (record? o) (not (annotation? o)))
          (walk o)]
        [else (void)]))
    (walk-any ir)
    (reverse result)))
)
