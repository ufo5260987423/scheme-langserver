(library (scheme-langserver virtual-file-system document)
  (export 
    make-document
    document?
    document-uri
    document-text
    document-text-set!
    document-index-node-list
    document-index-node-list-set!
    document-ordered-reference-list
    document-ordered-reference-list-set!
    document-refreshable?
    document-refreshable?-set!
    document-line-length-vector
    document-line-length-vector-set!
    document-diagnoses
    document-diagnoses-set!

    append-new-diagnoses

    document+position->bias 
    document+bias->position-list
    text->line-length-vector)
  (import 
    (chezscheme)
    (scheme-langserver util text))

(define-record-type document
  (nongenerative scheme-langserver-document)
  (fields 
    (immutable uri)
    (mutable text)
    (mutable index-node-list)
    (mutable ordered-reference-list)
    (mutable refreshable?)
    (mutable line-length-vector)
    (mutable diagnoses))
  (protocol
    (lambda (new)
      (lambda (uri text reference-list)
        (new uri text '() reference-list #t (text->line-length-vector text) '())))))

(define (append-new-diagnoses document diagnoses)
  (document-diagnoses-set!
    document
    (append (document-diagnoses document) `(,diagnoses))))

(define (document+bias->position-list document bias)
  (let loop ([current-line 0]
      [current-bias 0])
    (cond 
      [(= (vector-length (document-line-length-vector document)) current-line) (raise 'position-out-of-range)]
      [(< (+ current-bias (vector-ref (document-line-length-vector document) current-line)) bias)
        (loop (+ 1 current-line) (+ 1 current-bias (vector-ref (document-line-length-vector document) current-line)))]
      [(<= bias (+ current-bias (vector-ref (document-line-length-vector document) current-line)))
        `(,current-line ,(- bias current-bias))]
      [else (raise 'position-out-of-range)])))

(define (document+position->bias document line offset)
  (let loop ([current-line 0]
      [current-bias 0])
    (cond 
      [(= (vector-length (document-line-length-vector document)) current-line) (raise 'position-out-of-range)]
      [(< current-line line) (loop (+ 1 current-line) (+ 1 current-bias (vector-ref (document-line-length-vector document) current-line)))]
      [(and (= current-line line) (<= offset (vector-ref (document-line-length-vector document) current-line))) (+ current-bias offset)]
      [else (raise 'position-out-of-range)])))

(define (text->line-length-vector text)
  (let ([l (string-length text)])
    ;single scan with string-ref: the old per-line get-line-end-position called srfi-13
    ;string-index twice per line (~2us/char, measured 81 ms for a 40 KB file) and the
    ;quasiquote splice built the list in O(lines^2); together ~6.9 s of init on this repo
    ;(2026-10-11 profile). Line length semantics preserved: \\r\\n counts as one terminator
    ;with the \\r included in the line length (max NL RE), matching the old (min/max ...) rule.
    (let loop ([s 0] [acc '()])
      (if (>= s l)
        (list->vector (reverse acc))
        (let scan ([e s])
          (cond
            [(>= e l)
              (list->vector (reverse (cons (- e s) acc)))]
            [(char=? (string-ref text e) #\newline)
              (if (and (< (+ e 1) l) (char=? (string-ref text (+ e 1)) #\return))
                (loop (+ e 2) (cons (- (+ e 1) s) acc))
                (loop (+ e 1) (cons (- e s) acc)))]
            [(char=? (string-ref text e) #\return)
              (if (and (< (+ e 1) l) (char=? (string-ref text (+ e 1)) #\newline))
                (loop (+ e 2) (cons (- (+ e 1) s) acc))
                (loop (+ e 1) (cons (- e s) acc)))]
            [else (scan (+ e 1))]))))))
)
