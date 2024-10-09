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
    document-substitution-list
    document-substitution-list-set!
    document-refreshable?
    document-refreshable?-set!
    document-line-length-vector-set!

    document+position->bias 
    document+bias->position-list
    text->line-length-vector)
  (import 
    (chezscheme)
    (scheme-langserver util text))

(define-record-type document 
  (fields 
    (immutable uri)
    ;now it is only used for type-inference in analysis/type/substitutions/trivial.sls
    (mutable text)
    (mutable index-node-list)
    (mutable ordered-reference-list)
    (mutable substitution-list)
    (mutable refreshable?)
    (mutable line-length-vector))
  (protocol
    (lambda (new)
      (lambda (uri text index-node-list reference-list)
        (new uri text index-node-list reference-list '() #t (text->line-length-vector text))))))

(define (document+bias->position-list document bias)
  (let loop ([current-line 0]
      [current-bias 0])
    (cond 
      [(= (vector-length (document-line-length-vector document)) current-line)
        `(,(- current-line 1) ,(- current-bias 1))]
      [(< bias (+ current-bias (vector-ref (document-line-length-vector document) current-line)))
        `(,current-line ,(- bias current-bias))]
      [else (loop (+ 1 current-line) (+ current-bias (vector-ref (document-line-length-vector document) current-line)))])))

(define (document+position->bias document line offset)
  (let loop ([current-line 0]
      [current-bias 0])
    (cond 
      [(= (vector-length (document-line-length-vector document)) current-line) (- current-bias 1)]
      [(< current-line line) (loop (+ 1 current-line) (+ 1 current-bias (vector-ref (document-line-length-vector document) current-line)))]
      [(and (= current-line line) (<= offset (vector-ref (document-line-length-vector document) current-line))) (+ current-bias offset)]
      [else (raise 'position-out-of-range)])))

(define (text->line-length-vector text)
  (let ([l (string-length text)])
    (list->vector 
      (let loop ([s 0])
        (if (< s l)
          (let ([e (get-line-end-position text s)])
            `(,(- e s) ,@(loop (+ e 1))))
            '())))))
)
