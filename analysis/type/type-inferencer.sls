(library (scheme-langserver analysis type type-inferencer)
  (export 
    type-inference-for
    construct-substitution-list-for)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type rules trivial)
    ; (scheme-langserver analysis type rules lambda)
    ; (scheme-langserver analysis type rules define)

    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine))

;; We regard the indexes and references as a graph of existed variable and values. 
;;todo: first, construct type-tree, for procedure, it's like ((return-type-corresponding-index-node) ((first param-index-node)(second param-index-node)))
;; try to get result type by substitution
(define (type-inference-for index-node document)
  (let* ([substitutions (document-substitution-list document)]
        [reified (dedupe (reify substitutions index-node))])
    (index-node-actural-have-type-set! index-node reified)))

(define (construct-substitution-list-for document)
  (document-substitution-list-set! document 
    (append 
      private-initial-type-constraints 
      (map 
        (lambda (index-node)
          (private-construct-substitution-list document index-node))
        (document-index-node-list document)))))

;consistent with meta.sls rnrs-meta-rules.sls
(define private-initial-type-constraints
  (append 
    (map 
      (lambda (identifier) `(,(construct-type-expression-with-meta identifier) < something?))
      '(annotation? assertion-violation? binary-port? boolean? bytevector? cflonum? char? compile-time-value? condition? continuation-condition?
          cost-center? date? eq-hashtable? error? exact? fixnum? flonum? format-condition? fxvector? guadian? hash-table? hashtable? immutable-fxvector?
          immutable-string? immutable-vector? input-port? integer? list? number? output-port? pair? port? procedure? real? something? string? symbol?
          time? vector? void?))
    ;numeric tower
    (list
      `(,(construct-type-expression-with-meta 'fixnum?) < ,(construct-type-expression-with-meta 'bignum?))
      `(,(construct-type-expression-with-meta 'bignum?) < ,(construct-type-expression-with-meta 'integer?))
      `(,(construct-type-expression-with-meta 'integer?) < ,(construct-type-expression-with-meta 'cflonum?))
      `(,(construct-type-expression-with-meta 'cflonum?) < ,(construct-type-expression-with-meta 'flonum?))
      `(,(construct-type-expression-with-meta 'flonum?) < ,(construct-type-expression-with-meta 'rational?))
      `(,(construct-type-expression-with-meta 'rational?) < ,(construct-type-expression-with-meta 'real?))
      `(,(construct-type-expression-with-meta 'real?) < ,(construct-type-expression-with-meta 'complex?))
      `(,(construct-type-expression-with-meta 'complex?) < ,(construct-type-expression-with-meta 'number?)))))

(define (private-construct-substitution-list document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [children (index-node-children index-node)]
      [tmp-substitution-list 
        (let loop ([body 
              (list trivial-process 
              ; lambda-process define-process
              )]
            [result '()])
          (if (or (null? body) (not (zero? (length result))))
            result
            (loop (cdr body) (append result ((car body) document index-node result)))))]
      [children-substitution-list 
        (apply append (map (lambda (child) (private-construct-substitution-list document child)) children))])
    (append tmp-substitution-list children-substitution-list)))

)