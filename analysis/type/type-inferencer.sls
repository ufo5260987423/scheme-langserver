(library (scheme-langserver analysis type type-inferencer)
  (export 
    type-inference-for
    pretty-print-substitution
    construct-substitution-list-for
    find-type-conflicts)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type rules if)
    (scheme-langserver analysis type rules let)
    (scheme-langserver analysis type rules lambda)
    (scheme-langserver analysis type rules trivial)
    (scheme-langserver analysis type rules define)
    (scheme-langserver analysis type rules application)

    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine))

(define find-type-conflicts 
  (case-lambda 
    [(index-node document) 
      (let ([types (dedupe (filter is-pure-identifier-reference-misture? (type-inference-for index-node document)))])
        (if (< (length types) 2)
          '()
          (fold-left 
            (lambda (tmp-result pair)
              (if (has-intersection? (car pair) (cadr pair))
                tmp-result
                `(,@tmp-result ,pair)))
            '()
            (cartesian-product types types))))]
    [(document) 
      (filter
        (lambda (index-node) 
          (not (is-first-child? index-node)))
        (find-leaves (document-index-node-list document)))]))

(define (pretty-print-substitution document)
  (pretty-print (map 
    (lambda (substitution)
      (let* ([l (car substitution)]
          [r (car (reverse substitution))]
          [m (cadr substitution)]
          [r-o 
            (if (identifier-reference? r)
              (identifier-reference-identifier r)
              r)])
        `(,l ,m ,r-o)))
    (document-substitution-list document))))

;; We regard the indexes and references as a graph of existed variable and values. 
;; try to get result type by substitution
(define (type-inference-for index-node document)
  (dedupe (reify (document-substitution-list document) (index-node-variable index-node))))

(define (construct-substitution-list-for document)
  (document-substitution-list-set! 
    document 
    (fold-left 
      private-add-implicit-convertions
      (apply 
        append
        (map 
          (lambda (index-node) (private-construct-substitution-list document index-node '()))
          (document-index-node-list document)))
      (document-index-node-list document))))

;gradule typing: unsafe convertion
(define (private-add-implicit-convertions substitutions index-node)
  (let ([children (index-node-children index-node)])
    (if (null? children)
      substitutions
      (let* ([base (fold-left private-add-implicit-convertions substitutions children)]
          [head-index-node (car children)]
          [rest-index-nodes (cdr children)]
          [reified-head-result (reify base (index-node-variable head-index-node))]
          [filtered-lambdas (dedupe (filter lambda? reified-head-result))]
          [return-variable (index-node-variable index-node)])
        (lambda-templates->new-substitution-list base filtered-lambdas `(,return-variable) rest-index-nodes)))))

(define (private-construct-substitution-list document index-node base-substitution-list)
  ; (pretty-print (document-uri document))
  (let* ([children (index-node-children index-node)]
      [children-substitution-list
        (fold-left
          add-to-substitutions
          '()
          (apply 
            append 
            (map 
              (lambda (child) 
                (private-construct-substitution-list document child base-substitution-list))
              children)))])
    (fold-left
      (lambda (current-substitutions proc)
        ; (pretty-print 'proc)
        ; (if (not (debug:substitution-sorted? current-substitutions))
        ;   (pretty-print proc))
        (dedupe 
          (filter
            (lambda (a) (not (null? a)))
            (proc document index-node current-substitutions))))
      children-substitution-list
      ;all these processor except trivial-process must add its result to current index-node
      ;such as for (app param ...), app's result type could be (return-type (param-type ...))
      ;must extend substitution with (current-index-node-variable = return-type)
      (list 
        ;this should be the first
        trivial-process

        ;their position and order, I don't care.
        define-process
        let-process
        if-process
        lambda-process

        ;this should be the last
        application-process
      ))))
)