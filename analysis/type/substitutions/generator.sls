(library (scheme-langserver analysis type substitutions generator)
  (export 
    construct-substitution-list-for
    pretty-print-substitution )
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type substitutions rules if)
    (scheme-langserver analysis type substitutions rules let)
    (scheme-langserver analysis type substitutions rules lambda)
    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type substitutions rules define)
    (scheme-langserver analysis type substitutions rules application)
    (scheme-langserver analysis type substitutions util))

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
  substitutions
; ; (pretty-print 'implicit0)
;   (let ([children (index-node-children index-node)])
;     (if (null? children)
;       substitutions
;       (let* ([base (fold-left private-add-implicit-convertions substitutions children)]
;           [head-index-node (car children)]
;           [rest-index-nodes (cdr children)]
;           [reified-head-result (reify base (index-node-variable head-index-node))]
;           [filtered-lambdas (dedupe (filter lambda? reified-head-result))]
;           [return-variable (index-node-variable index-node)])
; ; (pretty-print 'implicit1)
; ; (pretty-print (annotation-stripped (index-node-datum/annotations index-node)))
;         (lambda-templates->new-substitution-list base filtered-lambdas `(,return-variable) rest-index-nodes))))
        )

(define (private-construct-substitution-list document index-node base-substitution-list)
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
        application-process))))
)