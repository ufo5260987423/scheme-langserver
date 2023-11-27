(library (scheme-langserver analysis type substitutions generator)
  (export 
    construct-substitution-list-for)
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
    (scheme-langserver analysis type substitutions rules record)
    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type substitutions rules define)
    (scheme-langserver analysis type substitutions rules application)
    (scheme-langserver analysis type substitutions util))

(define (construct-substitution-list-for document)
  (document-substitution-list-set! 
    document 
    (dedupe 
      (sort 
        substitution-compare
        (apply 
          append
          (map 
            (lambda (index-node) (private-construct-substitution-list document index-node '()))
            (document-index-node-list document)))))))

(define (private-construct-substitution-list document index-node base-substitution-list)
  (let* ([children (index-node-children index-node)]
      [children-substitution-list
        (apply 
          append 
          (map 
            (lambda (child) 
              (private-construct-substitution-list document child base-substitution-list))
            children))])
    (fold-left
      (lambda (current-substitutions proc)
        ; (pretty-print 'proc)
        ; (if (not (debug:substitution-sorted? current-substitutions))
        ;   (pretty-print proc))
        (if (= (length current-substitutions) (length children-substitution-list))
          (filter
            (lambda (a) (not (null? a)))
            (proc document index-node current-substitutions))
          current-substitutions))
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
        record-process

        ;this should be the last
        application-process))))
)